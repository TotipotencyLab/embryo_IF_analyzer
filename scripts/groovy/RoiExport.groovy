// RoiExport.groovy
//
// Export / measurement role, split out of the monolithic macro.
// Nothing here touches the ROI Manager: everything works from a plain
// List<Roi>, so it runs and is testable headless.

import ij.*
import ij.gui.*
import ij.io.RoiDecoder
import ij.io.RoiEncoder
import ij.measure.ResultsTable
import ij.plugin.filter.Analyzer
import java.util.zip.ZipEntry
import java.util.zip.ZipInputStream
import java.util.zip.ZipOutputStream

class RoiExport {

    /**
     * Outline coordinate table: name, roi, z, x, y -- tab separated, one row per
     * polygon vertex. Matches the format read by scripts/R/read_fiji_result.r.
     * x scales by pixelWidth and y by pixelHeight (the macro used pixelWidth for
     * both, which was wrong on anisotropic pixels).
     */
    static void saveOutlineCoords(ImagePlus imp, List<Roi> rois, List<String> names,
                                  List<Integer> slices, String basename, String path) {
        def cal = imp.getCalibration()
        double pw = cal.pixelWidth, ph = cal.pixelHeight
        def rt = new ResultsTable()
        rt.showRowNumbers(false)
        rt.setPrecision(3)
        rois.eachWithIndex { roi, i ->
            def poly = roi.getPolygon()
            for (int k = 0; k < poly.npoints; k++) {
                rt.incrementCounter()
                rt.addValue("name", basename)
                rt.addValue("roi",  names[i])
                rt.addValue("z",    slices[i])
                rt.addValue("x",    poly.xpoints[k] * pw)
                rt.addValue("y",    poly.ypoints[k] * ph)
            }
        }
        rt.save(path)
    }

    /**
     * Write ROIs as an ImageJ .zip, the same thing roiManager("Save") produces,
     * but via RoiEncoder directly so no ROI Manager (and so no display) is needed.
     */
    /**
     * Read an ImageJ ROI zip back, without the ROI Manager (so it works headless).
     *
     * The name each ROI was saved under is restored onto the Roi, since that name
     * is what ties an ROI to its row in the measurement table.
     *
     * @return the ROIs, in the order the zip lists them
     */
    static List<Roi> loadRoiZip(String path) {
        def file = new File(path)
        if (!file.isFile()) throw new IllegalArgumentException("no such ROI zip: ${file}")
        def rois = []
        def zis = new ZipInputStream(new BufferedInputStream(new FileInputStream(file)))
        try {
            def entry
            while ((entry = zis.getNextEntry()) != null) {
                if (!entry.getName().toLowerCase().endsWith(".roi")) continue
                // NB: read the entry with an explicit loop. Groovy's `stream.bytes`
                //     closes the stream it reads, which would end the zip after the
                //     first entry, and InputStream.readAllBytes() needs Java 9 --
                //     Fiji may still be on 8.
                def buf = new ByteArrayOutputStream()
                byte[] chunk = new byte[8192]
                int n
                while ((n = zis.read(chunk)) > 0) buf.write(chunk, 0, n)
                def roi = RoiDecoder.openFromByteArray(buf.toByteArray())
                if (roi != null) {
                    roi.setName(entry.getName().replaceFirst(/(?i)\.roi$/, ""))
                    rois << roi
                }
            }
        } finally {
            zis.close()
        }
        return rois
    }

    static void saveRoiZip(List<Roi> rois, List<String> names, String path) {
        def zos = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(path)))
        def dos = new DataOutputStream(new BufferedOutputStream(zos))
        def re  = new RoiEncoder(dos)
        try {
            rois.eachWithIndex { roi, i ->
                zos.putNextEntry(new ZipEntry(names[i] + ".roi"))
                re.write(roi)
                dos.flush()
            }
        } finally {
            dos.close()
        }
    }

    /**
     * Measure each ROI in each requested channel and save the Results table.
     *
     * NB: iterates the channels actually supplied. The macro used only the
     * LENGTH of its channel array and measured channels 1..N regardless.
     * Uses imp.setRoi() rather than the ROI Manager, so this is headless-safe.
     */
    static void measureRois(ImagePlus imp, List<Roi> rois, List<Integer> slices,
                            List<Integer> channels, String path, boolean resetAfter) {
        // NB: do NOT route this through IJ.run(imp, "Measure") and the global
        //     Results table -- each call overwrote the previous one rather than
        //     appending, leaving a single row for the last channel measured.
        //     Analyzer writing into a table we own is deterministic instead.
        def rt = new ResultsTable()
        rt.showRowNumbers(false)
        rt.setPrecision(3)
        int meas = Analyzer.getMeasurements()   // whatever Set Measurements configured
        channels.each { int ch ->
            rois.eachWithIndex { roi, i ->
                imp.setPosition(ch, slices[i], 1)
                imp.setRoi(roi)
                new Analyzer(imp, meas, rt).measure()
            }
        }
        imp.deleteRoi()
        rt.save(path)
    }

    /**
     * Write the run configuration next to the results, so a set of outputs
     * always carries the parameters that produced it. Two columns, tab
     * separated, so it is readable by eye and by read.table() alike.
     */
    static void saveRunConfig(Map<String, Object> params, String path) {
        def sb = new StringBuilder("parameter\tvalue\n")
        params.each { k, v -> sb.append(k).append("\t").append(v == null ? "" : v.toString()).append("\n") }
        new File(path).setText(sb.toString(), "UTF-8")
    }

    /**
     * Repository version, read from the VERSION file at the repo root.
     *
     * Versions are git tags, not per-file headers, but a results directory still
     * has to say what produced it -- so the one string that the tag also names
     * is read at run time and recorded in the run config. Non-fatal: a script
     * copied out of the repo still runs, it just cannot name its version.
     */
    static String repoVersion(String libDir) {
        try {
            // libDir is scripts/groovy; the VERSION file sits two levels up.
            def f = new File(new File(libDir).getParentFile().getParentFile(), "VERSION")
            if (f.exists()) return f.getText("UTF-8").trim()
        } catch (ignored) { }
        return "unknown"
    }

    /** Identifier for output filenames: a token from the slice label, else the title. */
    static String resolveImageId(ImagePlus imp, String pattern) {
        String label = imp.getStack().getSliceLabel(imp.getCurrentSlice()) ?: ""
        if (pattern && label) {
            def hit = label.split("/").find { it.contains(pattern) }
            if (hit) return sanitize(hit)
        }
        return sanitize(imp.getTitle())
    }

    static String sanitize(String s) {
        return (s ?: "").replaceAll(/(?i)\.(tif|tiff|lif|lifext|czi|nd2)$/, "")
                        .replaceAll(/[\/\\:\*\?"<>\|]/, "_")
                        .trim()
    }
}
