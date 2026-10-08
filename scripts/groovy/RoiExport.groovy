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
     * Outline coordinate table: name, roi, t, z, x, y -- tab separated, one row
     * per polygon vertex. Matches the format read by scripts/R/read_fiji_result.r.
     * x scales by pixelWidth and y by pixelHeight (the macro used pixelWidth for
     * both, which was wrong on anisotropic pixels).
     *
     * `t` is written for every image, one frame being t = 1: one file shape,
     * rather than a column that comes and goes. Counted from 1, like z.
     *
     * @param ts the frame of each ROI, parallel to rois
     */
    static void saveOutlineCoords(ImagePlus imp, List<Roi> rois, List<String> names,
                                  List<Integer> slices, List<Integer> ts,
                                  String seriesId, String path) {
        saveOutlineCoords(imp.getCalibration(), rois, names, slices, ts, seriesId, path)
    }

    /** The same, from the calibration alone: a streamed series has no whole image to ask. */
    static void saveOutlineCoords(ij.measure.Calibration cal, List<Roi> rois, List<String> names,
                                  List<Integer> slices, List<Integer> ts,
                                  String seriesId, String path) {
        double pw = cal.pixelWidth, ph = cal.pixelHeight
        def rt = new ResultsTable()
        rt.showRowNumbers(false)
        rt.setPrecision(3)
        rois.eachWithIndex { roi, i ->
            def poly = roi.getPolygon()
            for (int k = 0; k < poly.npoints; k++) {
                rt.incrementCounter()
                rt.addValue("name", seriesId)
                rt.addValue("roi",  names[i])
                rt.addValue("t",    ts[i])
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

    /**
     * Written under a temporary name and renamed on success. A zip that throws
     * part-way otherwise stays behind as a 189-byte file that loadRoiZip()
     * reads as a valid, shorter set of ROIs -- and a frame loop gives every
     * write more chances to throw. The rename is within one directory, so it
     * replaces the old file or leaves it alone, never half of either.
     */
    static void saveRoiZip(List<Roi> rois, List<String> names, String path) {
        def dest = new File(path)
        def tmp  = new File(dest.getParentFile(), dest.getName() + ".part")
        def zos = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(tmp)))
        def dos = new DataOutputStream(new BufferedOutputStream(zos))
        def re  = new RoiEncoder(dos)
        try {
            rois.eachWithIndex { roi, i ->
                zos.putNextEntry(new ZipEntry(names[i] + ".roi"))
                re.write(roi)
                dos.flush()
            }
            dos.close()   // writes the zip's directory; a failure here is a failure too
        } catch (Throwable t) {
            try { dos.close() } catch (Throwable ignored) { }
            tmp.delete()
            throw t
        }
        replaceWith(tmp, dest)
    }

    /** Move a finished temporary file over its destination, in one directory. */
    static void replaceWith(File tmp, File dest) {
        if (!tmp.renameTo(dest)) {
            // renameTo() will not replace an existing file on every platform.
            dest.delete()
            if (!tmp.renameTo(dest)) {
                tmp.delete()
                throw new IOException("could not move " + tmp + " to " + dest)
            }
        }
    }

    // --- joining a series' frames ---------------------------------------------
    //
    // A series is analysed a frame at a time, and each frame's tables are staged
    // on disk as soon as the frame is done (NucleusPipeline): a crash at frame
    // 90 of 96 must not lose the 89 before it. The series' files are then these
    // parts joined in t order -- as text, never by reading the tables back into
    // ImageJ, so a joined file holds exactly the bytes each frame wrote.

    /**
     * Tables sharing one header: the header once, then every part's rows in the
     * order given. Parts that do not exist are skipped -- a frame that found no
     * nucleoli wrote no nucleolus table.
     *
     * @param renumber the first column is ImageJ's row number, which every part
     *                 restarts at 1; rewritten to run 1..n across the join.
     * @return the number of rows written; nothing is written when it is 0
     */
    static int joinTables(List<File> parts, File dest, boolean renumber) {
        def present = parts.findAll { it.isFile() }
        if (!present) return 0
        def tmp = new File(dest.getParentFile(), dest.getName() + ".part")
        int n = 0
        try {
            tmp.withWriter("UTF-8") { w ->
                String header = null
                present.each { File f ->
                    def lines = f.readLines("UTF-8")
                    if (!lines) return
                    if (header == null) {
                        header = lines[0]
                        w.write(header); w.write("\n")
                    } else if (lines[0] != header) {
                        throw new IllegalStateException(
                            f.getName() + " has different columns from the frames before it:\n  " +
                            lines[0] + "\n  " + header)
                    }
                    for (int i = 1; i < lines.size(); i++) {
                        def l = lines[i]
                        if (renumber) {
                            int tab = l.indexOf("\t")
                            l = (n + 1) + (tab < 0 ? "" : l.substring(tab))
                        }
                        w.write(l); w.write("\n")
                        n++
                    }
                }
            }
        } catch (Throwable t) {
            tmp.delete()
            throw t
        }
        replaceWith(tmp, dest)
        return n
    }

    /**
     * ROI zips joined by copying their entries' bytes, in the order given.
     * Nothing is decoded and re-encoded, so a joined ROI is the ROI the frame
     * saved. A repeated entry name is refused -- the zip format would keep both
     * and ImageJ would open only one.
     *
     * @return the number of ROIs written; nothing is written when it is 0
     */
    static int joinRoiZips(List<File> parts, File dest) {
        def present = parts.findAll { it.isFile() }
        if (!present) return 0
        def tmp = new File(dest.getParentFile(), dest.getName() + ".part")
        def seen = new HashSet<String>()
        def zos = new ZipOutputStream(new BufferedOutputStream(new FileOutputStream(tmp)))
        try {
            byte[] chunk = new byte[8192]
            present.each { File f ->
                def zis = new ZipInputStream(new BufferedInputStream(new FileInputStream(f)))
                try {
                    def e
                    while ((e = zis.getNextEntry()) != null) {
                        if (!seen.add(e.getName())) {
                            throw new IllegalStateException(e.getName() + " is in two frames' ROI zips")
                        }
                        zos.putNextEntry(new ZipEntry(e.getName()))
                        int k
                        while ((k = zis.read(chunk)) > 0) zos.write(chunk, 0, k)
                        zos.closeEntry()
                    }
                } finally {
                    zis.close()
                }
            }
            zos.close()
        } catch (Throwable t) {
            try { zos.close() } catch (Throwable ignored) { }
            tmp.delete()
            throw t
        }
        replaceWith(tmp, dest)
        return seen.size()
    }

    /** A measurement table for measureInto() to append to; save it with saveMeasurements(). */
    static ResultsTable newMeasurementTable() {
        def rt = new ResultsTable()
        rt.showRowNumbers(false)
        rt.setPrecision(3)
        return rt
    }

    /**
     * Measure each ROI in each requested channel, appending to `rt`.
     *
     * Every row also gets `roi`, `z`, `t` and `ch`, WRITTEN BY US from what is
     * known, never parsed back out of the Label. ImageJ's own Ch/Slice/Frame
     * cannot be trusted -- which appear, and what Slice holds, depends on the
     * image's shape (note/time_series_plan.md §5.3: on 1c 1z 4t, Slice is the
     * TIME) -- so `stack` is no longer in Set Measurements and these are the
     * only position columns. All count from 1. saveMeasurements() writes them
     * straight after the Label, ahead of ImageJ's measurements.
     *
     * NB: iterates the channels actually supplied. The macro used only the
     * LENGTH of its channel array and measured channels 1..N regardless.
     * Uses imp.setRoi() rather than the ROI Manager, so this is headless-safe.
     *
     * @param imp one frame: position t is only recorded, never set
     * @param t   that frame's number in the series, from 1
     */
    static void measureInto(ImagePlus imp, List<Roi> rois, List<String> names,
                            List<Integer> slices, List<Integer> channels,
                            ResultsTable rt, int t) {
        // NB: do NOT route this through IJ.run(imp, "Measure") and the global
        //     Results table -- each call overwrote the previous one rather than
        //     appending, leaving a single row for the last channel measured.
        //     Analyzer writing into a table we own is deterministic instead.
        int meas = Analyzer.getMeasurements()   // whatever Set Measurements configured
        channels.each { int ch ->
            rois.eachWithIndex { roi, i ->
                imp.setPosition(ch, slices[i], 1)
                imp.setRoi(roi)
                new Analyzer(imp, meas, rt).measure()
                rt.addValue("roi", names[i])
                rt.addValue("z",   slices[i])
                rt.addValue("t",   t)
                rt.addValue("ch",  ch)
            }
        }
        imp.deleteRoi()
    }

    /** The columns a measurement table leads with, after Fiji's row number. */
    static final List<String> MEASUREMENT_LEAD = ["Label", "roi", "z", "t", "ch"]

    /**
     * Save a measurement table with the identity first: row number, Label, roi,
     * z, t, ch, then ImageJ's measurements in ImageJ's order.
     *
     * ImageJ cannot be asked for that order: its standard measurements have
     * fixed slots that precede any column we add, so ours always came last.
     * The table is therefore saved by ImageJ, as before, and its columns
     * reordered -- every cell keeps ImageJ's own formatting, so only the order
     * changes. The R reader finds columns by name, never by position.
     */
    static void saveMeasurements(ResultsTable rt, String path) {
        def dest = new File(path)
        def tmp  = new File(dest.getParentFile(), dest.getName() + ".part")
        try {
            rt.save(tmp.getPath())
            def lines = tmp.readLines("UTF-8")
            if (lines.isEmpty()) { tmp.renameTo(dest); return }
            def head = lines[0].split("\t", -1).toList()
            // ImageJ's unnamed row-number column, when present, stays first.
            def order = []
            if (head[0].trim().isEmpty()) order << 0
            MEASUREMENT_LEAD.each { String c -> int i = head.indexOf(c); if (i >= 0) order << i }
            (0..<head.size()).each { int i -> if (!order.contains(i)) order << i }
            def sb = new StringBuilder()
            lines.each { String l ->
                def f = l.split("\t", -1)
                sb.append(order.collect { int i -> i < f.length ? f[i] : "" }.join("\t")).append("\n")
            }
            dest.setText(sb.toString(), "UTF-8")
        } finally {
            tmp.delete()
        }
    }

    /**
     * One row per frame of what the nucleus threshold did, and what detection
     * kept: the per-frame half of what _config.txt records once per image.
     * Written for every image, one frame being one row, so every results folder
     * has the same files. The nucleolus threshold is not here: it is chosen per
     * nucleus per slice.
     *
     * @param rows maps with the keys of THRESHOLD_STATS_COLUMNS
     */
    static final List<String> THRESHOLD_STATS_COLUMNS = [
        "t", "nucleus_threshold_used", "nucleus_histogram_divisor", "nucleus_mask_pct", "nucleus_circ_rejected",
        "nucleus_count", "nucleolus_count"]

    static void saveThresholdStats(List<Map> rows, String path) {
        def sb = new StringBuilder(THRESHOLD_STATS_COLUMNS.join("\t")).append("\n")
        rows.each { r ->
            sb.append(THRESHOLD_STATS_COLUMNS.collect { k -> r[k] == null ? "" : r[k].toString() }
                                             .join("\t")).append("\n")
        }
        new File(path).setText(sb.toString(), "UTF-8")
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

    /**
     * Bio-Formats titles a series "<file>.lif - <series name>". Keep the series
     * name. Anchored on a real image extension followed by " - ", so a title
     * that merely contains a dash is left alone.
     */
    static String stripFileTitle(String title) {
        return (title ?: "").replaceFirst(/(?i)^.*\.(?:tif|tiff|lif|lifext|czi|nd2)\s+-\s+/, "")
    }

    /**
     * The series id an interactive run uses when none was typed: the image's
     * title, less the file part Bio-Formats puts in front of a series name.
     *
     * The title, not the slice label. A token search through the label (the
     * `position_pattern` of v0.6.0 and before) is retired: it was a guess at
     * where one acquisition software hid the name, and an id the operator
     * cannot see being chosen is a poor thing to name every output file after.
     * The raw title is logged, so a surprising id can be traced from the log.
     */
    static String seriesIdFromTitle(ImagePlus imp) {
        String rawTitle = imp.getTitle() ?: ""
        String out = sanitize(stripFileTitle(rawTitle))
        if (!out) {
            throw new IllegalArgumentException(
                "The image has no title to take a series id from. Type one into 'Series id'.")
        }
        IJ.log("  series id: '" + out + "'  [from the title]")
        IJ.log("      title      >>>" + rawTitle + "<<<")
        return out
    }

    /**
     * A series id that was GIVEN -- typed into the dialog, or read from the
     * series table -- refused rather than altered when sanitize() would change
     * it. It names files and is written into a tab-separated table, so a space
     * or a slash cannot stand; but rewriting it quietly would produce output
     * under a name nobody asked for, and the next step looking for the name
     * that WAS asked for would find nothing.
     */
    static String checkSeriesId(String id) {
        String clean = sanitize(id)
        if (!clean) {
            throw new IllegalArgumentException("The series id is blank.")
        }
        if (clean != id) {
            throw new IllegalArgumentException(
                "The series id '" + id + "' cannot name a file or sit in a tab-separated " +
                "table as it is (spaces, slashes, : * ? \" < > | and a trailing image " +
                "extension are not allowed). '" + clean + "' would be accepted.")
        }
        return id
    }

    static String sanitize(String s) {
        // NB: whitespace collapses to "_". The result becomes both a filename
        //     and the `name` column of every outline row -- and that table is
        //     tab-separated, so a value containing a space makes read.table()
        //     see more fields than the header unless the reader names the
        //     separator. A Leica series called "Image005 Denoised" produced
        //     exactly that.
        return (s ?: "").replaceAll(/(?i)\.(tif|tiff|lif|lifext|czi|nd2)$/, "")
                        .replaceAll(/[\/\\:\*\?"<>\|]/, "_")
                        .trim()
                        .replaceAll(/\s+/, "_")
    }
}
