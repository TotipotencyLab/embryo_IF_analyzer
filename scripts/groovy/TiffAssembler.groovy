// TiffAssembler.groovy
//
// Manifest rows -> one TIFF per output. The read side is LuxendoFile; this side
// knows nothing about Luxendo beyond the manifest columns, so a second format
// only has to produce a manifest.
//
// TWO WRITERS, AND THE DEFAULT IS THE ONE FIJI OPENS NATIVELY.
//
//   tiff     ij.io.FileSaver -- an ImageJ hyperstack TIFF. Measured: IJ.openImage
//            gives back nC, nZ, pixel size AND z step exactly, and our own
//            SampleSheet.inspect() reads the same. Capped at 4 GB.
//   bigtiff  OMETiffWriter with setBigTiff(true). No size cap, and Bio-Formats
//            reads it with calibration intact -- but ImageJ's own opener does
//            NOT handle it as cleanly (measured: an OME-TIFF came back as
//            nC=1, nZ=6 with the z step lost, because a name ending .tif is
//            taken by ImageJ's own decoder before Bio-Formats sees it).
//            So bigtiff is the degraded path, offered rather than defaulted to.
//
// That asymmetry is the whole reason the default is one file per timepoint.

import ij.ImagePlus
import ij.ImageStack
import ij.process.ShortProcessor
import ij.io.FileSaver
import java.util.zip.CRC32
import loci.formats.MetadataTools
import loci.formats.FormatTools
import loci.formats.out.OMETiffWriter
import ome.units.quantity.Length
import ome.units.UNITS

class TiffAssembler {

    /**
     * Pixel payload above which Bio-Formats abandons classic TIFF.
     *
     * The literal in loci.formats.out.TiffWriter, not 2^32: 3.896 GiB, keeping
     * ~106 MiB below the 32-bit offset limit for IFDs and metadata.
     */
    static final long CLASSIC_TIFF_MAX = 4183818240L

    static final String TIFF    = "tiff"
    static final String BIGTIFF = "bigtiff"
    static final List<String> FORMATS = [TIFF, BIGTIFF]

    Object LFC          // LuxendoFile class
    Object RX           // RoiExport, for repoVersion()
    String libDir

    static TiffAssembler load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader()
        def a = new TiffAssembler()
        a.libDir = libDir
        a.LFC = gcl.parseClass(new File(dir, "LuxendoFile.groovy"))
        a.RX  = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        return a
    }

    /**
     * Bytes of pixel payload one output will hold. Exact, from the manifest --
     * no trial write, so a whole run can be judged before anything is written.
     */
    static long predictBytes(List<Map> rows, int resize = 1) {
        if (!rows) return 0L
        def r = rows[0]
        long x = scaled(r.size_x as int, resize)
        long y = scaled(r.size_y as int, resize)
        return 2L * x * y * (r.size_z as int) * rows.size()
    }

    static int scaled(int n, int resize) {
        return (resize <= 1) ? n : Math.max(1, (int) Math.floor(n / (double) resize))
    }

    static boolean fitsClassicTiff(long bytes) { return bytes < CLASSIC_TIFF_MAX }

    /** Validate a format name in code: a `#@ String` with choices is NOT validated. */
    static String checkFormat(String f) {
        def v = (f ?: TIFF).toString().trim().toLowerCase()
        if (!FORMATS.contains(v)) {
            throw new IllegalArgumentException(
                "format must be one of " + FORMATS.join(", ") + "; got >>>" + f + "<<<")
        }
        return v
    }

    /** `true`/`yes`/`1` and friends, exactly as BatchRunner.isIncluded() reads them. */
    static boolean isIncluded(Object v) {
        if (v == null) return true
        def s = v.toString().trim().toLowerCase()
        if (!s) return true
        return s in ["true", "yes", "1", "t", "y"]
    }

    /** Output filename, carrying a token when the pixels are not full resolution. */
    static String outputName(String base, int resize, String format) {
        def stem = base.replaceAll(/(?i)\.(ome\.)?tiff?$/, "")
        if (resize > 1) stem += "_ds" + resize
        return stem + (format == BIGTIFF ? ".ome.tif" : ".tif")
    }

    /**
     * Assemble one output from its channel rows.
     *
     * Planes are read one at a time and scaled as they are read -- never build
     * the stack at full size and resize afterwards, which would hold two whole
     * copies of an image that is already ~1 GB. Same rule as buildMask's
     * helpers.
     *
     * @return a summary row; `status` is written | skipped | failed
     */
    Map assembleOne(List<Map> rows, File srcRoot, File outDir, Map opts = [:]) {
        String format = checkFormat(opts.format as String)
        int resize    = Math.max(1, (opts.resize ?: 1) as int)
        def first     = rows[0]
        def name      = outputName(first.target_output_path as String, resize, format)
        def summary   = [output_path: name, series_id: first.series_id,
                         position_id: first.position_id, t: first.t,
                         channels: rows.size(), format: format, resize: resize,
                         status: "", reason: "", bytes: 0L, checksum: ""]

        if (!rows.every { isIncluded(it.include) }) {
            // All rows of one output must agree; a half-included output is a
            // question, not an instruction.
            if (rows.any { isIncluded(it.include) }) {
                summary.status = "failed"
                summary.reason = "rows of this output disagree on include"
                return summary
            }
            summary.status = "skipped"
            summary.reason = "include=false"
            return summary
        }

        // The name is the whole check, and it is enough BECAUSE the name carries
        // the two things that change the pixels: _ds<N> and the extension. It
        // is not enough on its own to say the file is complete, so the
        // provenance file -- written only after a successful assemble -- has to
        // be there too. A run killed mid-write leaves the .tif without it and
        // is redone rather than silently kept.
        if (opts.skipExisting) {
            def existing = new File(outDir, name)
            def prov     = new File(outDir, name.replaceAll(/\.(ome\.)?tiff?$/, "") + "_gather.txt")
            if (existing.isFile() && prov.isFile()) {
                summary.status = "skipped"
                summary.reason = "already assembled"
                summary.bytes  = existing.length()
                return summary
            }
        }

        long predicted = predictBytes(rows, resize)
        if (format == TIFF && !fitsClassicTiff(predicted)) {
            // One row's failure must not cost the rest of the run.
            summary.status = "failed"
            summary.reason = "needs " + predicted + " bytes, over the classic TIFF limit of " +
                             CLASSIC_TIFF_MAX + "; use format=bigtiff"
            return summary
        }

        int nz = first.size_z as int
        int nc = rows.size()
        int outX = scaled(first.size_x as int, resize)
        int outY = scaled(first.size_y as int, resize)

        // Channels in manifest order, which is the source metadata's own channel
        // index -- never directory order, which the .ims files show can differ.
        def ordered = rows.sort(false) { a, b -> (a.channel as int) <=> (b.channel as int) }
        def files = ordered.collect { new File(srcRoot, it.source_path as String) }
        def open  = []
        def crc   = new CRC32()
        try {
            files.each { open << LFC.open(it) }
            open.eachWithIndex { lf, i ->
                if (lf.sizeZ != nz) {
                    throw new IllegalStateException(
                        name + ": channel " + i + " has " + lf.sizeZ + " slices, expected " + nz)
                }
            }
            // XYCZT: channel fastest, matching setDimensions(c, z, t) below.
            def stack = new ImageStack(outX, outY)
            for (int z = 0; z < nz; z++) {
                for (int c = 0; c < nc; c++) {
                    short[] px = open[c].plane(z)
                    def sp = new ShortProcessor(first.size_x as int, first.size_y as int, px, null)
                    if (resize > 1) sp = (ShortProcessor) sp.resize(outX, outY, true)
                    // Checksum what is WRITTEN, not what was read. At resize=1
                    // the two are the same; above it they are not, and a
                    // checksum of the source could not tell a resize bug from a
                    // correct downscale.
                    crc.update(toBytes((short[]) sp.getPixels()))
                    stack.addSlice(sliceLabel(ordered[c], z, nz, c, nc), sp)
                }
            }
            def imp = new ImagePlus(name.replaceAll(/\.(ome\.)?tiff?$/, ""), stack)
            imp.setDimensions(nc, nz, 1)
            applyCalibration(imp, first, resize)

            outDir.mkdirs()
            def out = new File(outDir, name)
            if (format == BIGTIFF) writeBigTiff(imp, out) else writeClassic(imp, out)
            imp.close(); imp.flush()

            summary.status   = "written"
            summary.bytes    = out.length()
            summary.checksum = Long.toHexString(crc.getValue())
        } catch (Throwable e) {
            summary.status = "failed"
            summary.reason = e.getClass().getSimpleName() + ": " + e.getMessage()
        } finally {
            open.each { try { it.close() } catch (ignored) { } }
        }
        return summary
    }

    /**
     * Every output in the manifest, one row's failure never costing the rest.
     *
     * The same contract as BatchRunner.runEach(): the summary is RECTANGULAR
     * whatever happened, so a skipped or failed output still has a row saying
     * which and why. A run that half worked has to be legible.
     */
    List<Map> assembleAll(List<Map> rows, File srcRoot, File outDir, Map opts = [:], Closure log = null) {
        String format = checkFormat(opts.format as String)
        int resize    = Math.max(1, (opts.resize ?: 1) as int)
        boolean verify = (opts.verify ?: false) as boolean

        def groups = groupByOutput(rows)
        log?.call("  " + groups.size() + " output(s), format=" + format +
                  (resize > 1 ? (", resize=" + resize) : "") +
                  (opts.skipExisting ? ", skipping ones already assembled" : "") +
                  (verify ? ", verifying" : ""))

        // Say what will not fit BEFORE writing anything, so a person can choose
        // the format rather than discover it after twenty minutes of writing.
        if (format == TIFF) {
            // Only outputs this run will actually attempt. Warning about a
            // position the operator already set include=false on is noise, and
            // noise is what stops warnings being read.
            def over = groups.findAll { k, v ->
                v.every { isIncluded(it.include) } && !fitsClassicTiff(predictBytes(v, resize))
            }
            if (over) {
                log?.call("WARNING: " + over.size() + " output(s) exceed the classic TIFF limit " +
                          "and will be recorded as failed; use format=bigtiff:")
                over.each { k, v ->
                    log?.call("           " + k + "  " + predictBytes(v, resize) + " bytes")
                }
            }
        }

        def summaries = []
        groups.each { String out, List<Map> group ->
            def s = assembleOne(group, srcRoot, outDir, opts)
            if (s.status == "written") {
                writeProvenance(outDir, s, group, srcRoot)
                if (verify) {
                    def v = verifyOne(outDir, s)
                    s.verified = v.ok ? "yes" : "NO"
                    if (!v.ok) {
                        s.status = "failed"
                        s.reason = "verification: " + v.reason
                    }
                } else {
                    s.verified = ""
                }
            } else {
                s.verified = ""
            }
            log?.call(String.format("  %-9s %-44s %s", s.status, s.output_path,
                                    s.reason ?: (s.bytes + " bytes, crc " + s.checksum)))
            summaries << s
        }
        return summaries
    }

    /** Manifest rows grouped into outputs, in manifest order. */
    static Map<String, List<Map>> groupByOutput(List<Map> rows) {
        def m = new LinkedHashMap()
        rows.sort(false) { a, b ->
            (a.position_id <=> b.position_id) ?: (a.t <=> b.t) ?: (a.channel <=> b.channel)
        }.each { r -> m.computeIfAbsent(r.target_output_path, { [] }) << r }
        return m
    }

    /**
     * What made this file, written beside it.
     *
     * A converted TIFF has no way back to the .lux.h5 it came from, and a
     * results folder is read long after, on another machine. Same reason
     * _config.txt records VERSION.
     */
    void writeProvenance(File outDir, Map summary, List<Map> rows, File srcRoot) {
        def sb = new StringBuilder("parameter\tvalue\n")
        def put = { k, v -> sb.append(k).append("\t").append(v == null ? "" : v.toString()).append("\n") }
        put("output_path",   summary.output_path)
        put("series_id",     summary.series_id)
        put("position_id",   summary.position_id)
        put("t",             summary.t)
        put("format",        summary.format)
        put("resize",        summary.resize)
        put("pixel_checksum", summary.checksum)
        put("output_bytes",  summary.bytes)
        put("source_root",   srcRoot?.getAbsolutePath())
        put("gatherer_version", RX.repoVersion(libDir))
        put("written_at",    new Date().format("yyyy-MM-dd'T'HH:mm:ss"))
        rows.eachWithIndex { r, i ->
            put("channel_" + r.channel + "_name",   r.channel_name)
            put("channel_" + r.channel + "_source", r.source_path)
            put("channel_" + r.channel + "_bytes",  r.source_bytes)
        }
        new File(outDir, summary.output_path.replaceAll(/\.(ome\.)?tiff?$/, "") + "_gather.txt")
            .setText(sb.toString(), "UTF-8")
    }

    /**
     * Read the written file back and check its pixels are what we wrote.
     *
     * "39 slices, 3 channels" passes happily while the channels are
     * transposed. Recomputing the checksum over the planes on disk is the only
     * check here that can fail for the right reason.
     */
    Map verifyOne(File outDir, Map summary) {
        def f = new File(outDir, summary.output_path as String)
        if (!f.isFile()) return [ok: false, reason: "output missing"]
        def imp = ij.IJ.openImage(f.getAbsolutePath())
        if (imp == null) {
            // OME-TIFF does not come back through ImageJ's own decoder; fall
            // back to Bio-Formats rather than calling it a failure.
            return verifyWithBioFormats(f, summary)
        }
        try {
            def crc = new CRC32()
            def stack = imp.getStack()
            for (int i = 1; i <= stack.getSize(); i++) {
                crc.update(toBytes((short[]) stack.getProcessor(i).getPixels()))
            }
            def got = Long.toHexString(crc.getValue())
            return (got == summary.checksum) ? [ok: true, reason: ""]
                 : [ok: false, reason: "pixel checksum " + got + ", expected " + summary.checksum]
        } finally {
            imp.close(); imp.flush()
        }
    }

    private Map verifyWithBioFormats(File f, Map summary) {
        def r = Class.forName("loci.formats.ImageReader").newInstance()
        try {
            r.setId(f.getAbsolutePath())
            def crc = new CRC32()
            for (int i = 0; i < r.getImageCount(); i++) crc.update(r.openBytes(i))
            def got = Long.toHexString(crc.getValue())
            return (got == summary.checksum) ? [ok: true, reason: ""]
                 : [ok: false, reason: "pixel checksum " + got + ", expected " + summary.checksum]
        } catch (Throwable e) {
            return [ok: false, reason: "could not re-read: " + e.getMessage()]
        } finally {
            try { r.close() } catch (ignored) { }
        }
    }

    /** The slice label ImageJ puts on a hyperstack plane, plus the channel's name. */
    static String sliceLabel(Map row, int z, int nz, int c, int nc) {
        def sb = new StringBuilder()
        if (nc > 1) sb.append("c:").append(c + 1).append("/").append(nc).append(" ")
        if (nz > 1) sb.append("z:").append(z + 1).append("/").append(nz).append(" ")
        sb.append("- ").append(row.channel_name ?: ("channel_" + row.channel))
        return sb.toString()
    }

    /**
     * Calibration, scaled by the resize factor.
     *
     * ⚠️ HALVE THE PIXELS AND THE PIXEL SIZE MUST DOUBLE. Miss this and every
     * area is out by the square of the factor while the image looks perfect --
     * exactly the silent failure this repo keeps meeting. pixelDepth is left
     * alone: resizing is in x and y only.
     */
    static void applyCalibration(ImagePlus imp, Map row, int resize) {
        def cal = imp.getCalibration()
        if (row.pixel_width  != null) cal.pixelWidth  = (row.pixel_width  as double) * resize
        if (row.pixel_height != null) cal.pixelHeight = (row.pixel_height as double) * resize
        // blank for a single plane, never a default of 1.0
        if (row.pixel_depth != null)  cal.pixelDepth  = row.pixel_depth as double
        if (row.pixel_unit) cal.setUnit(row.pixel_unit as String)
    }

    static void writeClassic(ImagePlus imp, File out) {
        if (!new FileSaver(imp).saveAsTiffStack(out.getAbsolutePath())) {
            throw new IllegalStateException("FileSaver refused to write " + out.getName())
        }
    }

    /**
     * BigTIFF through Bio-Formats.
     *
     * ⚠️ setCanDetectBigTiff(false) on BOTH paths. The writers default it to
     * TRUE, and above the ceiling they log "Switching to BigTIFF (by file size)"
     * and carry on -- so a run that asked for classic TIFF would silently get a
     * BigTIFF. Here the format is chosen, so the library must not choose again.
     */
    void writeBigTiff(ImagePlus imp, File out) {
        def cal = imp.getCalibration()
        def meta = MetadataTools.createOMEXMLMetadata()
        MetadataTools.populateMetadata(
            meta, 0, imp.getTitle(), true, "XYCZT",
            FormatTools.getPixelTypeString(FormatTools.UINT16),
            imp.getWidth(), imp.getHeight(),
            imp.getNSlices(), imp.getNChannels(), imp.getNFrames(), 1)

        // Calibration is NOT best-effort here. A stack written without it
        // measures in pixels, and on a 0.208 um pixel every area is ~23x wrong
        // with nothing on screen to say so.
        meta.setPixelsPhysicalSizeX(new Length(cal.pixelWidth  as Double, UNITS.MICROMETER), 0)
        meta.setPixelsPhysicalSizeY(new Length(cal.pixelHeight as Double, UNITS.MICROMETER), 0)
        if (imp.getNSlices() > 1 && cal.pixelDepth > 0) {
            meta.setPixelsPhysicalSizeZ(new Length(cal.pixelDepth as Double, UNITS.MICROMETER), 0)
        }

        def w = new OMETiffWriter()
        try {
            w.setMetadataRetrieve(meta)
            w.setBigTiff(true)
            w.setCanDetectBigTiff(false)
            w.setId(out.getAbsolutePath())
            def stack = imp.getStack()
            for (int i = 1; i <= stack.getSize(); i++) {
                w.saveBytes(i - 1, toBytes((short[]) stack.getProcessor(i).getPixels()))
            }
        } finally {
            try { w.close() } catch (ignored) { }
        }
    }

    /** uint16 little-endian, the order both writers use. */
    static byte[] toBytes(short[] px) {
        byte[] b = new byte[px.length * 2]
        for (int i = 0; i < px.length; i++) {
            b[2 * i]     = (byte) (px[i] & 0xFF)
            b[2 * i + 1] = (byte) ((px[i] >> 8) & 0xFF)
        }
        return b
    }
}
