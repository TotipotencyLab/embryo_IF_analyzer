// TiffAssembler.groovy
//
// Manifest rows -> one TIFF per output. The read side is LuxendoFile; this side
// knows nothing about Luxendo beyond the sources-table columns, so a second
// format only has to produce a sources table.
//
// TWO WRITERS, AND THE DEFAULT IS THE ONE FIJI OPENS NATIVELY.
//
//   tiff     ij.io.FileSaver -- an ImageJ hyperstack TIFF. Measured: IJ.openImage
//            gives back nC, nZ, pixel size AND z step exactly, and our own
//            SeriesSheet.inspect() reads the same. Capped at 4 GB.
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

    /** Scale, as a percentage of the original. 100 is full resolution. */
    static final int FULL = 100

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
     * Bytes of pixel payload one output will hold. Exact, from the sources table --
     * no trial write, so a whole run can be judged before anything is written.
     */
    static long predictBytes(List<Map> rows, int scalePercent = FULL) {
        if (!rows) return 0L
        def r = rows[0]
        long x = scaled(r.size_x as int, scalePercent)
        long y = scaled(r.size_y as int, scalePercent)
        // One row per (channel, timepoint), so gathering frames needs no special
        // case here: the row count already carries both axes.
        return 2L * x * y * (r.size_z as int) * rows.size()
    }

    /**
     * A dimension at the requested scale.
     *
     * ROUNDED, not floored. The percentage is a ratio being asked for, and
     * floor would bias every output a little small -- 33% of 2048 is 675.84,
     * which is 676 pixels, not 675.
     */
    static int scaled(int n, int scalePercent) {
        if (scalePercent >= FULL) return n
        return Math.max(1, (int) Math.round(n * scalePercent / 100.0d))
    }

    /**
     * Validate a scale in code, for the same reason checkFormat exists: a `#@`
     * parameter's bounds are a dialog affordance, not a guarantee, and nothing
     * validates a value handed in on the command line.
     *
     * Accepts 1..100 as a PERCENTAGE OF THE ORIGINAL. 100 is full resolution.
     * The upper bound is deliberate: this scales down for looking at, it does
     * not interpolate up, and an output larger than its source would be
     * invented pixels wearing a real calibration.
     */
    static int checkScalePercent(Object v) {
        if (v == null) return FULL
        def s = v.toString().trim()
        if (!s) return FULL
        int pct
        try {
            pct = (s as Double).intValue()
        } catch (Throwable e) {
            throw new IllegalArgumentException(
                "scale must be a number of percent between 1 and 100; got >>>" + v + "<<<")
        }
        if (pct < 1 || pct > FULL) {
            throw new IllegalArgumentException(
                "scale must be between 1 and 100 percent of the original; got " + pct +
                (pct > FULL ? " -- this downscales only, it does not enlarge" : ""))
        }
        return pct
    }

    static boolean fitsClassicTiff(long bytes) { return bytes < CLASSIC_TIFF_MAX }

    /**
     * The share of the heap one output may hold. assembleOne builds the whole
     * stack before writing it, so the payload has to fit in memory as well as
     * in the format -- and a gathered 96-frame position is ~94 GB, which
     * bigtiff would accept and the heap cannot. Checked from the sources table,
     * before a byte is read, rather than discovered as an OutOfMemoryError
     * after reading eight of those gigabytes over the network.
     */
    static final double HEAP_SHARE = 0.75d

    static boolean fitsHeap(long bytes) {
        return bytes <= (long) (Runtime.getRuntime().maxMemory() * HEAP_SHARE)
    }

    /**
     * A time-point selection: blank or "all" = every frame (null), otherwise
     * a comma list of time points and ranges -- "1", "1,48,96", "1-4,11".
     * Counted from 1, as sources.tsv's `t` is.
     * Validated in code, like the format and scale.
     */
    static List<Integer> parseFrames(Object v) {
        def s = (v == null) ? "" : v.toString().trim().toLowerCase()
        if (!s || s == "all") return null
        def out = new TreeSet<Integer>()
        s.split(/\s*,\s*/).each { String part ->
            def m = (part =~ /^(\d+)(?:\s*-\s*(\d+))?$/)
            if (!m) {
                throw new IllegalArgumentException(
                    "frames must be blank, 'all', or time points like 1,48,96 or 1-4; got >>>" + v + "<<<")
            }
            int a = Integer.parseInt(m[0][1] as String)
            int b = m[0][2] ? Integer.parseInt(m[0][2] as String) : a
            if (b < a) throw new IllegalArgumentException("frames: range " + part + " runs backwards")
            (a..b).each { out << it }
        }
        if (out.contains(0)) {
            // Not "no source has time point 0": that is true, and sends a person
            // looking for a missing file rather than at the count.
            throw new IllegalArgumentException(
                "frames: time points count from 1 since v0.8.0, as ImageJ shows them; got 0 in >>>" + v + "<<<")
        }
        return out.toList()
    }

    /** The output base for one frame taken out of a multi-frame series. */
    static String frameBase(String seriesId, Object t) {
        return seriesId + "_" + String.format("t%04d", t as int)
    }

    /**
     * The output base for chosen frames gathered into one file.
     *
     * Names the SELECTION, runs compressed: [2, 11, 21] is _t0002_t0011_t0021,
     * [1, 2, 3, 4] is _t0001-0004. Never the bare series id, which is what a
     * blank `frames` writes: skipExisting decides by name alone, so a file
     * holding frames 1-4 under that name would later be skipped as "already
     * assembled" when every frame was asked for. One frame is frameBase().
     * No commas: a comma in a filename is split in half by the R CLIs.
     */
    static String framesBase(String seriesId, Collection ts) {
        def sorted = ts.collect { it as int }.unique().sort()
        def parts = []
        int i = 0
        while (i < sorted.size()) {
            int j = i
            while (j + 1 < sorted.size() && sorted[j + 1] == sorted[j] + 1) j++
            parts << (j == i ? String.format("t%04d", sorted[i])
                             : String.format("t%04d-%04d", sorted[i], sorted[j]))
            i = j + 1
        }
        return seriesId + "_" + parts.join("_")
    }

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

    /**
     * Output filename, carrying a token when the pixels are not full resolution.
     *
     * `_downscale<PC>pc` reads as what it is -- 50 percent of the original --
     * where a bare number could be read as either a percentage or a divisor.
     * A downscaled file must not be mistakable for a full-resolution one by
     * anybody who meets it later without the sources table.
     */
    static String outputName(String base, int scalePercent, String format) {
        def stem = base.replaceAll(/(?i)\.(ome\.)?tiff?$/, "")
        if (scalePercent < FULL) stem += "_downscale" + scalePercent + "pc"
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
        int pct       = checkScalePercent(opts.scalePercent)
        def first     = rows[0]
        // DERIVED from series_id, not stored. target_output_path was only ever
        // series_id + ".tif", and a stored copy of a derived value is a second
        // thing to keep in step. The rows must carry series_id, which the
        // sources table does not: the caller attaches it through
        // LuxendoScan.withSeriesId(). `base` is set only when one frame is taken out of a multi-frame
        // series, and is then series_id + _t<TTTT> -- see groupByOutput.
        def name      = outputName((opts.base ?: first.series_id) as String, pct, format)
        // One row per (channel, time point). A whole series is many time
        // points; one chosen frame is 1, and every loop below collapses.
        def frames    = rows.collect { it.t }.unique().sort()
        def chans     = rows.collect { it.channel as int }.unique().sort()
        def summary   = [output_path: name, series_id: first.series_id, t: first.t,
                         frames: frames.size(), channels: chans.size(),
                         format: format, scale_percent: pct,
                         status: "", reason: "", bytes: 0L, checksum: ""]

        // include belongs to the SERIES, one value, so "rows of this output
        // disagree on include" is no longer a state that can be written down.
        // The caller resolves it from the series table and passes it per output.
        if (!isIncluded(opts.include)) {
            summary.status = "skipped"
            summary.reason = "include=false"
            return summary
        }

        // The name is the whole check, and it is enough BECAUSE the name carries
        // the two things that change the pixels: the scale token and the
        // extension. It
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

        long predicted = predictBytes(rows, pct)
        if (format == TIFF && !fitsClassicTiff(predicted)) {
            // One row's failure must not cost the rest of the run.
            summary.status = "failed"
            summary.reason = "needs " + predicted + " bytes, over the classic TIFF limit of " +
                             CLASSIC_TIFF_MAX + "; use format=bigtiff" +
                             (frames.size() > 1 ? ", or choose time points with frames=" : "")
            return summary
        }
        if (!fitsHeap(predicted)) {
            summary.status = "failed"
            summary.reason = "needs " + predicted + " bytes held in memory, over " +
                             (int) (HEAP_SHARE * 100) + "% of the " + Runtime.getRuntime().maxMemory() +
                             "-byte heap" + (frames.size() > 1 ? "; choose time points with frames=" : "") +
                             (pct == FULL ? ", or downscale" : "")
            return summary
        }

        int nz = first.size_z as int
        int nc = chans.size()
        int nt = frames.size()
        int outX = scaled(first.size_x as int, pct)
        int outY = scaled(first.size_y as int, pct)

        // Every frame must bring every channel, or the hyperstack would be
        // ragged and ImageJ would read the planes that follow as the wrong
        // (c, z, t) without complaining.
        def byFrame = rows.groupBy { it.t }
        def ragged = byFrame.findAll { t, v -> v.collect { it.channel as int }.sort() != chans }
        if (ragged) {
            summary.status = "failed"
            summary.reason = "frame(s) " + ragged.keySet().sort() + " do not have channels " + chans
            return summary
        }

        // Channels in sources-table order, which is the source metadata's own channel
        // index -- never directory order, which the .ims files show can differ.
        // Frames in time order, for the same reason.
        def ordered = []
        frames.each { tp -> ordered.addAll(byFrame[tp].sort(false) { a, b -> (a.channel as int) <=> (b.channel as int) }) }
        def open  = []
        def crc   = new CRC32()
        try {
            ordered.each { open << LFC.open(new File(srcRoot, it.source_path as String)) }
            open.eachWithIndex { lf, i ->
                if (lf.sizeZ != nz) {
                    throw new IllegalStateException(
                        name + ": " + ordered[i].source_path + " has " + lf.sizeZ +
                        " slices, expected " + nz +
                        " -- a gathered position must hold one volume shape, not several")
                }
            }
            // XYCZT: channel fastest, then z, then t -- matching
            // setDimensions(c, z, t) below. Getting this order wrong produces a
            // perfectly well-formed stack of the wrong planes.
            def stack = new ImageStack(outX, outY)
            for (int t = 0; t < nt; t++) {
                for (int z = 0; z < nz; z++) {
                    for (int c = 0; c < nc; c++) {
                        def row = ordered[t * nc + c]
                        short[] px = open[t * nc + c].plane(z)
                        def sp = new ShortProcessor(first.size_x as int, first.size_y as int, px, null)
                        if (pct < FULL) sp = (ShortProcessor) sp.resize(outX, outY, true)
                        // Checksum what is WRITTEN, not what was read. At 100%
                        // the two are the same; below it they are not, and a
                        // checksum of the source could not tell a scaling bug
                        // from a correct downscale.
                        crc.update(toBytes((short[]) sp.getPixels()))
                        stack.addSlice(sliceLabel(row, z, nz, c, nc, t, nt), sp)
                    }
                }
            }
            def imp = new ImagePlus(name.replaceAll(/\.(ome\.)?tiff?$/, ""), stack)
            imp.setDimensions(nc, nz, nt)
            applyCalibration(imp, first, outX, outY)

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
     * Every output in the sources table, one row's failure never costing the rest.
     *
     * The same contract as BatchRunner.runEach(): the summary is RECTANGULAR
     * whatever happened, so a skipped or failed output still has a row saying
     * which and why. A run that half worked has to be legible.
     */
    List<Map> assembleAll(List<Map> rows, File srcRoot, File outDir, Map opts = [:], Closure log = null) {
        String format = checkFormat(opts.format as String)
        int pct        = checkScalePercent(opts.scalePercent)
        boolean verify = (opts.verify ?: false) as boolean
        def frameSel   = parseFrames(opts.frames)
        boolean oneFile = (opts.oneFile ?: false) as boolean

        def groups = groupByOutput(rows, frameSel, oneFile)
        log?.call("  " + groups.size() + " output(s), format=" + format +
                  (frameSel != null ? (", time points " + frameSel +
                                       (oneFile ? " in one file per series" : " each in its own file")) : "") +
                  (pct < FULL ? (", downscaled to " + pct + "%") : "") +
                  (opts.skipExisting ? ", skipping ones already assembled" : "") +
                  (verify ? ", verifying" : ""))
        if (frameSel != null) {
            def absent = frameSel - rows.collect { it.t as int }.unique()
            if (absent) {
                log?.call("WARNING: no source has time point(s) " + absent + "; nothing is written for them")
            }
        }

        // Say what will not fit BEFORE writing anything, so a person can choose
        // the format rather than discover it after twenty minutes of writing.
        if (format == TIFF) {
            // Only outputs this run will actually attempt. Warning about a
            // position the operator already set include=false on is noise, and
            // noise is what stops warnings being read.
            def includeOf = (opts.includeBySeries ?: [:])
            def over = groups.findAll { k, v ->
                def sid = v[0].series_id as String
                isIncluded(includeOf.containsKey(sid) ? includeOf[sid] : "true") &&
                !fitsClassicTiff(predictBytes(v, pct))
            }
            if (over) {
                log?.call("WARNING: " + over.size() + " output(s) exceed the classic TIFF limit " +
                          "and will be recorded as failed; use format=bigtiff:")
                over.each { k, v ->
                    log?.call("           " + k + "  " + predictBytes(v, pct) + " bytes")
                }
            }
        }

        def summaries = []
        def includeBy = (opts.includeBySeries ?: [:])
        groups.each { String out, List<Map> group ->
            // include belongs to the SERIES, so it is looked up by series_id --
            // not by the output name, which differs once a frame is taken out.
            def sid = group[0].series_id as String
            def inc = includeBy.containsKey(sid) ? includeBy[sid] : "true"
            def s = assembleOne(group, srcRoot, outDir,
                                opts + [include: inc, base: (out == sid ? null : out)])
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

    /**
     * Sources rows grouped into outputs, in table order, keyed by output base.
     *
     * With no frame selection, one output per series -- every frame it holds.
     * With one, only the chosen time points, and each in its OWN file: a frame
     * taken out of a multi-frame series is named series_id + _t<TTTT>, so
     * frame 1 and frame 48 of one position cannot overwrite each other. With
     * `oneFile`, the chosen time points of a series go into one file instead,
     * named after the selection (framesBase). A series that holds a single
     * frame keeps its own name either way.
     */
    static Map<String, List<Map>> groupByOutput(List<Map> rows, List<Integer> frames = null,
                                                boolean oneFile = false) {
        def framesPerSeries = rows.groupBy { it.series_id }
                                  .collectEntries { k, v -> [k, v.collect { it.t as int }.unique().size()] }
        // The time points each series actually holds out of those chosen: the
        // name says what is IN the file, not what was asked for.
        def chosenPerSeries = frames == null ? [:] :
            rows.findAll { frames.contains(it.t as int) }.groupBy { it.series_id }
                .collectEntries { k, v -> [k, v.collect { it.t as int }.unique()] }
        def m = new LinkedHashMap()
        rows.sort(false) { a, b ->
            (a.series_id <=> b.series_id) ?:
            ((a.t as int) <=> (b.t as int)) ?:
            ((a.channel as int) <=> (b.channel as int))
        }.each { r ->
            if (frames != null && !frames.contains(r.t as int)) return
            def key = (frames == null || framesPerSeries[r.series_id] <= 1) ? (r.series_id as String)
                    : oneFile ? framesBase(r.series_id as String, chosenPerSeries[r.series_id])
                    : frameBase(r.series_id as String, r.t)
            m.computeIfAbsent(key, { [] }) << r
        }
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
        put("t",             summary.t)
        put("frames",        summary.frames)
        put("format",        summary.format)
        put("scale_percent", summary.scale_percent)
        put("pixel_checksum", summary.checksum)
        put("output_bytes",  summary.bytes)
        put("source_root",   srcRoot?.getAbsolutePath())
        put("gatherer_version", RX.repoVersion(libDir))
        put("written_at",    new Date().format("yyyy-MM-dd'T'HH:mm:ss"))
        // Every source, keyed so a gathered position names all of them: one
        // line per (timepoint, channel) rather than per channel, or three of a
        // position's twelve sources would be the only ones recorded.
        boolean gathered = (summary.frames as int) > 1
        // Frame k of a gathered file is the k-th time point here. The analysis
        // counts frames inside the file from 1, so for frames 2, 11, 21 its
        // t = 2 is time point 11 -- this line is the way back.
        if (gathered) {
            put("time_points", rows.collect { it.t as int }.unique().sort().join(" "))
        }
        rows.sort(false) { a, b -> (a.t <=> b.t) ?: ((a.channel as int) <=> (b.channel as int)) }.each { r ->
            def key = gathered ? ("t" + r.t + "_channel_" + r.channel) : ("channel_" + r.channel)
            put(key + "_name",   r.channel_name)
            put(key + "_source", r.source_path)
            put(key + "_bytes",  r.source_bytes)
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
    static String sliceLabel(Map row, int z, int nz, int c, int nc, int t = 0, int nt = 1) {
        def sb = new StringBuilder()
        if (nc > 1) sb.append("c:").append(c + 1).append("/").append(nc).append(" ")
        if (nz > 1) sb.append("z:").append(z + 1).append("/").append(nz).append(" ")
        if (nt > 1) sb.append("t:").append(t + 1).append("/").append(nt).append(" ")
        sb.append("- ").append(row.channel_name ?: ("channel_" + row.channel))
        return sb.toString()
    }

    /**
     * Calibration, scaled to match what was actually written.
     *
     * ⚠️ HALVE THE PIXELS AND THE PIXEL SIZE MUST DOUBLE. Miss this and every
     * area is out by the square of the factor while the image looks perfect --
     * exactly the silent failure this repo keeps meeting. pixelDepth is left
     * alone: resizing is in x and y only.
     */
    static void applyCalibration(ImagePlus imp, Map row, int outX, int outY) {
        def cal = imp.getCalibration()
        // Scaled by the ratio ACHIEVED, never by the ratio asked for. The pixel
        // count is rounded, so 33% of 2048 is 676 pixels -- a ratio of 3.0296,
        // not 3.0303. Using the requested figure would record a physical width
        // the specimen does not have, and area is that error squared.
        if (row.pixel_width  != null) {
            cal.pixelWidth  = (row.pixel_width  as double) * ((row.size_x as double) / outX)
        }
        if (row.pixel_height != null) {
            cal.pixelHeight = (row.pixel_height as double) * ((row.size_y as double) / outY)
        }
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
