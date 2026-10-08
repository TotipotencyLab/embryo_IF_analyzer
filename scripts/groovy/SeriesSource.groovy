// SeriesSource.groovy
//
// One series, handed over ONE FRAME AT A TIME.
//
// A Luxendo position is ~95 GB of pixels against a ~9 GB heap, so nothing may
// hold a series whole: the batch asks for frame t, analyses it, releases it,
// and asks for the next. Two kinds of series answer that question:
//
//   image     an ImagePlus that is already open -- the interactive runner's
//             image, or a series the batch opened from a file through
//             Bio-Formats. A single frame is handed over as ITSELF, no copy,
//             so nothing about the single-frame path changes; a multi-frame
//             one hands over a Duplicator copy of frame t.
//   luxendo   the sources table's rows for one series: one .lux.h5 per
//             (channel, time point). Frame t is assembled from its channels'
//             files when asked for, and nothing else is read.
//
// Which kind a batch row is decided by MEMBERSHIP -- the sources table has rows
// for its series_id, or it does not -- in BatchRunner.openSource(), never by
// guessing whether `path` looks like a file (note/time_series_plan.md §3.2b).
//
// `t` is the frame's number in the SERIES, from 1, as ImageJ shows frames and
// as sources.tsv counts them. For Luxendo it is the time point itself, so a
// run over time points 2, 11 and 21 reports t = 2, 11, 21 -- unlike a TIFF
// gathered from them, whose frames count 1, 2, 3.

import ij.ImagePlus
import ij.ImageStack
import ij.measure.Calibration
import ij.plugin.Duplicator
import ij.process.ShortProcessor
import groovy.json.JsonSlurper

class SeriesSource {

    /** How the series was opened: importer, reader, luxendo; blank if it was already open. */
    String method = ""
    /** The title every frame carries -- it reaches _res.txt's Label and _config.txt. */
    String title
    int width, height, nChannels, nSlices
    Calibration calibration
    /** Seconds (or frameUnit) between frames; null for one frame or when unknown, never 1. */
    Double frameInterval
    String frameUnit
    /** The frames this series has, ascending. Usually 1..n; a Luxendo gap stays a gap. */
    List<Integer> frameList = []

    // image-backed
    ImagePlus whole
    boolean ownsWhole

    // luxendo-backed
    Map<Integer, List<Map>> byFrame
    File root
    Class LFC

    /** frameList.size(). A field, not a getter: Groovy calls getNFrames()'s property `NFrames`. */
    int nFrames

    /** The series as an open image, which it may close when done if `owns`. */
    static SeriesSource ofImage(ImagePlus imp, String method, boolean owns) {
        def s = new SeriesSource(method: method ?: "", title: imp.getTitle(),
                                 width: imp.getWidth(), height: imp.getHeight(),
                                 nChannels: imp.getNChannels(), nSlices: imp.getNSlices(),
                                 calibration: imp.getCalibration(),
                                 whole: imp, ownsWhole: owns)
        s.frameList = (1..imp.getNFrames()).toList()
        s.nFrames = s.frameList.size()
        double fi = imp.getCalibration().frameInterval
        if (imp.getNFrames() > 1 && fi > 0d) {
            s.frameInterval = fi
            s.frameUnit = imp.getCalibration().getTimeUnit()
        }
        return s
    }

    /**
     * The series as the sources table's rows for it.
     *
     * @param rows  every sources row of ONE series, typed (t, channel, sizes as
     *              numbers; blank calibration as null)
     * @param root  the acquisition directory source_path is relative to
     * @param title what each frame is called; the batch passes the series_id
     * @param lfc   the LuxendoFile class
     */
    static SeriesSource ofLuxendo(List<Map> rows, File root, String title, Class lfc) {
        if (!rows) throw new IllegalArgumentException("no sources rows for " + title)
        def first = rows.min { a, b -> (a.t as int) <=> (b.t as int) ?: (a.channel as int) <=> (b.channel as int) }
        def s = new SeriesSource(method: "luxendo", title: title, root: root, LFC: lfc,
                                 width: first.size_x as int, height: first.size_y as int,
                                 nSlices: first.size_z as int)
        s.byFrame = rows.groupBy { it.t as int }.collectEntries { t, v ->
            [(t): v.sort(false) { a, b -> (a.channel as int) <=> (b.channel as int) }]
        }
        s.frameList = s.byFrame.keySet().sort()
        s.nFrames = s.frameList.size()
        // The channels of the series are the channels of its first frame; a
        // frame lacking one fails on its own when asked for, not the series.
        s.nChannels = s.byFrame[s.frameList[0]].size()
        // Dimensions must be one shape across the series. z varying between
        // time points would make "slice 12" mean different planes, and every
        // frame would still look fine.
        def shapes = rows.collect { [it.size_x, it.size_y, it.size_z].collect { it as int } }.unique()
        if (shapes.size() > 1) {
            throw new IllegalArgumentException(
                title + ": the sources disagree on (x, y, z): " + shapes +
                " -- one series must hold one volume shape")
        }
        def cal = new Calibration()
        if (first.pixel_width  != null) cal.pixelWidth  = first.pixel_width  as double
        if (first.pixel_height != null) cal.pixelHeight = first.pixel_height as double
        // Only where there IS a z axis; a z step that does not exist must not
        // arrive as a usable-looking number.
        if (first.pixel_depth != null && s.nSlices > 1) cal.pixelDepth = first.pixel_depth as double
        if (first.pixel_unit) cal.setUnit(first.pixel_unit as String)
        if (s.frameList.size() > 1) {
            def fi = intervalOf(new File(root, first.source_path as String), lfc)
            if (fi != null) {
                s.frameInterval = fi
                s.frameUnit = "sec"
                cal.frameInterval = fi
                cal.setTimeUnit("sec")
            }
        }
        s.calibration = cal
        return s
    }

    /**
     * The time-lapse interval Luxendo recorded, in seconds, or null.
     *
     * metaData.triggers[].interval_s in the acquisition JSON every .lux.h5
     * carries. Only an unambiguous answer is taken: exactly one trigger with a
     * positive interval. Several would mean a protocol this code has not seen,
     * and a guessed interval is worse than a blank one.
     */
    static Double intervalOf(File luxH5, Class lfc) {
        def h5reader = null
        try {
            h5reader = lfc.open(luxH5)
            if (!h5reader.metadataJson) return null
            def triggers = new JsonSlurper().parseText(h5reader.metadataJson as String)?.metaData?.triggers
            def withInterval = (triggers ?: []).findAll { (it?.interval_s ?: 0) as double > 0d }
            return (withInterval.size() == 1) ? (withInterval[0].interval_s as double) : null
        } catch (Throwable ignored) {
            return null
        } finally {
            if (h5reader != null) { try { h5reader.close() } catch (ignored2) { } }
        }
    }

    /**
     * Frame t (from 1, as listed in frameList) as an image of its own: every
     * channel and slice, one frame. The caller hands it back with release().
     */
    ImagePlus frame(int t) {
        if (!frameList.contains(t)) {
            throw new IllegalArgumentException(title + " has no frame t=" + t + " (it has " +
                                               describeFrames(frameList) + ")")
        }
        if (whole != null) {
            if (whole.getNFrames() == 1) return whole
            def f = new Duplicator().run(whole, 1, whole.getNChannels(), 1, whole.getNSlices(), t, t)
            // A Duplicator copy is called DUP_<title>, and the title reaches the
            // measurement Label.
            f.setTitle(whole.getTitle())
            return f
        }
        return assembleLuxendo(t)
    }

    /** Hand a frame back. The whole image is the caller's, and is never closed here. */
    void release(ImagePlus f) {
        if (f == null || f.is(whole)) return
        // close() then flush(): close() alone frees nothing headless.
        f.changes = false
        f.close(); f.flush()
    }

    /**
     * Done with the series: closes the whole image if this source opened it.
     * Not close(): an image's close() frees nothing without flush(), and the
     * library is checked for a bare one (Test_NucleusPipeline).
     */
    void dispose() {
        closeFrameFiles()
        if (whole != null && ownsWhole) {
            whole.changes = false
            whole.close(); whole.flush()
        }
    }

    // The files of ONE frame, held open while its planes are read: a frame at
    // a time, never the series -- a 96-frame position is 288 open files.
    private int openT = -1
    private List openFiles = []

    /**
     * One plane, raw uint16 row-major: frame t, channel c and slice z, each
     * counted from 1. The frame's files stay open until another frame is asked
     * for, so reading a frame plane by plane opens each file once.
     *
     * This is the one place a Luxendo series is read: frame() builds on it, and
     * Make_LuxendoTiff's assembler reads through it, plane by plane, so a
     * downscaled output never holds a full-resolution frame.
     */
    short[] plane(int t, int c, int z) {
        if (byFrame == null) throw new IllegalStateException("plane() reads a Luxendo series; this one is an image")
        openFrameFiles(t)
        if (c < 1 || c > nChannels || z < 1 || z > nSlices) {
            throw new IllegalArgumentException(title + ": no plane c=" + c + " z=" + z)
        }
        return openFiles[c - 1].plane(z - 1)
    }

    /** The sources row of frame t, channel c (from 1): its channel name, its file. */
    Map sourceRow(int t, int c) {
        return byFrame[t][c - 1]
    }

    private void openFrameFiles(int t) {
        if (openT == t) return
        closeFrameFiles()
        if (!frameList.contains(t)) {
            throw new IllegalArgumentException(title + " has no frame t=" + t + " (it has " +
                                               describeFrames(frameList) + ")")
        }
        def rows = byFrame[t]
        if (rows.size() != nChannels) {
            throw new IllegalStateException(title + " t=" + t + ": " + rows.size() + " channel(s), the series has " +
                                            nChannels + " -- a frame must bring every channel")
        }
        def files = []
        try {
            rows.each { files << LFC.open(new File(root, it.source_path as String)) }
            files.eachWithIndex { h5reader, i ->
                if (h5reader.sizeZ != nSlices || h5reader.sizeX != width || h5reader.sizeY != height) {
                    throw new IllegalStateException(
                        rows[i].source_path + " is " + h5reader.sizeX + "x" + h5reader.sizeY + "x" + h5reader.sizeZ +
                        ", the sources table says " + width + "x" + height + "x" + nSlices +
                        " -- a series must hold one volume shape")
                }
            }
        } catch (Throwable e) {
            files.each { h5reader -> try { h5reader.close() } catch (ignored) { } }
            throw e
        }
        openFiles = files
        openT = t
    }

    /** Close the held frame's files. */
    void closeFrameFiles() {
        openFiles.each { h5reader -> try { h5reader.close() } catch (ignored) { } }
        openFiles = []
        openT = -1
    }

    private ImagePlus assembleLuxendo(int t) {
        try {
            // XYCZT: channel fastest, then z -- matching setDimensions(c, z, 1).
            // Getting this order wrong is a well-formed stack of the wrong planes.
            // Each channel's stack read whole, in strips (LuxendoFile.volume):
            // plane by plane re-reads every chunk once per slice, which over a
            // network mount is most of a frame's time.
            openFrameFiles(t)
            def vols = (0..<nChannels).collect { openFiles[it].volume() }
            def stack = new ImageStack(width, height)
            for (int z = 1; z <= nSlices; z++) {
                for (int c = 1; c <= nChannels; c++) {
                    stack.addSlice(sliceLabel(sourceRow(t, c), z - 1, nSlices, c - 1, nChannels),
                                   new ShortProcessor(width, height, vols[c - 1][z - 1], null))
                }
            }
            def imp = new ImagePlus(title, stack)
            imp.setDimensions(nChannels, nSlices, 1)
            imp.setCalibration(calibration.copy())
            return imp
        } finally {
            closeFrameFiles()
        }
    }

    /**
     * The slice label of one plane: "c:1/3 z:5/39 - GFP", as an ImageJ
     * hyperstack TIFF of one time point carries it (TiffAssembler.sliceLabel),
     * so a frame read here and the same frame read from Make_LuxendoTiff's file
     * label their planes alike.
     */
    static String sliceLabel(Map row, int z, int nz, int c, int nc) {
        def sb = new StringBuilder()
        if (nc > 1) sb.append("c:").append(c + 1).append("/").append(nc).append(" ")
        if (nz > 1) sb.append("z:").append(z + 1).append("/").append(nz).append(" ")
        sb.append("- ").append(row.channel_name ?: ("channel_" + row.channel))
        return sb.toString()
    }

    /** "1-96", or "2, 11, 21": for messages, not for parsing. */
    static String describeFrames(List<Integer> ts) {
        if (!ts) return "no frames"
        def s = ts.sort(false)
        boolean run = (s[-1] - s[0] + 1) == s.size()
        return (run && s.size() > 2) ? (s[0] + "-" + s[-1]) : s.join(", ")
    }

    /**
     * Which frames to analyse: the selection, kept to frames the series has.
     *
     * A series with ONE frame is analysed whatever the selection says -- the
     * selection is for time courses, and a sheet may mix both.
     *
     * @param wanted parsed selection (TiffAssembler.parseFrames), or null = all
     * @return [frames: chosen, absent: asked-for frames the series lacks]
     */
    Map choose(List<Integer> wanted) {
        if (wanted == null || frameList.size() <= 1) return [frames: frameList, absent: []]
        return [frames: frameList.findAll { wanted.contains(it) },
                absent: wanted.findAll { !frameList.contains(it) }]
    }

    /**
     * Sources rows as Tsv reads them (all text) -> numbers where arithmetic is
     * done on them. BLANK calibration stays null, never 0.0.
     */
    static List<Map> typed(List<Map> rows) {
        // series_index stays text: it is half of the key the two tables join on
        // (LuxendoScan.seriesKey), and the series table's is text too.
        def ints = ["t", "channel", "size_x", "size_y", "size_z", "source_bytes"]
        def dbls = ["pixel_width", "pixel_height", "pixel_depth"]
        return rows.collect { r ->
            def c = new LinkedHashMap(r)
            ints.each { k -> if (c[k] != null && c[k].toString().trim()) c[k] = c[k].toString().trim() as Long }
            ["t", "channel", "size_x", "size_y", "size_z"].each { k ->
                if (c[k] instanceof Long) c[k] = (c[k] as Long).intValue()
            }
            dbls.each { k -> c[k] = (c[k] != null && c[k].toString().trim()) ? (c[k].toString().trim() as Double) : null }
            c
        }
    }
}
