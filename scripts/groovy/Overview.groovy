// Overview.groovy
//
// Quick-look images for checking a detection run: a z-projection of the planes
// that detection actually used, optionally with the detected outlines drawn on.
//
// Library only -- no `#@` parameters, no dialog. Load it the same way as the
// other modules:
//
//   def OV = new GroovyClassLoader().parseClass(new File(LIBDIR + "/Overview.groovy"))
//
// Steps, each its own function so a caller can use only what it needs:
//
//   project()      collapse the chosen z-planes, per channel
//   prepare()      pick a channel, set contrast, resize -> View
//   addOutlines()  draw ROIs onto an overlay, scaled to the view
//   savePng()      burn the overlay into pixels and write a PNG
//
//   def OV   = ...parseClass(...)
//   def proj = OV.project(imp, slices, "max", [dnaCh])
//   def view = OV.prepare(proj, dnaCh, [width: 500])
//   OV.addOutlines(view, nucRois,  [mode: "merged", color: "yellow"])
//   OV.addOutlines(view, nuclRois, [mode: "all",    color: "magenta"])
//   OV.savePng(view, OV.overviewPath(outDir, seriesId, dnaCh))
//
// Why not just call ZProjector: it projects one continuous range. Detection
// accepts gapped ranges ("1-20,35-40"), and an overview has to show what
// detection saw, so the chosen planes are gathered into a substack first.

import ij.*
import ij.gui.Overlay
import ij.gui.Roi
import ij.gui.ShapeRoi
import ij.io.FileSaver
import ij.plugin.Colors
import ij.plugin.ContrastEnhancer
import ij.plugin.RoiScaler
import ij.plugin.ZProjector
import ij.process.ImageProcessor
import java.awt.Color

class Overview {

    /**
     * One channel of a projection, ready to draw on and save.
     *
     * `sx` / `sy` are the resize factors from the projection to this image.
     * addOutlines() needs them: ROIs are in the original pixel coordinates and
     * have to be scaled by the same amount to land in the right place.
     *
     * `lo` / `hi` are the display range prepare() settled on. They are reported
     * rather than merely applied because "auto" contrast stretches whatever is
     * present: a channel holding nothing but noise gets that noise stretched to
     * full range and saves a convincing picture of nothing. A narrow lo-hi next
     * to a wide one on another channel is the tell, and it is only visible if
     * somebody writes the numbers down.
     *
     * Groovy writes the getters, setters and a named-argument constructor for
     * these fields, so `new View(image: imp, sx: 0.5, sy: 0.5, channel: 1)`
     * works without any constructor being declared.
     */
    static class View {
        ImagePlus image
        double sx
        double sy
        int channel
        double lo
        double hi

        String toString() {
            "View(ch${channel}, ${image.getWidth()}x${image.getHeight()}, scale ${sx}x${sy}, display ${lo}-${hi})"
        }
    }

    static final List<String> CONTRAST = ["auto", "none"]
    static final List<String> OUTLINE_MODES = ["all", "merged", "none"]

    /**
     * File-name suffix for the copy with outlines drawn on it; the bare
     * projection takes "". Lives here so the two runners cannot drift apart --
     * feature_outline_cli.r is handed these names by hand, so a mismatch would only
     * show up as a missing file much later.
     */
    static final String OVERLAY_SUFFIX = "_overlay"

    // Accepted projection names -> the strings ZProjector.run() understands.
    static final Map<String, String> METHODS = [
        max: "max", min: "min", sum: "sum", sd: "sd", median: "median",
        mean: "avg", avg: "avg"
    ]

    /**
     * Z-project the given planes of the given channels.
     *
     * @param imp       source image (a hyperstack, or a plain z-stack)
     * @param zSlices   z indices to include, 1-based; null or empty = all
     * @param method    max | mean | sum | sd | median | min
     * @param channels  channels to keep, 1-based; null or empty = all
     * @return a new image with one plane per requested channel, in the order
     *         given. Each plane is labelled "ch<c>" with its ORIGINAL channel
     *         number, so a caller can find channel 3 even if it is the only one.
     *
     * Only the first time frame is used; time-lapse data is not handled.
     */
    static ImagePlus project(ImagePlus imp, Set<Integer> zSlices, String method,
                             List<Integer> channels) {
        String zpMethod = projectionMethod(method)

        int nC = imp.getNChannels(), nZ = imp.getNSlices()
        // NB: `(1..nZ)` is a Range -- a List, but a READ-ONLY one, so sort() on it
        //     throws UnsupportedOperationException. toList() makes a real copy.
        List<Integer> zs = (zSlices ? zSlices.toList() : (1..nZ).toList()).sort()
        List<Integer> cs = channels ? channels : (1..nC).toList()

        // Fail with a message naming the problem, rather than an index error from
        // deep inside ImageStack.
        def badZ = zs.findAll { it < 1 || it > nZ }
        if (badZ) throw new IllegalArgumentException("z out of range 1-${nZ}: ${badZ}")
        def badC = cs.findAll { it < 1 || it > nC }
        if (badC) throw new IllegalArgumentException("channel out of range 1-${nC}: ${badC}")
        if (imp.getNFrames() > 1) {
            IJ.log("Overview.project: ${imp.getNFrames()} frames; using frame 1 only")
        }

        // Gather the chosen planes in HYPERSTACK ORDER, where the channel varies
        // fastest: c1z1, c2z1, c1z2, c2z2, ...  So z is the OUTER loop. Nesting
        // it the other way builds a stack that setDimensions() then silently
        // misreads, and ZProjector mixes channels into each other.
        //
        // NB: the processors are SHARED with the source, not copied. Nothing
        //     below writes to them -- ZProjector allocates its own output -- and
        //     sharing avoids duplicating half a gigabyte for a large section.
        //     Anything that ever modifies `sub` in place must copy first.
        def src = imp.getStack()
        def sub = new ImageStack(imp.getWidth(), imp.getHeight())
        zs.each { int z ->
            cs.each { int c ->
                sub.addSlice("c${c}_z${z}", src.getProcessor(imp.getStackIndex(c, z, 1)))
            }
        }

        def gathered = new ImagePlus(imp.getTitle() + "_sub", sub)
        gathered.setDimensions(cs.size(), zs.size(), 1)
        gathered.setCalibration(imp.getCalibration().copy())

        ImagePlus proj
        if (zs.size() == 1) {
            // Nothing to collapse. Must not go through ZProjector: with one z it
            // sees a plain stack of CHANNELS and would project across them.
            proj = gathered.duplicate()
        } else {
            // Flag it as a hyperstack so ZProjector projects each channel
            // separately. Without this a multi-channel stack is treated as one
            // long z-stack and every channel is mixed into one plane.
            gathered.setOpenAsHyperStack(cs.size() > 1)
            proj = ZProjector.run(gathered, zpMethod)
        }

        proj.setDimensions(cs.size(), 1, 1)
        proj.setCalibration(imp.getCalibration().copy())
        cs.eachWithIndex { int c, int i -> proj.getStack().setSliceLabel("ch${c}", i + 1) }
        proj.setTitle(imp.getTitle() + "_" + method.toLowerCase())
        return proj
    }

    /**
     * Take one channel of a projection, set its display contrast and resize it.
     *
     * @param proj     output of project()
     * @param channel  ORIGINAL channel number (as labelled by project())
     * @param opts     width, height : output size in pixels. Leave both out for
     *                                 the original size; give one and the other
     *                                 follows the aspect ratio; give both and the
     *                                 image is stretched to exactly that.
     *                                 null, "" and 0 all mean "not given".
     *                 contrast      : "auto" (default) stretches the display range
     *                                 to the data, clipping `saturated` percent;
     *                                 "none" leaves it at the type's full range --
     *                                 0-255 for 8-bit, 0-65535 for 16-bit. 32-bit
     *                                 has no fixed range, so "none" there means the
     *                                 data's own min-max.
     *                 saturated     : percent clipped by "auto" (default 0.35, as in
     *                                 Fiji's Enhance Contrast)
     *
     * Contrast changes only how pixels are DISPLAYED -- the values are untouched
     * until savePng() renders them. The projection itself is not modified.
     */
    static View prepare(ImagePlus proj, int channel, Map opts = [:]) {
        def chans = projectedChannels(proj)
        int idx = chans.indexOf(channel)
        if (idx < 0) {
            throw new IllegalArgumentException("channel ${channel} is not in the projection (has ${chans})")
        }
        String contrast = contrastMode(opts.contrast)
        double saturated = saturatedPercent(opts.saturated)

        // Copy: the projection may be prepared again for another channel.
        ImageProcessor ip = proj.getStack().getProcessor(idx + 1).duplicate()
        int W = ip.getWidth(), H = ip.getHeight()

        // Contrast is computed at FULL resolution, so the statistics are exact,
        // and applied after resizing so both images use the same range.
        double lo, hi
        if (contrast == "auto") {
            // NB: for 32-bit data the stretch works on a 256-bin histogram laid
            //     over the FULL min-max, so its limits snap to bin edges. With
            //     extreme outliers the bins get coarse: a 0-995 ramp plus three
            //     pixels of 100000 gets a top of 781, not ~995, and the brightest
            //     fifth saturates. Harmless unless outliers are hundreds of times
            //     brighter than the signal.
            new ContrastEnhancer().stretchHistogram(ip, saturated)
            lo = ip.getMin(); hi = ip.getMax()
        } else if (ip.getBitDepth() == 32) {
            ip.resetMinAndMax()
            lo = ip.getMin(); hi = ip.getMax()
        } else {
            lo = 0; hi = (ip.getBitDepth() == 16) ? 65535 : 255
        }

        def reduced = reduce(ip, opts)
        ImageProcessor out = reduced.ip
        int w = reduced.w, h = reduced.h
        // Force greyscale. A Bio-Formats import carries a per-channel colour LUT
        // (green, red, blue...), savePng() calls flatten(), and flatten renders
        // the image THROUGH its LUT -- so the quick-look PNG came out tinted by
        // whatever colour the acquisition happened to assign that channel.
        // Nothing was choosing colour; it was inherited. Overlay outlines are
        // unaffected: flatten draws them on top of the rendered image.
        //
        // NB: getDefaultColorModel() is an INSTANCE method, not static --
        //     ImageProcessor.getDefaultColorModel() compiles and then throws
        //     MissingMethodException at run time.
        //
        // NB: BEFORE setMinAndMax, not after. setColorModel() resets the
        //     display range, so setting it afterwards silently discarded the
        //     contrast stretch and 'auto' saved raw brightness.
        out.setColorModel(out.getDefaultColorModel())

        out.setMinAndMax(lo, hi)

        double sx = w / (double) W, sy = h / (double) H
        def img = new ImagePlus(proj.getTitle() + "_ch${channel}", out)
        def cal = proj.getCalibration().copy()
        cal.pixelWidth  = cal.pixelWidth  / sx      // fewer, larger pixels
        cal.pixelHeight = cal.pixelHeight / sy
        img.setCalibration(cal)

        return new View(image: img, sx: sx, sy: sy, channel: channel, lo: lo, hi: hi)
    }

    /**
     * Resize to the overview size: width/height as prepare() documents them.
     * Pixel VALUES only -- no display range is decided here -- so a multi-frame
     * series can be reduced frame by frame and rendered once its range is known.
     *
     * @return [ip: the resized processor (the given one when no resize was
     *          needed), w:, h:]
     */
    private static Map reduce(ImageProcessor ip, Map opts) {
        int W = ip.getWidth(), H = ip.getHeight()
        Integer w = sizeOpt(opts.width, "width"), h = sizeOpt(opts.height, "height")
        if (w == null && h == null) { w = W; h = H }
        else if (h == null)         { h = Math.max(1, Math.round(H * w / (double) W) as int) }
        else if (w == null)         { w = Math.max(1, Math.round(W * h / (double) H) as int) }

        ImageProcessor out = ip
        if (w != W || h != H) {
            ip.setInterpolationMethod(ImageProcessor.BILINEAR)
            // Averaging when shrinking: without it, a 4152 px section reduced to
            // 500 px just picks every 8th pixel and small objects flicker in and out.
            out = ip.resize(w, h, true)
        }
        return [ip: out, w: w, h: h]
    }

    // =========================================================================
    // A MULTI-FRAME SERIES: one TIFF per channel, one page per frame
    // =========================================================================
    //
    // A single frame keeps its PNG. A time course gets
    //
    //   <series_id>_overview_ch<c>.tif           8-bit grey, one page per frame
    //   <series_id>_overview_ch<c>_overlay.tif   the same, outlines burned in (RGB)
    //
    // with ONE display range per channel across every frame. "auto" fitted per
    // frame would make a cell appear to brighten because the stretch moved,
    // and over a time course that artifact looks like biology.
    //
    // The frames are not held. A Luxendo position is streamed one frame at a
    // time, so each frame leaves two small things in its staging directory --
    // the projection reduced to the overview size, at its own bit depth with
    // no range decided, and the FULL-RESOLUTION histogram of that projection
    // -- and the series is rendered from them once every frame has been seen.
    //
    // The range is ImageJ's own "auto", on the summed histogram. For 8- and
    // 16-bit data ContrastEnhancer works on the exact pixel values (the
    // 65536-bin histogram16 for 16-bit), clipping `saturated`/2 percent of the
    // pixel count from each end -- so the sum of the frames' histograms gives
    // exactly the range "auto" would choose for all the frames as one image,
    // and for a single frame exactly the range its PNG gets. Test_Overview
    // holds ContrastEnhancer to both.
    //
    // 32-bit projections (mean, sum, sd, median) are refused for a series of
    // several frames: ImageJ bins 32-bit data over its own min-max, which is not
    // known until the last frame, so the range cannot be accumulated.

    static final String STAGED_PLANE = "overview_ch%d.tif"
    static final String STAGED_HIST  = "overview_ch%d_hist.tsv"
    /** The methods whose projection keeps the image's own bit depth. */
    static final List<String> SERIES_METHODS = ["max", "min"]

    /**
     * Refuse, before any frame is read, settings a multi-frame overview cannot
     * honour. The per-image checks are validateSettings().
     */
    static void validateSeriesSettings(String method) {
        projectionMethod(method)
        if (!(method?.toLowerCase() in SERIES_METHODS)) {
            throw new IllegalArgumentException(
                "overview_method '" + method + "' gives a 32-bit projection, whose display range cannot be " +
                "decided frame by frame; a series of several frames takes " + SERIES_METHODS.join(" or ") +
                " (or switch save_overview off)")
        }
    }

    /**
     * Stage one frame of a multi-frame series' overview into `dir`: per
     * channel, the projection reduced to the overview size (STAGED_PLANE) and
     * its full-resolution histogram (STAGED_HIST, non-zero bins only).
     *
     * @param frame    ONE frame, every channel and slice (SeriesSource.frame)
     * @return the channels staged, in order
     */
    static List<Integer> stageFrame(ImagePlus frame, Set<Integer> zSlices, String method,
                                    List<Integer> channels, Map opts, File dir) {
        validateSeriesSettings(method)
        dir.mkdirs()
        def proj = project(frame, zSlices, method, channels)
        try {
            def chans = projectedChannels(proj)
            chans.eachWithIndex { int c, int i ->
                ImageProcessor ip = proj.getStack().getProcessor(i + 1)
                int[] hist = ip.getHistogram()
                if (hist == null) {
                    throw new IllegalStateException("no histogram for a " + ip.getBitDepth() + "-bit projection")
                }
                def sb = new StringBuilder("value\tcount\n")
                hist.eachWithIndex { int n, int v -> if (n > 0) sb.append(v).append('\t').append(n).append('\n') }
                new File(dir, String.format(STAGED_HIST, c)).setText(sb.toString(), "UTF-8")

                def red = reduce(ip.duplicate(), opts)
                def plane = new ImagePlus("ch" + c, red.ip)
                def cal = proj.getCalibration().copy()
                cal.pixelWidth  = cal.pixelWidth  * ip.getWidth()  / (double) red.w
                cal.pixelHeight = cal.pixelHeight * ip.getHeight() / (double) red.h
                plane.setCalibration(cal)
                def f = new File(dir, String.format(STAGED_PLANE, c))
                if (!new FileSaver(plane).saveAsTiff(f.getPath())) throw new IOException("could not write " + f)
                plane.close(); plane.flush()
            }
            return chans
        } finally {
            proj.close(); proj.flush()   // close() alone frees nothing headless
        }
    }

    /**
     * The display range for one channel over the staged frames: ImageJ's
     * "auto" on their summed histogram, or the type's full range for "none".
     *
     * @return [lo, hi]
     */
    static List<Double> seriesRange(List<File> frameDirs, int channel, Map opts, int bitDepth) {
        String contrast = contrastMode(opts.contrast)
        if (contrast == "none") return [0d, (bitDepth == 16 ? 65535d : 255d)]
        def hist = new TreeMap<Integer, Long>()
        frameDirs.each { File d ->
            new File(d, String.format(STAGED_HIST, channel)).readLines().drop(1).each { String l ->
                def p = l.split("\t")
                int v = p[0] as int
                hist[v] = (hist[v] ?: 0L) + (p[1] as long)
            }
        }
        return rangeFromHistogram(hist, saturatedPercent(opts.saturated), bitDepth)
    }

    /**
     * ContrastEnhancer.getMinAndMax() on an exact-valued histogram, as
     * value -> count.
     *
     * When nothing is left between the cut-offs -- a perfectly constant
     * channel -- ImageJ sets no range and the processor keeps whatever it had,
     * which for a projection plane can be a range left on its stack by another
     * channel (measured: a flat channel's PNG drawn at ch1's 100-2000). Here it
     * is the type's full range instead, the documented fallback: a channel of
     * zeros renders black rather than as someone else's stretch.
     */
    static List<Double> rangeFromHistogram(SortedMap<Integer, Long> hist, double saturated, int bitDepth) {
        long n = hist.values().sum(0L) as long
        long threshold = (saturated > 0d) ? (long) (n * saturated / 200.0d) : 0L
        int top = (bitDepth == 16) ? 65535 : 255
        // Walk the occupied values only: an empty bin never moves the count,
        // so the first value whose running total passes the threshold is the
        // same here as in ImageJ's walk over every bin -- except when none
        // does, where ImageJ stops at the last bin.
        Integer hmin = null, hmax = null
        long count = 0
        for (e in hist.entrySet()) { count += e.value; if (count > threshold) { hmin = e.key; break } }
        count = 0
        for (e in hist.descendingMap().entrySet()) { count += e.value; if (count > threshold) { hmax = e.key; break } }
        if (hmin == null) hmin = top
        if (hmax == null) hmax = 0
        if (hmax > hmin) return [hmin as double, hmax as double]
        return [0d, top as double]
    }

    /**
     * Render a multi-frame series' overview from its staged frames:
     * <series_id>_overview_ch<c>.tif and, when `layers` are given,
     * <series_id>_overview_ch<c>_overlay.tif -- one page per frame, in the
     * order given, labelled t=<t>.
     *
     * @param frameDirs  each frame's staging directory, in t order
     * @param ts         the frames' t, matching frameDirs
     * @param layers     outlines to draw, each [rois:, ts:, color:] with ts the
     *                   t of each ROI; drawn "merged", per frame, as the PNG
     *                   path draws them. null or empty = no overlay file.
     * @param srcW, srcH  the ORIGINAL image's size in pixels, which the ROIs
     *                   are in; the outlines are scaled from it to the plane
     * @param frameInterval  written into the TIFF's calibration, or null
     * @return channel -> [lo:, hi:, files: [...], size: "WxH"]
     */
    static Map<Integer, Map> writeSeries(List<File> frameDirs, List<Integer> ts, String outDir, String seriesId,
                                         List<Integer> channels, Map opts, int srcW, int srcH,
                                         List<Map> layers = null, Double frameInterval = null) {
        if (frameDirs.size() != ts.size() || frameDirs.isEmpty()) {
            throw new IllegalArgumentException("one staged directory per frame, and at least one: " +
                                               frameDirs.size() + " for " + ts.size())
        }
        def out = new LinkedHashMap<Integer, Map>()
        channels.each { int c ->
            // One staged plane open at a time: at full size a 96-frame series'
            // planes are most of a gigabyte, and the pages being built are
            // already a copy of them.
            def open = { File d ->
                def f = new File(d, String.format(STAGED_PLANE, c))
                def imp = IJ.openImage(f.getPath())
                if (imp == null) throw new IOException("could not read staged overview " + f)
                imp
            }
            def first = open(frameDirs[0])
            int bits = first.getBitDepth(), W = first.getWidth(), H = first.getHeight()
            def cal = first.getCalibration().copy()
            first.close(); first.flush()
            def range = seriesRange(frameDirs, c, opts, bits)
            def grey = new ImageStack(W, H)
            def rgb  = layers ? new ImageStack(W, H) : null
            frameDirs.eachWithIndex { File d, int i ->
                int t = ts[i]
                def plane = open(d)
                try {
                    if (plane.getBitDepth() != bits || plane.getWidth() != W || plane.getHeight() != H) {
                        throw new IllegalStateException("staged overview frames of channel " + c +
                                                        " differ in size or type (t=" + t + ")")
                    }
                    // The view is the staged plane with the series' range: the
                    // same object prepare() hands savePng(), so a frame here
                    // and a single frame's PNG are rendered by the same code.
                    // sx/sy map original pixels onto the reduced plane, for
                    // the outlines.
                    ImageProcessor ip = plane.getProcessor()
                    ip.setColorModel(ip.getDefaultColorModel())
                    ip.setMinAndMax(range[0], range[1])
                    def view = new View(image: plane, sx: W / (double) srcW, sy: H / (double) srcH,
                                        channel: c, lo: range[0], hi: range[1])
                    ImagePlus flat = plane.flatten()
                    try {
                        grey.addSlice("t=" + t, ((ij.process.ColorProcessor) flat.getProcessor()).getChannel(1, null))
                    } finally { flat.close(); flat.flush() }
                    if (rgb != null) {
                        layers.each { Map L ->
                            def mine = []
                            L.rois.eachWithIndex { r, int k -> if ((L.ts[k] as int) == t) mine << r }
                            addOutlines(view, mine as List<Roi>, [mode: "merged", color: L.color, lineWidth: 1])
                        }
                        ImagePlus ov = plane.flatten()
                        try { rgb.addSlice("t=" + t, ov.getProcessor()) } finally { ov.close(); ov.flush() }
                    }
                } finally {
                    plane.close(); plane.flush()
                }
            }
            if (frameInterval != null) { cal.frameInterval = frameInterval }
            def files = [saveFrames(grey, cal, new File(overviewTiffPath(outDir, seriesId, c, "")))]
            if (rgb != null) files << saveFrames(rgb, cal, new File(overviewTiffPath(outDir, seriesId, c, OVERLAY_SUFFIX)))
            out[c] = [lo: range[0], hi: range[1], files: files, size: W + "x" + H]
        }
        return out
    }

    private static File saveFrames(ImageStack stack, ij.measure.Calibration cal, File f) {
        f.getAbsoluteFile().getParentFile()?.mkdirs()
        def imp = new ImagePlus(f.getName(), stack)
        imp.setDimensions(1, 1, stack.getSize())
        imp.setCalibration(cal)
        try {
            boolean ok = (stack.getSize() > 1) ? new FileSaver(imp).saveAsTiffStack(f.getPath())
                                               : new FileSaver(imp).saveAsTiff(f.getPath())
            if (!ok) throw new IOException("could not write " + f)
        } finally {
            imp.close(); imp.flush()
        }
        return f
    }

    /** <series_id>_overview_ch<c><suffix>.tif -- overviewPath()'s rule, for a multi-frame series. */
    static String overviewTiffPath(String dir, String seriesId, int channel, String suffix = "") {
        overviewPath(dir, seriesId, channel, suffix).replaceFirst(/\.png$/, ".tif")
    }

    // An option's value, or `dflt` when it was not given.
    //
    // NB: not `opts.x ?: dflt`. Groovy's Elvis operator falls back whenever the
    //     left side is FALSE, and 0 is false -- so `saturated: 0`, which means
    //     "clip nothing", silently became 0.35, and `lineWidth: 0` silently became
    //     1 instead of being rejected. Only null and blank mean "not given".
    /**
     * The three settings validators, pulled out of project()/prepare() so that
     * they can be run BEFORE an image is opened.
     *
     * project() and prepare() run at the END of the pipeline, after detection,
     * export and measurement. A typo in the projection name would therefore
     * cost a full detection pass before it was noticed -- once interactively,
     * and 1261 times over a tile-scan batch. Each returns the normalised value
     * so there is one definition of "what that setting means", not two.
     */
    static String projectionMethod(String method) {
        String zp = METHODS[method?.toLowerCase()]
        if (zp == null) {
            throw new IllegalArgumentException(
                "unknown projection '${method}'; use one of ${METHODS.keySet().join(', ')}")
        }
        return zp
    }

    static String contrastMode(Object contrast) {
        String c = (contrast == null || contrast.toString().trim().isEmpty())
                   ? "auto" : contrast.toString().toLowerCase()
        if (!(c in CONTRAST)) {
            throw new IllegalArgumentException("unknown contrast '${contrast}'; use one of ${CONTRAST.join(', ')}")
        }
        return c
    }

    static double saturatedPercent(Object v) {
        double s = (v == null || v.toString().trim().isEmpty()) ? 0.35d : (v as double)
        if (s < 0 || s >= 100) {
            throw new IllegalArgumentException("saturated must be 0 to <100 percent, got ${s}")
        }
        return s
    }

    /**
     * Check a whole set of overview settings without an image.
     *
     * Throws exactly what project()/prepare() would throw, from the same code,
     * so "it validated" and "it will run" cannot come apart.
     */
    static void validateSettings(String method, Object contrast, Object saturated,
                                 Object width, Object height) {
        projectionMethod(method)
        contrastMode(contrast)
        saturatedPercent(saturated)
        sizeOpt(width, "width")
        sizeOpt(height, "height")
    }

    private static Object opt(Map opts, String key, Object dflt) {
        def v = opts[key]
        return (v == null || v.toString().trim().isEmpty()) ? dflt : v
    }

    // A size option: null, "" or 0 mean "not given"; anything else must be a
    // positive whole number.
    private static Integer sizeOpt(Object v, String name) {
        if (v == null || v.toString().trim().isEmpty()) return null
        int n
        try { n = v.toString().trim() as int }
        catch (NumberFormatException e) { throw new IllegalArgumentException("${name} must be a whole number, got '${v}'") }
        if (n < 0) throw new IllegalArgumentException("${name} must not be negative, got ${n}")
        return n == 0 ? null : n
    }

    /**
     * Draw ROIs onto the view's overlay, scaled into the view's coordinates.
     * Call it once per feature; later calls draw on top of earlier ones.
     *
     * @param rois  ROIs in the ORIGINAL image's pixel coordinates, e.g. straight
     *              from detection. They are copied; the caller's ROIs are left
     *              exactly as they were.
     * @param opts  mode      : "all" (default) -- every ROI, so an object spanning
     *                          15 slices shows 15 stacked outlines;
     *                          "merged" -- the union of all ROIs, one outline per
     *                          connected footprint;
     *                          "none" -- draw nothing.
     *              color     : a name ("yellow"), "#rrggbb", or a java.awt.Color.
     *                          Default yellow.
     *              lineWidth : in OUTPUT pixels, i.e. after resizing. Default 1.
     * @return the number of outlines drawn
     *
     * NB on "merged": the union is taken in the 2D projection, not in 3D. Two
     *     objects at different depths whose footprints overlap in x-y become one
     *     outline. That matches what the projection shows, but it is not a count
     *     of objects -- grouping across z is the R side's job.
     *
     * Nothing is burned into pixels here; savePng() does that.
     */
    static int addOutlines(View view, List<Roi> rois, Map opts = [:]) {
        String mode = (opts.mode ?: "all").toString().toLowerCase()
        if (!(mode in OUTLINE_MODES)) {
            throw new IllegalArgumentException("unknown outline mode '${opts.mode}'; use one of ${OUTLINE_MODES.join(', ')}")
        }
        Color color = (opts.color instanceof Color) ? (Color) opts.color
                    : Colors.decode((opts.color ?: "yellow").toString(), null)
        if (color == null) {
            throw new IllegalArgumentException("unknown colour '${opts.color}'; use a name such as yellow, or #rrggbb")
        }
        double lineWidth = opt(opts, "lineWidth", 1d) as double
        if (lineWidth <= 0) throw new IllegalArgumentException("lineWidth must be positive, got ${opts.lineWidth}")

        if (mode == "none" || !rois) return 0

        // Merge in the ORIGINAL coordinates, before scaling: the union is then
        // computed on the detected shapes themselves, not on rounded copies.
        List<Roi> shapes = (mode == "merged") ? union(rois) : rois

        def overlay = view.image.getOverlay() ?: new Overlay()
        shapes.each { Roi r ->
            Roi s = (view.sx == 1d && view.sy == 1d) ? (Roi) r.clone()
                                                     : RoiScaler.scale(r, view.sx, view.sy, false)
            // Detection sets each ROI's position to its slice. flatten() draws it
            // on a one-plane image regardless (checked), but a displayed image
            // filters overlay ROIs by position, so clear it on the copy.
            s.setPosition(0)
            s.setStrokeColor(color)
            s.setStrokeWidth(lineWidth)
            s.setFillColor(null)
            overlay.add(s)
        }
        view.image.setOverlay(overlay)
        return shapes.size()
    }

    // Union of all ROIs, split back into one Roi per connected piece.
    //
    // NB: a lone ROI is returned as itself. ShapeRoi.getRois() on a ShapeRoi
    //     that was never combined with another hands its ROI back at (0,0)
    //     (ImageJ 1.54p; measured), so an image -- or now a frame -- with one
    //     ROI of a feature had that outline drawn in the top-left corner.
    private static List<Roi> union(List<Roi> rois) {
        if (rois.size() == 1) return [(Roi) rois[0].clone()]
        ShapeRoi u = null
        rois.each { Roi r ->
            def s = new ShapeRoi(r)
            u = (u == null) ? s : u.or(s)
        }
        return u.getRois() as List<Roi>
    }

    /**
     * Burn the overlay into the pixels and write an RGB PNG.
     *
     * This is where the display range set by prepare() takes effect: flatten()
     * renders the image as it would be displayed. Missing directories are
     * created.
     */
    static File savePng(View view, String path) {
        def file = new File(path)
        file.getAbsoluteFile().getParentFile()?.mkdirs()
        ImagePlus flat = view.image.flatten()
        try {
            if (!new FileSaver(flat).saveAsPng(file.getPath())) {
                throw new IOException("could not write ${file}")
            }
        } finally {
            flat.close(); flat.flush()   // close() alone frees nothing headless
        }
        return file
    }

    /**
     * The one place the overview file name is decided:
     * <series_id>_overview_ch<c><suffix>.png
     *
     * The suffix exists because the raw projection and the same projection with
     * outlines drawn on it are two different pictures that used to be written to
     * one name, so producing either destroyed the other. The montage builder
     * wants both at once.
     *
     * It goes AFTER the channel so that an empty suffix reproduces the original
     * name exactly, and so `_overview_ch<c>` stays a stable stem.
     *
     * Deciding WHICH suffix is the caller's job, not this function's -- the
     * runners know whether they drew anything. Anything that is not a letter,
     * digit, dot, dash or underscore is replaced, so a suffix typed into a
     * dialog cannot turn into a path separator and scatter files into
     * directories.
     */
    static String overviewPath(String dir, String seriesId, int channel, String suffix = "") {
        String s = (suffix ?: "").trim().replaceAll(/[^A-Za-z0-9._-]/, "_")
        new File(dir, "${seriesId}_overview_ch${channel}${s}.png").getPath()
    }

    /** Original channel numbers of a projection, read from its "ch<c>" labels. */
    static List<Integer> projectedChannels(ImagePlus proj) {
        (1..proj.getStackSize()).collect { int i ->
            def label = proj.getStack().getSliceLabel(i) ?: ""
            def m = label =~ /^ch(\d+)$/
            if (!m) throw new IllegalStateException("plane ${i} is not labelled ch<c>: '${label}'")
            m[0][1] as Integer
        }
    }
}
