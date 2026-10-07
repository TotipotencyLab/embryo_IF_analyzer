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
//   OV.savePng(view, OV.overviewPath(outDir, basename, dnaCh))
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
import ij.process.LUT
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
     * montage_qc_cli.r is handed these names by hand, so a mismatch would only
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
        double[] range = displayRange(ip, contrast, saturated)
        double lo = range[0], hi = range[1]

        // Output size.
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
     * The display window a contrast setting chooses for one plane: [lo, hi].
     *
     * One definition shared by the PNG (prepare) and the TIFF (composite), so
     * the window a TIFF channel opens with is the one batch_summary.tsv's
     * display_range reports for the PNG of the same channel.
     *
     * Sets `ip`'s own min/max as a side effect (stretchHistogram works that
     * way); pass a copy if that matters. Pixel values are not changed.
     */
    static double[] displayRange(ImageProcessor ip, String contrast, double saturated) {
        if (contrast == "auto") {
            // NB: for 32-bit data the stretch works on a 256-bin histogram laid
            //     over the FULL min-max, so its limits snap to bin edges. With
            //     extreme outliers the bins get coarse: a 0-995 ramp plus three
            //     pixels of 100000 gets a top of 781, not ~995, and the brightest
            //     fifth saturates. Harmless unless outliers are hundreds of times
            //     brighter than the signal.
            new ContrastEnhancer().stretchHistogram(ip, saturated)
            return [ip.getMin(), ip.getMax()] as double[]
        } else if (ip.getBitDepth() == 32) {
            ip.resetMinAndMax()
            return [ip.getMin(), ip.getMax()] as double[]
        }
        return [0d, (ip.getBitDepth() == 16) ? 65535d : 255d] as double[]
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

    /**
     * addOutlines() onto a full-resolution image rather than a resized view:
     * the overview TIFF, whose pixels are the projection's own (no scaling).
     */
    static int addOutlines(ImagePlus img, List<Roi> rois, Map opts) {
        return addOutlines(new View(image: img, sx: 1d, sy: 1d, channel: 0), rois, opts)
    }

    // Union of all ROIs, split back into one Roi per connected piece.
    private static List<Roi> union(List<Roi> rois) {
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
     * Default TIFF channel colours, in channel order: the order Fiji's own
     * composites use. Written out rather than left to CompositeImage so the
     * TIFF does not change with the ImageJ version -- or with how the series
     * was opened: the importer may carry the acquisition's colours and the
     * reader path carries none, so inheriting them would colour one series two
     * ways depending on openMode.
     */
    static final List<String> DEFAULT_COLOURS = ["red", "green", "blue", "gray", "cyan", "magenta", "yellow"]

    /**
     * Channel colours from a comma-separated list of names or #rrggbb. Blank
     * means DEFAULT_COLOURS. Checked before any image is opened, so a typo
     * costs one message, not one per row. Whether the count matches the
     * channels can only be known per image (composite() checks it).
     */
    static List<Color> channelColours(Object spec) {
        String t = (spec == null) ? "" : spec.toString().trim()
        List<String> names = t ? t.split(",").collect { it.trim() } : DEFAULT_COLOURS
        return names.collect { String n ->
            Color c = n ? Colors.decode(n, null) : null
            if (c == null) {
                throw new IllegalArgumentException("unknown colour '${n}' in '${spec}'; use names such as blue,green, or #rrggbb")
            }
            c
        }
    }

    /**
     * The projection as ONE multi-channel image: full resolution, calibrated,
     * pixel values untouched -- what Image > Stacks > Z Project gives in the
     * GUI. Each channel opens at the display window displayRange() chooses, so
     * the file looks right without its pixels being changed.
     *
     * @param proj     output of project()
     * @param opts     contrast, saturated : as prepare()
     *                 colours  : List<Color> from channelColours(), or null for
     *                            the defaults. Given explicitly, it must have one
     *                            colour per projected channel; the defaults are
     *                            taken in order and must simply be enough.
     * @return [image: ImagePlus (a CompositeImage for 2+ channels), ranges:
     *         List<double[]>, one per channel in projection order]
     *
     * NB: the image SHARES the projection's planes -- a tile merge's projection
     *     is hundreds of MB, and a copy would be another. Use the projection for
     *     anything else (prepare() copies what it needs) BEFORE flushing either.
     */
    static Map composite(ImagePlus proj, Map opts = [:]) {
        def chans = projectedChannels(proj)
        int n = chans.size()
        String contrast = contrastMode(opts.contrast)
        double saturated = saturatedPercent(opts.saturated)
        List<Color> cols = (List<Color>) opts.colours
        if (cols == null) {
            cols = channelColours(null)
            if (n > cols.size()) {
                throw new IllegalArgumentException("${n} channels and only ${cols.size()} default colours; give the colours")
            }
            cols = cols.take(n)
        } else if (cols.size() != n) {
            throw new IllegalArgumentException(
                "${cols.size()} colour(s) given for ${n} projected channel(s) ${chans}; give one per channel")
        }

        def stack = proj.getStack()
        List<double[]> ranges = (1..n).collect { int i ->
            // A copy: stretchHistogram sets the plane's min/max.
            displayRange(stack.getProcessor(i).duplicate(), contrast, saturated)
        }

        def img = new ImagePlus(proj.getTitle(), stack)
        img.setDimensions(n, 1, 1)
        img.setCalibration(proj.getCalibration().copy())
        ImagePlus out
        if (n > 1) {
            def ci = new CompositeImage(img, IJ.COMPOSITE)
            (0..<n).each { int i ->
                LUT lut = LUT.createLutFromColor(cols[i])
                lut.min = ranges[i][0]; lut.max = ranges[i][1]
                ci.setChannelLut(lut, i + 1)
            }
            // setChannelLut does not move the current channel's processor
            // range; set it too so channel 1 opens at its own window.
            ci.setPositionWithoutUpdate(1, 1, 1)
            ci.getProcessor().setMinAndMax(ranges[0][0], ranges[0][1])
            out = ci
        } else {
            LUT lut = LUT.createLutFromColor(cols[0])
            img.getProcessor().setColorModel(lut)
            // After setColorModel, which resets the range (see prepare()).
            img.setDisplayRange(ranges[0][0], ranges[0][1])
            out = img
        }
        out.setCalibration(proj.getCalibration().copy())
        return [image: out, ranges: ranges]
    }

    /** <basename>_overview.tif -- one file, every projected channel. */
    static String tiffPath(String dir, String basename) {
        new File(dir, "${basename}_overview.tif").getPath()
    }

    /**
     * Write an image as TIFF, overlay included (ImageJ stores it in the file;
     * Image > Overlay > Hide/Show toggles it, To ROI Manager lists it).
     */
    static File saveTiff(ImagePlus img, String path) {
        def file = new File(path)
        file.getAbsoluteFile().getParentFile()?.mkdirs()
        if (!new FileSaver(img).saveAsTiff(file.getPath())) {
            throw new IOException("could not write ${file}")
        }
        return file
    }

    /**
     * The one place the overview file name is decided:
     * <basename>_overview_ch<c><suffix>.png
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
    static String overviewPath(String dir, String basename, int channel, String suffix = "") {
        String s = (suffix ?: "").trim().replaceAll(/[^A-Za-z0-9._-]/, "_")
        new File(dir, "${basename}_overview_ch${channel}${s}.png").getPath()
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
