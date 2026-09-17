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
//   addOutlines()  draw ROIs onto an overlay, scaled to the view   (next)
//   savePng()      burn the overlay into pixels and write a PNG    (next)
//
// Why not just call ZProjector: it projects one continuous range. Detection
// accepts gapped ranges ("1-20,35-40"), and an overview has to show what
// detection saw, so the chosen planes are gathered into a substack first.

import ij.*
import ij.plugin.ContrastEnhancer
import ij.plugin.ZProjector
import ij.process.ImageProcessor

class Overview {

    /**
     * One channel of a projection, ready to draw on and save.
     *
     * `sx` / `sy` are the resize factors from the projection to this image.
     * addOutlines() needs them: ROIs are in the original pixel coordinates and
     * have to be scaled by the same amount to land in the right place.
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

        String toString() {
            "View(ch${channel}, ${image.getWidth()}x${image.getHeight()}, scale ${sx}x${sy})"
        }
    }

    static final List<String> CONTRAST = ["auto", "none"]

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
        String zpMethod = METHODS[method?.toLowerCase()]
        if (zpMethod == null) {
            throw new IllegalArgumentException(
                "unknown projection '${method}'; use one of ${METHODS.keySet().join(', ')}")
        }

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
        String contrast = (opts.contrast ?: "auto").toString().toLowerCase()
        if (!(contrast in CONTRAST)) {
            throw new IllegalArgumentException("unknown contrast '${opts.contrast}'; use one of ${CONTRAST.join(', ')}")
        }
        double saturated = (opts.saturated ?: 0.35) as double

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
        out.setMinAndMax(lo, hi)

        double sx = w / (double) W, sy = h / (double) H
        def img = new ImagePlus(proj.getTitle() + "_ch${channel}", out)
        def cal = proj.getCalibration().copy()
        cal.pixelWidth  = cal.pixelWidth  / sx      // fewer, larger pixels
        cal.pixelHeight = cal.pixelHeight / sy
        img.setCalibration(cal)

        return new View(image: img, sx: sx, sy: sy, channel: channel)
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
