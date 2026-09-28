// RoiDetect.groovy
//
// Particle detection that returns ROIs without the ROI Manager.
//
// ParticleAnalyzer itself runs fine headless; it is only its OUTPUT routing that
// does not (SHOW_OVERLAY_OUTLINES exhausts the heap, the ROI Manager throws
// HeadlessException). Overriding saveResults() intercepts each particle as it is
// found, before any routing happens, so no display is ever required.

import ij.*
import ij.gui.Roi
import ij.measure.Measurements
import ij.measure.ResultsTable
import ij.process.ImageProcessor
import ij.process.ImageStatistics
import ij.plugin.Duplicator
import ij.plugin.filter.ParticleAnalyzer
import fiji.threshold.Auto_Threshold

class RoiDetect {

    /** ParticleAnalyzer that collects the ROIs it finds instead of routing them. */
    static class CollectingPA extends ParticleAnalyzer {
        List<Roi> found = []
        int currentSlice = 1
        CollectingPA(int opts, int meas, ResultsTable rt, double mn, double mx,
                     double minCirc, double maxCirc) {
            super(opts, meas, rt, mn, mx, minCirc, maxCirc)
        }
        @Override protected void saveResults(ImageStatistics stats, Roi roi) {
            super.saveResults(stats, roi)
            def r = (Roi) roi.clone()
            r.setPosition(currentSlice)
            found << r
        }
    }

    /** Histogram bins excluded before the algorithm sees them. Both are ON, and
     *  were ON as literals in the option string before they had names: a frame
     *  padded with zeros or clipped to saturation would otherwise decide the
     *  threshold by sheer bin count. */
    static final boolean IGNORE_BLACK = true
    static final boolean IGNORE_WHITE = true

    /** Reported as the threshold when a frame has nothing to separate. A word,
     *  not a range, so it cannot be mistaken for one or pasted into a manual
     *  setting. See buildMask() for when it happens and why it is not blank --
     *  blank already means "this row never ran". */
    static final String NO_THRESHOLD = "none"

    /**
     * Build a binary mask from one channel: blur, auto-threshold, optionally fill
     * holes, and optionally split touching objects by watershed.
     *
     * Returns a Map -- the mask is a NEW ImagePlus the caller must close, and
     * the source is untouched:
     *
     *   mask      the 8-bit 0/255 binary stack
     *   lo, hi    the pixel range the threshold SELECTED, as applied
     *   coverage  percent of pixels inside that range, measured on the raw
     *             threshold result BEFORE fill holes and watershed, because the
     *             question it answers is "what did the threshold choose"
     *
     * Watershed splits objects that threshold into a single blob but are two
     * things -- a zygote's two pronuclei, oocytes packed together in a section.
     * It is off by default: with watershed=false this reproduces the previous
     * inline mask step exactly, so turning it on is the only thing that can
     * change existing results.
     *
     * WHY exec() RATHER THAN IJ.run("Auto Threshold", ...)
     *
     * It is the same plugin, reached through its API instead of its macro
     * recorder string, and it RETURNS THE THRESHOLD IT CHOSE. That number was
     * previously thrown away, which meant a run could not say what it had
     * thresholded at -- no way to tell a sensible threshold from a disastrous
     * one after the fact, and no way to read off a value in order to pin it.
     *
     * Measured against the old call on synthesised 8- and 16-bit stacks, four
     * methods each (Test_BuildMask keeps the old call as the oracle):
     *
     *   8-bit    byte-identical
     *   16-bit   the same pixels selected, but exec() leaves a 16-bit 0/65535
     *            mask where the macro path converts to 8-bit 0/255
     *
     * Hence to8BitMask(). Without it a 16-bit run would produce a mask whose
     * "on" value is 65535, and detect() sets a threshold of 128-255 on it --
     * the ROIs would be right, by luck, and the mask would be wrong.
     *
     * ⚠️ exec() thresholds only the CURRENT SLICE unless the stack histogram is
     * used; the slice loop lives in the plugin's run(), not in exec(). This
     * passes true, as the option string did.
     */
    static Map buildMask(ImagePlus imp, int channel, double sigma, String method,
                         boolean fillHoles, boolean watershed) {
        def mask = new Duplicator().run(imp, channel, channel, 1, imp.getNSlices(), 1, 1)
        if (sigma > 0) IJ.run(mask, "Gaussian Blur...", "sigma=${sigma} stack")

        // exec(imp, method, noWhite, noBlack, doIwhite, doIset, doIlog, doIstackHistogram)
        def out = null
        boolean degenerate = false
        try {
            out = new Auto_Threshold().exec(mask, method, IGNORE_WHITE, IGNORE_BLACK,
                                            true, false, false, true)
        } catch (ArrayIndexOutOfBoundsException e) {
            // NOTHING TO SEPARATE. ignore_black and ignore_white zero the two
            // end bins before the algorithm runs, so a frame whose pixels are
            // ALL pure black or all saturated leaves an empty histogram and the
            // plugin's min/max bin search returns -1.
            //
            // This is not hypothetical on a slide that scans across sections --
            // a field of blank mounting medium is exactly this -- and it is the
            // reason the exception is caught rather than left to fail the row.
            //
            // ⚠️ THIS IS A BEHAVIOUR CHANGE, and the old behaviour was the
            //    dangerous one. IJ.run() routes through ImageJ's Executer,
            //    which CATCHES the plugin's exception, logs it, and returns --
            //    leaving the image UNTHRESHOLDED, after which buildMask handed
            //    back the raw pixels as if they were a mask. Measured on an
            //    all-255 frame: IJ.run left 1600/1600 pixels "on"; on an
            //    all-black frame, 0/1600. Both looked like a successful run.
            //
            // The honest answer is that no threshold separates a uniform frame,
            // so nothing is selected and the caller is told so by name.
            degenerate = true
        }
        if (!degenerate && (out == null || out[0] == null)) {
            throw new IllegalArgumentException(
                "Auto Threshold returned nothing for method '${method}'; " +
                "it must be one of the Auto Threshold plugin's own names")
        }

        Integer lo = null, hi = null
        String thresholdUsed = NO_THRESHOLD
        if (degenerate) {
            blank(mask)
        } else {
            int t = (out[0] as Number).intValue()
            to8BitMask(mask)
            // The range as APPLIED, not the bare number: "white" objects are the
            // pixels ABOVE the threshold, so the algorithm's t is the bottom of
            // the selected range and the top is whatever the type can hold.
            // Recording the pair is what makes the value copy-pasteable into a
            // manual threshold later without anyone having to work out which end
            // it was.
            lo = t + 1
            hi = (imp.getBitDepth() == 16) ? 65535 : 255
            thresholdUsed = lo + "-" + hi
        }
        double coverage = coveragePct(mask)

        if (fillHoles) IJ.run(mask, "Fill Holes", "stack")

        if (watershed) {
            // NB: Watershed reads Prefs.blackBackground to decide which phase is
            //     object and which is background. Left to whatever the operator
            //     happens to have ticked, it will erode the background instead of
            //     splitting the objects -- and produce a plausible-looking mask
            //     while doing it. Forced here for the same reason Set Measurements
            //     is forced. This overwrites the preference, and it persists.
            Prefs.blackBackground = true
            // Holes are filled first on purpose: watershed cuts through an unfilled
            // hole and shatters one object into a ring of fragments.
            IJ.run(mask, "Watershed", "stack")
        }
        return [mask: mask, threshold: thresholdUsed, lo: lo, hi: hi, coverage: coverage]
    }

    /** Replace a stack with an empty 8-bit mask of the same shape. */
    static void blank(ImagePlus imp) {
        def out = new ij.ImageStack(imp.getWidth(), imp.getHeight())
        for (int z = 1; z <= imp.getStackSize(); z++) {
            out.addSlice(imp.getStack().getSliceLabel(z),
                         new ij.process.ByteProcessor(imp.getWidth(), imp.getHeight()))
        }
        imp.setStack(out)
    }

    /**
     * Force a thresholded stack to 8-bit 0/255, whatever it arrived as.
     *
     * Any non-zero pixel is "on". A 16-bit threshold result is 0/65535, and
     * everything downstream -- detect()'s setThreshold(128, 255), Fill Holes,
     * Watershed -- assumes the 8-bit form.
     */
    static void to8BitMask(ImagePlus imp) {
        if (imp.getBitDepth() == 8) { return }
        def src = imp.getStack()
        def out = new ij.ImageStack(imp.getWidth(), imp.getHeight())
        for (int z = 1; z <= src.getSize(); z++) {
            def ip = src.getProcessor(z)
            def bp = new ij.process.ByteProcessor(imp.getWidth(), imp.getHeight())
            for (int y = 0; y < imp.getHeight(); y++) {
                for (int x = 0; x < imp.getWidth(); x++) {
                    if (ip.get(x, y) != 0) bp.set(x, y, 255)
                }
            }
            out.addSlice(src.getSliceLabel(z), bp)
        }
        imp.setStack(out)
    }

    /**
     * Percent of pixels that are "on" across the whole stack.
     *
     * The cheapest signal there is that a threshold went wrong, and it catches
     * both directions: 0.00 means it selected nothing (a blank field, or a
     * manual value above everything present), and a number in the tens means it
     * selected the frame rather than the objects in it. Neither shows up in an
     * ROI count, because the size filter turns both into "no nuclei".
     */
    static double coveragePct(ImagePlus imp) {
        long on = 0L, total = 0L
        def st = imp.getStack()
        for (int z = 1; z <= st.getSize(); z++) {
            def ip = st.getProcessor(z)
            for (int y = 0; y < imp.getHeight(); y++) {
                for (int x = 0; x < imp.getWidth(); x++) {
                    if (ip.get(x, y) != 0) on++
                    total++
                }
            }
        }
        return (total == 0L) ? 0.0d : (100.0d * on / total)
    }

    /**
     * Detect particles in a thresholded/binary stack.
     *
     * sizeRange: "80-Infinity" -- in CALIBRATED units (um^2) when the image is
     *            calibrated, matching the Analyze Particles dialog.
     * circRange: "0.50-1.00"
     * slices:    which z-slices to analyse; null means all.
     */
    static List<Roi> detect(ImagePlus binary, String sizeRange, String circRange,
                            Set<Integer> slices, boolean excludeEdges, boolean includeHoles) {
        def (double minSize, double maxSize) = parseRange(sizeRange, 0d, Double.MAX_VALUE)
        def (double minCirc, double maxCirc) = parseRange(circRange, 0d, 1d)

        // NB: ParticleAnalyzer's constructor filters in PIXELS, even though it
        //     REPORTS Area in calibrated units. Verified: a 1257 px^2 / 12.57 um^2
        //     blob passes minSize=100 and fails minSize=2000. The dialog accepts
        //     calibrated units, so convert here or the filter is silently wrong
        //     by a factor of 1/pixelArea.
        def cal = binary.getCalibration()
        double pxArea = (cal != null && cal.pixelWidth > 0 && cal.pixelHeight > 0) ?
                        cal.pixelWidth * cal.pixelHeight : 1.0d
        double minPx = (minSize <= 0d) ? 0d : minSize / pxArea
        double maxPx = (maxSize >= Double.MAX_VALUE) ? Double.MAX_VALUE : maxSize / pxArea

        int opts = ParticleAnalyzer.SHOW_NONE
        if (excludeEdges) opts |= ParticleAnalyzer.EXCLUDE_EDGE_PARTICLES
        if (includeHoles) opts |= ParticleAnalyzer.INCLUDE_HOLES

        def rt = new ResultsTable()
        // NB: let ParticleAnalyzer apply the circularity filter via its 7-arg
        //     constructor. Computing it afterwards from roi.getStatistics().area
        //     (pixels) and roi.getLength() (calibrated when an image is attached)
        //     mixes units and filters on a meaningless number.
        def pa = new CollectingPA(opts, Measurements.AREA | Measurements.SHAPE_DESCRIPTORS,
                                  rt, minPx, maxPx, minCirc, maxCirc)
        pa.setHideOutputImage(true)

        int n = binary.getStackSize()
        for (int z = 1; z <= n; z++) {
            if (slices != null && !slices.contains(z)) continue
            pa.currentSlice = z
            def ip = binary.getStack().getProcessor(z)
            // NB: set the threshold on the PROCESSOR and pass it explicitly.
            //     Going through imp.setSlice() loses the threshold, after which
            //     the analyzer can try to trace the whole frame and blow the heap.
            ip.setThreshold(128, 255, ImageProcessor.NO_LUT_UPDATE)
            pa.analyze(binary, ip)
        }

        return pa.found
    }

    /**
     * Reproduce the ROI Manager's auto-label: SSSS-NNNN-YYYY, i.e. slice number,
     * per-slice index, and the y-centre of the ROI bounds. Derived empirically
     * from existing output; the downstream R (read_fiji_result.r) identifies the
     * roi column by matching \\d{4}-\\d{4}-\\d{4}$, so the shape must be kept.
     */
    static List<String> autoLabels(List<Roi> rois) {
        def perSlice = [:]
        return rois.collect { roi ->
            int z = roi.getPosition()
            int idx = (perSlice[z] = (perSlice[z] ?: 0) + 1)
            def b = roi.getBounds()
            int yc = b.y + (int) (b.height / 2)
            return String.format("%04d-%04d-%04d", z, idx, yc)
        }
    }

    /** "80-Infinity" / "0.50-1.00" / "3-150" -> [min, max] */
    static List<Double> parseRange(String spec, double dflMin, double dflMax) {
        if (!spec?.trim()) return [dflMin, dflMax]
        def parts = spec.trim().split("-")
        if (parts.length < 2) return [asNum(parts[0], dflMin), dflMax]
        return [asNum(parts[0], dflMin), asNum(parts[1], dflMax)]
    }

    private static double asNum(String s, double dfl) {
        s = s?.trim()
        if (!s) return dfl
        if (s.equalsIgnoreCase("Infinity")) return Double.MAX_VALUE
        try { return Double.parseDouble(s) } catch (ignored) { return dfl }
    }

    /**
     * "1-20,35-40" / "5" / "10-Infinity" / "" -> the set of slices to analyse.
     * Blank means every slice. Out-of-range values are clamped, not an error.
     */
    static Set<Integer> parseSlices(String spec, int nSlices) {
        if (!spec?.trim()) return (1..nSlices) as TreeSet
        def out = new TreeSet<Integer>()
        spec.split(",").each { String part ->
            part = part.trim()
            if (!part) return
            if (part.contains("-")) {
                def seg = part.split("-", 2)
                int lo = (int) asNum(seg[0], 1d)
                double hiRaw = asNum(seg.length > 1 ? seg[1] : null, (double) nSlices)
                int hi = (hiRaw >= nSlices) ? nSlices : (int) hiRaw
                for (int i = Math.max(1, lo); i <= Math.min(nSlices, hi); i++) out << i
            } else {
                int v = (int) asNum(part, 0d)
                if (v >= 1 && v <= nSlices) out << v
            }
        }
        return out.isEmpty() ? ((1..nSlices) as TreeSet) : out
    }
}
