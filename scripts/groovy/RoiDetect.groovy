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

    /** The method name that means "do not compute one, use the range I gave". */
    static final String MANUAL = "Manual"

    /** Not a thresholding algorithm; Auto_Threshold exposes it alongside them. */
    private static final String NOT_A_METHOD = "bilevel"

    /**
     * Every threshold method name this pipeline accepts.
     *
     * Derived from the plugin itself rather than listed, so it cannot drift
     * when Auto_Threshold is next updated: its per-algorithm statics ARE the
     * vocabulary. "Manual" is ours and is the one name the plugin never sees.
     */
    static List<String> methodNames() {
        def names = Auto_Threshold.class.getMethods().findAll {
            java.lang.reflect.Modifier.isStatic(it.getModifiers()) &&
            it.getParameterTypes().length == 1 &&
            it.getParameterTypes()[0] == int[].class &&
            it.getName() != NOT_A_METHOD
        }.collect { it.getName() } as Set
        // The plugin's menu spells two of them differently from its statics,
        // and the menu spelling is what a config file carries.
        names << "Default"
        names << "MinError(I)"
        names << MANUAL
        return names.toList().sort()
    }

    /**
     * Check a threshold request without an image.
     *
     * Called before any work: the alternative is a typo costing a full
     * detection, export and measurement pass before it is noticed -- once
     * interactively, and once per row over a tile-scan batch.
     */
    static void validateThreshold(String method, String rangeSpec) {
        def known = methodNames()
        if (!method || !known.contains(method)) {
            throw new IllegalArgumentException(
                "unknown threshold method '${method}'; use one of " + known.join(", "))
        }
        if (MANUAL.equalsIgnoreCase(method)) {
            if (!rangeSpec?.trim()) {
                throw new IllegalArgumentException(
                    "nucleus_threshold=Manual needs nucleus_threshold_range, " +
                    "e.g. \"1200-65535\" -- there is no sensible default for a raw " +
                    "pixel value")
            }
            // -1 as the sentinel: parseRange substitutes the default for any
            // part it cannot read, so a default that is never valid is how an
            // unparseable range is told apart from an omitted one.
            def r = parseRange(rangeSpec, -1d, -1d)
            if (r[0] < 0d || r[1] < 0d) {
                throw new IllegalArgumentException(
                    "nucleus_threshold_range must be lo-hi, got '${rangeSpec}'")
            }
            if (r[0] > r[1]) {
                throw new IllegalArgumentException(
                    "nucleus_threshold_range has its low end above its high end: '${rangeSpec}'")
            }
        }
    }

    // --- Choosing an automatic threshold --------------------------------------
    //
    // The threshold is chosen HERE, from a histogram this code builds, by the
    // Auto Threshold plugin's per-method statics -- not by its exec(). Two
    // reasons, both about size:
    //
    //   1. exec() sums the stack histogram into an int[], and three of its
    //      methods (Huang, IsoData, Li) accumulate value x count in int, and
    //      MinError(I) forms value x count and value^2 x count per bin in int.
    //      On a large stack those overflow and the threshold is silently wrong
    //      (what was known bug 1: on an oocyte tile merge, Huang 1 where 7
    //      was right, Li 0).
    //   2. A threshold for a whole SERIES (nucleus_threshold_scope = series)
    //      is chosen from every frame's histogram at once -- ~10^10 voxels for a
    //      96-frame Luxendo position, which no int holds.
    //
    // chooseThreshold() is exec()'s own sequence, read from the 1.18.0 bytecode:
    // bilevel() on the whole histogram; else zero the end bins (ignore black,
    // ignore white), trim to the first..last non-empty bin, run the method on
    // that, and add the trim offset back. The histogram is a long[], and the
    // counts are divided by a power of two only as far as the method needs to
    // stay inside int -- recorded as the divisor, 1 meaning exact counts.
    // Test_BuildMask keeps exec() as the oracle wherever nothing overflows.

    /** Methods that sum value x count over the histogram in int. */
    static final Set<String> SUMS_VALUE_X_COUNT  = ["Huang", "IsoData", "Li"] as Set
    /** Methods that form value x count for one bin in int (then widen). */
    static final Set<String> BIN_VALUE_X_COUNT   = ["Default", "IJDefault", "Huang2", "Li",
                                                    "MinError(I)", "MinErrorI"] as Set
    /** Methods that form value^2 x count for one bin in int. */
    static final Set<String> BIN_VALUE2_X_COUNT  = ["MinError(I)", "MinErrorI"] as Set

    /**
     * The histogram of every slice of a stack, or of one slice, as long counts.
     * Bins are the type's own: 256 for 8-bit, 65536 for 16-bit -- what
     * getHistogram() gives and exec() sums.
     */
    static long[] histogramOf(ImagePlus imp, Integer slice = null) {
        def st = imp.getStack()
        long[] acc = null
        def zs = (slice == null) ? (1..st.getSize()) : [slice]
        zs.each { int z ->
            int[] h = st.getProcessor(z).getHistogram()
            if (acc == null) acc = new long[h.length]
            for (int i = 0; i < h.length; i++) acc[i] += h[i]
        }
        return acc
    }

    /** Add one histogram into another, as a series accumulates its frames. */
    static long[] addHistogram(long[] acc, long[] h) {
        if (acc == null) return h.clone()
        if (acc.length != h.length) {
            throw new IllegalArgumentException("histograms of " + acc.length + " and " + h.length +
                                               " bins cannot be added: the frames differ in bit depth")
        }
        for (int i = 0; i < h.length; i++) acc[i] += h[i]
        return acc
    }

    /**
     * The threshold Auto_Threshold.exec() would choose for this histogram, with
     * IGNORE_BLACK and IGNORE_WHITE, and without its overflows.
     *
     * @return [t: the threshold (pixels ABOVE it are objects), or null when the
     *         histogram has nothing to separate; divisor: what the counts were
     *         divided by before the method ran, 1 = exact, null for bilevel or
     *         nothing to separate]
     */
    static Map chooseThreshold(long[] hist, String method) {
        // bilevel(): exactly two values present decides it outright, before
        // the end bins are touched.
        int first = -1, second = -1, present = 0
        for (int i = 0; i < hist.length; i++) {
            if (hist[i] > 0) {
                present++
                if (first < 0) first = i else second = i
            }
        }
        if (present == 2) return [t: second - 1, divisor: null]

        long[] d = hist.clone()
        if (IGNORE_BLACK) d[0] = 0
        if (IGNORE_WHITE) d[d.length - 1] = 0
        int minbin = -1, maxbin = -1
        for (int i = 0; i < d.length; i++) if (d[i] > 0) { if (minbin < 0) minbin = i; maxbin = i }
        if (minbin < 0) return [t: null, divisor: null]        // exec(): ArrayIndexOutOfBounds
        int n = maxbin - minbin + 1
        if (n < 2) return [t: minbin, divisor: 1L]              // exec(): 0 + minbin

        long[] b = new long[n]
        System.arraycopy(d, minbin, b, 0, n)
        long divisor = countDivisor(method, b)
        int[] counts = new int[n]
        for (int i = 0; i < n; i++) counts[i] = (int) ((b[i] + divisor.intdiv(2)).intdiv(divisor))
        return [t: callMethod(method, counts) + minbin, divisor: divisor]
    }

    /**
     * The smallest power of two the counts must be divided by for `method`'s
     * int arithmetic to hold them. Every method is held to the total count and
     * the largest bin fitting in an int; the ones that multiply in int are held
     * to their own products as well. Rounded counts are checked, not ideal ones.
     */
    static long countDivisor(String method, long[] b) {
        long max = Integer.MAX_VALUE
        for (long f = 1L; f <= (1L << 40); f <<= 1) {
            long total = 0L, sumVC = 0L, maxVC = 0L, maxV2C = 0L
            boolean fits = true
            for (int i = 0; i < b.length && fits; i++) {
                long c = (b[i] + f.intdiv(2)).intdiv(f)
                total += c
                sumVC += (long) i * c
                maxVC  = Math.max(maxVC, (long) i * c)
                maxV2C = Math.max(maxV2C, (long) i * i * c)
                fits = total <= max &&
                       (!SUMS_VALUE_X_COUNT.contains(method)  || sumVC  <= max) &&
                       (!BIN_VALUE_X_COUNT.contains(method)   || maxVC  <= max) &&
                       (!BIN_VALUE2_X_COUNT.contains(method)  || maxV2C <= max)
            }
            if (fits) return f
        }
        throw new IllegalStateException("no divisor up to 2^40 keeps " + method + "'s arithmetic in int")
    }

    /** One of the plugin's per-method statics, by the name a config carries. */
    static int callMethod(String method, int[] counts) {
        // The plugin's menu spells two methods differently from its statics.
        String name = (method == "Default") ? "IJDefault"
                    : (method == "MinError(I)") ? "MinErrorI" : method
        def m = Auto_Threshold.class.getMethods().find {
            java.lang.reflect.Modifier.isStatic(it.getModifiers()) && it.getName() == name &&
            it.getParameterTypes().length == 1 && it.getParameterTypes()[0] == int[].class &&
            it.getName() != NOT_A_METHOD
        }
        if (m == null) throw new IllegalArgumentException("unknown threshold method '" + method + "'")
        return (int) m.invoke(null, [counts] as Object[])
    }

    /** The scopes a nucleus threshold can be chosen over, for a multi-frame series. */
    static final List<String> THRESHOLD_SCOPES = ["frame", "series"]

    /**
     * Check the scope against the rest of the threshold request, without an
     * image. `series` pools every slice of every frame into one histogram, so
     * it cannot be combined with a threshold per slice.
     */
    static void validateScope(String scope, String method, boolean stackHistogram) {
        if (!THRESHOLD_SCOPES.contains(scope)) {
            throw new IllegalArgumentException("nucleus_threshold_scope must be one of " +
                                               THRESHOLD_SCOPES.join(", ") + "; got '" + scope + "'")
        }
        if (scope == "series" && !MANUAL.equalsIgnoreCase(method) && !stackHistogram) {
            throw new IllegalArgumentException(
                "nucleus_threshold_scope=series pools every slice of every frame into one histogram, " +
                "and nucleus_stack_histogram=false asks for a threshold per slice; choose one")
        }
    }

    /**
     * One channel of an image, blurred plane by plane: what the nucleus
     * threshold looks at. A new single-channel stack the caller must close.
     */
    static ImagePlus blurredChannel(ImagePlus imp, int channel, double sigma) {
        def ch = new Duplicator().run(imp, channel, channel, 1, imp.getNSlices(), 1, 1)
        // Blurred one plane at a time, NOT with IJ.run(..., "stack").
        //
        // The stack form is parallelised over slices (PARALLELIZE_STACKS), and
        // each worker converts its byte plane to a float one -- 415 MB per
        // thread on the large tile merges, eight of them at once. Measured peak
        // there was 9218 MB against a ceiling that starts at 8889 MB and only
        // grows to 9607 MB: it survived on the garbage collector keeping up,
        // which is a margin that passes in testing and fails on the run that
        // matters. Per slice, one float plane is live at a time.
        //
        // The pixels are identical: measured on 8- and 16-bit synthetic stacks,
        // 0 of 16384 pixels differ from the IJ.run form, because "stack" mode
        // is this same 2D blur applied per plane. Test_BuildMask pins that.
        if (sigma > 0) {
            def blur = new ij.plugin.filter.GaussianBlur()
            def blurStack = ch.getStack()
            for (int z = 1; z <= blurStack.getSize(); z++) {
                blur.blurGaussian(blurStack.getProcessor(z), sigma)
            }
        }
        return ch
    }

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
     * HOW THE THRESHOLD IS CHOSEN
     *
     * By chooseThreshold(), on a histogram built here -- the Auto Threshold
     * plugin's own per-method statics, in exec()'s own sequence, without
     * exec()'s int overflows (see the section above). It RETURNS THE THRESHOLD
     * IT CHOSE, which the macro call this once was threw away, leaving a run
     * unable to say what it had thresholded at.
     *
     * Before this, exec() did it, and before that the macro string; each was
     * measured against the one before on synthesised 8- and 16-bit stacks, and
     * Test_BuildMask keeps both as oracles: where nothing overflows, the same
     * pixels are selected. The mask is always 8-bit 0/255 (applyRange), which
     * is what detect()'s setThreshold(128, 255), Fill Holes and Watershed
     * assume.
     *
     * opts.chosen: a threshold already chosen (chooseThreshold's result) -- a
     * series' threshold, chosen once over every frame -- applied here instead
     * of one chosen from this stack. The result's `divisor` says what the
     * histogram counts were divided by to keep the method's arithmetic in int:
     * 1 for exact counts, null where no method ran.
     */
    static Map buildMask(ImagePlus imp, int channel, double sigma, String method,
                         boolean fillHoles, boolean watershed, Map opts = [:]) {
        String rangeSpec      = (opts.range ?: "") as String
        boolean stackHistogram = (opts.stackHistogram == null) ? true : (opts.stackHistogram as boolean)
        validateThreshold(method, rangeSpec)

        def mask = blurredChannel(imp, channel, sigma)
        int maxValue = (imp.getBitDepth() == 16) ? 65535 : 255

        // --- Manual: no algorithm runs at all ------------------------------
        if (MANUAL.equalsIgnoreCase(method)) {
            def r  = parseRange(rangeSpec, 0d, (double) maxValue)
            int lo0 = Math.max(0, Math.min(maxValue, (int) Math.round(r[0])))
            int hi0 = Math.max(0, Math.min(maxValue, (int) Math.round(
                          (r[1] >= Double.MAX_VALUE) ? (double) maxValue : r[1])))
            // Applied by hand rather than through Convert to Mask, which reads
            // Prefs.blackBackground to decide which phase is object and would
            // produce an inverted mask under the wrong operator setting -- the
            // same trap watershed has. This reads no preference at all.
            applyRange(mask, lo0, hi0)
            double cov0 = coveragePct(mask)
            if (fillHoles) IJ.run(mask, "Fill Holes", "stack")
            if (watershed) { Prefs.blackBackground = true; IJ.run(mask, "Watershed", "stack") }
            return [mask: mask, threshold: lo0 + "-" + hi0, lo: lo0, hi: hi0, coverage: cov0]
        }

        // --- Per slice: exec() thresholds ONE slice, so the loop is ours ----
        // The plugin's own run() does this loop; exec() does not. With a
        // per-slice threshold an empty slice has nothing bimodal to work with
        // and its noise becomes objects, which is why the stack histogram is
        // the default.
        if (!stackHistogram) {
            // One threshold per slice, each from that slice's histogram alone.
            def perSlice = (1..mask.getStackSize()).collect { int z ->
                chooseThreshold(histogramOf(mask, z), method) }
            // A slice with nothing to separate is blanked rather than left with
            // its raw pixels standing in for a mask.
            applyRanges(mask, perSlice.collect { it.t == null ? null : (it.t as int) + 1 }, maxValue)
            double covN = coveragePct(mask)
            def los = perSlice.findAll { it.t != null }.collect { (it.t as int) + 1 }
            // A spread, not a range: there were as many thresholds as slices, so
            // there is no single value to paste into a manual setting. The ".."
            // says so at a glance.
            String rep = los.isEmpty() ? NO_THRESHOLD
                                       : ("per-slice " + los.min() + ".." + los.max())
            def divs = perSlice.findAll { it.divisor != null }.collect { it.divisor as long }
            if (fillHoles) IJ.run(mask, "Fill Holes", "stack")
            if (watershed) { Prefs.blackBackground = true; IJ.run(mask, "Watershed", "stack") }
            return [mask: mask, threshold: rep, lo: null, hi: null, coverage: covN,
                    divisor: (divs ? divs.max() : null)]
        }

        // One threshold for the stack: from its own pooled histogram, or the one
        // a caller chose over a whole series (opts.chosen, chooseThreshold's
        // result) -- applied the same way either way.
        def chosen = (opts.chosen != null) ? (Map) opts.chosen : chooseThreshold(histogramOf(mask), method)

        Integer lo = null, hi = null
        String thresholdUsed = NO_THRESHOLD
        if (chosen.t == null) {
            // NOTHING TO SEPARATE. ignore_black and ignore_white zero the two
            // end bins before the algorithm runs, so a frame whose pixels are
            // ALL pure black or all saturated leaves an empty histogram (exec()
            // threw ArrayIndexOutOfBounds there).
            //
            // This is not hypothetical on a slide that scans across sections --
            // a field of blank mounting medium is exactly this.
            //
            // The old IJ.run() route was the dangerous one: ImageJ's Executer
            // CAUGHT the plugin's exception, logged it, and returned -- leaving
            // the image UNTHRESHOLDED, after which buildMask handed back the
            // raw pixels as if they were a mask. Measured on an all-255 frame:
            // 1600/1600 pixels "on"; on an all-black frame, 0/1600. Both looked
            // like a successful run. No threshold separates a uniform frame, so
            // nothing is selected and the caller is told so by name.
            blank(mask)
        } else {
            int t = chosen.t as int
            // The range as APPLIED, not the bare number: "white" objects are the
            // pixels ABOVE the threshold, so the algorithm's t is the bottom of
            // the selected range and the top is whatever the type can hold.
            // Recording the pair is what makes the value copy-pasteable into a
            // manual threshold later without anyone having to work out which end
            // it was.
            lo = t + 1
            hi = maxValue
            applyRange(mask, lo, hi)
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
        return [mask: mask, threshold: thresholdUsed, lo: lo, hi: hi, coverage: coverage,
                divisor: chosen.divisor]
    }

    /**
     * Mark pixels in [lo, hi] as 255 and everything else 0, as an 8-bit stack.
     *
     * ⚠️ MUTATES THE STACK IT IS GIVEN. Every caller is buildMask, and the stack
     * it passes is buildMask's own Duplicator copy, which nothing else holds.
     * Do not call this on an image you did not just duplicate.
     *
     * WHY IN PLACE
     *   The old version built a second full stack while the first was still
     *   referenced by the loop, so both were live from slice 1 onward. On an
     *   11344 x 9590 x 25 tile merge that is a third complete copy of the pixel
     *   data (2594 MB) arriving at the worst possible moment -- the original
     *   image is still held because both channels are measured after the mask is
     *   built -- and it was where the run died with OutOfMemoryError. On 8-bit
     *   input the copy bought nothing whatsoever: 8-bit in, 8-bit out.
     *   Measured peak for that series: 10375 MB required against ~9607 MB
     *   available; in place it is 7781 MB.
     */
    static void applyRange(ImagePlus imp, int lo, int hi) {
        applyRanges(imp, (1..imp.getStackSize()).collect { lo }, hi)
    }

    /**
     * applyRange() with its own low end per slice; a null low end blanks that
     * slice. The same in-place rules.
     */
    static void applyRanges(ImagePlus imp, List<Integer> los, int hi) {
        if (imp.getBitDepth() == 8) {
            def src = imp.getStack()
            for (int z = 1; z <= src.getSize(); z++) {
                // getPixels() hands back the LIVE array, so this edits the stack.
                byte[] px = (byte[]) src.getProcessor(z).getPixels()
                Integer loZ = los[z - 1]
                int lo = (loZ == null) ? Integer.MAX_VALUE : loZ
                for (int i = 0; i < px.length; i++) {
                    int v = px[i] & 0xff
                    // ⚠️ The else branch is not optional. The old code started
                    // from a fresh ByteProcessor -- already all zero -- and only
                    // ever wrote 255. In place, a pixel outside the range keeps
                    // its original grey value unless it is written, and detect()
                    // puts setThreshold(128, 255) on the result: the background
                    // would be the raw image and the ROIs would look plausible.
                    // Assigning unconditionally makes that impossible to forget.
                    px[i] = (byte) ((v >= lo && v <= hi) ? 255 : 0)
                }
            }
            return
        }
        replaceWith8Bit(imp) { int z, int v -> los[z - 1] != null && v >= los[z - 1] && v <= hi }
    }

    /** Zero one slice in place, leaving the rest of the stack alone. */
    static void blankSlice(ImagePlus imp, int z) {
        def ip = imp.getStack().getProcessor(z)
        for (int y = 0; y < imp.getHeight(); y++) {
            for (int x = 0; x < imp.getWidth(); x++) ip.set(x, y, 0)
        }
    }

    /** Replace a stack with an empty 8-bit mask of the same shape. */
    static void blank(ImagePlus imp) {
        if (imp.getBitDepth() == 8) {
            def src = imp.getStack()
            for (int z = 1; z <= src.getSize(); z++) {
                java.util.Arrays.fill((byte[]) src.getProcessor(z).getPixels(), (byte) 0)
            }
            return
        }
        replaceWith8Bit(imp) { int z, int v -> false }
    }

    /**
     * Force a thresholded stack to 8-bit 0/255, whatever it arrived as.
     *
     * Any non-zero pixel is "on". A 16-bit threshold result is 0/65535, and
     * everything downstream -- detect()'s setThreshold(128, 255), Fill Holes,
     * Watershed -- assumes the 8-bit form.
     *
     * The 8-bit early return is deliberate and is NOT the same as running the
     * mapping: it leaves an already-8-bit stack exactly as it arrived, values
     * and all, rather than forcing every non-zero pixel to 255.
     */
    static void to8BitMask(ImagePlus imp) {
        if (imp.getBitDepth() == 8) { return }
        replaceWith8Bit(imp) { int z, int v -> v != 0 }
    }

    /**
     * Rebuild a non-8-bit stack as an 8-bit 0/255 mask, one plane at a time,
     * releasing each source plane as soon as it has been read.
     *
     * ⚠️ ImageStack.setProcessor() CANNOT be used to swap a ByteProcessor into a
     * 16-bit stack. It accepts the call without complaint and then CONVERTS:
     * measured on IJ 1.54p, the stored array is still short[], is not the array
     * that was handed in, and the stack still reports 16-bit. A plane-swap
     * written that way allocates a short plane per slice -- no saving at all --
     * and yields a 16-bit mask whose "on" value is 255, which detect() would
     * still turn into plausible-looking ROIs. That is why this releases through
     * setPixels(null, z) instead, which stores what it is given.
     *
     * The release is what keeps the peak down: the source shrinks by one plane
     * for every plane the output grows by, so the two together never exceed the
     * source stack's original size. The old form held both stacks whole.
     */
    private static void replaceWith8Bit(ImagePlus imp, Closure<Boolean> isOn) {
        int w = imp.getWidth(), h = imp.getHeight()
        def src = imp.getStack()
        def out = new ij.ImageStack(w, h)
        for (int z = 1; z <= src.getSize(); z++) {
            def ip = src.getProcessor(z)
            def bp = new ij.process.ByteProcessor(w, h)
            byte[] px = (byte[]) bp.getPixels()
            int i = 0
            for (int y = 0; y < h; y++) {
                for (int x = 0; x < w; x++, i++) {
                    if (isOn(z, ip.get(x, y))) px[i] = (byte) 255
                }
            }
            out.addSlice(src.getSliceLabel(z), bp)
            src.setPixels(null, z)   // this plane is garbage from here on
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
