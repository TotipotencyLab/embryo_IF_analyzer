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
import ij.plugin.filter.ParticleAnalyzer

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
