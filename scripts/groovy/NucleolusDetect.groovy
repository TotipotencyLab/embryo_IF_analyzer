// NucleolusDetect.groovy
//
// Per-nucleus, per-slice nucleolus detection.
//
// WHY THIS EXISTS
//   The IJ1 macro version (mask_nucloli in nucleus_selector.ijm) could only
//   compute ONE threshold, from ONE arbitrary slice, over the z-FLATTENED union
//   of all nucleus ROIs. That produced two failure modes:
//     - noise detected in empty space, because the flattened footprint of a
//       nucleus at one z fences in background at another z;
//     - whole nuclei detected as nucleoli, because a threshold taken from one
//       slice is wrong on slices where the nucleus is dimmer.
//   Both come from the same root: the ROI Manager is the only way to hold a set
//   of ROIs in the macro language, so per-object iteration is impractical.
//   Here each nucleus is thresholded on its own pixels, on its own slice.
//
// STRUCTURE
//   buildNucleolusMask(...) is pure ImageProcessor arithmetic -- no ROI Manager,
//   no windows -- so it runs and is testable headless. Turning the returned mask
//   into ROIs is left to the caller, because particle extraction is the one step
//   that still needs a display.

import ij.*
import ij.gui.*
import ij.process.*
import ij.measure.Measurements

class NucleolusDetect {

/**
 * Threshold for one nucleus, from that nucleus's own pixels on its own slice.
 *
 * method: an AutoThresholder method name ("Default", "Triangle", "Otsu", ...)
 *         or "Relative" to use mean * relFraction instead.
 *
 * Observed on real embryo DAPI (2026-09-14): "Default" and "Relative" both give
 * sensible nucleoli. "Otsu" and "Triangle" mask essentially the whole nucleus --
 * once the histogram is restricted to a single nucleus it is no longer strongly
 * bimodal, and both methods split nucleoplasm from rim rather than nucleolus
 * from nucleoplasm. Prefer Default, or Relative when you need a threshold that
 * transfers across datasets.
 * Returns the upper bound of the DARK range [min, t] that selects nucleoli.
 */
static double nucleolusThreshold(ImageProcessor ip, Roi roi, String method, double relFraction) {
    ip.setRoi(roi)
    def stats = ImageStatistics.getStatistics(ip, Measurements.MEAN | Measurements.MIN_MAX, null)

    if (method.equalsIgnoreCase("Relative")) {
        return stats.mean * relFraction
    }

    // ImageStatistics gives a 256-bin histogram scaled between histMin/histMax.
    // AutoThresholder returns a BIN INDEX, which must be mapped back to a real
    // pixel value -- for 16-bit data those are not the same number.
    // NB: ImageStatistics.getHistogram() returns long[] for 16-bit data, but
    //     AutoThresholder.getThreshold() only accepts int[]. Convert, clamping
    //     defensively -- a bin count cannot realistically overflow int here, but
    //     an unchecked cast would wrap silently if it did.
    long[] lh = stats.getHistogram()
    int[] hist = new int[lh.length]
    for (int i = 0; i < lh.length; i++) {
        hist[i] = (int) Math.min(lh[i], (long) Integer.MAX_VALUE)
    }

    def at = new AutoThresholder()
    int bin = at.getThreshold(AutoThresholder.Method.valueOf(method), hist)
    double binSize = (stats.histMax - stats.histMin) / 256.0
    return (binSize > 0) ? stats.histMin + (bin + 1) * binSize : stats.histMin
}

/**
 * Build an 8-bit binary mask of nucleoli.
 *
 * dna     : the DNA/DAPI channel as a stack
 * nuclei  : nucleus ROIs; each must carry the slice it belongs to via
 *           roi.getPosition(), or pass positions in `slices` (1-based)
 * Nucleoli are DARK in DAPI, so pixels at or below the per-nucleus threshold
 * and inside that nucleus are marked 255. Nothing outside a nucleus is ever
 * marked, which is what kills the stray-background detections.
 */
static ImagePlus buildNucleolusMask(ImagePlus dna, List<Roi> nuclei, List<Integer> slices,
                                    double sigma, String method, double relFraction) {
    int w = dna.getWidth(), h = dna.getHeight(), n = dna.getStackSize()

    // blurred working copy; never mutate the caller's image
    def work = dna.duplicate()
    if (sigma > 0) {
        def blur = new ij.plugin.filter.GaussianBlur()
        for (int z = 1; z <= n; z++) blur.blurGaussian(work.getStack().getProcessor(z), sigma)
    }

    def maskStack = new ImageStack(w, h)
    for (int z = 1; z <= n; z++) maskStack.addSlice(new ByteProcessor(w, h))

    nuclei.eachWithIndex { roi, i ->
        int z = (slices != null) ? slices[i] : roi.getPosition()
        if (z < 1 || z > n) return                    // ROI with no usable slice

        def src  = work.getStack().getProcessor(z)
        def dest = maskStack.getProcessor(z)
        double t = nucleolusThreshold(src, roi, method, relFraction)

        def b = roi.getBounds()
        def m = roi.getMask()                          // non-rectangular ROIs
        for (int y = 0; y < b.height; y++) {
            for (int x = 0; x < b.width; x++) {
                if (m != null && m.get(x, y) == 0) continue   // outside the ROI
                int px = b.x + x, py = b.y + y
                if (px < 0 || py < 0 || px >= w || py >= h) continue
                if (src.getf(px, py) <= t) dest.set(px, py, 255)
            }
        }
    }

    def out = new ImagePlus(dna.getTitle() + "_nucleolus_mask", maskStack)
    out.setCalibration(dna.getCalibration())
    return out
}
}
