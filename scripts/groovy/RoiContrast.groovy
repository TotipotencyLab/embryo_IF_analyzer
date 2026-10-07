// RoiContrast.groovy
//
// How much each ROI stands out from the tissue right around it: the mean
// inside the ROI, and the mean in a ring a few micrometres outside its edge,
// on the ROI's OWN slice, per channel.
//
// analysis-oo_count-physical_blur (the oocyte count), not merged. It exists
// because a global threshold cannot tell an oocyte from the texture of a bright
// tile: both are brighter than the section as a whole, but only the oocyte is
// brighter than its own surroundings -- the tile lifts the ring with it. On the
// oocyte tuning set (10 hand-annotated sections) the ratio inside/ring for DDX4
// was ~1.3-1.7 on bright-tile texture against a median of ~7 for annotated
// oocytes. The decision -- which ratio, which cut-off -- is the R side's
// (feature_contrast_cli.r), per feature; this only measures, per ROI.
//
// The ring is EVERYTHING in the band, other ROIs included. In a tight cluster
// of oocytes that lowers a real oocyte's ratio; it was measured that way on the
// tuning set, and the cut-off was chosen on it.
//
// Usage:
//   def RC = new GroovyClassLoader().parseClass(new File(LIBDIR + "/RoiContrast.groovy"))
//   def rows = RC.measure(imp, rois, "sample_id", [1, 2], 3.0d, 12.0d)
//   RC.write(rows, new File(out, "sample_id_nucleus_contrast.txt"))

import ij.ImagePlus
import ij.gui.Roi
import ij.gui.ShapeRoi
import ij.plugin.RoiEnlarger

class RoiContrast {

    /** The table's columns, in order. One row per ROI per channel, like _res.txt. */
    static final List<String> COLUMNS =
        ["name", "roi", "z", "ch", "area_px", "inside_mean", "ring_area_px", "ring_mean"]

    /** Calibration units that are micrometres (Bio-Formats says "micron"). */
    static final List<String> MICRON_UNITS = ["micron", "microns", "um", "µm", "μm"]

    /**
     * The slice an ROI sits on, from its name: `<feature>_SSSS-NNNN-YYYY`, the
     * name every ROI this repo writes carries. A leading frame (`TTTT-`) is the
     * multi-frame form, which this does not handle -- refused by measure().
     * Falls back to the ROI's own position for an ROI named some other way.
     */
    static int sliceOf(Roi roi) {
        def m = ((roi.getName() ?: "") =~ /_(\d{4}-)?(\d{4})-\d{4}-\d{4}$/)
        if (m.find()) return m.group(2) as int
        int z = roi.getZPosition()
        if (z > 0) return z
        z = roi.getPosition()
        if (z > 0) return z
        throw new IllegalArgumentException(
            "cannot tell which slice ROI >>>" + roi.getName() + "<<< is on (no SSSS-NNNN-YYYY name, no position)")
    }

    /** The band between innerPx and outerPx outside the ROI's edge. */
    static Roi ring(Roi roi, double innerPx, double outerPx) {
        def outer = new ShapeRoi(RoiEnlarger.enlarge(roi, outerPx))
        def inner = new ShapeRoi(RoiEnlarger.enlarge(roi, innerPx))
        return outer.not(inner)
    }

    /** [pixelCount, mean] of the processor under the ROI (clipped to the image). */
    static List stats(ij.process.ImageProcessor ip, Roi roi) {
        ip.setRoi(roi)
        def s = ip.getStats()
        ip.resetRoi()
        return [s.pixelCount, s.mean]
    }

    /**
     * Measure every ROI, inside and in its ring, on its own slice.
     *
     * @param imp      the image the ROIs were found on (single frame)
     * @param rois     ROIs named <feature>_SSSS-NNNN-YYYY
     * @param name     the series id, written into every row (as _outline.txt does)
     * @param channels channels to measure, 1-based
     * @param innerUm  ring starts this far outside the edge, in micrometres
     * @param outerUm  ...and ends this far
     * @return one Map per ROI per channel, keyed by COLUMNS
     */
    static List<Map> measure(ImagePlus imp, List<Roi> rois, String name, List<Integer> channels,
                             double innerUm, double outerUm) {
        if (imp.getNFrames() > 1) {
            throw new IllegalArgumentException("RoiContrast handles one frame; this image has " + imp.getNFrames())
        }
        if (!(innerUm >= 0d) || !(outerUm > innerUm)) {
            throw new IllegalArgumentException("ring must be 0 <= inner < outer um; got " + innerUm + " - " + outerUm)
        }
        def cal = imp.getCalibration()
        if (!MICRON_UNITS.contains(cal.getUnit())) {
            throw new IllegalArgumentException(
                "the ring is set in um, so the image must be calibrated in micrometres; it is in >>>" +
                cal.getUnit() + "<<<")
        }
        double pw = cal.pixelWidth, ph = cal.pixelHeight
        if (!(pw > 0) || Math.abs(pw - ph) > 0.01d * pw) {
            throw new IllegalArgumentException("the ring needs square pixels; this image is " + pw + " x " + ph)
        }
        channels.each { int ch ->
            if (ch < 1 || ch > imp.getNChannels()) {
                throw new IllegalArgumentException("channel " + ch + " requested; the image has " + imp.getNChannels())
            }
        }
        double innerPx = innerUm / pw, outerPx = outerUm / pw
        def bounds = new java.awt.Rectangle(0, 0, imp.getWidth(), imp.getHeight())
        def stack = imp.getStack()
        def out = []
        rois.each { Roi roi ->
            int z = sliceOf(roi)
            if (z < 1 || z > imp.getNSlices()) {
                throw new IllegalArgumentException("ROI " + roi.getName() + " is on slice " + z +
                    "; the image has " + imp.getNSlices() + " -- were these ROIs found on this image?")
            }
            if (!roi.getBounds().intersects(bounds)) {
                throw new IllegalArgumentException("ROI " + roi.getName() + " lies outside the image " +
                    "-- were these ROIs found on this image?")
            }
            def rg = ring(roi, innerPx, outerPx)
            channels.each { int ch ->
                def ip = stack.getProcessor(imp.getStackIndex(ch, z, 1))
                def a = stats(ip, roi)
                def b = stats(ip, rg)
                out << [name: name, roi: roi.getName(), z: z, ch: ch,
                        area_px: a[0], inside_mean: String.format("%.4f", a[1] as double),
                        ring_area_px: b[0], ring_mean: String.format("%.4f", b[1] as double)]
            }
        }
        return out
    }

    /** Write the table; header only when there were no ROIs. */
    static void write(List<Map> rows, File dest) {
        def sb = new StringBuilder(COLUMNS.join("\t")).append("\n")
        rows.each { r -> sb.append(COLUMNS.collect { (r[it] == null) ? "" : r[it].toString() }.join("\t")).append("\n") }
        dest.setText(sb.toString(), "UTF-8")
    }
}
