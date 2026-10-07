// FeatureOverlay.groovy
//
// Feature outlines as a NAMED overlay on the overview TIFF, for placing points
// by hand on the oocyte count (analysis-oo_count-physical_blur; not merged).
//
// Library only. Run_Overview_Batch.groovy is the caller.
//
// The outlines come from R, not from the ROI zip: feature_footprint_cli.r
// writes each feature's footprint -- the union of its ROIs over z -- as a vertex
// table, and this only draws it. The union is deliberately NOT rebuilt here
// (ShapeRoi): confirm_features_cli.r tests the hand-placed points against R's
// footprints, so the outline on screen has to be that geometry, not a second
// one that differs at the edge where a point on a dim oocyte gets placed.
//
// Each polygon is named by its feature_id, so Image > Overlay > To ROI Manager
// lists which feature is which, and coloured by the feature's status, read from
// its id the way every R reader reads it:
//
//   <feature>_N                    counted
//   invalid_*  (incl. invalid_contrast_*)   invalid
//   failed_<feature>_<reason>      failed   (failed_*_overlap: single-slice ROIs)

import ij.ImagePlus
import ij.gui.Overlay
import ij.gui.PolygonRoi
import ij.gui.Roi
import ij.measure.Calibration
import ij.plugin.Colors
import ij.process.FloatPolygon
import java.awt.Color

class FeatureOverlay {

    /** feature_footprint_cli.r's columns (FOOTPRINT_COLUMNS there). */
    static final List<String> FOOTPRINT_COLUMNS = ["name", "feature_id", "part", "ring", "x", "y"]

    /** Which statuses besides `counted` are drawn. */
    static final List<String> DRAW_REJECTED = ["none", "invalid", "all"]

    static final List<String> STATUSES = ["counted", "invalid", "failed"]

    /** How far (in px) a vertex may sit outside the image before the table is refused. */
    static final double EDGE_SLACK_PX = 1.0d

    static String footprintPath(String dir, String prefix, String feature) {
        new File(dir, prefix + "_" + feature + "_footprint.txt").getPath()
    }

    static String roiZipPath(String dir, String prefix, String feature) {
        new File(dir, prefix + "_" + feature + "_outline_ROIs.zip").getPath()
    }

    static String statusOf(String featureId) {
        if (featureId.startsWith("invalid_")) return "invalid"
        if (featureId.startsWith("failed_")) return "failed"
        return "counted"
    }

    static String drawRejectedMode(Object v) {
        String d = (v == null) ? "" : v.toString().trim()
        if (!(d in DRAW_REJECTED)) {
            throw new IllegalArgumentException("drawRejected must be one of " + DRAW_REJECTED.join(", ") + "; got >>>" + v + "<<<")
        }
        return d
    }

    static Set<String> statusesDrawn(Object drawRejected) {
        switch (drawRejectedMode(drawRejected)) {
            case "none":    return ["counted"] as Set
            case "invalid": return ["counted", "invalid"] as Set
            default:        return ["counted", "invalid", "failed"] as Set
        }
    }

    static Color colour(Object spec, String what) {
        Color c = (spec == null || !spec.toString().trim()) ? null : Colors.decode(spec.toString().trim(), null)
        if (c == null) {
            throw new IllegalArgumentException(what + ": unknown colour >>>" + spec + "<<<; use a name such as yellow, or #rrggbb")
        }
        return c
    }

    /**
     * Every overview-batch setting that concerns the TIFF and its overlay,
     * checked BEFORE an image is opened and returned normalised.
     *
     * In a library rather than the `#@` script because a front end is only ever
     * compiled by the tests, never run -- a check written there is untested.
     *
     * @return [savePng, saveTiff, source ("footprint" | "roi" | "none"), dir
     *         (File or null), feature, roiMode, statuses (Set), colours
     *         (status -> Color), channelColours (List<Color> or null)]
     */
    static Map validateBatch(Map o, Class OV) {
        boolean savePng = o.savePng as boolean, saveTiff = o.saveTiff as boolean
        if (!savePng && !saveTiff) {
            throw new IllegalArgumentException("savePng and saveTiff are both off; there would be nothing to write")
        }
        String fpDir = (o.footprintDir ?: "").toString().trim()
        String roiDir = (o.roiDir ?: "").toString().trim()
        if (fpDir && roiDir) {
            throw new IllegalArgumentException("give footprintDir OR roiDir, not both: the overlay has one source")
        }
        String source = fpDir ? "footprint" : roiDir ? "roi" : "none"
        if (source != "none" && !saveTiff) {
            throw new IllegalArgumentException("an overlay (" + (fpDir ? "footprintDir" : "roiDir") +
                ") is drawn only on the TIFF, and saveTiff is off")
        }
        File dir = null
        if (source != "none") {
            dir = new File(fpDir ?: roiDir)
            if (!dir.isDirectory()) throw new IllegalArgumentException("no such folder: " + dir)
        }
        String feature = (o.feature ?: "").toString().trim()
        if (source != "none" && !feature) throw new IllegalArgumentException("feature is blank")
        String roiMode = (o.roiMode ?: "merged").toString().trim()
        if (!(roiMode in ["merged", "all"])) {
            throw new IllegalArgumentException("roiMode must be merged or all; got >>>" + o.roiMode + "<<<")
        }
        def statuses = statusesDrawn(o.drawRejected)
        if (source == "roi" && drawRejectedMode(o.drawRejected) != "none") {
            throw new IllegalArgumentException("drawRejected needs footprints: an ROI zip does not say which ROIs R rejected")
        }
        def colours = [counted: colour(o.countedColor, "countedColor"),
                       invalid: colour(o.invalidColor, "invalidColor"),
                       failed : colour(o.failedColor, "failedColor")]
        def chCols = saveTiff ? OV.channelColours(o.tiffColors) : null
        boolean givenCols = o.tiffColors != null && o.tiffColors.toString().trim()
        return [savePng: savePng, saveTiff: saveTiff, source: source, dir: dir, feature: feature,
                roiMode: roiMode, statuses: statuses, colours: colours,
                channelColours: givenCols ? chCols : null]
    }

    /**
     * Read one series' footprint table into polygons in PIXEL coordinates.
     *
     * Each (feature_id, part, ring) is one PolygonRoi named feature_id, with
     * properties `status`, `part` and `ring`. x and y are divided by the
     * image's own calibration: the inverse of how RoiExport wrote the outline
     * table the footprints were built from (x_px * pixelWidth, y_px *
     * pixelHeight).
     *
     * Refuses, rather than drawing something plausible:
     *   - a table without the footprint columns;
     *   - a `name` other than `expectName` (identity from content, not filename);
     *   - a vertex outside the image by more than EDGE_SLACK_PX -- footprints
     *     from another series, or a segmentation of a different image;
     *   - a ring of fewer than 3 vertices.
     * A header-only table is a series with no features: an empty list.
     */
    static List<Roi> readFootprints(File f, String expectName, Calibration cal, int width, int height) {
        if (!f.isFile()) throw new IllegalArgumentException("no footprint table " + f + " -- run feature_footprint_cli.r on this series' features")
        def lines = f.getText("UTF-8").readLines().findAll { it.trim() }
        if (!lines) throw new IllegalArgumentException(f.getName() + " is empty, not even a header")
        def header = lines[0].split("\t", -1).collect { it.trim() }
        def miss = FOOTPRINT_COLUMNS.findAll { !header.contains(it) }
        if (miss) throw new IllegalArgumentException(f.getName() + " is not a footprint table (no " + miss.join(", ") + ")")
        def ix = FOOTPRINT_COLUMNS.collectEntries { [(it): header.indexOf(it)] }
        double pw = cal.pixelWidth, ph = cal.pixelHeight
        if (!(pw > 0) || !(ph > 0)) throw new IllegalArgumentException("image has no usable pixel size (" + pw + " x " + ph + ")")

        // (feature_id, part, ring) -> vertices, in file order
        def rings = new LinkedHashMap<String, Map>()
        lines.drop(1).eachWithIndex { String line, int i ->
            def c = line.split("\t", -1)
            String name = c[ix.name].trim()
            if (name != expectName) {
                throw new IllegalArgumentException(f.getName() + " line " + (i + 2) + " is for '" + name +
                    "', not '" + expectName + "'")
            }
            String fid = c[ix.feature_id].trim()
            String key = fid + "\t" + c[ix.part].trim() + "\t" + c[ix.ring].trim()
            def r = rings[key]
            if (r == null) {
                r = [fid: fid, part: c[ix.part].trim() as Integer, ring: c[ix.ring].trim() as Integer,
                     xs: new ArrayList<Float>(), ys: new ArrayList<Float>()]
                rings[key] = r
            }
            double x = (c[ix.x].trim() as double) / pw, y = (c[ix.y].trim() as double) / ph
            if (x < -EDGE_SLACK_PX || y < -EDGE_SLACK_PX || x > width + EDGE_SLACK_PX || y > height + EDGE_SLACK_PX) {
                throw new IllegalArgumentException(f.getName() + ": " + fid + " has a vertex at (" +
                    String.format("%.1f, %.1f", x, y) + ") px, outside this " + width + "x" + height +
                    " image. These footprints are not from this series' segmentation.")
            }
            r.xs << (float) x; r.ys << (float) y
        }
        return rings.values().collect { Map r ->
            if (r.xs.size() < 3) {
                throw new IllegalArgumentException(f.getName() + ": " + r.fid + " part " + r.part + " ring " + r.ring +
                    " has " + r.xs.size() + " vertices")
            }
            def roi = new PolygonRoi(new FloatPolygon(r.xs as float[], r.ys as float[]), Roi.POLYGON)
            roi.setName(r.fid)
            roi.setProperty("status", statusOf(r.fid))
            roi.setProperty("part", r.part.toString())
            roi.setProperty("ring", r.ring.toString())
            roi
        }
    }

    /**
     * Add the footprints whose status is in `statuses` to the image's overlay.
     * Copies; the caller's ROIs are left as they were.
     *
     * Stroke width 0 is ImageJ's "one screen pixel at every zoom": a line a
     * fixed number of IMAGE pixels wide vanishes when a full-resolution section
     * is zoomed out to fit the screen.
     *
     * @return status -> number of polygons drawn (every status, 0 included)
     */
    static Map<String, Integer> draw(ImagePlus img, List<Roi> rois, Set<String> statuses, Map<String, Color> colours) {
        def overlay = img.getOverlay() ?: new Overlay()
        def n = STATUSES.collectEntries { [(it): 0] }
        rois.each { Roi r ->
            String st = r.getProperty("status") ?: statusOf(r.getName())
            if (!(st in statuses)) return
            Roi c = (Roi) r.clone()
            c.setStrokeColor(colours[st])
            c.setStrokeWidth(0)
            c.setFillColor(null)
            c.setPosition(0)
            overlay.add(c)
            n[st] = n[st] + 1
        }
        img.setOverlay(overlay)
        return n
    }

    /**
     * Whether a series' _config.txt in `dir` says detection found no `feature`
     * -- the nucleus batch writes no ROI zip then, so a missing zip is not an
     * error. The same rule Run_RoiContrast_Batch applies.
     */
    static boolean configSaysNone(File dir, String prefix, String feature) {
        def cfg = new File(dir, prefix + "_config.txt")
        if (!cfg.isFile()) return false
        def line = cfg.readLines().find { it.startsWith(feature + "_count\t") }
        return line != null && line.split("\t", -1)[1].trim() == "0"
    }
}
