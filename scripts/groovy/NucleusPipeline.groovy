// NucleusPipeline.groovy
//
// The nucleus + nucleolus pipeline for ONE image: detect, export, measure,
// overview, run config. Everything that happens to an image once it is open.
//
// It lives here rather than inside Run_NucleusSelector.groovy because a second
// caller is coming -- the batch runner, which opens its own images from a sample
// sheet instead of taking the active one. The macros in scripts/fiji/ were
// forked per experiment because IJ1 has no import mechanism; that is the mistake
// this split exists to avoid repeating in Groovy.
//
// Same division of labour as the R side: scripts/R/ holds the functions and
// <name>_cli.r holds the argument parsing. Here the class holds the work and
// Run_*.groovy holds the `#@` block. The split is pushed FURTHER than in R,
// though, because a `#@` script cannot be tested without stripping its
// parameter lines and injecting a Binding, while a class can simply be parsed
// and called. So the front end should be parameter declaration, coercion, and
// one call -- nothing else.
//
// Usage:
//
//   def NP = new GroovyClassLoader(this.class.classLoader)
//                 .parseClass(new File(LIBDIR + "/NucleusPipeline.groovy"))
//   def res = NP.load(LIBDIR).run(imp, outdir, params)
//
// `params` keys are the names saveRunConfig() already writes, not the `#@`
// variable names, so that the config file a run produces can be fed straight
// back in as the parameters of another run. That loop is the point of the
// naming; see note/groovy_batch_plan.md.

import ij.*
import ij.gui.*
import ij.plugin.Duplicator
import ij.plugin.RoiEnlarger

class NucleusPipeline {

    // Forced, so output columns do not depend on the operator's Fiji
    // preferences. Set Measurements is a PERSISTENT user preference.
    static final String MEASUREMENTS =
        "area mean standard min centroid shape integrated median stack display"
    // Overview settings, fixed for the pipeline; use Run_Overview.groovy to vary
    // them.
    static final int OVERVIEW_WIDTH = 500
    static final String OVERVIEW_METHOD = "max"

    // The parameters run() reads, and their types. This is the vocabulary a
    // config file may use -- RunConfig.params() rejects anything else rather
    // than letting a typo fall through to a default.
    //
    // `basename` is deliberately NOT here. It names one image's output, so a
    // config file setting it would give every image in a batch the same name
    // and each would overwrite the last. It is passed per image, by the caller.
    // `script_name` likewise identifies the caller, not the request.
    static final Map<String, String> PARAM_TYPES = [
        output_prefix          : "string",
        position_pattern       : "string",
        z_spec                 : "string",
        dna_channel            : "int",
        channels_measured      : "string",
        nucleus_blur_sigma     : "double",
        nucleus_threshold      : "string",
        nucleus_particle_size  : "string",
        nucleus_watershed      : "boolean",
        nucleoli_enabled       : "boolean",
        nucleolus_blur_sigma   : "double",
        nucleolus_threshold    : "string",
        nucleolus_rel_fraction : "double",
        nucleolus_erode_px     : "int",
        nucleolus_particle_size: "string",
        nucleolus_circularity  : "string",
        save_roi_zips          : "boolean",
        save_outlines          : "boolean",
        save_measurements      : "boolean",
        save_config            : "boolean",
        save_overview          : "boolean",
    ]

    // Defaults, so a config need only state what it changes. These MUST match
    // the `value=` literals in Run_NucleusSelector.groovy's `#@` block, or the
    // GUI and the batch would do different things under the same settings --
    // Test_RunConfig asserts exactly that, because SciJava requires the dialog
    // defaults to be literals and they cannot simply be read from here.
    static final Map<String, Object> DEFAULTS = [
        output_prefix          : "",
        position_pattern       : "Position",
        z_spec                 : "",
        dna_channel            : 1,
        channels_measured      : "1,2,3",
        nucleus_blur_sigma     : 8.0d,
        nucleus_threshold      : "Huang2",
        nucleus_particle_size  : "80-Infinity",
        nucleus_watershed      : false,
        nucleoli_enabled       : true,
        nucleolus_blur_sigma   : 3.0d,
        nucleolus_threshold    : "Relative",
        nucleolus_rel_fraction : 0.6d,
        nucleolus_erode_px     : 0,
        nucleolus_particle_size: "3-150",
        nucleolus_circularity  : "0.50-1.00",
        save_roi_zips          : true,
        save_outlines          : true,
        save_measurements      : true,
        save_config            : true,
        save_overview          : false,
    ]

    /**
     * Turn a parsed config into parameters, over the defaults.
     *
     * One value needs undoing: saveRunConfig() writes a blank z_spec as the
     * human-readable "(all)", which parseSlices() would reject as a slice
     * range. Round-tripping a config through the file therefore has to map it
     * back, or feeding a run's own config to a rerun fails on the one field
     * nobody set.
     */
    static Map fromConfig(Map<String, Object> cfgParams) {
        def out = new LinkedHashMap(DEFAULTS)
        out.putAll(cfgParams)
        if (out.z_spec == "(all)") {
            out.z_spec = ""
        }
        return out
    }

    String libDir
    // The sibling libraries, held as Class objects rather than imported types:
    // scripts/groovy/ is parsed at run time the way scripts/R/ is source()d, and
    // there is no build step that would make them real imports.
    Class ND, RX, RD, OV

    /** Parse the sibling libraries out of the same directory this file sits in. */
    static NucleusPipeline load(String libDir) {
        def dir = new File(libDir)
        if (!new File(dir, "NucleolusDetect.groovy").exists()) {
            throw new IllegalStateException(
                "NucleusPipeline: no Groovy library at " + libDir)
        }
        def gcl = new GroovyClassLoader(NucleusPipeline.class.classLoader)
        def p = new NucleusPipeline()
        p.libDir = dir.getAbsolutePath()
        p.ND = gcl.parseClass(new File(dir, "NucleolusDetect.groovy"))
        p.RX = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        p.RD = gcl.parseClass(new File(dir, "RoiDetect.groovy"))
        p.OV = gcl.parseClass(new File(dir, "Overview.groovy"))
        return p
    }

    /**
     * Run the whole per-image pipeline.
     *
     * @param imp    the open image
     * @param outdir where the results go
     * @param p      parameters, keyed as saveRunConfig() writes them
     * @return a map of what was produced: basename, the ROIs and their names
     *         per feature, and the slice set analysed. The caller owns anything
     *         that needs a display -- the ROI Manager, imp.show() -- because
     *         those are not part of producing the files.
     */
    Map run(ImagePlus imp, File outdir, Map p) {
        IJ.run("Set Measurements...", MEASUREMENTS + " redirect=None decimal=3")

        def channels = ((String) p.channels_measured).split(",").collect { it.trim() as Integer }
        int dnaCh = p.dna_channel as int
        // Overviews cover the DNA channel detection used, plus every channel
        // being measured. The outlines were found on DNA, so drawing them over
        // the other channels is exactly how you check a signal against the
        // compartment it is supposed to be in.
        def ovChannels = ([dnaCh] + channels).unique().sort()
        def slices     = RD.parseSlices(p.z_spec ?: "", imp.getNSlices())
        def outDirPath = outdir.getAbsolutePath() + File.separator

        // An explicit basename wins over resolving one from the image. The batch
        // runner passes the sample sheet's `prefix`, which is authoritative --
        // resolveImageId() digs the id out of the slice label or title, and a
        // Leica default like "Series001" recurs in every file, so it cannot be
        // unique across a batch. Absent one, behave exactly as before.
        def basename = (p.basename) ? (p.basename as String)
                                    : ((p.output_prefix ?: "") + RX.resolveImageId(imp, p.position_pattern))

        IJ.log("=== " + basename + " ===")
        IJ.log("  analysing " + slices.size() + " of " + imp.getNSlices() + " slices")

        def writeFeature = { String feature, List rois, List names, List sls ->
            IJ.log("  " + feature + ": " + rois.size() + " ROIs")
            if (rois.isEmpty()) return
            def stem = outDirPath + basename + "_" + feature
            if (p.save_outlines)     RX.saveOutlineCoords(imp, rois, names, sls, basename, stem + "_outline.txt")
            if (p.save_roi_zips)     RX.saveRoiZip(rois, names, stem + "_outline_ROIs.zip")
            if (p.save_measurements) RX.measureRois(imp, rois, sls, channels, stem + "_res.txt", true)
        }

        // --- Nucleus ---------------------------------------------------------
        def dna = RD.buildMask(imp, dnaCh, p.nucleus_blur_sigma as double,
                               p.nucleus_threshold as String, true,
                               p.nucleus_watershed as boolean)
        def nucRois   = RD.detect(dna, p.nucleus_particle_size as String, "", slices, true, true)
        def nucNames  = RD.autoLabels(nucRois).collect { "nucleus_" + it }
        def nucSlices = nucRois.collect { it.getPosition() }
        // NB: set the name ON the Roi, not just in the parallel names list. The
        //     name is encoded into the .roi file and picked up by Analyzer into
        //     the Label column, which is how read_fiji_result.r joins
        //     measurements to outlines.
        nucRois.eachWithIndex { r, i -> r.setName(nucNames[i]) }
        dna.close()
        writeFeature("nucleus", nucRois, nucNames, nucSlices)

        // --- Nucleolus -------------------------------------------------------
        def nuclRois = [], nuclNames = [], nuclSlices = []
        if (p.nucleoli_enabled && !nucRois.isEmpty()) {
            int erodePx = p.nucleolus_erode_px as int
            def useRois = (erodePx > 0) ? nucRois.collect { RoiEnlarger.enlarge(it, -erodePx) } : nucRois
            def dna2 = new Duplicator().run(imp, dnaCh, dnaCh, 1, imp.getNSlices(), 1, 1)
            def mask = ND.buildNucleolusMask(dna2, useRois, nucSlices,
                                             p.nucleolus_blur_sigma as double,
                                             p.nucleolus_threshold as String,
                                             p.nucleolus_rel_fraction as double)
            dna2.close()
            nuclRois   = RD.detect(mask, p.nucleolus_particle_size as String,
                                   p.nucleolus_circularity as String, slices, true, false)
            nuclNames  = RD.autoLabels(nuclRois).collect { "nucleolus_" + it }
            nuclSlices = nuclRois.collect { it.getPosition() }
            nuclRois.eachWithIndex { r, i -> r.setName(nuclNames[i]) }
            mask.close()
            writeFeature("nucleolus", nuclRois, nuclNames, nuclSlices)
        }

        // --- Overview PNGs ---------------------------------------------------
        // A quick visual check, not an input to anything: the chosen channels
        // projected over the same slices detection used. Fixed settings here on
        // purpose -- Run_Overview.groovy is the script for choosing them.
        //
        // TWO files per channel, raw and outlined. They used to share one name,
        // which made them mutually exclusive: writing either destroyed the
        // other, and montage_qc_cli.r wants both side by side. One prepare()
        // serves both saves -- savePng() flattens into a NEW image and leaves
        // the view untouched, so the raw copy can go out before the outlines
        // are added.
        if (p.save_overview) {
            def proj = OV.project(imp, slices, OVERVIEW_METHOD, ovChannels)
            ovChannels.each { int c ->
                def view = OV.prepare(proj, c, [width: OVERVIEW_WIDTH, contrast: "auto"])
                def raw  = OV.savePng(view, OV.overviewPath(outDirPath, basename, c, ""))
                // NB: "merged" unions the outlines in the PROJECTION, so touching
                //     or z-overlapping objects share one outline. It is a
                //     picture, not a count.
                OV.addOutlines(view, nucRois,  [mode: "merged", color: "yellow",  lineWidth: 1])
                OV.addOutlines(view, nuclRois, [mode: "merged", color: "magenta", lineWidth: 1])
                def ovl  = OV.savePng(view, OV.overviewPath(outDirPath, basename, c, OV.OVERLAY_SUFFIX))
                // The display range, because "auto" contrast stretches whatever
                // is there: a channel carrying only noise has that noise
                // stretched to full range and saves a convincing picture of
                // nothing. A narrow range beside a wide one on another channel
                // is the tell -- but only if it is written down.
                IJ.log("  overview ch" + c + ": display " + IJ.d2s(view.lo, 1) + "-" + IJ.d2s(view.hi, 1) +
                       " -> " + raw.getName() + ", " + ovl.getName())
            }
            proj.close()
        }

        // --- Run configuration -----------------------------------------------
        if (p.save_config) {
            RX.saveRunConfig([
                timestamp              : new Date().format("yyyy-MM-dd HH:mm:ss"),
                // The CALLER names itself: provenance has to say which entry
                // point produced the directory, and there is more than one now.
                script                 : (p.script_name ?: "NucleusPipeline.groovy") + " " + RX.repoVersion(libDir),
                imagej_version         : IJ.getVersion(),
                image_title            : imp.getTitle(),
                // NB: width/height in PIXELS. The outline tables are written in
                //     calibrated units, so without these the R side cannot
                //     reconstruct the image extent -- the bounding box of the
                //     detected objects is not the frame. A QC panel drawn from R
                //     would then be cropped differently from the Fiji overview
                //     PNG it is meant to sit beside.
                image_width            : imp.getWidth(),
                image_height           : imp.getHeight(),
                image_slices           : imp.getNSlices(),
                image_channels         : imp.getNChannels(),
                pixel_width            : imp.getCalibration().pixelWidth,
                pixel_height           : imp.getCalibration().pixelHeight,
                // The z step, which nothing recorded before: feature_stat_cli.r
                // needs it for `volume` and had to be told by hand. Bio-Formats
                // populates it on import.
                //
                // BLANK for a single plane, not 1.0. ImageJ defaults pixelDepth
                // to 1.0 when there is no z axis, and Bio-Formats reports the
                // physical size as null there -- writing 1.0 would hand a later
                // reader a plausible number for a distance that does not exist,
                // and `area_sum x 1.0` is an area wearing a volume's name.
                pixel_depth            : (imp.getNSlices() > 1 ? imp.getCalibration().pixelDepth : null),
                pixel_unit             : imp.getCalibration().getUnit(),
                output_basename        : basename,
                position_pattern       : p.position_pattern,
                z_spec                 : (p.z_spec ?: "(all)"),
                z_slices_analysed      : slices.size(),
                dna_channel            : dnaCh,
                channels_measured      : p.channels_measured,
                measurements           : MEASUREMENTS,
                nucleus_blur_sigma     : p.nucleus_blur_sigma,
                nucleus_threshold      : p.nucleus_threshold,
                nucleus_particle_size  : p.nucleus_particle_size,
                nucleus_watershed      : p.nucleus_watershed,
                overview_saved         : p.save_overview,
                // Which overview files exist, so a results folder can be read
                // later without guessing. Blank when none were written.
                overview_channels      : (p.save_overview ? ovChannels.join(",") : ""),
                overview_overlay_suffix: (p.save_overview ? OV.OVERLAY_SUFFIX : ""),
                nucleus_count          : nucRois.size(),
                nucleoli_enabled       : p.nucleoli_enabled,
                nucleolus_blur_sigma   : p.nucleolus_blur_sigma,
                nucleolus_threshold    : p.nucleolus_threshold,
                nucleolus_rel_fraction : p.nucleolus_rel_fraction,
                nucleolus_erode_px     : p.nucleolus_erode_px,
                nucleolus_particle_size: p.nucleolus_particle_size,
                nucleolus_circularity  : p.nucleolus_circularity,
                nucleolus_count        : nuclRois.size()
            ], outDirPath + basename + "_config.txt")
        }

        IJ.log("Done: " + basename)
        return [basename  : basename,
                outDirPath: outDirPath,
                slices    : slices,
                nucRois   : nucRois,  nucNames : nucNames,  nucSlices : nucSlices,
                nuclRois  : nuclRois, nuclNames: nuclNames, nuclSlices: nuclSlices]
    }
}
