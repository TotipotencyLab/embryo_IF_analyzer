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
// naming.

import ij.*
import ij.gui.*
import ij.plugin.Duplicator
import ij.plugin.RoiEnlarger

class NucleusPipeline {

    // Forced, so output columns do not depend on the operator's Fiji
    // preferences. Set Measurements is a PERSISTENT user preference.
    //
    // No `stack` since time_axis: it adds ImageJ's Ch/Slice/Frame, which mean
    // different things on different image shapes (Slice is the TIME on a
    // 1c 1z 4t image; note/time_series_plan.md §5.3). RoiExport.measureInto()
    // writes roi, z, t and ch itself, from what it knows, and those are the
    // only position columns.
    static final String MEASUREMENTS =
        "area mean standard min centroid shape integrated median display"
    // Overview settings used to be fixed here, with Run_Overview.groovy as the
    // way to vary them. That is no good for a merged tile scan: 500 px of a
    // 20000 px mosaic diagnoses nothing, and re-running a second script over a
    // whole batch to get a readable picture is not a quick visual check. They
    // are parameters now; the defaults below are the constants they replaced.

    // The parameters run() reads, and their types. This is the vocabulary a
    // config file may use -- RunConfig.params() rejects anything else rather
    // than letting a typo fall through to a default.
    //
    // `series_id` is deliberately NOT here. It names one image's output, so a
    // config file setting it would give every image in a batch the same name
    // and each would overwrite the last. It is passed per image, by the caller:
    // the batch from the series table, the interactive script from its dialog.
    // `script_name` likewise identifies the caller, not the request. What the
    // id was is recorded, as the `series_id` provenance field.
    //
    // `output_prefix` and `position_pattern` were here until v0.7.0 and are
    // RunConfig.RETIRED_KEYS now: a config written before then still reads.
    static final Map<String, String> PARAM_TYPES = [
        z_spec                 : "string",
        dna_channel            : "int",
        channels_measured      : "string",
        nucleus_blur_sigma     : "double",
        nucleus_threshold      : "string",
        nucleus_threshold_range: "string",
        nucleus_stack_histogram: "boolean",
        nucleus_threshold_scope: "string",
        nucleus_particle_size  : "string",
        nucleus_circularity    : "string",
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
        overview_method        : "string",
        overview_width         : "int",
        overview_height        : "int",
        overview_contrast      : "string",
        overview_saturated     : "double",
    ]

    // Defaults, so a config need only state what it changes. These MUST match
    // the `value=` literals in Run_NucleusSelector.groovy's `#@` block, or the
    // GUI and the batch would do different things under the same settings --
    // Test_RunConfig asserts exactly that, because SciJava requires the dialog
    // defaults to be literals and they cannot simply be read from here.
    static final Map<String, Object> DEFAULTS = [
        z_spec                 : "",
        dna_channel            : 1,
        channels_measured      : "1,2,3",
        nucleus_blur_sigma     : 8.0d,
        nucleus_threshold      : "Huang2",
        // Only read when nucleus_threshold is "Manual", exactly as
        // nucleolus_rel_fraction is only read for "Relative".
        nucleus_threshold_range: "",
        // One threshold from the pooled histogram of every slice. Off means one
        // per slice, which lets an empty slice's noise become objects -- see
        // RoiDetect.buildMask.
        nucleus_stack_histogram: true,
        // Over what an automatic threshold is chosen in a multi-frame series:
        // each frame on its own, or every frame at once (one threshold for the
        // series). `frame` is how a series was analysed before this existed.
        nucleus_threshold_scope: "frame",
        nucleus_particle_size  : "80-Infinity",
        // 0.00-1.00 is every shape, i.e. no filter -- the behaviour before this
        // existed. See the detection block for why turning it on is not free.
        nucleus_circularity    : "0.00-1.00",
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
        overview_method        : "max",
        overview_width         : 500,
        overview_height        : 0,
        overview_contrast      : "auto",
        overview_saturated     : 0.35d,
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
    Class ND, RX, RD, OV, SS

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
        p.SS = gcl.parseClass(new File(dir, "SeriesSource.groovy"))
        return p
    }

    /**
     * Run the whole per-image pipeline.
     *
     * @param imp    the open image
     * @param outdir where the results go
     * @param p      parameters, keyed as saveRunConfig() writes them
     * @return a map of what was produced: series_id, the ROIs and their names
     *         per feature, and the slice set analysed. The caller owns anything
     *         that needs a display -- the ROI Manager, imp.show() -- because
     *         those are not part of producing the files.
     */
    Map run(ImagePlus imp, File outdir, Map p) {
        // An open image is one kind of series source; the batch hands over the
        // other kinds. Not owned: the image is the caller's to close.
        return runSource(SS.ofImage(imp, (p.open_method ?: "") as String, false), outdir, p)
    }

    /** Where a series' frames are staged while it runs, under the output directory. */
    static final String STAGING_DIR = ".staging"

    /**
     * Run the pipeline over a series source (SeriesSource), one frame at a time.
     *
     * Typed Object, not SeriesSource: the batch and this class each parse their
     * own copy of SeriesSource, and two parsings are two different classes.
     *
     * @param src    the series, handed over frame by frame
     * @param wanted the frames to analyse (TiffAssembler.parseFrames), null = all.
     *               A series of one frame is analysed whatever this says.
     * @param resume what to do with what an earlier, unfinished run of this
     *               series staged: true keeps its finished frames and analyses
     *               only the rest -- refusing if it was run with other settings
     *               -- false discards it. Only a multi-frame series resumes.
     * @return as run(), plus `frames`: one entry per frame attempted -- t,
     *         status, the frame's threshold stats, seconds, message
     */
    Map runSource(Object src, File outdir, Map p, List<Integer> wanted = null, boolean resume = false) {
        IJ.run("Set Measurements...", MEASUREMENTS + " redirect=None decimal=3")

        def channels = ((String) p.channels_measured).split(",").collect { it.trim() as Integer }
        int dnaCh = p.dna_channel as int
        // Overviews cover the DNA channel detection used, plus every channel
        // being measured. The outlines were found on DNA, so drawing them over
        // the other channels is exactly how you check a signal against the
        // compartment it is supposed to be in.
        def ovChannels = ([dnaCh] + channels).unique().sort()
        def slices     = RD.parseSlices(p.z_spec ?: "", src.nSlices)
        def outDirPath = outdir.getAbsolutePath() + File.separator

        // The series id names everything this run writes: every file, and the
        // `name` column of every outline row -- one id for both, so the R side
        // can find `<series_id>_config.txt` from the name it read out of a table.
        // The batch passes the series table's, which is authoritative; the
        // interactive script passes what was typed, or nothing, and then the
        // image title is used. A given id is refused, not rewritten, if it
        // cannot name a file.
        String seriesId
        if (p.series_id) {
            seriesId = RX.checkSeriesId(p.series_id as String)
            IJ.log("  series id: '" + seriesId + "'  [given]")
        } else if (src.whole != null) {
            seriesId = RX.seriesIdFromTitle(src.whole)
        } else {
            seriesId = RX.checkSeriesId(src.title as String)
        }

        IJ.log("=== " + seriesId + " ===")
        IJ.log("  analysing " + slices.size() + " of " + src.nSlices + " slices")

        // Overview settings are checked HERE, not where they are used. The
        // overview is the last thing the pipeline does, so an unknown
        // projection name would otherwise be found only after detection,
        // export and measurement had run -- per image, 1261 times over a tile
        // scan. Overview.validateSettings() throws exactly what project() and
        // prepare() throw, from the same code, so passing here cannot mean
        // failing there.
        if (p.save_overview) {
            OV.validateSettings(p.overview_method as String, p.overview_contrast,
                                p.overview_saturated, p.overview_width, p.overview_height)
        }
        // Same reasoning for the threshold: "Manual" with no range, or a method
        // name that does not exist, should not be discovered after the blur has
        // run. buildMask checks it again at the point of use -- this is the
        // early copy, not the only one.
        RD.validateThreshold(p.nucleus_threshold as String,
                             (p.nucleus_threshold_range ?: "") as String)
        String scope = (p.nucleus_threshold_scope ?: "frame") as String
        RD.validateScope(scope, p.nucleus_threshold as String,
                         (p.nucleus_stack_histogram == null) ? true : (p.nucleus_stack_histogram as boolean))

        // --- Frames ----------------------------------------------------------
        // Every frame is analysed as an image of its own, by runFrame(), handed
        // over by the source one at a time and released before the next: a
        // Luxendo position does not fit in memory, and nothing here may hold
        // it. A single-frame image is passed through as itself -- no copy, so
        // nothing about it changes. Every whole-stack step inside -- the pooled
        // histogram, the Duplicator of the z range -- is therefore per frame
        // without being told.
        //
        // `multi` is a fact about the SERIES, not about this run: frame 11 of a
        // 96-frame position is t = 11 and carries TTTT- in its ROI ids whether
        // one frame was asked for or all of them, so two runs over different
        // frames of one series cannot collide.
        boolean multi = src.nFrames > 1
        def pick = src.choose(wanted)
        def frames = pick.frames
        if (pick.absent) {
            IJ.log("  WARNING: no frame " + SS.describeFrames(pick.absent) + " in this series (it has " +
                   SS.describeFrames(src.frameList) + "); nothing is done for them")
        }
        if (!frames) {
            throw new IllegalArgumentException("none of the frames asked for is in this series (it has " +
                                               SS.describeFrames(src.frameList) + ")")
        }
        if (multi) {
            IJ.log("  " + (frames.size() == src.nFrames ? ("" + frames.size())
                                                         : (frames.size() + " of " + src.nFrames)) +
                   " frames, each analysed on its own")
            if (p.save_overview) {
                // Checked before the first frame, as validateSettings() above
                // is: a 32-bit projection's range cannot be accumulated, and
                // finding out after 96 frames would cost the whole series.
                OV.validateSeriesSettings(p.overview_method as String)
            }
        }
        // A multi-frame series' overview is one TIFF per channel, rendered at
        // the end from what each frame leaves in its staging directory (see
        // Overview.writeSeries); a single frame keeps its PNGs, below.
        boolean seriesOverview = multi && p.save_overview
        def ovOpts = [width    : p.overview_width,
                      height   : p.overview_height,
                      contrast : p.overview_contrast,
                      saturated: p.overview_saturated]

        // STAGING. Each frame's tables go to disk as soon as the frame is done,
        // into a directory renamed into place only once all of them are
        // written: a frame is finished exactly when <stage>/t<TTTT>/ exists. A
        // crash at frame 90 of 96 then costs one frame, not 90 -- and resuming
        // is skipping the frames already there. The series' files are the
        // frames joined in t order at the end.
        def stage = new File(new File(outdir, STAGING_DIR), seriesId)
        // RESUME. A leftover is a run that died. Its finished frames are kept
        // only if they were made exactly as this run would make them: joined
        // otherwise, one series' files would hold frames analysed two ways and
        // nothing in them would say so. Hence the settings, written before the
        // first frame and compared before this run's first frame -- a refusal
        // costs nothing. A single frame is not worth resuming: its overview
        // needs its pixels open anyway.
        boolean resumable = resume && multi
        // SERIES THRESHOLD (nucleus_threshold_scope = series): one automatic
        // threshold for every frame, chosen before the first frame is analysed
        // from all of their histograms at once -- a first pass that reads each
        // frame, blurs its DNA channel exactly as detection will, and keeps
        // nothing but the histogram. It costs a second read of every frame, not
        // memory. A single frame is its own series, so it takes the ordinary
        // path; Manual has no histogram to pool.
        boolean seriesThreshold = multi && scope == "series" && !RD.MANUAL.equalsIgnoreCase(p.nucleus_threshold as String)
        def settings = stagingSettings(src, p)
        // Which frames the series threshold is over is part of what a staged
        // frame was made with: other frames, another threshold.
        if (seriesThreshold) settings.series_frames = frames.join(" ")
        Map chosen = null                 // the series threshold, when there is one
        List<Integer> inThreshold = null  // the frames whose pixels chose it
        Set<Integer> staged = new TreeSet<Integer>()
        if (stage.exists()) {
            if (resumable) {
                def taken = resumeStaging(stage, settings, seriesId)
                staged.addAll(taken.frames)
                if (seriesThreshold && staged) {
                    // Chosen by the run that staged them, recorded beside them:
                    // the frames are not read a second time to choose it again.
                    def then = taken.settings
                    chosen = [t      : (then[SERIES_T] ? (then[SERIES_T] as int) : null),
                              divisor: (then[SERIES_DIVISOR] ? (then[SERIES_DIVISOR] as long) : null)]
                    inThreshold = (then[SERIES_IN] ?: "").tokenize(" ").collect { it as int }
                    IJ.log("  series threshold from the earlier run: " + describeChosen(chosen, p))
                }
                def unwanted = staged.findAll { !frames.contains(it) }
                if (unwanted) {
                    IJ.log("  WARNING: frame(s) " + SS.describeFrames(unwanted as List) + " staged by the " +
                           "earlier run are not among the frames asked for: they are not joined, and are " +
                           "discarded with the staging when this run finishes")
                }
            } else {
                IJ.log("  discarding the staged frames of an earlier, unfinished run")
                stage.deleteDir()
            }
        }
        if (seriesThreshold && chosen == null) {
            def first = seriesThresholdOver(src, frames, p, dnaCh)
            chosen = first.chosen
            inThreshold = first.frames
        }
        if (!stage.exists()) {
            stage.mkdirs()
            def record = new LinkedHashMap(settings)
            if (seriesThreshold) {
                record[SERIES_T]       = (chosen.t == null ? "" : chosen.t.toString())
                record[SERIES_DIVISOR] = (chosen.divisor == null ? "" : chosen.divisor.toString())
                record[SERIES_IN]      = inThreshold.join(" ")
            }
            RX.saveRunConfig(record, new File(stage, STAGING_SETTINGS).getPath())
        }

        // What every frame found, in memory as well: the caller gets the ROIs
        // back (the interactive runner fills the ROI Manager from them), and a
        // single frame's overview draws them.
        def found  = [nucleus  : [rois: [], names: [], slices: [], ts: []],
                      nucleolus: [rois: [], names: [], slices: [], ts: []]]
        def frameResults = []
        ImagePlus kept = null    // a single frame, kept for its overview
        try {
            frames.each { int t ->
                if (seriesThreshold && !inThreshold.contains(t)) {
                    // Unreadable when the series threshold was chosen, so not
                    // part of it: analysed now, it would be a frame thresholded
                    // by pixels other than its series'.
                    frameResults << [t: t, status: "failed", stats: null, seconds: null,
                                     message: "could not be read when the series threshold was chosen"]
                    return
                }
                if (staged.contains(t)) {
                    // Finished by the earlier run: what it found is read back
                    // from its staging -- the overlay draws its ROIs, the
                    // config counts them -- and the frame is not read at all.
                    def fr = loadStagedFrame(stagedFrame(stage, t))
                    ["nucleus", "nucleolus"].each { String feature ->
                        def got = fr[feature]
                        found[feature].rois.addAll(got.rois)
                        found[feature].names.addAll(got.names)
                        found[feature].slices.addAll(got.slices)
                        found[feature].ts.addAll(got.rois.collect { t })
                    }
                    frameResults << [t: t, status: "ok", stats: fr.stats, message: RESUMED_MESSAGE,
                                     seconds: null]
                    return
                }
                long t0 = System.currentTimeMillis()
                ImagePlus frame = null
                try {
                    frame = src.frame(t)
                    def tables = [nucleus: RX.newMeasurementTable(), nucleolus: RX.newMeasurementTable()]
                    def fr = runFrame(frame, t, multi, p, dnaCh, channels, slices, tables, chosen)
                    stageFrame(stage, t, seriesId, src.calibration, fr, tables, p,
                               seriesOverview ? { File part ->
                                   OV.stageFrame(frame, slices, p.overview_method as String, ovChannels, ovOpts, part)
                               } : null)
                    ["nucleus", "nucleolus"].each { String feature ->
                        def got = fr[feature]
                        found[feature].rois.addAll(got.rois)
                        found[feature].names.addAll(got.names)
                        found[feature].slices.addAll(got.slices)
                        found[feature].ts.addAll(got.rois.collect { t })
                    }
                    frameResults << [t: t, status: "ok", stats: fr.stats, message: "",
                                     seconds: System.currentTimeMillis() - t0]
                    if (!multi) kept = frame
                } catch (Throwable e) {
                    // One frame is one image's worth of work: a failure is a
                    // fact about that frame, and the rest of the series goes on
                    // -- as one row's failure does not stop a batch. A single
                    // frame's failure is the series' failure, and propagates as
                    // it always has.
                    if (!multi) throw e
                    def msg = e.getClass().getSimpleName() + ": " + (e.getMessage() ?: "(no message)")
                    IJ.log("  t" + t + " FAILED: " + msg)
                    frameResults << [t: t, status: "failed", stats: null, message: msg,
                                     seconds: System.currentTimeMillis() - t0]
                } finally {
                    if (frame != null && !frame.is(kept)) src.release(frame)
                }
            }
        } catch (Throwable e) {
            // What ends a series here is not a frame's failure -- those are
            // caught above -- and the frames it finished are exactly what a
            // resume is for, so they stay.
            if (!resumable) stage.deleteDir()
            throw e
        }
        def okFrames = frameResults.findAll { it.status == "ok" }
        if (!okFrames) {
            // Resumable, it may still hold frames an earlier run finished
            // that this one did not ask for; a later run can take them.
            if (!resumable) stage.deleteDir()
            throw new IllegalStateException("every frame failed; first: t" + frameResults[0].t + " " +
                                            frameResults[0].message)
        }
        def frameStats = okFrames.collect { it.stats }
        def nucRois  = found.nucleus.rois
        def nuclRois = found.nucleolus.rois
        def nucNames = found.nucleus.names,  nucSlices  = found.nucleus.slices
        def nuclNames = found.nucleolus.names, nuclSlices = found.nucleolus.slices

        // The series' files: the staged frames joined, in t order.
        def okTs = okFrames.collect { it.t as int }
        ["nucleus", "nucleolus"].each { String feature ->
            if (multi) IJ.log("  " + feature + ": " + found[feature].rois.size() + " ROIs over " +
                              okTs.size() + " frame(s)")
            def part = { String suffix -> okTs.collect { new File(stagedFrame(stage, it), feature + suffix) } }
            def stem = outDirPath + seriesId + "_" + feature
            RX.joinTables(part("_outline.txt"), new File(stem + "_outline.txt"), false)
            // Staged whatever save_roi_zips says (a resume reads them back);
            // written out only when it asks.
            if (p.save_roi_zips) RX.joinRoiZips(part("_outline_ROIs.zip"), new File(stem + "_outline_ROIs.zip"))
            RX.joinTables(part("_res.txt"), new File(stem + "_res.txt"), true)
        }

        // Per image, as _config.txt has always recorded them, when there is one
        // frame; "per-frame" when there are several, with the numbers in
        // _threshold_stats.tsv -- a word, not a number, so it cannot be read as
        // one, the same device as the threshold's `none`.
        // A series threshold is one value for every frame, and is written as
        // one; so is the divisor its histogram needed.
        def thresholdUsed = seriesThreshold ? frameStats[0].nucleus_threshold_used
                          : multi ? "per-frame" : frameStats[0].nucleus_threshold_used
        def histDivisor   = seriesThreshold ? (chosen.divisor ?: "")
                          : multi ? "per-frame" : frameStats[0].nucleus_histogram_divisor
        def maskPct       = multi ? "per-frame" : frameStats[0].nucleus_mask_pct
        def rejected      = frameStats.collect { it.nucleus_circ_rejected }
        def circRejected  = (rejected.every { it == "" }) ? "" : rejected.sum { (it ?: 0) as int }
        boolean overviewWritten = p.save_overview
        // What each overview channel was displayed at, for _config.txt.
        def ovRanges = []
        // A single frame's pixels, still open for its overview; released below.
        def imp = kept

        // --- Overview TIFFs, a multi-frame series ----------------------------
        // One display range per channel across every frame analysed, so a cell
        // does not appear to brighten because the stretch moved. One page per
        // frame in t order -- the frames frames_analysed names -- with this
        // frame's outlines only on the overlay.
        if (seriesOverview) {
            def ov = OV.writeSeries(okTs.collect { stagedFrame(stage, it) }, okTs, outDirPath, seriesId,
                                    ovChannels, ovOpts, src.width as int, src.height as int,
                                    [[rois: found.nucleus.rois,   ts: found.nucleus.ts,   color: "yellow"],
                                     [rois: found.nucleolus.rois, ts: found.nucleolus.ts, color: "magenta"]],
                                    src.frameInterval as Double)
            ov.each { c, r ->
                ovRanges << ("ch" + c + ":" + IJ.d2s(r.lo as double, 1) + "-" + IJ.d2s(r.hi as double, 1))
                IJ.log("  overview ch" + c + ": display " + IJ.d2s(r.lo as double, 1) + "-" +
                       IJ.d2s(r.hi as double, 1) + " over " + okTs.size() + " frame(s) -> " +
                       r.files.collect { it.getName() }.join(", "))
            }
        }

        // --- Overview PNGs ---------------------------------------------------
        // A quick visual check, not an input to anything: the chosen channels
        // projected over the same slices detection used. Fixed settings here on
        // purpose -- Run_Overview.groovy is the script for choosing them.
        //
        // TWO files per channel, raw and outlined. They used to share one name,
        // which made them mutually exclusive: writing either destroyed the
        // other, and feature_outline_cli.r wants both side by side. One prepare()
        // serves both saves -- savePng() flattens into a NEW image and leaves
        // the view untouched, so the raw copy can go out before the outlines
        // are added.
        if (overviewWritten && !multi) {
            def proj = OV.project(imp, slices, p.overview_method as String, ovChannels)
            ovChannels.each { int c ->
                // width/height 0 means "the original size", and one given makes
                // the other follow the aspect ratio -- so one number handles any
                // tile geometry. Overview.prepare() owns that rule; do not
                // second-guess it here.
                def view = OV.prepare(proj, c, [width    : p.overview_width,
                                                height   : p.overview_height,
                                                contrast : p.overview_contrast,
                                                saturated: p.overview_saturated])
                def raw  = OV.savePng(view, OV.overviewPath(outDirPath, seriesId, c, ""))
                // NB: "merged" unions the outlines in the PROJECTION, so touching
                //     or z-overlapping objects share one outline. It is a
                //     picture, not a count.
                OV.addOutlines(view, nucRois,  [mode: "merged", color: "yellow",  lineWidth: 1])
                OV.addOutlines(view, nuclRois, [mode: "merged", color: "magenta", lineWidth: 1])
                def ovl  = OV.savePng(view, OV.overviewPath(outDirPath, seriesId, c, OV.OVERLAY_SUFFIX))
                // The display range, because "auto" contrast stretches whatever
                // is there: a channel carrying only noise has that noise
                // stretched to full range and saves a convincing picture of
                // nothing. A narrow range beside a wide one on another channel
                // is the tell -- but only if it is written down.
                IJ.log("  overview ch" + c + ": display " + IJ.d2s(view.lo, 1) + "-" + IJ.d2s(view.hi, 1) +
                       " -> " + raw.getName() + ", " + ovl.getName())
                ovRanges << ("ch" + c + ":" + IJ.d2s(view.lo, 1) + "-" + IJ.d2s(view.hi, 1))
            }
            proj.close(); proj.flush()   // close() alone frees nothing; see above
        }
        if (kept != null) src.release(kept)

        // --- Run configuration -----------------------------------------------
        if (p.save_config) {
            RX.saveRunConfig([
                timestamp              : new Date().format("yyyy-MM-dd HH:mm:ss"),
                // The CALLER names itself: provenance has to say which entry
                // point produced the directory, and there is more than one now.
                script                 : (p.script_name ?: "NucleusPipeline.groovy") + " " + RX.repoVersion(libDir),
                imagej_version         : IJ.getVersion(),
                image_title            : src.title,
                // How the image was opened: "importer" or "reader" from the
                // batch, BLANK when the image was already open (the interactive
                // runner, where the operator opened it however they liked).
                // Two runs that used different readers must not be
                // indistinguishable afterwards.
                open_method            : (p.open_method ?: ""),
                // Which series of which file this directory came from. Blank in
                // the interactive runner, where the image was already open and
                // nothing told us. Identity comes from content, not from the
                // filename -- so the series id should not have to be parsed
                // apart to answer this.
                source_file            : (p.source_file ?: ""),
                series_index           : (p.series_index == null ? "" : p.series_index),
                series_name            : (p.series_name ?: ""),
                // NB: width/height in PIXELS. The outline tables are written in
                //     calibrated units, so without these the R side cannot
                //     reconstruct the image extent -- the bounding box of the
                //     detected objects is not the frame. A QC panel drawn from R
                //     would then be cropped differently from the Fiji overview
                //     PNG it is meant to sit beside.
                image_width            : src.width,
                image_height           : src.height,
                image_slices           : src.nSlices,
                image_channels         : src.nChannels,
                image_frames           : src.nFrames,
                // Which of them these results hold: the frames asked for, less
                // any that failed (batch_summary.tsv says why). Space-separated,
                // never commas -- a comma in a value is split by the R CLIs.
                frames_analysed        : okTs.join(" "),
                pixel_width            : src.calibration.pixelWidth,
                pixel_height           : src.calibration.pixelHeight,
                // The z step, which nothing recorded before: feature_stat_cli.r
                // needs it for `volume` and had to be told by hand. Bio-Formats
                // populates it on import.
                //
                // BLANK for a single plane, not 1.0. ImageJ defaults pixelDepth
                // to 1.0 when there is no z axis, and Bio-Formats reports the
                // physical size as null there -- writing 1.0 would hand a later
                // reader a plausible number for a distance that does not exist,
                // and `area_sum x 1.0` is an area wearing a volume's name.
                pixel_depth            : (src.nSlices > 1 ? src.calibration.pixelDepth : null),
                pixel_unit             : src.calibration.getUnit(),
                // The time step, as pixel_depth is the z step: measured off the
                // image, so provenance, never a parameter. BLANK for one frame
                // -- there is no interval -- and when the file did not say:
                // ImageJ's 0 means "unknown", and a written 1 would be a
                // plausible-looking second that nobody measured.
                frame_interval         : src.frameInterval,
                frame_unit             : (src.frameInterval == null ? null : src.frameUnit),
                // The id every file of this run is named after, and the value
                // of the outline tables' `name` column. Provenance, not a
                // parameter: see PARAM_TYPES.
                series_id              : seriesId,
                z_spec                 : (p.z_spec ?: "(all)"),
                z_slices_analysed      : slices.size(),
                dna_channel            : dnaCh,
                channels_measured      : p.channels_measured,
                measurements           : MEASUREMENTS,
                nucleus_blur_sigma     : p.nucleus_blur_sigma,
                nucleus_threshold      : p.nucleus_threshold,
                nucleus_threshold_range: p.nucleus_threshold_range,
                nucleus_stack_histogram: p.nucleus_stack_histogram,
                nucleus_threshold_scope: scope,
                nucleus_particle_size  : p.nucleus_particle_size,
                nucleus_circularity    : p.nucleus_circularity,
                nucleus_watershed      : p.nucleus_watershed,
                // The overview REQUEST, written whether or not it was honoured.
                // These are parameters, so they have to survive the round trip:
                // a config from a GUI run that is fed to the batch must carry
                // the settings that run used, or the batch quietly falls back
                // to the defaults and produces different pictures from the one
                // that was tuned.
                overview_method        : p.overview_method,
                overview_width         : p.overview_width,
                overview_height        : p.overview_height,
                overview_contrast      : p.overview_contrast,
                overview_saturated     : p.overview_saturated,
                // The five save_* switches. They decide WHAT WORK HAPPENS, so
                // they are parameters and must survive the round trip: a config
                // produced by a run that had overviews on, fed back in without
                // them, silently writes none -- and the four that default ON
                // come back on after being switched off. Test_NucleusPipeline
                // asserts every PARAM_TYPES key is written, which is the guard
                // that was missing when these five were not.
                save_roi_zips          : p.save_roi_zips,
                save_outlines          : p.save_outlines,
                save_measurements      : p.save_measurements,
                save_config            : p.save_config,
                save_overview          : p.save_overview,
                // Redundant with save_overview above, and kept: it is the
                // OUTCOME rather than the request, it reads back as provenance
                // rather than as a parameter, and it has been in this file
                // since 0.2.0 beside overview_channels. The two cannot drift --
                // they are the same expression.
                overview_saved         : overviewWritten,
                // Which overview files exist, so a results folder can be read
                // later without guessing. Blank when none were written. PNGs
                // for one frame, TIFFs (one page per frame of frames_analysed)
                // for several -- image_frames says which.
                overview_channels      : (overviewWritten ? ovChannels.join(",") : ""),
                overview_overlay_suffix: (overviewWritten ? OV.OVERLAY_SUFFIX : ""),
                // The display range each channel was rendered at -- "auto"
                // stretches whatever is there, so a channel of pure noise saves
                // a convincing picture, and a narrow range beside a wide one is
                // the tell. One range per channel for a whole series.
                overview_display_range : ovRanges.join(" "),
                // How many ROIs the circularity filter deleted, and BLANK
                // when it was off -- "0" would claim a filter ran and found
                // nothing to remove. The ROIs themselves are gone: unlike the R
                // side's --min_circularity, which marks a row and leaves it in
                // the table, this one drops them before anything is written, so
                // this number is the only surviving evidence.
                // What the threshold actually did, as opposed to what was
                // asked for. nucleus_threshold is the REQUEST and reads back in
                // as a parameter; these two are the RESULT and are ignored on
                // the way in -- a config fed forward must re-derive the
                // threshold for the image it is given, never freeze this one.
                // With several frames these two read `per-frame`, and the
                // counts here are totals: _threshold_stats.tsv has each frame.
                nucleus_threshold_used : thresholdUsed,
                // What the histogram's counts were divided by before the method
                // ran, to keep its int arithmetic from overflowing: 1 = exact
                // counts (RoiDetect.chooseThreshold). A divided threshold is not
                // bit-for-bit what exact arithmetic would give, so it is said.
                // Blank for Manual, which has no histogram.
                nucleus_histogram_divisor: histDivisor,
                nucleus_mask_pct       : maskPct,
                nucleus_circ_rejected  : circRejected,
                nucleus_count          : nucRois.size(),
                nucleoli_enabled       : p.nucleoli_enabled,
                nucleolus_blur_sigma   : p.nucleolus_blur_sigma,
                nucleolus_threshold    : p.nucleolus_threshold,
                nucleolus_rel_fraction : p.nucleolus_rel_fraction,
                nucleolus_erode_px     : p.nucleolus_erode_px,
                nucleolus_particle_size: p.nucleolus_particle_size,
                nucleolus_circularity  : p.nucleolus_circularity,
                nucleolus_count        : nuclRois.size()
            ], outDirPath + seriesId + "_config.txt")
            // The per-frame half of the record above, for every image -- one
            // row when there is one frame -- so every results folder holds the
            // same files.
            RX.joinTables(okTs.collect { new File(stagedFrame(stage, it), "threshold_stats.tsv") },
                          new File(outDirPath + seriesId + "_threshold_stats.tsv"), false)
        }
        // Joined: the staging has done its job.
        stage.deleteDir()
        def stageParent = stage.getParentFile()
        if (stageParent.isDirectory() && !stageParent.list()) stageParent.delete()

        IJ.log("Done: " + seriesId)
        return [series_id : seriesId,
                outDirPath: outDirPath,
                slices    : slices,
                // For batch_summary.tsv: per row, what the threshold chose. The
                // alternative is opening a thousand _config.txt files to find
                // the rows where it went wrong.
                threshold : thresholdUsed,
                maskPct   : maskPct,
                nucRois   : nucRois,  nucNames : nucNames,  nucSlices : nucSlices,
                nuclRois  : nuclRois, nuclNames: nuclNames, nuclSlices: nuclSlices,
                frames    : frameResults]
    }

    /** A finished frame's staged directory: t0001. */
    static File stagedFrame(File stage, int t) {
        return new File(stage, String.format("t%04d", t))
    }

    /** The settings a series' staged frames were made with, beside them. */
    static final String STAGING_SETTINGS = "settings.txt"

    /** batch_summary.tsv's message for a frame a resumed run did not redo. */
    static final String RESUMED_MESSAGE = "staged by an earlier run"

    /** settings.txt keys holding a series threshold as it was chosen. */
    static final String SERIES_T = "series_threshold", SERIES_DIVISOR = "series_histogram_divisor",
                        SERIES_IN = "series_threshold_frames"
    static final List<String> SERIES_CHOSEN_KEYS = [SERIES_T, SERIES_DIVISOR, SERIES_IN]

    /**
     * The first pass of a series threshold: every frame read, its DNA channel
     * blurred exactly as buildMask() will blur it, its histogram added to the
     * series', and the frame released. Nothing but the histogram is kept --
     * 65,536 longs for 16-bit -- so the cost is reading the frames twice.
     *
     * A frame that cannot be read here is left out of the threshold, and
     * reported by the caller as failed rather than analysed with a threshold
     * its pixels were not part of.
     *
     * @return [chosen: chooseThreshold's result, frames: the frames it was chosen over]
     */
    Map seriesThresholdOver(Object src, List<Integer> frames, Map p, int dnaCh) {
        long[] hist = null
        def read = []
        long t0 = System.currentTimeMillis()
        frames.each { int t ->
            ImagePlus frame = null, ch = null
            try {
                frame = src.frame(t)
                ch = RD.blurredChannel(frame, dnaCh, p.nucleus_blur_sigma as double)
                hist = RD.addHistogram(hist, RD.histogramOf(ch))
                read << t
            } catch (Throwable e) {
                IJ.log("  t" + t + ": not readable for the series threshold -- " +
                       e.getClass().getSimpleName() + ": " + (e.getMessage() ?: "(no message)"))
            } finally {
                if (ch != null) { ch.close(); ch.flush() }   // close() alone frees nothing
                if (frame != null) src.release(frame)
            }
        }
        if (hist == null) {
            throw new IllegalStateException("no frame could be read to choose the series threshold")
        }
        def chosen = RD.chooseThreshold(hist, p.nucleus_threshold as String)
        IJ.log("  series threshold over " + read.size() + " frame(s) (" +
               String.format("%.0f", (System.currentTimeMillis() - t0) / 1000d) + " s): " +
               describeChosen(chosen, p))
        return [chosen: chosen, frames: read]
    }

    /** "Otsu -> 113-65535", with the divisor when the counts had to be divided. */
    static String describeChosen(Map chosen, Map p) {
        if (chosen.t == null) return p.nucleus_threshold + " -> nothing to separate"
        return p.nucleus_threshold + " -> " + ((chosen.t as int) + 1) + "-max" +
               ((chosen.divisor != null && (chosen.divisor as long) > 1L) ? (" (counts divided by " + chosen.divisor + ")") : "")
    }

    /**
     * Everything that decides what a staged frame holds: every run parameter,
     * the code, and which image it is. Two runs that agree on all of it stage
     * the same bytes for a frame, so their frames can be joined. Which frames
     * is not here -- a resume may ask for more or fewer than the run it
     * continues -- and nor is how the image was opened, which
     * Test_BatchRunner holds to identical bytes either way.
     */
    Map<String, String> stagingSettings(Object src, Map p) {
        def s = new LinkedHashMap<String, String>()
        def str = { v -> v == null ? "" : v.toString() }
        s.code_version   = RX.repoVersion(libDir)
        // The thresholds and the particle analysis are ImageJ's: an update
        // between two runs can move them.
        s.imagej_version = IJ.getVersion()
        s.source_file    = str(p.source_file)
        s.series_index   = str(p.series_index)
        s.series_name    = str(p.series_name)
        s.image_width    = str(src.width)
        s.image_height   = str(src.height)
        s.image_slices   = str(src.nSlices)
        s.image_channels = str(src.nChannels)
        s.image_frames   = str(src.nFrames)
        s.pixel_width    = str(src.calibration.pixelWidth)
        s.pixel_height   = str(src.calibration.pixelHeight)
        s.pixel_depth    = str(src.calibration.pixelDepth)
        PARAM_TYPES.keySet().each { k -> s[k] = str(p[k]) }
        return s
    }

    /**
     * Take over what an earlier, unfinished run of this series staged: its
     * finished frames stay, its half-written ones (`.part`) go. Refused when
     * the earlier run's settings differ from `settings`, or were never
     * recorded -- frames nobody can vouch for are not joined into anything.
     *
     * @return the frames already finished; none means the staging was
     *         discarded and the run starts afresh
     */
    Map resumeStaging(File stage, Map<String, String> settings, String seriesId) {
        def entries = (stage.listFiles() ?: []) as List<File>
        int parts = 0
        entries.findAll { it.getName().endsWith(".part") }.each { it.deleteDir(); parts++ }
        def done = entries.findAll { it.isDirectory() && it.getName() ==~ /t\d{4}/ }
                          .collect { it.getName().substring(1) as int }.sort()
        if (!done) {
            IJ.log("  discarding the staging of an earlier run, which finished no frame")
            stage.deleteDir()
            return [frames: [], settings: [:]]
        }
        def fix = "Rerun with the settings it used to resume it, or with existingOutput=redo_all to discard it " +
                  "(" + stage.getPath() + ")."
        def f = new File(stage, STAGING_SETTINGS)
        if (!f.isFile()) {
            throw new IllegalStateException(seriesId + ": an earlier, unfinished run staged frame(s) " +
                SS.describeFrames(done) + " without recording its settings, so they cannot be joined " +
                "to frames analysed now. " + fix)
        }
        def then = [:]
        f.readLines("UTF-8").drop(1).each { String l ->
            int tab = l.indexOf("\t")
            if (tab > 0) then[l.substring(0, tab)] = l.substring(tab + 1)
        }
        // The series threshold is what the run CHOSE, not what it was asked;
        // it is taken from here, never compared.
        def differ = (settings.keySet() + then.keySet()).findAll {
            !(it in SERIES_CHOSEN_KEYS) && settings[it] != then[it] }
        if (differ) {
            def show = { v -> v == null ? "(absent)" : ("'" + v + "'") }
            throw new IllegalStateException(seriesId + ": an earlier, unfinished run staged frame(s) " +
                SS.describeFrames(done) + " with other settings -- " +
                differ.collect { it + " " + show(then[it]) + " then, " + show(settings[it]) + " now" }.join("; ") +
                ". Joined, the series' files would hold frames analysed two ways. " + fix)
        }
        IJ.log("  resuming: frame(s) " + SS.describeFrames(done) + " staged by an earlier run" +
               (parts ? (", " + parts + " half-written one(s) discarded") : ""))
        return [frames: done, settings: then]
    }

    /**
     * A finished frame back from its staging: its ROIs, as the zip saved them
     * -- name, slice and frame included -- and its threshold-stats row.
     * Checked against the counts that row recorded, so a staging that lost a
     * file fails here rather than joining short.
     */
    Map loadStagedFrame(File dir) {
        def lines = new File(dir, "threshold_stats.tsv").readLines("UTF-8")
        def head = lines[0].split("\t", -1), vals = lines[1].split("\t", -1)
        def stats = [:]
        head.eachWithIndex { String h, int i -> stats[h] = (i < vals.length ? vals[i] : "") }
        def out = [stats: stats]
        ["nucleus", "nucleolus"].each { String feature ->
            def zip = new File(dir, feature + "_outline_ROIs.zip")
            def rois = zip.isFile() ? RX.loadRoiZip(zip.getPath()) : []
            def want = stats[feature + "_count"]
            if (want != null && want != "" && (want as int) != rois.size()) {
                throw new IllegalStateException(dir.getPath() + ": " + rois.size() + " " + feature +
                    " ROI(s) staged where the frame found " + want)
            }
            out[feature] = [rois  : rois,
                            names : rois.collect { it.getName() },
                            slices: rois.collect { it.getZPosition() }]
        }
        return out
    }

    /**
     * Write one finished frame's tables into the staging, as the series' files
     * would hold them, then rename the frame's directory into place. A frame
     * whose directory exists is complete; a `.part` one is not.
     */
    void stageFrame(File stage, int t, String seriesId, Object cal, Map fr, Map tables, Map p,
                    Closure overview = null) {
        def done = stagedFrame(stage, t)
        def part = new File(stage, done.getName() + ".part")
        part.deleteDir()
        part.mkdirs()
        ["nucleus", "nucleolus"].each { String feature ->
            def f = fr[feature]
            if (f.rois.isEmpty()) return
            def stem = new File(part, feature).getPath()
            def ts = f.rois.collect { t }
            if (p.save_outlines)     RX.saveOutlineCoords(cal, f.rois, f.names, f.slices, ts, seriesId, stem + "_outline.txt")
            // Always: the zip is how a resumed run gets this frame's ROIs back
            // (loadStagedFrame). The join writes it out only for save_roi_zips.
            RX.saveRoiZip(f.rois, f.names, stem + "_outline_ROIs.zip")
            if (p.save_measurements) RX.saveMeasurements(tables[feature], stem + "_res.txt")
        }
        RX.saveThresholdStats([fr.stats], new File(part, "threshold_stats.tsv").getPath())
        // The frame's share of a multi-frame overview: staged with its tables,
        // so a frame is in the overview exactly when its results are.
        if (overview != null) overview.call(part)
        if (!part.renameTo(done)) {
            throw new IOException("could not finish staging " + done)
        }
    }

    /**
     * One frame of an open multi-frame image, as an image of its own: every
     * channel and slice of frame t. Titled as the original -- the measurement
     * Label carries the title, and a Duplicator copy is called DUP_<title>.
     */
    /** The frame interval, or null for one frame or when the file did not record one. */
    static Double frameInterval(ImagePlus imp) {
        double fi = imp.getCalibration().frameInterval
        return (imp.getNFrames() > 1 && fi > 0d) ? fi : null
    }

    static ImagePlus frameOf(ImagePlus imp, int t) {
        def f = new Duplicator().run(imp, 1, imp.getNChannels(), 1, imp.getNSlices(), t, t)
        f.setTitle(imp.getTitle())
        return f
    }

    /**
     * Detect, name and measure the nuclei and nucleoli of ONE frame.
     *
     * `frame` is always a single-frame image; t is its number in the series,
     * from 1. ROI ids gain a `TTTT-` field only when the series has more than
     * one frame (`multi`): four frames of one object would otherwise share a
     * name, and the zip refuses a repeated entry. Measurement happens here,
     * while the frame's pixels exist; the rows go into `tables`.
     *
     * @return [nucleus: [rois, names, slices], nucleolus: [...], stats: the
     *         frame's _threshold_stats.tsv row]
     */
    Map runFrame(ImagePlus frame, int t, boolean multi, Map p, int dnaCh,
                 List<Integer> channels, Collection<Integer> slices, Map tables, Map chosen = null) {
        String tag   = multi ? ("t" + t + " ") : ""
        String tPart = multi ? String.format("%04d-", t) : ""
        def measure = { String feature, List rois, List names, List sls ->
            IJ.log("  " + tag + feature + ": " + rois.size() + " ROIs")
            if (rois.isEmpty() || !p.save_measurements) return
            RX.measureInto(frame, rois, names, sls, channels, tables[feature], t)
        }

        // --- Nucleus ---------------------------------------------------------
        def built = RD.buildMask(frame, dnaCh, p.nucleus_blur_sigma as double,
                                 p.nucleus_threshold as String, true,
                                 p.nucleus_watershed as boolean,
                                 [range         : (p.nucleus_threshold_range ?: ""),
                                  stackHistogram: (p.nucleus_stack_histogram == null)
                                                  ? true : (p.nucleus_stack_histogram as boolean),
                                  // A series threshold, chosen over every frame.
                                  chosen        : chosen])
        def dna = built.mask
        // The threshold is reported as the RANGE it selected, and the coverage
        // beside it. Both were previously thrown away, which left a run unable
        // to say what it had thresholded at: no way to tell a sensible
        // threshold from a disastrous one afterwards, and no way to read a
        // value off in order to pin it.
        //
        // Coverage is the cheap signal that catches both failures an ROI count
        // hides. 0.00% is a blank field; a number in the tens is the frame
        // being selected rather than the objects in it. The size filter turns
        // both into "no nuclei", which look identical.
        def thresholdUsed = built.threshold
        def maskPct       = String.format("%.2f", built.coverage)
        IJ.log("  " + tag + "threshold: " + p.nucleus_threshold + " -> " + thresholdUsed +
               "  (mask " + maskPct + "% of pixels)")
        // Circularity is a SECOND line of defence after size, for imaging
        // artefacts -- a reflection off the section edge thresholds like an
        // object and is often the wrong shape for one.
        //
        // It is a pre-grouping filter, so it carries the hazard CLAUDE.md
        // records for --min_circularity on the R side: dropping an ROI from the
        // middle of an object opens a z-gap, and one object gets counted as two.
        // Worse here than there, because the R filter marks the ROI and leaves
        // it in the table for --bridge_roi to use, while this one deletes it
        // before anything downstream can see that it existed.
        //
        // Hence the count: a filter that removes things silently is this repo's
        // signature failure. The unfiltered pass runs ONLY when the filter is
        // on, so the default costs nothing.
        def nucCirc     = (p.nucleus_circularity ?: "") as String
        def circRange   = RD.parseRange(nucCirc, 0d, 1d)
        boolean circOn  = (circRange[0] > 0d || circRange[1] < 1d)
        def nucRois   = RD.detect(dna, p.nucleus_particle_size as String, nucCirc, slices, true, true)
        def circRejected = ""
        if (circOn) {
            int before = RD.detect(dna, p.nucleus_particle_size as String, "", slices, true, true).size()
            circRejected = before - nucRois.size()
            IJ.log("  " + tag + "nucleus: circularity " + nucCirc + " rejected " + circRejected +
                   " of " + before + " ROI(s)")
        }
        def nucNames  = RD.autoLabels(nucRois).collect { "nucleus_" + tPart + it }
        def nucSlices = nucRois.collect { it.getPosition() }
        // NB: set the name ON the Roi, not just in the parallel names list. The
        //     name is encoded into the .roi file and picked up by Analyzer into
        //     the Label column, which is how read_fiji_result.r joins
        //     measurements to outlines.
        nucRois.eachWithIndex { r, i -> r.setName(nucNames[i]) }
        // close() then flush(). close() alone frees NOTHING while a reference
        // is still in scope -- measured headless, 0 MB of 768 MB released --
        // because it only detaches a window, and there is no window. `dna` stays
        // in scope to the end of the method, so without flush() this mask
        // survives every later stage. It is why s0014 still ran out of heap in
        // the overview after buildMask was fixed: the mask was nominally closed
        // and still occupying 2594 MB.
        dna.close(); dna.flush()
        measure("nucleus", nucRois, nucNames, nucSlices)

        // --- Nucleolus -------------------------------------------------------
        def nuclRois = [], nuclNames = [], nuclSlices = []
        if (p.nucleoli_enabled && !nucRois.isEmpty()) {
            int erodePx = p.nucleolus_erode_px as int
            def useRois = (erodePx > 0) ? nucRois.collect { RoiEnlarger.enlarge(it, -erodePx) } : nucRois
            def dna2 = new Duplicator().run(frame, dnaCh, dnaCh, 1, frame.getNSlices(), 1, 1)
            def mask = ND.buildNucleolusMask(dna2, useRois, nucSlices,
                                             p.nucleolus_blur_sigma as double,
                                             p.nucleolus_threshold as String,
                                             p.nucleolus_rel_fraction as double)
            dna2.close(); dna2.flush()   // close() alone frees nothing; see above
            nuclRois   = RD.detect(mask, p.nucleolus_particle_size as String,
                                   p.nucleolus_circularity as String, slices, true, false)
            nuclNames  = RD.autoLabels(nuclRois).collect { "nucleolus_" + tPart + it }
            nuclSlices = nuclRois.collect { it.getPosition() }
            nuclRois.eachWithIndex { r, i -> r.setName(nuclNames[i]) }
            mask.close(); mask.flush()   // close() alone frees nothing; see above
            measure("nucleolus", nuclRois, nuclNames, nuclSlices)
        }


        // Where a multi-frame ROI belongs, for anything that shows it over the
        // series: the ROI Manager, or the zip dropped on the hyperstack. Set
        // after measuring, which positions the frame itself. A single frame's
        // ROIs keep the flat position they always had.
        if (multi) {
            [[nucRois, nucSlices], [nuclRois, nuclSlices]].each { pair ->
                pair[0].eachWithIndex { r, i -> r.setPosition(0, pair[1][i] as int, t) }
            }
        }
        return [nucleus  : [rois: nucRois,  names: nucNames,  slices: nucSlices],
                nucleolus: [rois: nuclRois, names: nuclNames, slices: nuclSlices],
                stats    : [t                     : t,
                            nucleus_threshold_used: thresholdUsed,
                            nucleus_histogram_divisor: (built.divisor == null ? "" : built.divisor),
                            nucleus_mask_pct      : maskPct,
                            nucleus_circ_rejected : circRejected,
                            nucleus_count         : nucRois.size(),
                            nucleolus_count       : nuclRois.size()]]
    }
}
