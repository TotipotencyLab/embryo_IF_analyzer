// BatchRunner.groovy
//
// Run the nucleus pipeline over every included row of a series table.
//
// The loop itself is the interesting part, not the analysis: the analysis is
// NucleusPipeline, unchanged and shared with the interactive runner. What this
// adds is everything that only matters when there are two hundred of them.
//
//   * ONE FAILURE MUST NOT COST THE REST. An exception on series 57 of 200 is a
//     fact about series 57. Every row is wrapped, and what happened to each is
//     written to batch_summary.tsv -- which is the difference between "the batch
//     ran" and "the batch ran and I know which three need attention".
//   * EVERY IMAGE IS CLOSED. 16 GB here, and one 1400x1400x61 stack wanted ~6 GB.
//     A leak dies around image twelve with an OutOfMemoryError that looks
//     exactly like a code bug.
//   * THE SHEET IS CHECKED AGAINST THE FILE. If the recorded dimensions no
//     longer match what opened, the sheet was generated from a different version
//     of that file, and every parameter chosen from it is suspect.
//   * MIXED PIXEL SIZES ARE REPORTED. nucleus_particle_size is in calibrated
//     units and transfers between series; nucleus_blur_sigma goes to
//     "Gaussian Blur... sigma=" and is in PIXELS. One Leica file here holds
//     0.4456, 0.2227 and 0.1098 um pixels, so the same sigma is four times the
//     physical blur at one end of it than the other, and nothing would say so.

import ij.ImagePlus
import ij.ImageStack
import ij.Prefs
import loci.formats.ChannelSeparator
import loci.formats.ImageReader
import loci.formats.MetadataTools
import loci.plugins.util.ImageProcessorReader

class BatchRunner {

    String libDir
    Class TSV, NP, RC, RD, RX, SCHEMA, SS, LFC, LS, TA
    Object pipeline

    static BatchRunner load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader(BatchRunner.class.classLoader)
        def b = new BatchRunner()
        b.libDir = dir.getAbsolutePath()
        b.TSV = gcl.parseClass(new File(dir, "Tsv.groovy"))
        b.NP = gcl.parseClass(new File(dir, "NucleusPipeline.groovy"))
        b.RC = gcl.parseClass(new File(dir, "RunConfig.groovy"))
        // For the up-front threshold check only; the per-image work goes
        // through NucleusPipeline, which parses its own copy.
        b.RD = gcl.parseClass(new File(dir, "RoiDetect.groovy"))
        b.SCHEMA = gcl.parseClass(new File(dir, "SheetSchema.groovy"))
        b.RX = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        // A series that comes from a sources table (Luxendo) rather than a file.
        b.SS  = gcl.parseClass(new File(dir, "SeriesSource.groovy"))
        b.LFC = gcl.parseClass(new File(dir, "LuxendoFile.groovy"))
        b.LS  = gcl.parseClass(new File(dir, "LuxendoScan.groovy"))
        // For parseFrames(), the one reading of a time-point selection.
        b.TA  = gcl.parseClass(new File(dir, "TiffAssembler.groovy"))
        b.pipeline = b.NP.load(b.libDir)
        return b
    }

    /** include is a real boolean, never "a non-empty string is true". */
    /** What a mixed-pixel-size batch means, when nothing more is known. */
    static final String PIXEL_SIZE_NOTE_GENERIC =
        "Settings given in PIXELS are a different physical size on each; settings in " +
        "calibrated units transfer. Check one series from each group before trusting the rest."

    /** ...and what it means for the nucleus pipeline specifically. */
    static final String PIXEL_SIZE_NOTE_NUCLEUS =
        "nucleus_particle_size is in calibrated units and transfers; nucleus_blur_sigma, " +
        "nucleolus_blur_sigma and nucleolus_erode_px are in PIXELS and do NOT -- the same " +
        "value is a different physical size on each. Check the result on one series from " +
        "each group before trusting the rest."

    static boolean isIncluded(Object v) {
        if (v == null) return true                       // no column: everything
        def s = v.toString().trim().toLowerCase()
        if (!s) return true
        if (s in ["true", "yes", "1"]) return true
        if (s in ["false", "no", "0"]) return false
        throw new IllegalArgumentException(
            "include must be true/false (or yes/no, 1/0); got >>>" + v + "<<<")
    }

    static File resolve(String path, File imageRoot) {
        def f = new File(path)
        if (f.isAbsolute() || imageRoot == null) return f
        return new File(imageRoot, path)
    }

    // --- opening one series ---------------------------------------------------
    //
    // TWO WAYS TO DO IT, and on a big file they differ by three orders of
    // magnitude.
    //
    //   importer  BF.openImagePlus -- the code behind the Bio-Formats dialog. It
    //             prepares a description of EVERY series in the file before
    //             returning the one asked for, because that is what a series
    //             chooser needs. setWindowless(true) skips SHOWING the dialog,
    //             not building it. Measured on a 1563-series .lif: 182 s per
    //             call, against 1.5 s for the same call on a 15-series file.
    //             Paid once per ROW, so ~10 h of overhead for 200 rows.
    //
    //   reader    One reader held open for the whole file; setSeries() moves a
    //             cursor and the planes are assembled into an ImagePlus here.
    //             The same 16 MiB series that took 182 s takes 0.11 s, and a
    //             second series through the same reader 0.09 s.
    //
    // The importer is KEPT, not replaced. It handles format corners this code
    // does not (RGB, palette, packed bit depths, multi-file datasets), and
    // keeping both is what lets Test_BatchRunner assert the two produce the same
    // output. That assertion is the only thing standing between us and a silent
    // drift when Bio-Formats is next upgraded: the hand-built path reimplements
    // what the library does for us, which is exactly the kind of code that rots
    // without anyone noticing.
    //
    // WHICH ONE RAN IS RECORDED, in _config.txt and in batch_summary.tsv. Two
    // runs that used different readers must not be indistinguishable afterwards
    // -- same reasoning as recording VERSION.

    static final List<String> OPEN_MODES = ["auto", "importer", "reader"]

    /** The batch dialog's overview value meaning "whatever the config says". */
    static final String OVERVIEW_FROM_CONFIG = "(from config)"
    static final List<String> OVERVIEW_CHOICES = [OVERVIEW_FROM_CONFIG, "yes", "no"]

    /**
     * Resolve the batch dialog's overview switch. `null` means leave the
     * config's own save_overview alone.
     *
     * It lives here rather than in the `#@` front end so that it can be tested:
     * a front end is only ever compiled by the test suite, never run.
     *
     * ⚠️ AND IT MUST REFUSE WHAT IT DOES NOT RECOGNISE. A `#@ String` with
     * `choices={...}` is NOT validated against those choices on the command
     * line -- SciJava passes any string straight through. Measured: a script
     * declaring choices {"(from config)","yes","no"} and run with `pick=true`
     * receives the String "true".
     *
     * That matters because this switch used to be a Boolean. A caller carrying
     * `saveOverview=true` from before the change would otherwise land in the
     * override branch, compare "true" == "yes", and SILENTLY TURN OVERVIEWS
     * OFF -- the opposite of what it says. Same reasoning as OPEN_MODES, which
     * has refused an unknown mode since it was added.
     */
    static Boolean overviewOverride(Object v) {
        String s = (v == null || !v.toString().trim()) ? OVERVIEW_FROM_CONFIG
                                                       : v.toString().trim()
        if (!OVERVIEW_CHOICES.contains(s)) {
            throw new IllegalArgumentException(
                "saveOverview must be one of " + OVERVIEW_CHOICES.join(", ") +
                "; got >>>" + v + "<<<\n" +
                "  It was a true/false switch before, and a command line is not " +
                "checked against the dialog's choices -- so a leftover " +
                "saveOverview=true would have meant 'no'.")
        }
        return (s == OVERVIEW_FROM_CONFIG) ? null : (s == "yes")
    }

    /**
     * Above this many series in the FILE, `auto` stops using the importer.
     *
     * The cost is (rows x series-in-file), not series alone, so no single number
     * is right everywhere. 16 keeps the importer for ordinary acquisitions,
     * where it is well-tested and costs about a second, and switches away before
     * the per-call overhead can accumulate into anything.
     */
    static final int AUTO_SERIES_MAX = 16

    // One reader, held open, reused by every row of the same file.
    //
    // Deliberately a single entry and not a map: rows arrive grouped by file,
    // and a one-entry cache degrades to REOPENING (2.3 s) rather than to the
    // importer's 182 s if someone reorders the sheet. That keeps
    // batch_summary.tsv in sheet order, which is the order a human reads it in.
    private String openPath = null
    private Object openReader = null
    private Object openMeta = null

    /** The held-open reader for this file, opening it if it is a different one. */
    Object readerFor(File image) {
        def path = image.getAbsolutePath()
        if (openPath == path && openReader != null) {
            return openReader
        }
        closeReader()
        def meta = MetadataTools.createOMEXMLMetadata()
        // ChannelSeparator: one channel per plane, so getIndex(z, c, t) is valid
        // whatever the file's own interleaving is.
        def rd = new ImageProcessorReader(new ChannelSeparator(new ImageReader()))
        rd.setMetadataStore(meta)
        rd.setId(path)
        openPath = path; openReader = rd; openMeta = meta
        return rd
    }

    /** Release the held file handle. run() calls this on every exit path. */
    void closeReader() {
        if (openReader != null) {
            try { openReader.close() } catch (ignored) { }
        }
        openPath = null; openReader = null; openMeta = null
    }

    /** "auto" -> the method actually used; anything else is taken literally. */
    String resolveMethod(String mode, File image) {
        def m = (mode ?: "auto").toString().trim().toLowerCase()
        if (!OPEN_MODES.contains(m)) {
            throw new IllegalArgumentException(
                "open_mode must be one of " + OPEN_MODES.join(", ") + "; got >>>" + mode + "<<<")
        }
        if (m != "auto") {
            return m
        }
        // Costs one header parse per file -- which the reader path needs anyway,
        // and which is 2.3 s even on the 200 GiB file.
        return (readerFor(image).getSeriesCount() <= AUTO_SERIES_MAX) ? "importer" : "reader"
    }

    Object openSeries(File image, int seriesIndex, String method) {
        return (method == "reader") ? openSeriesReader(image, seriesIndex)
                                    : openSeriesImporter(image, seriesIndex)
    }

    /**
     * Open one series through the Bio-Formats importer.
     *
     * setZEnd takes a 0-BASED INDEX, not a count: passing the slice count on a
     * single-plane series throws "Invalid Z index: 1/1". Not used here -- the
     * whole stack is wanted -- but the reason this opens by series and not by
     * range is that the trap is one line away.
     */
    Object openSeriesImporter(File image, int seriesIndex) {
        // ImporterOptions is BACKED BY IMAGEJ PREFERENCES, and the importer
        // calls saveOptions() after a successful open -- so setWindowless(true)
        // here does not stay here. It lands in IJ_Prefs.txt as
        // `.bioformats.windowless=true` and the operator's Fiji then stops
        // showing the series chooser when they drag a .lif onto it. Observed
        // exactly that after one run.
        //
        // Same family as Set Measurements, Prefs.blackBackground and SciJava's
        // `#@` persistence: a global preference that a run must not be allowed
        // to redecorate. The value is put back whatever happens.
        boolean prevWindowless = Prefs.get("bioformats.windowless", false)
        try {
            def opt = Class.forName("loci.plugins.in.ImporterOptions").newInstance()
            opt.setId(image.getAbsolutePath())
            opt.setWindowless(true)
            opt.clearSeries()
            opt.setSeriesOn(seriesIndex, true)
            def imps = Class.forName("loci.plugins.BF").openImagePlus(opt)
            if (imps == null || imps.length == 0) {
                throw new IllegalStateException(
                    "Bio-Formats opened no image for series " + seriesIndex + " of " + image.getName())
            }
            return imps[0]
        } finally {
            Prefs.set("bioformats.windowless", prevWindowless)
        }
    }

    /**
     * Assemble one series into an ImagePlus off the held-open reader.
     *
     * Everything the importer was doing for us now has to be done here, and the
     * two that matter are silent when wrong:
     *
     *   CALIBRATION. A bare ImagePlus measures in PIXELS. On a 0.22 um pixel
     *   every area would come out ~20x wrong with nothing on screen to say so --
     *   the exact failure mode this repo keeps being bitten by.
     *   TITLE. It reaches _config.txt as image_title, so the two paths have to
     *   spell it identically or two output folders differ for no real reason.
     */
    Object openSeriesReader(File image, int seriesIndex) {
        def rd = readerFor(image)
        if (seriesIndex < 0 || seriesIndex >= rd.getSeriesCount()) {
            throw new IllegalArgumentException(
                "Series " + seriesIndex + " out of range: " + image.getName() +
                " has 0.." + (rd.getSeriesCount() - 1))
        }
        rd.setSeries(seriesIndex)
        int nz = rd.getSizeZ(), nc = rd.getSizeC(), nt = rd.getSizeT()
        def stack = new ImageStack(rd.getSizeX(), rd.getSizeY())
        // XYCZT: channels fastest, matching setDimensions(c, z, t) below. Get
        // this order wrong and channels and slices transpose -- the measurements
        // stay valid numbers and are attributed to the wrong channel.
        def name = openMeta?.getImageName(seriesIndex)
        for (int t = 0; t < nt; t++) {
            for (int z = 0; z < nz; z++) {
                for (int c = 0; c < nc; c++) {
                    stack.addSlice(sliceLabel(name, z, nz, c, nc, t, nt),
                                   rd.openProcessors(rd.getIndex(z, c, t))[0])
                }
            }
        }
        def imp = new ImagePlus(seriesTitle(image, seriesIndex), stack)
        imp.setDimensions(nc, nz, nt)
        applyCalibration(imp, seriesIndex, nz)
        return imp
    }

    /**
     * The slice label Bio-Formats puts on each plane: "c:1/3 z:1/56 - Series001".
     *
     * Not decoration. It reaches _res.txt through the Label column -- the same
     * column read_fiji_result.r digs the roi id out of -- so a stack without it
     * writes a measurement table that differs from the importer's in every row.
     * Found exactly that way: every NUMBER agreed and the labels did not.
     *
     * A component is omitted when its dimension has only one entry, which is
     * why the format has to be reproduced rather than guessed at.
     */
    static String sliceLabel(String seriesName, int z, int nz, int c, int nc, int t, int nt) {
        def sb = new StringBuilder()
        if (nc > 1) sb.append("c:").append(c + 1).append("/").append(nc).append(" ")
        if (nz > 1) sb.append("z:").append(z + 1).append("/").append(nz).append(" ")
        if (nt > 1) sb.append("t:").append(t + 1).append("/").append(nt).append(" ")
        sb.append("- ").append(seriesName ?: "")
        return sb.toString()
    }

    /**
     * The title the importer gives a series.
     *
     * "<file> - <series name>" when the file holds MORE THAN ONE series, and the
     * bare filename when it holds one: Bio-Formats only disambiguates when there
     * is something to disambiguate. The title reaches _config.txt as
     * image_title, so the rule has to be copied rather than tidied up -- a
     * single-series file titled "x.tif - P" is a difference in every output
     * folder written that way.
     */
    String seriesTitle(File image, int seriesIndex) {
        def rd = readerFor(image)
        def name = openMeta?.getImageName(seriesIndex)
        return (name && rd.getSeriesCount() > 1) ? (image.getName() + " - " + name)
                                                 : image.getName()
    }

    /** Pixel size and unit, copied off the metadata store by hand. */
    void applyCalibration(Object imp, int seriesIndex, int nz) {
        def cal = imp.getCalibration()
        def px = openMeta.getPixelsPhysicalSizeX(seriesIndex)
        def py = openMeta.getPixelsPhysicalSizeY(seriesIndex)
        def pz = openMeta.getPixelsPhysicalSizeZ(seriesIndex)
        if (px != null) {
            cal.pixelWidth = px.value().doubleValue()
            cal.setUnit(ijUnit(px.unit().getSymbol()))
        }
        if (py != null) {
            cal.pixelHeight = py.value().doubleValue()
        }
        // Only where there IS a z axis. ImageJ defaults pixelDepth to 1.0, and a
        // z step that does not exist must not arrive as a usable-looking number;
        // _config.txt follows the same rule.
        if (pz != null && nz > 1) {
            cal.pixelDepth = pz.value().doubleValue()
        }
    }

    /**
     * OME unit symbol -> the spelling ImageJ uses.
     *
     * The importer writes "micron"; the metadata store says "um" (mu). That
     * string reaches _config.txt as pixel_unit, so without this the two paths
     * produce output folders differing in one word -- invisible until someone
     * diffs them, which is how it was found.
     */
    static String ijUnit(String symbol) {
        if (symbol == "µm" || symbol == "um") {
            return "micron"
        }
        return symbol
    }

    // --- the one resolver -----------------------------------------------------
    //
    // How a series row becomes frames. MEMBERSHIP decides, never a guess about
    // `path`: a row whose series_id has rows in the sources table is read from
    // them; any other row is a file. A stale path or a moved mount must not
    // quietly take the other branch (note/time_series_plan.md §3.2b), and
    // membership lets one sheet mix Luxendo and ordinary files.

    /**
     * The sources table, typed and joined to the series table.
     *
     * The join is LuxendoScan.withSeriesId(): on (alias, series_index), never on
     * series_id, which you may edit. A source with no series row stops the
     * batch -- the two tables did not come from one scan.
     *
     * @return series_id -> its sources rows
     */
    Map<String, List<Map>> loadSources(List<Map> sourceRows, List<Map> seriesRows) {
        if (!sourceRows) return [:]
        def need = ["source_path", "alias", "series_index", "channel", "t"]
        def absent = need - sourceRows[0].keySet().toList()
        if (absent) {
            throw new IllegalArgumentException(
                "The sources table is missing column(s) " + absent.join(", ") +
                (sourceRows[0].containsKey("series_id")
                 ? " -- it was written before v0.7.0. Regenerate both tables with Make_LuxendoSheets."
                 : " -- is it a sources.tsv from Make_LuxendoSheets?"))
        }
        def joined = LS.withSeriesId(SS.typed(sourceRows), seriesRows)
        return joined.sources.groupBy { it.series_id as String }
    }

    /**
     * Open one series row as a SeriesSource.
     *
     * @param sources series_id -> sources rows (loadSources), or empty
     */
    Object openSource(Map row, File imageRoot, String openMode, Map<String, List<Map>> sources) {
        def seriesId = (row.series_id ?: "").toString()
        if (sources?.containsKey(seriesId)) {
            // `path` on a Luxendo row is the ACQUISITION DIRECTORY, the root
            // source_path is relative to.
            def dir = resolve(row.path.toString(), imageRoot)
            if (!dir.isDirectory()) {
                throw new IllegalArgumentException("no such acquisition directory: " + dir.getAbsolutePath())
            }
            return SS.ofLuxendo(sources[seriesId], dir, seriesId, LFC)
        }
        def image = resolve(row.path.toString(), imageRoot)
        if (!image.isFile()) {
            throw new IllegalArgumentException("no such image file: " + image.getAbsolutePath() +
                                               (sources ? " (and no rows in the sources table)" : ""))
        }
        int si = (row.series_index ?: "0").toString() as Integer
        def method = resolveMethod(openMode, image)
        return SS.ofImage(openSeries(image, si, method), method, true)
    }

    /** Sheet says one thing, the file says another: the sheet is stale. */
    static List<String> checkDimensions(Map row, Object imp) {
        def out = []
        def cmp = { String col, Object got ->
            def want = (row[col] ?: "").toString()
            if (want && want != got.toString()) {
                out << (col + ": sheet says " + want + ", the image says " + got)
            }
        }
        // An ImagePlus or a SeriesSource: the same four facts either way.
        boolean isImp = (imp instanceof ImagePlus)
        cmp("size_x", isImp ? imp.getWidth()     : imp.width)
        cmp("size_y", isImp ? imp.getHeight()    : imp.height)
        cmp("size_z", isImp ? imp.getNSlices()   : imp.nSlices)
        cmp("size_c", isImp ? imp.getNChannels() : imp.nChannels)
        return out
    }

    /**
     * The pixel sizes present among the rows about to run.
     *
     * @return map of pixel_width -> list of series_ids
     */
    static Map pixelSizes(List<Map> rows) {
        return rows.findAll { (it.pixel_width ?: "").toString() }
                   .groupBy { it.pixel_width.toString() }
                   .collectEntries { k, v -> [(k): v.collect { it.series_id }] }
    }

    /**
     * Run the batch.
     *
     * @param rows      series table rows (all of them; include is applied here)
     * @param imageRoot base for relative paths, or null
     * @param params    NucleusPipeline parameters, already merged over defaults
     * @param outdir    where results and batch_summary.tsv go
     * @param log       progress closure
     * @return [summary: rows, ok: n, failed: n, excluded: n, warnings: []]
     */
    Map run(List<Map> rows, File imageRoot, Map params, File outdir, Closure log = null) {
        // The nucleus batch: the shared loop below, plus the work one row does
        // and the columns that work reports. Kept as its own entry point
        // because it is what every existing caller asks for.
        def cols = ["threshold", "mask_pct", "n_nucleus", "n_nucleolus"]
        // Which time points of a multi-frame series to analyse; parsed ONCE,
        // so a typo costs one message rather than one per row.
        def wanted = TA.parseFrames(params.frames)
        def res = runEach(rows, imageRoot,
                          params + [pixel_size_note: PIXEL_SIZE_NOTE_NUCLEUS],
                          outdir, cols, log) {
                      src, seriesId, si, method, row ->
            // The sheet's series_id is authoritative: an id taken from the image
            // title would be "Series001", which recurs in every file.
            // Where this image came from, recorded in its own _config.txt: a
            // results folder should say which series of which file produced it
            // without anyone having to parse the id back apart.
            def r = pipeline.runSource(src, outdir,
                                       params + [series_id   : seriesId,
                                                 open_method : method,
                                                 source_file : row.path,
                                                 series_index: si,
                                                 series_name : (row.series_name ?: "")],
                                       wanted)
            // What the threshold chose, per FRAME. Every _config.txt and
            // _threshold_stats.tsv carries it too, but finding the handful of
            // frames where it went wrong should not mean opening a thousand
            // files -- and on a slide that scans across empty sections, or a
            // time course that bleaches, "went wrong" is the common case.
            return [frames: r.frames.collect { fr ->
                def st = fr.stats ?: [:]
                [t: fr.t, status: fr.status, message: fr.message ?: "",
                 seconds: fmtSeconds(fr.seconds as long),
                 threshold: st.nucleus_threshold_used ?: "", mask_pct: st.nucleus_mask_pct ?: "",
                 n_nucleus: (st.nucleus_count == null ? "" : st.nucleus_count),
                 n_nucleolus: (st.nucleolus_count == null ? "" : st.nucleolus_count)]
            }]
        }

        // The parameters actually used, as a file that can be fed straight back
        // in -- provenance for the batch as a whole, beside the per-image config.
        RC.writeParams(params.findAll { k, v -> NP.PARAM_TYPES.containsKey(k) },
                       new File(outdir, "batch_params.txt"))
        return res
    }

    /**
     * Run a closure over every included row, and write the summary.
     *
     * Everything that is the same for any batch lives here: include, the
     * duplicate-series_id refusal, resolving and opening the image, the pixel-size
     * warning, closing the stack on both paths, one row's failure not costing
     * the other hundred and ninety-nine, and batch_summary.tsv.
     *
     * @param extraCols the columns `work` contributes, in order. They sit
     *                  between open_method and seconds, and are written blank
     *                  for excluded and failed rows -- so every row has every
     *                  column and the table is rectangular whatever happened.
     * @param work      called as (source, seriesId, seriesIndex, openMethod, row),
     *                  `source` a SeriesSource; returns a Map of extraCols ->
     *                  value -- one summary row -- or [frames: [...]], one row
     *                  per frame, each with its own t, status, seconds, message
     *                  and extraCols.
     *
     * params.sources, when given, is the sources table's rows: a row whose
     * series_id has rows there is read from them (Luxendo), any other from
     * its file.
     */
    Map runEach(List<Map> rows, File imageRoot, Map params, File outdir,
                List<String> extraCols, Closure log = null, Closure work) {
        outdir.mkdirs()
        def blanks = extraCols.collectEntries { [(it): ""] }
        def say = { String m -> log?.call(m) }
        def warnings = []

        // An old sheet is named as one, before anything else is read from it.
        // Without this, every id reads blank and the duplicate check below
        // reports that the rows "share" one -- loud, but about the wrong thing.
        SCHEMA.requireId(rows, "The series table")

        def included = rows.findAll { isIncluded(it.include) }
        say("=== batch: " + included.size() + " of " + rows.size() + " row(s) included ===")

        // Make_SeriesSheet writes a sheet with duplicates on purpose, so they can
        // be seen and fixed. Here they are fatal: the series_id names the output
        // files, so two rows sharing one would overwrite each other and the R
        // side would merge them into a single series. Only INCLUDED rows matter
        // -- an excluded duplicate writes nothing.
        def dupId = included.groupBy { (it.series_id ?: "").toString() }
                            .findAll { k, v -> v.size() > 1 }
        if (dupId) {
            throw new IllegalArgumentException(
                "These included rows share a series_id, and would overwrite each other:\n    " +
                dupId.collect { k, v ->
                    k + " <- " + v.collect { it.path + "[" + it.series_index + "]" }.join(" AND ")
                }.join("\n    ") +
                "\n  Fix the series_id column, or set include=false on all but one.")
        }

        // Check the threshold request ONCE, here, before a single image opens.
        // buildMask checks it too, at the point of use -- but by then a row has
        // opened a stack and blurred it, and on a tile scan that cost is paid
        // per row. "Manual with no range" typed into a config should cost one
        // error message, not a thousand identical ones.
        if (params.containsKey("nucleus_threshold")) {
            RD.validateThreshold(
                (params.nucleus_threshold ?: "") as String,
                (params.nucleus_threshold_range ?: "") as String)
        }

        // Whether a mixed-pixel-size batch MATTERS depends on what the rows do,
        // so the detection is here and the advice comes from the caller. An
        // overview batch spans pixel sizes happily -- warning it about a blur
        // sigma it never uses would be noise, and noise is what stops warnings
        // being read.
        def sizes = pixelSizes(included)
        if (sizes.size() > 1) {
            def w = "This batch spans " + sizes.size() + " different pixel sizes (" +
                    sizes.keySet().sort().join(", ") + "). " +
                    (params.pixel_size_note ?: PIXEL_SIZE_NOTE_GENERIC)
            warnings << w
            say("WARNING: " + w)
        }

        // The REQUEST ("auto"), not the method used: resolveMethod() turns it
        // into one of importer/reader per file, and that is what gets recorded.
        def openMode = (params.open_mode ?: "auto").toString()

        // Joined and checked before any row runs: two tables that disagree are
        // one error, not one per row.
        def sources = loadSources((params.sources ?: []) as List<Map>, rows)
        if (sources) {
            def n = included.count { sources.containsKey((it.series_id ?: "").toString()) }
            say("  " + n + " included row(s) read from the sources table")
        }

        def summary = []
        int ok = 0, failed = 0
        try {
        rows.each { row ->
            def seriesId = (row.series_id ?: "").toString()
            if (!isIncluded(row.include)) {
                summary << ([series_id: seriesId, t: "", path: row.path, series_index: row.series_index,
                             status: "excluded", open_method: ""] + blanks +
                            [seconds: "", message: ""])
                return
            }
            long t0 = System.currentTimeMillis()
            def src = null
            // Outside the try: a failure before the image opens still has to say
            // what was attempted.
            def method = ""
            try {
                if (!seriesId) {
                    throw new IllegalArgumentException("row has no series_id; nothing to name its output")
                }
                // A hand-edited id names files too. Refused per row, before the
                // image opens, rather than rewritten into a name the sheet does
                // not hold -- see RoiExport.checkSeriesId.
                RX.checkSeriesId(seriesId)
                int si = (row.series_index ?: "0").toString() as Integer
                src = openSource(row, imageRoot, openMode, sources)
                method = src.method
                say("--- " + seriesId + "  (" + new File(row.path.toString()).getName() + " series " + si +
                    ", " + method + (src.nFrames > 1 ? (", " + src.nFrames + " frames") : "") + ")")

                def mism = checkDimensions(row, src)
                if (mism) {
                    def w = seriesId + ": the sheet does not match the image (" + mism.join("; ") +
                            "). Regenerate the sheet -- it was made from a different version of this file."
                    warnings << w
                    say("WARNING: " + w)
                }

                // What this row DOES is the caller's; everything around it --
                // include, duplicate series_ids, opening, closing, the summary
                // row, one row's failure not costing the rest -- is the same
                // for any batch and is not worth a second copy.
                def extra = work.call(src, seriesId, si, method, row) ?: [:]
                if (extra.frames instanceof List) {
                    // The unit of work is the FRAME: one row each, so a frame
                    // that failed is visible beside the ones that did not.
                    extra.frames.each { Map fr ->
                        summary << ([series_id: seriesId, t: fr.t, path: row.path, series_index: row.series_index,
                                     status: fr.status, open_method: method] +
                                    extraCols.collectEntries { [(it): (fr[it] == null ? "" : fr[it])] } +
                                    [seconds: fr.seconds ?: "", message: oneLine(fr.message as String)])
                    }
                } else {
                    summary << ([series_id: seriesId, t: (extra.t == null ? "" : extra.t), path: row.path,
                                 series_index: row.series_index, status: "ok", open_method: method] +
                                extraCols.collectEntries { [(it): (extra[it] == null ? "" : extra[it])] } +
                                [seconds: fmtSeconds(System.currentTimeMillis() - t0), message: ""])
                }
                ok++
            } catch (Throwable t) {
                // One bad image must not cost the other hundred and ninety-nine.
                def msg = t.getClass().getSimpleName() + ": " + (t.getMessage() ?: "(no message)")
                say("FAILED " + seriesId + ": " + msg)
                summary << ([series_id: seriesId, t: "", path: row.path, series_index: row.series_index,
                             status: "failed", open_method: method] + blanks +
                            [seconds: fmtSeconds(System.currentTimeMillis() - t0),
                             message: oneLine(msg)])
                failed++
            } finally {
                // Always, on both paths: a stack held here is gigabytes.
                if (src != null) {
                    try { src.dispose() } catch (ignored) { }
                }
            }
        }
        } finally {
            // The reader path holds a file handle open across the whole batch.
            closeReader()
        }

        // `t` beside series_id: the two are the key of a row once a series has
        // frames. Blank where no frame was reached (excluded, failed to open).
        def cols = ["series_id", "t", "path", "series_index", "status", "open_method"] +
                   extraCols + ["seconds", "message"]
        TSV.write(summary, new File(outdir, "batch_summary.tsv"), cols)

        // A series whose frames partly failed is an ok ROW -- its files hold
        // the frames that worked -- so the failed frames are counted apart.
        int framesFailed = summary.count { it.status == "failed" && it.t != "" }
        say("Done: " + ok + " ok, " + failed + " failed, " +
            (rows.size() - included.size()) + " excluded" +
            (framesFailed ? (", " + framesFailed + " frame(s) failed") : ""))
        return [summary: summary, ok: ok, failed: failed, frames_failed: framesFailed,
                excluded: rows.size() - included.size(), warnings: warnings]
    }

    /** A message has to survive being a TSV cell. */
    static String oneLine(String s) {
        return (s ?: "").replaceAll(/[\t\r\n]+/, " ").trim()
    }

    static String fmtSeconds(long ms) {
        return String.format("%.1f", ms / 1000.0d)
    }
}
