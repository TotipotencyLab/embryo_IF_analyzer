// BatchRunner.groovy
//
// Run the nucleus pipeline over every included row of a sample sheet.
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

class BatchRunner {

    String libDir
    Class TSV, NP, RC
    Object pipeline

    static BatchRunner load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader(BatchRunner.class.classLoader)
        def b = new BatchRunner()
        b.libDir = dir.getAbsolutePath()
        b.TSV = gcl.parseClass(new File(dir, "Tsv.groovy"))
        b.NP = gcl.parseClass(new File(dir, "NucleusPipeline.groovy"))
        b.RC = gcl.parseClass(new File(dir, "RunConfig.groovy"))
        b.pipeline = b.NP.load(b.libDir)
        return b
    }

    /** include is a real boolean, never "a non-empty string is true". */
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

    /**
     * Open one series, pixels and all.
     *
     * setZEnd takes a 0-BASED INDEX, not a count: passing the slice count on a
     * single-plane series throws "Invalid Z index: 1/1". Not used here -- the
     * whole stack is wanted -- but the reason this opens by series and not by
     * range is that the trap is one line away.
     */
    Object openSeries(File image, int seriesIndex) {
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
        cmp("size_x", imp.getWidth())
        cmp("size_y", imp.getHeight())
        cmp("size_z", imp.getNSlices())
        cmp("size_c", imp.getNChannels())
        return out
    }

    /**
     * The pixel sizes present among the rows about to run.
     *
     * @return map of pixel_width -> list of prefixes
     */
    static Map pixelSizes(List<Map> rows) {
        return rows.findAll { (it.pixel_width ?: "").toString() }
                   .groupBy { it.pixel_width.toString() }
                   .collectEntries { k, v -> [(k): v.collect { it.prefix }] }
    }

    /**
     * Run the batch.
     *
     * @param rows      sample sheet rows (all of them; include is applied here)
     * @param imageRoot base for relative paths, or null
     * @param params    NucleusPipeline parameters, already merged over defaults
     * @param outdir    where results and batch_summary.tsv go
     * @param log       progress closure
     * @return [summary: rows, ok: n, failed: n, excluded: n, warnings: []]
     */
    Map run(List<Map> rows, File imageRoot, Map params, File outdir, Closure log = null) {
        outdir.mkdirs()
        def say = { String m -> log?.call(m) }
        def warnings = []

        def included = rows.findAll { isIncluded(it.include) }
        say("=== batch: " + included.size() + " of " + rows.size() + " row(s) included ===")

        def sizes = pixelSizes(included)
        if (sizes.size() > 1) {
            def w = "This batch spans " + sizes.size() + " different pixel sizes (" +
                    sizes.keySet().sort().join(", ") + "). nucleus_particle_size is in " +
                    "calibrated units and transfers; nucleus_blur_sigma is in PIXELS and does " +
                    "NOT -- the same sigma is a different physical blur on each. Check the " +
                    "result on one series from each group before trusting the rest."
            warnings << w
            say("WARNING: " + w)
        }

        def summary = []
        int ok = 0, failed = 0
        rows.each { row ->
            def prefix = (row.prefix ?: "").toString()
            if (!isIncluded(row.include)) {
                summary << [prefix: prefix, path: row.path, series_index: row.series_index,
                            status: "excluded", n_nucleus: "", n_nucleolus: "",
                            seconds: "", message: ""]
                return
            }
            long t0 = System.currentTimeMillis()
            def imp = null
            try {
                if (!prefix) {
                    throw new IllegalArgumentException("row has no prefix; nothing to name its output")
                }
                def image = resolve(row.path.toString(), imageRoot)
                if (!image.isFile()) {
                    throw new IllegalArgumentException("no such image file: " + image.getAbsolutePath())
                }
                int si = (row.series_index ?: "0").toString() as Integer
                say("--- " + prefix + "  (" + image.getName() + " series " + si + ")")
                imp = openSeries(image, si)

                def mism = checkDimensions(row, imp)
                if (mism) {
                    def w = prefix + ": the sheet does not match the image (" + mism.join("; ") +
                            "). Regenerate the sheet -- it was made from a different version of this file."
                    warnings << w
                    say("WARNING: " + w)
                }

                // The sheet's prefix is authoritative: resolveImageId() would dig
                // "Series001" out of the slice label, which recurs in every file.
                def res = pipeline.run(imp, outdir, params + [basename: prefix])
                summary << [prefix: prefix, path: row.path, series_index: row.series_index,
                            status: "ok",
                            n_nucleus: res.nucRois.size(), n_nucleolus: res.nuclRois.size(),
                            seconds: fmtSeconds(System.currentTimeMillis() - t0), message: ""]
                ok++
            } catch (Throwable t) {
                // One bad image must not cost the other hundred and ninety-nine.
                def msg = t.getClass().getSimpleName() + ": " + (t.getMessage() ?: "(no message)")
                say("FAILED " + prefix + ": " + msg)
                summary << [prefix: prefix, path: row.path, series_index: row.series_index,
                            status: "failed", n_nucleus: "", n_nucleolus: "",
                            seconds: fmtSeconds(System.currentTimeMillis() - t0),
                            message: oneLine(msg)]
                failed++
            } finally {
                // Always, on both paths: a stack held here is gigabytes.
                if (imp != null) {
                    try { imp.changes = false; imp.close(); imp.flush() } catch (ignored) { }
                }
            }
        }

        def cols = ["prefix", "path", "series_index", "status", "n_nucleus", "n_nucleolus",
                    "seconds", "message"]
        TSV.write(summary, new File(outdir, "batch_summary.tsv"), cols)

        // The parameters actually used, as a file that can be fed straight back
        // in -- provenance for the batch as a whole, beside the per-image config.
        RC.writeParams(params.findAll { k, v -> NP.PARAM_TYPES.containsKey(k) },
                       new File(outdir, "batch_params.txt"))

        say("Done: " + ok + " ok, " + failed + " failed, " +
            (rows.size() - included.size()) + " excluded")
        return [summary: summary, ok: ok, failed: failed,
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
