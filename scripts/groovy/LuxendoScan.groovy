// LuxendoScan.groovy
//
// A Luxendo acquisition directory -> the assembly manifest.
//
// IDENTITY COMES FROM EACH FILE'S OWN METADATA, NOT FROM ITS PATH. The
// directory names do carry the stack, channel and position
// ("stack_0-L26A pos1_channel_1-GFP_obj_bottom"), and parsing them would be
// shorter. It would also be the mistake this repo has already made once: the
// filename parse in the R CLI used a greedy prefix and mis-split any feature
// name containing "_", silently. A directory name is not an anchored,
// fixed-shape string, and `stack_description` is free text a user typed. So
// every fact here is read from the `/metadata` JSON inside the file, and the
// path is used only to find the files.
//
// The manifest is RECTANGULAR: one row per SOURCE file, carrying the output it
// feeds. Rows sharing a `target_output_path` are the channels of one output, and
// together they "name every source file" the plan asks for -- a single row with
// a variable-length list of sources would not survive being a TSV.
//
// Columns are declared in schema/sheet_columns.tsv under sheet `manifest`.

class LuxendoScan {

    /** Padding is fixed, as it is for the sample prefix, and for the same reason. */
    static final String POSITION_FORMAT = "s%04d"
    static final String TIME_FORMAT     = "t%04d"

    Object LFC          // LuxendoFile class
    Object RX           // RoiExport class, for sanitize()
    String libDir

    static LuxendoScan load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader()
        def s = new LuxendoScan()
        s.libDir = libDir
        s.LFC = gcl.parseClass(new File(dir, "LuxendoFile.groovy"))
        s.RX  = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        return s
    }

    /** Every *.lux.h5 under `dir`, recursively, sorted so a run is reproducible. */
    static List<File> findLuxFiles(File dir) {
        if (!dir.isDirectory()) {
            throw new IllegalArgumentException("Not a directory: " + dir.getAbsolutePath())
        }
        def found = []
        dir.eachFileRecurse { File f ->
            // macOS AppleDouble sidecars (._name) are not images and are not
            // hidden from listFiles(); reading one as HDF5 throws.
            if (f.isFile() && f.getName().toLowerCase().endsWith(".lux.h5") &&
                !f.getName().startsWith("._")) {
                found << f
            }
        }
        return found.sort { it.getAbsolutePath() }
    }

    /** Path of `f` relative to `root`, or the absolute path when it is not below it. */
    static String relative(File root, File f) {
        def rp = root.getAbsolutePath()
        def fp = f.getAbsolutePath()
        return fp.startsWith(rp + File.separator) ? fp.substring(rp.length() + 1) : fp
    }

    /**
     * Read every file's header and build the manifest rows.
     *
     * One open per file, header only -- no pixels are touched.
     *
     * @param dir  the acquisition directory (the one holding raw/)
     * @param log  called with progress lines
     */
    List<Map> scan(File dir, Closure log = null) {
        def files = findLuxFiles(dir)
        if (!files) {
            throw new IllegalArgumentException(
                "No .lux.h5 files under " + dir.getAbsolutePath())
        }
        // A Luxendo tree holds main_raw.lux.h5 beside the real stacks: link-only,
        // no /Data. Told apart by content, not by name, and REPORTED rather than
        // quietly dropped -- a file that should have held pixels and does not is
        // exactly what a person needs to hear about.
        def index = files.findAll { !LFC.holdsPixels(it) }
        files = files - index
        log?.call("  " + files.size() + " .lux.h5 file(s) with pixels")
        index.each { log?.call("  skipped (no /Data, an index file): " + relative(dir, it)) }
        if (!files) {
            throw new IllegalArgumentException(
                "No .lux.h5 file under " + dir.getAbsolutePath() + " holds a /Data stack")
        }

        def rows = []
        files.each { File f ->
            def lf = LFC.open(f)
            try {
                def info = lf.info
                // A file with no /metadata cannot be placed: the whole point is
                // that position, channel and time come from the content.
                if (!info || info.stack == null || info.time_point == null || info.channel == null) {
                    throw new IllegalArgumentException(
                        relative(dir, f) + " has no usable /metadata " +
                        "(stack, channel and time_point are what place a file); refusing to guess from the path")
                }
                int stack = info.stack as int
                int tp    = info.time_point as int
                int ch    = info.channel as int
                def posId = RX.sanitize(String.format(POSITION_FORMAT, stack) +
                                        "_" + (info.stack_description ?: ""))
                def serId = RX.sanitize(posId + "_" + String.format(TIME_FORMAT, tp))

                rows << [
                    target_output_path: serId + ".tif",
                    series_id         : serId,
                    position_id       : posId,
                    t                 : tp,
                    channel           : ch,
                    channel_name      : (info.channel_description ?: ""),
                    source_path       : relative(dir, f),
                    source_bytes      : f.length(),
                    size_x            : lf.sizeX,
                    size_y            : lf.sizeY,
                    size_z            : lf.sizeZ,
                    pixel_width       : lf.pixelWidth,
                    pixel_height      : lf.pixelHeight,
                    // null, not 1.0, for a single plane -- see LuxendoFile
                    pixel_depth       : lf.pixelDepth,
                    pixel_unit        : (lf.pixelWidth != null ? "micron" : null),
                    stack_description : (info.stack_description ?: ""),
                    include           : "true",
                ]
            } finally {
                lf.close()
            }
        }
        rows = sortRows(rows)
        validate(rows, log)
        return rows
    }

    /**
     * The checks that must happen before 33 GB is written.
     *
     * Severity differs on purpose, following the sample sheet: something that
     * makes the manifest unbuildable stops; something that is merely the
     * signature of a mistake is reported loudly and left for a person to judge.
     */
    List<String> validate(List<Map> rows, Closure log = null) {
        def warnings = []

        // Two sources claiming the same channel of the same output would
        // overwrite each other in the assembled stack, silently.
        def byKey = rows.groupBy { [it.target_output_path, it.channel] }
        def dupes = byKey.findAll { k, v -> v.size() > 1 }
        if (dupes) {
            throw new IllegalArgumentException(
                "Two sources claim the same output channel:\n  " +
                dupes.collect { k, v ->
                    k[0] + " channel " + k[1] + " <- " + v.collect { it.source_path }.join(" AND ")
                }.join("\n  "))
        }

        def byOutput = rows.groupBy { it.target_output_path }

        // Every output must have the same channel set, or one assembled stack
        // has channel 2 where another has channel 3 and nothing says so.
        def channelSets = byOutput.collectEntries { k, v -> [k, v.collect { it.channel }.sort()] }
        def expected = channelSets.values().first()
        def odd = channelSets.findAll { k, v -> v != expected }
        if (odd) {
            warnings << ("Not every output has the same channels; expected " + expected + " -- " +
                         odd.collect { k, v -> k + " has " + v }.join(", "))
        }

        // Within ONE output the channels must agree on dimensions, or they
        // cannot go into one stack at all.
        byOutput.each { String out, List<Map> v ->
            def shapes = v.collect { [it.size_x, it.size_y, it.size_z] }.unique()
            if (shapes.size() > 1) {
                throw new IllegalArgumentException(
                    out + ": its channels disagree on dimensions: " +
                    v.collect { "ch" + it.channel + "=" + it.size_x + "x" + it.size_y + "x" + it.size_z }.join(", "))
            }
        }

        // LOCKED: z uniform across the time points of ONE position. It is NOT
        // uniform across positions -- the real dataset runs 16..39, with one at
        // z=1 -- so this is deliberately per position and not global.
        byOutput.values().groupBy { it[0].position_id }.each { String pos, List<List<Map>> outs ->
            def zs = outs.collect { it[0].size_z }.unique()
            if (zs.size() > 1) {
                warnings << ("Position " + pos + " changes z between time points: " + zs.sort() +
                             " -- the time course is not a single volume")
            }
        }

        // Time points should be a run from 0. A gap means a file is missing,
        // which is worth knowing BEFORE the tracking step reports a broken track.
        byOutput.values().groupBy { it[0].position_id }.each { String pos, List<List<Map>> outs ->
            def ts = outs.collect { it[0].t }.sort()
            if (ts != (0..<ts.size()).toList()) {
                warnings << ("Position " + pos + " has time points " + ts + ", not a run from 0")
            }
        }

        warnings.each { log?.call("WARNING: " + it) }
        return warnings
    }

    /**
     * Manifest order: position, then time, then channel.
     *
     * ⚠️ AN EXPLICIT TWO-ARGUMENT COMPARATOR, NOT `sort { [a, b, c] }`.
     * A one-argument closure is used by Groovy as a key extractor, and the keys
     * are then compared with `<=>` -- which for two Lists THROWS
     * ("Cannot compare java.util.ArrayList with value '[a, 1]' ..."). Inside
     * the sort that exception is swallowed and the list comes back in an
     * ARBITRARY order, with nothing reported.
     *
     * Observed here: the manifest for the real acquisition came out beginning
     * s0011, s0010, s0004, s0003 -- which looks like an order until you check
     * it. A manifest is meant to be read and diffed, so an unstable order is
     * not cosmetic. Test_LuxendoScan pins it.
     */
    static List<Map> sortRows(List<Map> rows) {
        return rows.sort(false) { a, b ->
            (a.position_id <=> b.position_id) ?: (a.t <=> b.t) ?: (a.channel <=> b.channel)
        }
    }

    /** Outputs in the manifest, each as its list of channel rows, in manifest order. */
    static Map<String, List<Map>> byOutput(List<Map> rows) {
        def m = new LinkedHashMap()
        sortRows(rows).each { r ->
            m.computeIfAbsent(r.target_output_path, { [] }) << r
        }
        return m
    }
}
