// LuxendoScan.groovy
//
// A Luxendo acquisition directory -> the TWO tables the pipeline runs on.
//
//   the series table    one row per series, `samples`-shaped, carrying `include`
//                       and `prefix` and room for the operator's metadata
//   the sources table   one row per `.lux.h5`, keyed to `series_id`
//
// WHY TWO TABLES
//   Every format this repo read before Luxendo put one or more SERIES INSIDE
//   ONE FILE. Luxendo puts ONE SERIES ACROSS SEVERAL FILES -- one per channel,
//   one per timepoint. A series row cannot absorb that, because it would need
//   more than one `path`. So the file facts live in a second table, which is
//   not a new pattern: files.tsv -> samples.tsv is already a file table and a
//   series table, and this is the same pair with the cardinality reversed.
//
//   `include` lives on the SERIES table, one value per series. Carried per
//   source it made "rows of this output disagree on include" a representable
//   state, which confused a real user; on the series table it cannot be
//   written down.
//
// IDENTITY COMES FROM CONTENT, NOT FROM THE PATH. The directory names do carry
// the stack, channel and position ("stack_0-L26A pos1_channel_1-GFP_obj_bottom")
// and parsing them was tried -- it agrees with the metadata on 404 of 404 files.
// It is still not used, because the path cannot supply size_x/size_y/size_z or
// the voxel size, and those are what the assembler needs; a path-derived table
// is blank in exactly the columns that do the work. See the plan 6.16.
//
// The content read is the `.json` sidecar rather than the HDF5 -- see
// LuxendoSidecar for the measurements, and for why pairing a `.json` with its
// `.lux.h5` replaces the old open-and-look-for-/Data test.
//
// ONE NARROW EXCEPTION, AND IT IS CHECKED. Under `quickScan` the TIME POINT of
// the files that were not sampled comes from the filename suffix, because the
// time point is the axis a channel directory runs along and so is the one fact
// the sampled sidecar cannot supply. It is used only after the mapping has been
// confirmed against the sampled file's real `time_point`; a directory where the
// name and the sidecar disagree is read in full. Turn `quickScan` off and no
// path is consulted at all.

class LuxendoScan {

    /** Padding is fixed, as it is for the sample prefix, and for the same reason. */
    static final String POSITION_FORMAT = "s%04d"
    static final String TIME_FORMAT     = "t%04d"

    static final String PIXEL_TYPE = "uint16"
    static final String PIXEL_UNIT = "micron"

    Object SC           // LuxendoSidecar class
    Object RX           // RoiExport class, for sanitize()
    String libDir

    static LuxendoScan load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader()
        def s = new LuxendoScan()
        s.libDir = libDir
        s.SC = gcl.parseClass(new File(dir, "LuxendoSidecar.groovy"))
        s.RX = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        return s
    }

    /**
     * Every PAIRED `.lux.h5` under `dir`, recursively, sorted so a run is
     * reproducible.
     *
     * Paired means the `.lux.h5` and its `.json` sidecar are both present. That
     * is the whole index-file test: `main_raw.lux.h5` has no sidecar, and a
     * stray `.json` left behind by somebody else's analysis has no image.
     */
    List<File> findPairs(File dir, Closure log = null) {
        return findPairs(dir, log, null)
    }

    /**
     * @param sizes if given, filled in as path -> bytes from the SAME walk.
     *              On a network mount a stat is ~20 ms, so the file sizes are
     *              taken once here rather than again per row: measured on an
     *              800 GB samba acquisition, the walk and its stats dominate
     *              the scan, not the sidecar reads.
     */
    List<File> findPairs(File dir, Closure log, Map<String, Long> sizes) {
        if (!dir.isDirectory()) {
            throw new IllegalArgumentException("Not a directory: " + dir.getAbsolutePath())
        }
        // ONE WALK, AND PAIRING BY SET MEMBERSHIP. Asking isFile() per sidecar
        // would be a second stat for every image -- 4032 of them on the real
        // acquisition -- when the walk has already seen both halves of each
        // pair. The walk is the expensive part, so it happens once and
        // everything else is answered from memory.
        def h5 = [], jsonPaths = new HashSet()
        dir.eachFileRecurse { File f ->
            if (!f.isFile() || f.getName().startsWith("._")) return   // macOS AppleDouble
            def n = f.getName().toLowerCase()
            if (n.endsWith(SC.SUFFIX_H5)) {
                h5 << f
                if (sizes != null) sizes[f.getAbsolutePath()] = f.length()
            } else if (n.endsWith(SC.SUFFIX_JSON)) {
                jsonPaths << f.getAbsolutePath()
            }
        }
        // Set difference both ways, not a nested scan: with 4032 of each, a
        // findAll inside an any would be 16 million closure calls.
        def wantedJson = new HashSet()
        def paired = [], lonelyH5 = []
        h5.each { File f ->
            def side = SC.sidecarFor(f).getAbsolutePath()
            wantedJson << side
            if (jsonPaths.contains(side)) { paired << f } else { lonelyH5 << f }
        }
        def lonelyJson = (jsonPaths - wantedJson).toList()

        paired = paired.sort { it.getAbsolutePath() }
        log?.call("  " + paired.size() + " paired .lux.h5 + .json file(s)")
        // REPORTED, never silently dropped: a file that should have been part of
        // the acquisition and is not is exactly what a person needs to hear.
        lonelyH5.sort { it.getAbsolutePath() }.each {
            log?.call("  skipped (no .json sidecar): " + relative(dir, it))
        }
        if (lonelyJson) {
            log?.call("  ignored " + lonelyJson.size() + " .json with no .lux.h5 beside it" +
                      (lonelyJson.size() <= 3
                       ? (": " + lonelyJson.collect { relative(dir, new File(it)) }.sort().join(", ")) : ""))
        }
        if (!paired) {
            throw new IllegalArgumentException(
                "No paired .lux.h5 + .json under " + dir.getAbsolutePath())
        }
        return paired
    }

    /** Path of `f` relative to `root`, or the absolute path when it is not below it. */
    static String relative(File root, File f) {
        def rp = root.getAbsolutePath()
        def fp = f.getAbsolutePath()
        return fp.startsWith(rp + File.separator) ? fp.substring(rp.length() + 1) : fp
    }

    /**
     * Scan an acquisition into the two tables.
     *
     * @param dir   the acquisition directory (the one holding raw/)
     * @param opts  gatherFrames -- every timepoint of a position in one series
     *              quickScan    -- one sidecar per DIRECTORY, not per file (default true)
     * @param log   called with progress lines
     * @return [series: List&lt;Map&gt;, sources: List&lt;Map&gt;, warnings: List&lt;String&gt;]
     */
    Map scan(File dir, Closure log = null) { return scan(dir, [:], log) }

    Map scan(File dir, Map opts, Closure log = null) {
        boolean gatherFrames = (opts?.gatherFrames ?: false) as boolean
        boolean quick = (opts != null && opts.containsKey("quickScan")) ? (opts.quickScan as boolean) : true

        def sizes = new HashMap<String, Long>()
        def files = findPairs(dir, log, sizes)
        def alias = RX.sanitize(dir.getName())

        def info = quick ? readSampled(dir, files, sizes, log) : readEvery(dir, files, log)

        def sources = []
        files.each { File f ->
            def sc = info[f.getAbsolutePath()]
            def posId = RX.sanitize(String.format(POSITION_FORMAT, sc.stack) +
                                    "_" + (sc.stackDescription ?: ""))
            // THE TABLE DECIDES THE GROUPING, not the assembler: gathering is
            // settled here so the series table says what will be produced.
            def serId = gatherFrames ? posId
                                     : RX.sanitize(posId + "_" + String.format(TIME_FORMAT, sc.timePoint))
            sources << [
                source_path  : relative(dir, f),
                series_id    : serId,
                channel      : sc.channel,
                channel_name : (sc.channelDescription ?: ""),
                t            : sc.timePoint,
                size_x       : sc.sizeX,
                size_y       : sc.sizeY,
                size_z       : sc.sizeZ,
                pixel_width  : sc.pixelWidth,
                pixel_height : sc.pixelHeight,
                // null, not 1.0, for a single plane -- see LuxendoSidecar
                pixel_depth  : sc.pixelDepth,
                pixel_unit   : (sc.pixelWidth != null ? PIXEL_UNIT : null),
                source_bytes : sizes[f.getAbsolutePath()],
                // Not columns. Carried for building the series table and the
                // per-position checks, then removed before the table is handed
                // back -- the schema is the contract, and these are workings.
                _position_id : posId,
                _stack       : sc.stack,
                _stack_desc  : (sc.stackDescription ?: ""),
            ]
        }
        sources = sortSources(sources)
        def series = buildSeries(sources, dir, alias)
        def warnings = validate(series, sources, log)
        sources.each { r -> r.remove("_position_id"); r.remove("_stack"); r.remove("_stack_desc") }
        return [series: series, sources: sources, warnings: warnings]
    }

    /**
     * One sidecar per file. Correct whatever the tree looks like, and the slow
     * path: 88 ms per file over a network mount, so ~6 minutes for 4032 files.
     */
    private Map readEvery(File dir, List<File> files, Closure log) {
        log?.call("  reading every sidecar (quickScan off)")
        def m = [:]
        files.each { m[it.getAbsolutePath()] = SC.read(SC.sidecarFor(it)) }
        return m
    }

    /**
     * One sidecar per DIRECTORY, plus the file size of every file.
     *
     * A Luxendo channel directory holds one position and one channel across
     * every timepoint, so the dimensions and voxel size are constant within it
     * -- the same assumption the z-uniformity check already makes. 42 reads
     * instead of 4032: ~3 s against ~6 min.
     *
     * SAMPLED, SO IT IS CHECKED. `File.length()` is cheap and a z-plane is
     * millions of bytes, so a timepoint of a different depth cannot hide:
     * measured on the real acquisition, sizes within one directory vary by 8
     * bytes while one plane of 2048x2048x16-bit is 8,388,608. Any file more
     * than half a plane from the sampled one gets read in full and the
     * difference is logged, so a truncated acquisition is caught rather than
     * assumed away.
     */
    private Map readSampled(File dir, List<File> files, Map<String, Long> sizes, Closure log) {
        def byDir = files.groupBy { it.getParentFile().getAbsolutePath() }
        log?.call("  quickScan: 1 sidecar per directory, across " + byDir.size() + " directory(ies)")
        def m = [:]
        int extra = 0, fellBack = 0
        byDir.keySet().sort().each { String d ->
            def sorted = byDir[d].sort { it.getName() }
            def first  = sorted[0]
            def sc     = SC.read(SC.sidecarFor(first))

            // ⚠️ WHAT IS AND IS NOT CONSTANT IN A CHANNEL DIRECTORY. The stack,
            // the channel, the dimensions and the voxel size are; the TIME
            // POINT is the axis the directory runs along, so reusing the
            // sampled sidecar's time_point would give every file t=0 and the
            // duplicate-source check would (correctly) reject the whole scan.
            //
            // So the time point comes from the filename -- the one place in
            // this repo where identity touches a path -- and only after the
            // mapping has been CONFIRMED on the sampled file, whose real
            // time_point we have just read. If the sampled file disagrees with
            // its own name, this directory is read in full instead.
            def firstTp = SC.timePointFromName(first)
            if (firstTp == null || firstTp != sc.timePoint) {
                fellBack++
                log?.call("    filename does not encode the time point (" + first.getName() +
                          " says " + firstTp + ", its sidecar says " + sc.timePoint +
                          "); reading every sidecar in " + relative(dir, new File(d)))
                sorted.each { m[it.getAbsolutePath()] = SC.read(SC.sidecarFor(it)) }
                return
            }

            long plane = 2L * sc.sizeX * sc.sizeY
            long ref   = sizes[first.getAbsolutePath()]
            sorted.each { File f ->
                if (f.getAbsolutePath() == first.getAbsolutePath()) {
                    m[f.getAbsolutePath()] = sc
                    return
                }
                def tp = SC.timePointFromName(f)
                // SAMPLED, SO IT IS CHECKED. File.length() is cheap and a
                // z-plane is millions of bytes, so a timepoint of a different
                // depth cannot hide: measured on the real acquisition, sizes
                // within one directory vary by 8 bytes while one plane of
                // 2048x2048x16-bit is 8,388,608. Anything more than half a
                // plane away is read in full and the difference logged, so a
                // truncated acquisition is caught rather than assumed away.
                long len = sizes[f.getAbsolutePath()]
                if (tp == null || Math.abs(len - ref) * 2L > plane) {
                    m[f.getAbsolutePath()] = SC.read(SC.sidecarFor(f))
                    extra++
                    if (tp != null) {
                        log?.call("    size differs from its siblings, read in full: " +
                                  relative(dir, f) + " (" + len + " vs " + ref + " bytes)")
                    }
                } else {
                    m[f.getAbsolutePath()] = sc.atTimePoint(tp)
                }
            }
        }
        if (extra > 0)    log?.call("  " + extra + " file(s) read in full because their size stood out")
        if (fellBack > 0) log?.call("  " + fellBack + " directory(ies) read in full")
        return m
    }

    /**
     * The series table: one row per `series_id`, `samples`-shaped.
     *
     * LOCKED: four of `samples`' required columns presume "a series addressed
     * inside one file". Rather than relax them for every other format, or
     * rename the sheet and break cli_helpers.r (which reads machine columns
     * with sheet == "samples"), they are given meanings that are TRUE here:
     *
     *   path          the acquisition directory -- the root source_path resolves against
     *   series_index  the `stack` number from the metadata
     *   series_name   stack_description, before sanitising
     *   alias         the acquisition folder name
     *
     * So `prefix` still builds by the repo's existing rule and stays unique by
     * construction, and no R CLI changes.
     */
    private List<Map> buildSeries(List<Map> sources, File dir, String alias) {
        def out = []
        sources.groupBy { it.series_id }.each { String serId, List<Map> rows ->
            def first = rows[0]
            def chans = rows.collect { it.channel as int }.unique().sort()
            def ts    = rows.collect { it.t as int }.unique().sort()
            out << [
                prefix      : serId,
                include     : "true",
                alias       : alias,
                series_index: first._stack,
                series_name : first._stack_desc,
                path        : dir.getAbsolutePath(),
                size_x      : first.size_x,
                size_y      : first.size_y,
                size_z      : first.size_z,
                size_c      : chans.size(),
                size_t      : ts.size(),
                pixel_type  : PIXEL_TYPE,
                pixel_width : first.pixel_width,
                pixel_height: first.pixel_height,
                pixel_depth : first.pixel_depth,
                pixel_unit  : first.pixel_unit,
                file_size   : rows.sum { it.source_bytes as long },
            ]
        }
        return out.sort(false) { a, b -> (a.prefix <=> b.prefix) }
    }

    /**
     * The checks that must happen before a long run.
     *
     * Severity differs on purpose, following the sample sheet: something that
     * makes the tables unusable stops; something that is merely the signature
     * of a mistake is reported loudly and left for a person to judge.
     */
    List<String> validate(List<Map> series, List<Map> sources, Closure log = null) {
        def warnings = []

        // Two sources claiming the same channel of the same series FRAME would
        // overwrite each other in the assembled stack, silently. Keyed on the
        // timepoint too, because under gatherFrames one series legitimately
        // holds channel 0 once per frame.
        def dupes = sources.groupBy { [it.series_id, it.t, it.channel] }.findAll { k, v -> v.size() > 1 }
        if (dupes) {
            throw new IllegalArgumentException(
                "Two sources claim the same series channel:\n  " +
                dupes.collect { k, v ->
                    k[0] + " t=" + k[1] + " channel " + k[2] + " <- " +
                    v.collect { it.source_path }.join(" AND ")
                }.join("\n  "))
        }

        // Within ONE series every source must agree on dimensions, or they
        // cannot go into one stack at all. Under gatherFrames this is also
        // where z changing between timepoints becomes FATAL rather than a
        // warning, and rightly so: there is then no single volume shape to
        // build the hyperstack from.
        sources.groupBy { it.series_id }.each { String ser, List<Map> v ->
            def shapes = v.collect { [it.size_x, it.size_y, it.size_z] }.unique()
            if (shapes.size() > 1) {
                throw new IllegalArgumentException(
                    ser + ": its sources disagree on dimensions: " +
                    v.collect { "t" + it.t + "/ch" + it.channel + "=" +
                                it.size_x + "x" + it.size_y + "x" + it.size_z }.unique().join(", "))
            }
        }

        // The two tables are joined on series_id, and nothing may be orphaned on
        // either side. A join that matches nothing is the failure this repo is
        // built around, so it is checked where the tables are made rather than
        // discovered later by whatever consumes them.
        def serIds = series.collect { it.prefix }.toSet()
        def srcIds = sources.collect { it.series_id }.toSet()
        def orphanSources = srcIds - serIds
        def emptySeries   = serIds - srcIds
        if (orphanSources || emptySeries) {
            throw new IllegalStateException(
                "The two tables do not agree on series_id" +
                (orphanSources ? ("\n  sources with no series row: " + orphanSources.sort().join(", ")) : "") +
                (emptySeries   ? ("\n  series with no sources: "    + emptySeries.sort().join(", ")) : ""))
        }

        // Every FRAME must have the same channel set, or one assembled stack has
        // channel 2 where another has channel 3 and nothing says so. Per frame
        // rather than per series, so a gathered position missing one channel at
        // one timepoint is still caught.
        def channelSets = sources.groupBy { [it.series_id, it.t] }
                                 .collectEntries { k, v -> [k, v.collect { it.channel }.sort()] }
        def expected = channelSets.values().first()
        def odd = channelSets.findAll { k, v -> v != expected }
        if (odd) {
            warnings << ("Not every frame has the same channels; expected " + expected + " -- " +
                         odd.collect { k, v -> k[0] + " t=" + k[1] + " has " + v }.join(", "))
        }

        // LOCKED: z uniform across the time points of ONE position. It is NOT
        // uniform across positions -- the real dataset runs 16..39, with one at
        // z=1 -- so this is deliberately per position and not global.
        def byPosition = sources.groupBy { it._position_id ?: it.series_id }
        byPosition.each { String pos, List<Map> v ->
            def zs = v.collect { it.size_z }.unique()
            if (zs.size() > 1) {
                warnings << ("Position " + pos + " changes z between time points: " + zs.sort() +
                             " -- the time course is not a single volume")
            }
        }

        // Time points should be a run from 0. A gap means a file is missing,
        // which is worth knowing BEFORE the tracking step reports a broken track.
        byPosition.each { String pos, List<Map> v ->
            def ts = v.collect { it.t }.unique().sort()
            if (ts != (0..<ts.size()).toList()) {
                warnings << ("Position " + pos + " has time points " + ts + ", not a run from 0")
            }
        }

        warnings.each { log?.call("WARNING: " + it) }
        return warnings
    }

    /**
     * Sources order: series, then time, then channel.
     *
     * AN EXPLICIT TWO-ARGUMENT COMPARATOR, NOT `sort { [a, b, c] }`.
     * A one-argument closure is used by Groovy as a key extractor, and the keys
     * are then compared with `<=>` -- which for two Lists THROWS
     * ("Cannot compare java.util.ArrayList with value '[a, 1]' ..."). Inside
     * the sort that exception is swallowed and the list comes back in an
     * ARBITRARY order, with nothing reported.
     *
     * Observed here: the table for the real acquisition came out beginning
     * s0011, s0010, s0004, s0003 -- which looks like an order until you check
     * it. A table meant to be read and diffed must not have an unstable order.
     * Test_LuxendoScan pins it.
     */
    static List<Map> sortSources(List<Map> rows) {
        return rows.sort(false) { a, b ->
            (a.series_id <=> b.series_id) ?:
            ((a.t as int) <=> (b.t as int)) ?:
            ((a.channel as int) <=> (b.channel as int))
        }
    }

    /** Sources grouped by the series they feed, in table order. */
    static Map<String, List<Map>> bySeries(List<Map> rows) {
        def m = new LinkedHashMap()
        sortSources(rows).each { r -> m.computeIfAbsent(r.series_id, { [] }) << r }
        return m
    }
}
