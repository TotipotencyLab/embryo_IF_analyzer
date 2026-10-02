// LuxendoScan.groovy
//
// A Luxendo acquisition directory -> the TWO tables the pipeline runs on.
//
//   the series table    one row per series -- one per STACK, every time point
//                       inside -- carrying `include` and `series_id` and room
//                       for the operator's metadata
//   the sources table   one row per `.lux.h5`, keyed to its series by
//                       (alias, series_index), NEVER by series_id
//
// WHY NOT BY series_id. You may edit series_id for readability; a join on it
// would orphan every source row the moment you did. alias and series_index are
// machine columns -- the acquisition and the stack -- so the join cannot be
// broken by an edit, and two acquisitions in one table cannot collide on it.
//
// WHY TWO TABLES
//   Every format this repo read before Luxendo put one or more SERIES INSIDE
//   ONE FILE. Luxendo puts ONE SERIES ACROSS SEVERAL FILES -- one per channel,
//   one per timepoint. A series row cannot absorb that, because it would need
//   more than one `path`. So the file facts live in a second table, which is
//   not a new pattern: files.tsv -> series.tsv is already a file table and a
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
//
// WHERE THE FILE LIST COMES FROM -- `listing`, and the scan records which.
//   index  `bdv.h5` + `bdv.xml`, which Luxendo writes beside raw/: the files
//          the index links, each one stat-ed for its size and to prove it is
//          there. No directory walk. LuxendoIndex has the measurements.
//   walk   every directory under the acquisition, pairing `.lux.h5` with
//          `.json` -- the v0.6.0 route, and the only one that can see a file
//          the index does not list. With an index present it also compares
//          the two and warns on any difference.
//   auto   (default) the index when both files are there, else the walk.
// Either way IDENTITY still comes from the sidecars, and on the index route
// every placed file is checked against the index -- time point, channel,
// stack, size, voxel size -- and a disagreement stops the scan.
//
// ONE SERIES PER STACK, ALWAYS. A Luxendo stack is one series in Bio-Formats'
// own sense -- x, y, z, c AND t -- which is how Bio-Formats itself presents
// bdv.xml. v0.6.0 split it per time point by default; that option
// (`gatherFrames`) is gone, and a time point is chosen at use, as
// Make_LuxendoTiff's `frames`.

class LuxendoScan {

    static final String PIXEL_TYPE = "uint16"
    static final String PIXEL_UNIT = "micron"

    static final String LISTING_AUTO  = "auto"
    static final String LISTING_INDEX = "index"
    static final String LISTING_WALK  = "walk"
    static final List<String> LISTINGS = [LISTING_AUTO, LISTING_INDEX, LISTING_WALK]

    /**
     * Validated in code: a `#@ String` with choices={} is not validated on the
     * command line, so a typo must not quietly mean "auto".
     */
    static String checkListing(Object v) {
        def s = (v == null) ? "" : v.toString().trim().toLowerCase()
        if (!s) return LISTING_AUTO
        if (!LISTINGS.contains(s)) {
            throw new IllegalArgumentException("listing must be one of " + LISTINGS + ", not '" + v + "'")
        }
        return s
    }

    Object SC           // LuxendoSidecar class
    Object IX           // LuxendoIndex class
    Object RX           // RoiExport class, for sanitize()
    Object SS           // SeriesSheet instance, for composeSeriesId()
    String libDir

    static LuxendoScan load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader()
        def s = new LuxendoScan()
        s.libDir = libDir
        s.SC = gcl.parseClass(new File(dir, "LuxendoSidecar.groovy"))
        s.IX = gcl.parseClass(new File(dir, "LuxendoIndex.groovy"))
        s.RX = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        // THE PREFIX RULE LIVES IN ONE PLACE. composeSeriesId() is the repo's
        // own `sanitise(<alias>_s<NNNN>_<series_name>)`, and Luxendo follows it
        // rather than reimplementing it -- so a change to the rule lands on
        // both paths at once.
        s.SS = gcl.parseClass(new File(dir, "SeriesSheet.groovy")).load(libDir)
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
     * The files the index links, in the order findPairs() would give them.
     *
     * Every one is stat-ed: that is what gives `source_bytes`, what proves the
     * file is on disk, and what feeds quickScan's size check. It is also nearly
     * all of what the index route costs -- 51-89 s of ~1 minute on the 4032-file
     * acquisition over samba. Skipping it would make the scan seconds long and
     * lose all three; that trade is recorded in the plan, not taken.
     *
     * `length()` first and `isFile()` only when it says 0, so a present file
     * costs one round trip rather than two.
     */
    List<File> filesFromIndex(File dir, Object ix, Map<String, Long> sizes, Closure log) {
        def files = [], missing = []
        ix.entries.each { e ->
            def f = new File(dir, e.source_path as String)
            long len = f.length()
            if (len == 0L && !f.isFile()) { missing << e.source_path; return }
            sizes[f.getAbsolutePath()] = len
            files << f
        }
        if (missing) {
            throw new IllegalArgumentException(
                IX.H5 + " lists " + missing.size() + " file(s) that are not on disk, e.g.\n  " +
                missing.sort().take(5).join("\n  ") +
                "\n  The index is stale or the data has moved. Scan with listing=walk to ignore it.")
        }
        log?.call("  " + files.size() + " file(s) listed by " + IX.H5 + ", none missing")
        return files.unique().sort { it.getAbsolutePath() }
    }

    /**
     * Every placed file against what the index says about it.
     *
     * On the index route the index chose the files, so if it disagrees with
     * the sidecars about what one of them IS, one of the two is wrong and the
     * tables would be built on whichever happened to be consulted. That stops
     * the scan. The stack is compared through the setup NAME (`st:N`), never
     * through `<tile>` -- see LuxendoIndex.
     */
    List<String> crossCheck(File dir, Object ix, Map info) {
        def bySource = ix.entries.collectEntries { [(it.source_path): it] }
        def bad = []
        info.each { String path, sc ->
            def rel = relative(dir, new File(path)).replace('\\', '/')
            def e = bySource[rel]
            if (e == null) return
            def su = ix.setups[e.setup]
            def diffs = []
            if (e.t != sc.timePoint)                          diffs << ("t " + e.t + " vs " + sc.timePoint)
            if (su.channel != null && su.channel != sc.channel) diffs << ("channel " + su.channel + " vs " + sc.channel)
            if (su.stack != null && su.stack != sc.stack)       diffs << ("stack " + su.stack + " vs " + sc.stack)
            if ([su.sizeX, su.sizeY, su.sizeZ] != [sc.sizeX, sc.sizeY, sc.sizeZ]) {
                diffs << ("size " + [su.sizeX, su.sizeY, su.sizeZ].join("x") + " vs " +
                          [sc.sizeX, sc.sizeY, sc.sizeZ].join("x"))
            }
            if (!near(su.pixelWidth, sc.pixelWidth) || !near(su.pixelHeight, sc.pixelHeight) ||
                !near(su.pixelDepth, sc.pixelDepth)) {
                diffs << ("voxel " + [su.pixelWidth, su.pixelHeight, su.pixelDepth] + " vs " +
                          [sc.pixelWidth, sc.pixelHeight, sc.pixelDepth])
            }
            if (diffs) bad << (rel + " (setup " + e.setup + "): " + diffs.join(", ") + "  [index vs sidecar]")
        }
        return bad
    }

    private static boolean near(Double a, Double b) {
        if (a == null || b == null) return a == b
        return Math.abs(a - b) <= 1e-9 * Math.max(1d, Math.abs(a))
    }

    /** The walk and the index, compared as file sets. Paths only; warnings, not errors. */
    List<String> compareWithIndex(File dir, List<File> walked, Object ix) {
        def onDisk  = walked.collect { relative(dir, it).replace('\\', '/') }.toSet()
        def indexed = ix.entries.collect { it.source_path as String }.toSet()
        def out = []
        def notIndexed = (onDisk - indexed).sort()
        def notOnDisk  = (indexed - onDisk).sort()
        if (notIndexed) {
            out << (notIndexed.size() + " file(s) on disk that " + IX.H5 + " does not list, e.g. " +
                    notIndexed.take(3).join(", ") +
                    " -- listing=index would leave them out of the tables")
        }
        if (notOnDisk) {
            out << (notOnDisk.size() + " file(s) listed by " + IX.H5 + " that the walk did not find " +
                    "(missing, or no .json sidecar), e.g. " + notOnDisk.take(3).join(", "))
        }
        return out
    }

    /**
     * Scan an acquisition into the two tables.
     *
     * @param dir   the acquisition directory (the one holding raw/)
     * @param opts  alias        -- short handle for this acquisition; blank uses
     *                              the folder name
     *              quickScan    -- one sidecar per DIRECTORY, not per file (default true)
     *              listing      -- auto | index | walk (default auto); see the header
     * @param log   called with progress lines
     * @return [series: List&lt;Map&gt;, sources: List&lt;Map&gt;, warnings: List&lt;String&gt;,
     *          listing: "index" | "walk" -- the route actually taken]
     */
    Map scan(File dir, Closure log = null) { return scan(dir, [:], log) }

    Map scan(File dir, Map opts, Closure log = null) {
        if (opts != null && opts.containsKey("gatherFrames")) {
            // Removed in v0.7.0. Refused rather than ignored: a caller that
            // asked for per-time-point series must not quietly get one series
            // per stack and misread every id that follows.
            throw new IllegalArgumentException(
                "gatherFrames was removed in v0.7.0: a series is always the whole stack, every time " +
                "point inside. To take out one time point, use Make_LuxendoTiff's `frames`.")
        }
        boolean quick = (opts != null && opts.containsKey("quickScan")) ? (opts.quickScan as boolean) : true
        String listing = checkListing(opts?.listing)
        if (!dir.isDirectory()) {
            throw new IllegalArgumentException("Not a directory: " + dir.getAbsolutePath())
        }

        def sizes = new HashMap<String, Long>()
        def listWarnings = []
        boolean haveIndex = IX.present(dir)
        def ix = null
        String route
        List<File> files
        if (listing == LISTING_INDEX || (listing == LISTING_AUTO && haveIndex)) {
            if (!haveIndex) {
                throw new IllegalArgumentException(
                    "listing=index, but there is no " + IX.H5 + " + " + IX.XML + " in " + dir.getAbsolutePath() +
                    "\n  Luxendo writes them at the end of an acquisition. Use listing=walk.")
            }
            route = LISTING_INDEX
            log?.call("  file list: " + IX.H5 + " + " + IX.XML + " (no directory walk)")
            ix = IX.read(dir)
            files = filesFromIndex(dir, ix, sizes, log)
        } else {
            route = LISTING_WALK
            log?.call("  file list: walking the directory tree" +
                      (haveIndex ? "" : " (no " + IX.H5 + " + " + IX.XML + " index here)"))
            files = findPairs(dir, log, sizes)
            if (haveIndex) {
                // The walk is the only route that can see a file the index does
                // not list, so with both available it says how they differ.
                ix = IX.read(dir)
                listWarnings.addAll(compareWithIndex(dir, files, ix))
                if (!listWarnings) log?.call("  the walk and " + IX.H5 + " list the same files")
            }
        }
        // THE ALIAS IS THE OPERATOR'S, and the folder name is only its DEFAULT
        // -- exactly as `files.tsv`'s alias defaults to the basename. The whole
        // point of the column is to let a person name a run something other
        // than whatever the camera called the directory.
        def alias = RX.sanitize(((opts?.alias ?: "").toString().trim()) ?: dir.getName())
        if (!alias) {
            throw new IllegalArgumentException(
                "alias is empty after sanitising; give one explicitly")
        }

        def info = quick ? readSampled(dir, files, sizes, log) : readEvery(dir, files, log)

        if (route == LISTING_INDEX) {
            def bad = crossCheck(dir, ix, info)
            if (bad) {
                throw new IllegalArgumentException(
                    IX.H5 + "/" + IX.XML + " and the sidecars disagree on " + bad.size() + " file(s), e.g.\n  " +
                    bad.take(5).join("\n  ") +
                    "\n  One of them is wrong. Scan with listing=walk to ignore the index.")
            }
            log?.call("  " + info.size() + " file(s) agree with the index on time point, channel, stack, size and voxel")
        }

        def sources = []
        files.each { File f ->
            def sc = info[f.getAbsolutePath()]
            // ⚠️ THE ALIAS IS PART OF THE PREFIX, and it has to be.
            // `s<NNNN>_<stack_description>` is unique only WITHIN one
            // acquisition: measured on two real acquisitions, all 14 stack
            // identities were identical ("stack_0-L26A pos1" in both), so
            // without the alias both produce `s0000_L26A_pos1_t0000` and, run
            // separately into one output directory, silently overwrite each
            // other's _outline.txt, _res.txt and _config.txt. That is exactly
            // what `CLAUDE.md`'s "unique by construction" rule exists to stop.
            def serId = SS.composeSeriesId(alias, sc.stack, sc.stackDescription ?: "")
            sources << [
                source_path  : relative(dir, f),
                // THE JOIN KEY, and both halves are machine-owned -- see the
                // header for why it is not series_id.
                alias        : alias,
                series_index : sc.stack,
                // Counted from 1, as ImageJ shows them -- the repo's rule for
                // every image axis it writes (note/fiji_vocabulary.md). The
                // ONE place Luxendo's 0-based count becomes ours: the sidecar
                // values stay as Luxendo wrote them everywhere else, and are
                // compared with the index in that count.
                channel      : sc.channel + 1,
                channel_name : (sc.channelDescription ?: ""),
                t            : sc.timePoint + 1,
                size_x       : sc.sizeX,
                size_y       : sc.sizeY,
                size_z       : sc.sizeZ,
                pixel_width  : sc.pixelWidth,
                pixel_height : sc.pixelHeight,
                // null, not 1.0, for a single plane -- see LuxendoSidecar
                pixel_depth  : sc.pixelDepth,
                pixel_unit   : (sc.pixelWidth != null ? PIXEL_UNIT : null),
                source_bytes : sizes[f.getAbsolutePath()],
                // Not columns. Carried for building the series table and for
                // naming series in messages, then removed before the table is
                // handed back -- the schema is the contract, and these are
                // workings.
                _series_id   : serId,
                _stack_desc  : (sc.stackDescription ?: ""),
            ]
        }
        sources = sortSources(sources)
        def series = buildSeries(sources, dir, alias)
        listWarnings.each { log?.call("WARNING: " + it) }
        def warnings = listWarnings + validate(series, sources, log)
        sources.each { r -> r.remove("_series_id"); r.remove("_stack_desc") }
        return [series: series, sources: sources, warnings: warnings, listing: route]
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
     * The key a source row joins its series row on: (alias, series_index).
     *
     * Both halves are machine columns -- the acquisition and the stack -- so an
     * edited series_id cannot break the join, and two acquisitions in one table
     * cannot collide on a stack number. Strings, because a TSV read gives
     * "3" where a scan gives 3, and the two must meet. NOT `?:` on the index:
     * Groovy reads 0 as false, and stack 0 exists in every acquisition.
     */
    static List seriesKey(Map r) {
        def idx = r.series_index
        return [(r.alias == null ? "" : r.alias.toString().trim()),
                (idx == null ? "" : idx.toString().trim())]
    }

    /**
     * Refuse a sources table written before v0.8.0, which counted `t` and
     * `channel` from 0.
     *
     * Read by today's code it would be off by one SILENTLY -- frame 0 assembled
     * as if it were the first of a 1-based run, channel 0 measured as ch0, which
     * matches no ch1 anywhere. But every such table has t = 0 and channel = 0
     * rows (every acquisition has a first frame and a first channel), and a
     * table written since cannot hold a 0, so "both at least 1" catches every
     * old one and never a new one.
     */
    static void requireOneBased(List<Map> sources) {
        def zero = sources.find { r ->
            (r.t != null && r.t.toString().trim() == "0") ||
            (r.channel != null && r.channel.toString().trim() == "0")
        }
        if (zero != null) {
            throw new IllegalArgumentException(
                "This sources table counts t and channel from 0 (" + zero.source_path +
                " has t=" + zero.t + ", channel=" + zero.channel + "): it was written before v0.8.0, " +
                "which counts every image axis from 1, as ImageJ shows them. Run Make_LuxendoSheets " +
                "into another directory and take only its sources.tsv: the series table is " +
                "unchanged by this, so yours, with its edits, still pairs with the new one.")
        }
    }

    /**
     * Each source row with its series' `series_id` attached, looked up through
     * seriesKey() -- the one join between the two tables, used by everything
     * that needs it (Make_LuxendoTiff, the tests, and the resolver to come).
     *
     * A source with no series row is FATAL, named: a join that matches nothing
     * is the failure this repo is built around. A series with no sources is
     * returned in `unsourced` for the caller to judge -- a person may trim the
     * sources table on purpose.
     *
     * @return [sources: copies with series_id set, unsourced: [series_id, ...]]
     */
    static Map withSeriesId(List<Map> sources, List<Map> series) {
        requireOneBased(sources)
        def idByKey = [:]
        series.each { r ->
            def k = seriesKey(r)
            if (idByKey.containsKey(k)) {
                throw new IllegalArgumentException(
                    "Two series rows share alias " + k[0] + " and series_index " + k[1] + ": " +
                    idByKey[k] + " and " + r.series_id + ". The series table must come from one scan per acquisition.")
            }
            idByKey[k] = r.series_id
        }
        def orphans = new TreeSet()
        def out = sources.collect { r ->
            def k = seriesKey(r)
            if (!idByKey.containsKey(k)) { orphans << (k[0] + "/" + k[1]); return null }
            def c = new LinkedHashMap(r); c.series_id = idByKey[k]; return c
        }
        if (orphans) {
            throw new IllegalArgumentException(
                orphans.size() + " (alias/series_index) in the sources table have no series row: " +
                orphans.take(10).join(", ") + (orphans.size() > 10 ? ", ..." : "") +
                "\n  The two tables must come from the same scan.")
        }
        def used = sources.collect { seriesKey(it) }.toSet()
        def unsourced = idByKey.findAll { k, v -> !used.contains(k) }.values().toList().sort()
        return [sources: out, unsourced: unsourced]
    }

    /**
     * The series table: one row per STACK, every time point inside.
     *
     * Four of the series table's required columns, read the Luxendo way --
     * true here, not borrowed:
     *
     *   path          the acquisition directory -- the root source_path resolves against
     *   series_index  the `stack` number: the index this container addresses a series by
     *   series_name   stack_description, before sanitising
     *   alias         the acquisition's handle, the operator's, defaulting to the folder name
     */
    private List<Map> buildSeries(List<Map> sources, File dir, String alias) {
        def out = []
        sources.groupBy { seriesKey(it) }.each { List key, List<Map> rows ->
            def first = rows[0]
            def chans = rows.collect { it.channel as int }.unique().sort()
            def ts    = rows.collect { it.t as int }.unique().sort()
            out << [
                series_id   : first._series_id,
                include     : "true",
                alias       : alias,
                series_index: first.series_index,
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
        return out.sort(false) { a, b -> (a.series_id <=> b.series_id) }
    }

    /**
     * The checks that must happen before a long run.
     *
     * Severity differs on purpose, following the series table: something that
     * makes the tables unusable stops; something that is merely the signature
     * of a mistake is reported loudly and left for a person to judge.
     */
    List<String> validate(List<Map> series, List<Map> sources, Closure log = null) {
        def warnings = []
        // Series are NAMED by their id in messages, and FOUND by their key.
        def nameOf = series.collectEntries { [(seriesKey(it)): it.series_id] }
        def name = { List k -> nameOf[k] ?: (k[0] + "/" + k[1]) }

        // Two sources claiming the same channel of the same series FRAME would
        // overwrite each other in the assembled stack, silently. Keyed on the
        // time point too, because one series holds channel 0 once per frame.
        def dupes = sources.groupBy { seriesKey(it) + [it.t, it.channel] }.findAll { k, v -> v.size() > 1 }
        if (dupes) {
            throw new IllegalArgumentException(
                "Two sources claim the same series channel:\n  " +
                dupes.collect { k, v ->
                    name(k.take(2)) + " t=" + k[2] + " channel " + k[3] + " <- " +
                    v.collect { it.source_path }.join(" AND ")
                }.join("\n  "))
        }

        // Within ONE series every source must agree on dimensions, or they
        // cannot go into one stack at all -- which makes z changing between
        // time points FATAL: there is no single volume shape for the series.
        sources.groupBy { seriesKey(it) }.each { List k, List<Map> v ->
            def shapes = v.collect { [it.size_x, it.size_y, it.size_z] }.unique()
            if (shapes.size() > 1) {
                throw new IllegalArgumentException(
                    name(k) + ": its sources disagree on dimensions: " +
                    v.collect { "t" + it.t + "/ch" + it.channel + "=" +
                                it.size_x + "x" + it.size_y + "x" + it.size_z }.unique().join(", "))
            }
        }

        // The two tables are joined on (alias, series_index), and nothing may be
        // orphaned on either side. A join that matches nothing is the failure
        // this repo is built around, so it is checked where the tables are made
        // rather than discovered later by whatever consumes them.
        def serKeys = series.collect { seriesKey(it) }.toSet()
        def srcKeys = sources.collect { seriesKey(it) }.toSet()
        def orphanSources = srcKeys - serKeys
        def emptySeries   = serKeys - srcKeys
        if (orphanSources || emptySeries) {
            throw new IllegalStateException(
                "The two tables do not agree on (alias, series_index)" +
                (orphanSources ? ("\n  sources with no series row: " + orphanSources.collect { it.join("/") }.sort().join(", ")) : "") +
                (emptySeries   ? ("\n  series with no sources: "    + emptySeries.collect { name(it) }.sort().join(", ")) : ""))
        }

        // Every FRAME must have the same channel set, or one assembled stack has
        // channel 2 where another has channel 3 and nothing says so. Per frame
        // rather than per series, so a series missing one channel at one time
        // point is still caught.
        def channelSets = sources.groupBy { seriesKey(it) + [it.t] }
                                 .collectEntries { k, v -> [k, v.collect { it.channel }.sort()] }
        def expected = channelSets.values().first()
        def odd = channelSets.findAll { k, v -> v != expected }
        if (odd) {
            warnings << ("Not every frame has the same channels; expected " + expected + " -- " +
                         odd.collect { k, v -> name(k.take(2)) + " t=" + k[2] + " has " + v }.join(", "))
        }

        // Time points should be a run from 1. A gap means a file is missing,
        // which is worth knowing BEFORE the tracking step reports a broken track.
        sources.groupBy { seriesKey(it) }.each { List k, List<Map> v ->
            def ts = v.collect { it.t as int }.unique().sort()
            if (ts != (1..ts.size()).toList()) {
                warnings << ("Series " + name(k) + " has time points " + ts + ", not a run from 1")
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
            ((a.alias ?: "").toString() <=> (b.alias ?: "").toString()) ?:
            ((a.series_index as int) <=> (b.series_index as int)) ?:
            ((a.t as int) <=> (b.t as int)) ?:
            ((a.channel as int) <=> (b.channel as int))
        }
    }

    /** Sources grouped by the series they feed -- keyed by seriesKey() -- in table order. */
    static Map<List, List<Map>> bySeries(List<Map> rows) {
        def m = new LinkedHashMap()
        sortSources(rows).each { r -> m.computeIfAbsent(seriesKey(r), { [] }) << r }
        return m
    }
}
