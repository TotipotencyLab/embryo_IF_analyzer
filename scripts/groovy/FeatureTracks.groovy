// FeatureTracks.groovy
//
// Features linked across time: annotate's centroid tables -> TrackMate's LAP
// tracker -> one table of links per series. The library behind
// Make_FeatureTracks.groovy; note/time_series_plan.md §4 `tracking` is the
// design, note/data_formats.md the files.
//
// TrackMate is driven WITHOUT an image (§5.2): a SpotCollection built from the
// centroid table, SparseLAPTrackerFactory, the graph read back. Features exist
// only after R's grouping, which is why the round trip goes R -> Fiji -> R.
//
// What TrackMate 7.14.0 does, measured on synthetic spots, because the
// settings do not say it and each one changes what a track is:
//   - A frame is linked to the next frame that HOLDS SPOTS. A frame with none
//     is not a gap; it is not there.
//   - MAX_FRAME_GAP is a difference in frame numbers: 2 bridges ONE missing
//     frame, 1 bridges none.
//   - Gap closing and splitting count frame NUMBERS, so time points sampled
//     1, 10, 20 are nine frames apart to them: nothing would ever be
//     gap-closed, and no division recorded (a split must be one frame). The
//     tracker is therefore given each time point's RANK among the time points
//     the run ANALYSED -- `frames_analysed`, which annotate stamps on the
//     centroid table from Fiji's _config.txt -- and `t` is restored on the way
//     out. Not the time points that happen to hold a feature: a frame where
//     nothing of the type was found would then not be a step at all, so a
//     link across it would look adjacent (not gap-closed, allowed at
//     max_frame_gap 1, and a division across it recorded as a split), and
//     whether it did would depend on whether some OTHER feature was found
//     that frame. Because of the first point, such a frame is given a
//     placeholder spot, far from everything, removed again before anything is
//     written (link()).
//   - Gap closing and splitting link track SEGMENTS, and a spot linked to
//     nothing frame to frame is not a segment: a feature seen in one frame
//     only is never gap-closed, and a daughter seen in one frame only is
//     never split onto -- it starts a track of its own.
//   - Distances must be strictly below the maximum: at exactly
//     LINKING_MAX_DISTANCE nothing is linked.
//   - A division after a gap (the mother lost for a frame at envelope
//     breakdown) is NOT recorded as a split: one daughter is gap-closed onto
//     the mother, the other starts a track of its own. Splitting happens only
//     between adjacent frames. That is what hand edits (`link`) are for.
//   - The result does not depend on thread count or on input order (checked
//     on 240 spots), and the input is sorted anyway.

import fiji.plugin.trackmate.Spot
import fiji.plugin.trackmate.SpotCollection
import fiji.plugin.trackmate.TrackMate
import fiji.plugin.trackmate.tracking.jaqaman.SparseLAPTrackerFactory

class FeatureTracks {

    static final String CENTROID_SUFFIX = "_feature_centroids.tsv"

    /** The `# frames_analysed: ...` line annotate writes above the header. */
    static final String AXIS_KEY = "frames_analysed"

    /** A placeholder spot's name: never a feature_id, which is `<type>_NNNN`. */
    static final String PLACEHOLDER = "__placeholder_"

    /** What a centroid table must hold for this step (it holds more). */
    static final List<String> CENTROID_NEED =
        ["series_id", "t", "feature_type", "feature_id", "x", "y", "z", "run_id", "fingerprint"]

    /** `<stem>_<type>_tracks.tsv`: one row per link, and a start row per feature nothing leads to. */
    static final List<String> TRACK_COLUMNS =
        ["series_id", "t", "feature_id", "prev_feature_id", "run_id"]

    /** `<stem>_<type>_track_edits.tsv`: hand corrections, applied by R's join_tracks(). */
    static final List<String> EDIT_COLUMNS =
        ["action", "from_feature_id", "to_feature_id", "fingerprint", "note"]

    /**
     * The tracker settings this step exposes, in calibrated units (the centroid
     * table's, µm here) and TrackMate's own defaults. Gap closing and splitting
     * are always on: a frame where detection drops an object, and a division,
     * are both ordinary here.
     */
    static final Map<String, Object> DEFAULTS = [
        linking_max_distance    : 15.0d,
        gap_closing_max_distance: 15.0d,
        max_frame_gap           : 2,
        splitting_max_distance  : 15.0d,
        allow_merging           : false,
        merging_max_distance    : 15.0d,
    ]

    Object TSV          // Tsv class
    Object RX           // RoiExport class, for repoVersion()
    String libDir

    static FeatureTracks load(String libDir) {
        def dir = new File(libDir)
        def gcl = new GroovyClassLoader(FeatureTracks.class.classLoader)
        def ft = new FeatureTracks()
        ft.libDir = libDir
        ft.TSV = gcl.parseClass(new File(dir, "Tsv.groovy"))
        ft.RX = gcl.parseClass(new File(dir, "RoiExport.groovy"))
        return ft
    }

    static String trackmateVersion() {
        try { return TrackMate.PLUGIN_NAME_VERSION } catch (Throwable ignored) { return "unknown" }
    }

    // --- settings ---------------------------------------------------------------

    /**
     * `true` or `false`, and nothing else -- not blank, not `yes`.
     *
     * Required, with no default: whether z belongs in the distance cannot be
     * settled on the data available (§5.4), so the run must say which it chose.
     * A `#@ Boolean` cannot be "not chosen" -- it has a default or it hangs
     * headless -- hence a string, refused when blank.
     */
    static boolean parseUseZ(Object v) {
        def s = (v == null) ? "" : v.toString().trim()
        if (s == "true") return true
        if (s == "false") return false
        throw new IllegalArgumentException(
            "useZ must be true (3-D distance) or false (x-y only); got >>>" + s + "<<<. " +
            "There is no default: choose it for the data (note/time_series_plan.md §5.4).")
    }

    /**
     * The settings for one run, checked: the defaults overlaid with what the
     * caller gave, plus the two that have no default (feature_type, use_z).
     */
    static Map<String, Object> checkSettings(Map raw) {
        def unknown = raw.keySet() - (DEFAULTS.keySet() + ["feature_type", "use_z"])
        if (unknown) throw new IllegalArgumentException("Unknown tracking setting(s): " + unknown.join(", "))
        def s = new LinkedHashMap<String, Object>()
        def type = (raw.feature_type == null) ? "" : raw.feature_type.toString().trim()
        if (!type) {
            throw new IllegalArgumentException(
                "featureType is blank: name the feature to track as annotate reports it " +
                "(after --rename), e.g. nucleus")
        }
        s.feature_type = type
        s.use_z = parseUseZ(raw.use_z)
        // Absent means "the default"; present but null is a field someone blanked,
        // and turning that into 15 um without a word is the silent default this
        // repo keeps getting bitten by.
        def blank = DEFAULTS.keySet().findAll { raw.containsKey(it) && raw[it] == null }
        if (blank) throw new IllegalArgumentException("Tracking setting(s) left blank: " + blank.join(", "))
        DEFAULTS.each { k, d -> s[k] = raw.containsKey(k) ? raw[k] : d }
        ["linking_max_distance", "gap_closing_max_distance", "splitting_max_distance",
         "merging_max_distance"].each { k ->
            double v = s[k] as double
            // Not `!(v > 0)` alone: Groovy compares a NaN through compareTo(),
            // and `Double.NaN > 0` is TRUE there.
            if (Double.isNaN(v) || !(v > 0) || Double.isInfinite(v)) {
                throw new IllegalArgumentException(k + " must be a positive distance; got " + s[k])
            }
            s[k] = v
        }
        int gap = s.max_frame_gap as int
        if (gap < 1) {
            throw new IllegalArgumentException(
                "max_frame_gap must be at least 1 (1 bridges no missing frame, 2 bridges one); got " + gap)
        }
        s.max_frame_gap = gap
        def m = s.allow_merging
        if (!(m instanceof Boolean)) {
            // Never `as boolean` on a string: "false" is a non-empty string, and true.
            def ms = m?.toString()?.trim()
            if (!(ms in ["true", "false"])) {
                throw new IllegalArgumentException("allow_merging must be true or false; got >>>" + ms + "<<<")
            }
            m = (ms == "true")
        }
        s.allow_merging = m
        return s
    }

    /** TrackMate's settings map: its defaults, with ours over them. */
    static Map<String, Object> trackerSettings(Map s) {
        def st = new SparseLAPTrackerFactory().getDefaultSettings()
        st.LINKING_MAX_DISTANCE     = s.linking_max_distance as Double
        st.ALLOW_GAP_CLOSING        = true
        st.GAP_CLOSING_MAX_DISTANCE = s.gap_closing_max_distance as Double
        st.MAX_FRAME_GAP            = s.max_frame_gap as Integer
        st.ALLOW_TRACK_SPLITTING    = true
        st.SPLITTING_MAX_DISTANCE   = s.splitting_max_distance as Double
        st.ALLOW_TRACK_MERGING      = s.allow_merging as Boolean
        st.MERGING_MAX_DISTANCE     = s.merging_max_distance as Double
        return st
    }

    // --- input ------------------------------------------------------------------

    /**
     * One centroid table's features of one type, checked and parsed.
     *
     * Refused rather than skipped: a table this step cannot read completely
     * would give a tracks table missing features, which the join would then
     * have to catch. Blank z is refused only when z is used -- annotate writes
     * it blank when no z step was found, and an x-y run does not need it.
     *
     * The time axis is the `# frames_analysed:` line annotate copies from
     * Fiji's _config.txt. A table holding several time points without it is
     * refused: tracking across a frame nothing was found in would otherwise be
     * silently wrong (see the top of this file). A `t` the line does not name
     * is refused too -- the centroids and that config are from different runs.
     *
     * @return [series_id, run_id, fingerprint, features: [[feature_id, t, x, y, z]],
     *         types: every type in the table, frames: the time axis, sorted]
     */
    Map readCentroids(File f, String type, boolean useZ) {
        def rows = TSV.read(f)
        def name = f.getName()
        // From the header, not the first row: a table with no rows must still be
        // a centroid table, or any file with the right suffix passes as "a
        // series with no features".
        def missing = CENTROID_NEED - headerOf(f)
        if (missing) {
            throw new IllegalArgumentException(name + " lacks column(s): " + missing.join(", ") +
                " -- is it a centroid table from annotate_features_cli.r?")
        }
        def stamp = stampOf(f)
        List<Integer> axis = null
        if (stamp.containsKey(AXIS_KEY)) {
            axis = parseAxis(stamp[AXIS_KEY], name)
        }
        def tsAll = rows.collect { it.t }.findAll { it ==~ /^\d+$/ }.collect { it as int }.unique().sort()
        if (axis == null) {
            if (tsAll.size() > 1) {
                throw new IllegalArgumentException(name + " holds " + tsAll.size() + " time points but no '# " +
                    AXIS_KEY + ":' line, so which frames the run analysed -- including any where nothing " +
                    "was found -- is unknown. Re-run annotate_features_cli.r with Fiji's _config.txt beside " +
                    "its inputs (it is read from there, like pixel_depth).")
            }
            axis = tsAll
        } else {
            def stray = tsAll - axis
            if (stray) {
                throw new IllegalArgumentException(name + ": t = " + stray.join(", ") + " is not among the " +
                    "frames the run analysed (" + AXIS_KEY + ": " + stamp[AXIS_KEY] + "). The centroids and " +
                    "the _config.txt annotate read are from different runs -- re-run annotate on the current one.")
            }
        }
        if (rows.isEmpty()) {
            return [series_id: null, run_id: null, fingerprint: null, features: [], types: [], frames: axis]
        }
        def sids = rows.collect { it.series_id }.unique()
        if (sids.size() != 1 || !sids[0]) {
            throw new IllegalArgumentException(name + " must hold one series; it holds " +
                (sids.collect { it ?: "(blank)" }.join(", ")))
        }
        def mine = rows.findAll { it.feature_type == type }
        def out = [series_id: sids[0], types: rows.collect { it.feature_type }.unique().sort(),
                   features: [], run_id: null, fingerprint: null, frames: axis]
        if (mine.isEmpty()) return out

        def one = { String col ->
            def v = mine.collect { it[col] }.unique()
            if (v.size() != 1 || !v[0]) {
                throw new IllegalArgumentException(name + ": the " + type + " rows must share one " + col +
                    "; found " + v.collect { it ?: "(blank)" }.join(", ") +
                    (col == "run_id" ? " -- re-run annotate_features_cli.r" : ""))
            }
            return v[0]
        }
        out.run_id = one("run_id")
        out.fingerprint = one("fingerprint")

        def num = { Map r, String col, boolean need ->
            def s = r[col]
            if (!s) {
                if (!need) return null
                throw new IllegalArgumentException(name + ": " + r.feature_id + " has a blank " + col)
            }
            try {
                double d = Double.parseDouble(s)
                if (Double.isNaN(d) || Double.isInfinite(d)) throw new NumberFormatException()
                return d
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException(name + ": " + r.feature_id + " has " + col + " = '" + s + "', not a number")
            }
        }
        def blankZ = mine.findAll { !it.z }
        if (useZ && blankZ) {
            throw new IllegalArgumentException(name + ": useZ=true, but z is blank for " + blankZ.size() +
                " of " + mine.size() + " " + type + " feature(s). annotate leaves z blank when it found " +
                "no z step (pixel_depth in the _config.txt beside its input) -- fix that and re-run it, " +
                "or track on x-y with useZ=false.")
        }
        def seen = [] as Set
        mine.each { r ->
            def id = r.feature_id
            if (!id?.startsWith(type + "_")) {
                throw new IllegalArgumentException(name + ": '" + id + "' is not a " + type +
                    " feature (" + type + "_NNNN); a centroid table holds real features only")
            }
            if (!seen.add(id)) throw new IllegalArgumentException(name + ": " + id + " appears twice")
            if (!(r.t ==~ /^\d+$/) || (r.t as int) < 1) {
                throw new IllegalArgumentException(name + ": " + id + " has t = '" + r.t + "'; t counts from 1")
            }
            out.features << [feature_id: id, t: r.t as int, x: num(r, "x", true), y: num(r, "y", true),
                             z: useZ ? num(r, "z", true) : 0.0d]
        }
        out.features.sort { a, b -> a.t <=> b.t ?: a.feature_id <=> b.feature_id }
        return out
    }

    /** Text without the byte-order mark a spreadsheet may save first (as Tsv.read() drops it). */
    static String bomless(String text) {
        return (text != null && text.startsWith("\uFEFF")) ? text.substring(1) : text
    }

    /** A table's header as Tsv.read() finds it: the first line not blank and not a comment. */
    static List<String> headerOf(File f) {
        def line = bomless(f.getText("UTF-8")).readLines().find { it.trim() && !it.trim().startsWith("#") }
        return line == null ? [] : line.split("\t", -1).collect { it.trim() }
    }

    /**
     * The `# key: value` lines above a table's header, as annotate stamps them
     * on a centroid table. Only lines before the header count, and only that
     * shape; any other comment is ignored.
     */
    static Map<String, String> stampOf(File f) {
        def out = new LinkedHashMap<String, String>()
        for (String line : bomless(f.getText("UTF-8")).readLines()) {
            def tl = line.trim()
            if (!tl) continue
            if (!tl.startsWith("#")) break
            def m = (tl =~ /^#\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*(.*)$/)
            if (m) out[m[0][1] as String] = (m[0][2] as String).trim()
        }
        return out
    }

    /**
     * `frames_analysed` as _config.txt writes it: time points separated by
     * spaces (commas accepted too), each from 1, none twice.
     */
    static List<Integer> parseAxis(String v, String name) {
        def parts = (v ?: "").split(/[\s,]+/).findAll { it }
        if (!parts || parts.any { !(it ==~ /^\d+$/) }) {
            throw new IllegalArgumentException(name + ": '# " + AXIS_KEY + ": " + v + "' is not a list of " +
                "time points like 1 10 20")
        }
        def ts = parts.collect { it as int }
        if (ts.contains(0)) throw new IllegalArgumentException(name + ": " + AXIS_KEY + " holds 0; t counts from 1")
        if (ts.unique(false).size() != ts.size()) {
            throw new IllegalArgumentException(name + ": " + AXIS_KEY + " names a time point twice: " + v)
        }
        return ts.sort()
    }

    // --- linking ----------------------------------------------------------------

    /**
     * Link one series' features. Pure: features in, link rows out.
     *
     * @param feats  [feature_id, t, x, y, z] maps, one per feature
     * @param s      checkSettings() output
     * @param axis   the time points the run analysed (readCentroids()'s
     *               frames); null = the features' own, which is right only
     *               when every one of them holds a feature
     * @return [rows: one map per link, plus a start row (blank prev) for every
     *         feature nothing leads to, sorted by t, feature_id, prev_feature_id;
     *         frames: the time axis, in order; absent: the time points on it
     *         holding no feature; stats: n_links, n_gap_closed, n_divisions,
     *         n_merges]
     */
    static Map link(List<Map> feats, Map s, List<Integer> axis = null) {
        def frames = (axis != null ? axis : feats.collect { it.t as int }).unique(false).sort(false)
        def rank = [:]
        frames.eachWithIndex { int t, int i -> rank[t] = i + 1 }
        def tOf = feats.collectEntries { [it.feature_id, it.t as int] }
        def held = tOf.values() as Set
        def stray = held - frames
        if (stray) throw new IllegalArgumentException("t = " + stray.join(", ") + " is not on the time axis " + frames)
        def absent = frames.findAll { !held.contains(it) }

        def edges = []          // [prev, next] feature_ids
        if (frames.size() > 1 && feats) {
            def sc = new SpotCollection()
            feats.each { f ->
                // The radius and quality are not used by the tracker without
                // feature penalties, and none are set.
                def spot = new Spot(f.x as double, f.y as double, f.z as double, 1.0d, 1.0d, f.feature_id as String)
                sc.add(spot, rank[f.t as int] as Integer)
            }
            // TrackMate links a frame to the next frame that HOLDS spots, so a
            // time point with no feature would not be a step. One placeholder
            // there makes it one. Each sits further than any distance setting
            // from every feature and from every other placeholder, so nothing
            // can be linked to it -- and if anything ever were, the run stops
            // below rather than write a link through a spot that is not there.
            if (absent) {
                double maxD = [s.linking_max_distance, s.gap_closing_max_distance,
                               s.splitting_max_distance, s.merging_max_distance].collect { it as double }.max()
                def coords = feats.collectMany { f -> [f.x as double, f.y as double, f.z as double] }
                double extent = coords.collect { Math.abs(it) }.max()
                double step = 10.0d * (maxD + 2.0d * extent) + 1.0e6d
                absent.each { int t ->
                    double far = extent + step * rank[t]
                    sc.add(new Spot(far, far, 0.0d, 1.0d, 1.0d, PLACEHOLDER + rank[t]), rank[t] as Integer)
                }
            }
            sc.setVisible(true)
            def factory = new SparseLAPTrackerFactory()
            def st = trackerSettings(s)
            if (!factory.checkSettingsValidity(st)) {
                throw new IllegalStateException("TrackMate refused the settings: " + factory.getErrorMessage())
            }
            def tracker = factory.create(sc, st)
            tracker.setNumThreads(1)
            if (!tracker.checkInput() || !tracker.process()) {
                throw new IllegalStateException("TrackMate failed: " + tracker.getErrorMessage())
            }
            def g = tracker.getResult()
            g.edgeSet().each { e ->
                def a = g.getEdgeSource(e).getName(), b = g.getEdgeTarget(e).getName()
                if (a.startsWith(PLACEHOLDER) || b.startsWith(PLACEHOLDER)) {
                    throw new IllegalStateException("TrackMate linked a placeholder spot (" + a + " - " + b +
                        "); placeholders are placed out of reach so that this cannot happen. Nothing written.")
                }
                edges << (tOf[a] < tOf[b] ? [a, b] : [b, a])
            }
        }

        def preds = [:].withDefault { [] }
        edges.each { pr -> preds[pr[1]] << pr[0] }
        def rows = []
        feats.each { f ->
            def ps = preds[f.feature_id].sort()
            if (ps.isEmpty()) {
                rows << [t: f.t, feature_id: f.feature_id, prev_feature_id: ""]
            } else {
                ps.each { p -> rows << [t: f.t, feature_id: f.feature_id, prev_feature_id: p] }
            }
        }
        rows.sort { a, b -> a.t <=> b.t ?: a.feature_id <=> b.feature_id ?: a.prev_feature_id <=> b.prev_feature_id }

        def succ = edges.countBy { it[0] }
        def stats = [n_links    : edges.size(),
                     n_gap_closed: edges.count { rank[tOf[it[1]]] - rank[tOf[it[0]]] > 1 },
                     n_divisions: succ.count { k, v -> v > 1 },
                     n_merges   : preds.count { k, v -> v.size() > 1 }]
        return [rows: rows, frames: frames, absent: absent, stats: stats]
    }

    // --- one directory ----------------------------------------------------------

    /** `<stem>` of `<stem>_feature_centroids.tsv`: the outputs sit beside it under the same stem. */
    static String stemOf(File f) {
        return f.getName().substring(0, f.getName().length() - CENTROID_SUFFIX.length())
    }

    static File tracksFile(File centroids, String type) {
        return new File(centroids.getParentFile(), stemOf(centroids) + "_" + type + "_tracks.tsv")
    }
    static File editsFile(File centroids, String type) {
        return new File(centroids.getParentFile(), stemOf(centroids) + "_" + type + "_track_edits.tsv")
    }
    static File paramsFile(File centroids, String type) {
        return new File(centroids.getParentFile(), stemOf(centroids) + "_" + type + "_track_params.txt")
    }

    /**
     * Track every `*_feature_centroids.tsv` in `dir`, one series per file, each
     * on its own.
     *
     * Every input is read and checked before anything is written, so a bad
     * table in the middle of a directory stops the run with nothing half done.
     *
     * @param raw  feature_type, use_z, and any of DEFAULTS
     * @param log  receives one line at a time
     * @return [settings, series: [[series_id, stem, n_features, frames, stats, edits]]]
     */
    Map trackDirectory(File dir, Map raw, Closure log = { }) {
        def s = checkSettings(raw)
        if (!dir?.isDirectory()) throw new IllegalArgumentException("Not a directory: " + dir)
        // `._<name>` is the AppleDouble file macOS writes beside a file on a
        // non-Mac volume (the lab SSD) once an app has touched it: same suffix,
        // binary content, not a table (as LuxendoScan skips them).
        def files = (dir.listFiles({ File f -> f.isFile() && f.getName().endsWith(CENTROID_SUFFIX) &&
                                               !f.getName().startsWith("._") } as FileFilter)
                     ?: []).toList().sort { it.getName() }
        if (files.isEmpty()) {
            throw new IllegalArgumentException("No *" + CENTROID_SUFFIX + " in " + dir +
                " -- point this at annotate_features_cli.r's --outdir")
        }
        def tables = files.collect { f -> [file: f, cen: readCentroids(f, s.feature_type as String, s.use_z as boolean)] }
        if (tables.every { it.cen.features.isEmpty() }) {
            def present = tables.collectMany { it.cen.types }.unique().sort()
            throw new IllegalArgumentException("No '" + s.feature_type + "' features in any of the " + files.size() +
                " centroid table(s) in " + dir + "; types present: " + (present ? present.join(", ") : "none"))
        }

        log("Tracking " + s.feature_type + " in " + files.size() + " series: use_z=" + s.use_z +
            ", linking < " + s.linking_max_distance + ", gap closing < " + s.gap_closing_max_distance +
            " over up to " + s.max_frame_gap + " frame(s), splitting < " + s.splitting_max_distance +
            ", merging " + (s.allow_merging ? "< " + s.merging_max_distance : "off"))

        // Link every series before writing any: a TrackMate failure on the
        // third series must not leave the first two rewritten under these
        // settings and the rest under the last.
        tables.each { tb -> tb.res = link(tb.cen.features as List<Map>, s, tb.cen.frames as List<Integer>) }

        def version = RX.repoVersion(libDir)
        def out = [settings: s, series: []]
        tables.each { tb ->
            File f = tb.file
            def cen = tb.cen
            def res = tb.res
            def rows = res.rows.collect { r ->
                [series_id: cen.series_id, t: r.t, feature_id: r.feature_id,
                 prev_feature_id: r.prev_feature_id, run_id: cen.run_id]
            }
            TSV.write(rows, tracksFile(f, s.feature_type as String), TRACK_COLUMNS)
            def edits = seedEdits(editsFile(f, s.feature_type as String), cen.fingerprint as String,
                                  s.feature_type as String, log)
            writeParams(paramsFile(f, s.feature_type as String), s, version, f, cen, res)

            def st = res.stats
            def what
            if (cen.features.isEmpty()) {
                what = "no " + s.feature_type + " features; an empty tracks table"
            } else if (cen.features.collect { it.t }.unique().size() < 2) {
                what = cen.features.size() + " feature(s) in one frame: nothing to link, every feature a start row"
            } else {
                what = cen.features.size() + " feature(s) over " + res.frames.size() + " frame(s) -> " +
                       st.n_links + " link(s): " + st.n_gap_closed + " gap-closed, " +
                       st.n_divisions + " division(s), " + st.n_merges + " merge(s)"
            }
            log("  " + (cen.series_id ?: stemOf(f)) + ": " + what)
            if (cen.features && res.absent) {
                log("    no " + s.feature_type + " found at t = " + res.absent.join(", ") +
                    " (still time points: a link across them is gap-closed)")
            }
            out.series << [series_id: cen.series_id, stem: stemOf(f), n_features: cen.features.size(),
                           frames: res.frames, stats: st, edits: edits]
        }
        return out
    }

    /**
     * Seed the edits table, or leave a person's edits where they are.
     *
     * Rewritten, with the current fingerprint on its template line, ONLY when
     * it is absent or holds nothing but comments, blank lines and exactly the
     * header this method writes -- so an edits file left empty never blocks a
     * re-annotation. Anything else is a person's, and is never rewritten:
     * judged by what the file holds, not by whether it parses, because a file
     * a person has damaged (a duplicated column, a deleted header line) still
     * holds their edits. Held edits made against another annotation, or a file
     * that cannot be read as an edits table, are reported here; join_tracks()
     * refuses both.
     *
     * @return "seeded", "reseeded", "kept", "kept-stale" or "kept-unreadable"
     */
    String seedEdits(File dest, String fingerprint, String type, Closure log) {
        if (dest.isFile()) {
            def content = bomless(dest.getText("UTF-8")).readLines().findAll { it.trim() && !it.trim().startsWith("#") }
            boolean onlyHeader = content.isEmpty() ||
                (content.size() == 1 && content[0].split("\t", -1).collect { it.trim() } == EDIT_COLUMNS)
            if (!onlyHeader) {
                def missing = EDIT_COLUMNS - headerOf(dest)
                def rows = null
                def problem = missing ? "no header naming " + missing.join(", ") : null
                if (!problem) {
                    try { rows = TSV.read(dest) } catch (IllegalArgumentException e) { problem = e.getMessage() }
                }
                if (problem) {
                    log("  WARNING " + dest.getName() + " cannot be read as an edits table (" + problem +
                        "). Left exactly as it is; join_tracks() will refuse it until it is mended. " +
                        "The header is: " + EDIT_COLUMNS.join(" "))
                    return "kept-unreadable"
                }
                def stale = rows.findAll { it.fingerprint != fingerprint }
                if (stale) {
                    log("  WARNING " + dest.getName() + ": " + stale.size() + " of " + rows.size() +
                        " edit(s) were made against another annotation (fingerprint " +
                        stale.collect { it.fingerprint ?: "(blank)" }.unique().join(", ") + "; now " +
                        (fingerprint ?: "(none)") + "). join_tracks() will refuse them: redo them on " +
                        "these features, or restore the annotation they were made on.")
                    return "kept-stale"
                }
                return "kept"
            }
        }
        boolean existed = dest.isFile()
        def fp = fingerprint ?: ""
        def ex = type + "_0001"
        def lines = [
            "# Hand corrections to the tracks beside this file, applied by join_tracks() in R",
            "# (note/data_formats.md). One edit per row, in feature_ids only.",
            "# To add one: copy the template line at the bottom, delete its leading '# ', fill it in.",
            "#   link  " + ex + " " + type + "_0002   the second follows the first (earlier t first)",
            "#   cut   " + ex + " " + type + "_0002   remove that link",
            "#   join  " + ex + " " + type + "_0002   the first ends a branch, the second starts one; one object",
            "# Keep the fingerprint as written: it names the annotation these edits are made on,",
            "# and a row made on another annotation is refused rather than applied to other nuclei.",
            "# This file is rewritten by Make_FeatureTracks while it holds no edits, and kept once it does.",
            EDIT_COLUMNS.join("\t"),
            "# " + ["link", ex, type + "_0002", fp, "why"].collect { TSV.cell(it) }.join("\t"),
        ]
        dest.getParentFile()?.mkdirs()
        dest.setText(lines.join("\n") + "\n", "UTF-8")
        return existed ? "reseeded" : "seeded"
    }

    /** The settings used and what they made, as `parameter  value` rows. */
    void writeParams(File dest, Map s, String version, File centroids, Map cen, Map res) {
        def p = new LinkedHashMap<String, Object>()
        p.script = "Make_FeatureTracks.groovy"
        p.repo_version = version
        p.trackmate_version = trackmateVersion()
        p.tracker = "SparseLAPTracker"
        p.centroid_table = centroids.getName()
        p.series_id = cen.series_id
        p.feature_type = s.feature_type
        p.run_id = cen.run_id ?: ""
        p.fingerprint = cen.fingerprint ?: ""
        p.use_z = s.use_z
        p.linking_max_distance = s.linking_max_distance
        p.allow_gap_closing = true
        p.gap_closing_max_distance = s.gap_closing_max_distance
        p.max_frame_gap = s.max_frame_gap
        p.allow_track_splitting = true
        p.splitting_max_distance = s.splitting_max_distance
        p.allow_merging = s.allow_merging
        p.merging_max_distance = s.merging_max_distance
        p.frames = res.frames.join(",")
        p.frames_without_feature = res.absent.join(",")
        p.n_features = cen.features.size()
        res.stats.each { k, v -> p[k] = v }
        TSV.write(p.collect { k, v -> [parameter: k, value: v] }, dest, ["parameter", "value"])
    }
}
