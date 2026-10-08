// Test_FeatureTracks.groovy
//
// Centroid tables -> TrackMate -> tracks, on tables this test WRITES: every
// case is a handful of features placed so that only one set of links is right.
// The cases are note/time_series_plan.md §4 `tracking`'s verification list;
// where TrackMate's behaviour was not known in advance (a division after a
// gap, merging off, z left out) the test pins what it does, so a TrackMate
// upgrade that changes it fails here rather than in someone's lineage.
//
// Run headless from the repo root:
//
//   ImageJ-macosx --headless --console --run tests/groovy/Test_FeatureTracks.groovy

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "FeatureTracks.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-62s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def errOf = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() } }

def gcl = new GroovyClassLoader(this.class.classLoader)
def FTC = gcl.parseClass(new File(LIBDIR, "FeatureTracks.groovy"))
def TSV = gcl.parseClass(new File(LIBDIR, "Tsv.groovy"))
def FT = FTC.load(LIBDIR)

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_featuretracks_" + System.nanoTime())
tmp.mkdirs()
try {

def CEN_COLS = ["series_id", "t", "feature_type", "feature_id", "x", "y", "z",
                "n_roi", "area_sum", "area_max", "run_id", "fingerprint"]

/**
 * Write a centroid table, as annotate does, into its own directory.
 *
 * `spots` are [name, t, x, y, z]; z null is written blank. Feature ids are
 * numbered in list order, so a case reads in names (A1, D2) and the returned
 * map turns them into ids.
 */
def writeCase = { String label, List spots, Map opt = [:] ->
    def dir = new File(tmp, label); dir.mkdirs()
    def type = opt.type ?: "nucleus"
    def sid = opt.sid ?: ("S_" + label)
    def ids = [:]
    def rows = []
    spots.eachWithIndex { sp, int i ->
        def (n, t, x, y, z) = sp
        def ft = (sp.size() > 5) ? sp[5] : type
        def id = ft + "_" + String.format("%04d", i + 1)
        ids[n] = id
        rows << [series_id: sid, t: t, feature_type: ft, feature_id: id, x: x, y: y,
                 z: (z == null ? "" : z), n_roi: 3, area_sum: 300, area_max: 120,
                 run_id: opt.run_id ?: "run0000aaaa", fingerprint: opt.fp ?: "fp00000001"]
    }
    def f = new File(dir, (opt.stem ?: sid) + "_feature_centroids.tsv")
    TSV.write(rows, f, CEN_COLS)
    return [dir: dir, file: f, ids: ids, names: ids.collectEntries { k, v -> [v, k] }]
}

/** Run a case; return its rows and links in names: ["A2<-A1", "A1<-", ...]. */
def track = { Map cs, Map settings, List logs = null ->
    def res = FT.trackDirectory(cs.dir, [feature_type: "nucleus"] + settings) { if (logs != null) logs << it }
    def tf = FTC.tracksFile(cs.file, (settings.feature_type ?: "nucleus") as String)
    def rows = TSV.read(tf)
    def links = rows.collect { r -> cs.names[r.feature_id] + "<-" + (r.prev_feature_id ? cs.names[r.prev_feature_id] : "") }
    return [res: res, rows: rows, links: links, file: tf]
}

/**
 * What every tracks table must satisfy whatever the case -- the rules
 * join_tracks() will hold it to: every feature has a row, no pair repeats,
 * every predecessor exists and is earlier, one run_id.
 */
def invariants = { String label, Map cs, List rows ->
    def feats = TSV.read(cs.file).findAll { it.feature_type == "nucleus" }
    def tOf = feats.collectEntries { [it.feature_id, it.t as int] }
    check(label + ": every feature has a row", (feats.collect { it.feature_id } - rows.collect { it.feature_id }).size(), 0)
    check(label + ": no feature that is not in the table", (rows.collect { it.feature_id } - feats.collect { it.feature_id }).size(), 0)
    def pairs = rows.collect { it.feature_id + "|" + it.prev_feature_id }
    check(label + ": no (feature, prev) pair twice", pairs.size() - pairs.unique(false).size(), 0)
    check(label + ": every prev is a feature, earlier in time",
          rows.findAll { it.prev_feature_id }.count { !(tOf[it.prev_feature_id] != null && tOf[it.prev_feature_id] < (it.t as int)) }, 0)
    check(label + ": header", FTC.tracksFile(cs.file, "nucleus").readLines()[0], FTC.TRACK_COLUMNS.join("\t"))
}

def XY = [use_z: "false"]
def XYZ = [use_z: "true"]

// -----------------------------------------------------------------------------
println "\n--- 1. two objects moving apart over four frames: two tracks, not one and not four"
def c1 = writeCase("apart", [["A1",1,0,0,0],["A2",2,-3,0,0],["A3",3,-6,0,0],["A4",4,-9,0,0],
                             ["B1",1,20,0,0],["B2",2,23,0,0],["B3",3,26,0,0],["B4",4,29,0,0]])
[XY, XYZ].each { set ->
    def r = track(c1, set)
    check("apart use_z=" + set.use_z + ": links", r.links.sort(),
          ["A1<-", "A2<-A1", "A3<-A2", "A4<-A3", "B1<-", "B2<-B1", "B3<-B2", "B4<-B3"])
    invariants("apart use_z=" + set.use_z, c1, r.rows)
}
def r1 = track(c1, XY)
check("apart: run_id carried on every row", r1.rows.collect { it.run_id }.unique(), ["run0000aaaa"])
check("apart: series_id from the table", r1.rows.collect { it.series_id }.unique(), ["S_apart"])
check("apart: rows sorted by t, feature_id", r1.rows.collect { it.feature_id },
      r1.rows.sort(false) { a, b -> (a.t as int) <=> (b.t as int) ?: a.feature_id <=> b.feature_id }.collect { it.feature_id })

// -----------------------------------------------------------------------------
println "\n--- 2. a dividing object: two features sharing a prev_feature_id"
def c2 = writeCase("divide", [["M1",1,0,0,0],["M2",2,0,0,0],["D1",3,-4,0,0],["E1",3,4,0,0],["D2",4,-7,0,0],["E2",4,7,0,0]])
[XY, XYZ].each { set ->
    def r = track(c2, set)
    check("divide use_z=" + set.use_z + ": links", r.links.sort(),
          ["D1<-M2", "D2<-D1", "E1<-M2", "E2<-E1", "M1<-", "M2<-M1"])
    check("divide use_z=" + set.use_z + ": counted as a division", r.res.series[0].stats.n_divisions, 1)
    invariants("divide use_z=" + set.use_z, c2, r.rows)
}

// -----------------------------------------------------------------------------
println "\n--- 3. a division after the mother is lost for a frame: what TrackMate does"
// F is a bystander present in every frame, so that frame 3 EXISTS: a frame
// holding no feature at all is not a gap to TrackMate, it is not there.
def c3 = writeCase("divide_gap", [["M1",1,0,0,0],["M2",2,0,0,0],["D1",4,-4,0,0],["E1",4,4,0,0],["D2",5,-7,0,0],["E2",5,7,0,0],
                                  ["F1",1,60,0,0],["F2",2,60,0,0],["F3",3,60,0,0],["F4",4,60,0,0],["F5",5,60,0,0]])
def r3 = track(c3, XY)
def daughtersFromM = r3.links.findAll { it in ["D1<-M2", "E1<-M2"] }
println "    recorded: " + r3.links.findAll { it.startsWith("D1") || it.startsWith("E1") }
check("divide after a gap: NOT a split (one daughter gap-closed)", daughtersFromM.size(), 1)
check("divide after a gap: the other daughter starts a track", r3.links.count { it in ["D1<-", "E1<-"] }, 1)
check("divide after a gap: divisions counted", r3.res.series[0].stats.n_divisions, 0)
check("divide after a gap: gap-closed links counted", r3.res.series[0].stats.n_gap_closed, 1)
invariants("divide after a gap", c3, r3.rows)

// -----------------------------------------------------------------------------
println "\n--- 4. an object missing from one frame: one track, through a gap-closed link"
def c4 = writeCase("gap", [["A1",1,0,0,0],["A2",2,1,0,0],["A4",4,3,0,0],["A5",5,4,0,0],
                           ["F1",1,60,0,0],["F2",2,60,0,0],["F3",3,60,0,0],["F4",4,60,0,0],["F5",5,60,0,0]])
def r4 = track(c4, XY)
check("gap, max_frame_gap 2: A4 follows A2 across the missing frame", r4.links.findAll { it.startsWith("A") }.sort(),
      ["A1<-", "A2<-A1", "A4<-A2", "A5<-A4"])
check("gap: counted as gap-closed", r4.res.series[0].stats.n_gap_closed, 1)
invariants("gap", c4, r4.rows)
def r4b = track(c4, XY + [max_frame_gap: 1])
check("gap, max_frame_gap 1: bridges nothing, A4 starts again", r4b.links.findAll { it.startsWith("A") }.sort(),
      ["A1<-", "A2<-A1", "A4<-", "A5<-A4"])

// -----------------------------------------------------------------------------
println "\n--- 5. time points sampled sparsely (frames=1,10,20,...): steps, not frame numbers"
// Gap closing and splitting count frame NUMBERS inside TrackMate. Given the
// raw t, a miss at t=20 would be a 20-frame gap and a division between t=10
// and t=20 not a split at all. Ranked, both are what they are.
def c5 = writeCase("sparse", [["A1",1,0,0,0],["A10",10,1,0,0],["A30",30,3,0,0],["A40",40,4,0,0],
                              ["F1",1,60,0,0],["F10",10,60,0,0],["F20",20,60,0,0],["F30",30,60,0,0],["F40",40,60,0,0],
                              ["M1",1,-60,0,0],["M10",10,-60,0,0],["D20",20,-64,0,0],["E20",20,-56,0,0],
                              ["D30",30,-66,0,0],["E30",30,-54,0,0]])
def r5 = track(c5, XY)
check("sparse: a miss at t=20 gap-closed with max_frame_gap 2", r5.links.findAll { it.startsWith("A") }.sort(),
      ["A10<-A1", "A1<-", "A30<-A10", "A40<-A30"])
check("sparse: a division from t=10 to t=20 is a split", r5.links.findAll { it.startsWith("D20") || it.startsWith("E20") }.sort(),
      ["D20<-M10", "E20<-M10"])
// The control: the same spots given to TrackMate with the RAW t. If this also
// linked them, the ranking would be doing nothing and the two checks above
// would prove nothing about it.
def rawLinks = { List feats ->
    def sc = new fiji.plugin.trackmate.SpotCollection()
    feats.each { f -> sc.add(new fiji.plugin.trackmate.Spot(f.x as double, f.y as double, 0d, 1d, 1d, f.feature_id as String), f.t as Integer) }
    sc.setVisible(true)
    def tr = new fiji.plugin.trackmate.tracking.jaqaman.SparseLAPTrackerFactory().create(sc, FTC.trackerSettings(FTC.checkSettings([feature_type: "nucleus", use_z: "false"])))
    tr.setNumThreads(1); tr.process()
    def g = tr.getResult()
    g.edgeSet().collect { e -> [g.getEdgeSource(e).getName(), g.getEdgeTarget(e).getName()].collect { c5.names[it] }.sort().join("-") }.sort()
}
def raw5 = rawLinks(FT.readCentroids(c5.file, "nucleus", false).features)
println "    raw t, for comparison: " + raw5.findAll { !it.startsWith("F") }
check("control, raw t: the miss at t=20 is NOT gap-closed", raw5.contains("A10-A30"), false)
check("control, raw t: the division is NOT a split", ["D20-M10", "E20-M10"].count { raw5.contains(it) }, 1)
check("sparse: t written back as the time point", r5.rows.find { it.feature_id == c5.ids["A30"] }?.t, "30")
check("sparse: frames recorded", r5.res.series[0].frames, [1, 10, 20, 30, 40])
// A spot linked to nothing is not in TrackMate's graph, so it can be neither
// gap-closed nor split onto: a daughter seen in ONE frame stays a start row.
def c5b = writeCase("lone_daughter", [["M1",1,0,0,0],["M2",2,0,0,0],["D3",3,-4,0,0],["E3",3,4,0,0],["D4",4,-7,0,0]])
check("a daughter seen in one frame is not split onto", track(c5b, XY).links.findAll { it.startsWith("E3") }, ["E3<-"])
invariants("sparse", c5, r5.rows)

// -----------------------------------------------------------------------------
println "\n--- 6. two objects segmented as one for two frames, then two again (pronuclei)"
def c6 = writeCase("pronuclei", [["P1",1,-8,0,0],["Q1",1,8,0,0],["P2",2,-4,0,0],["Q2",2,4,0,0],
                                 ["U3",3,0,0,0],["U4",4,0,0,0],["P5",5,-4,0,0],["Q5",5,4,0,0],["P6",6,-8,0,0],["Q6",6,8,0,0]])
def r6on = track(c6, XY + [allow_merging: true])
check("merging on: the merged feature has two link rows", r6on.links.findAll { it.startsWith("U3") }.sort(), ["U3<-P2", "U3<-Q2"])
check("merging on: counted as a merge", r6on.res.series[0].stats.n_merges, 1)
check("merging on: and splits again after", r6on.links.findAll { it.startsWith("P5") || it.startsWith("Q5") }.sort(), ["P5<-U4", "Q5<-U4"])
invariants("merging on", c6, r6on.rows)
def r6off = track(c6, XY)
check("merging off: the merged feature has one predecessor", r6off.links.count { it.startsWith("U3<-") }, 1)
def ends = ["P2", "Q2"].findAll { n -> !r6off.links.any { it.endsWith("<-" + n) } }
println "    the track that ends: " + ends
check("merging off: one of the two tracks ends there", ends.size(), 1)
check("merging off: no merge counted", r6off.res.series[0].stats.n_merges, 0)
invariants("merging off", c6, r6off.rows)

// -----------------------------------------------------------------------------
println "\n--- 7. two stationary nuclei stacked in z (§5.4)"
def c7 = writeCase("stacked", [["L1",1,0,0,0],["L2",2,0,0,0],["L3",3,0,0,0],["H1",1,0,0,15],["H2",2,0,0,15],["H3",3,0,0,15]])
def r7z = track(c7, XYZ)
check("stacked, use_z=true: kept two tracks", r7z.links.sort(), ["H1<-", "H2<-H1", "H3<-H2", "L1<-", "L2<-L1", "L3<-L2"])
def r7xy = track(c7, XY)
println "    use_z=false recorded: " + r7xy.links.sort()
check("stacked, use_z=false: still four links", r7xy.res.series[0].stats.n_links, 4)
// Which of two identical-in-x-y nuclei follows which is a tie TrackMate breaks
// arbitrarily (printed above: here it crosses them). Pinning that tie-break
// would fail on an upgrade that changed nothing that matters; what matters is
// that each later feature still has exactly one predecessor.
check("stacked, use_z=false: one predecessor each, whichever it is",
      ["L2", "L3", "H2", "H3"].collect { n -> r7xy.links.count { it.startsWith(n + "<-") && it != n + "<-" } }, [1, 1, 1, 1])
invariants("stacked use_z=false", c7, r7xy.rows)

// -----------------------------------------------------------------------------
println "\n--- 8. a link must be strictly shorter than the maximum"
def c8 = writeCase("boundary", [["A1",1,0,0,0],["A2",2,10,0,0],["B1",1,100,0,0],["B2",2,110.0001,0,0]])
def r8 = track(c8, XY + [linking_max_distance: 10.0001d])
check("boundary: 10 < 10.0001 linked, 10.0001 not", r8.links.sort(), ["A1<-", "A2<-A1", "B1<-", "B2<-"])

// -----------------------------------------------------------------------------
println "\n--- 9. a single frame: nothing to link, every feature a start row"
def c9 = writeCase("single", [["A1",1,0,0,0],["B1",1,5,0,0]])
def logs9 = []
def r9 = track(c9, XY, logs9)
check("single frame: start rows only", r9.links.sort(), ["A1<-", "B1<-"])
check("single frame: and says so", logs9.any { it.contains("one frame: nothing to link") }, true)

// -----------------------------------------------------------------------------
println "\n--- 10. refusals"
check("use_z blank refused", errOf { FT.trackDirectory(c1.dir, [feature_type: "nucleus", use_z: ""]) }?.contains("useZ must be true"), true)
check("use_z 'yes' refused", errOf { FT.trackDirectory(c1.dir, [feature_type: "nucleus", use_z: "yes"]) }?.contains(">>>yes<<<"), true)
check("use_z absent refused", errOf { FT.trackDirectory(c1.dir, [feature_type: "nucleus"]) }?.contains("useZ must be true"), true)
check("feature_type blank refused", errOf { FT.trackDirectory(c1.dir, [feature_type: " ", use_z: "false"]) }?.contains("featureType is blank"), true)
def noType = errOf { FT.trackDirectory(c1.dir, [feature_type: "nuclues", use_z: "false"]) }
check("a type no table holds refused, naming those present", noType?.contains("types present: nucleus"), true)
check("max_frame_gap 0 refused", errOf { FT.trackDirectory(c1.dir, [feature_type: "nucleus", use_z: "false", max_frame_gap: 0]) }?.contains("max_frame_gap must be at least 1"), true)
check("distance 0 refused", errOf { FT.trackDirectory(c1.dir, [feature_type: "nucleus", use_z: "false", linking_max_distance: 0d]) }?.contains("positive distance"), true)
check("unknown setting refused", errOf { FT.trackDirectory(c1.dir, [feature_type: "nucleus", use_z: "false", max_gap: 3]) }?.contains("Unknown tracking setting"), true)
check("a directory with no centroid table refused", errOf { FT.trackDirectory(new File(tmp, "nothing").with { mkdirs(); it }, XY + [feature_type: "nucleus"]) }?.contains("No *_feature_centroids.tsv"), true)

def c10 = writeCase("blank_z", [["A1",1,0,0,null],["A2",2,1,0,null]])
def zErr = errOf { track(c10, XYZ) }
check("use_z=true on blank z refused", zErr?.contains("useZ=true, but z is blank for 2 of 2"), true)
check("...and nothing written", FTC.tracksFile(c10.file, "nucleus").exists(), false)
check("use_z=false on blank z works", track(c10, XY).links.sort(), ["A1<-", "A2<-A1"])

def c11 = writeCase("dup", [["A1",1,0,0,0],["A2",2,1,0,0]])
c11.file.append(["S_dup", "2", "nucleus", "nucleus_0002", "1", "0", "0", "3", "300", "120", "run0000aaaa", "fp00000001"].join("\t") + "\n")
check("a feature twice refused", errOf { track(c11, XY) }?.contains("nucleus_0002 appears twice"), true)
def c12 = writeCase("two_runs", [["A1",1,0,0,0],["A2",2,1,0,0]])
c12.file.setText(c12.file.text.replaceFirst("run0000aaaa\tfp00000001\n\$", "run0000bbbb\tfp00000001\n"))
check("two run_ids in one table refused", errOf { track(c12, XY) }?.contains("must share one run_id"), true)
def c13 = writeCase("two_series", [["A1",1,0,0,0],["A2",2,1,0,0]])
c13.file.setText(c13.file.text.replaceFirst("\nS_two_series\t2", "\nS_other\t2"))
check("two series in one table refused", errOf { track(c13, XY) }?.contains("must hold one series"), true)
def c14 = writeCase("zero_t", [["A0",0,0,0,0],["A1",1,1,0,0]])
check("t = 0 refused", errOf { track(c14, XY) }?.contains("t counts from 1"), true)

// A directory holding a bad table and a good one: nothing is written for either.
def mixed = new File(tmp, "mixed"); mixed.mkdirs()
def good = writeCase("mixed_good", [["A1",1,0,0,0],["A2",2,1,0,0]])
def bad = writeCase("mixed_bad", [["A1",1,0,0,null],["A2",2,1,0,null]])
[good.file, bad.file].each { new File(mixed, it.getName()).setText(it.text) }
check("one bad table stops the run", errOf { FT.trackDirectory(mixed, [feature_type: "nucleus", use_z: "true"]) } != null, true)
check("...before the good one is written", new File(mixed, "S_mixed_good_nucleus_tracks.tsv").exists(), false)

// -----------------------------------------------------------------------------
println "\n--- 11. several types in one table, and a prefixed stem"
def c15 = writeCase("types", [["A1",1,0,0,0],["A2",2,1,0,0],["n1",1,0,0,0,"nucleolus"],["n2",2,1,0,0,"nucleolus"]],
                    [stem: "pre_S_types", sid: "S_types"])
def r15 = track(c15, XY)
check("types: only nucleus features tracked", r15.rows.collect { it.feature_id }.every { it.startsWith("nucleus_") }, true)
check("types: outputs under the input's stem and the type", r15.file.getName(), "pre_S_types_nucleus_tracks.tsv")
check("types: series_id from the content, not the name", r15.rows.collect { it.series_id }.unique(), ["S_types"])
def r15b = FT.trackDirectory(c15.dir, [feature_type: "nucleolus", use_z: "false"])
check("types: the other type tracked beside it, not over it",
      [new File(c15.dir, "pre_S_types_nucleus_tracks.tsv").exists(), new File(c15.dir, "pre_S_types_nucleolus_tracks.tsv").exists()], [true, true])
def c16 = writeCase("no_type_here", [["A1",1,0,0,0]], [type: "nucleolus"])
new File(c16.dir, c1.file.getName()).setText(c1.file.text)
def logs16 = []
FT.trackDirectory(c16.dir, [feature_type: "nucleus", use_z: "false"]) { logs16 << it }
check("a series without the type gets an empty tracks table",
      TSV.write([], new File(tmp, "hdr.tsv"), FTC.TRACK_COLUMNS).text, new File(c16.dir, "S_no_type_here_nucleus_tracks.tsv").text)
check("...and says so", logs16.any { it.contains("no nucleus features") }, true)

// -----------------------------------------------------------------------------
println "\n--- 12. the edits table: seeded, reseeded while empty, kept once it holds edits"
def c17 = writeCase("edits", [["A1",1,0,0,0],["A2",2,1,0,0],["A3",3,2,0,0]])
def ef = FTC.editsFile(c17.file, "nucleus")
def res17 = FT.trackDirectory(c17.dir, [feature_type: "nucleus", use_z: "false"])
check("edits: seeded on the first run", res17.series[0].edits, "seeded")
check("edits: the seed holds no edit rows", TSV.read(ef).size(), 0)
check("edits: its header", ef.readLines().find { !it.startsWith("#") }, FTC.EDIT_COLUMNS.join("\t"))
def tmpl = ef.readLines().last()
check("edits: a template line carrying the fingerprint", tmpl.split("\t")[3], "fp00000001")
// A re-annotation regroups ROIs: new fingerprint. An empty edits file follows it.
c17.file.setText(c17.file.text.replace("fp00000001", "fp00000002"))
def res17b = FT.trackDirectory(c17.dir, [feature_type: "nucleus", use_z: "false"])
check("edits: an empty one is rewritten, not refused", res17b.series[0].edits, "reseeded")
check("edits: ...with the new fingerprint", ef.readLines().last().split("\t")[3], "fp00000002")
// Someone uncomments the template and fills it in.
ef.setText(ef.text.replaceFirst(/(?m)^# (link\t.*)$/, '$1'))
check("edits: the uncommented template reads as one edit", TSV.read(ef).size(), 1)
def before = ef.text
def res17c = FT.trackDirectory(c17.dir, [feature_type: "nucleus", use_z: "false"])
check("edits: kept once it holds an edit", res17c.series[0].edits, "kept")
check("edits: ...byte for byte", ef.text == before, true)
c17.file.setText(c17.file.text.replace("fp00000002", "fp00000003"))
def logs17 = []
def res17d = FT.trackDirectory(c17.dir, [feature_type: "nucleus", use_z: "false"]) { logs17 << it }
check("edits: made on another annotation, kept and flagged", res17d.series[0].edits, "kept-stale")
check("edits: ...with a warning naming both fingerprints", logs17.any { it.contains("WARNING") && it.contains("fp00000002") && it.contains("fp00000003") }, true)
check("edits: ...and still untouched", ef.text == before, true)

// -----------------------------------------------------------------------------
println "\n--- 12b. an edits file a person has damaged is never rewritten (code review, PR 2)"
// Each of these was reseeded -- the edit deleted, the log saying "reseeded" --
// before the fix: the old test was "does it parse into rows", and a damaged
// file does not, though it still holds the edits.
def damaged = { String label, String text, String want ->
    def cs = writeCase("edits_" + label, [["A1",1,0,0,0],["A2",2,1,0,0]])
    def e = FTC.editsFile(cs.file, "nucleus")
    e.text = text
    def lg = []
    def res = FT.trackDirectory(cs.dir, [feature_type: "nucleus", use_z: "false"]) { lg << it }
    check("damaged edits, " + label + ": status", res.series[0].edits, want)
    if (want.startsWith("kept")) check("damaged edits, " + label + ": byte for byte", e.text, text)
    return lg
}
def H = FTC.EDIT_COLUMNS.join("\t")
def lgDup = damaged("duplicated column",
    H + "\tnote\nlink\tnucleus_0001\tnucleus_0002\tfp00000001\tmine\tx\n", "kept-unreadable")
check("damaged edits, duplicated column: warned", lgDup.any { it.contains("WARNING") && it.contains("cannot be read as an edits table") }, true)
def lgHdr = damaged("header deleted",
    "# my notes\nlink\tnucleus_0001\tnucleus_0002\tfp00000001\tmine\n", "kept-unreadable")
check("damaged edits, header deleted: warned, naming the header", lgHdr.any { it.contains("no header naming") }, true)
damaged("header with an extra column, no rows", H + "\twho\n", "kept")
damaged("header alone, no comments", H + "\n", "reseeded")
damaged("comments alone", "# nothing yet\n\n", "reseeded")

println "\n--- 12c. what the directory listing takes for a centroid table (code review, PR 2)"
// macOS writes ._<name> beside a file on a non-Mac volume once an app touches
// it. Before the fix one was tracked as a series with no features, and three
// hidden ._ output files were written beside the real ones.
def c19 = writeCase("appledouble", [["A1",1,0,0,0],["A2",2,1,0,0]])
def ad = new byte[4096]; ad[1] = 5; ad[2] = 22; ad[3] = 7; ad[100] = 10   // a newline, so it has "rows"
new File(c19.dir, "._" + c19.file.getName()).bytes = ad
def logs19 = []
def r19 = FT.trackDirectory(c19.dir, [feature_type: "nucleus", use_z: "false"]) { logs19 << it }
check("AppleDouble: one series tracked, not two", r19.series.size(), 1)
check("AppleDouble: no ._ output written", c19.dir.list().findAll { it.startsWith("._") && it != "._" + c19.file.getName() }, [])
def c20 = writeCase("wrong_header", [["A1",1,0,0,0],["A2",2,1,0,0]])
new File(c20.dir, "Z_feature_centroids.tsv").text = "foo\tbar\n"
check("a header-only table of the wrong shape is refused, not taken as empty",
      errOf { FT.trackDirectory(c20.dir, [feature_type: "nucleus", use_z: "false"]) }?.contains("Z_feature_centroids.tsv lacks column(s)"), true)
def c21 = writeCase("empty_series", [["A1",1,0,0,0],["A2",2,1,0,0]])
new File(c21.dir, "E_feature_centroids.tsv").text = CEN_COLS.join("\t") + "\n"
def logs21 = []
FT.trackDirectory(c21.dir, [feature_type: "nucleus", use_z: "false"]) { logs21 << it }
check("a real centroid table with no rows: an empty tracks table", new File(c21.dir, "E_nucleus_tracks.tsv").readLines(), [FTC.TRACK_COLUMNS.join("\t")])
check("...logged under its file's name, not 'null'", [logs21.any { it.startsWith("  E: ") }, logs21.any { it.contains("null") }], [true, false])
check("a setting passed blank is refused, not defaulted",
      errOf { FT.trackDirectory(c1.dir, [feature_type: "nucleus", use_z: "false", linking_max_distance: null]) }?.contains("left blank: linking_max_distance"), true)

// -----------------------------------------------------------------------------
println "\n--- 13. the same input twice gives the same bytes; the parameters are recorded"
def a = r1.file.text
track(c1, XY)
check("deterministic: tracks identical across runs", r1.file.text == a, true)
def pf = FTC.paramsFile(c5.file, "nucleus")
track(c5, XY)
def params = TSV.read(pf).collectEntries { [it.parameter, it.value] }
check("params: use_z", params.use_z, "false")
check("params: frames", params.frames, "1,10,20,30,40")
check("params: max_frame_gap", params.max_frame_gap, "2")
check("params: allow_merging", params.allow_merging, "false")
check("params: repo_version from VERSION", params.repo_version, new File("VERSION").text.trim())
check("params: trackmate_version", params.trackmate_version, "7.14.0")
check("params: run_id and fingerprint", [params.run_id, params.fingerprint], ["run0000aaaa", "fp00000001"])

// -----------------------------------------------------------------------------
println "\n--- 14. the front end: Make_FeatureTracks.groovy, #@ lines stripped, driven by a Binding"
def SCRIPT = new File(LIBDIR, "Make_FeatureTracks.groovy").getAbsolutePath()
def src = new File(SCRIPT).text.readLines().findAll { !it.trim().startsWith("#@") }.join("\n")
check("front end compiles", errOf { new GroovyShell(this.class.classLoader).parse(src) }, null)
def decl = new File(SCRIPT).readLines().findAll { it.startsWith("#@") && !it.contains("visibility=MESSAGE") }
check("front end: every parameter persist=false", decl.count { !it.contains("persist=false") }, 0)
check("front end: every parameter has a value= (or it hangs headless)", decl.count { !it.contains("value=") && !it.contains("style=\"directory\"") }, 0)
def c18 = writeCase("front", [["A1",1,0,0,0],["A2",2,1,0,0]])
def b = new Binding(["javax.script.filename": SCRIPT, inputDir: c18.dir, featureType: "nucleus", useZ: "false",
                     linkingMaxDistance: 15.0d, maxFrameGap: 2, gapClosingMaxDistance: 15.0d,
                     splittingMaxDistance: 15.0d, allowMerging: false, mergingMaxDistance: 15.0d])
def feErr = errOf { new GroovyShell(this.class.classLoader, b).evaluate(src) }
check("front end runs", feErr, null)
check("front end: wrote the tracks", TSV.read(FTC.tracksFile(c18.file, "nucleus")).size(), 2)
def b2 = new Binding(b.getVariables() + [useZ: ""])
check("front end: a blank useZ stops it", errOf { new GroovyShell(this.class.classLoader, b2).evaluate(src) }?.contains("useZ must be true"), true)

println String.format("%nTest_FeatureTracks: %d passed, %d FAILED", passed, failed)
} catch (Throwable t) {
    println "  FAILED with an exception -- the counts above are incomplete"
    t.printStackTrace(System.out)
} finally {
    tmp.deleteDir()
    println "DONE"
}
