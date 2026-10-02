// Test_LuxendoScan.groovy
//
// A synthetic Luxendo tree -> the series table + the sources table. Run
// headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_LuxendoScan.groovy
//
// No data is needed; the tree is built by LuxFixture.

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "LuxendoScan.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def gcl = new GroovyClassLoader()
def LS  = gcl.parseClass(new File(LIBDIR + "/LuxendoScan.groovy"))
def FIX = gcl.parseClass(new File("tests/groovy/LuxFixture.groovy"))
def scanner = LS.load(LIBDIR)

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-54s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def throwsWith = { String what, String fragment, Closure body ->
    String msg = null
    try { body() } catch (Throwable e) { msg = e.getMessage() }
    boolean ok = msg != null && msg.contains(fragment)
    println String.format("  %-6s %-54s threw=%s", ok ? "ok" : "FAILED", what,
                          msg ? msg.readLines()[0] : "(nothing)")
    ok ? passed++ : failed++
}

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_luxendoscan_" + System.nanoTime())
tmp.mkdirs()

// Two positions with DIFFERENT z, as the real data has, and three channels of
// which one is unnamed -- also as the real data has.
def positions = [[stack: 0, desc: "L26A pos1", nz: 3],
                 [stack: 1, desc: "fucci pos2", nz: 2]]
def channels  = [[index: 0, name: "BF"], [index: 1, name: "GFP"], [index: 2, name: null]]
def root = FIX.buildTree(new File(tmp, "acq"), positions, channels, 2)

println "=== the tree scans into TWO tables ==="
def logLines = []
// The WALK, explicitly: the first sections pin its own behaviour (pairing, the
// skipped index file), and the index route became the default -- it is
// compared against this further down.
def res  = scanner.scan(root, [listing: "walk"]) { logLines << it }
def rows = res.sources
def ser  = res.series
check("sources = positions x channels x t", rows.size(), 2 * 3 * 2)
// ONE SERIES PER STACK, every time point inside -- not one per (stack, t).
check("series = positions",                 ser.size(), 2)
check("every source names its series by key",
      rows.every { it.alias == "acq" && it.series_index != null }, true)

println "\n=== the sources table carries ONLY declared columns ==="
// The scan uses a few underscore-prefixed workings to build the series table.
// They must not survive into the table, or Tsv.write drops them silently and
// the schema stops being the contract.
check("no leaked workings", rows[0].keySet().findAll { it.startsWith("_") }.toList(), [])
check("sources columns",
      rows[0].keySet().toList().sort(),
      ["alias", "channel", "channel_name", "pixel_depth", "pixel_height", "pixel_unit", "pixel_width",
       "series_index", "size_x", "size_y", "size_z", "source_bytes", "source_path", "t"])
// NOT series_id: it is editable, and a join on it would orphan every source
// the moment somebody made an id readable.
check("no series_id on a source", rows[0].containsKey("series_id"), false)

println "\n=== the link-only index file is skipped, and says so ==="
// main_raw.lux.h5 has no .json sidecar, which is the whole test -- no HDF5 is
// opened to find out.
check("index not scanned as data", rows.any { it.source_path.contains("main_raw") }, false)
check("skip is reported",
      logLines.any { it.contains("main_raw.lux.h5") && it.contains("no .json sidecar") }, true)

println "\n=== a stray .json with no image is ignored, and reported ==="
// Somebody else's analysis output in the same tree must not invent a row.
FIX.writeSidecar(new File(root, "raw/notes.json"),
                 [stack: "9", channel: "9", time_point: "9",
                  image_size_vx: [width: 1, height: 1, depth: 1]])
def strayLog = []
def strayRes = scanner.scan(root, [listing: "walk"]) { strayLog << it }
check("no row for the stray json", strayRes.sources.size(), rows.size())
check("and it is reported",        strayLog.any { it.contains("no .lux.h5 beside it") }, true)
new File(root, "raw/notes.json").delete()

println "\n=== identity comes from the sidecar, not the path ==="
def r0 = rows.find { it.series_index == 0 && it.t == 0 && it.channel == 1 }
check("channel_name", r0.channel_name, "GFP")
check("unnamed channel stays blank", rows.find { it.channel == 2 }.channel_name, "")
def s0 = ser.find { it.series_index == 0 }
check("series_id",                          s0.series_id, "acq_s0000_L26A_pos1")
check("series_name is the raw description", s0.series_name, "L26A pos1")
check("series_index is the stack number",   s0.series_index, 0)

println "\n=== dimensions and calibration per position ==="
check("L26A z", rows.find { it.series_index == 0 }.size_z, 3)
check("fucci z", rows.find { it.series_index == 1 }.size_z, 2)
check("pixel_unit set", r0.pixel_unit, "micron")
check("size_x", r0.size_x, 4)
check("size_y", r0.size_y, 6)

println "\n=== the series table is the one every format writes ==="
def SCHEMA = gcl.parseClass(new File(LIBDIR + "/SheetSchema.groovy")).loadFromLibDir(LIBDIR)
check("every required series column present",
      SCHEMA.required("series") - ser[0].keySet().toList(), [])
check("no undeclared series column",
      ser[0].keySet().toList() - SCHEMA.columns("series"), [])
check("every required sources column present",
      SCHEMA.required("manifest") - rows[0].keySet().toList(), [])
check("include seeded true",      s0.include, "true")
check("alias is the acquisition", s0.alias, "acq")

println "\n=== the alias is the OPERATOR'S, and it is part of the series id ==="
// Without it, `s<NNNN>_<stack_description>` is unique only WITHIN one
// acquisition. Measured on two real acquisitions: all 14 stack identities were
// identical, so both produced s0000_L26A_pos1 and, run separately into one
// output directory, would overwrite each other's results.
def named = scanner.scan(root, [alias: "fucci_rep2"]) { }
check("the given alias leads the series id",
      named.series.every { it.series_id.startsWith("fucci_rep2_s") }, true)
check("and is recorded on the series row", named.series[0].alias, "fucci_rep2")
check("and on every source, as the join key", named.sources.every { it.alias == "fucci_rep2" }, true)
check("blank falls back to the folder name",
      ser.every { it.series_id.startsWith("acq_s") }, true)
// THE COLLISION THIS FIXES, shown rather than asserted: the same tree scanned
// under two aliases shares no series id at all, where before it shared all of
// them.
check("two aliases share no series id",
      (named.series.collect { it.series_id }.toSet()
       .intersect(ser.collect { it.series_id }.toSet())).toList(), [])
check("but the same number of them", named.series.size(), ser.size())
// And it is the repo's own composer, not a second copy of the rule.
def SSS = gcl.parseClass(new File(LIBDIR + "/SampleSheet.groovy")).load(LIBDIR)
check("the id rule is SampleSheet.composePrefix",
      SSS.composePrefix("fucci_rep2", 0, "L26A pos1"),
      named.series.find { it.series_index == 0 }.series_id)
// Whitespace in a stack description must not survive into an id: the outline
// table is tab-separated and `name` holds this string.
check("spaces collapse, as sanitize() requires",
      named.series.every { !it.series_id.contains(" ") }, true)
// Whitespace-only is BLANK, so it means "use the folder name" -- the documented
// behaviour, not an error. (The empty-alias guard in the scanner covers the
// remaining case of a folder name that itself sanitises away; there is no
// portable way to create one, so it is belt and braces rather than tested.)
check("a whitespace alias is blank, not an error",
      scanner.scan(root, [alias: "   "]) { }.series.every { it.series_id.startsWith("acq_s") },
      true)
// Spaces inside an alias collapse rather than reaching the id.
check("an alias with a space is collapsed",
      scanner.scan(root, [alias: "rep 2"]) { }.series[0].series_id.startsWith("rep_2_s"), true)
// path is the ACQUISITION DIRECTORY here, not a file -- the root source_path
// resolves against.
check("path is the directory",       new File(s0.path).isDirectory(), true)
check("size_c is the channel count", s0.size_c, 3)
check("size_t counts the frames",    s0.size_t, 2)
check("pixel_type",                  s0.pixel_type, "uint16")
check("file_size sums the sources",
      s0.file_size,
      rows.findAll { it.series_index == s0.series_index }.sum { it.source_bytes as long })

println "\n=== sources order is series, then time, then channel ==="
// A sources table is read and diffed, so the order has to be stable. A closure
// returning a List is NOT a multi-key sort in Groovy -- it silently yields an
// arbitrary order -- which is what this pins.
def keys = rows.collect { it.alias + "|" + String.format("%04d", it.series_index as int) + "|" +
                          String.format("%04d", it.t as int) + "|" + it.channel }
check("already in sorted order", keys, keys.sort(false))
check("first row is series 0, t 0, channel 0",
      "s" + rows[0].series_index + "/t" + rows[0].t + "/c" + rows[0].channel, "s0/t0/c0")
// and it is reproducible run to run
def rows2 = scanner.scan(root) { }.sources
check("second scan gives the same order",
      rows2.collect { it.source_path }, rows.collect { it.source_path })

println "\n=== gatherFrames is gone, and asking for it is refused ==="
// A caller that asked for per-time-point series must not quietly get one
// series per stack and misread every id that follows.
throwsWith("gatherFrames=false is refused", "gatherFrames was removed",
           { scanner.scan(root, [gatherFrames: false]) { } })
throwsWith("so is gatherFrames=true",       "gatherFrames was removed",
           { scanner.scan(root, [gatherFrames: true]) { } })

println "\n=== the one join: sources to series on (alias, series_index) ==="
def joined = LS.withSeriesId(rows, ser)
check("every source gets its series_id",
      joined.sources.every { it.series_id in ser.collect { s -> s.series_id } }, true)
check("the right one",
      joined.sources.find { it.series_index == 1 }.series_id, "acq_s0001_fucci_pos2")
check("the input rows are not modified", rows[0].containsKey("series_id"), false)
check("nothing unsourced",               joined.unsourced, [])
// THE POINT OF THE KEY: a hand-edited series_id still finds its sources, and
// names them -- an edit for readability must not orphan anything.
def edited = ser.collect { new LinkedHashMap(it) }
edited.find { it.series_index == 0 }.series_id = "my_embryo_A"
def joinedEd = LS.withSeriesId(rows, edited)
check("an edited series_id still joins",
      joinedEd.sources.findAll { it.series_index == 0 }.collect { it.series_id }.unique(), ["my_embryo_A"])
check("validate accepts the edit", scanner.validate(edited, rows) { }, [])
// What a TSV read gives: everything a string. The key must still meet.
def asText = { List<Map> rs -> rs.collect { r -> r.collectEntries { k, v -> [k, v == null ? "" : v.toString()] } } }
check("strings from a TSV still join",
      LS.withSeriesId(asText(rows), asText(ser)).sources.size(), rows.size())
// Stack 0 is the trap: Groovy reads 0 as false.
check("stack 0 is a key like any other",
      LS.seriesKey([alias: "a", series_index: 0]), ["a", "0"])
// A source whose series row is gone is FATAL, named.
throwsWith("a source with no series row stops the join", "have no series row",
           { LS.withSeriesId(rows, ser.findAll { it.series_index != 1 }) })
// A series with no sources is returned for the caller to judge.
check("a series with no sources is reported",
      LS.withSeriesId(rows.findAll { it.series_index != 1 }, ser).unsourced, ["acq_s0001_fucci_pos2"])
// Two series rows on one key would make the join ambiguous.
throwsWith("two series rows on one key refuse", "share alias",
           { LS.withSeriesId(rows, ser + [new LinkedHashMap(ser[0]) + [series_id: "dup"]]) })

println "\n=== single-plane positions carry no z step ==="
def flatRoot = FIX.buildTree(new File(tmp, "flat"), [[stack: 0, desc: "pos1", nz: 1]],
                             [[index: 0, name: "BF"]], 1)
def flatRes = scanner.scan(flatRoot) { }
check("size_z",      flatRes.sources[0].size_z, 1)
check("pixel_depth", flatRes.sources[0].pixel_depth, null)
check("and the series row too", flatRes.series[0].pixel_depth, null)

println "\n=== what it refuses ==="
def bare = new File(tmp, "bare"); bare.mkdirs()
throwsWith("a directory with no .lux.h5", "No paired .lux.h5", { scanner.scan(bare) { } })
throwsWith("a missing directory",         "Not a directory",        { scanner.scan(new File(tmp, "nope")) { } })

def onlyIndex = new File(tmp, "onlyindex"); onlyIndex.mkdirs()
FIX.writeIndex(new File(onlyIndex, "main_raw.lux.h5"))
throwsWith("only index files", "No paired .lux.h5", { scanner.scan(onlyIndex) { } })

// An image whose sidecar cannot place it must STOP the scan, not be dropped.
// Dropping it would silently shrink the dataset.
def noPlace = new File(tmp, "noplace"); noPlace.mkdirs()
FIX.writeLux(new File(noPlace, "Cam_long_00000.lux.h5"), 2, 4, 4, [sidecar: false])
FIX.writeSidecar(FIX.sidecarPath(new File(noPlace, "Cam_long_00000.lux.h5")),
                 [stack: "0", channel: "0", image_size_vx: [width: 4, height: 4, depth: 2]])
throwsWith("sidecar cannot place the file", "refusing to guess from the path",
           { scanner.scan(noPlace) { } })

// Dimensions are not optional either: without them the assembler cannot size
// anything, and the predict-before-writing warning stops working.
def noSize = new File(tmp, "nosize"); noSize.mkdirs()
FIX.writeLux(new File(noSize, "Cam_long_00000.lux.h5"), 2, 4, 4, [sidecar: false])
FIX.writeSidecar(FIX.sidecarPath(new File(noSize, "Cam_long_00000.lux.h5")),
                 [stack: "0", channel: "0", time_point: "0"])
throwsWith("sidecar with no dimensions", "the dimensions are not optional",
           { scanner.scan(noSize) { } })

// An unpaired image is skipped and SAID, not dropped quietly.
def unpaired = new File(tmp, "unpaired"); unpaired.mkdirs()
FIX.writeLux(new File(unpaired, "ok/Cam_long_00000.lux.h5"), 2, 4, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(unpaired, "ok/Cam_long_00001.lux.h5"), 2, 4, 4,
             [stack: 0, channel: 0, tp: 1, sidecar: false])
def upLog = []
def upRes = scanner.scan(unpaired) { upLog << it }
check("the unpaired image is not a row", upRes.sources.size(), 1)
check("and the skip is reported",
      upLog.any { it.contains("Cam_long_00001") && it.contains("no .json sidecar") }, true)

println "\n=== two sources claiming one output channel is fatal ==="
// Same stack, channel and time point in two different directories: in a stack
// they would overwrite each other silently.
def clash = new File(tmp, "clash"); clash.mkdirs()
FIX.writeLux(new File(clash, "a/Cam_long_00000.lux.h5"), 2, 4, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(clash, "b/Cam_long_00000.lux.h5"), 2, 4, 4, [stack: 0, channel: 0, tp: 0])
throwsWith("duplicate series channel", "Two sources claim the same series channel",
           { scanner.scan(clash) { } })

println "\n=== channels of one output must agree on dimensions ==="
def ragged = new File(tmp, "ragged"); ragged.mkdirs()
FIX.writeLux(new File(ragged, "c0/Cam_long_00000.lux.h5"), 3, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(ragged, "c1/Cam_long_00000.lux.h5"), 2, 6, 4, [stack: 0, channel: 1, tp: 0])
throwsWith("channels disagree on z", "disagree on dimensions",
           { scanner.scan(ragged) { } })

println "\n=== one series holds every frame, and that is not a duplicate ==="
// One series legitimately holds channel 0 once per frame, so the duplicate
// check has to key on the time point or it would reject every real table.
check("t is on every source",            rows.collect { it.t }.unique().sort(), [0, 1])
check("frames are not mistaken for duplicate sources",
      scanner.validate(ser, rows) { }.findAll { it.contains("same series channel") }, [])

println "\n=== quickScan reads one sidecar per directory, and agrees with the slow path ==="
def slow = scanner.scan(root, [quickScan: false, listing: "walk"]) { }
check("same sources either way", slow.sources, rows)
check("same series either way",  slow.series, ser)
// A time point of a different depth must NOT be assumed away: its file size
// stands out, so it is read in full -- and since one series has one volume
// shape, the scan then stops. Without the size check, quickScan would report
// the sampled file's z for every frame and the series would look whole.
def trunc = new File(tmp, "trunc"); trunc.mkdirs()
FIX.writeLux(new File(trunc, "c0/Cam_long_00000.lux.h5"), 8, 16, 16, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(trunc, "c0/Cam_long_00001.lux.h5"), 2, 16, 16, [stack: 0, channel: 0, tp: 1])
def tLog = []
def tErr = null
try { scanner.scan(trunc, [quickScan: true]) { tLog << it } } catch (Throwable e) { tErr = e.getMessage() }
check("the odd frame is read in full, and logged",
      tLog.any { it.contains("size differs from its siblings") }, true)
check("so the series' two depths are SEEN, and refused",
      tErr != null && tErr.contains("disagree on dimensions") && tErr.contains("16x16x8") && tErr.contains("16x16x2"), true)
def tSlowErr = null
try { scanner.scan(trunc, [quickScan: false]) { } } catch (Throwable e) { tSlowErr = e.getMessage() }
check("quickScan agrees with the slow path here too", tErr, tSlowErr)

// The time point is the ONE fact the sampled sidecar cannot supply -- it is the
// axis the directory runs along -- so quickScan takes it from the filename, and
// only after confirming the mapping on the sampled file. Reusing the sampled
// sidecar's time_point instead gave every file t=0, which the duplicate-source
// check then (correctly) rejected; this pins the fix.
def tpDir = new File(tmp, "tp"); tpDir.mkdirs()
(0..3).each { int t ->
    FIX.writeLux(new File(tpDir, "c0/Cam_long_" + String.format("%05d", t) + ".lux.h5"),
                 2, 6, 4, [stack: 0, channel: 0, tp: t])
}
def tpRes = scanner.scan(tpDir, [quickScan: true]) { }
check("every time point survives quickScan",
      tpRes.sources.collect { it.t }.sort(), [0, 1, 2, 3])
check("in one series of four frames", tpRes.series.collect { it.size_t }, [4])
check("and it matches the slow path",
      scanner.scan(tpDir, [quickScan: false]) { }.sources, tpRes.sources)

// A directory whose filenames do NOT encode the time point falls back to
// reading every sidecar, and says so -- rather than trusting the name.
def oddNames = new File(tmp, "oddnames"); oddNames.mkdirs()
FIX.writeLux(new File(oddNames, "c0/first.lux.h5"),  2, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(oddNames, "c0/second.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 1])
def onLog = []
def onRes = scanner.scan(oddNames, [quickScan: true]) { onLog << it }
check("both time points still read", onRes.sources.collect { it.t }.sort(), [0, 1])
check("and the fallback is reported",
      onLog.any { it.contains("does not encode the time point") }, true)

println "\n=== what stops the scan, and what only warns ==="
// z changing BETWEEN time points of one stack: one series has one volume
// shape, so there is nothing to build -- fatal, naming both shapes.
def wobble = new File(tmp, "wobble"); wobble.mkdirs()
FIX.writeLux(new File(wobble, "t0/Cam_long_00000.lux.h5"), 3, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(wobble, "t1/Cam_long_00001.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 1])
throwsWith("z changing between time points is fatal", "disagree on dimensions",
           { scanner.scan(wobble) { } })

// A gap in the time points means a file is missing: worth knowing, a person's
// call.
def gap = new File(tmp, "gap"); gap.mkdirs()
FIX.writeLux(new File(gap, "t0/Cam_long_00000.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(gap, "t2/Cam_long_00002.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 2])
def gLog = []
scanner.scan(gap) { gLog << it }
check("time point gap warns", gLog.any { it.contains("not a run from 0") }, true)

println "\n=== bySeries groups one series' sources together ==="
def grouped = LS.bySeries(rows)
check("group count", grouped.size(), 2)
check("keyed by (alias, series_index)", grouped.keySet().toList(), [["acq", "0"], ["acq", "1"]])
check("channels x frames per group", grouped.values().collect { it.size() }.unique(), [6])
check("channels in index order, frame by frame",
      grouped[["acq", "0"]].collect { it.channel }, [0, 1, 2, 0, 1, 2])

println "\n=== the two tables must agree on (alias, series_index) ==="
// A join that matches nothing is the failure this repo is built around, so the
// scan asserts it where the tables are made. An edited series_id is NOT such a
// failure (shown above); a series row that names another stack is.
def badSeries = ser.collect { new LinkedHashMap(it) }
badSeries[0].series_index = 99
throwsWith("an orphaned source is fatal", "do not agree on (alias, series_index)",
           { scanner.validate(badSeries, rows) { } })

println "\n=== the file list comes from bdv.h5 when it is there ==="
// Luxendo writes bdv.h5 + bdv.xml at the end of an acquisition; LuxFixture's
// tree has them. Over samba the walk is minutes and the index ~1 s, so the
// default takes the index -- and must then build EXACTLY the walk's tables.
def autoRes = scanner.scan(root) { }
check("auto took the index",           autoRes.listing, "index")
check("walk says it walked",           res.listing, "walk")
def viaIndex = scanner.scan(root, [listing: "index"]) { }
def viaWalk  = scanner.scan(root, [listing: "walk"]) { }
check("index == walk, sources", viaIndex.sources, viaWalk.sources)
check("index == walk, series",  viaIndex.series,  viaWalk.series)
check("and the index sees every file",  autoRes.sources.size(), rows.size())
def bothLog = []
scanner.scan(root, [listing: "walk"]) { bothLog << it }
check("walking beside an index compares the two",
      bothLog.any { it.contains("list the same files") }, true)
throwsWith("a typo in listing refuses", "listing must be one of", { LS.checkListing("idx") })

// No index: auto walks, and says why; index refuses rather than walking anyway.
def plain = FIX.buildTree(new File(tmp, "plain"), positions, channels, 2, 6, 4, false)
def plainLog = []
check("no index -> auto walks", scanner.scan(plain) { plainLog << it }.listing, "walk")
check("and says there was no index", plainLog.any { it.contains("no bdv.h5 + bdv.xml index here") }, true)
throwsWith("listing=index with no index", "there is no bdv.h5", { scanner.scan(plain, [listing: "index"]) { } })

println "\n=== <tile> is not the stack number, and nothing reads it ==="
// Luxendo numbers setups in TEXT order of the stack, so with stacks 0, 2 and 10
// the tiles are 0 (st:0), 1 (st:10), 2 (st:2). Shown on the fixture first, so
// this cannot pass because the fixture never set the trap.
def tilePos = [[stack: 0, desc: "a", nz: 2], [stack: 2, desc: "b", nz: 2], [stack: 10, desc: "c", nz: 2]]
def tileChs = [[index: 0, name: "BF"]]
def spec10  = FIX.bdvSpec(tilePos, tileChs, 1)
check("fixture: st:10 sits at tile 1", spec10.setups.find { it.name.contains("_st:10_") }.tile, 1)
check("fixture: st:2 sits at tile 2",  spec10.setups.find { it.name.contains("_st:2_") }.tile, 2)
def tileRoot = FIX.buildTree(new File(tmp, "tile"), tilePos, tileChs, 1)
def tileRes  = scanner.scan(tileRoot, [listing: "index"]) { }
check("stack 10 keeps its own number",
      tileRes.series.find { it.series_name == "c" }.series_index, 10)
check("and stack 2 its own",
      tileRes.series.find { it.series_name == "b" }.series_index, 2)

println "\n=== the index decides the file list, so a stale one is caught ==="
// "Same tables as the walk" could also mean "the index route never ran". These
// prove it ran: the index alone decides which files are rows.
def staleRoot = FIX.buildTree(new File(tmp, "stale"), positions, channels, 2)
def full = FIX.bdvSpec(positions, channels, 2)
def dropped = full.links[-1]
FIX.writeBdv(staleRoot, full.setups, full.links.findAll { !it.is(dropped) })
def stLog = []
def stIdx = scanner.scan(staleRoot, [listing: "index"]) { stLog << it }
check("a file the index omits is not a row",
      stIdx.sources.size(), rows.size() - 1)
check("namely that one", stIdx.sources.any { it.source_path == dropped.target }, false)
def stWalkLog = []
def stWalk = scanner.scan(staleRoot, [listing: "walk"]) { stWalkLog << it }
check("the walk still finds it",           stWalk.sources.size(), rows.size())
check("and warns that the index omits it", stWalk.warnings.any { it.contains("does not list") }, true)

// The other way round: the index lists a file that is not on disk.
FIX.writeBdv(staleRoot, full.setups, full.links)
new File(staleRoot, dropped.target as String).delete()
throwsWith("an indexed file that is gone stops the scan", "not on disk",
           { scanner.scan(staleRoot, [listing: "index"]) { } })

println "\n=== the index and the sidecars must agree on what each file is ==="
def lieRoot = FIX.buildTree(new File(tmp, "lie"), positions, channels, 2)
def lie = FIX.bdvSpec(positions, channels, 2)
lie.setups[0].nz = 7
FIX.writeBdv(lieRoot, lie.setups, lie.links)
throwsWith("index says 7 slices, sidecar says 3", "disagree on",
           { scanner.scan(lieRoot, [listing: "index"]) { } })
// A link pointing at another time point's file: the sidecar says t=1, the
// index says t=0.
def swap = FIX.bdvSpec(positions, channels, 2)
def l0 = swap.links.find { it.t == 0 }, l1 = swap.links.find { it.setup == l0.setup && it.t == 1 }
def tgt = l0.target; l0.target = l1.target; l1.target = tgt
FIX.writeBdv(lieRoot, swap.setups, swap.links)
throwsWith("index and sidecar disagree on the time point", "disagree on",
           { scanner.scan(lieRoot, [listing: "index", quickScan: false]) { } })
check("the walk does not consult it for identity",
      scanner.scan(lieRoot, [listing: "walk"]) { }.sources.size(), rows.size())

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
