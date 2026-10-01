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
def res  = scanner.scan(root) { logLines << it }
def rows = res.sources
def ser  = res.series
check("sources = positions x channels x t", rows.size(), 2 * 3 * 2)
check("series = positions x t",             ser.size(), 4)
check("every source names a series",        rows.every { it.series_id }, true)

println "\n=== the sources table carries ONLY declared columns ==="
// The scan uses a few underscore-prefixed workings to build the series table.
// They must not survive into the table, or Tsv.write drops them silently and
// the schema stops being the contract.
check("no leaked workings", rows[0].keySet().findAll { it.startsWith("_") }.toList(), [])
check("sources columns",
      rows[0].keySet().toList().sort(),
      ["channel", "channel_name", "pixel_depth", "pixel_height", "pixel_unit", "pixel_width",
       "series_id", "size_x", "size_y", "size_z", "source_bytes", "source_path", "t"])

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
def strayRes = scanner.scan(root) { strayLog << it }
check("no row for the stray json", strayRes.sources.size(), rows.size())
check("and it is reported",        strayLog.any { it.contains("no .lux.h5 beside it") }, true)
new File(root, "raw/notes.json").delete()

println "\n=== identity comes from the sidecar, not the path ==="
def r0 = rows.find { it.series_id.contains("L26A") && it.t == 0 && it.channel == 1 }
check("series_id",    r0.series_id, "s0000_L26A_pos1_t0000")
check("channel_name", r0.channel_name, "GFP")
check("unnamed channel stays blank", rows.find { it.channel == 2 }.channel_name, "")
def s0 = ser.find { it.prefix == "s0000_L26A_pos1_t0000" }
check("series_name is the raw description", s0.series_name, "L26A pos1")
check("series_index is the stack number",   s0.series_index, 0)

println "\n=== dimensions and calibration per position ==="
check("L26A z", rows.find { it.series_id.contains("L26A") }.size_z, 3)
check("fucci z", rows.find { it.series_id.contains("fucci") }.size_z, 2)
check("pixel_unit set", r0.pixel_unit, "micron")
check("size_x", r0.size_x, 4)
check("size_y", r0.size_y, 6)

println "\n=== the series table is samples-shaped, so nothing downstream changes ==="
def SCHEMA = gcl.parseClass(new File(LIBDIR + "/SheetSchema.groovy")).loadFromLibDir(LIBDIR)
check("every required samples column present",
      SCHEMA.required("samples") - ser[0].keySet().toList(), [])
check("no undeclared series column",
      ser[0].keySet().toList() - SCHEMA.columns("samples"), [])
check("prefix is the series id",  s0.prefix, "s0000_L26A_pos1_t0000")
check("include seeded true",      s0.include, "true")
check("alias is the acquisition", s0.alias, "acq")
// path is the ACQUISITION DIRECTORY here, not a file -- the root source_path
// resolves against. That is the whole reason the four columns were kept.
check("path is the directory",    new File(s0.path).isDirectory(), true)
check("size_c is the channel count", s0.size_c, 3)
check("size_t is 1 per timepoint",   s0.size_t, 1)
check("pixel_type",                  s0.pixel_type, "uint16")
check("file_size sums the sources",
      s0.file_size,
      rows.findAll { it.series_id == s0.prefix }.sum { it.source_bytes as long })

println "\n=== manifest order is position, then time, then channel ==="
// A manifest is read and diffed, so the order has to be stable. A closure
// returning a List is NOT a multi-key sort in Groovy -- it silently yields an
// arbitrary order -- which is what this pins.
def keys = rows.collect { it.series_id + "|" + String.format("%04d", it.t as int) + "|" + it.channel }
check("already in sorted order", keys, keys.sort(false))
check("first row is series 0, t 0, channel 0",
      rows[0].series_id + "/t" + rows[0].t + "/c" + rows[0].channel,
      "s0000_L26A_pos1_t0000/t0/c0")
// and it is reproducible run to run
def rows2 = scanner.scan(root) { }.sources
check("second scan gives the same order",
      rows2.collect { it.source_path }, rows.collect { it.source_path })

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

println "\n=== gatherFrames regroups the tables, and nothing else ==="
// The gathering decision belongs to the SCAN, so the series table describes the
// file it will produce. Same sources, same count, different grouping.
def gRes  = scanner.scan(root, [gatherFrames: true]) { }
check("same sources",            gRes.sources.size(), rows.size())
check("one series per position", gRes.series.size(), 2)
check("was one per timepoint",   ser.size(), 4)
check("no t in the series id",   gRes.sources.every { !it.series_id.contains("_t0") }, true)
check("t is still on every row", gRes.sources.collect { it.t }.unique().sort(), [0, 1])
check("the sources are the same files",
      gRes.sources.collect { it.source_path }.sort(), rows.collect { it.source_path }.sort())
check("size_t now counts the frames", gRes.series[0].size_t, 2)
check("and size_c is unchanged",      gRes.series[0].size_c, 3)
// One series now legitimately holds channel 0 once per frame, so the duplicate
// check has to key on the timepoint or it would reject a good gathered table.
check("gathering is not mistaken for a duplicate source",
      scanner.validate(gRes.series, gRes.sources) { }.findAll { it.contains("same series channel") }, [])

println "\n=== quickScan reads one sidecar per directory, and agrees with the slow path ==="
def slow = scanner.scan(root, [quickScan: false]) { }
check("same sources either way", slow.sources, rows)
check("same series either way",  slow.series, ser)
// A timepoint of a different depth must NOT be assumed away: its file size
// stands out, so it is read in full. Without that, quickScan would report the
// sampled file's z for every frame.
def trunc = new File(tmp, "trunc"); trunc.mkdirs()
FIX.writeLux(new File(trunc, "c0/Cam_long_00000.lux.h5"), 8, 16, 16, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(trunc, "c0/Cam_long_00001.lux.h5"), 2, 16, 16, [stack: 0, channel: 0, tp: 1])
def tLog = []
def tRes = scanner.scan(trunc, [quickScan: true]) { tLog << it }
check("the odd frame is read in full, not assumed",
      tRes.sources.collect { it.size_z }.sort(), [2, 8])
check("and the difference is logged",
      tLog.any { it.contains("size differs from its siblings") }, true)
check("quickScan agrees with the slow path here too",
      scanner.scan(trunc, [quickScan: false]) { }.sources, tRes.sources)

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
check("one series per time point", tpRes.series.size(), 4)
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

println "\n=== warnings, not errors, for things a person should judge ==="
// z changing BETWEEN time points of one position: the time course is not one
// volume. Loud, but not fatal -- it may be what the operator meant.
def wobble = new File(tmp, "wobble"); wobble.mkdirs()
FIX.writeLux(new File(wobble, "t0/Cam_long_00000.lux.h5"), 3, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(wobble, "t1/Cam_long_00001.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 1])
def wLog = []
def wRows = scanner.scan(wobble) { wLog << it }.sources
check("z wobble warns, does not throw", wRows.size(), 2)
check("and says which position",        wLog.any { it.contains("changes z between time points") }, true)

// A gap in the time points means a file is missing.
def gap = new File(tmp, "gap"); gap.mkdirs()
FIX.writeLux(new File(gap, "t0/Cam_long_00000.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(gap, "t2/Cam_long_00002.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 2])
def gLog = []
scanner.scan(gap) { gLog << it }
check("time point gap warns", gLog.any { it.contains("not a run from 0") }, true)

println "\n=== bySeries groups the channels of one series together ==="
def grouped = LS.bySeries(rows)
check("group count", grouped.size(), 4)
check("channels per group", grouped.values().collect { it.size() }.unique(), [3])
check("channels in index order",
      grouped[grouped.keySet().toList()[0]].collect { it.channel }, [0, 1, 2])

println "\n=== the two tables must agree on series_id ==="
// A join that matches nothing is the failure this repo is built around, so the
// scan asserts it where the tables are made.
def badSeries = ser.collect { new LinkedHashMap(it) }
badSeries[0].prefix = "renamed_by_hand"
throwsWith("an orphaned source is fatal", "do not agree on series_id",
           { scanner.validate(badSeries, rows) { } })

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
