// Test_LuxendoScan.groovy
//
// A synthetic Luxendo tree -> the assembly manifest. Run headless from the
// repo root:
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

println "=== the tree scans into a manifest ==="
def logLines = []
def rows = scanner.scan(root) { logLines << it }
check("rows = positions x channels x t", rows.size(), 2 * 3 * 2)
check("outputs = positions x t",         rows.collect { it.target_output_path }.unique().size(), 4)
check("positions",                       rows.collect { it.position_id }.unique().size(), 2)

println "\n=== the link-only index file is skipped, and says so ==="
check("index not scanned as data", rows.any { it.source_path.contains("main_raw") }, false)
check("skip is reported",          logLines.any { it.contains("main_raw.lux.h5") && it.contains("index") }, true)

println "\n=== identity comes from the metadata, not the path ==="
def r0 = rows.find { it.position_id.contains("L26A") && it.t == 0 && it.channel == 1 }
check("position_id",       r0.position_id, "s0000_L26A_pos1")
check("series_id",         r0.series_id,   "s0000_L26A_pos1_t0000")
check("target_output_path", r0.target_output_path, "s0000_L26A_pos1_t0000.tif")
check("stack_description", r0.stack_description, "L26A pos1")
check("channel_name",      r0.channel_name, "GFP")
check("unnamed channel stays blank",
      rows.find { it.channel == 2 }.channel_name, "")

println "\n=== dimensions and calibration per position ==="
check("L26A z", rows.find { it.position_id.contains("L26A") }.size_z, 3)
check("fucci z", rows.find { it.position_id.contains("fucci") }.size_z, 2)
check("pixel_unit set", r0.pixel_unit, "micron")
check("size_x", r0.size_x, 4)
check("size_y", r0.size_y, 6)

println "\n=== manifest order is position, then time, then channel ==="
// A manifest is read and diffed, so the order has to be stable. A closure
// returning a List is NOT a multi-key sort in Groovy -- it silently yields an
// arbitrary order -- which is what this pins.
def keys = rows.collect { it.position_id + "|" + String.format("%04d", it.t) + "|" + it.channel }
check("already in sorted order", keys, keys.sort(false))
check("first row is position 0, t 0, channel 0",
      rows[0].position_id + "/t" + rows[0].t + "/c" + rows[0].channel,
      "s0000_L26A_pos1/t0/c0")
// and it is reproducible run to run
def rows2 = scanner.scan(root) { }
check("second scan gives the same order",
      rows2.collect { it.source_path }, rows.collect { it.source_path })

println "\n=== single-plane positions carry no z step ==="
def flatRoot = FIX.buildTree(new File(tmp, "flat"), [[stack: 0, desc: "pos1", nz: 1]],
                             [[index: 0, name: "BF"]], 1)
def flatRows = scanner.scan(flatRoot) { }
check("size_z",      flatRows[0].size_z, 1)
check("pixel_depth", flatRows[0].pixel_depth, null)

println "\n=== what it refuses ==="
def bare = new File(tmp, "bare"); bare.mkdirs()
throwsWith("a directory with no .lux.h5", "No .lux.h5 files under", { scanner.scan(bare) { } })
throwsWith("a missing directory",         "Not a directory",        { scanner.scan(new File(tmp, "nope")) { } })

def onlyIndex = new File(tmp, "onlyindex"); onlyIndex.mkdirs()
FIX.writeIndex(new File(onlyIndex, "main_raw.lux.h5"))
throwsWith("only index files", "holds a /Data stack", { scanner.scan(onlyIndex) { } })

def noMeta = new File(tmp, "nometa"); noMeta.mkdirs()
FIX.writeLux(new File(noMeta, "Cam_long_00000.lux.h5"), 2, 4, 4, [metadata: false])
throwsWith("no metadata to place the file by", "refusing to guess from the path",
           { scanner.scan(noMeta) { } })

println "\n=== two sources claiming one output channel is fatal ==="
// Same stack, channel and time point in two different directories: in a stack
// they would overwrite each other silently.
def clash = new File(tmp, "clash"); clash.mkdirs()
FIX.writeLux(new File(clash, "a/Cam_long_00000.lux.h5"), 2, 4, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(clash, "b/Cam_long_00000.lux.h5"), 2, 4, 4, [stack: 0, channel: 0, tp: 0])
throwsWith("duplicate output channel", "Two sources claim the same output channel",
           { scanner.scan(clash) { } })

println "\n=== channels of one output must agree on dimensions ==="
def ragged = new File(tmp, "ragged"); ragged.mkdirs()
FIX.writeLux(new File(ragged, "c0/Cam_long_00000.lux.h5"), 3, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(ragged, "c1/Cam_long_00000.lux.h5"), 2, 6, 4, [stack: 0, channel: 1, tp: 0])
throwsWith("channels disagree on z", "disagree on dimensions",
           { scanner.scan(ragged) { } })

println "\n=== gatherFrames regroups the manifest, and nothing else ==="
// The gathering decision belongs to the SCAN, so the table describes the file
// it will produce. Same sources, same count, different grouping.
def gRows = scanner.scan(root, [gatherFrames: true]) { }
check("same sources",             gRows.size(), rows.size())
check("one output per position",  gRows.collect { it.target_output_path }.unique().size(), 2)
check("was one per timepoint",    rows.collect { it.target_output_path }.unique().size(), 4)
check("series_id is the position", gRows.every { it.series_id == it.position_id }, true)
check("no t in the output name",  gRows.every { !it.target_output_path.contains("_t0") }, true)
check("t is still on every row",  gRows.collect { it.t }.unique().sort(), [0, 1])
check("position_id is unchanged",
      gRows.collect { it.position_id }.unique().sort(),
      rows.collect { it.position_id }.unique().sort())
check("the sources are the same files",
      gRows.collect { it.source_path }.sort(), rows.collect { it.source_path }.sort())
// One output now legitimately holds channel 0 more than once -- once per frame
// -- so the duplicate check has to key on the timepoint or it would reject a
// perfectly good gathered manifest.
check("gathering is not mistaken for a duplicate source",
      scanner.validate(gRows) { }.findAll { it.contains("same output channel") }, [])

println "\n=== warnings, not errors, for things a person should judge ==="
// z changing BETWEEN time points of one position: the time course is not one
// volume. Loud, but not fatal -- it may be what the operator meant.
def wobble = new File(tmp, "wobble"); wobble.mkdirs()
FIX.writeLux(new File(wobble, "t0/Cam_long_00000.lux.h5"), 3, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(wobble, "t1/Cam_long_00001.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 1])
def wLog = []
def wRows = scanner.scan(wobble) { wLog << it }
check("z wobble warns, does not throw", wRows.size(), 2)
check("and says which position",        wLog.any { it.contains("changes z between time points") }, true)

// A gap in the time points means a file is missing.
def gap = new File(tmp, "gap"); gap.mkdirs()
FIX.writeLux(new File(gap, "t0/Cam_long_00000.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 0])
FIX.writeLux(new File(gap, "t2/Cam_long_00002.lux.h5"), 2, 6, 4, [stack: 0, channel: 0, tp: 2])
def gLog = []
scanner.scan(gap) { gLog << it }
check("time point gap warns", gLog.any { it.contains("not a run from 0") }, true)

println "\n=== byOutput groups the channels of one file together ==="
def grouped = LS.byOutput(rows)
check("group count", grouped.size(), 4)
check("channels per group", grouped.values().collect { it.size() }.unique(), [3])
check("channels in index order",
      grouped[grouped.keySet().toList()[0]].collect { it.channel }, [0, 1, 2])

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
