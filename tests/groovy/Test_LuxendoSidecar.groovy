// Test_LuxendoSidecar.groovy
//
// The `.json` sidecar reader. Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_LuxendoSidecar.groovy
//
// No data is needed; the sidecars are written by LuxFixture.

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "LuxendoSidecar.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def gcl = new GroovyClassLoader()
def SC  = gcl.parseClass(new File(LIBDIR + "/LuxendoSidecar.groovy"))
def LFC = gcl.parseClass(new File(LIBDIR + "/LuxendoFile.groovy"))
def FIX = gcl.parseClass(new File("tests/groovy/LuxFixture.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-56s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def throwsWith = { String what, String fragment, Closure body ->
    String msg = null
    try { body() } catch (Throwable e) { msg = e.getMessage() }
    boolean ok = msg != null && msg.contains(fragment)
    println String.format("  %-6s %-56s threw=%s", ok ? "ok" : "FAILED", what,
                          msg ? msg.readLines()[0] : "(nothing)")
    ok ? passed++ : failed++
}

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_luxsidecar_" + System.nanoTime())
tmp.mkdirs()

println "=== pairing: an image and its sidecar, never one alone ==="
def h5 = FIX.writeLux(new File(tmp, "d/Cam_long_00003.lux.h5"), 4, 6, 5,
                      [stack: 2, stackDesc: "L26A pos3", channel: 1, chanDesc: "GFP", tp: 3])
check("sidecarFor",  SC.sidecarFor(h5).getName(), "Cam_long_00003.json")
check("imageFor",    SC.imageFor(SC.sidecarFor(h5)).getName(), "Cam_long_00003.lux.h5")
check("round trip",  SC.imageFor(SC.sidecarFor(h5)).getAbsolutePath(), h5.getAbsolutePath())
check("isPaired",    SC.isPaired(h5), true)
// An index file has no sidecar -- which is the whole index-file test, and the
// reason the old "open it and look for /Data" check could be deleted.
def idx = FIX.writeIndex(new File(tmp, "d/main_raw.lux.h5"))
check("the index file is not paired", SC.isPaired(idx), false)
SC.sidecarFor(h5).renameTo(new File(tmp, "d/moved.json"))
check("an image whose sidecar left is not paired", SC.isPaired(h5), false)
new File(tmp, "d/moved.json").renameTo(SC.sidecarFor(h5))
check("and is paired again when it returns", SC.isPaired(h5), true)
check("a name that is not a .lux.h5", SC.sidecarFor(new File(tmp, "x.tif")), null)

println "\n=== the sidecar says the same thing as the HDF5 inside ==="
// This is the claim the whole design rests on: measured 25 of 25 on the real
// acquisition, and pinned here so a future Luxendo release that moves a field
// fails a test instead of producing a wrong table.
def sc = SC.read(SC.sidecarFor(h5))
def lf = LFC.open(h5)
try {
    check("stack",       sc.stack, 2)
    check("channel",     sc.channel, 1)
    check("time_point",  sc.timePoint, 3)
    check("stack_description",   sc.stackDescription, "L26A pos3")
    check("channel_description", sc.channelDescription, "GFP")
    check("sizeX matches the HDF5", sc.sizeX, lf.sizeX)
    check("sizeY matches the HDF5", sc.sizeY, lf.sizeY)
    check("sizeZ matches the HDF5", sc.sizeZ, lf.sizeZ)
    check("pixelWidth matches",  Math.abs(sc.pixelWidth  - lf.pixelWidth)  < 1e-6, true)
    check("pixelHeight matches", Math.abs(sc.pixelHeight - lf.pixelHeight) < 1e-6, true)
    check("pixelDepth matches",  Math.abs(sc.pixelDepth  - lf.pixelDepth)  < 1e-6, true)
} finally { lf.close() }

println "\n=== a single plane carries NO z step, never 1.0 ==="
// Same rule as _config.txt's pixel_depth: a z step that does not exist must not
// arrive as a usable-looking number, because something downstream multiplies
// by it. The JSON still has a depth; the reader is what drops it.
def flat = FIX.writeLux(new File(tmp, "d/flat_00000.lux.h5"), 1, 4, 4, [stack: 0, channel: 0, tp: 0])
def fsc = SC.read(SC.sidecarFor(flat))
check("sizeZ", fsc.sizeZ, 1)
check("pixelDepth is null", fsc.pixelDepth, null)
check("but x and y survive", fsc.pixelWidth != null && fsc.pixelHeight != null, true)

println "\n=== numbers come as JSON STRINGS, and are parsed not cast ==="
// Luxendo writes "stack": "0", not "stack": 0. A Groovy cast of "0" to Integer
// throws, so these must be parsed -- and a value that is not a number must say
// so rather than become null.
def strs = FIX.writeSidecar(new File(tmp, "e/a.json"),
    [stack: "11", channel: "2", time_point: "95",
     image_size_vx: [width: 2048, height: 2048, depth: 39],
     voxel_size_um: [width: 0.208, height: 0.208, depth: 5.0]])
def ssc = SC.read(strs)
check("stack from a string",      ssc.stack, 11)
check("channel from a string",    ssc.channel, 2)
check("time_point from a string", ssc.timePoint, 95)
check("sizeZ from a number",      ssc.sizeZ, 39)

println "\n=== what it refuses, and what it merely leaves blank ==="
// The three fields that PLACE a file are not optional: a file that cannot be
// placed must stop the scan, not be dropped or guessed at from its path.
["stack", "channel", "time_point"].each { String missing ->
    def m = [stack: "0", channel: "0", time_point: "0",
             image_size_vx: [width: 4, height: 4, depth: 2]]
    m.remove(missing)
    def f = FIX.writeSidecar(new File(tmp, "e/no_${missing}.json"), m)
    throwsWith("no " + missing, "refusing to guess from the path", { SC.read(f) })
}
def notNum = FIX.writeSidecar(new File(tmp, "e/notnum.json"),
    [stack: "zero", channel: "0", time_point: "0",
     image_size_vx: [width: 4, height: 4, depth: 2]])
throwsWith("a non-numeric stack", "not a whole number", { SC.read(notNum) })

def noSize = FIX.writeSidecar(new File(tmp, "e/nosize.json"),
    [stack: "0", channel: "0", time_point: "0"])
throwsWith("no dimensions", "the dimensions are not optional", { SC.read(noSize) })

def zeroSize = FIX.writeSidecar(new File(tmp, "e/zerosize.json"),
    [stack: "0", channel: "0", time_point: "0", image_size_vx: [width: 0, height: 4, depth: 2]])
throwsWith("a zero dimension", "image_size_vx is", { SC.read(zeroSize) })

new File(tmp, "e/garbage.json").setText("this is not json {{{", "UTF-8")
throwsWith("not JSON at all", "not readable as JSON", { SC.read(new File(tmp, "e/garbage.json")) })

new File(tmp, "e/other.json").setText('{"somethingElse": 1}', "UTF-8")
throwsWith("JSON that is not a sidecar", "not a Luxendo sidecar",
           { SC.read(new File(tmp, "e/other.json")) })

throwsWith("a missing file", "No such sidecar", { SC.read(new File(tmp, "e/absent.json")) })

// Blank, not fatal: an unnamed channel and an absent voxel size are things the
// acquisition legitimately omits.
def sparse = FIX.writeSidecar(new File(tmp, "e/sparse.json"),
    [stack: "0", channel: "2", time_point: "0", image_size_vx: [width: 4, height: 4, depth: 2]])
def spsc = SC.read(sparse)
check("unnamed channel is null, not an error", spsc.channelDescription, null)
check("no voxel size is null, not 1.0",        spsc.pixelWidth, null)
check("and the depth too",                     spsc.pixelDepth, null)

println "\n=== the time point in a filename, used only after it is confirmed ==="
check("plain",          SC.timePointFromName(new File("Cam_long_00007.lux.h5")), 7)
check("five digits",    SC.timePointFromName(new File("Cam_long_00095.lux.h5")), 95)
check("leading zeros",  SC.timePointFromName(new File("Cam_long_00000.lux.h5")), 0)
check("uppercase ext",  SC.timePointFromName(new File("Cam_long_00003.LUX.H5")), 3)
check("no number",      SC.timePointFromName(new File("Cam_long.lux.h5")), null)
check("number not at the end", SC.timePointFromName(new File("Cam_00003_long.lux.h5")), null)

println "\n=== atTimePoint copies everything but the time point ==="
def base = SC.read(SC.sidecarFor(h5))
def moved = base.atTimePoint(41)
check("time point replaced", moved.timePoint, 41)
check("stack kept",          moved.stack, base.stack)
check("channel kept",        moved.channel, base.channel)
check("shape kept",          moved.shape(), base.shape())
check("the original is untouched", base.timePoint, 3)

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
