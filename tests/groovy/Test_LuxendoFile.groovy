// Test_LuxendoFile.groovy
//
// Checks for reading a Luxendo `.lux.h5`. Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_LuxendoFile.groovy
//
// NO DATA IS NEEDED. JHDF5 writes HDF5 as well as reading it, so the fixtures
// here are synthesised in `writeLux` below -- the same rule the rest of
// tests/groovy follows. That also means the fixture states the format
// explicitly: if a future Luxendo release changes `element_size_um` or moves
// `/Data`, this file is where the expectation is written down, and a real file
// that no longer matches it will fail against a fixture rather than against a
// vague memory of what the format was.

import ch.systemsx.cisd.hdf5.HDF5Factory
import ch.systemsx.cisd.base.mdarray.MDShortArray
import groovy.json.JsonOutput

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "LuxendoFile.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def LF = new GroovyClassLoader().parseClass(new File(LIBDIR + "/LuxendoFile.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-52s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
// element_size_um is float32 in the file, so its double form is not the
// decimal literal that was written. Compare within a tolerance rather than
// pinning a conversion artefact into the expectation.
def checkNear = { String what, Double got, double want, double tol = 1e-6 ->
    boolean ok = (got != null) && Math.abs(got - want) <= tol
    println String.format("  %-6s %-52s got=%s want=%s (+/- %s)", ok ? "ok" : "FAILED", what, got, want, tol)
    ok ? passed++ : failed++
}
def throwsWith = { String what, String fragment, Closure body ->
    String msg = null
    try { body() } catch (Throwable e) { msg = e.getMessage() }
    boolean ok = msg != null && msg.contains(fragment)
    println String.format("  %-6s %-52s threw=%s", ok ? "ok" : "FAILED", what, msg ?: "(nothing)")
    ok ? passed++ : failed++
}

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_luxendofile_" + System.nanoTime())
tmp.mkdirs()

/**
 * Write a synthetic .lux.h5 with exactly the shape a TruLive3D emits:
 * /Data as uint16 [z][y][x] with an element_size_um attribute in [z, y, x]
 * order, and /metadata holding the acquisition JSON.
 *
 * Pixel value at (z, y, x) is z*10000 + y*100 + x, so a plane read back
 * identifies which plane it came from -- a transposed or off-by-one read shows
 * up as a wrong number rather than as a plausible one.
 */
def writeLux = { File f, int nz, int ny, int nx, Map opts = [:] ->
    f.getParentFile()?.mkdirs()
    f.delete()
    def w = HDF5Factory.open(f)
    short[] flat = new short[nz * ny * nx]
    for (int z = 0; z < nz; z++)
        for (int y = 0; y < ny; y++)
            for (int x = 0; x < nx; x++)
                flat[(z * ny + y) * nx + x] = (short) (z * 10000 + y * 100 + x)
    w.uint16().writeMDArray("/Data", new MDShortArray(flat, [nz, ny, nx] as int[]))
    if (opts.get("elementSize", true)) {
        def el = (opts.el ?: [5.0f, 0.208f, 0.208f]) as float[]
        w.float32().setArrayAttr("/Data", "element_size_um", el)
    }
    if (opts.get("metadata", true)) {
        def vox = opts.containsKey("vox") ? opts.vox : [width: 0.208, height: 0.208, depth: 5.0]
        w.string().write("/metadata", JsonOutput.toJson([
            processingInformation: [
                image_id           : "2026-09-10T17:47:37.268Z-test",
                stack              : "0",
                stack_description  : (opts.stackDesc ?: "L26A pos1"),
                channel            : "1",
                channel_description: opts.containsKey("chanDesc") ? opts.chanDesc : "GFP",
                time_point         : (opts.tp ?: "0"),
                objective          : "bottom",
                camera             : "long",
                voxel_size_um      : vox,
                image_size_vx      : [width: nx, height: ny, depth: nz],
            ]
        ]))
    }
    w.close()
    return f
}

println "=== LuxendoFile: header of a normal stack ==="
def f1 = writeLux(new File(tmp, "Cam_long_00000.lux.h5"), 4, 8, 6)
def lf = LF.open(f1)
check("sizeZ",        lf.sizeZ, 4)
check("sizeY",        lf.sizeY, 8)
check("sizeX",        lf.sizeX, 6)
checkNear("pixelWidth",  lf.pixelWidth,  0.208d)
checkNear("pixelHeight", lf.pixelHeight, 0.208d)
checkNear("pixelDepth",  lf.pixelDepth,  5.0d)
check("payloadBytes", lf.payloadBytes(), 2L * 4 * 8 * 6)

println "\n=== metadata is parsed, and an absent channel name stays absent ==="
check("stack_description", lf.info.stack_description, "L26A pos1")
check("channel_description", lf.info.channel_description, "GFP")
check("time_point",        lf.info.time_point, "0")
check("metadataJson kept", lf.metadataJson.contains("TruLive") || lf.metadataJson.contains("processingInformation"), true)
lf.close()

def fNoChan = writeLux(new File(tmp, "nochan.lux.h5"), 2, 4, 4, [chanDesc: null])
def lfNoChan = LF.open(fNoChan)
check("channel_description null stays null", lfNoChan.info.channel_description, null)
lfNoChan.close()

println "\n=== planes come back in the right order ==="
// value = z*10000 + y*100 + x, so the first pixel of plane z is z*10000.
def lf2 = LF.open(f1)
(0..3).each { int z ->
    short[] p = lf2.plane(z)
    check("plane ${z} first px",  (p[0] & 0xFFFF),                z * 10000)
    check("plane ${z} px (y2,x3)", (p[2 * 6 + 3] & 0xFFFF),       z * 10000 + 200 + 3)
    check("plane ${z} length",    p.length,                       8 * 6)
}
throwsWith("plane above range refuses", "out of range", { lf2.plane(4) })
throwsWith("negative plane refuses",    "out of range", { lf2.plane(-1) })
lf2.close()

println "\n=== a single plane has NO z step, rather than a default one ==="
def fFlat = writeLux(new File(tmp, "single.lux.h5"), 1, 4, 4)
def lfFlat = LF.open(fFlat)
check("sizeZ",              lfFlat.sizeZ, 1)
check("pixelDepth is null", lfFlat.pixelDepth, null)
check("pixelWidth survives", lfFlat.pixelWidth != null, true)
lfFlat.close()

println "\n=== the two calibrations must agree ==="
// element_size_um says z=5.0; the JSON is made to say 9.9. Believing either
// silently would put every volume out by nearly a factor of two.
def fBad = writeLux(new File(tmp, "disagree.lux.h5"), 3, 4, 4,
                    [vox: [width: 0.208, height: 0.208, depth: 9.9]])
throwsWith("z step disagreement refuses", "pixel_depth disagrees", { LF.open(fBad) })

def fBadXY = writeLux(new File(tmp, "disagree_xy.lux.h5"), 3, 4, 4,
                      [vox: [width: 0.5, height: 0.208, depth: 5.0]])
throwsWith("pixel width disagreement refuses", "pixel_width disagrees", { LF.open(fBadXY) })

println "\n=== a file that is not a .lux.h5 is refused, not guessed at ==="
def fEmpty = new File(tmp, "empty.h5")
def we = HDF5Factory.open(fEmpty); we.string().write("/something", "x"); we.close()
throwsWith("no /Data refuses", "not a Luxendo", { LF.open(fEmpty) })
throwsWith("missing file refuses", "No such", { LF.open(new File(tmp, "nope.lux.h5")) })

println "\n=== an uncalibrated file reads, but says so ==="
def fNoCal = writeLux(new File(tmp, "nocal.lux.h5"), 2, 4, 4, [elementSize: false, metadata: false])
def lfNoCal = LF.open(fNoCal)
check("pixelWidth absent",  lfNoCal.pixelWidth, null)
check("pixelDepth absent",  lfNoCal.pixelDepth, null)
check("dims still read",    lfNoCal.sizeZ, 2)
check("info empty",         lfNoCal.info.isEmpty(), true)
lfNoCal.close()

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
