// Test_RoiExport.groovy
//
// Round-trip checks for RoiExport's ROI zip writing and reading. Run headless
// from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_RoiExport.groovy
//
// The ROI name is not decoration: it is written into the .roi file and into the
// measurement Label column, and it is how the R side joins measurements to
// outlines. So the round trip has to preserve names, not just shapes.

import ij.*
import ij.gui.OvalRoi
import ij.gui.PolygonRoi
import ij.gui.Roi
import ij.process.FloatPolygon

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "RoiExport.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def RX = new GroovyClassLoader().parseClass(new File(LIBDIR + "/RoiExport.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-48s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def throwsWith = { String what, String fragment, Closure body ->
    String msg = null
    try { body() } catch (IllegalArgumentException e) { msg = e.getMessage() }
    boolean ok = msg != null && msg.contains(fragment)
    println String.format("  %-6s %-48s threw=%s", ok ? "ok" : "FAILED", what, msg ?: "(nothing)")
    ok ? passed++ : failed++
}

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_roiexport_" + System.nanoTime())
tmp.mkdirs()

println "=== RoiExport zip round trip ==="

// Shapes of different kinds, and names in the SSSS-NNNN-YYYY form the pipeline uses.
def rois = [
    new Roi(10, 20, 30, 40),
    new OvalRoi(50, 60, 20, 20),
    new PolygonRoi(new FloatPolygon([5f, 45f, 45f, 5f] as float[], [5f, 5f, 35f, 35f] as float[]), Roi.POLYGON)
]
rois[0].setPosition(3); rois[1].setPosition(7); rois[2].setPosition(11)
def names = ["nucleus_0003-0001-0040", "nucleus_0007-0002-0070", "nucleus_0011-0003-0020"]

def zip = new File(tmp, "round_trip_ROIs.zip").getPath()
RX.saveRoiZip(rois, names, zip)
def back = RX.loadRoiZip(zip)

check("every ROI comes back",                 back.size(), 3)
check("names survive the round trip",         back*.getName(), names)
check("bounds survive",                       back*.getBounds()*.toString(), rois*.getBounds()*.toString())
check("shape types survive",                  back*.getTypeAsString(), rois*.getTypeAsString())
check("slice positions survive",              back*.getPosition(), [3, 7, 11])
// Vertex-level check on the polygon: bounds alone would not catch a mangled outline.
check("polygon vertex count survives",        back[2].getFloatPolygon().npoints, rois[2].getFloatPolygon().npoints)
check("polygon x coords survive",             back[2].getFloatPolygon().xpoints as List, rois[2].getFloatPolygon().xpoints as List)

// Reading must not stop after the first entry -- the reason the reader loops
// over the stream by hand instead of using Groovy's `stream.bytes`, which closes
// the stream it reads.
def many = (1..25).collect { new Roi(it, it, 5, 5) }
// NB: String.format, not "...".formatted(it) -- the latter is Java 15+ and this
//     Fiji runs on Java 8.
def manyNames = (1..25).collect { String.format("nucleus_%04d-0001-0010", it) }
RX.saveRoiZip(many, manyNames, new File(tmp, "many_ROIs.zip").getPath())
check("all 25 entries are read",              RX.loadRoiZip(new File(tmp, "many_ROIs.zip").getPath()).size(), 25)

throwsWith("a missing file is rejected",      "no such ROI zip") { RX.loadRoiZip(new File(tmp, "nope.zip").getPath()) }

// --- the real thing, when it is there ------------------------------------------
// fixture/if_data/data/*.zip is untracked, so skip when absent.
def real = new File("fixture/if_data/data/GRV_Position010_nucleus_outline_ROIs.zip")
if (real.isFile()) {
    def fixture = RX.loadRoiZip(real.getPath())
    check("fixture zip: 70 nucleus ROIs",     fixture.size(), 70)
    check("fixture zip: names are nucleus_SSSS-NNNN-YYYY",
          fixture.every { it.getName() ==~ /^nucleus_\d{4}-\d{4}-\d{4}$/ }, true)
    check("fixture zip: positions are real slices",
          fixture.every { it.getPosition() >= 1 && it.getPosition() <= 50 }, true)
} else {
    println "  skip   fixture zip not present (${real})"
}

tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} RoiExport check(s) failed")
