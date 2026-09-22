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


// --- image id resolution -------------------------------------------------------
// The strings below are the real ones, read off a Leica .lif and the embryo
// fixture. ImageJ prefixes a hyperstack slice label with the plane coordinates
// ("c:1/3 z:12/56 - Series001"), and those slashes are NOT path separators --
// splitting the raw label on "/" handed back "56 - Series001".

println ""
println "=== resolveImageId ==="

def mkImp = { String title, String label ->
    def st = new ij.ImageStack(4, 4)
    st.addSlice(label, new ij.process.ByteProcessor(4, 4))
    return new ImagePlus(title, st)
}

def idCheck = { String what, String title, String label, String pattern, String want ->
    def got = RX.resolveImageId(mkImp(title, label), pattern)
    boolean ok = (got == want)
    println String.format("  %-6s %-34s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}

idCheck("lif z-stack: the reported bug",
        "f.lif - Series001", "c:1/3 z:12/56 - Series001", "Series", "Series001")
idCheck("lif single plane, via the title",
        "260909_IHC.lif - Image002", "c:1/3 - Image002", "Series", "Image002")
idCheck("embryo fixture is unchanged",
        "x.lif-Position010-1.tif",
        "c:1/4 z:1/50 - Lightning 001/Mark_and_Find 001/Position010", "Position", "Position010")
idCheck("a series name containing a space",
        "f.lif - Image005 Denoised", "c:1/3 - Image005 Denoised", "Image", "Image005_Denoised")
idCheck("no pattern falls back to the title",
        "f.lif - Series004", "c:1/3 - Series004", "", "Series004")
idCheck("pattern later in a path-like label",
        "f.tif", "c:1/2 z:3/9 - A 001/B 002/Position077", "Position", "Position077")

check("stripSliceCoords leaves a bare label alone", RX.stripSliceCoords("Position010"), "Position010")
check("stripSliceCoords on an empty label",         RX.stripSliceCoords(""), "")
check("stripFileTitle keeps a plain title",         RX.stripFileTitle("Position010"), "Position010")
// A dash that is not the Bio-Formats separator must survive.
check("stripFileTitle ignores an ordinary dash",    RX.stripFileTitle("a-b-c.tif"), "a-b-c.tif")

println ""
println "=== sanitize ==="
// The image id becomes a filename AND the `name` column of every outline row.
// That table is tab separated, so a value containing a space makes R's
// read.table() see more fields than the header unless it names the separator.
check("whitespace collapses to underscore", RX.sanitize("Image005 Denoised"), "Image005_Denoised")
check("runs of whitespace collapse to one", RX.sanitize("multi   space"), "multi_space")
check("ends are trimmed, not underscored",  RX.sanitize("  padded  "), "padded")
check("path characters still go",           RX.sanitize("a/b:c"), "a_b_c")
check("a trailing extension still goes",    RX.sanitize("thing.lif"), "thing")
check("no space survives sanitize",         RX.sanitize("a b c").contains(" "), false)

tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} RoiExport check(s) failed")
