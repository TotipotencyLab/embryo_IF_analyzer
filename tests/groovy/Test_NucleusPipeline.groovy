// Test_NucleusPipeline.groovy
//
// The pipeline extracted out of Run_NucleusSelector.groovy, exercised directly.
//
// The image is synthesised, so this needs no fixture and runs in seconds. What
// it covers is the part a reference diff on real data CANNOT cover: the
// `basename` override, which is new. A reference run proves the override
// changes nothing when it is absent; identical output is equally consistent
// with "the parameter is never read", so the other half has to be proved
// separately and on purpose.
//
// Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_NucleusPipeline.groovy

import ij.ImagePlus
import ij.ImageStack
import ij.gui.OvalRoi
import ij.process.ByteProcessor

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "NucleusPipeline.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-54s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_nucpipe_" + System.nanoTime())
tmp.mkdirs()

// Two well-separated discs, away from the edge (detect() excludes edge
// particles), on every slice of a 4-slice single-channel stack.
def makeImp = { String title ->
    def st = new ImageStack(200, 200)
    (1..4).each {
        def ip = new ByteProcessor(200, 200)
        ip.setColor(255)
        ip.fill(new OvalRoi(30, 30, 50, 50))
        ip.fill(new OvalRoi(120, 120, 50, 50))
        st.addSlice(ip)
    }
    def imp = new ImagePlus(title, st)
    imp.setDimensions(1, 4, 1)
    return imp
}

def baseParams = [
    script_name            : "Test_NucleusPipeline.groovy",
    position_pattern       : "",
    z_spec                 : "",
    dna_channel            : 1,
    channels_measured      : "1",
    nucleus_blur_sigma     : 0.0d,       // the discs are already binary-clean
    nucleus_threshold      : "Otsu",
    nucleus_particle_size  : "200-Infinity",
    nucleus_watershed      : false,
    nucleoli_enabled       : false,
    nucleolus_blur_sigma   : 3.0d,
    nucleolus_threshold    : "Relative",
    nucleolus_rel_fraction : 0.6d,
    nucleolus_erode_px     : 0,
    nucleolus_particle_size: "3-150",
    nucleolus_circularity  : "0.50-1.00",
    save_roi_zips          : true,
    save_outlines          : true,
    save_measurements      : true,
    save_config            : true,
    save_overview          : false,
]

def NP = new GroovyClassLoader().parseClass(new File(LIBDIR, "NucleusPipeline.groovy"))

println "=== load() ==="
def pipe = NP.load(LIBDIR)
check("load() returns a pipeline",             pipe != null, true)
String loadErr = null
try { NP.load(new File(tmp, "nowhere").getPath()) }
catch (Throwable t) { loadErr = t.getClass().getSimpleName() }
check("load() on a bad dir throws clearly",    loadErr, "IllegalStateException")

println ""
println "=== basename OFF: resolved from the image, as before ==="
def outA = new File(tmp, "a"); outA.mkdirs()
def resA = pipe.run(makeImp("probeimage"), outA, baseParams + [output_prefix: "TEST_"])
check("basename resolved from the title",      resA.basename, "TEST_probeimage")
check("8 ROIs found (2 discs x 4 slices)",     resA.nucRois.size(), 8)
check("outline written under that name",
      new File(outA, "TEST_probeimage_nucleus_outline.txt").isFile(), true)
check("config written under that name",
      new File(outA, "TEST_probeimage_config.txt").isFile(), true)

println ""
println "=== basename ON: the caller's name wins ==="
def outB = new File(tmp, "b"); outB.mkdirs()
def resB = pipe.run(makeImp("probeimage"), outB,
                    baseParams + [output_prefix: "TEST_", basename: "sheetAlias_Series001"])
check("basename is the one supplied",          resB.basename, "sheetAlias_Series001")
check("output_prefix does NOT get prepended",  resB.basename.startsWith("TEST_"), false)
check("outline uses the supplied name",
      new File(outB, "sheetAlias_Series001_nucleus_outline.txt").isFile(), true)
check("the resolved name is NOT used",
      new File(outB, "TEST_probeimage_nucleus_outline.txt").exists(), false)

// Same image, same settings -- so anything differing between A and B beyond the
// name would be the override changing the analysis, which it must not.
check("same ROI count either way",             resB.nucRois.size(), resA.nucRois.size())
// Defensive: a failure above leaves one of these files absent, and an
// exception here would kill the run before it printed its summary -- which is
// the line a caller actually reads.
def stripName = { File f ->
    f.isFile() ? f.getText("UTF-8").readLines().drop(1).collect { it.split("\t", -1).drop(1).join("\t") }
               : null
}
def geomA = stripName(new File(outA, "TEST_probeimage_nucleus_outline.txt"))
def geomB = stripName(new File(outB, "sheetAlias_Series001_nucleus_outline.txt"))
check("outline geometry identical either way", (geomA != null && geomA == geomB), true)

println ""
println "=== the config records the CALLER, not the library ==="
def cfg = [:]
new File(outB, "sheetAlias_Series001_config.txt").eachLine { line ->
    def parts = line.split("\t", -1)
    if (parts.length == 2 && parts[0] != "parameter") cfg[parts[0]] = parts[1]
}
check("script names the entry point",          cfg["script"]?.startsWith("Test_NucleusPipeline.groovy"), true)
check("output_basename matches the override",  cfg["output_basename"], "sheetAlias_Series001")
check("nucleus_count is recorded",             cfg["nucleus_count"], "8")

tmp.deleteDir()
println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} nucleus-pipeline check(s) failed")
