// Test_RunConfig.groovy
//
// The run config is provenance: a results directory is often read long after,
// on another machine. Two checks here.
//
//   1. saveRunConfig round-trips the image geometry, including the
//      image_width / image_height fields the R montage needs to draw its panel
//      over the same frame as the Fiji overview PNG.
//   2. Run_NucleusSelector.groovy still parses, and actually records those two
//      fields. A `#@` parameter line is NOT Groovy -- SciJava strips it before
//      compiling -- so the runner is parsed here with those lines removed,
//      which is the closest cheap check to "Fiji would compile this".
//
// Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_RunConfig.groovy

import ij.ImagePlus
import ij.process.ByteProcessor

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "RoiExport.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def RX = new GroovyClassLoader().parseClass(new File(LIBDIR + "/RoiExport.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-52s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_runconfig_" + System.nanoTime())
tmp.mkdirs()

println "=== saveRunConfig round trip ==="

// Deliberately non-square, so a width/height swap cannot pass unnoticed.
def imp = new ImagePlus("probe", new ByteProcessor(37, 23))
imp.getCalibration().pixelWidth  = 0.5
imp.getCalibration().pixelHeight = 0.25
imp.getCalibration().setUnit("micron")

def cfgPath = new File(tmp, "probe_config.txt").getPath()
RX.saveRunConfig([
    image_title  : imp.getTitle(),
    image_width  : imp.getWidth(),
    image_height : imp.getHeight(),
    pixel_width  : imp.getCalibration().pixelWidth,
    pixel_height : imp.getCalibration().pixelHeight,
    pixel_unit   : imp.getCalibration().getUnit(),
    nothing      : null
], cfgPath)

def cfg = [:]
new File(cfgPath).eachLine { line ->
    def parts = line.split("\t", -1)
    if (parts.length == 2 && parts[0] != "parameter") cfg[parts[0]] = parts[1]
}

check("image_width is the pixel width",        cfg["image_width"],  "37")
check("image_height is the pixel height",      cfg["image_height"], "23")
check("width and height are not swapped",      cfg["image_width"] != cfg["image_height"], true)
check("pixel_width survives",                  cfg["pixel_width"],  "0.5")
check("pixel_height survives",                 cfg["pixel_height"], "0.25")
check("a null value becomes empty, not 'null'", cfg["nothing"],     "")

// The R side multiplies these together to get the frame in calibrated units.
double xmax = (cfg["image_width"] as double) * (cfg["pixel_width"] as double)
double ymax = (cfg["image_height"] as double) * (cfg["pixel_height"] as double)
check("frame in microns, x",                   xmax, 18.5d)
check("frame in microns, y",                   ymax, 5.75d)

println ""
println "=== Run_NucleusSelector.groovy records them ==="

def runner = new File(LIBDIR, "Run_NucleusSelector.groovy")
check("the runner is present",                 runner.isFile(), true)

def src = runner.getText("UTF-8")
check("records image_width",                   src.contains("image_width"), true)
check("records image_height",                  src.contains("image_height"), true)
check("reads them from the ImagePlus",         src.contains("imp.getWidth()") && src.contains("imp.getHeight()"), true)

// The overview pair. The raw projection and the outlined one used to share one
// file name, which made them mutually exclusive; montage_qc_cli.r needs both.
check("records overview_channels",             src.contains("overview_channels"), true)
// The suffix string itself lives in Overview.groovy, so the two runners cannot
// drift apart -- a literal here would be a second source of truth.
check("takes the suffix from the library",     src.contains("OV.OVERLAY_SUFFIX"), true)
check("...and does not hardcode it",           src.contains('"_overlay"'), false)
check("saves raw BEFORE adding outlines",
      src.indexOf('overviewPath(outDirPath, basename, c, "")') < src.indexOf("addOutlines"), true)
check("saves the overlaid copy too",
      src.contains("overviewPath(outDirPath, basename, c, OV.OVERLAY_SUFFIX)"), true)
check("overviews cover the measured channels", src.contains("([dnaCh] + channels).unique()"), true)
check("logs the display range",                src.contains("view.lo") && src.contains("view.hi"), true)

// Parse every runner with the SciJava `#@` lines stripped -- they are directives
// to Fiji, not Groovy, and would be a syntax error here. This is the closest
// cheap check to "Fiji would compile this", and it is the only coverage the
// Run_*.groovy scripts have.
println ""
println "=== every Run_*.groovy still compiles ==="
new File(LIBDIR).listFiles().findAll { it.getName().startsWith("Run_") }.sort().each { f ->
    def body = f.getText("UTF-8").readLines().findAll { !(it.trim().startsWith("#@")) }.join("\n")
    String err = null
    try {
        new GroovyClassLoader().parseClass(body, f.getName().replace(".groovy", "_stripped.groovy"))
    } catch (Throwable t) {
        err = t.getClass().getSimpleName() + ": " + t.getMessage()
    }
    check(f.getName(), err, null)
}

tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} run-config check(s) failed")
