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
import ij.gui.Roi
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

// The same two discs plus ONE elongated bar per slice. The bar is 60x10 = 600
// px^2, so it passes the 200-Infinity size filter that the discs pass -- which
// is the point: it is the artefact that SIZE does not catch, which is the whole
// reason circularity was asked for. Its circularity is ~0.39 against the discs'
// ~1.0, so a 0.50 cut separates them.
def makeImpWithBar = { String title ->
    def st = new ImageStack(200, 200)
    (1..4).each {
        def ip = new ByteProcessor(200, 200)
        ip.setColor(255)
        ip.fill(new OvalRoi(30, 30, 50, 50))
        ip.fill(new OvalRoi(120, 120, 50, 50))
        ip.fill(new Roi(30, 150, 60, 10))
        st.addSlice(ip)
    }
    def imp = new ImagePlus(title, st)
    imp.setDimensions(1, 4, 1)
    return imp
}

// Discs whose intensity VARIES with z, so that max, mean and min projections
// differ from one another. Without that, every projection method produces the
// same picture and a test comparing them would pass whether or not the
// parameter was ever read.
def makeImpZVarying = { String title ->
    def st = new ImageStack(200, 200)
    (1..4).each { int z ->
        def ip = new ByteProcessor(200, 200)
        ip.setColor(60 + 40 * z)            // 100, 140, 180, 220
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
    nucleus_circularity    : "0.00-1.00",
    overview_method        : "max",
    overview_width         : 500,
    overview_height        : 0,
    overview_contrast      : "auto",
    overview_saturated     : 0.35d,
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

println ""
println "=== pixel_depth: recorded for a stack, BLANK for a single plane ==="
// feature_stat_cli.r needs the z step for `volume` and had to be told by hand,
// because nothing wrote it down. Bio-Formats populates it on import.
def readCfg = { File f ->
    def m = [:]
    f.eachLine { line ->
        def parts = line.split("\t", -1)
        if (parts.length == 2 && parts[0] != "parameter") m[parts[0]] = parts[1]
    }
    return m
}

def outC = new File(tmp, "c"); outC.mkdirs()
def impC = makeImp("stackimage")
impC.getCalibration().pixelDepth = 0.9999285454545455d
impC.getCalibration().pixelWidth = 0.25d
pipe.run(impC, outC, baseParams + [basename: "stk"])
def cfgC = readCfg(new File(outC, "stk_config.txt"))
check("pixel_depth is written for a stack",    cfgC["pixel_depth"], "0.9999285454545455")
check("pixel_width still written beside it",   cfgC["pixel_width"], "0.25")
check("image_slices says it is a stack",       cfgC["image_slices"], "4")

// ImageJ defaults pixelDepth to 1.0 when there is no z axis, and Bio-Formats
// reports the physical size as null there. Writing 1.0 would hand a reader a
// plausible number for a distance that does not exist -- area_sum x 1.0 is an
// area wearing a volume's name.
def outD = new File(tmp, "d"); outD.mkdirs()
def st1 = new ij.ImageStack(200, 200)
def ip1 = new ByteProcessor(200, 200)
ip1.setColor(255); ip1.fill(new OvalRoi(30, 30, 50, 50)); ip1.fill(new OvalRoi(120, 120, 50, 50))
st1.addSlice(ip1)
def impD = new ImagePlus("planeimage", st1)
impD.setDimensions(1, 1, 1)
check("the single-plane probe really has 1 slice", impD.getNSlices(), 1)
check("...and ImageJ's default depth is 1.0",  impD.getCalibration().pixelDepth, 1.0d)
pipe.run(impD, outD, baseParams + [basename: "pln"])
def cfgD = readCfg(new File(outD, "pln_config.txt"))
check("pixel_depth is BLANK for one plane",    cfgD["pixel_depth"], "")
check("...the key is still present",           cfgD.containsKey("pixel_depth"), true)
check("2 ROIs found on the single plane",      cfgD["nucleus_count"], "2")

tmp.deleteDir()
println ""
println "=== nucleus circularity: off is the old behaviour, on drops the bar ==="
// The bar exists to be the artefact the SIZE filter cannot catch. Proving the
// filter works needs both halves: that it changes nothing at its default, and
// that it does something when set. Identical counts alone would be equally
// consistent with the parameter never being read.
def outE = new File(tmp, "e"); outE.mkdirs()
def resE = pipe.run(makeImpWithBar("bar"), outE, baseParams + [basename: "circ_off"])
check("filter off: 12 ROIs (2 discs + 1 bar) x 4",  resE.nucRois.size(), 12)
def cfgE = new File(outE, "circ_off_config.txt").readLines().collectEntries {
    def f = it.split("\t", -1); [(f[0]): (f.length > 1 ? f[1] : "")] }
check("filter off: rejected is BLANK, not 0",       cfgE["nucleus_circ_rejected"], "")

def outF = new File(tmp, "f"); outF.mkdirs()
def resF = pipe.run(makeImpWithBar("bar"), outF,
                    baseParams + [basename: "circ_on", nucleus_circularity: "0.50-1.00"])
check("filter on: 8 ROIs left",                     resF.nucRois.size(), 8)
// WHICH four went is the assertion that matters -- a count alone would be
// equally satisfied by dropping a disc. The bar is 10 px tall against the
// discs' 50, so height separates them by what actually made them different.
check("filter off: 4 of the 12 were the thin bar",
      resE.nucRois.count { it.getBounds().height <= 20 }, 4)
check("filter on: no thin ROI survives",
      resF.nucRois.count { it.getBounds().height <= 20 }, 0)
def cfgF = new File(outF, "circ_on_config.txt").readLines().collectEntries {
    def f = it.split("\t", -1); [(f[0]): (f.length > 1 ? f[1] : "")] }
check("filter on: the 4 rejects are recorded",      cfgF["nucleus_circ_rejected"], "4")
check("...and the parameter itself is recorded",    cfgF["nucleus_circularity"], "0.50-1.00")

println ""
println "=== overview: the projection, size and contrast parameters are read ==="
def pngBytes = { File dir, String base ->
    def f = new File(dir, base + "_overview_ch1.png")
    return f.isFile() ? f.getBytes() : null
}
def pngDims = { File dir, String base ->
    def im = ij.IJ.openImage(new File(dir, base + "_overview_ch1.png").getPath())
    def d = (im == null) ? null : [im.getWidth(), im.getHeight()]
    im?.close()
    return d
}
def ovParams = baseParams + [save_overview: true, nucleus_threshold: "Otsu"]

def outG = new File(tmp, "g"); outG.mkdirs()
pipe.run(makeImpZVarying("zv"), outG, ovParams + [basename: "ov_max", overview_width: 0])
check("width 0 gives the original 200x200",         pngDims(outG, "ov_max"), [200, 200])

def outH = new File(tmp, "h"); outH.mkdirs()
pipe.run(makeImpZVarying("zv"), outH, ovParams + [basename: "ov_small", overview_width: 60])
check("width 60 gives 60x60 (aspect kept)",         pngDims(outH, "ov_small"), [60, 60])

// NB: compare the projections with contrast OFF. "auto" stretches each
//     picture to full range on its own, so a flat disc at 220 (max) and the
//     same disc at 100 (min) render to identical bytes -- the two PNGs agree
//     while the underlying projections differ, which is the very thing the
//     code comments warn about. Measured: both saved display 0-255.
def outJ0 = new File(tmp, "j0"); outJ0.mkdirs()
pipe.run(makeImpZVarying("zv"), outJ0,
         ovParams + [basename: "ov_maxraw", overview_width: 0,
                     overview_method: "max", overview_contrast: "none"])

def outI = new File(tmp, "i"); outI.mkdirs()
pipe.run(makeImpZVarying("zv"), outI,
         ovParams + [basename: "ov_minraw", overview_width: 0,
                     overview_method: "min", overview_contrast: "none"])
check("min projection differs from max (raw)",
      java.util.Arrays.equals(pngBytes(outJ0, "ov_maxraw"), pngBytes(outI, "ov_minraw")), false)

def outJ = new File(tmp, "j"); outJ.mkdirs()
pipe.run(makeImpZVarying("zv"), outJ,
         ovParams + [basename: "ov_flat", overview_width: 0, overview_contrast: "none"])
check("contrast none differs from auto",
      java.util.Arrays.equals(pngBytes(outG, "ov_max"), pngBytes(outJ, "ov_flat")), false)

println ""
println "=== a bad overview setting fails BEFORE the work, not after ==="
// The overview is the LAST thing the pipeline does. If its settings were
// checked where they are used, a typo would cost a full detection, export and
// measurement pass first -- once here, 1261 times over a tile scan. The
// assertion is therefore not "it threw" but "it threw having written nothing".
def outK = new File(tmp, "k"); outK.mkdirs()
String ovErr = null
try {
    pipe.run(makeImpZVarying("zv"), outK,
             ovParams + [basename: "ov_bad", overview_method: "banana"])
} catch (Throwable t) {
    ovErr = t.getClass().getSimpleName()
}
check("an unknown projection throws",               ovErr, "IllegalArgumentException")
check("...and no outline was written first",
      new File(outK, "ov_bad_nucleus_outline.txt").exists(), false)
check("...nor a config",
      new File(outK, "ov_bad_config.txt").exists(), false)

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} nucleus-pipeline check(s) failed")
