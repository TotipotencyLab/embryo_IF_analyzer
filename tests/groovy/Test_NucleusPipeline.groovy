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
    nucleus_threshold_range: "",
    nucleus_stack_histogram: true,
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

def GCL = new GroovyClassLoader()
def NP = GCL.parseClass(new File(LIBDIR, "NucleusPipeline.groovy"))
// RunConfig, to read a written config back the way a RERUN would -- which is
// the only way to tell a parameter from a provenance field.
def RC = GCL.parseClass(new File(LIBDIR, "RunConfig.groovy"))

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
// The threshold the run actually used, as the RANGE it selected rather than
// the algorithm's bare number -- so it can be copied into a manual threshold
// later without anyone working out which end it was. And the coverage beside
// it, which is the cheap signal that a threshold went wrong in either
// direction: 0.00 selected nothing, a number in the tens selected the frame.
check("the threshold used is recorded as a range",
      cfg["nucleus_threshold_used"] ==~ /\d+-\d+/, true)
check("...with the type maximum as its top",
      cfg["nucleus_threshold_used"].endsWith("-255"), true)
check("mask coverage is recorded, 2 dp",
      cfg["nucleus_mask_pct"] ==~ /\d+\.\d\d/, true)
check("...and the two discs are a few percent of the frame",
      (cfg["nucleus_mask_pct"] as double) > 1.0d && (cfg["nucleus_mask_pct"] as double) < 30.0d, true)
// Provenance, not a parameter: feeding this config forward must re-derive the
// threshold for the image it is given rather than freezing this one.
def asParams = RC.readParams(new File(outB, "sheetAlias_Series001_config.txt"), NP.PARAM_TYPES)
check("threshold_used does NOT read back as a parameter",
      asParams.containsKey("nucleus_threshold_used"), false)
check("...nor does mask_pct",                  asParams.containsKey("nucleus_mask_pct"), false)
check("...while the REQUEST does",             asParams["nucleus_threshold"], "Otsu")

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
println "=== Manual travels through the config, which is the point of it ==="
// The workflow the range format exists for: run auto, read the range off,
// paste it into Manual. It only works if the manual setting survives the
// round trip -- a parameter missing from the written config silently becomes
// the default on the next run.
def outM = new File(tmp, "m"); outM.mkdirs()
def resM = pipe.run(makeImp("probeimage"), outM,
                    baseParams + [basename: "manual", nucleus_threshold: "Manual",
                                  nucleus_threshold_range: "200-255"])
def cfgM = readCfg(new File(outM, "manual_config.txt"))
check("the manual range is recorded",          cfgM["nucleus_threshold_range"], "200-255")
check("...and reads back as a parameter",
      RC.readParams(new File(outM, "manual_config.txt"), NP.PARAM_TYPES)["nucleus_threshold_range"],
      "200-255")
check("...with threshold_used echoing it",     cfgM["nucleus_threshold_used"], "200-255")
// The discs are drawn at 255, so a 200-255 window finds them exactly as Otsu did.
check("...and it found the same 8 ROIs",       resM.nucRois.size(), 8)

// The guard, at the pipeline level rather than the library level.
def outBadM = new File(tmp, "mbad"); outBadM.mkdirs()
String errM = null
try {
    pipe.run(makeImp("probeimage"), outBadM,
             baseParams + [basename: "bad", nucleus_threshold: "Manual"])
} catch (Throwable t) { errM = t.getMessage() }
check("Manual with no range fails the run",    errM?.contains("needs nucleus_threshold_range"), true)
check("...before anything was written",        (outBadM.exists() ? outBadM.listFiles().size() : 0), 0)

println ""
println "=== every parameter is written back, or the round trip is a lie ==="
// THE GUARD THAT WAS MISSING.
//
// _config.txt is a round trip: a run writes it, and RunConfig reads it back as
// the parameters of another run. A key that is DECLARED in PARAM_TYPES but
// never written does not fail -- readParams rejects unknown keys, not absent
// ones -- so the next run silently takes the DEFAULT instead of what ran.
//
// Three of the four couplings around this file already have a set-difference
// assertion in Test_RunConfig (PARAM_TYPES vs the dialog vars, the dialog
// defaults vs DEFAULTS, PARAM_TYPES vs the shipped template) and none of them
// has ever drifted. This one had no assertion and drifted three times: the
// overview_* keys, nucleus_circularity, and the five save_* switches, the last
// found by reading a diff rather than by a test.
//
// Asserted against a config that was actually WRITTEN, not against a regex
// over the source -- scraping the source for key names is how a test of this
// shape comes to agree with itself and with nothing else.
def writtenKeys = readCfg(new File(outA, "TEST_probeimage_config.txt")).keySet()

// The one deliberate exclusion, with its reason, so that its absence reads as a
// decision rather than as the oversight the save_* keys were. output_prefix
// decides what the output is CALLED, not what work happens -- the same category
// as outdir, which is likewise not in the config. Writing it would make a
// re-run reproduce the previous run's filenames and overwrite it instead of
// landing beside it. What the names were is already recorded, as
// output_basename.
def NOT_WRITTEN = ["output_prefix"]

check("every parameter is written back",
      (NP.PARAM_TYPES.keySet() - writtenKeys - NOT_WRITTEN).toList().sort(), [])
// The exclusion has to stay real: if output_prefix ever starts being written,
// this fails and the decision gets revisited rather than quietly reversed.
check("...and the exclusion is still excluded",
      writtenKeys.contains("output_prefix"), false)
// Not vacuous -- the exclusion list must not grow to swallow a real miss.
check("...with exactly one exclusion",         NOT_WRITTEN.size(), 1)

// The five that were missing, by name, so a regression names itself.
["save_roi_zips", "save_outlines", "save_measurements",
 "save_config", "save_overview"].each { String k ->
    check("  " + k + " survives the round trip", writtenKeys.contains(k), true)
}

// And the round trip end to end, on the switch whose default makes the loss
// silent in the dangerous direction: save_overview defaults FALSE, so a run
// that had overviews on and fed its own config forward would write none.
def outRT = new File(tmp, "rt"); outRT.mkdirs()
pipe.run(makeImpZVarying("zv"), outRT,
         baseParams + [basename: "rt", save_overview: true, nucleus_threshold: "Otsu",
                       overview_width: 40])
def rtParams = NP.fromConfig(RC.readParams(new File(outRT, "rt_config.txt"), NP.PARAM_TYPES))
check("a config from an overview run says so",  rtParams.save_overview, true)
check("...and the PNG really was written",
      new File(outRT, "rt_overview_ch1.png").isFile(), true)

def outRT2 = new File(tmp, "rt2"); outRT2.mkdirs()
pipe.run(makeImpZVarying("zv"), outRT2, rtParams + [basename: "rt2", script_name: "rerun"])
check("...so the rerun from that config writes one too",
      new File(outRT2, "rt2_overview_ch1.png").isFile(), true)
// The failure this replaces, stated: before the save_* keys were written, the
// config above carried no save_overview, fromConfig() supplied false, and this
// file was absent while everything else about the rerun looked identical.


println ""
println "=== nucleus_blur_unit: px is the old behaviour, um is a physical size ==="
// analysis-oo_count-physical_blur. Three things to tell apart: "um" converting
// correctly, "um" being read at all, and "px" changing nothing. A run in um must
// equal the same blur given in pixels (correct), and must DIFFER from the same
// number taken as pixels (read) -- identical output alone would be consistent
// with the unit never being looked at.
def cal = { double pw, double ph, String unit ->
    def c = new ij.measure.Calibration(); c.pixelWidth = pw; c.pixelHeight = ph; c.setUnit(unit); c
}
check("px passes the sigma through",           NP.blurSigmaPx(8.0d, "px", cal(0.5d, 0.5d, "micron")), 8.0d)
check("no unit is px (older callers)",         NP.blurSigmaPx(8.0d, null, cal(0.5d, 0.5d, "micron")), 8.0d)
check("um divides by the pixel width",         NP.blurSigmaPx(2.0d, "um", cal(0.5d, 0.5d, "micron")), 4.0d)
check("...and a um-calibrated image works",    NP.blurSigmaPx(1.0d, "um", cal(0.25d, 0.25d, "um")), 4.0d)
def unitErr = { Closure c -> try { c(); "no error" } catch (IllegalArgumentException e) { e.getMessage() } }
check("an unknown unit is refused",
      unitErr { NP.blurSigmaPx(2.0d, "nm", cal(0.5d, 0.5d, "micron")) }.contains("must be one of"), true)
check("um on an uncalibrated image is refused",
      unitErr { NP.blurSigmaPx(2.0d, "um", new ij.measure.Calibration()) }.contains("calibrated in micrometres"), true)
check("um on non-square pixels is refused",
      unitErr { NP.blurSigmaPx(2.0d, "um", cal(0.5d, 0.6d, "micron")) }.contains("square pixels"), true)

// End to end on a calibrated image. The blur has to be large enough to move a
// thresholded edge, or every run would agree whatever the unit did: 8 px on a
// 50 px disc shifts the Otsu contour by about a pixel, 2 px does not.
def makeCalImp = { String title ->
    def imp = makeImp(title)
    imp.getCalibration().pixelWidth = 0.25d; imp.getCalibration().pixelHeight = 0.25d
    imp.getCalibration().setUnit("micron")
    return imp
}
// The discs are 122 um^2 at 0.25 um/px, under baseParams' 200.
def calParams = baseParams + [nucleus_particle_size: "20-Infinity"]
def runU = { String name, double sigma, String unit ->
    def o = new File(tmp, "blur_" + name); o.mkdirs()
    def r = pipe.run(makeCalImp("blur" + name), o, calParams + [nucleus_blur_sigma: sigma, nucleus_blur_unit: unit])
    def c = [:]
    new File(o, "blur" + name + "_config.txt").eachLine { line ->
        def parts = line.split("\t", -1)
        if (parts.length == 2 && parts[0] != "parameter") c[parts[0]] = parts[1]
    }
    return [res: r, cfg: c, geom: stripName(new File(o, "blur" + name + "_nucleus_outline.txt"))]
}
def b8px  = runU("8px", 8.0d, "px")
def b2um  = runU("2um", 2.0d, "um")
def b2px  = runU("2px", 2.0d, "px")
check("8 px finds the discs",                  b8px.res.nucRois.size(), 8)
check("2 um at 0.25 um/px = 8 px: same outlines",
      (b8px.geom != null && b8px.geom == b2um.geom), true)
check("2 px is a different blur: outlines differ",
      (b2px.geom != null && b2px.geom != b2um.geom), true)
check("the config records the unit",           b2um.cfg["nucleus_blur_unit"], "um")
check("...the sigma as given",                 b2um.cfg["nucleus_blur_sigma"], "2.0")
check("...and the pixel sigma it became",      b2um.cfg["nucleus_blur_sigma_px_used"], "8.0")
check("a px run records px and its own sigma", [b8px.cfg["nucleus_blur_unit"], b8px.cfg["nucleus_blur_sigma_px_used"]],
                                                ["px", "8.0"])
def umParams = RC.readParams(new File(tmp, "blur_2um/blur2um_config.txt"), NP.PARAM_TYPES)
check("the unit reads back as a parameter",    umParams.nucleus_blur_unit, "um")
check("the pixel sigma does NOT read back",    umParams.containsKey("nucleus_blur_sigma_px_used"), false)
// Fails before any output, not after detection.
def oUncal = new File(tmp, "blur_uncal"); oUncal.mkdirs()
check("um on an uncalibrated image fails before writing",
      unitErr { pipe.run(makeImp("uncal"), oUncal, calParams + [nucleus_blur_sigma: 2.0d, nucleus_blur_unit: "um"]) }
          .contains("calibrated in micrometres") && oUncal.listFiles().size() == 0, true)

println ""
println "=== every ImagePlus close() is paired with flush() ==="

// ImagePlus.close() does NOT release pixels while a reference is still in
// scope: it detaches a window, and headless there is no window. Measured on a
// 768 MB stack -- close() with the variable still live freed 0 MB, flush()
// freed 730 MB. Every intermediate in this pipeline is a full copy of the
// image, so one unflushed "closed" image is gigabytes carried through every
// later stage. That is what still exhausted the heap on an 11344 x 9590 x 25
// tile merge after RoiDetect.buildMask() had been fixed: the nucleus mask was
// closed, 2594 MB, and alive right through the overview projection.
//
// A source assertion rather than a checklist, for the reason CLAUDE.md gives:
// a checklist is something a person has to remember to read.
def bareCloses = { String path ->
    def bad = []
    new File(path).readLines().eachWithIndex { String line, int i ->
        String code = line.replaceFirst(/\/\/.*$/, "")
        if (code =~ /\.close\(\)/ && !(code =~ /\.flush\(\)/)) {
            // Readers and streams have no flush() of this kind and are not images.
            if (!(code =~ /(?i)(reader|stream|writer|scanner)\s*\.close\(\)/)) {
                bad << ((path.split("/")[-1]) + ":" + (i + 1) + " " + code.trim())
            }
        }
    }
    return bad
}

def unpaired = []
["scripts/groovy/NucleusPipeline.groovy",
 "scripts/groovy/Overview.groovy",
 "scripts/groovy/RoiDetect.groovy",
 "scripts/groovy/NucleolusDetect.groovy",
 "scripts/groovy/BatchRunner.groovy"].each { unpaired.addAll(bareCloses(it)) }
check("no close() without flush() in the library", unpaired, [])

// Once everything has been read back, not partway through: sections added after
// this line used to run against a directory that had already been deleted.
tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} nucleus-pipeline check(s) failed")
