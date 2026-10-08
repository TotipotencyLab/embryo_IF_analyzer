// Test_NucleusPipeline.groovy
//
// The pipeline extracted out of Run_NucleusSelector.groovy, exercised directly.
//
// The image is synthesised, so this needs no fixture and runs in seconds. What
// it covers is the part a reference diff on real data CANNOT cover: the
// `series_id` override. A reference run proves the override
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
println "=== series_id OFF: taken from the image title ==="
def outA = new File(tmp, "a"); outA.mkdirs()
def nameCol = { File f ->
    f.isFile() ? f.getText("UTF-8").readLines().drop(1).collect { it.split("\t", -1)[0] }.unique() : null
}
def resA = pipe.run(makeImp("probeimage"), outA, baseParams)
check("series_id taken from the title",        resA.series_id, "probeimage")
check("8 ROIs found (2 discs x 4 slices)",     resA.nucRois.size(), 8)
check("outline written under that name",
      new File(outA, "probeimage_nucleus_outline.txt").isFile(), true)
check("config written under that name",
      new File(outA, "probeimage_config.txt").isFile(), true)
// The point of v0.7.0's change: the name column and the file stem are ONE
// string, so R can find <name>_config.txt from a name it read out of a table.
check("the name column IS the file stem",
      nameCol(new File(outA, "probeimage_nucleus_outline.txt")), ["probeimage"])

println ""
println "=== series_id ON: the caller's id wins ==="
def outB = new File(tmp, "b"); outB.mkdirs()
def resB = pipe.run(makeImp("probeimage"), outB,
                    baseParams + [series_id: "sheetAlias_Series001"])
check("series_id is the one supplied",         resB.series_id, "sheetAlias_Series001")
check("outline uses the supplied id",
      new File(outB, "sheetAlias_Series001_nucleus_outline.txt").isFile(), true)
check("...and so does its name column",
      nameCol(new File(outB, "sheetAlias_Series001_nucleus_outline.txt")), ["sheetAlias_Series001"])
check("the title is NOT used",
      new File(outB, "probeimage_nucleus_outline.txt").exists(), false)

println ""
println "=== a given id that cannot name a file is refused, not rewritten ==="
def outBad = new File(tmp, "badid"); outBad.mkdirs()
String badIdErr = null
try { pipe.run(makeImp("probeimage"), outBad, baseParams + [series_id: "my embryo"]) }
catch (IllegalArgumentException e) { badIdErr = e.getMessage() }
check("refused, offering the clean form",      badIdErr?.contains("'my_embryo'"), true)
check("...before anything was written",        outBad.list().toList(), [])

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
def geomA = stripName(new File(outA, "probeimage_nucleus_outline.txt"))
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
check("series_id is recorded",                 cfg["series_id"], "sheetAlias_Series001")
check("output_basename is gone (same as series_id)", cfg.containsKey("output_basename"), false)
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
pipe.run(impC, outC, baseParams + [series_id: "stk"])
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
pipe.run(impD, outD, baseParams + [series_id: "pln"])
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
def resE = pipe.run(makeImpWithBar("bar"), outE, baseParams + [series_id: "circ_off"])
check("filter off: 12 ROIs (2 discs + 1 bar) x 4",  resE.nucRois.size(), 12)
def cfgE = new File(outE, "circ_off_config.txt").readLines().collectEntries {
    def f = it.split("\t", -1); [(f[0]): (f.length > 1 ? f[1] : "")] }
check("filter off: rejected is BLANK, not 0",       cfgE["nucleus_circ_rejected"], "")

def outF = new File(tmp, "f"); outF.mkdirs()
def resF = pipe.run(makeImpWithBar("bar"), outF,
                    baseParams + [series_id: "circ_on", nucleus_circularity: "0.50-1.00"])
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
pipe.run(makeImpZVarying("zv"), outG, ovParams + [series_id: "ov_max", overview_width: 0])
check("width 0 gives the original 200x200",         pngDims(outG, "ov_max"), [200, 200])

def outH = new File(tmp, "h"); outH.mkdirs()
pipe.run(makeImpZVarying("zv"), outH, ovParams + [series_id: "ov_small", overview_width: 60])
check("width 60 gives 60x60 (aspect kept)",         pngDims(outH, "ov_small"), [60, 60])

// NB: compare the projections with contrast OFF. "auto" stretches each
//     picture to full range on its own, so a flat disc at 220 (max) and the
//     same disc at 100 (min) render to identical bytes -- the two PNGs agree
//     while the underlying projections differ, which is the very thing the
//     code comments warn about. Measured: both saved display 0-255.
def outJ0 = new File(tmp, "j0"); outJ0.mkdirs()
pipe.run(makeImpZVarying("zv"), outJ0,
         ovParams + [series_id: "ov_maxraw", overview_width: 0,
                     overview_method: "max", overview_contrast: "none"])

def outI = new File(tmp, "i"); outI.mkdirs()
pipe.run(makeImpZVarying("zv"), outI,
         ovParams + [series_id: "ov_minraw", overview_width: 0,
                     overview_method: "min", overview_contrast: "none"])
check("min projection differs from max (raw)",
      java.util.Arrays.equals(pngBytes(outJ0, "ov_maxraw"), pngBytes(outI, "ov_minraw")), false)

def outJ = new File(tmp, "j"); outJ.mkdirs()
pipe.run(makeImpZVarying("zv"), outJ,
         ovParams + [series_id: "ov_flat", overview_width: 0, overview_contrast: "none"])
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
             ovParams + [series_id: "ov_bad", overview_method: "banana"])
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
                    baseParams + [series_id: "manual", nucleus_threshold: "Manual",
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
             baseParams + [series_id: "bad", nucleus_threshold: "Manual"])
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
def writtenKeys = readCfg(new File(outA, "probeimage_config.txt")).keySet()

// No exclusions since v0.7.0. output_prefix was the one -- it named the output
// rather than deciding the analysis -- and it is retired; the series id that
// replaced it is not a parameter at all, and is written as provenance.
check("every parameter is written back",
      (NP.PARAM_TYPES.keySet() - writtenKeys).toList().sort(), [])
check("...and the series id is recorded",      writtenKeys.contains("series_id"), true)
// Retired keys must not reappear in a new config: one written today would
// otherwise carry a key a later reader skips, and look as though it mattered.
check("...and no retired key is written",
      writtenKeys.findAll { RC.RETIRED_KEYS.containsKey(it) }.toList(), [])

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
         baseParams + [series_id: "rt", save_overview: true, nucleus_threshold: "Otsu",
                       overview_width: 40])
def rtParams = NP.fromConfig(RC.readParams(new File(outRT, "rt_config.txt"), NP.PARAM_TYPES))
check("a config from an overview run says so",  rtParams.save_overview, true)
check("...and the PNG really was written",
      new File(outRT, "rt_overview_ch1.png").isFile(), true)

def outRT2 = new File(tmp, "rt2"); outRT2.mkdirs()
pipe.run(makeImpZVarying("zv"), outRT2, rtParams + [series_id: "rt2", script_name: "rerun"])
check("...so the rerun from that config writes one too",
      new File(outRT2, "rt2_overview_ch1.png").isFile(), true)
// The failure this replaces, stated: before the save_* keys were written, the
// config above carried no save_overview, fromConfig() supplied false, and this
// file was absent while everything else about the rerun looked identical.


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

println ""
println "=== the time axis: every image gets t, a multi-frame one is analysed per frame ==="
def RX = GCL.parseClass(new File(LIBDIR, "RoiExport.groovy"))
def readTsv = { File f ->
    def lines = f.readLines()
    def head = lines[0].split("\t", -1).toList()
    return [head: head, rows: lines.drop(1).collect { l ->
        def v = l.split("\t", -1)
        def m = [:]; head.eachWithIndex { h, i -> m[h] = (i < v.length ? v[i] : "") }; m }]
}

// Single frame first: the columns are there, counted from 1, and ImageJ's own
// position columns are not.
def outOne = new File(tmp, "single"); outOne.mkdirs()
pipe.run(makeImp("one"), outOne, baseParams + [series_id: "one"])
def oOne = readTsv(new File(outOne, "one_nucleus_outline.txt"))
check("outline header has t, before z",        oOne.head, ["name", "roi", "t", "z", "x", "y"])
check("one frame is t = 1 everywhere",         oOne.rows.collect { it.t }.unique(), ["1"])
def rOne = readTsv(new File(outOne, "one_nucleus_res.txt"))
check("res carries roi, z, t, ch",             rOne.head.containsAll(["roi", "z", "t", "ch"]), true)
// Identity first, then ImageJ's measurements in ImageJ's order.
check("res leads with row number, Label, roi, z, t, ch",
      rOne.head.take(6), [" ", "Label", "roi", "z", "t", "ch"])
check("...then ImageJ's columns, starting at Area", rOne.head[6], "Area")
check("no .part file left behind",             outOne.list().findAll { it.endsWith(".part") }.toList(), [])
check("...and not ImageJ's Ch or Slice",       rOne.head.findAll { it in ["Ch", "Slice", "Frame"] }, [])
// The explicit columns against the Label, which carries the same facts in
// ImageJ's words: <title>:<roi name>:<slice label>.
check("res roi is the name in the Label",
      rOne.rows.every { it.Label.split(":")[1] == it.roi }, true)
check("res z is the slice the roi name says",
      rOne.rows.every { (it.roi =~ /_(\d{4})-\d{4}-\d{4}$/)[0][1] as int == it.z as int }, true)
check("res ch is the measured channel",        rOne.rows.collect { it.ch }.unique(), ["1"])
def tsOne = readTsv(new File(outOne, "one_threshold_stats.tsv"))
check("threshold stats: one row for one frame", tsOne.rows.size(), 1)
check("...with the documented columns",        tsOne.head, RX.THRESHOLD_STATS_COLUMNS)
def cfgOne = readCfg(new File(outOne, "one_config.txt"))
check("config: image_frames 1",                cfgOne.image_frames, "1")
check("config: no frame interval for one frame", cfgOne.frame_interval, "")
check("config: the threshold as a range, as before", cfgOne.nucleus_threshold_used ==~ /\d+-\d+/, true)
check("...and it equals the stats row's",      cfgOne.nucleus_threshold_used, tsOne.rows[0].nucleus_threshold_used)
check("config: measurements no longer ask for stack",
      cfgOne.measurements.split(" ").contains("stack"), false)

// Four frames, two channels. The discs move and the signal brightens frame by
// frame, so a frame measured as another, or all frames measured as the first,
// cannot pass.
def makeTimeImp = { String title ->
    def st = new ImageStack(200, 200)
    (1..4).each { int t ->
        (1..3).each { int z ->
            def dna = new ByteProcessor(200, 200)
            dna.setColor(255)
            dna.fill(new OvalRoi(30 + 5 * t, 30, 50, 50))
            dna.fill(new OvalRoi(120, 110 + 5 * t, 50, 50))
            def sig = new ByteProcessor(200, 200)
            sig.setColor(20 * t + 10 * z)
            sig.fill(new Roi(0, 0, 200, 200))
            st.addSlice("c:1/2 z:" + z + "/3 t:" + t + "/4", dna)
            st.addSlice("c:2/2 z:" + z + "/3 t:" + t + "/4", sig)
        }
    }
    def imp = new ImagePlus(title, st)
    imp.setDimensions(2, 3, 4)
    imp.setOpenAsHyperStack(true)
    imp.getCalibration().frameInterval = 30d
    imp.getCalibration().setTimeUnit("sec")
    return imp
}
def mParams = baseParams + [channels_measured: "1,2"]
def outTL = new File(tmp, "multi"); outTL.mkdirs()
def timeImp = makeTimeImp("tl")
def resTL = pipe.run(timeImp, outTL, mParams + [series_id: "tl", save_overview: true])
def oTL = readTsv(new File(outTL, "tl_nucleus_outline.txt"))
def rTL = readTsv(new File(outTL, "tl_nucleus_res.txt"))
check("4 frames x 2 discs x 3 slices = 24 ROIs", resTL.nucRois.size(), 24)
check("every ROI id is distinct",               resTL.nucNames.unique(false).size(), 24)
check("ids carry TTTT- equal to their frame",
      oTL.rows.every { (it.roi =~ /^nucleus_(\d{4})-\d{4}-\d{4}-\d{4}$/)[0][1] as int == it.t as int }, true)
check("outline t runs 1..4",                    oTL.rows.collect { it.t as int }.unique().sort(), [1, 2, 3, 4])
check("res t runs 1..4",                        rTL.rows.collect { it.t as int }.unique().sort(), [1, 2, 3, 4])
check("res rows = ROIs x 2 channels",           rTL.rows.size(), 48)
check("res ch is 1 and 2",                      rTL.rows.collect { it.ch }.unique().sort(), ["1", "2"])
// The signal is 20t + 10z in channel 2, so its Mean says which frame and slice
// were measured -- not just which were written in the t column.
check("each ch2 Mean is its own frame's signal",
      rTL.rows.findAll { it.ch == "2" }.every { (it.Mean as double) == 20d * (it.t as int) + 10d * (it.z as int) }, true)
// The frame copy keeps the series' title, so the Label still names the image.
check("the Label names the image, not a DUP_ copy",
      rTL.rows.every { it.Label.startsWith("tl:") }, true)

// Each frame analysed in the loop must equal that frame analysed ALONE -- the
// proof that the loop hands over the frame rather than a neighbour, and adds
// nothing but t and the id's frame field.
def frameAloneSame = (1..4).every { int t ->
    def outAlone = new File(tmp, "alone" + t); outAlone.mkdirs()
    pipe.run(NP.frameOf(timeImp, t), outAlone, mParams + [series_id: "tl"])
    def alone = readTsv(new File(outAlone, "tl_nucleus_res.txt")).rows
    def inLoop = rTL.rows.findAll { it.t == t.toString() }
    def strip = { List rows -> rows.collect { r ->
        r.findAll { k, v -> !(k in ["roi", "t", "Label", " "]) } +
        [roi: r.roi.replaceFirst(/_\d{4}-(\d{4}-\d{4}-\d{4})$/, '_$1')] } }
    strip(alone) == strip(inLoop)
}
check("every frame equals that frame analysed alone", frameAloneSame, true)

def tsTL = readTsv(new File(outTL, "tl_threshold_stats.tsv"))
check("threshold stats: one row per frame",     tsTL.rows.collect { it.t }, ["1", "2", "3", "4"])
check("...counts sum to the total",             tsTL.rows.sum { it.nucleus_count as int }, 24)
def cfgTL = readCfg(new File(outTL, "tl_config.txt"))
check("config: image_frames 4",                 cfgTL.image_frames, "4")
check("config: frame interval from the calibration", [cfgTL.frame_interval, cfgTL.frame_unit], ["30.0", "sec"])
check("config: threshold is `per-frame`",       [cfgTL.nucleus_threshold_used, cfgTL.nucleus_mask_pct], ["per-frame", "per-frame"])
check("config: the count is the total",         cfgTL.nucleus_count, "24")
check("no overview PNG for a multi-frame image",
      outTL.list().findAll { it.endsWith(".png") }.toList(), [])
// Its overview is a TIFF per channel, a page per frame (Test_Overview and
// Test_SeriesSource check what is on the pages).
def tifsTL = outTL.list().findAll { it.endsWith(".tif") }.sort()
check("...but a TIFF per channel, and its overlay", tifsTL,
      cfgTL.overview_channels.split(",").collect { ["tl_overview_ch" + it + ".tif", "tl_overview_ch" + it + "_overlay.tif"] }.flatten().sort())
check("...a page per frame",                    ij.IJ.openImage(new File(outTL, tifsTL[0]).getPath()).getNFrames(), 4)
check("...and the config says it was written",  [cfgTL.overview_saved, cfgTL.overview_display_range.split(" ").size()],
      ["true", cfgTL.overview_channels.split(",").size()])
def zipped = RX.loadRoiZip(new File(outTL, "tl_nucleus_outline_ROIs.zip").getPath())
check("the zip holds all 24",                   zipped.size(), 24)
check("each zipped ROI sits on its own frame",
      zipped.every { r -> r.getTPosition() == ((r.getName() =~ /^nucleus_(\d{4})-/)[0][1] as int) }, true)
check("...and its own slice",
      zipped.every { r -> r.getZPosition() == ((r.getName() =~ /-(\d{4})-\d{4}-\d{4}$/)[0][1] as int) }, true)

println ""
println "=== a failed ROI zip leaves nothing behind ==="
def zipDir = new File(tmp, "zipfail"); zipDir.mkdirs()
def zipPath = new File(zipDir, "x_ROIs.zip").getPath()
// A repeated entry name is what four frames of one object would have produced
// without TTTT-: ZipOutputStream throws on the second.
def oneRoi = new OvalRoi(10, 10, 20, 20)
String zipErr = null
try { RX.saveRoiZip([oneRoi, oneRoi], ["dup", "dup"], zipPath) } catch (Throwable t) { zipErr = t.getClass().getSimpleName() }
check("a repeated name throws",                 zipErr != null, true)
check("...and leaves no zip and no part file",  zipDir.list().toList(), [])

println ""
println "=== resume: an interrupted series carries on from the frames it staged ==="
// The time-lapse again, with a dim spot in each disc so nucleoli are found too:
// a resumed frame has to give back BOTH features' ROIs, and the overlay TIFF
// draws them.
def makeSpotImp = { String title ->
    def imp = makeTimeImp(title)
    (1..4).each { int t ->
        (1..3).each { int z ->
            def ip = imp.getStack().getProcessor(imp.getStackIndex(1, z, t))
            ip.setColor(60)
            ip.fill(new OvalRoi(30 + 5 * t + 18, 48, 12, 12))
            ip.fill(new OvalRoi(138, 110 + 5 * t + 18, 12, 12))
        }
    }
    return imp
}
def rsParams = mParams + [series_id: "rs", save_overview: true, nucleoli_enabled: true]
def SS = pipe.SS
def rsImp = makeSpotImp("rs")
// The source, watched: which frames were read, and -- for the interrupted run --
// the run dying after frame `stopAfter`. It dies from release(), outside the
// per-frame catch, which is the path a killed run's staging is left by.
def watched = { int stopAfter ->
    def inner = SS.ofImage(rsImp, "", false)
    def read = []
    def w = new Expando()
    ["title", "whole", "nFrames", "nSlices", "nChannels", "width", "height", "calibration",
     "frameList", "frameInterval", "frameUnit"].each { w[it] = inner[it] }
    w.choose  = { List wanted -> inner.choose(wanted) }
    w.frame   = { int t -> read << t; inner.frame(t) }
    w.release = { ImagePlus f ->
        inner.release(f)
        if (stopAfter > 0 && read && read[-1] == stopAfter)
            throw new IllegalStateException("simulated: the run dies after frame " + stopAfter)
    }
    w.read = read
    return w
}
def interrupt = { File out, Map params, int k ->
    String err = null
    try { pipe.runSource(watched(k), out, params, null, true) } catch (Throwable e) { err = e.getMessage() }
    return err
}
// Zip entries, not zip bytes: ZipOutputStream stamps each entry with the time it
// was written, so two uninterrupted runs differ there too.
def zipEntries = { File f ->
    def zis = new java.util.zip.ZipInputStream(new FileInputStream(f)), m = [:]
    try {
        def e
        while ((e = zis.getNextEntry()) != null) {
            def buf = new ByteArrayOutputStream(); byte[] b = new byte[8192]; int n
            while ((n = zis.read(b)) > 0) buf.write(b, 0, n)
            m[e.getName()] = buf.toByteArray().encodeHex().toString()
        }
    } finally { zis.close() }
    return m
}
// Every file the series wrote, as comparable values: bytes, zip entries, and
// the config without its timestamp.
def written = { File out ->
    out.listFiles().findAll { it.isFile() }.sort { it.getName() }.collectEntries { File f ->
        def v = f.getName().endsWith(".zip") ? zipEntries(f)
              : f.getName().endsWith("_config.txt") ? f.readLines().findAll { !it.startsWith("timestamp\t") }
              : f.getBytes().encodeHex().toString()
        [(f.getName()): v]
    }
}
def stageOf = { File out -> new File(new File(out, NP.STAGING_DIR), "rs") }

// The reference: one uninterrupted run.
def rsRef = new File(tmp, "rs_ref"); rsRef.mkdirs()
def wRef = watched(-1)
def resRef = pipe.runSource(wRef, rsRef, rsParams, null, false)
def ref = written(rsRef)
check("reference read every frame",             wRef.read, [1, 2, 3, 4])
check("reference found nucleoli (so they are tested)", resRef.nuclRois.size() > 0, true)
println "    reference: " + resRef.nucRois.size() + " nuclei, " + resRef.nuclRois.size() + " nucleoli, " +
        ref.size() + " files"

// Interrupted after frame 2, then a half-written frame 3 planted, as a kill
// mid-write leaves one.
def rsOut = new File(tmp, "rs_resumed"); rsOut.mkdirs()
def errI = interrupt(rsOut, rsParams, 2)
check("the interrupted run died",               errI?.startsWith("simulated"), true)
check("...leaving frames 1 and 2 staged, and its settings",
      stageOf(rsOut).list().sort().toList(), ["settings.txt", "t0001", "t0002"])
check("...and no series file written",          rsOut.listFiles().findAll { it.isFile() }.size(), 0)
def junk = new File(stageOf(rsOut), "t0003.part"); junk.mkdirs()
new File(junk, "nucleus_res.txt").setText("half a table")

def wRes = watched(-1)
def resRes = pipe.runSource(wRes, rsOut, rsParams, null, true)
check("the resumed run read only frames 3 and 4", wRes.read, [3, 4])
check("...every file the same as the uninterrupted run's", written(rsOut), ref)
check("...including the ROIs it returns",       [resRes.nucNames, resRes.nuclNames], [resRef.nucNames, resRef.nuclNames])
check("...frames 1-2 marked as an earlier run's, with no time",
      resRes.frames.collect { [it.t, it.status, it.message, it.seconds] }.take(2),
      [[1, "ok", NP.RESUMED_MESSAGE, null], [2, "ok", NP.RESUMED_MESSAGE, null]])
check("...frames 3-4 as this run's",            resRes.frames.drop(2).collect { [it.t, it.status, it.message] },
      [[3, "ok", ""], [4, "ok", ""]])
check("...and the staging gone",                new File(rsOut, NP.STAGING_DIR).exists(), false)
// The joined ROI zip and the overlay are compared above; this says the
// comparison had something in it.
check("...the overlay TIFFs among what was compared",
      ref.keySet().findAll { it.endsWith("_overlay.tif") }.size(), 2)

// Other settings: refused before a frame is read, the staging untouched. Then
// resume=false (the batch's redo_all) discards it.
def rsOther = new File(tmp, "rs_other"); rsOther.mkdirs()
interrupt(rsOther, rsParams, 2)
def wOther = watched(-1)
String errO = null
try { pipe.runSource(wOther, rsOther, rsParams + [nucleus_blur_sigma: 1.0d], null, true) }
catch (IllegalStateException e) { errO = e.getMessage() }
println "    refusal: " + errO
check("other settings are refused",             errO?.contains("nucleus_blur_sigma '0.0' then, '1.0' now"), true)
check("...naming only what differs",            errO?.count(" then, "), 1)
check("...before a frame is read",              wOther.read, [])
check("...leaving the staged frames alone",     stageOf(rsOther).list().sort().toList(), ["settings.txt", "t0001", "t0002"])
def wRestart = watched(-1)
pipe.runSource(wRestart, rsOther, rsParams + [nucleus_blur_sigma: 1.0d], null, false)
check("resume=false reads every frame",         wRestart.read, [1, 2, 3, 4])
check("...and its config has the new setting",  readCfg(new File(rsOther, "rs_config.txt")).nucleus_blur_sigma, "1.0")

// Frames staged with no record of their settings -- as 0.8.0-dev before this
// wrote one -- are refused too: nobody can vouch for them.
def rsBare = new File(tmp, "rs_bare"); rsBare.mkdirs()
interrupt(rsBare, rsParams, 2)
new File(stageOf(rsBare), NP.STAGING_SETTINGS).delete()
String errB = null
try { pipe.runSource(watched(-1), rsBare, rsParams, null, true) } catch (IllegalStateException e) { errB = e.getMessage() }
check("staged frames without settings are refused", errB?.contains("without recording its settings"), true)

// A resume may ask for other frames than the run it continues: 2 is reused,
// 3 analysed, and 1 -- staged but not asked for -- is not joined.
def rsSome = new File(tmp, "rs_some"); rsSome.mkdirs()
interrupt(rsSome, rsParams, 2)
def wSome = watched(-1)
pipe.runSource(wSome, rsSome, rsParams, [2, 3], true)
check("a resume over frames 2-3 reads only 3",  wSome.read, [3])
check("...and its results hold 2 and 3",        readCfg(new File(rsSome, "rs_config.txt")).frames_analysed, "2 3")
check("...outline t is 2 and 3",
      readTsv(new File(rsSome, "rs_nucleus_outline.txt")).rows.collect { it.t }.unique(), ["2", "3"])

// The zip is staged for every frame now, for the resume to read; it must still
// not be WRITTEN when save_roi_zips is off.
def rsNoZip = new File(tmp, "rs_nozip"); rsNoZip.mkdirs()
pipe.runSource(watched(-1), rsNoZip, rsParams + [save_roi_zips: false], null, true)
check("save_roi_zips off writes no zip",        rsNoZip.list().findAll { it.endsWith(".zip") }.toList(), [])
check("...but the outlines",                    new File(rsNoZip, "rs_nucleus_outline.txt").isFile(), true)

def unpaired = []
["scripts/groovy/NucleusPipeline.groovy",
 "scripts/groovy/Overview.groovy",
 "scripts/groovy/RoiDetect.groovy",
 "scripts/groovy/NucleolusDetect.groovy",
 "scripts/groovy/BatchRunner.groovy",
 "scripts/groovy/SeriesSource.groovy"].each { unpaired.addAll(bareCloses(it)) }
check("no close() without flush() in the library", unpaired, [])

// Once everything has been read back, not partway through: sections added after
// this line used to run against a directory that had already been deleted.
tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} nucleus-pipeline check(s) failed")
