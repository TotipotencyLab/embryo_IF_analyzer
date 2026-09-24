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
println "=== NucleusPipeline.groovy records them ==="

// These used to be asserted against Run_NucleusSelector.groovy. The work moved
// into NucleusPipeline.groovy so the batch runner shares it rather than copying
// it; the assertions follow the code.
def runner = new File(LIBDIR, "NucleusPipeline.groovy")
check("the pipeline library is present",       runner.isFile(), true)

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

// The front end must stay a front end: if the detection calls creep back into
// it, the batch runner and the interactive one have started to diverge again.
// Comment lines are stripped first: these assertions are about what the front
// end DOES, and a comment that merely mentions saveRunConfig() is not a call to
// it. Without this the check fails on its own documentation.
def frontEnd = new File(LIBDIR, "Run_NucleusSelector.groovy").getText("UTF-8")
                   .readLines().findAll { !it.trim().startsWith("//") }.join("\n")
check("front end delegates to the pipeline",   frontEnd.contains("NucleusPipeline.groovy"), true)
check("front end does not detect",             frontEnd.contains("buildMask"), false)
check("front end does not write outlines",     frontEnd.contains("saveOutlineCoords"), false)
check("front end does not write the config",   frontEnd.contains("saveRunConfig"), false)
check("front end still shows the image",       frontEnd.contains("imp.show()"), true)
check("logs the display range",                src.contains("view.lo") && src.contains("view.hi"), true)

// Parse every runner with the SciJava `#@` lines stripped -- they are directives
// to Fiji, not Groovy, and would be a syntax error here. This is the closest
// cheap check to "Fiji would compile this", and it is the only coverage the
// Run_*.groovy scripts have.
println ""
println "=== every `#@` front end still compiles ==="
// Found by CONTENT, not by name prefix. Listing prefixes meant a new verb was
// compiled by nothing at all -- Inspect_ and Open_ scripts were escaping this
// check the moment they were added. Anything carrying a `#@` line is a front
// end and belongs here.
def frontEnds = new File(LIBDIR).listFiles().findAll { f ->
    f.getName().endsWith(".groovy") &&
    f.getText("UTF-8").readLines().any { it.trim().startsWith("#@") }
}.sort()
check("more than one verb prefix is covered",
      frontEnds.collect { it.getName().split("_")[0] }.unique().size() > 1, true)
check("the inspector is covered",
      frontEnds.any { it.getName() == "Inspect_ImageFile.groovy" }, true)
check("the opener is covered",
      frontEnds.any { it.getName() == "Open_LifFile.groovy" }, true)
// A `choices=` parameter with no `value=` is REQUIRED WITH NO DEFAULT, and a
// headless run given no value for it blocks forever waiting for a dialog that
// cannot appear: no output, no error, no exit. Diagnosed from a 284-byte log
// holding nothing but the launcher's two "Unable to locate a Java Runtime"
// lines -- which are normal stderr noise here, and were not the problem.
//
// Naming the first choice explicitly changes nothing, because that is what
// SciJava already picks for the dialog. So there is no reason for any of them
// to be left implicit, and this is cheap to enforce for all of them rather
// than arguing per script about which might one day run headless.
frontEnds.each { f ->
    def bad = f.getText("UTF-8").readLines().findAll {
        it.trim().startsWith("#@") && it.contains("choices=") && !it.contains("value=")
    }
    check("every choices= in " + f.getName() + " names its default", bad, [])
}

frontEnds.each { f ->
    def body = f.getText("UTF-8").readLines().findAll { !(it.trim().startsWith("#@")) }.join("\n")
    String err = null
    try {
        new GroovyClassLoader().parseClass(body, f.getName().replace(".groovy", "_stripped.groovy"))
    } catch (Throwable t) {
        err = t.getClass().getSimpleName() + ": " + t.getMessage()
    }
    check(f.getName(), err, null)
}

println ""
println "=== RunConfig: the read half of the same format ==="

def RC = new GroovyClassLoader().parseClass(new File(LIBDIR, "RunConfig.groovy"))
def NP = new GroovyClassLoader().parseClass(new File(LIBDIR, "NucleusPipeline.groovy"))

// The point of reading the format we write: a run's own config goes back in.
def roundTrip = RC.parse(RC.format([a: "x", b: 2, c: true, d: null]))
check("format/parse round trips keys",         roundTrip.keySet().toList(), ["a", "b", "c", "d"])
check("...and values",                         roundTrip.values().toList(), ["x", "2", "true", ""])

// RoiExport.saveRunConfig() is the OTHER writer. If the two shapes drift, the
// loop this whole class exists for is broken -- so parse its actual output.
def w = new File(tmp, "written_config.txt")
RX.saveRunConfig([timestamp: "now", dna_channel: 3, nucleus_watershed: false], w.getPath())
def readBack = RC.read(w)
check("parses what saveRunConfig wrote",       readBack["dna_channel"], "3")
check("...header is not a parameter",          readBack.containsKey("parameter"), false)

def parsed = RC.parse("# a comment\n\nparameter\tvalue\ndna_channel\t2\n\n#another\n")
check("comments and blank lines are skipped",  parsed.keySet().toList(), ["dna_channel"])

def errOf = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() } }
check("a `key = value` line is refused",
      errOf { RC.parse("dna_channel = 2") }?.contains("TAB"), true)
check("a repeated key is refused",
      errOf { RC.parse("a\t1\na\t2") }?.contains("twice"), true)

println ""
println "=== RunConfig: unknown keys are an error, provenance is not ==="
def types = NP.PARAM_TYPES
check("provenance keys are dropped, not rejected",
      RC.params([timestamp: "now", nucleus_count: "70", dna_channel: "2"], types).keySet().toList(),
      ["dna_channel"])
def unknownMsg = errOf { RC.params([nucleus_sigma: "8"], types) }
check("a near-miss key is refused",            unknownMsg?.contains("nucleus_sigma"), true)
check("...and the message lists what is valid", unknownMsg?.contains("nucleus_blur_sigma"), true)

println ""
println "=== RunConfig: coercion, especially booleans ==="
// In Groovy a non-empty String is truthy, so "false" read from a file would
// ENABLE whatever it guards. This is the check that matters most here.
check("'false' becomes the boolean false",     RC.coerce("k", "false", "boolean"), false)
check("...and is actually falsy",              RC.coerce("k", "false", "boolean") ? "yes" : "no", "no")
check("'0' becomes false",                     RC.coerce("k", "0", "boolean"), false)
check("'true' becomes true",                   RC.coerce("k", "true", "boolean"), true)
check("a bad boolean is refused",              errOf { RC.coerce("k", "maybe", "boolean") }?.contains("boolean"), true)
check("an int coerces",                        RC.coerce("k", "3", "int"), 3)
check("a bad int is refused",                  errOf { RC.coerce("k", "3.5", "int") }?.contains("whole number"), true)
check("a double coerces",                      RC.coerce("k", "8.0", "double"), 8.0d)
check("a bad double is refused",               errOf { RC.coerce("k", "eight", "double") }?.contains("number"), true)

println ""
println "=== NucleusPipeline: the parameter vocabulary ==="
check("every type has a default",              (NP.PARAM_TYPES.keySet() - NP.DEFAULTS.keySet()).toList(), [])
check("every default has a type",              (NP.DEFAULTS.keySet() - NP.PARAM_TYPES.keySet()).toList(), [])
// basename must not be settable from a config: one name for every image in a
// batch would have each overwrite the last.
check("basename is NOT a config parameter",    NP.PARAM_TYPES.containsKey("basename"), false)
check("script_name is NOT a config parameter", NP.PARAM_TYPES.containsKey("script_name"), false)

def defaultTypeErrors = NP.DEFAULTS.findAll { k, v ->
    try { RC.coerce(k, v.toString(), NP.PARAM_TYPES[k]); return false } catch (Throwable t) { return true }
}.keySet().toList()
check("every default matches its own type",    defaultTypeErrors, [])

println ""
println "=== fromConfig: defaults, and the z_spec round trip ==="
def fc = NP.fromConfig([dna_channel: 4])
check("an unset parameter takes its default",  fc.nucleus_blur_sigma, 8.0d)
check("a set parameter wins",                  fc.dna_channel, 4)
// saveRunConfig writes a blank z_spec as the readable "(all)". Fed back in
// unmapped, parseSlices() would reject it -- so a run's own config would fail
// on the one field nobody set.
check("'(all)' maps back to blank",            NP.fromConfig([z_spec: "(all)"]).z_spec, "")
check("a real z_spec is untouched",            NP.fromConfig([z_spec: "1-20"]).z_spec, "1-20")

println ""
println "=== the GUI defaults and the class defaults agree ==="
// SciJava needs the dialog's `value=` to be a literal, so it cannot read
// DEFAULTS. If the two drift, the GUI and the batch do different things under
// the same settings -- silently.
def VAR_TO_PARAM = [
    outPrefix: "output_prefix", positionPattern: "position_pattern", zSpec: "z_spec",
    dnaCh: "dna_channel", channelsCsv: "channels_measured",
    nucSigma: "nucleus_blur_sigma", nucMethod: "nucleus_threshold",
    nucSize: "nucleus_particle_size", nucWatershed: "nucleus_watershed",
    doNucleoli: "nucleoli_enabled", nucleolusSigma: "nucleolus_blur_sigma",
    nucleolusMethod: "nucleolus_threshold", relFraction: "nucleolus_rel_fraction",
    erodePx: "nucleolus_erode_px", nucleolusSize: "nucleolus_particle_size",
    nucleolusCirc: "nucleolus_circularity", saveRoiZips: "save_roi_zips",
    saveOutlines: "save_outlines", saveMeasurements: "save_measurements",
    saveConfig: "save_config", saveOverview: "save_overview",
]
check("the mapping covers every parameter",
      (NP.PARAM_TYPES.keySet() - VAR_TO_PARAM.values().toSet()).toList(), [])

def guiDefaults = [:]
new File(LIBDIR, "Run_NucleusSelector.groovy").eachLine { String line ->
    def m = (line =~ /^#@\s+\w+\s*\((.*)\)\s+(\w+)\s*$/)
    if (!m.find()) return
    String attrs = m.group(1), var = m.group(2)
    if (!VAR_TO_PARAM.containsKey(var)) return
    def vm = (attrs =~ /value\s*=\s*(?:"([^"]*)"|([^,)]+))/)
    def cm = (attrs =~ /choices\s*=\s*\{\s*"([^"]*)"/)
    String got = vm.find() ? (vm.group(1) != null ? vm.group(1) : vm.group(2).trim())
                           : (cm.find() ? cm.group(1) : null)
    if (got != null) guiDefaults[VAR_TO_PARAM[var]] = got
}
check("every parameter's dialog default was found",
      (NP.PARAM_TYPES.keySet() - guiDefaults.keySet()).toList(), [])
def drift = guiDefaults.findAll { k, v -> NP.DEFAULTS[k].toString() != v }
             .collect { k, v -> k + ": dialog=" + v + " class=" + NP.DEFAULTS[k] }
check("dialog and class defaults agree",       drift, [])

println ""
println "=== the fixture's own _config.txt goes back in ==="
// The realistic case, and the one that would embarrass us: an actual file
// written by an actual run, read as parameters.
def fixtureCfg = new File("fixture/if_data/data/GRV_Position010_config.txt")
check("the fixture config is present",         fixtureCfg.isFile(), true)
if (fixtureCfg.isFile()) {
    String fixErr = errOf { RC.readParams(fixtureCfg, types) }
    check("it reads without error",            fixErr, null)
    def fixParams = RC.readParams(fixtureCfg, types)
    check("its nucleus sigma survives",        fixParams.nucleus_blur_sigma, 8.0d)
    check("its threshold survives",            fixParams.nucleus_threshold, "Otsu")
    check("nucleus_count is NOT a parameter",  fixParams.containsKey("nucleus_count"), false)
    // It predates pixel_depth; an absent field must not break the read.
    check("an older config still reads",       fixParams.containsKey("pixel_depth"), false)
    def full = NP.fromConfig(fixParams)
    check("z_spec '(all)' came back blank",    full.z_spec, "")
}

tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} run-config check(s) failed")
