// Inspect_Session.groovy
//
// Report what the running Fiji session currently holds: open images, the active
// image, the ROI Manager, and the measurement settings that will be written into
// the next Results table.
//
// Useful when a script "does nothing": usually the active image is not the one
// you think, the ROI Manager still holds ROIs from the previous run, or
// Set Measurements has been changed and the output columns no longer match what
// the R side expects.
//
// Takes no parameters -- it reports on state, so it deliberately has no `#@`
// lines and pops up no dialog.
//
// ---------------------------------------------------------------------------
// Groovy notes, since this doubles as a reading script:
//
//   * `ij.WindowManager` is the registry of open images. It is the ImageJ1 way;
//     ImageJ2 has its own (`net.imagej.display`). This repo is IJ1 throughout.
//
//   * `imps.each { ... }` iterates with a closure. Inside a closure, `it` is the
//     implicit single parameter when you do not name one.
//
//   * `.collect { }` maps (like R's `sapply`); `.findAll { }` filters (like
//     `Filter`/`subset`); `.sum()`, `.join()`, `.sort()` do what they say.

import ij.IJ
import ij.ImagePlus
import ij.WindowManager
import ij.Prefs
import ij.plugin.frame.RoiManager
import ij.plugin.filter.Analyzer

println "=" * 72
println "ImageJ  : " + IJ.getVersion()
println "Headless: " + java.awt.GraphicsEnvironment.isHeadless()
println "Memory  : " + IJ.freeMemory()          // e.g. "1234MB of 4000MB (30%)"

// --- Open images -----------------------------------------------------------
// getIDList() returns null, not an empty array, when nothing is open -- a
// classic IJ1 trap. `?:` supplies the empty array so the code below is uniform.
int[] ids = WindowManager.getIDList() ?: new int[0]
println ""
println "Open images: ${ids.length}"

if (ids.length > 0) {
    println String.format("  %-38s %6s %6s %4s %4s %4s %5s  %s",
                          "title", "X", "Y", "C", "Z", "T", "bits", "calibration")
    println "  " + "-" * 92
    ids.each { id ->
        ImagePlus imp = WindowManager.getImage(id)
        def cal = imp.getCalibration()
        def calStr = cal.scaled()
            ? String.format("%.4f %s/px", cal.pixelWidth, cal.getUnit())
            : "uncalibrated (pixels)"
        def title = imp.getTitle()
        if (title.length() > 38) title = "..." + title.substring(title.length() - 35)
        println String.format("  %-38s %6d %6d %4d %4d %4d %5d  %s",
                              title, imp.getWidth(), imp.getHeight(),
                              imp.getNChannels(), imp.getNSlices(), imp.getNFrames(),
                              imp.getBitDepth(), calStr)
    }
}

// --- The active image ------------------------------------------------------
// Scripts that take `#@ ImagePlus imp` receive THIS image. When a script seems
// to run against the wrong data, this line is usually the explanation.
def cur = WindowManager.getCurrentImage()
println ""
println "Active image: " + (cur ? "${cur.getTitle()}  (slice ${cur.getCurrentSlice()} of ${cur.getStackSize()})"
                                : "(none)")

// --- ROI Manager -----------------------------------------------------------
// getInstance() returns null when the ROI Manager was never opened. Do NOT call
// `new RoiManager()` just to inspect it: that constructs and shows one, and it
// throws HeadlessException without a display.
def rm = RoiManager.getInstance()
println ""
if (rm == null) {
    println "ROI Manager: not open"
} else {
    def names = rm.getRoisAsArray().collect { it.getName() }
    println "ROI Manager: ${names.size()} ROI(s)"
    if (names) {
        println "  first: " + names.take(3).join(", ")
        println "  last : " + names.takeRight(3).join(", ")
        // Leftover ROIs from a previous run are a common cause of wrong output.
        def prefixes = names.collect { it.replaceAll(/_?\d{4}-\d{4}-\d{4}$/, "") }.unique()
        println "  name prefixes: " + (prefixes ?: ["(none)"]).join(", ")
    }
}

// --- Measurements ----------------------------------------------------------
// Set Measurements is a PERSISTENT USER PREFERENCE, so the columns Fiji writes
// depend on the machine unless a script forces them. The Run_* scripts force
// theirs; anything measured by hand inherits whatever is set here.
println ""
println "Set Measurements (current): " + measurementFlags(Analyzer.getMeasurements())
println "Black background          : " + Prefs.blackBackground
println "Scale conversions         : " + Prefs.get("options.scale", "(unset)")
println "=" * 72

// A plain Groovy method. Declared with `def`, it may be defined after use --
// the script body is compiled as a whole before it runs.
def measurementFlags(int m) {
    def flags = [
        (ij.measure.Measurements.AREA)             : "area",
        (ij.measure.Measurements.MEAN)             : "mean",
        (ij.measure.Measurements.STD_DEV)          : "standard",
        (ij.measure.Measurements.MIN_MAX)          : "min/max",
        (ij.measure.Measurements.CENTROID)         : "centroid",
        (ij.measure.Measurements.SHAPE_DESCRIPTORS): "shape",
        (ij.measure.Measurements.INTEGRATED_DENSITY): "integrated",
        (ij.measure.Measurements.MEDIAN)           : "median",
        (ij.measure.Measurements.STACK_POSITION)   : "stack",
        (ij.measure.Measurements.LABELS)           : "display label"
    ]
    // Bitwise AND against each flag; keep the names whose bit is set.
    def on = flags.findAll { bit, name -> (m & bit) != 0 }.values()
    return on ? on.join(" ") : "(none)"
}
