#@ ImagePlus imp
#@ File    (label="Output directory", style="directory") outdir
#@ String  (label="Output prefix", value="") outPrefix
#@ String  (label="Position token in slice label (blank = use title)", value="Position") positionPattern
#@ String  (label="Z-slices to project (blank = all; e.g. 1-20,35-40)", value="") zSpec
#@ String  (label="Channels (comma separated; blank = all)", value="") channelsCsv
#@ String  (label="Projection", choices={"max","mean","median","sum","sd","min"}) method
#@ String  (label="Contrast", choices={"auto","none"}) contrast
#@ Integer (label="Output width in px (0 = original)", value=500) outWidth
#@ Integer (label="Output height in px (0 = follow width)", value=0) outHeight
#@ String  (label="Outlines from", choices={"None","ROI zip file(s)","ROI Manager"}) roiSource
#@ File    (label="ROI zip (optional)", style="file", required=false) roiZip
#@ String  (label="  colour", value="yellow") roiColor
#@ File    (label="Second ROI zip (optional)", style="file", required=false) roiZip2
#@ String  (label="  colour", value="magenta") roiColor2
#@ String  (label="Outline mode", choices={"merged","all","none"}) roiMode
#@ Double  (label="Line width (output px)", value=1.0) lineWidth
#@ String  (label="Output file suffix ((auto) = blank when no outlines, _overlay when there are)", value="(auto)") outSuffix

// Run_Overview.groovy
//
// Write a quick-look PNG per channel: a z-projection of the chosen planes, with
// detected outlines drawn on top.
//
// Two ways to supply outlines:
//   ROI zip file(s)  the *_outline_ROIs.zip written by Run_NucleusSelector. This
//                    works headless, and it means overviews can be regenerated
//                    for a whole folder later, without re-running detection.
//   ROI Manager      whatever is currently loaded (interactive only).
//
// Output: <prefix><image id>_overview_ch<c><suffix>.png in the output directory.
// The suffix defaults to "" for a bare projection and "_overlay" once outlines
// are drawn, so regenerating one never overwrites the other.

import ij.*
import ij.plugin.frame.RoiManager

// Locate the library files next to this script; SciJava injects the running
// script's path into the binding. An unsaved editor buffer supplies neither.
def resolveLibDir = {
    def cands = []
    try { cands << binding.variables["javax.script.filename"] } catch (ignored) {}
    try { cands << binding.variables["org.scijava.script.ScriptModule"]?.getInfo()?.getPath() } catch (ignored) {}
    for (c in cands) {
        if (c) { def f = new File(c.toString()); if (f.exists() && f.getParentFile() != null) return f.getParentFile() }
    }
    return null
}
def libDir = resolveLibDir()
if (libDir == null || !new File(libDir, "Overview.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
LIBDIR = libDir.getAbsolutePath()

def gcl = new GroovyClassLoader(this.class.classLoader)
def OV = gcl.parseClass(new File(LIBDIR + "/Overview.groovy"))
def RX = gcl.parseClass(new File(LIBDIR + "/RoiExport.groovy"))
def RD = gcl.parseClass(new File(LIBDIR + "/RoiDetect.groovy"))

def slices     = RD.parseSlices(zSpec, imp.getNSlices())
def channels   = channelsCsv?.trim() ? channelsCsv.split(",").collect { it.trim() as Integer }
                                     : (1..imp.getNChannels()).toList()
def outDirPath = outdir.getAbsolutePath()
def basename   = outPrefix + RX.resolveImageId(imp, positionPattern)

// --- outlines ---------------------------------------------------------------
// Each layer is [rois, colour]; they are drawn in order, so later layers sit on
// top of earlier ones.
def layers = []
if (roiSource == "ROI zip file(s)") {
    [[roiZip, roiColor], [roiZip2, roiColor2]].each { f, colour ->
        if (f != null && f.isFile()) layers << [RX.loadRoiZip(f.getAbsolutePath()), colour]
    }
    if (layers.isEmpty()) IJ.log("  no ROI zip given -- writing the projection only")
} else if (roiSource == "ROI Manager") {
    if (java.awt.GraphicsEnvironment.isHeadless()) {
        IJ.error("The ROI Manager needs a display. Use 'ROI zip file(s)' when running headless.")
        return
    }
    def rm = RoiManager.getInstance()
    if (rm == null || rm.getCount() == 0) {
        IJ.error("The ROI Manager is empty.")
        return
    }
    layers << [rm.getRoisAsArray() as List, roiColor]
}

// --- output name ------------------------------------------------------------
// The raw projection and the outlined one are different pictures, and they used
// to be written to the same name -- so producing either destroyed the other,
// and montage_qc_cli.r wants both at once.
//
// "(auto)" derives the suffix from whether outlines will actually be drawn,
// rather than defaulting to blank. A blank default would just relocate the
// collision to a convention the operator has to remember: regenerating an
// overlay would still overwrite the raw PNG beside it.
def willDraw = !layers.isEmpty() && roiMode != "none"
def suffix = (outSuffix?.trim() ?: "(auto)") == "(auto)" ? (willDraw ? OV.OVERLAY_SUFFIX : "")
                                                         : outSuffix.trim()

IJ.log("=== " + basename + " (overview) ===")
IJ.log("  projecting " + slices.size() + " of " + imp.getNSlices() + " slices, " + method)

def proj = OV.project(imp, slices, method, channels)

channels.each { int c ->
    def view = OV.prepare(proj, c, [width: outWidth, height: outHeight, contrast: contrast])
    int drawn = 0
    layers.each { rois, colour ->
        drawn += OV.addOutlines(view, rois, [mode: roiMode, color: colour, lineWidth: lineWidth])
    }
    def file = OV.savePng(view, OV.overviewPath(outDirPath, basename, c, suffix))
    // Display range included because "auto" stretches whatever is present: a
    // channel holding only noise saves a convincing picture of nothing, and a
    // narrow range is the only warning.
    IJ.log("  ch" + c + ": " + view.image.getWidth() + "x" + view.image.getHeight() +
           ", display " + IJ.d2s(view.lo, 1) + "-" + IJ.d2s(view.hi, 1) +
           ", " + drawn + " outline(s) -> " + file.getName())
}

IJ.log("Done: " + basename)
