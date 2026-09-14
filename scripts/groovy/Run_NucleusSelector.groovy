#@ ImagePlus imp
#@ File    (label="Output directory", style="directory") outdir
#@ String  (label="Output prefix", value="") outPrefix
#@ String  (label="Position token in slice label (blank = use title)", value="Position") positionPattern
#@ Integer (label="DNA/DAPI channel", value=1) dnaCh
#@ String  (label="Channels to measure (comma separated)", value="1,2,3") channelsCsv
#@ Double  (label="Nucleus: blur sigma", value=8.0) nucSigma
#@ String  (label="Nucleus: threshold method", choices={"Huang2","Huang","Default","Otsu","Triangle","IsoData"}) nucMethod
#@ String  (label="Nucleus: particle size (px^2)", value="40-Infinity") nucSize
#@ Boolean (label="Detect nucleoli", value=true) doNucleoli
#@ Double  (label="Nucleolus: blur sigma", value=2.0) nucleolusSigma
#@ String  (label="Nucleolus: threshold method", choices={"Default","Relative","Otsu","Triangle","Huang","IsoData"}) nucleolusMethod
#@ Double  (label="Nucleolus: relative fraction (if Relative)", value=0.6) relFraction
#@ Integer (label="Nucleolus: shrink nucleus ROI (px)", value=0) erodePx
#@ String  (label="Nucleolus: particle size (px^2)", value="3-300") nucleolusSize
#@ String  (label="Nucleolus: circularity", value="0.50-1.00") nucleolusCirc
#@ Boolean (label="Save ROI zips", value=true) saveRoiZip
#@ Boolean (label="Save outline coordinates", value=true) saveOutlines
#@ Boolean (label="Save measurements", value=true) saveMeasurements

// Run_NucleusSelector.groovy -- version 0.1.0
//
// Groovy port of scripts/fiji/nucleus_selector.ijm.
//
// Detection parameters are script parameters, not edited constants. Detection,
// export and measurement live in separate roles (NucleolusDetect, RoiExport);
// this file only orchestrates.
//
// The ROI Manager is used for ONE thing: Analyze Particles' "add" is still the
// practical way to turn a binary mask into ROIs. Everything downstream works
// from a plain List<Roi>. Replacing that one step (Wand tracing) would make the
// whole pipeline headless.

import ij.*
import ij.gui.*
import ij.plugin.Duplicator
import ij.plugin.RoiEnlarger
import ij.plugin.frame.RoiManager

// Locate the library files next to this script, rather than hardcoding a path.
// SciJava injects the running script's path into the binding; ScriptModule is a
// second route. Both were verified to give an absolute path. An unsaved buffer
// in the Script Editor may supply neither, hence the explicit failure below.
def resolveLibDir = {
    def cands = []
    try { cands << binding.variables["javax.script.filename"] } catch (ignored) {}
    try { cands << binding.variables["org.scijava.script.ScriptModule"]?.getInfo()?.getPath() } catch (ignored) {}
    for (c in cands) {
        if (c) {
            def f = new File(c.toString())
            if (f.exists() && f.getParentFile() != null) return f.getParentFile()
        }
    }
    return null
}

def libDir = resolveLibDir()
if (libDir == null || !new File(libDir, "NucleolusDetect.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\n" +
             "Expected NucleolusDetect.groovy next to this script.\n" +
             "Save this script into scripts/groovy/ and run it from there\n" +
             "(an unsaved editor buffer has no path to resolve from).")
    return
}
LIBDIR = libDir.getAbsolutePath()

def gcl = new GroovyClassLoader(this.class.classLoader)
def ND  = gcl.parseClass(new File(LIBDIR + "/NucleolusDetect.groovy"))
def RX  = gcl.parseClass(new File(LIBDIR + "/RoiExport.groovy"))

// Forced so the output columns do not depend on the operator's Fiji preferences.
MEASUREMENTS = "area mean standard min centroid shape integrated median stack display"
IJ.run("Set Measurements...", MEASUREMENTS + " redirect=None decimal=3")

def channels = channelsCsv.split(",").collect { it.trim() as Integer }
def outDirPath = outdir.getAbsolutePath() + File.separator
def basename = outPrefix + RX.resolveImageId(imp, positionPattern)
IJ.log("=== " + basename + " ===")

def rm = RoiManager.getInstance() ?: new RoiManager()
rm.reset()

// Helper: pull the ROIs added since `from`, name them, and write the outputs.
def exportFeature = { String feature, int from ->
    def all = rm.getRoisAsArray() as List
    if (all.size() <= from) { IJ.log("  no " + feature + " found"); return [] }
    def rois   = all[from..<all.size()]
    def names  = rois.collect { feature + "_" + it.getName() }
    def slices = rois.collect { r -> r.getPosition() > 0 ? r.getPosition() : imp.getSlice() }
    rois.eachWithIndex { r, i -> rm.rename(from + i, names[i]) }
    IJ.log("  " + feature + ": " + rois.size() + " ROIs")

    def stem = outDirPath + basename + "_" + feature
    if (saveOutlines)     RX.saveOutlineCoords(imp, rois, names, slices, basename, stem + "_outline.txt")
    if (saveRoiZip)       RX.saveRoiZip(rois, names, stem + "_outline_ROIs.zip")
    if (saveMeasurements) RX.measureRois(imp, rois, slices, channels, stem + "_res.txt", true)
    return [rois: rois, slices: slices]
}

// --- Nucleus -------------------------------------------------------------
def dna = new Duplicator().run(imp, dnaCh, dnaCh, 1, imp.getNSlices(), 1, 1)
IJ.run(dna, "Gaussian Blur...", "sigma=${nucSigma} stack")
IJ.run(dna, "Auto Threshold", "method=${nucMethod} ignore_black ignore_white white stack use_stack_histogram")
IJ.run(dna, "Fill Holes", "stack")
Prefs.blackBackground = true
dna.show()
IJ.run(dna, "Analyze Particles...", "size=${nucSize} exclude include add stack")
dna.changes = false
dna.close()

def nucleus = exportFeature("nucleus", 0)
int nAfterNucleus = rm.getCount()

// --- Nucleolus -----------------------------------------------------------
if (doNucleoli && nucleus && nucleus.rois) {
    def useRois = (erodePx > 0) ? nucleus.rois.collect { RoiEnlarger.enlarge(it, -erodePx) }
                                : nucleus.rois
    def dna2 = new Duplicator().run(imp, dnaCh, dnaCh, 1, imp.getNSlices(), 1, 1)
    def mask = ND.buildNucleolusMask(dna2, useRois, nucleus.slices,
                                     nucleolusSigma, nucleolusMethod, relFraction)
    dna2.close()

    mask.show()
    IJ.setThreshold(mask, 128, 255)
    IJ.run(mask, "Analyze Particles...",
           "size=${nucleolusSize} circularity=${nucleolusCirc} exclude add stack")
    mask.changes = false
    mask.close()

    exportFeature("nucleolus", nAfterNucleus)
}

imp.show()
IJ.log("Done: " + basename)
