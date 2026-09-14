#@ ImagePlus imp
#@ Integer (label="DNA/DAPI channel", value=1) dnaCh
#@ Double  (label="Blur sigma", value=2.0) sigma
#@ String  (label="Threshold method", choices={"Default","Triangle","Otsu","Huang","IsoData","Mean","Relative"}) method
#@ Double  (label="Relative fraction (only if method=Relative)", value=0.6) relFraction
#@ Integer (label="Shrink nucleus ROI before thresholding (px)", value=0) erodePx
#@ String  (label="Particle size (px^2)", value="3-300") particleSize
#@ String  (label="Circularity", value="0.50-1.00") circularity
#@ Boolean (label="Show the intermediate mask", value=true) showMask

// Run_NucleolusDetect.groovy -- version 0.1.0
//
// Wires NucleolusDetect.buildNucleolusMask() to the interactive Fiji workflow.
//
// EXPECTS: the nucleus ROIs already sitting in the ROI Manager, i.e. run
//   nucleus_selector.ijm first with run_mask_nucleoli = false.
// APPENDS: the detected nucleolus ROIs to the same ROI Manager, prefixed
//   "nucleolus_", leaving the nucleus ROIs untouched ahead of them.
//
// The mask itself is built headlessly-safe; only the particle extraction below
// needs the GUI, which is why that step lives here rather than in the library.

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

def ND = new GroovyClassLoader(this.class.classLoader)
             .parseClass(new File(LIBDIR + File.separator + "NucleolusDetect.groovy"))

def rm = RoiManager.getInstance()
if (rm == null || rm.getCount() == 0) {
    IJ.error("No ROIs in the ROI Manager.\n\n" +
             "Run nucleus_selector.ijm first (with run_mask_nucleoli = false)\n" +
             "so the nucleus ROIs are present, then run this script.")
    return
}

int nBefore = rm.getCount()
def nucleusRois = rm.getRoisAsArray() as List
IJ.log("Nucleolus detection: " + nBefore + " nucleus ROIs in the manager")

// Single-channel z-stack, so slice indices line up with the ROI positions.
def dna = new Duplicator().run(imp, dnaCh, dnaCh, 1, imp.getNSlices(), 1, 1)

// Resolve each ROI's slice; hyperstack ROIs may carry it as Z rather than position.
def slices = nucleusRois.collect { roi ->
    int z = roi.getZPosition() > 0 ? roi.getZPosition() : roi.getPosition()
    return (z > 0) ? z : imp.getSlice()
}

// Optionally shrink each nucleus, to keep the dim rim out of the histogram
// and out of the mask (the rim is dark, so it can read as nucleolus).
def useRois = nucleusRois
if (erodePx > 0) {
    useRois = nucleusRois.collect { RoiEnlarger.enlarge(it, -erodePx) }
    IJ.log("  nucleus ROIs shrunk by " + erodePx + " px before thresholding")
}

def mask = ND.buildNucleolusMask(dna, useRois, slices, sigma, method, relFraction)
dna.close()

mask.show()
IJ.setThreshold(mask, 128, 255)
IJ.run(mask, "Analyze Particles...",
       "size=${particleSize} circularity=${circularity} exclude add stack")

int nAfter = rm.getCount()
for (int i = nBefore; i < nAfter; i++) {
    rm.rename(i, "nucleolus_" + rm.getName(i))
}
IJ.log("  detected " + (nAfter - nBefore) + " nucleolus ROIs"
       + " (method=" + method + ", sigma=" + sigma + ")")

if (!showMask) mask.close()
imp.show()
