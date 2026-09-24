#@ ImagePlus imp
#@ File    (label="Output directory", style="directory") outdir
#@ String  (label="Output prefix", value="") outPrefix
#@ String  (label="Position token in slice label (blank = use title)", value="Position") positionPattern
#@ String  (label="Z-slices to analyse (blank = all; e.g. 1-20,35-40)", value="") zSpec
#@ Integer (label="DNA/DAPI channel", value=1) dnaCh
#@ String  (label="Channels to measure (comma separated)", value="1,2,3") channelsCsv
#@ Double  (label="Nucleus: blur sigma", value=8.0) nucSigma
#@ String  (label="Nucleus: threshold method", value="Huang2", choices={"Huang2","Huang","Default","Otsu","Triangle","IsoData"}) nucMethod
#@ String  (label="Nucleus: particle size (calibrated units^2)", value="80-Infinity") nucSize
#@ Boolean (label="Nucleus: split touching nuclei (watershed)", value=false) nucWatershed
#@ Boolean (label="Detect nucleoli", value=true) doNucleoli
#@ Double  (label="Nucleolus: blur sigma", value=3.0) nucleolusSigma
#@ String  (label="Nucleolus: threshold method", value="Relative", choices={"Relative","Default","Otsu","Triangle","Huang","IsoData"}) nucleolusMethod
#@ Double  (label="Nucleolus: relative fraction (if Relative)", value=0.6) relFraction
#@ Integer (label="Nucleolus: shrink nucleus ROI (px)", value=0) erodePx
#@ String  (label="Nucleolus: particle size (calibrated units^2)", value="3-150") nucleolusSize
#@ String  (label="Nucleolus: circularity", value="0.50-1.00") nucleolusCirc
#@ Boolean (label="Add ROIs to ROI Manager (needs GUI)", value=true) addToRoiManager
#@ Boolean (label="Save ROI zips", value=true) saveRoiZips
#@ Boolean (label="Save outline coordinates", value=true) saveOutlines
#@ Boolean (label="Save measurements", value=true) saveMeasurements
#@ Boolean (label="Save run configuration", value=true) saveConfig
#@ Boolean (label="Save overview PNG (quick visual check)", value=false) saveOverview

// Run_NucleusSelector.groovy
//
// Nucleus + nucleolus detection, export and measurement, for the ACTIVE image.
//
// This is the interactive entry point: a `#@` block, and one call. The work
// itself lives in NucleusPipeline.groovy so that the batch runner -- which
// opens its own images from a sample sheet rather than taking the active one --
// runs exactly the same code rather than a copy of it.

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
if (libDir == null || !new File(libDir, "NucleusPipeline.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
LIBDIR = libDir.getAbsolutePath()

def NP = new GroovyClassLoader(this.class.classLoader)
             .parseClass(new File(LIBDIR + "/NucleusPipeline.groovy"))

// Keys are the names saveRunConfig() writes, not the `#@` variable names, so a
// config file produced by a run can be fed straight back in later.
def res = NP.load(LIBDIR).run(imp, outdir, [
    script_name            : "Run_NucleusSelector.groovy",
    output_prefix          : outPrefix,
    position_pattern       : positionPattern,
    z_spec                 : zSpec,
    dna_channel            : dnaCh,
    channels_measured      : channelsCsv,
    nucleus_blur_sigma     : nucSigma,
    nucleus_threshold      : nucMethod,
    nucleus_particle_size  : nucSize,
    nucleus_watershed      : nucWatershed,
    nucleoli_enabled       : doNucleoli,
    nucleolus_blur_sigma   : nucleolusSigma,
    nucleolus_threshold    : nucleolusMethod,
    nucleolus_rel_fraction : relFraction,
    nucleolus_erode_px     : erodePx,
    nucleolus_particle_size: nucleolusSize,
    nucleolus_circularity  : nucleolusCirc,
    save_roi_zips          : saveRoiZips,
    save_outlines          : saveOutlines,
    save_measurements      : saveMeasurements,
    save_config            : saveConfig,
    save_overview          : saveOverview,
])

// --- Display, which is this entry point's own business -------------------
// The pipeline produces files; showing them is what makes this the interactive
// runner. The batch runner does neither.
if (addToRoiManager && !java.awt.GraphicsEnvironment.isHeadless()) {
    def rm = RoiManager.getInstance() ?: new RoiManager()
    rm.reset()
    [[res.nucRois, res.nucNames], [res.nuclRois, res.nuclNames]].each { pair ->
        pair[0].eachWithIndex { r, i -> r.setName(pair[1][i]); rm.addRoi(r) }
    }
}

imp.show()
