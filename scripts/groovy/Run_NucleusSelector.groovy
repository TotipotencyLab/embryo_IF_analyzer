#@ ImagePlus imp
#@ String  (visibility=MESSAGE, value="Output specification", required=false) msg0
#@ File    (label="Output directory", style="directory") outdir
#@ String  (label="Output prefix", value="") outPrefix
#@ String  (visibility=MESSAGE, value="Image information", required=false) msg1
#@ String  (label="Position token in slice label (blank = use title)", value="Position") positionPattern
#@ String  (label="Z-slices to analyse (blank = all; e.g. 1-20,35-40)", value="") zSpec
#@ Integer (label="DNA/DAPI channel", value=1) dnaCh
#@ String  (label="Channels to measure (comma separated)", value="1,2,3") channelsCsv
#@ String  (visibility=MESSAGE, value="Nucleus detection", required=false) msg2
#@ Double  (label="Blur sigma", value=8.0) nucSigma
#@ String  (persist=false, label="Threshold method", value="Huang2", choices={"Huang2","Huang","Default","Otsu","Triangle","IsoData","Li","Yen","Mean","Moments","Percentile","MaxEntropy","RenyiEntropy","Shanbhag","Intermodes","Minimum","IJ_IsoData","MinError(I)","Manual"}) nucMethod
#@ String  (persist=false, label="  ...if Manual: threshold range lo-hi", value="") nucRange
#@ Boolean (persist=false, label="One threshold from the whole stack (off = per slice)", value=true) nucStackHist
#@ String  (label="Particle size (calibrated units^2)", value="80-Infinity") nucSize
#@ String  (label="Circularity (0.00-1.00 = no filter)", value="0.00-1.00") nucCircularity
#@ Boolean (label="Split touching nuclei (watershed)", value=false) nucWatershed
// todo: manual threshold
#@ String  (visibility=MESSAGE, value="Nucleolus detection", required=false) msg3
#@ Boolean (label="Detect nucleoli", value=true) doNucleoli
#@ Double  (label="Blur sigma", value=3.0) nucleolusSigma
#@ String  (label="Threshold method", value="Relative", choices={"Relative","Default","Otsu","Triangle","Huang","Huang2","IsoData","Li","Yen","Mean","Moments","Percentile","MaxEntropy","RenyiEntropy","Shanbhag","Intermodes","Minimum","IJ_IsoData","MinError(I)"}) nucleolusMethod
#@ Double  (label="Relative fraction (if Relative)", value=0.6) relFraction
#@ Integer (label="Shrink nucleus ROI (px)", value=0) erodePx
#@ String  (label="Particle size (calibrated units^2)", value="3-150") nucleolusSize
#@ String  (label="Circularity", value="0.50-1.00") nucleolusCirc
#@ String  (visibility=MESSAGE, value="Behavior control", required=false) msg4
#@ Boolean (label="Add ROIs to ROI Manager (needs GUI)", value=true) addToRoiManager
#@ Boolean (label="Save ROI zips", value=true) saveRoiZips
#@ Boolean (label="Save outline coordinates", value=true) saveOutlines
#@ Boolean (label="Save measurements", value=true) saveMeasurements
#@ Boolean (label="Save run configuration", value=true) saveConfig
#@ String  (visibility=MESSAGE, value="Image overview", required=false) msg5
#@ Boolean (label="Save overview PNG (quick visual check)", value=false) saveOverview
#@ String  (label="Projection", value="max", choices={"max","mean","median","sum","sd","min"}) ovMethod
#@ Integer (label="Width in px (0 = original)", value=500) ovWidth
#@ Integer (label="Height in px (0 = follow width)", value=0) ovHeight
#@ String  (label="Contrast", value="auto", choices={"auto","none"}) ovContrast
#@ Double  (label="Contrast: percent saturated (if auto)", value=0.35) ovSaturated

// Run_NucleusSelector.groovy
//
// Nucleus + nucleolus detection, export and measurement, for the ACTIVE image.
//
// This dialog REMEMBERS what you last set, deliberately -- it is the tuning
// entry point and a human is looking at it. The three threshold fields are the
// exception and reset every run. A manual threshold is a raw pixel value, which
// is meaningless on a different bit depth or exposure, and a per-slice
// histogram is the risky setting of the two; neither should be inherited by the
// next image because it was tried once on this one. They are still written to
// _config.txt, so tuning here and feeding that config to the batch is
// unaffected -- persist=false means "do not remember into the next DIALOG", not
// "do not record".
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
    nucleus_threshold_range: nucRange,
    nucleus_stack_histogram: nucStackHist,
    nucleus_particle_size  : nucSize,
    nucleus_circularity    : nucCircularity,
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
    overview_method        : ovMethod,
    overview_width         : ovWidth,
    overview_height        : ovHeight,
    overview_contrast      : ovContrast,
    overview_saturated     : ovSaturated,
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
