#@ ImagePlus imp
#@ File    (label="Output directory", style="directory") outdir
#@ String  (label="Output prefix", value="") outPrefix
#@ String  (label="Position token in slice label (blank = use title)", value="Position") positionPattern
#@ String  (label="Z-slices to analyse (blank = all; e.g. 1-20,35-40)", value="") zSpec
#@ Integer (label="DNA/DAPI channel", value=1) dnaCh
#@ String  (label="Channels to measure (comma separated)", value="1,2,3") channelsCsv
#@ Double  (label="Nucleus: blur sigma", value=8.0) nucSigma
#@ String  (label="Nucleus: threshold method", choices={"Huang2","Huang","Default","Otsu","Triangle","IsoData"}) nucMethod
#@ String  (label="Nucleus: particle size (calibrated units^2)", value="80-Infinity") nucSize
#@ Boolean (label="Nucleus: split touching nuclei (watershed)", value=false) nucWatershed
#@ Boolean (label="Detect nucleoli", value=true) doNucleoli
#@ Double  (label="Nucleolus: blur sigma", value=3.0) nucleolusSigma
#@ String  (label="Nucleolus: threshold method", choices={"Relative","Default","Otsu","Triangle","Huang","IsoData"}) nucleolusMethod
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
// Nucleus + nucleolus detection, export and measurement.
//
// 0.2.0: particle extraction moved off the ROI Manager (RoiDetect), so the whole
//        pipeline runs headless; z-slice range selection; run configuration
//        written alongside the results.

import ij.*
import ij.gui.*
import ij.plugin.Duplicator
import ij.plugin.RoiEnlarger
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
if (libDir == null || !new File(libDir, "NucleolusDetect.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
LIBDIR = libDir.getAbsolutePath()

def gcl = new GroovyClassLoader(this.class.classLoader)
def ND = gcl.parseClass(new File(LIBDIR + "/NucleolusDetect.groovy"))
def RX = gcl.parseClass(new File(LIBDIR + "/RoiExport.groovy"))
def RD = gcl.parseClass(new File(LIBDIR + "/RoiDetect.groovy"))
def OV = gcl.parseClass(new File(LIBDIR + "/Overview.groovy"))

// Forced, so output columns do not depend on the operator's Fiji preferences.
MEASUREMENTS = "area mean standard min centroid shape integrated median stack display"
// Overview settings, fixed for the pipeline; use Run_Overview.groovy to vary them.
OVERVIEW_WIDTH = 500
OVERVIEW_METHOD = "max"
IJ.run("Set Measurements...", MEASUREMENTS + " redirect=None decimal=3")

def channels   = channelsCsv.split(",").collect { it.trim() as Integer }
// Overviews cover the DNA channel detection used, plus every channel being
// measured. The outlines were found on DNA, so drawing them over the other
// channels is exactly how you check a signal against the compartment it is
// supposed to be in.
def ovChannels = ([dnaCh] + channels).unique().sort()
def slices     = RD.parseSlices(zSpec, imp.getNSlices())
def outDirPath = outdir.getAbsolutePath() + File.separator
def basename   = outPrefix + RX.resolveImageId(imp, positionPattern)
IJ.log("=== " + basename + " ===")
IJ.log("  analysing " + slices.size() + " of " + imp.getNSlices() + " slices")

def writeFeature = { String feature, List rois, List names, List sls ->
    IJ.log("  " + feature + ": " + rois.size() + " ROIs")
    if (rois.isEmpty()) return
    def stem = outDirPath + basename + "_" + feature
    if (saveOutlines)     RX.saveOutlineCoords(imp, rois, names, sls, basename, stem + "_outline.txt")
    if (saveRoiZips)      RX.saveRoiZip(rois, names, stem + "_outline_ROIs.zip")
    if (saveMeasurements) RX.measureRois(imp, rois, sls, channels, stem + "_res.txt", true)
}

// --- Nucleus -------------------------------------------------------------
def dna = RD.buildMask(imp, dnaCh, nucSigma, nucMethod, true, nucWatershed)
def nucRois   = RD.detect(dna, nucSize, "", slices, true, true)
def nucNames  = RD.autoLabels(nucRois).collect { "nucleus_" + it }
def nucSlices = nucRois.collect { it.getPosition() }
// NB: set the name ON the Roi, not just in the parallel names list. The name is
//     encoded into the .roi file and picked up by Analyzer into the Label
//     column, which is how read_fiji_result.r joins measurements to outlines.
nucRois.eachWithIndex { r, i -> r.setName(nucNames[i]) }
dna.close()
writeFeature("nucleus", nucRois, nucNames, nucSlices)

// --- Nucleolus -----------------------------------------------------------
def nuclRois = [], nuclNames = [], nuclSlices = []
if (doNucleoli && !nucRois.isEmpty()) {
    def useRois = (erodePx > 0) ? nucRois.collect { RoiEnlarger.enlarge(it, -erodePx) } : nucRois
    def dna2 = new Duplicator().run(imp, dnaCh, dnaCh, 1, imp.getNSlices(), 1, 1)
    def mask = ND.buildNucleolusMask(dna2, useRois, nucSlices, nucleolusSigma, nucleolusMethod, relFraction)
    dna2.close()
    nuclRois   = RD.detect(mask, nucleolusSize, nucleolusCirc, slices, true, false)
    nuclNames  = RD.autoLabels(nuclRois).collect { "nucleolus_" + it }
    nuclSlices = nuclRois.collect { it.getPosition() }
    nuclRois.eachWithIndex { r, i -> r.setName(nuclNames[i]) }
    mask.close()
    writeFeature("nucleolus", nuclRois, nuclNames, nuclSlices)
}

// --- Overview PNGs -------------------------------------------------------
// A quick visual check, not an input to anything: the chosen channels projected
// over the same slices detection used. Fixed settings here on purpose --
// Run_Overview.groovy is the script for choosing them.
//
// TWO files per channel, raw and outlined. They used to share one name, which
// made them mutually exclusive: writing either destroyed the other, and
// montage_qc_cli.r wants both side by side. One prepare() serves both saves --
// savePng() flattens into a NEW image and leaves the view untouched, so the raw
// copy can go out before the outlines are added.
if (saveOverview) {
    def proj = OV.project(imp, slices, OVERVIEW_METHOD, ovChannels)
    ovChannels.each { int c ->
        def view = OV.prepare(proj, c, [width: OVERVIEW_WIDTH, contrast: "auto"])
        def raw  = OV.savePng(view, OV.overviewPath(outDirPath, basename, c, ""))
        // NB: "merged" unions the outlines in the PROJECTION, so touching or
        //     z-overlapping objects share one outline. It is a picture, not a count.
        OV.addOutlines(view, nucRois,  [mode: "merged", color: "yellow",  lineWidth: 1])
        OV.addOutlines(view, nuclRois, [mode: "merged", color: "magenta", lineWidth: 1])
        def ovl  = OV.savePng(view, OV.overviewPath(outDirPath, basename, c, OV.OVERLAY_SUFFIX))
        // The display range, because "auto" contrast stretches whatever is there:
        // a channel carrying only noise has that noise stretched to full range
        // and saves a convincing picture of nothing. A narrow range beside a wide
        // one on another channel is the tell -- but only if it is written down.
        IJ.log("  overview ch" + c + ": display " + IJ.d2s(view.lo, 1) + "-" + IJ.d2s(view.hi, 1) +
               " -> " + raw.getName() + ", " + ovl.getName())
    }
    proj.close()
}

// --- ROI Manager, for visual inspection only -----------------------------
if (addToRoiManager && !java.awt.GraphicsEnvironment.isHeadless()) {
    def rm = RoiManager.getInstance() ?: new RoiManager()
    rm.reset()
    [[nucRois, nucNames], [nuclRois, nuclNames]].each { pair ->
        pair[0].eachWithIndex { r, i -> r.setName(pair[1][i]); rm.addRoi(r) }
    }
}

// --- Run configuration ---------------------------------------------------
if (saveConfig) {
    RX.saveRunConfig([
        timestamp              : new Date().format("yyyy-MM-dd HH:mm:ss"),
        script                 : "Run_NucleusSelector.groovy " + RX.repoVersion(LIBDIR),
        imagej_version         : IJ.getVersion(),
        image_title            : imp.getTitle(),
        // NB: width/height in PIXELS. The outline tables are written in calibrated
        //     units, so without these the R side cannot reconstruct the image
        //     extent -- the bounding box of the detected objects is not the frame.
        //     A QC panel drawn from R would then be cropped differently from the
        //     Fiji overview PNG it is meant to sit beside.
        image_width            : imp.getWidth(),
        image_height           : imp.getHeight(),
        image_slices           : imp.getNSlices(),
        image_channels         : imp.getNChannels(),
        pixel_width            : imp.getCalibration().pixelWidth,
        pixel_height           : imp.getCalibration().pixelHeight,
        pixel_unit             : imp.getCalibration().getUnit(),
        output_basename        : basename,
        position_pattern       : positionPattern,
        z_spec                 : (zSpec ?: "(all)"),
        z_slices_analysed      : slices.size(),
        dna_channel            : dnaCh,
        channels_measured      : channelsCsv,
        measurements           : MEASUREMENTS,
        nucleus_blur_sigma     : nucSigma,
        nucleus_threshold      : nucMethod,
        nucleus_particle_size  : nucSize,
        nucleus_watershed      : nucWatershed,
        overview_saved         : saveOverview,
        // Which overview files exist, so a results folder can be read later
        // without guessing. Blank when none were written.
        overview_channels      : (saveOverview ? ovChannels.join(",") : ""),
        overview_overlay_suffix: (saveOverview ? OV.OVERLAY_SUFFIX : ""),
        nucleus_count          : nucRois.size(),
        nucleoli_enabled       : doNucleoli,
        nucleolus_blur_sigma   : nucleolusSigma,
        nucleolus_threshold    : nucleolusMethod,
        nucleolus_rel_fraction : relFraction,
        nucleolus_erode_px     : erodePx,
        nucleolus_particle_size: nucleolusSize,
        nucleolus_circularity  : nucleolusCirc,
        nucleolus_count        : nuclRois.size()
    ], outDirPath + basename + "_config.txt")
}

imp.show()
IJ.log("Done: " + basename)
