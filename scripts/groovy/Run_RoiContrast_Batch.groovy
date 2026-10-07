#@ File    (persist=false, label="Sample sheet (samples.tsv)", style="file", required=false) sheetFile
#@ File    (persist=false, label="Segmentation folder (the batch's outlines + ROI zips)", style="directory", required=false) segDir
#@ File    (persist=false, label="Output directory", style="directory", required=false) outdir
#@ String  (persist=false, label="Image root (blank = paths as given)", value="") imageRoot
#@ String  (persist=false, label="Feature", value="nucleus") feature
#@ String  (persist=false, label="Channels to measure (comma separated)", value="1,2") channels
#@ Double  (persist=false, label="Ring starts (um outside the edge)", value=3.0) innerUm
#@ Double  (persist=false, label="Ring ends (um outside the edge)", value=12.0) outerUm
#@ String  (persist=false, label="Image opening method", value="auto", choices={"auto","importer","reader"}) openMode

// Run_RoiContrast_Batch.groovy
//
// For every included row of a sample sheet: open the series, read the ROIs
// the nucleus batch saved for it (<prefix>_<feature>_outline_ROIs.zip in
// segDir), and measure each ROI and a ring around it on its own slice
// (RoiContrast.groovy). Writes <prefix>_<feature>_contrast.txt per series into
// outdir, plus batch_summary.tsv and contrast_params.txt.
//
// A post-annotation step for the oocyte count (analysis-oo_count-physical_blur,
// not merged): feature_contrast_cli.r then turns these per-ROI numbers into a
// keep/drop per feature. Same sheet, same image root and same opening as the
// nucleus batch, through the same loop (BatchRunner.runEach), so it runs on the
// Mac and the HPC alike:
//
//   ImageJ-macosx --headless --console --mem=10000m \
//     --run scripts/groovy/Run_RoiContrast_Batch.groovy \
//     "sheetFile='/p/samples.tsv',segDir='/p/seg',outdir='/p/contrast',imageRoot='/p/raw',openMode='reader'"
//
// A series with no ROI zip is not a failure when its _config.txt says it found
// no ROIs -- the nucleus batch writes no zip then -- and gets a header-only
// table. Without that config line it IS a failure: a wrong segDir would
// otherwise look like a run of empty sections.

import ij.IJ

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
if (libDir == null || !new File(libDir, "RoiContrast.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
def LIBDIR = libDir.getAbsolutePath()

// Every parameter has a default or is optional, so an omitted one cannot hang a
// headless run; the three that have no sensible default are checked here.
[sheetFile: sheetFile, segDir: segDir, outdir: outdir].each { k, v ->
    if (v == null) throw new IllegalArgumentException(k + " is required")
}
if (!sheetFile.isFile()) throw new IllegalArgumentException("no such sample sheet: " + sheetFile)
if (!segDir.isDirectory()) throw new IllegalArgumentException("no such segmentation folder: " + segDir)
def chList = channels.split(",").collect { it.trim() }.findAll { it }.collect { it as Integer }
if (!chList) throw new IllegalArgumentException("no channels given")
if (!(feature ==~ /[A-Za-z][A-Za-z0-9]*/)) throw new IllegalArgumentException("feature must be a plain name; got >>>" + feature + "<<<")

def gcl = new GroovyClassLoader(this.class.classLoader)
def BR  = gcl.parseClass(new File(LIBDIR + "/BatchRunner.groovy"))
def TSV = gcl.parseClass(new File(LIBDIR + "/Tsv.groovy"))
def RX  = gcl.parseClass(new File(LIBDIR + "/RoiExport.groovy"))
def RCT = gcl.parseClass(new File(LIBDIR + "/RoiContrast.groovy"))

outdir.mkdirs()
new File(outdir, "contrast_params.txt").setText(
    "parameter\tvalue\n" +
    "script\tRun_RoiContrast_Batch.groovy " + RX.repoVersion(LIBDIR) + "\n" +
    "imagej_version\t" + IJ.getFullVersion() + "\n" +
    "sheet\t" + sheetFile.getAbsolutePath() + "\n" +
    "seg_dir\t" + segDir.getAbsolutePath() + "\n" +
    "feature\t" + feature + "\n" +
    "channels\t" + chList.join(",") + "\n" +
    "ring_inner_um\t" + innerUm + "\n" +
    "ring_outer_um\t" + outerUm + "\n", "UTF-8")
IJ.log("contrast: " + feature + " ROIs from " + segDir + ", channels " + chList +
       ", ring " + innerUm + "-" + outerUm + " um")

def rows = TSV.read(sheetFile)
def root = (imageRoot?.trim()) ? new File(imageRoot.trim()) : null
def params = [open_mode: openMode,
              pixel_size_note: "The ring is set in um and the ROIs come from each series' own zip, " +
                               "so nothing here depends on pixel size."]
def res = BR.load(LIBDIR).runEach(rows, root, params, outdir, ["n_roi"], { IJ.log(it) }) {
    imp, prefix, si, method, row ->
    def zip = new File(segDir, prefix + "_" + feature + "_outline_ROIs.zip")
    def dest = new File(outdir, prefix + "_" + feature + "_contrast.txt")
    if (!zip.isFile()) {
        def cfg = new File(segDir, prefix + "_config.txt")
        def n = cfg.isFile() ? cfg.readLines().find { it.startsWith(feature + "_count\t") }?.split("\t", -1)?.getAt(1) : null
        if (n == "0") {
            RCT.write([], dest)
            IJ.log("  no " + feature + " ROIs (its _config.txt says " + feature + "_count 0)")
            return [n_roi: 0]
        }
        throw new IllegalStateException("no " + zip.getName() + " in " + segDir +
            (cfg.isFile() ? (" although its _config.txt says " + feature + "_count " + n)
                          : " and no _config.txt either -- is segDir the right folder?"))
    }
    def rois = RX.loadRoiZip(zip.getAbsolutePath())
    def out = RCT.measure(imp, rois, prefix, chList, innerUm as double, outerUm as double)
    RCT.write(out, dest)
    IJ.log("  " + rois.size() + " ROIs measured")
    return [n_roi: rois.size()]
}

IJ.log("Summary: " + new File(outdir, "batch_summary.tsv").getAbsolutePath())
if (res.failed > 0) {
    IJ.log(res.failed + " row(s) FAILED -- the rest completed. In batch_summary.tsv:")
    res.summary.findAll { it.status == "failed" }.take(10).each { IJ.log("  " + it.prefix + "  " + it.message) }
}
