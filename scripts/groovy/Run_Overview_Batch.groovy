#@ File    (persist=false, label="Sample sheet (samples.tsv)", style="file") sheetFile
#@ File    (persist=false, label="Output directory", style="directory") outdir
#@ String  (persist=false, label="Image root (blank = paths as given)", value="") imageRoot
#@ String  (persist=false, label="Z-slices to project (blank = all; e.g. 1-20,35-40)", value="") zSpec
#@ String  (persist=false, label="Channels (comma separated; blank = all)", value="") channelsCsv
#@ String  (persist=false, label="Projection", value="max", choices={"max","mean","median","sum","sd","min"}) method
#@ String  (persist=false, label="Contrast", value="auto", choices={"auto","none"}) contrast
#@ Double  (persist=false, label="Contrast: percent saturated (if auto)", value=0.35) saturated
#@ Integer (persist=false, label="Output width in px (0 = original)", value=1000) outWidth
#@ Integer (persist=false, label="Output height in px (0 = follow width)", value=0) outHeight
#@ String  (persist=false, label="Image opening method", value="auto", choices={"auto","importer","reader"}) openMode

// Run_Overview_Batch.groovy
//
// Overview PNGs for every included row of a sample sheet, and NOTHING else --
// no threshold, no particles, no measurement, no ROI files.
//
// WHY IT EXISTS
//   Looking at a dataset should not cost a segmentation run. Detection on a
//   tile-merged slide is minutes per series and needs a tuned config you cannot
//   write until you have seen the images; this is the step before that. It is
//   also what makes a group montage possible on data nobody has segmented yet.
//
// OUTPUT NAMES ARE THE SAME AS THE NUCLEUS PATH'S, on purpose:
//
//   <prefix>_overview_ch<N>.png
//
// The nucleus pipeline writes exactly this for its raw overview (Overview
// .overviewPath with no suffix), so a file from here and a file from there are
// interchangeable to anything downstream. No suffix parameter is offered --
// the suffix exists to mark that outlines were drawn, and nothing here draws
// any. A batch that could quietly name its files something else would make
// "which run produced this PNG?" unanswerable from the name, which is the one
// thing the shared pattern buys.
//
// Runs identically in the GUI and headless:
//
//   ImageJ-macosx --headless --console \
//     --run scripts/groovy/Run_Overview_Batch.groovy \
//     "sheetFile='/p/samples.tsv',outdir='/p/overviews',imageRoot='/p/raw',outWidth=1000"
//
// Like the nucleus batch: the sheet's `prefix` names the output and `include`
// decides what runs, one row's failure does not stop the others, and
// batch_summary.tsv says what happened to every row including the excluded.

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
if (libDir == null || !new File(libDir, "BatchRunner.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
def LIBDIR = libDir.getAbsolutePath()

def gcl = new GroovyClassLoader(this.class.classLoader)
def BR  = gcl.parseClass(new File(LIBDIR + "/BatchRunner.groovy"))
def TSV = gcl.parseClass(new File(LIBDIR + "/Tsv.groovy"))
def OV  = gcl.parseClass(new File(LIBDIR + "/Overview.groovy"))
def RD  = gcl.parseClass(new File(LIBDIR + "/RoiDetect.groovy"))

// Checked ONCE, before a single image opens. Overview.validateSettings throws
// exactly what project() and prepare() throw, from the same code, so passing
// here cannot mean failing there -- and an unknown projection name typed into
// a dialog should cost one error message, not one per row of a thousand-row
// sheet, each after a series has been read off disk.
OV.validateSettings(method, contrast, saturated, outWidth, outHeight)

// Parsed once too, and for the same reason.
def wanted = (channelsCsv?.trim()) ? channelsCsv.trim().split(",").collect {
                 def t = it.trim()
                 if (!t.isInteger()) {
                     throw new IllegalArgumentException(
                         "channel list must be numbers separated by commas, not '" + channelsCsv + "'")
                 }
                 t as Integer
             } : null

def rows = TSV.read(sheetFile)
def root = (imageRoot?.trim()) ? new File(imageRoot.trim()) : null
def runner = BR.load(LIBDIR)

// display_range is here because "auto" contrast stretches whatever it is given:
// a channel holding only noise has that noise stretched to full range and saves
// a convincing picture of nothing. A narrow range beside a wide one on another
// channel is the tell -- but only if it is written down, and opening every PNG
// to find the handful that went wrong is exactly what this table exists to
// avoid.
def cols = ["channels", "png_size", "display_range"]

// Both closures passed INSIDE the parentheses on purpose. runEach's log
// parameter has a default, so two trailing closure blocks would leave which
// one is which to arity resolution -- and getting that wrong silently swaps
// the progress log for the work.
def perRow = { imp, prefix, si, openMethod, row ->
    def slices = RD.parseSlices(zSpec ?: "", imp.getNSlices())
    def proj   = OV.project(imp, slices, method, wanted)
    try {
        def chans  = OV.projectedChannels(proj)
        def ranges = []
        def size   = ""
        chans.each { int c ->
            def view = OV.prepare(proj, c, [width    : outWidth,
                                            height   : outHeight,
                                            contrast : contrast,
                                            saturated: saturated])
            def f = OV.savePng(view, OV.overviewPath(outdir.getPath(), prefix, c))
            ranges << ("ch" + c + ":" + IJ.d2s(view.lo, 1) + "-" + IJ.d2s(view.hi, 1))
            size = view.image.getWidth() + "x" + view.image.getHeight()
            IJ.log("  overview ch" + c + " -> " + f.getName())
        }
        return [channels: chans.join(","), png_size: size,
                display_range: ranges.join(" ")]
    } finally {
        // A projection of a tile merge is hundreds of MB. close() alone frees
        // nothing while `proj` is in scope -- it detaches a window and there is
        // none headless -- so both calls, every time.
        proj.close(); proj.flush()
    }
}

def res = runner.runEach(rows, root, [open_mode: openMode], outdir, cols,
                         { IJ.log(it) }, perRow)

IJ.log("Summary: " + new File(outdir, "batch_summary.tsv").getAbsolutePath())
if (res.failed > 0) {
    IJ.log("")
    IJ.log(res.failed + " row(s) FAILED -- the rest completed. In batch_summary.tsv:")
    res.summary.findAll { it.status == "failed" }.take(10).each {
        IJ.log("  " + it.prefix + "  " + it.message)
    }
}
IJ.log("Done: overview batch")
