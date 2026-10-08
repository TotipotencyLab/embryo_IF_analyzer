#@ File    (persist=false, label="Series table (series.tsv)", style="file") sheetFile
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
#@ File    (persist=false, label="Sources table (sources.tsv; Luxendo only, blank = none)", style="file", required=false) sourcesFile
#@ String  (persist=false, label="Time points (blank = all)", description="Which frames of a multi-frame series to draw: 1, or 2,11,21, or 1-4 -- counted from 1. A series of one frame is drawn whatever this says.", value="") frames
#@ String  (persist=false, label="Run tag (blank = none)", description="Names this run's batch_summary_<tag>.tsv (and batch_params_<tag>.txt), so several runs can share one output directory -- the tasks of a SLURM array, say. Letters, digits, _ and - only.", value="") runTag

// Run_Overview_Batch.groovy
//
// Overview PNGs for every included row of a series table, and NOTHING else --
// no threshold, no particles, no measurement, no ROI files. A series of several
// frames gets one TIFF per channel instead, a page per frame at one display
// range per channel (Overview.writeSeries), streamed a frame at a time -- so a
// Luxendo position, read through sourcesFile, can be looked at too.
//
// WHY IT EXISTS
//   Looking at a dataset should not cost a segmentation run. Detection on a
//   tile-merged slide is minutes per series and needs a tuned config you cannot
//   write until you have seen the images; this is the step before that. It is
//   also what makes a group montage possible on data nobody has segmented yet.
//
// OUTPUT NAMES ARE THE SAME AS THE NUCLEUS PATH'S, on purpose:
//
//   <series_id>_overview_ch<N>.png      one frame
//   <series_id>_overview_ch<N>.tif      several frames
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
//     "sheetFile='/p/series.tsv',outdir='/p/overviews',imageRoot='/p/raw',outWidth=1000"
//
// Like the nucleus batch: the sheet's `series_id` names the output and `include`
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
// run_tag names batch_summary.tsv apart when several runs share outdir.
def params = [open_mode: openMode, run_tag: runTag]
// LUXENDO, as in the nucleus batch: a series whose series_id has rows in the
// sources table is read from its .lux.h5 files, one frame at a time.
if (sourcesFile != null && sourcesFile.isFile()) {
    params.sources = TSV.read(sourcesFile)
    IJ.log("sources: " + sourcesFile.getName() + ", " + params.sources.size() + " file(s)")
}
def wantedFrames = runner.TA.parseFrames(frames)
// Settings only a multi-frame series needs, checked before any image opens --
// but only refused if one turns up: a sheet of single frames may use any method.
def seriesMethodProblem = null
try { OV.validateSeriesSettings(method) } catch (IllegalArgumentException e) { seriesMethodProblem = e }
def ovOpts = [width: outWidth, height: outHeight, contrast: contrast, saturated: saturated]

// display_range is here because "auto" contrast stretches whatever it is given:
// a channel holding only noise has that noise stretched to full range and saves
// a convincing picture of nothing. A narrow range beside a wide one on another
// channel is the tell -- but only if it is written down, and opening every PNG
// to find the handful that went wrong is exactly what this table exists to
// avoid.
def cols = ["channels", "png_size", "display_range"]

// A series of several frames: each frame projected and staged as it is read,
// then one TIFF per channel at one display range across them all. The unit of
// work is the frame, as in the nucleus batch -- a frame that cannot be read is
// a row of its own in batch_summary.tsv, and the rest are still drawn.
def seriesOverview = { src, String seriesId ->
    if (seriesMethodProblem != null) throw seriesMethodProblem
    def pick = src.choose(wantedFrames)
    if (pick.absent) {
        IJ.log("  WARNING: no frame " + runner.SS.describeFrames(pick.absent) + " in this series (it has " +
               runner.SS.describeFrames(src.frameList) + ")")
    }
    if (!pick.frames) {
        throw new IllegalArgumentException("none of the frames asked for is in this series (it has " +
                                           runner.SS.describeFrames(src.frameList) + ")")
    }
    def slices = RD.parseSlices(zSpec ?: "", src.nSlices)
    def stage = new File(new File(outdir, runner.NP.STAGING_DIR), seriesId)
    stage.deleteDir()
    stage.mkdirs()
    try {
        def rowsOut = [], okTs = []
        List<Integer> chans = null
        pick.frames.each { int t ->
            long t0 = System.currentTimeMillis()
            def frame = null
            try {
                frame = src.frame(t)
                def done = runner.NP.stagedFrame(stage, t)
                def part = new File(stage, done.getName() + ".part")
                part.deleteDir()
                chans = OV.stageFrame(frame, slices, method, wanted, ovOpts, part)
                if (!part.renameTo(done)) throw new IOException("could not finish staging " + done)
                okTs << t
                rowsOut << [t: t, status: "ok", message: "", seconds: BR.fmtSeconds(System.currentTimeMillis() - t0)]
            } catch (Throwable e) {
                def msg = e.getClass().getSimpleName() + ": " + (e.getMessage() ?: "(no message)")
                IJ.log("  t" + t + " FAILED: " + msg)
                rowsOut << [t: t, status: "failed", message: msg, seconds: BR.fmtSeconds(System.currentTimeMillis() - t0)]
            } finally {
                if (frame != null) src.release(frame)
            }
        }
        if (!okTs) throw new IllegalStateException("every frame failed; first: t" + rowsOut[0].t + " " + rowsOut[0].message)
        def ov = OV.writeSeries(okTs.collect { runner.NP.stagedFrame(stage, it) }, okTs, outdir.getPath(), seriesId,
                                chans, ovOpts, src.width as int, src.height as int, null, src.frameInterval as Double)
        def ranges = ov.collect { c, r -> "ch" + c + ":" + IJ.d2s(r.lo as double, 1) + "-" + IJ.d2s(r.hi as double, 1) }
        ov.each { c, r -> IJ.log("  overview ch" + c + " over " + okTs.size() + " frame(s) -> " + r.files[0].getName()) }
        def size = ov.values().first().size
        // The series' facts on each of its frames' rows: the range is one for
        // the whole series, and a frame row is read on its own.
        rowsOut.findAll { it.status == "ok" }.each {
            it.channels = chans.join(","); it.png_size = size; it.display_range = ranges.join(" ")
        }
        return [frames: rowsOut]
    } finally {
        stage.deleteDir()
        def stageParent = stage.getParentFile()
        if (stageParent.isDirectory() && !stageParent.list()) stageParent.delete()
    }
}

// Both closures passed INSIDE the parentheses on purpose. runEach's log
// parameter has a default, so two trailing closure blocks would leave which
// one is which to arity resolution -- and getting that wrong silently swaps
// the progress log for the work.
def perRow = { src, seriesId, si, openMethod, row ->
    if (src.nFrames > 1) return seriesOverview(src, seriesId)
    // One frame: the PNGs, exactly as before.
    def imp = (src.whole != null) ? src.whole : src.frame(1)
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
            def f = OV.savePng(view, OV.overviewPath(outdir.getPath(), seriesId, c))
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
        src.release(imp)
    }
}

def res = runner.runEach(rows, root, params, outdir, cols,
                         { IJ.log(it) }, perRow)

IJ.log("Summary: " + res.summary_file.getAbsolutePath())
if (res.failed > 0) {
    IJ.log("")
    IJ.log(res.failed + " row(s) FAILED -- the rest completed. In batch_summary.tsv:")
    res.summary.findAll { it.status == "failed" }.take(10).each {
        IJ.log("  " + it.series_id + "  " + it.message)
    }
}
IJ.log("Done: overview batch")
