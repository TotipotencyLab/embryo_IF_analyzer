#@ File    (persist=false, label="Series table (series.tsv)", style="file") sheetFile
#@ File    (persist=false, label="Output directory", style="directory") outdir
#@ String  (persist=false, label="Image root (blank = paths as given)", value="") imageRoot
#@ File    (persist=false, label="Run config (blank = defaults)", style="file", required=false) configFile
#@ String  (persist=false, label="Save overview PNGs", value="(from config)", choices={"(from config)","yes","no"}) saveOverview
#@ String  (persist=false, label="Image opening method", value="auto", choices={"auto","importer","reader"}) openMode
#@ File    (persist=false, label="Sources table (sources.tsv; Luxendo only, blank = none)", style="file", required=false) sourcesFile
#@ String  (persist=false, label="Time points (blank = all)", description="Which frames of a multi-frame series to analyse: 1, or 2,11,21, or 1-4 -- counted from 1, and reported as themselves (t = 11 is time point 11). A series of one frame is analysed whatever this says.", value="") frames

// Run_NucleusSelector_Batch.groovy
//
// Run_NucleusSelector.groovy over a whole series table instead of the active
// image. Same name because it is the same analysis -- the difference is only how
// the images arrive, and which of the two you want depends on whether you are
// tuning or running.
//
// The parameters come from a run config, NOT from a dialog: that is the whole
// point. Tune one image interactively, take the _config.txt it wrote, and feed
// it here. Anything the config does not set takes NucleusPipeline.DEFAULTS,
// which Test_RunConfig pins to the dialog's own defaults.
//
// Runs identically in the GUI and headless:
//
//   ImageJ-macosx --headless --console --mem=6000m \
//     --run scripts/groovy/Run_NucleusSelector_Batch.groovy \
//     "sheetFile='/p/series.tsv',outdir='/p/out',imageRoot='/p/raw',configFile='/p/nucleus_config.txt'"
//
// There is no output-prefix parameter. The series table's `series_id` column
// names every series' output, and nothing is put in front of it: the file names
// and the `name` column inside the outline tables are the same string, which
// is how the R side finds a `_config.txt` from a name it read out of a table.

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
def NP  = gcl.parseClass(new File(LIBDIR + "/NucleusPipeline.groovy"))
def RC  = gcl.parseClass(new File(LIBDIR + "/RunConfig.groovy"))

// Parameters: config over defaults, then the two this dialog still offers.
def fromFile = (configFile != null && configFile.isFile())
    ? RC.readParams(configFile, NP.PARAM_TYPES)
    : [:]
def params = NP.fromConfig(fromFile)
params.script_name = "Run_NucleusSelector_Batch.groovy"
// An OVERRIDE, not a setting. save_overview is a real parameter and a config
// now carries it, so a Boolean here would be two sources of truth with the
// dialog always winning and the config's value unreachable -- which is what a
// Boolean forces, since it has no third state for "leave it alone".
//
// Same convention the output prefix used to use: a neutral value means "do not
// override". Useful on a long batch, where a quick no-PNG pass should not mean
// editing the config and then editing it back.
// BatchRunner owns the vocabulary and REFUSES anything else -- a `#@ String`
// with choices is not validated on the command line, so an old
// saveOverview=true would otherwise silently mean "no".
def overviewOverride = BR.overviewOverride(saveOverview)
if (overviewOverride != null) params.save_overview = overviewOverride
// A request, not a decision: BatchRunner turns "auto" into importer or reader
// per file and records which one ran. See BatchRunner's opening section for why
// the two exist.
params.open_mode = openMode

if (configFile != null && configFile.isFile()) {
    IJ.log("config: " + configFile.getName() + " set " + fromFile.size() + " parameter(s)")
    RC.retiredIn(RC.read(configFile)).each { k ->
        IJ.log("config: " + k + " skipped, retired in " + RC.RETIRED_KEYS[k])
    }
} else {
    IJ.log("config: none given, using defaults")
}

def rows = TSV.read(sheetFile)
def root = (imageRoot?.trim()) ? new File(imageRoot.trim()) : null

// LUXENDO. A series whose series_id has rows in the sources table is read from
// its .lux.h5 files, one frame at a time -- a position does not fit in memory,
// and nothing is converted first. Every other row is opened from its file.
if (sourcesFile != null && sourcesFile.isFile()) {
    params.sources = TSV.read(sourcesFile)
    IJ.log("sources: " + sourcesFile.getName() + ", " + params.sources.size() + " file(s)")
}
// Not a run parameter: which frames to look at is a choice about this batch,
// like include, not about how an image is analysed. _config.txt records the
// frames each series' results hold (frames_analysed).
params.frames = frames

def res = BR.load(LIBDIR).run(rows, root, params, outdir) { IJ.log(it) }

// The summary is the deliverable when a batch is large: it says which rows to
// look at, and it exists whether or not any of them failed.
IJ.log("Summary: " + new File(outdir, "batch_summary.tsv").getAbsolutePath())
if (res.failed > 0 || res.frames_failed > 0) {
    IJ.log("")
    IJ.log(res.failed + " row(s) and " + res.frames_failed + " frame(s) FAILED -- the rest completed. " +
           "In batch_summary.tsv:")
    res.summary.findAll { it.status == "failed" }.take(10).each {
        IJ.log("  " + it.series_id + (it.t != "" ? (" t" + it.t) : "") + "  " + it.message)
    }
}
