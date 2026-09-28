#@ File    (persist=false, label="Sample sheet (samples.tsv)", style="file") sheetFile
#@ File    (persist=false, label="Output directory", style="directory") outdir
#@ String  (persist=false, label="Image root (blank = paths as given)", value="") imageRoot
#@ File    (persist=false, label="Run config (blank = defaults)", style="file", required=false) configFile
#@ String  (persist=false, label="Save overview PNGs", value="(from config)", choices={"(from config)","yes","no"}) saveOverview
#@ String  (persist=false, label="Image opening method", value="auto", choices={"auto","importer","reader"}) openMode

// Run_NucleusSelector_Batch.groovy
//
// Run_NucleusSelector.groovy over a whole sample sheet instead of the active
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
//     "sheetFile='/p/samples.tsv',outdir='/p/out',imageRoot='/p/raw',configFile='/p/nucleus_config.txt'"
//
// There is no output-prefix parameter. The sample sheet's `prefix` column names
// every sample's output and is authoritative -- BatchRunner passes it as
// `basename`, which wins over `output_prefix` outright. A dialog field for it
// existed and could never take effect; it was removed rather than left to look
// as though it did.

import ij.IJ

// The dialog value that means "whatever the config says".
final String OVERVIEW_FROM_CONFIG = "(from config)"

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
if (saveOverview != OVERVIEW_FROM_CONFIG) {
    params.save_overview = (saveOverview == "yes")
}
// A request, not a decision: BatchRunner turns "auto" into importer or reader
// per file and records which one ran. See BatchRunner's opening section for why
// the two exist.
params.open_mode = openMode

if (configFile != null && configFile.isFile()) {
    IJ.log("config: " + configFile.getName() + " set " + fromFile.size() + " parameter(s)")
} else {
    IJ.log("config: none given, using defaults")
}

def rows = TSV.read(sheetFile)
def root = (imageRoot?.trim()) ? new File(imageRoot.trim()) : null
def res = BR.load(LIBDIR).run(rows, root, params, outdir) { IJ.log(it) }

// The summary is the deliverable when a batch is large: it says which rows to
// look at, and it exists whether or not any of them failed.
IJ.log("Summary: " + new File(outdir, "batch_summary.tsv").getAbsolutePath())
if (res.failed > 0) {
    IJ.log("")
    IJ.log(res.failed + " row(s) FAILED -- the rest completed. In batch_summary.tsv:")
    res.summary.findAll { it.status == "failed" }.take(10).each {
        IJ.log("  " + it.prefix + "  " + it.message)
    }
}
