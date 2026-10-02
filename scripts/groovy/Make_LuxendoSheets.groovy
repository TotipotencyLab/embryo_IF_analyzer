#@ String  (visibility=MESSAGE, value="Scan a Luxendo acquisition into the two tables the pipeline runs on", required=false) help_title
#@ File    (persist=true,  label="Luxendo acquisition directory", style="directory") luxDir
#@ File    (persist=true,  label="Output directory", style="directory") outdir
#@ String  (persist=false, label="Alias (blank = the acquisition folder name)", description="A short handle for this acquisition. It becomes the first part of every series id, so two acquisitions of the same positions do not collide: without it, both produce s0000_L26A_pos1_t0000. Blank uses the folder name, exactly as files.tsv's alias defaults to the basename.", value="") alias
#@ String  (visibility=MESSAGE, value=" ", required=false) help_sep0
#@ String  (visibility=MESSAGE, value="Behavior control:", required=false) help_msg1
#@ Boolean (persist=false, label="Gather time frames", description="One series per imaging position holding every time point -- the default, and what the time-axis work builds on: a Luxendo stack is one series, as Bio-Formats itself presents it. Off gives one series per time point, the layout v0.6.0 made by default.", value=true) gatherFrames
#@ Boolean (persist=false, label="Quick scan", description="Read one .json sidecar per directory instead of one per file. The dimensions are constant within a channel directory, and every file's size is checked against its siblings so a timepoint of a different depth is still read in full. On a network mount this is the difference between about a minute and about an hour.", value=true) quickScan
#@ String  (persist=false, label="File list from", description="auto: the bdv.h5 + bdv.xml index Luxendo writes at the end of an acquisition when both are present, else a walk of the directory tree. index: require the index. walk: always walk -- slower over a network mount, but the only way to see a file the index does not list, and with an index present it reports any difference.", value="auto", choices={"auto","index","walk"}) listing

// Make_LuxendoSheets.groovy
//
// A Luxendo acquisition directory -> the TWO tables the rest of the pipeline
// runs on, and nothing else. No pixels are read and no images are written.
//
//   series.tsv    one row per series. `samples`-shaped, so the batch runner and
//                 every R CLI read it with no changes. THIS is where `include`
//                 lives and where you add your own metadata columns.
//   sources.tsv   one row per .lux.h5, keyed to the series it feeds.
//
// WHY TWO
//   Every format this repo read before Luxendo put one or more series INSIDE
//   one file. Luxendo puts one series ACROSS several files -- one per channel,
//   one per time point -- so a series row cannot hold a single `path`. The file
//   facts go in the second table. It is the same pair as files.tsv ->
//   samples.tsv, with the cardinality reversed.
//
// WHAT TO DO WITH THEM
//   Open series.tsv. Set include=false on what you do not want, add your own
//   columns (condition, genotype, ...), then hand BOTH tables to
//   Make_LuxendoTiff (to convert a few for tuning) or to the batch runner.
//
// THE ALIAS IS YOURS, and the folder name is only its default -- the same
// contract as `files.tsv`'s alias. It becomes the first part of every series id,
// which is what makes the id unique ACROSS acquisitions and not merely within
// one: `s<NNNN>_<stack_description>` repeats between runs (measured: two real
// acquisitions shared all 14 stack identities), and two runs landing in one
// output directory would overwrite each other's results without it.
//
// THE FILE LIST comes from the bdv.h5 index Luxendo writes beside raw/ when it
// is there, rather than from walking raw/ -- about a minute instead of three or
// four over samba on a 4032-file acquisition. `listing=walk` still walks, and
// is how to find a file the index does not list. LuxendoIndex says why both.
//
// IDENTITY COMES FROM EACH FILE'S OWN SIDECAR, never from its path. See
// LuxendoScan for why, LuxendoSidecar for what it costs, and
// note/luxendo_file_format.md for the format.
//
// The columns are declared in schema/sheet_columns.tsv under sheets `samples`
// and `manifest`, which both languages read at run time -- so this script does
// not name them.
//
// Headless:
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run scripts/groovy/Make_LuxendoSheets.groovy \
//     "luxDir='/path/to/2026-09-10_184731',outdir='/path/out',alias='fucci_rep2',quickScan=true,listing='auto'"

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
if (libDir == null || !new File(libDir, "LuxendoScan.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
def LIBDIR = libDir.getAbsolutePath()

def gcl    = new GroovyClassLoader()
def LS     = gcl.parseClass(new File(LIBDIR, "LuxendoScan.groovy"))
def TSV    = gcl.parseClass(new File(LIBDIR, "Tsv.groovy"))
def SCHEMA = gcl.parseClass(new File(LIBDIR, "SheetSchema.groovy")).loadFromLibDir(LIBDIR)

IJ.log("=== Luxendo -> series + sources ===")
IJ.log("  source : " + luxDir.getAbsolutePath())
IJ.log("  output : " + outdir.getAbsolutePath())
IJ.log("  alias  : " + ((alias?.trim()) ?: ("(from the folder name) " + luxDir.getName())))
IJ.log("  series : " + (gatherFrames ? "one per POSITION, all time points inside"
                                     : "one per (position, time point)"))

long t0 = System.currentTimeMillis()
def res = LS.load(LIBDIR).scan(luxDir,
             [alias: alias, gatherFrames: gatherFrames, quickScan: quickScan,
              listing: LS.checkListing(listing)]) { IJ.log(it) }
def series  = res.series
def sources = res.sources

// Column order from the schema, so the files and the declaration cannot drift,
// and a scanner that forgot a declared column is a bug rather than a bad input.
outdir.mkdirs()
[[name: "series.tsv",  sheet: "samples",  rows: series],
 [name: "sources.tsv", sheet: "manifest", rows: sources]].each { spec ->
    def cols = SCHEMA.columns(spec.sheet)
    def missing = cols - spec.rows[0].keySet().toList()
    if (missing) {
        throw new IllegalStateException(
            "Scanner did not produce declared " + spec.sheet + " column(s): " + missing.join(", "))
    }
    // Any column the scan produced that the schema does not declare would be
    // dropped silently by Tsv.write, which is how a fact goes missing.
    def undeclared = spec.rows[0].keySet().toList() - cols
    if (undeclared) {
        throw new IllegalStateException(
            "Scanner produced undeclared " + spec.sheet + " column(s): " + undeclared.join(", ") +
            "\n  Either declare them in schema/sheet_columns.tsv or stop writing them.")
    }
    TSV.write(spec.rows, new File(outdir, spec.name), cols)
    IJ.log("  wrote " + new File(outdir, spec.name).getAbsolutePath() +
           "  (" + spec.rows.size() + " row(s), " + cols.size() + " column(s))")
}

IJ.log("")
IJ.log("  " + sources.size() + " source file(s) -> " + series.size() + " series")
IJ.log("  file list  : " + (res.listing == "index" ? "from the bdv.h5 index" : "from a directory walk"))
IJ.log("  time points: " + sources.collect { it.t }.unique().sort())
IJ.log("  channels   : " + sources.collect { it.channel }.unique().sort())
IJ.log("  took " + String.format("%.1f", (System.currentTimeMillis() - t0) / 1000.0d) + " s")
if (res.warnings) {
    IJ.log("")
    IJ.log("  " + res.warnings.size() + " warning(s) above -- read them before converting anything.")
}
IJ.log("")
IJ.log("  Next: edit include= in series.tsv, then run Make_LuxendoTiff or the batch runner")
IJ.log("  with BOTH tables.")
IJ.log("Done: Luxendo sheets")
