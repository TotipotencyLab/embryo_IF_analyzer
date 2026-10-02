#@ String  (visibility=MESSAGE, value="Assemble Luxendo sources into TIFF files, for tuning and for drag-and-drop", required=false) help_title
#@ File    (persist=true,  label="Series table (series.tsv)", style="file") seriesFile
#@ File    (persist=true,  label="Sources table (sources.tsv)", style="file") sourcesFile
#@ File    (persist=true,  label="Output directory", style="directory") outdir
#@ String  (visibility=MESSAGE, value=" ", required=false) help_sep0
#@ String  (visibility=MESSAGE, value="Behavior control:", required=false) help_msg2
#@ String  (persist=false, label="Output format", description="TIFF can be reopened easily in ImageJ, but have size limit of ~4GB. BigTIFF can hold larger file, but may not be compatible with ImageJ", value="tiff", choices={"tiff","bigtiff"}) format
#@ String  (persist=false, label="Time points (blank = all)", description="Which time points to write, each into its own file named <series_id>_t<TTTT>: 0, or 0,47,95, or 0-3. Blank writes every time point of a series into one file, which for a long time course is too big for TIFF or for memory -- for tuning, one time point is what you want.", value="") frames
#@ Integer (persist=false, label="Output scale (% of original)", description="Both x and y. 100 = full resolution, 50 = half width & height, The pixel size is scaled to match, so measurements stay in real units, and the filename gains _downscale<PC>pc. For looking, not for measuring.", min="1", max="100", value=100) scalePercent
#@ Boolean (persist=false, label="Verify output", description="Read each written file back and check its pixels against the checksum taken while writing", value=true) verify
#@ Boolean (persist=false, label="Skip existing targets", description="An output whose TIFF and _gather.txt are both already in the output directory is left alone, so an interrupted run can be resumed", value=true) skipExisting

// Make_LuxendoTiff.groovy
//
// The two tables from Make_LuxendoSheets -> one TIFF per included series.
//
// THIS IS FOR TUNING AND FOR LOOKING, NOT A REQUIRED STEP.
//   `Run_NucleusSelector` needs a real openable file to tune a threshold
//   against, and drag-and-drop is worth keeping. But the batch runner reads the
//   `.lux.h5` through the same two tables and assembles channels in memory, so
//   it never reads anything written here. That is deliberate: the sources ARE
//   the pixels and TIFF does not compress them, so a required conversion step
//   would mean holding two copies of an 800 GB acquisition for as long as the
//   analysis exists.
//
//   So: set include=false on all but a few representative series first.
//
// WHAT COMES FROM WHERE
//   series.tsv    which series to build (`include`), and `path` -- the
//                 acquisition directory that `source_path` is relative to
//   sources.tsv   which files feed each series, and their shape
//
// The output NAME is derived from `series_id` -- as it stands in series.tsv,
// so a hand-edited id names the file -- never stored, with no second copy of
// the name to keep in step.
//
// TIME POINTS. A series is a whole stack -- 96 frames, ~94 GB, on the real
// acquisition -- which neither classic TIFF nor the heap can hold. Choose time
// points (`frames`) and each is written to its own file, `<series_id>_t<TTTT>`.
// Blank writes every frame of a series into one file, which suits a short time
// course. An output too big for the heap is a FAILED row naming `frames`,
// decided from the tables before anything is read.
//
// FORMAT: `tiff` is an ImageJ hyperstack TIFF -- what Fiji opens natively, with
// channels, slices and calibration intact, and capped at ~3.9 GB of pixels.
// `bigtiff` lifts the cap but is the degraded path: ImageJ's own opener does
// not read it as cleanly, so it is offered rather than defaulted to. An output
// too large for `tiff` is named before anything is written AND recorded as a
// FAILED row naming the fix, and the rest of the run still happens.
//
// SCALE is a PERCENTAGE OF THE ORIGINAL: 100 is full resolution, 50 is half the
// width and half the height. The calibration is scaled to match what was
// actually written, and `_downscale<PC>pc` goes in the filename, so a
// downscaled file cannot be mistaken for full resolution by anybody who meets
// it later without the tables.
//
// Headless:
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run scripts/groovy/Make_LuxendoTiff.groovy \
//     "seriesFile='/p/series.tsv',sourcesFile='/p/sources.tsv',outdir='/p/out',scalePercent=100,frames='0'"

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
if (libDir == null || !new File(libDir, "TiffAssembler.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
def LIBDIR = libDir.getAbsolutePath()

def gcl    = new GroovyClassLoader()
def TA     = gcl.parseClass(new File(LIBDIR, "TiffAssembler.groovy"))
def LS     = gcl.parseClass(new File(LIBDIR, "LuxendoScan.groovy"))
def TSV    = gcl.parseClass(new File(LIBDIR, "Tsv.groovy"))
def SCHEMA = gcl.parseClass(new File(LIBDIR, "SheetSchema.groovy")).loadFromLibDir(LIBDIR)

// Validated HERE, in code. A `#@ String` with choices={} and an `#@ Integer`
// with min/max are NOT validated on the command line -- SciJava passes the
// value straight through -- so a stale caller must not be able to pick a format
// or a scale by accident.
def fmt = TA.checkFormat(format)
int pct = TA.checkScalePercent(scalePercent)
def frameSel = TA.parseFrames(frames)

def readSheet = { File f, String sheet ->
    if (f == null || !f.isFile()) throw new IllegalArgumentException("No such table: " + f)
    def rows = TSV.read(f)
    if (!rows) throw new IllegalArgumentException("Table has no rows: " + f)
    // An old series table is named as one, not reported as a missing column.
    if (sheet == SCHEMA.SERIES) SCHEMA.requireId(rows, f.getName())
    def absent = SCHEMA.required(sheet) - rows[0].keySet().toList()
    if (absent) {
        def old = (sheet == "sources" && rows[0].containsKey("series_id"))
        throw new IllegalArgumentException(
            f.getName() + " is missing required " + sheet + " column(s): " + absent.join(", ") +
            "\n  found: " + rows[0].keySet().join(", ") +
            (old ? "\n  It was written before v0.7.0, which keys sources on (alias, series_index). " +
                   "Regenerate both tables with Make_LuxendoSheets." : ""))
    }
    return rows
}

def seriesRows  = readSheet(seriesFile,  SCHEMA.SERIES)
def sourceRows  = readSheet(sourcesFile, "sources")

// Tsv reads everything as text; the assembler does arithmetic on these.
def ints = ["t", "channel", "size_x", "size_y", "size_z", "source_bytes"]
def dbls = ["pixel_width", "pixel_height", "pixel_depth"]
sourceRows.each { r ->
    ints.each { k -> if (r[k] != null && r[k].toString().trim()) r[k] = r[k].toString().trim() as Integer }
    // BLANK stays null, never 0.0 -- a z step that does not exist must not
    // arrive as a usable-looking number.
    dbls.each { k -> r[k] = (r[k] != null && r[k].toString().trim()) ? (r[k].toString().trim() as Double) : null }
}

// THE JOIN, CHECKED. The two tables meet on (alias, series_index) -- never on
// series_id, which you may edit -- through LuxendoScan.withSeriesId(), the one
// implementation of that join. A source with no series row stops the run: a
// join that matches nothing is the failure this repo is built around.
def joined = LS.withSeriesId(sourceRows, seriesRows)
sourceRows = joined.sources
def includeBySeries = seriesRows.collectEntries { [(it.series_id as String): it.include] }
def srcIds = sourceRows.collect { it.series_id as String }.toSet()
def unsourced = joined.unsourced
if (unsourced) {
    // Not fatal: a person may have trimmed the sources table on purpose. Loud,
    // because the alternative is quietly building fewer files than the series
    // table implies.
    IJ.log("WARNING: " + unsourced.size() + " series in " + seriesFile.getName() +
           " have no sources and cannot be built: " + unsourced.sort().take(5).join(", ") +
           (unsourced.size() > 5 ? ", ..." : ""))
}

// `path` on a Luxendo series row is the ACQUISITION DIRECTORY -- the root that
// source_path is relative to. All included rows must agree on it.
def roots = seriesRows.findAll { srcIds.contains(it.series_id as String) }
                      .collect { (it.path ?: "").toString() }.unique()
if (roots.size() != 1 || !roots[0]) {
    throw new IllegalArgumentException(
        "The series table must name exactly one acquisition directory in `path`; found " +
        (roots ? roots.join(", ") : "none"))
}
def srcRoot = new File(roots[0])
if (!srcRoot.isDirectory()) {
    throw new IllegalArgumentException("Not a directory: " + srcRoot.getAbsolutePath() +
        "\n  `path` in the series table is the acquisition directory. Has the volume moved?")
}

IJ.log("=== Luxendo -> TIFF ===")
IJ.log("  series : " + seriesFile.getAbsolutePath() + "  (" + seriesRows.size() + " row(s))")
IJ.log("  sources: " + sourcesFile.getAbsolutePath() + "  (" + sourceRows.size() + " row(s))")
IJ.log("  images : " + srcRoot.getAbsolutePath())
IJ.log("  output : " + outdir.getAbsolutePath())
IJ.log("  format : " + fmt + (pct < 100 ? ("  downscaled to " + pct + "%") : ""))
IJ.log("  frames : " + (frameSel == null ? "all, one file per series" : (frameSel.toString() + ", one file per time point")))

def sums = TA.load(LIBDIR).assembleAll(sourceRows, srcRoot, outdir,
                                       [format: fmt, scalePercent: pct, verify: verify, frames: frames,
                                        skipExisting: skipExisting,
                                        includeBySeries: includeBySeries]) { IJ.log(it) }

// Rectangular whatever happened: a skipped or failed output still has a row
// saying which and why, so a half-completed run is legible.
def summaryCols = ["output_path", "series_id", "t", "frames", "channels",
                   "format", "scale_percent", "status", "reason", "bytes", "checksum", "verified"]
outdir.mkdirs()
TSV.write(sums, new File(outdir, "gather_summary.tsv"), summaryCols)

def written = sums.count { it.status == "written" }
def skipped = sums.count { it.status == "skipped" }
def failed  = sums.count { it.status == "failed" }
IJ.log("")
IJ.log("  written " + written + ", skipped " + skipped + ", failed " + failed)
IJ.log("  summary: " + new File(outdir, "gather_summary.tsv").getAbsolutePath())
if (failed > 0) {
    IJ.log("")
    IJ.log(failed + " output(s) FAILED -- the rest completed. In gather_summary.tsv:")
    sums.findAll { it.status == "failed" }.take(10).each { IJ.log("  " + it.output_path + "  " + it.reason) }
}
IJ.log("Done: Luxendo -> TIFF")
