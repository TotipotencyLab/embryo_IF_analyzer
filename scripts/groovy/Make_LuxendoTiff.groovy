#@ File    (persist=true,  label="Luxendo acquisition directory", style="directory") luxDir
#@ File    (persist=true,  label="Output directory", style="directory") outdir
#@ File    (persist=false, label="Manifest table (blank = scan the directory now)", style="file", required=false) manifestFile
#@ Boolean (persist=false, label="Stop after the manifest (write the table, convert nothing)", description="Scan the acquisition and write manifest.tsv, then stop. Edit include= in that table and run again to convert.", value=true) manifestOnly
#@ String  (persist=false, label="Output format", description="TIFF can be reopened easily in ImageJ, but have size limit of ~4GB. BigTIFF can hold larger file, but may not be compatible with ImageJ", value="tiff", choices={"tiff","bigtiff"}) format
#@ Integer (persist=false, label="Downscale factor (1 = full resolution, 2 = half width and height)", description="Divides both x and y. The pixel size is multiplied to match, and the filename gains _ds<N>. For looking, not for measuring.", value=1) resize
#@ Boolean (persist=false, label="Skip outputs already assembled here", description="An output whose TIFF and _gather.txt are both already in the output directory is left alone, so an interrupted run can be resumed", value=true) skipExisting
#@ Boolean (persist=false, label="Verify output", description="Read each written file back and check its pixels against the checksum taken while writing", value=true) verify

// Make_LuxendoTiff.groovy
//
// Luxendo .lux.h5 files -> one TIFF per (position, time point), which is what
// the rest of the pipeline already reads.
//
// IT ALWAYS WRITES THE MANIFEST, AND ALWAYS BEFORE ANY PIXELS.
//
//   outdir/manifest.tsv    what this run will convert, one row per source file
//
// So the table exists whether or not the conversion then runs, whether or not
// it succeeds, and whatever kills the JVM afterwards. `manifestOnly` decides
// only WHERE THE RUN STOPS -- which is the actual question, because deciding
// what to convert should not cost the tens of minutes that converting does:
//
//   manifestOnly=true   scan, write manifest.tsv, stop. Nothing is read twice
//                       and no pixels are read at all.
//   manifestOnly=false  ... and then assemble.
//
// The intended loop is: run once with manifestOnly, open manifest.tsv, set
// include=false on what you do not want, point `manifestFile` at it and run
// again. Handing the edited table back is what makes the second run do exactly
// what you read in the first.
//
// `luxDir` is needed either way: source paths in the manifest are relative to
// it, and nothing in the table is an absolute path.
//
// FORMAT: `tiff` is an ImageJ hyperstack TIFF -- what Fiji opens natively, with
// channels, slices and calibration intact, and capped at ~3.9 GB of pixels.
// `bigtiff` lifts the cap but is the degraded path: ImageJ's own opener does
// not read it as cleanly, so it is offered rather than defaulted to. An output
// too large for `tiff` is named before anything is written AND recorded as a
// FAILED row naming the fix, and the rest of the run still happens.
//
// RESIZE is a DIVISOR, not a size: 2 means half the width and half the height.
// It scales the calibration with the pixels -- halve the pixels and the pixel
// size doubles -- and puts `_ds<N>` in the filename, so a downscaled file
// cannot be mistaken for full resolution. It is for looking, not measuring.
//
// Headless:
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run scripts/groovy/Make_LuxendoTiff.groovy \
//     "luxDir='/path/to/2026-09-10_184731',outdir='/path/out',manifestOnly=false,format='tiff',resize=1,verify=true"

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
def LS     = gcl.parseClass(new File(LIBDIR, "LuxendoScan.groovy"))
def TA     = gcl.parseClass(new File(LIBDIR, "TiffAssembler.groovy"))
def TSV    = gcl.parseClass(new File(LIBDIR, "Tsv.groovy"))
def SCHEMA = gcl.parseClass(new File(LIBDIR, "SheetSchema.groovy")).loadFromLibDir(LIBDIR)

// Validated HERE, in code. A `#@ String` with choices={} is NOT validated on
// the command line -- SciJava passes any string straight through -- so a stale
// caller must not be able to select a format by accident.
def fmt = TA.checkFormat(format)
int ds  = Math.max(1, (resize ?: 1) as int)

IJ.log("=== Luxendo -> TIFF ===")
IJ.log("  source : " + luxDir.getAbsolutePath())
IJ.log("  output : " + outdir.getAbsolutePath())
if (!manifestOnly) {
    IJ.log("  format : " + fmt + (ds > 1 ? ("  downscale x" + ds) : ""))
}

// ---- 1. the manifest -------------------------------------------------------

def rows
if (manifestFile != null && manifestFile.isFile()) {
    IJ.log("  manifest: reading " + manifestFile.getAbsolutePath())
    rows = TSV.read(manifestFile)
    if (!rows) throw new IllegalArgumentException("Manifest has no rows: " + manifestFile)
    def required = SCHEMA.required("manifest")
    def absent = required - rows[0].keySet().toList()
    if (absent) {
        throw new IllegalArgumentException(
            "Manifest is missing required column(s): " + absent.join(", ") +
            "\n  found: " + rows[0].keySet().join(", "))
    }
    // Tsv reads everything as text; the assembler does arithmetic on these.
    def ints = ["t", "channel", "size_x", "size_y", "size_z", "source_bytes"]
    def dbls = ["pixel_width", "pixel_height", "pixel_depth"]
    rows.each { r ->
        ints.each { k -> if (r[k] != null && r[k].toString().trim()) r[k] = r[k].toString().trim() as Integer }
        // BLANK stays null, never 0.0 -- a z step that does not exist must not
        // arrive as a usable-looking number.
        dbls.each { k -> r[k] = (r[k] != null && r[k].toString().trim()) ? (r[k].toString().trim() as Double) : null }
    }
} else {
    IJ.log("  manifest: none given, scanning now")
    rows = LS.load(LIBDIR).scan(luxDir) { IJ.log(it) }
    if (!rows) throw new IllegalArgumentException("No .lux.h5 files holding pixels under " + luxDir)
}

// Written before a single plane is read, and written even when the run stops
// here. Column order from the schema, so the file and the declaration cannot
// drift.
outdir.mkdirs()
def manifestOut = new File(outdir, "manifest.tsv")
def cols = SCHEMA.columns("manifest")
def missing = cols - rows[0].keySet().toList()
if (missing) {
    // The scanner and the schema disagreeing is a bug, not a bad input.
    throw new IllegalStateException(
        "Manifest is missing declared column(s): " + missing.join(", "))
}
if (manifestFile != null && manifestFile.isFile() &&
    manifestFile.getCanonicalPath() == manifestOut.getCanonicalPath()) {
    // Handed back the very file we would write. Rewriting it would be a no-op
    // at best and would clobber an edit at worst.
    IJ.log("  manifest: is already " + manifestOut.getAbsolutePath() + ", left as it is")
} else {
    TSV.write(rows, manifestOut, cols)
    IJ.log("  manifest: wrote " + manifestOut.getAbsolutePath())
}

def outputs   = rows.collect { it.target_output_path }.unique()
def positions = rows.collect { it.position_id }.unique()
IJ.log("  " + rows.size() + " source file(s) -> " + outputs.size() +
       " output(s) across " + positions.size() + " position(s)")
IJ.log("  time points: " + rows.collect { it.t }.unique().sort())

if (manifestOnly) {
    IJ.log("")
    IJ.log("  Stopped after the manifest -- nothing was converted.")
    IJ.log("  Edit include= in the table above, then run again with")
    IJ.log("  manifestFile pointed at it and 'Stop after the manifest' off.")
    IJ.log("Done: Luxendo manifest")
    return
}

// ---- 2. the pixels ---------------------------------------------------------

def sums = TA.load(LIBDIR).assembleAll(rows, luxDir, outdir,
                                       [format: fmt, resize: ds, verify: verify,
                                        skipExisting: skipExisting]) { IJ.log(it) }

// Rectangular whatever happened: a skipped or failed output still has a row
// saying which and why, so a half-completed run is legible.
def summaryCols = ["output_path", "series_id", "position_id", "t", "channels",
                   "format", "resize", "status", "reason", "bytes", "checksum", "verified"]
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
