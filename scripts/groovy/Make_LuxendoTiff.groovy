#@ File    (persist=false, label="Luxendo acquisition directory", style="directory") luxDir
#@ File    (persist=false, label="Manifest table (blank = scan the directory now)", style="file", required=false) manifestFile
#@ File    (persist=false, label="Output directory", style="directory") outdir
#@ String  (persist=false, label="Output format", value="tiff", choices={"tiff","bigtiff"}) format
#@ Integer (persist=false, label="Downscale x and y by (1 = full resolution)", value=1) resize
#@ Boolean (persist=false, label="Verify each output by reading it back", value=true) verify
#@ Boolean (persist=false, label="Save the manifest that was used, beside the outputs", value=true) saveManifest

// Make_LuxendoTiff.groovy
//
// Luxendo .lux.h5 files -> one TIFF per (position, time point), which is what
// the rest of the pipeline already reads.
//
// TWO WAYS TO RUN IT, and the difference is only where the manifest comes from:
//
//   end to end      leave `manifestFile` blank. Scans, then assembles.
//   from a manifest give `manifestFile`. Assembles exactly what the table says,
//                   so you can set include=false on rows first. `luxDir` is
//                   still needed: source paths in the manifest are relative
//                   to it.
//
// Make_LuxendoManifestTable writes the table without converting anything, which
// is how you look before committing to tens of gigabytes.
//
// FORMAT: `tiff` is an ImageJ hyperstack TIFF -- what Fiji opens natively, with
// channels, slices and calibration intact, and capped at ~3.9 GB of pixels.
// `bigtiff` lifts the cap but is the degraded path: ImageJ's own opener does
// not read it as cleanly, so it is offered rather than defaulted to. An output
// too large for `tiff` is reported as a FAILED row naming the fix, and the rest
// of the run still happens.
//
// RESIZE scales the calibration with the pixels -- halve the pixels and the
// pixel size doubles -- and puts `_ds<N>` in the filename, so a downscaled file
// cannot be mistaken for full resolution. It is for looking, not measuring.
//
// Headless:
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run scripts/groovy/Make_LuxendoTiff.groovy \
//     "luxDir='/path/to/2026-09-10_184731',outdir='/path/out',format='tiff',resize=1,verify=true"

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
IJ.log("  format : " + fmt + (ds > 1 ? ("  downscale x" + ds) : ""))

def rows
if (manifestFile != null && manifestFile.isFile()) {
    IJ.log("  manifest: " + manifestFile.getAbsolutePath())
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
}

def sums = TA.load(LIBDIR).assembleAll(rows, luxDir, outdir, [format: fmt, resize: ds, verify: verify]) { IJ.log(it) }

// Rectangular whatever happened: a skipped or failed output still has a row
// saying which and why, so a half-completed run is legible.
def summaryCols = ["output_path", "series_id", "position_id", "t", "channels",
                   "format", "resize", "status", "reason", "bytes", "checksum", "verified"]
outdir.mkdirs()
TSV.write(sums, new File(outdir, "gather_summary.tsv"), summaryCols)

if (saveManifest) {
    TSV.write(rows, new File(outdir, "manifest_used.tsv"), SCHEMA.columns("manifest"))
}

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
