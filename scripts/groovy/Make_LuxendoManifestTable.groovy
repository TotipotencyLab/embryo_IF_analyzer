#@ File   (persist=false, label="Luxendo acquisition directory", style="directory") luxDir
#@ File   (persist=false, label="Manifest table to write (manifest.tsv)", style="save") outFile

// Make_LuxendoManifestTable.groovy
//
// A Luxendo acquisition directory -> the assembly manifest, and nothing else.
// No pixels are read and no images are written.
//
// WHY IT IS ITS OWN STEP
//   Converting a Luxendo run is tens of gigabytes and tens of minutes. Deciding
//   what to convert should cost neither. This writes the plan as a table you can
//   open, read, edit and diff BEFORE committing to any of that -- set
//   include=false on the positions you do not want, and hand the same table to
//   Make_LuxendoTiff.
//
// IDENTITY COMES FROM EACH FILE'S OWN METADATA, never from its path. See
// LuxendoScan for why, and note/luxendo_file_format.md for the format.
//
// The columns are declared in schema/sheet_columns.tsv under sheet `manifest`,
// which both languages read at run time -- so this script does not name them.
//
// Headless:
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run scripts/groovy/Make_LuxendoManifestTable.groovy \
//     "luxDir='/path/to/2026-09-10_184731',outFile='/path/to/manifest.tsv'"

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

IJ.log("=== Luxendo manifest ===")
IJ.log("  scanning: " + luxDir.getAbsolutePath())

def rows = LS.load(LIBDIR).scan(luxDir) { IJ.log(it) }

// Column order from the schema, so the file and the declaration cannot drift.
def cols = SCHEMA.columns("manifest")
def missing = cols - rows[0].keySet().toList()
if (missing) {
    // The scanner and the schema disagreeing is a bug, not a bad input.
    throw new IllegalStateException(
        "Scanner did not produce declared manifest column(s): " + missing.join(", "))
}
TSV.write(rows, outFile, cols)

def outputs = rows.collect { it.output_path }.unique()
def positions = rows.collect { it.position_id }.unique()
IJ.log("")
IJ.log("  " + rows.size() + " source file(s)")
IJ.log("  " + outputs.size() + " output(s) across " + positions.size() + " position(s)")
IJ.log("  time points: " + rows.collect { it.t }.unique().sort())
IJ.log("  wrote " + outFile.getAbsolutePath())
IJ.log("")
IJ.log("  Edit include= to choose what gets converted, then run Make_LuxendoTiff.")
IJ.log("Done: Luxendo manifest")
