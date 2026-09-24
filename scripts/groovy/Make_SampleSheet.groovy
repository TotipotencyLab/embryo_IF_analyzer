#@ File    (label="Files table (files.tsv)", style="file", required=false) filesSheet
#@ File    (label="Sample sheet to write (samples.tsv)", style="save") outSheet
#@ String  (label="Image root (blank = paths as given)", value="") imageRoot
#@ String  (label="File columns to seed onto each series (blank = all)", value="") inherit
#@ String  (label="SCAN MODE: directory to list instead (blank = off)", value="") scanDir
#@ String  (label="Scan: extensions", value="lif czi nd2 tif tiff") scanExt
#@ Boolean (label="Skip files already in the sheet whose size is unchanged", value=false) skipExplored
#@ String  (label="Reseed these columns from files.tsv (space separated)", value="") reseed
#@ Boolean (label="Reseed EVERY seeded column (includes alias, rewrites prefixes)", value=false) reseedAll
#@ Boolean (label="Drop sheet rows whose file has left files.tsv", value=false) prune

// Make_SampleSheet.groovy
//
// files.tsv -> samples.tsv. A `#@` block and one call; the work is in
// SampleSheet.groovy so it can be tested without a dialog.
//
// Two modes:
//
//   SCAN   set scanDir. Lists the images in that directory and writes a
//          files.tsv skeleton to outSheet, with alias defaulted to the
//          basename. Nothing is opened. Edit it, then run the other mode.
//
//   BUILD  the default. Reads files.tsv, reads each image's metadata (no
//          pixels), and writes one row per series. If outSheet already exists
//          it is MERGED: machine columns refresh, your columns survive.
//
// Headless:
//
//   ImageJ-macosx --headless --console --run scripts/groovy/Make_SampleSheet.groovy \
//     "filesSheet='/path/files.tsv',outSheet='/path/samples.tsv',imageRoot='/path/raw'"

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
if (libDir == null || !new File(libDir, "SampleSheet.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
def LIBDIR = libDir.getAbsolutePath()

def SS = new GroovyClassLoader(this.class.classLoader)
             .parseClass(new File(LIBDIR + "/SampleSheet.groovy"))
def TSV = new GroovyClassLoader(this.class.classLoader)
             .parseClass(new File(LIBDIR + "/Tsv.groovy"))
def sheet = SS.load(LIBDIR)

def root = (imageRoot?.trim()) ? new File(imageRoot.trim()) : null
def splitList = { String s -> (s ?: "").trim() ? s.trim().split(/[\s,]+/).toList() : [] }

// --- SCAN mode -------------------------------------------------------------
if (scanDir?.trim()) {
    def rows = sheet.scan(new File(scanDir.trim()), splitList(scanExt))
    if (rows.isEmpty()) {
        IJ.log("No images matching '" + scanExt + "' in " + scanDir)
        IJ.log("Done: nothing written")
        return
    }
    TSV.write(rows, outSheet, ["path", "alias", "include"])
    IJ.log("Scanned " + scanDir + ": " + rows.size() + " file(s)")
    rows.each { IJ.log("  " + it.path + "  alias=" + it.alias) }
    IJ.log("Wrote " + outSheet.getAbsolutePath())
    IJ.log("Edit it -- add condition/genotype columns, set include -- then run this again without scanDir.")
    IJ.log("Done: " + rows.size() + " file(s)")
    return
}

// --- BUILD mode ------------------------------------------------------------
if (filesSheet == null || !filesSheet.isFile()) {
    IJ.error("No files table.\n\nSet filesSheet, or set scanDir to make one first.")
    return
}

def fileRows = sheet.readFiles(filesSheet)
IJ.log("=== " + filesSheet.getName() + ": " + fileRows.size() + " file(s) ===")

// Fatal checks first: nothing is read until the table itself is sound.
sheet.checkFiles(fileRows, root).each { IJ.log("WARNING: " + it) }

// Which file columns are seeded onto the series rows. Default: everything that
// is not already a sample column, so metadata typed once per file lands on all
// of its series without being asked for.
def sampleCols = sheet.schema.columns("samples")
def inheritCols = splitList(inherit)
if (inheritCols.isEmpty()) {
    inheritCols = fileRows[0].keySet().findAll { !(it in ["path", "alias", "include"]) && !(it in sampleCols) }.toList()
}
if (inheritCols) IJ.log("  seeding onto each series: " + inheritCols.join(", "))

def existing = outSheet.isFile() ? TSV.read(outSheet) : []
if (existing) IJ.log("  merging into " + existing.size() + " existing row(s)")

// --skip_explored means "skip the UNCHANGED", never "trust blindly": a file
// whose recorded size no longer matches is re-read anyway and said so.
def toRead = fileRows
if (skipExplored && existing) {
    def seen = [:]
    existing.each { r -> seen[r.path] = r.file_size }
    def skipped = []
    toRead = fileRows.findAll { fr ->
        def img = SS.resolve(fr.path, root)
        def known = seen[fr.path]
        if (known == null) return true
        if (!img.isFile()) return true
        if (known.toString() != img.length().toString()) {
            IJ.log("  size changed, re-reading: " + fr.path +
                   " (" + known + " -> " + img.length() + ")")
            return true
        }
        skipped << fr.path
        return false
    }
    if (skipped) IJ.log("  skipped " + skipped.size() + " unchanged file(s)")
}

def fresh = sheet.build(toRead, root, inheritCols) { IJ.log(it) }

// Rows for files we skipped are carried over untouched, so the sheet stays a
// complete inventory rather than shrinking to whatever was read this time.
if (skipExplored && existing) {
    def readPaths = toRead.collect { it.path } as Set
    fresh = fresh + existing.findAll { !readPaths.contains(it.path) }
}

def reseedCols = reseedAll ? sheet.schema.columns("samples", "seeded") : splitList(reseed)
if (reseedCols) IJ.log("  reseeding: " + reseedCols.join(", "))

def merged = sheet.merge(fresh, existing, reseedCols, prune)
def rows = merged.rows

// The composed prefix is checked AFTER the merge, because an edited prefix is
// as capable of colliding as a generated one.
sheet.checkPrefixes(rows)

TSV.write(rows, outSheet, sheet.columnOrder(rows))

IJ.log("  " + merged.added + " row(s) added, " + merged.updated + " updated")
merged.reseeded.each { c, n -> IJ.log("  reseeded " + c + " on " + n + " row(s)") }
if (merged.missing) {
    IJ.log((prune ? "  DROPPED " : "  KEPT ") + merged.missing.size() +
           " row(s) whose file is no longer in " + filesSheet.getName() + ":")
    merged.missing.take(5).each { IJ.log("    " + it.prefix) }
    if (!prune) IJ.log("    (pass prune to remove them)")
}
int nOn = rows.count { (it.include ?: "true").toString().toLowerCase() in ["true", "yes", "1"] }
IJ.log("Wrote " + outSheet.getAbsolutePath())
IJ.log("Done: " + rows.size() + " series, " + nOn + " included")
