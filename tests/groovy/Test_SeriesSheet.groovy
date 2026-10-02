// Test_SeriesSheet.groovy
//
// files.tsv -> series.tsv, on images this test WRITES rather than ships.
//
// The fixture is generated, not tracked: a multi-series OME-TIFF is a binary
// and the thing worth keeping under review is the code that makes it. It is
// cross-checked against the real Leica .lif by hand -- the shapes it exercises
// (several series, one of them single-plane, names that need sanitising) are
// the ones that file actually has.
//
// Run headless from the repo root:
//
//   ImageJ-macosx --headless --console --run tests/groovy/Test_SeriesSheet.groovy

import loci.formats.MetadataTools
import loci.formats.out.OMETiffWriter
import ome.units.UNITS
import ome.units.quantity.Length
import ome.xml.model.enums.DimensionOrder
import ome.xml.model.enums.PixelType
import ome.xml.model.primitives.PositiveInteger

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "SeriesSheet.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-58s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def errOf = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() } }

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_samplesheet_" + System.nanoTime())
tmp.mkdirs()

/**
 * Write a multi-series OME-TIFF.
 *
 * `specs` is a list of [name, sizeX, sizeY, sizeZ] -- one entry per series.
 * Physical sizes are set so the sheet has something real to copy, and a
 * single-plane series is included because that is where physicalSizeZ is null
 * on the real .lif.
 */
def writeMultiSeries = { File dest, List specs, double physX, double physZ ->
    def meta = MetadataTools.createOMEXMLMetadata()
    specs.eachWithIndex { spec, int i ->
        def (String nm, int sx, int sy, int sz) = spec
        MetadataTools.populateMetadata(meta, i, nm, false, DimensionOrder.XYZCT.toString(),
                                       PixelType.UINT8.toString(), sx, sy, sz, 1, 1, 1)
        meta.setPixelsPhysicalSizeX(new Length(physX, UNITS.MICROMETER), i)
        meta.setPixelsPhysicalSizeY(new Length(physX, UNITS.MICROMETER), i)
        // Only a real z stack gets a z step, exactly as Bio-Formats reports it.
        if (sz > 1) meta.setPixelsPhysicalSizeZ(new Length(physZ, UNITS.MICROMETER), i)
    }
    def w = new OMETiffWriter()
    w.setMetadataRetrieve(meta)
    w.setId(dest.getAbsolutePath())
    specs.eachWithIndex { spec, int i ->
        def (String nm, int sx, int sy, int sz) = spec
        w.setSeries(i)
        (0..<sz).each { int z -> w.saveBytes(z, new byte[sx * sy]) }
    }
    w.close()
    return dest
}

println "=== generate the multi-series fixture ==="
def rawDir = new File(tmp, "raw"); rawDir.mkdirs()
def imgA = writeMultiSeries(new File(rawDir, "plateA.ome.tif"),
    [["Series001", 32, 24, 4], ["Series002", 32, 24, 3], ["Image005 Denoised", 16, 16, 1]],
    0.25d, 0.9d)
// A SECOND file whose series names are the same -- the collision the alias
// exists to solve, and the one a real batch of .lif files always has.
// Two series under ONE name: a tile scan is many fields of one acquisition, and
// this is the shape that could not produce a sheet at all before the index was
// forced into the series_id.
def imgT = writeMultiSeries(new File(rawDir, "tiles.ome.tif"),
    [["O1_1 10x", 16, 16, 1], ["O1_1 10x", 16, 16, 1]], 0.5d, 1.0d)

def imgB = writeMultiSeries(new File(rawDir, "plateB.ome.tif"),
    [["Series001", 40, 40, 2], ["Series002", 40, 40, 2]],
    0.5d, 1.1d)
check("fixture A written",                     imgA.isFile(), true)
check("fixture B written",                     imgB.isFile(), true)

def SS = new GroovyClassLoader().parseClass(new File(LIBDIR, "SeriesSheet.groovy"))
def TSV = new GroovyClassLoader().parseClass(new File(LIBDIR, "Tsv.groovy"))
def sheet = SS.load(LIBDIR)

println ""
println "=== the schema loads and knows its owners ==="
check("series has a series_id column",         sheet.schema.columns("series").contains("series_id"), true)
check("series_id is seeded, not machine",      sheet.schema.owner("series", "series_id"), "seeded")
check("series_index is machine",               sheet.schema.owner("series", "series_index"), "machine")
check("include is seeded",                     sheet.schema.owner("series", "include"), "seeded")
check("path is a files column",                sheet.schema.columns("files").contains("path"), true)

println ""
println "=== scan mode ==="
def scanned = sheet.scan(rawDir, ["ome.tif", "tif", "lif"])
check("scan finds every image",                scanned.size(), 3)
check("alias defaults to the basename",        scanned[0].alias, "plateA.ome")
check("...and scan is sorted by name",         scanned.collect { it.path },
      ["plateA.ome.tif", "plateB.ome.tif", "tiles.ome.tif"])
check("include defaults on",                   scanned[0].include, "true")

println ""
println "=== build: one row per series, series_ids unique across files ==="
def filesTsv = new File(tmp, "files.tsv")
TSV.write([[path: "plateA.ome.tif", alias: "A", include: "true", condition: "wt"],
           [path: "plateB.ome.tif", alias: "B", include: "false", condition: "ko"]],
          filesTsv, ["path", "alias", "include", "condition"])

def fileRows = sheet.readFiles(filesTsv)
check("files.tsv reads two rows",              fileRows.size(), 2)
check("checkFiles is quiet on a clean table",  sheet.checkFiles(fileRows, rawDir), [])

def built = sheet.build(fileRows, rawDir, ["condition"])
check("5 series across the two files",         built.size(), 5)
check("series_id is alias + index + series",   built[0].series_id, "A_s0000_Series001")
// Same series name, different file -- unique only because of the alias.
check("the other file's Series001 differs",    built.find { it.path == "plateB.ome.tif" && it.series_index == 0 }.series_id, "B_s0000_Series001")
check("a space in the name is sanitised",      built[2].series_id, "A_s0002_Image005_Denoised")
check("no series_id collision",                errOf { sheet.checkIds(built) }, null)

println ""
println "=== the machine columns are the file's own facts ==="
def a0 = built[0]
check("size_x",                                a0.size_x, 32)
check("size_z",                                a0.size_z, 4)
check("pixel_width",                           a0.pixel_width, 0.25d)
check("pixel_depth on a stack",                a0.pixel_depth, 0.9d)
check("file_size is recorded",                 a0.file_size, imgA.length())
def plane = built.find { it.series_name == "Image005 Denoised" }
// The single-plane case: null, not 1.0, and not the stack's 0.9.
check("pixel_depth is null on a single plane", plane.pixel_depth, null)
check("...and it really is one slice",         plane.size_z, 1)
def b0 = built.find { it.path == "plateB.ome.tif" }
check("the second file keeps its own pixel size", b0.pixel_width, 0.5d)

println ""
println "=== include and metadata are seeded from the file row ==="
check("include seeded from the file",          b0.include, "false")
check("...and the other file is on",           a0.include, "true")
check("condition seeded onto every series",    built.every { it.condition in ["wt", "ko"] }, true)
check("A's series carry A's condition",        a0.condition, "wt")

println ""
println "=== the checks that must stop the run ==="
def dupPath = [[path: "x.tif", alias: "a", include: "true"], [path: "x.tif", alias: "b", include: "true"]]
check("a repeated path is fatal",              errOf { sheet.checkFiles(dupPath, rawDir) }?.contains("same path"), true)
def dupAlias = [[path: "x.tif", alias: "a", include: "true"], [path: "y.tif", alias: "a", include: "true"]]
check("a repeated alias is fatal",             errOf { sheet.checkFiles(dupAlias, rawDir) }?.contains("same alias"), true)

// Unique alias, unique series names, COLLIDING composite. Much rarer now that
// the index sits between them, but still reachable with an alias that ends the
// way an index begins -- which is why the check runs on the composed string and
// not on the parts.
def collide = [[series_id: sheet.composeSeriesId("A", 0, "s0000_B"), path: "p", series_index: 0, series_name: "s0000_B"],
               [series_id: sheet.composeSeriesId("A_s0000", 0, "B"), path: "q", series_index: 0, series_name: "B"]]
check("the two compose to the same series_id", collide[0].series_id, collide[1].series_id)
check("...and that is fatal",                  errOf { sheet.checkIds(collide) }?.contains("not unique"), true)
check("duplicateIds names the offender",  sheet.duplicateIds(collide).keySet().toList(), [collide[0].series_id])
check("...and is quiet on a clean table",      sheet.duplicateIds(built), [:])

// sanitise() collapses whitespace, so "Image005 Denoised" and
// "Image005_Denoised" become one filename. They are different SERIES of one
// file, so the index now separates them -- this used to be fatal.
def sanitiseClash = [[series_id: sheet.composeSeriesId("A", 0, "Image005 Denoised"), path: "p", series_index: 0, series_name: "Image005 Denoised"],
                     [series_id: sheet.composeSeriesId("A", 1, "Image005_Denoised"), path: "p", series_index: 1, series_name: "Image005_Denoised"]]
check("the names still sanitise alike",
      sanitiseClash[0].series_id.replace("s0000", ""), sanitiseClash[1].series_id.replace("s0001", ""))
check("...but the index keeps them apart",     errOf { sheet.checkIds(sanitiseClash) }, null)

println ""
println "=== the index is forced, so a repeated series name is not a collision ==="
// Before this, a tile scan could not produce a sheet: checkIds refused it,
// correctly, and the error message was the only artifact of the run.
def tileRows = sheet.build([[path: "tiles.ome.tif", alias: "T", include: "true"]], rawDir, [])
check("two series really do share one name",   tileRows.collect { it.series_name }.unique(), ["O1_1 10x"])
check("...but not one series_id",              tileRows.collect { it.series_id },
      ["T_s0000_O1_1_10x", "T_s0001_O1_1_10x"])
check("...so the sheet builds",                errOf { sheet.checkIds(tileRows) }, null)

// Fixed width, not derived from the series count: a file growing from 999 to
// 1001 series must not re-pad every series_id it already had.
check("index 0 pads to four digits",           sheet.composeSeriesId("A", 0, "x"), "A_s0000_x")
check("index 331",                             sheet.composeSeriesId("A", 331, "x"), "A_s0331_x")
check("past 9999 it widens, never wraps",      sheet.composeSeriesId("A", 12345, "x"), "A_s12345_x")
check("a string index is accepted",            sheet.composeSeriesId("A", "7", "x"), "A_s0007_x")

println ""
println "=== duplicate basenames warn rather than stop ==="
def subdir = new File(tmp, "other"); subdir.mkdirs()
def copyB = new File(subdir, "plateA.ome.tif")
copyB.bytes = imgA.bytes
def copied = [[path: "raw/plateA.ome.tif", alias: "one", include: "true"],
              [path: "other/plateA.ome.tif", alias: "two", include: "true"]]
def warns = sheet.checkFiles(copied, tmp)
check("a shared basename is reported",         warns.any { it.contains("share the basename") }, true)
// The alias cannot see a copy in another folder; basename+size can.
check("a same-size copy is reported too",      warns.any { it.contains("same size") }, true)
check("neither is fatal",                      warns.size() >= 2, true)

println ""
println "=== regeneration keeps what you typed ==="
def outTsv = new File(tmp, "series.tsv")
TSV.write(built, outTsv, sheet.columnOrder(built))
def firstPass = TSV.read(outTsv)
check("written and read back",                 firstPass.size(), 5)

// Edit it the way a person would: turn one series off, add a column of your own.
firstPass[0].include = "false"
firstPass[0].cell_type = "oocyte"
firstPass[0].series_id = "A_renamed_by_hand"
TSV.write(firstPass, outTsv, sheet.columnOrder(firstPass))

def refreshed = sheet.build(fileRows, rawDir, ["condition"])
def merged = sheet.merge(refreshed, TSV.read(outTsv), [], false)
def m0 = merged.rows.find { it.path == "plateA.ome.tif" && it.series_index.toString() == "0" }
check("nothing added on a rerun",              merged.added, 0)
check("every row updated",                     merged.updated, 5)
check("an edited include survives",            m0.include, "false")
check("a column of your own survives",         m0.cell_type, "oocyte")
// The merge key is path+series_index precisely so this can be edited.
check("an edited series_id survives",          m0.series_id, "A_renamed_by_hand")
check("machine columns still refresh",         m0.size_x.toString(), "32")

println ""
println "=== reseed is opt-in, and says what it changed ==="
def reseeded = sheet.merge(refreshed, TSV.read(outTsv), ["series_id", "include"], false)
def r0 = reseeded.rows.find { it.path == "plateA.ome.tif" && it.series_index.toString() == "0" }
check("reseed restores the generated id",      r0.series_id, "A_s0000_Series001")
check("reseed restores the seeded include",    r0.include, "true")
check("it reports which columns it touched",   reseeded.reseeded.keySet().sort(), ["include", "series_id"])
check("cell_type is yours and untouched",      r0.cell_type, "oocyte")

println ""
println "=== a renumbering is refused, not merged ==="
// Same file, same index, DIFFERENT series: the file was re-exported in another
// series order. Merging would carry cell_type=oocyte onto the wrong image.
def renum = TSV.read(outTsv)
def victim = renum.find { it.path == "plateA.ome.tif" && it.series_index.toString() == "0" }
victim.series_name = "Series_that_used_to_be_here"
def renumErr = errOf { sheet.merge(refreshed, renum, [], false) }
check("the merge stops",                       renumErr?.contains("DIFFERENT series"), true)
check("...naming the row and both names",
      renumErr?.contains("plateA.ome.tif[0]") && renumErr?.contains("Series_that_used_to_be_here") &&
      renumErr?.contains("Series001"), true)
// And it is the NAME that decides: the unchanged sheet still merges, as above.
check("an unchanged sheet still merges",       errOf { sheet.merge(refreshed, TSV.read(outTsv), [], false) }, null)

println ""
println "=== a sheet from before v0.7.0 is migrated, edits and all ==="
def oldRows = TSV.read(outTsv).collect { r ->
    def c = new LinkedHashMap(); r.each { k, v -> c[k == "series_id" ? "prefix" : k] = v }; c
}
check("the old sheet has no series_id",        oldRows[0].containsKey("series_id"), false)
check("migrateOldId renames every row",        SS.migrateOldId(oldRows), oldRows.size())
check("...keeping the hand-edited value",
      oldRows.find { it.path == "plateA.ome.tif" && it.series_index.toString() == "0" }.series_id, "A_renamed_by_hand")
check("...and leaving no prefix behind",       oldRows.any { it.containsKey("prefix") }, false)
check("a current sheet is left alone",         SS.migrateOldId(TSV.read(outTsv)), 0)
def migrated = sheet.merge(refreshed, oldRows, [], false)
check("then it merges like any other",
      migrated.rows.find { it.path == "plateA.ome.tif" && it.series_index.toString() == "0" }.series_id,
      "A_renamed_by_hand")

println ""
println "=== a vanished file is reported, not silently dropped ==="
def oneFile = fileRows.findAll { it.path == "plateA.ome.tif" }
def shrunk = sheet.build(oneFile, rawDir, ["condition"])
def kept = sheet.merge(shrunk, TSV.read(outTsv), [], false)
check("rows for the absent file are kept",     kept.rows.size(), 5)
check("...and named as missing",               kept.missing.size(), 2)
def pruned = sheet.merge(shrunk, TSV.read(outTsv), [], true)
check("prune removes them on request",         pruned.rows.size(), 3)
check("...still reporting what went",          pruned.missing.size(), 2)

println ""
println "=== SeriesSpec: which series did they mean? ==="
// Shared by Inspect_ImageFile and Open_LifFile. Tested here beside Tsv, the
// other small utility both sides rely on.
def SPEC = new GroovyClassLoader().parseClass(new File(LIBDIR, "SeriesSpec.groovy"))
def specNames = ["a", "b", "tile", "tile", "tile", "z"]
def sp = { String q -> SPEC.parse(q, specNames, 6) }

check("blank means every series",              sp(""), [0, 1, 2, 3, 4, 5])
check("a single index",                        sp("3"), [3])
check("space separated",                       sp("1 3"), [1, 3])
// SciJava keeps a comma inside a quoted value, unlike argparser on the R side,
// so a comma list has to work rather than silently becoming one token.
check("comma separated",                       sp("1,3"), [1, 3])
check("an inclusive range",                    sp("1-3"), [1, 2, 3])
check("mixed, with stray whitespace",          sp("1,4, 2-3"), [1, 2, 3, 4])
check("duplicates collapse",                   sp("2 2 1-2"), [1, 2])
check("order does not matter",                 sp("5 0"), [0, 5])
check("a name selects every series with it",   sp("name:tile"), [2, 3, 4])

def errOf2 = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() } }
// Asking for 0-2000 of a 1563-series file and quietly getting 1563 is how you
// conclude you inspected everything when you did not.
check("out of range is refused",               errOf2 { sp("4-9") }?.contains("out of range"), true)
check("...naming the valid bounds",            errOf2 { sp("4-9") }?.contains("0..5"), true)
check("a backwards range is refused",          errOf2 { sp("4-2") }?.contains("backwards"), true)
check("nonsense is refused",                   errOf2 { sp("banana") }?.contains("Cannot read"), true)
check("an unknown name is refused",            errOf2 { sp("name:nope") }?.contains("No series is named"), true)
// A trailing space in a Leica name is invisible in every listing.
def padded = ["x", "tile "]
check("a whitespace-only mismatch says so",
      errOf2 { SPEC.parse("name:tile", padded, 2) }?.contains("trimming whitespace"), true)

check("describe() folds runs back up",         SPEC.describe([1, 2, 3, 7, 9, 10]), "1-3, 7, 9-10")
check("describe() on one index",               SPEC.describe([4]), "4")

println ""
println "=== Tsv refuses what would corrupt a column ==="
check("a tab in a cell is refused",            errOf { TSV.cell("a\tb") }?.contains("tab"), true)
check("a newline in a cell is refused",        errOf { TSV.cell("a\nb") }?.contains("newline"), true)
check("null becomes blank",                    TSV.cell(null), "")

println ""
println "=== Make_SeriesSheet writes the sheet, THEN refuses ==="
// The ordering lives in the `#@` front end, so it is exercised the way the
// fiji-headless-testing skill describes: strip the parameter lines and inject a
// Binding. It is worth a test rather than a read-through, because the whole
// point is WHICH HAPPENS FIRST -- a duplicate you cannot open the table to see
// is a duplicate you cannot fix, and the error message would otherwise be the
// only artifact of the run.
def msFile = new File(LIBDIR, "Make_SeriesSheet.groovy")
def msBody = msFile.getText("UTF-8").readLines().findAll { !it.trim().startsWith("#@") }.join("\n")
def runMakeSheet = { File msFilesArg, File msOutArg, boolean msAllowArg ->
    def b = new Binding()
    b.setVariable("javax.script.filename", msFile.getAbsolutePath())
    b.setVariable("filesSheet", msFilesArg)
    b.setVariable("outSheet", msOutArg)
    b.setVariable("imageRoot", rawDir.getAbsolutePath())
    b.setVariable("inherit", "")
    b.setVariable("scanDir", "")
    b.setVariable("scanExt", "ome.tif tif lif")
    b.setVariable("skipExplored", false)
    b.setVariable("reseed", "")
    b.setVariable("reseedAll", false)
    b.setVariable("prune", false)
    b.setVariable("allowDuplicateId", msAllowArg)
    new GroovyShell(b).evaluate(msBody, "Make_SeriesSheet_stripped.groovy")
}

def msFiles = new File(tmp, "ms_files.tsv")
def msOut = new File(tmp, "ms_series.tsv")
TSV.write([[path: "tiles.ome.tif", alias: "T", include: "true"]], msFiles, ["path", "alias", "include"])

check("a tile scan builds without complaint",  errOf { runMakeSheet(msFiles, msOut, false) }, null)
def msRows = TSV.read(msOut)
check("...into two distinct series_ids",       msRows.collect { it.series_id },
      ["T_s0000_O1_1_10x", "T_s0001_O1_1_10x"])

// Now break it the only way that is still possible: by hand.
msRows[1].series_id = msRows[0].series_id
TSV.write(msRows, msOut, sheet.columnOrder(msRows))

def msErr = errOf { runMakeSheet(msFiles, msOut, false) }
check("an edited duplicate is refused",        msErr?.contains("duplicated series_id"), true)
check("...naming the sheet to go and fix",     msErr?.contains(msOut.getAbsolutePath()), true)
// The point of the whole exercise: the file exists, holding the duplicate, so
// it can be opened and corrected.
check("...and the sheet was written anyway",   msOut.isFile(), true)
check("...still holding both rows",            TSV.read(msOut).size(), 2)
check("...and still duplicated, not silently repaired",
      TSV.read(msOut).collect { it.series_id }.unique().size(), 1)

check("allowDuplicateId finishes quietly", errOf { runMakeSheet(msFiles, msOut, true) }, null)

println ""
println "=== the columns you edit sit on the left ==="
// A sheet is read left to right by a person deciding what to run, and the
// columns that decision turns on are series_id, include and whatever metadata they
// typed -- while size_x and pixel_type are reference material. Order is
// presentation only: every reader here and in R works by column NAME, and the
// merge matches on path + series_index, so this is free to arrange.
def ordered = sheet.columnOrder([[series_id: "p", include: "true", condition: "wt",
                                  operator: "cr", alias: "A", series_index: 0,
                                  series_name: "s", path: "x", size_x: 1]])
check("series_id, then include",               ordered.take(2), ["series_id", "include"])
check("...then YOUR columns, in the order given", ordered[2..3], ["condition", "operator"])
check("...then the file's own facts",          ordered[4..7],
      ["alias", "series_index", "series_name", "path"])
check("...and the measurements last",          ordered[-1], "size_x")
check("nothing was dropped or invented",       ordered.sort(false),
      ["alias", "condition", "include", "operator", "path", "series_id",
       "series_index", "series_name", "size_x"])
// A sheet with no metadata of its own must not grow a hole where they would go.
def bare = sheet.columnOrder([[series_id: "p", include: "true", path: "x"]])
check("no extras, no gap",                     bare, ["series_id", "include", "path"])


tmp.deleteDir()
println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} series-table check(s) failed")
