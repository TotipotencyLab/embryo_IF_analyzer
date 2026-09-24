// Test_SampleSheet.groovy
//
// files.tsv -> samples.tsv, on images this test WRITES rather than ships.
//
// The fixture is generated, not tracked: a multi-series OME-TIFF is a binary
// and the thing worth keeping under review is the code that makes it. It is
// cross-checked against the real Leica .lif by hand -- the shapes it exercises
// (several series, one of them single-plane, names that need sanitising) are
// the ones that file actually has.
//
// Run headless from the repo root:
//
//   ImageJ-macosx --headless --console --run tests/groovy/Test_SampleSheet.groovy

import loci.formats.MetadataTools
import loci.formats.out.OMETiffWriter
import ome.units.UNITS
import ome.units.quantity.Length
import ome.xml.model.enums.DimensionOrder
import ome.xml.model.enums.PixelType
import ome.xml.model.primitives.PositiveInteger

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "SampleSheet.groovy").exists()) {
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
def imgB = writeMultiSeries(new File(rawDir, "plateB.ome.tif"),
    [["Series001", 40, 40, 2], ["Series002", 40, 40, 2]],
    0.5d, 1.1d)
check("fixture A written",                     imgA.isFile(), true)
check("fixture B written",                     imgB.isFile(), true)

def SS = new GroovyClassLoader().parseClass(new File(LIBDIR, "SampleSheet.groovy"))
def TSV = new GroovyClassLoader().parseClass(new File(LIBDIR, "Tsv.groovy"))
def sheet = SS.load(LIBDIR)

println ""
println "=== the schema loads and knows its owners ==="
check("samples has a prefix column",           sheet.schema.columns("samples").contains("prefix"), true)
check("prefix is seeded, not machine",         sheet.schema.owner("samples", "prefix"), "seeded")
check("series_index is machine",               sheet.schema.owner("samples", "series_index"), "machine")
check("include is seeded",                     sheet.schema.owner("samples", "include"), "seeded")
check("path is a files column",                sheet.schema.columns("files").contains("path"), true)

println ""
println "=== scan mode ==="
def scanned = sheet.scan(rawDir, ["ome.tif", "tif", "lif"])
check("scan finds both files",                 scanned.size(), 2)
check("alias defaults to the basename",        scanned[0].alias, "plateA.ome")
check("include defaults on",                   scanned[0].include, "true")

println ""
println "=== build: one row per series, prefixes unique across files ==="
def filesTsv = new File(tmp, "files.tsv")
TSV.write([[path: "plateA.ome.tif", alias: "A", include: "true", condition: "wt"],
           [path: "plateB.ome.tif", alias: "B", include: "false", condition: "ko"]],
          filesTsv, ["path", "alias", "include", "condition"])

def fileRows = sheet.readFiles(filesTsv)
check("files.tsv reads two rows",              fileRows.size(), 2)
check("checkFiles is quiet on a clean table",  sheet.checkFiles(fileRows, rawDir), [])

def built = sheet.build(fileRows, rawDir, ["condition"])
check("5 series across the two files",         built.size(), 5)
check("prefix is alias + series",              built[0].prefix, "A_Series001")
// Same series name, different file -- unique only because of the alias.
check("the other file's Series001 differs",    built.find { it.path == "plateB.ome.tif" && it.series_index == 0 }.prefix, "B_Series001")
check("a space in the name is sanitised",      built[2].prefix, "A_Image005_Denoised")
check("no prefix collision",                   errOf { sheet.checkPrefixes(built) }, null)

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

// Unique alias, unique series names, COLLIDING composite. This is why the check
// runs on the composed string and not on the parts.
def collide = [[prefix: sheet.composePrefix("A", "B_C"),  path: "p", series_index: 0, series_name: "B_C"],
               [prefix: sheet.composePrefix("A_B", "C"),  path: "q", series_index: 0, series_name: "C"]]
check("the two compose to the same prefix",    collide[0].prefix, collide[1].prefix)
check("...and that is fatal",                  errOf { sheet.checkPrefixes(collide) }?.contains("not unique"), true)

// The other way two distinct names become one filename: sanitise() collapses
// whitespace, so "Image005 Denoised" and "Image005_Denoised" are the same file.
def sanitiseClash = [[prefix: sheet.composePrefix("A", "Image005 Denoised"), path: "p", series_index: 0, series_name: "Image005 Denoised"],
                     [prefix: sheet.composePrefix("A", "Image005_Denoised"), path: "p", series_index: 1, series_name: "Image005_Denoised"]]
check("a space and an underscore sanitise alike", sanitiseClash[0].prefix, sanitiseClash[1].prefix)
check("...and that is fatal too",              errOf { sheet.checkPrefixes(sanitiseClash) }?.contains("not unique"), true)

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
def outTsv = new File(tmp, "samples.tsv")
TSV.write(built, outTsv, sheet.columnOrder(built))
def firstPass = TSV.read(outTsv)
check("written and read back",                 firstPass.size(), 5)

// Edit it the way a person would: turn one series off, add a column of your own.
firstPass[0].include = "false"
firstPass[0].cell_type = "oocyte"
firstPass[0].prefix = "A_renamed_by_hand"
TSV.write(firstPass, outTsv, sheet.columnOrder(firstPass))

def refreshed = sheet.build(fileRows, rawDir, ["condition"])
def merged = sheet.merge(refreshed, TSV.read(outTsv), [], false)
def m0 = merged.rows.find { it.path == "plateA.ome.tif" && it.series_index.toString() == "0" }
check("nothing added on a rerun",              merged.added, 0)
check("every row updated",                     merged.updated, 5)
check("an edited include survives",            m0.include, "false")
check("a column of your own survives",         m0.cell_type, "oocyte")
// The merge key is path+series_index precisely so this can be edited.
check("an edited prefix survives",             m0.prefix, "A_renamed_by_hand")
check("machine columns still refresh",         m0.size_x.toString(), "32")

println ""
println "=== reseed is opt-in, and says what it changed ==="
def reseeded = sheet.merge(refreshed, TSV.read(outTsv), ["prefix", "include"], false)
def r0 = reseeded.rows.find { it.path == "plateA.ome.tif" && it.series_index.toString() == "0" }
check("reseed restores the generated prefix",  r0.prefix, "A_Series001")
check("reseed restores the seeded include",    r0.include, "true")
check("it reports which columns it touched",   reseeded.reseeded.keySet().sort(), ["include", "prefix"])
check("cell_type is yours and untouched",      r0.cell_type, "oocyte")

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
println "=== Tsv refuses what would corrupt a column ==="
check("a tab in a cell is refused",            errOf { TSV.cell("a\tb") }?.contains("tab"), true)
check("a newline in a cell is refused",        errOf { TSV.cell("a\nb") }?.contains("newline"), true)
check("null becomes blank",                    TSV.cell(null), "")

tmp.deleteDir()
println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} sample-sheet check(s) failed")
