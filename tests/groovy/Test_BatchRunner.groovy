// Test_BatchRunner.groovy
//
// The loop, not the analysis. What matters when there are two hundred images:
// that one failure does not cost the rest, that the summary says which ones to
// look at, that `include` is honoured as a boolean and not as "a non-empty
// string is true", and that the run reports a sheet which no longer matches its
// file.
//
// Images are synthesised, so this needs no fixture.
//
//   ImageJ-macosx --headless --console --run tests/groovy/Test_BatchRunner.groovy

import loci.formats.MetadataTools
import loci.formats.out.OMETiffWriter
import ome.units.UNITS
import ome.units.quantity.Length
import ome.xml.model.enums.DimensionOrder
import ome.xml.model.enums.PixelType

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "BatchRunner.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-56s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def errOf = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() } }

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_batch_" + System.nanoTime())
tmp.mkdirs()
def raw = new File(tmp, "raw"); raw.mkdirs()

// Two discs per plane, away from the edge so detect() keeps them.
def writeImg = { File dest, int nz, double physX ->
    def meta = MetadataTools.createOMEXMLMetadata()
    MetadataTools.populateMetadata(meta, 0, "Series001", false, DimensionOrder.XYZCT.toString(),
                                   PixelType.UINT8.toString(), 200, 200, nz, 1, 1, 1)
    meta.setPixelsPhysicalSizeX(new Length(physX, UNITS.MICROMETER), 0)
    meta.setPixelsPhysicalSizeY(new Length(physX, UNITS.MICROMETER), 0)
    if (nz > 1) meta.setPixelsPhysicalSizeZ(new Length(1.0d, UNITS.MICROMETER), 0)
    def ip = new ij.process.ByteProcessor(200, 200)
    ip.setColor(255)
    ip.fill(new ij.gui.OvalRoi(30, 30, 50, 50))
    ip.fill(new ij.gui.OvalRoi(120, 120, 50, 50))
    def w = new OMETiffWriter()
    w.setMetadataRetrieve(meta)
    w.setId(dest.getAbsolutePath())
    (0..<nz).each { int z -> w.saveBytes(z, (byte[]) ip.getPixels()) }
    w.close()
    return dest
}
writeImg(new File(raw, "one.ome.tif"), 3, 0.25d)
writeImg(new File(raw, "two.ome.tif"), 3, 0.25d)
writeImg(new File(raw, "coarse.ome.tif"), 3, 0.50d)

def BR = new GroovyClassLoader().parseClass(new File(LIBDIR, "BatchRunner.groovy"))
def TSV = new GroovyClassLoader().parseClass(new File(LIBDIR, "Tsv.groovy"))
def NP = new GroovyClassLoader().parseClass(new File(LIBDIR, "NucleusPipeline.groovy"))
def RC = new GroovyClassLoader().parseClass(new File(LIBDIR, "RunConfig.groovy"))
def runner = BR.load(LIBDIR)

def params = NP.fromConfig([
    nucleus_blur_sigma    : 0.0d,
    nucleus_threshold     : "Otsu",
    nucleus_particle_size : "50-Infinity",   // um^2: the images are calibrated
    nucleoli_enabled      : false,
    channels_measured     : "1",
    save_overview         : false,
])
params.script_name = "Test_BatchRunner.groovy"

println "=== include is a boolean, never 'a non-empty string is true' ==="
check("true",                                  BR.isIncluded("true"), true)
check("false",                                 BR.isIncluded("false"), false)
check("FALSE, any case",                       BR.isIncluded("FALSE"), false)
check("no",                                    BR.isIncluded("no"), false)
check("0",                                     BR.isIncluded("0"), false)
check("blank means included",                  BR.isIncluded(""), true)
check("absent column means included",          BR.isIncluded(null), true)
// "maybe" must not quietly mean one or the other.
check("an unrecognised word is refused",       errOf { BR.isIncluded("maybe") }?.contains("true/false"), true)

println ""
println "=== one failure must not cost the rest ==="
def rows = [
    [prefix: "A", path: "one.ome.tif",     series_index: 0, include: "true",  size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
    [prefix: "B", path: "missing.ome.tif", series_index: 0, include: "true",  size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
    [prefix: "C", path: "two.ome.tif",     series_index: 0, include: "true",  size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
    [prefix: "D", path: "two.ome.tif",     series_index: 0, include: "false", size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
]
def out1 = new File(tmp, "out1")
def res = runner.run(rows, raw, params, out1)
check("the good rows ran",                     res.ok, 2)
check("the bad row failed alone",              res.failed, 1)
check("the excluded row was not run",          res.excluded, 1)
// The row AFTER the failure is the one that proves the loop continued.
check("C produced its outline",                new File(out1, "C_nucleus_outline.txt").isFile(), true)
check("B produced nothing",                    new File(out1, "B_nucleus_outline.txt").exists(), false)
check("D was skipped entirely",                new File(out1, "D_nucleus_outline.txt").exists(), false)

println ""
println "=== batch_summary.tsv is the deliverable ==="
def sum = TSV.read(new File(out1, "batch_summary.tsv"))
check("one row per sheet row, excluded too",   sum.size(), 4)
check("columns",                               sum[0].keySet().toList(),
      ["prefix", "path", "series_index", "status", "n_nucleus", "n_nucleolus", "seconds", "message"])
check("A is ok",                               sum.find { it.prefix == "A" }.status, "ok")
check("A counted its nuclei",                  sum.find { it.prefix == "A" }.n_nucleus, "6")
check("B is failed",                           sum.find { it.prefix == "B" }.status, "failed")
check("B says what went wrong",                sum.find { it.prefix == "B" }.message.contains("missing.ome.tif"), true)
check("D is excluded",                         sum.find { it.prefix == "D" }.status, "excluded")
check("a failed row has no counts",            sum.find { it.prefix == "B" }.n_nucleus, "")

println ""
println "=== the parameters used are written back, re-readable ==="
def pf = new File(out1, "batch_params.txt")
check("batch_params.txt written",              pf.isFile(), true)
def reread = RC.readParams(pf, NP.PARAM_TYPES)
check("it round trips",                        reread.nucleus_particle_size, "50-Infinity")
check("booleans survive the round trip",       reread.nucleoli_enabled, false)
// script_name identifies the caller and is not a config parameter, so it must
// not appear in a file meant to be fed back in.
check("script_name is not written",            pf.getText("UTF-8").contains("script_name"), false)

println ""
println "=== mixed pixel sizes are reported ==="
def mixed = [
    [prefix: "P", path: "one.ome.tif",    series_index: 0, include: "true", pixel_width: "0.25"],
    [prefix: "Q", path: "coarse.ome.tif", series_index: 0, include: "true", pixel_width: "0.5"],
]
check("two sizes are seen",                    BR.pixelSizes(mixed).keySet().sort(), ["0.25", "0.5"])
def res2 = runner.run(mixed, raw, params, new File(tmp, "out2"))
check("...and warned about",                   res2.warnings.any { it.contains("different pixel sizes") }, true)
check("the warning names the risk",            res2.warnings.any { it.contains("PIXELS") }, true)
check("both still ran",                        res2.ok, 2)
// One pixel size must NOT warn, or the warning means nothing.
def same = [[prefix: "P", path: "one.ome.tif", series_index: 0, include: "true", pixel_width: "0.25"],
            [prefix: "Q", path: "two.ome.tif", series_index: 0, include: "true", pixel_width: "0.25"]]
def res3 = runner.run(same, raw, params, new File(tmp, "out3"))
check("one pixel size is quiet",               res3.warnings.size(), 0)

println ""
println "=== a stale sheet is reported ==="
def stale = [[prefix: "S", path: "one.ome.tif", series_index: 0, include: "true",
              size_x: 999, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"]]
def res4 = runner.run(stale, raw, params, new File(tmp, "out4"))
check("the mismatch is warned",                res4.warnings.any { it.contains("does not match the image") }, true)
check("...naming the column",                  res4.warnings.any { it.contains("size_x") }, true)
// A warning, not a refusal: the image is what it is, and stopping would lose
// the run over a stale number that may not matter.
check("it still ran",                          res4.ok, 1)
def agree = [[prefix: "T", path: "one.ome.tif", series_index: 0, include: "true",
              size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"]]
check("a matching sheet is quiet",             runner.run(agree, raw, params, new File(tmp, "out5")).warnings.size(), 0)

println ""
println "=== the sheet's prefix names the output, not the image ==="
// resolveImageId() would dig "Series001" out of both files, which is exactly
// the collision the sheet exists to prevent.
def two = [[prefix: "first_one",  path: "one.ome.tif", series_index: 0, include: "true"],
           [prefix: "second_two", path: "two.ome.tif", series_index: 0, include: "true"]]
def out6 = new File(tmp, "out6")
runner.run(two, raw, params, out6)
check("first output named from the sheet",     new File(out6, "first_one_nucleus_outline.txt").isFile(), true)
check("second output named from the sheet",    new File(out6, "second_two_nucleus_outline.txt").isFile(), true)
check("no Series001 collision on disk",        new File(out6, "Series001_nucleus_outline.txt").exists(), false)

tmp.deleteDir()
println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} batch check(s) failed")
