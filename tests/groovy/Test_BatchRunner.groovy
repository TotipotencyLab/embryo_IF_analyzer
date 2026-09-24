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

// A file holding several series, with DIFFERENT content in each: a reader path
// that quietly ignored setSeries() would otherwise read series 0 every time and
// pass every other check in this file.
def writeMultiSeries = { File dest, List discsPerSeries, int nz ->
    def meta = MetadataTools.createOMEXMLMetadata()
    discsPerSeries.eachWithIndex { nd, int i ->
        MetadataTools.populateMetadata(meta, i, "S" + i, false, DimensionOrder.XYZCT.toString(),
                                       PixelType.UINT8.toString(), 200, 200, nz, 1, 1, 1)
        meta.setPixelsPhysicalSizeX(new Length(0.25d, UNITS.MICROMETER), i)
        meta.setPixelsPhysicalSizeY(new Length(0.25d, UNITS.MICROMETER), i)
        if (nz > 1) meta.setPixelsPhysicalSizeZ(new Length(1.0d, UNITS.MICROMETER), i)
    }
    def w = new OMETiffWriter()
    w.setMetadataRetrieve(meta)
    w.setId(dest.getAbsolutePath())
    discsPerSeries.eachWithIndex { nd, int i ->
        w.setSeries(i)
        def ip = new ij.process.ByteProcessor(200, 200)
        ip.setColor(255)
        [[30, 30], [120, 120], [30, 120]].take((int) nd).each { xy ->
            ip.fill(new ij.gui.OvalRoi((int) xy[0], (int) xy[1], 50, 50))
        }
        (0..<nz).each { int z -> w.saveBytes(z, (byte[]) ip.getPixels()) }
    }
    w.close()
    return dest
}
writeMultiSeries(new File(raw, "multi.ome.tif"), [2, 3], 3)
// Over AUTO_SERIES_MAX, so `auto` must stop reaching for the importer.
writeMultiSeries(new File(raw, "many.ome.tif"), (1..18).collect { 2 }, 1)

// Each plane a DIFFERENT constant, so a channel/slice transposition is visible.
// With identical planes an ordering bug reads as a pass.
def writePlanes = { File dest, int nz, int nc ->
    def meta = MetadataTools.createOMEXMLMetadata()
    MetadataTools.populateMetadata(meta, 0, "P", false, DimensionOrder.XYZCT.toString(),
                                   PixelType.UINT8.toString(), 64, 64, nz, nc, 1, 1)
    meta.setPixelsPhysicalSizeX(new Length(0.25d, UNITS.MICROMETER), 0)
    meta.setPixelsPhysicalSizeY(new Length(0.25d, UNITS.MICROMETER), 0)
    if (nz > 1) meta.setPixelsPhysicalSizeZ(new Length(1.0d, UNITS.MICROMETER), 0)
    def w = new OMETiffWriter()
    w.setMetadataRetrieve(meta)
    w.setId(dest.getAbsolutePath())
    (0..<(nz * nc)).each { int i ->
        def ip = new ij.process.ByteProcessor(64, 64)
        ip.setValue(10 * (i + 1))
        ip.fill()
        w.saveBytes(i, (byte[]) ip.getPixels())
    }
    w.close()
    return dest
}
writePlanes(new File(raw, "chan.ome.tif"), 2, 2)     // c AND z
writePlanes(new File(raw, "single.ome.tif"), 1, 1)   // neither

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
      ["prefix", "path", "series_index", "status", "open_method", "n_nucleus",
       "n_nucleolus", "seconds", "message"])
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
println "=== the batch must not redecorate the operator's Fiji ==="
// ImporterOptions is backed by ImageJ preferences and the importer saves them,
// so an unguarded setWindowless(true) leaves `.bioformats.windowless=true`
// behind -- after which dragging a .lif onto Fiji silently opens the first
// series instead of offering the chooser. It happened, to a real person, from
// one run. Same family as Set Measurements and Prefs.blackBackground.
ij.Prefs.set("bioformats.windowless", false)
def guarded = [[prefix: "G", path: "one.ome.tif", series_index: 0, include: "true"]]
runner.run(guarded, raw, params, new File(tmp, "out7"))
check("windowless is left as it was found",
      ij.Prefs.get("bioformats.windowless", false), false)
// And the other way round: a user who WANTS it on must keep it on.
ij.Prefs.set("bioformats.windowless", true)
runner.run(guarded, raw, params, new File(tmp, "out8"))
check("...and a true value is preserved too",
      ij.Prefs.get("bioformats.windowless", false), true)
ij.Prefs.set("bioformats.windowless", false)

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

println ""
println "=== which reader opened the image ==="
// The importer prepares EVERY series in a file before handing back the one
// asked for, so its cost is O(series in the file) per call -- 182 s on a
// 1563-series .lif against 1.5 s on a 15-series one, and openSeries() is called
// once per row. The reader path holds one reader open and costs 0.11 s.
check("the modes offered",                     BR.OPEN_MODES, ["auto", "importer", "reader"])
check("an explicit mode is taken literally",   runner.resolveMethod("reader", new File(raw, "one.ome.tif")), "reader")
check("...both ways",                          runner.resolveMethod("importer", new File(raw, "one.ome.tif")), "importer")
check("a typo is refused, not guessed",
      errOf { runner.resolveMethod("fast", new File(raw, "one.ome.tif")) }?.contains("open_mode must be one of"), true)
check("auto keeps the importer for a small file",
      runner.resolveMethod("auto", new File(raw, "one.ome.tif")), "importer")
check("auto switches on a many-series file",
      runner.resolveMethod("auto", new File(raw, "many.ome.tif")), "reader")
runner.closeReader()

// The importer writes "micron"; the metadata store says the mu symbol. That
// string lands in _config.txt as pixel_unit, so the two paths would otherwise
// produce output folders differing in exactly one word.
check("the mu symbol becomes ImageJ's spelling", BR.ijUnit("µm"), "micron")
check("...and so does um",                     BR.ijUnit("um"), "micron")
check("anything else is left alone",           BR.ijUnit("nm"), "nm")

println ""
println "=== plane order, labels and calibration, across image shapes ==="
// Compared directly, without the pipeline in between, and across three shapes
// because the slice label OMITS a component when that dimension has one entry:
// "c:1/2 z:1/2 - P" against "z:1/3 - S0" against "- P". Guessing that format
// wrong changes the Label column of every measurement row.
def planeReport = { File img, int idx, String method ->
    def imp = runner.openSeries(img, idx, method)
    try {
        def st = imp.getStack()
        return [dims  : [imp.getNChannels(), imp.getNSlices(), imp.getNFrames()],
                labels: (1..st.getSize()).collect { st.getSliceLabel(it) },
                means : (1..st.getSize()).collect { String.format("%.1f", st.getProcessor(it).getStatistics().mean) },
                title : imp.getTitle(),
                cal   : [imp.getCalibration().pixelWidth,
                         imp.getCalibration().pixelDepth,
                         imp.getCalibration().getUnit()]]
    } finally {
        imp.changes = false; imp.close(); imp.flush(); runner.closeReader()
    }
}
["multi.ome.tif", "chan.ome.tif", "single.ome.tif"].each { String n ->
    def f = new File(raw, n)
    def viaImp = planeReport(f, 0, "importer")
    def viaRdr = planeReport(f, 0, "reader")
    check("  " + n + " dimensions",   viaRdr.dims,   viaImp.dims)
    check("  " + n + " plane order",  viaRdr.means,  viaImp.means)
    check("  " + n + " slice labels", viaRdr.labels, viaImp.labels)
    check("  " + n + " title",        viaRdr.title,  viaImp.title)
    check("  " + n + " calibration",  viaRdr.cal,    viaImp.cal)
}

println ""
println "=== the two readers must produce the SAME output ==="
// This is why the importer is kept rather than replaced. openSeriesReader()
// reimplements by hand what the library was doing for us -- calibration, plane
// order, the window title -- and that is exactly the kind of code that drifts
// silently when Bio-Formats is next upgraded. Nothing else would catch it.
//
// multi.ome.tif holds two series with DIFFERENT contents (2 discs and 3), so a
// reader path that quietly ignored setSeries would fail here rather than pass
// every calibration check while reading series 0 twice.
def eqRows = [
    [prefix: "E0", path: "multi.ome.tif", series_index: 0, include: "true"],
    [prefix: "E1", path: "multi.ome.tif", series_index: 1, include: "true"],
]
def outImp = new File(tmp, "out_importer")
def outRdr = new File(tmp, "out_reader")
def rImp = runner.run(eqRows, raw, params + [open_mode: "importer"], outImp)
def rRdr = runner.run(eqRows, raw, params + [open_mode: "reader"],   outRdr)
check("both ran every row",                    [rImp.ok, rRdr.ok], [2, 2])

def sumImp = TSV.read(new File(outImp, "batch_summary.tsv"))
def sumRdr = TSV.read(new File(outRdr, "batch_summary.tsv"))
check("the importer said so",                  sumImp.collect { it.open_method }, ["importer", "importer"])
check("the reader said so",                    sumRdr.collect { it.open_method }, ["reader", "reader"])
// The series really were told apart -- 2 discs in one, 3 in the other.
// ROIs, not objects: 3 slices x 2 discs, and 3 x 3.
check("series 0 and 1 differ, via the importer", sumImp.collect { it.n_nucleus }, ["6", "9"])
check("...and identically via the reader",     sumRdr.collect { it.n_nucleus }, ["6", "9"])

def listing = { File d -> d.listFiles().collect { it.getName() }.sort() }
check("the same files were written",           listing(outImp), listing(outRdr))

// _config.txt legitimately differs in two lines. ROI zips carry a zip entry
// timestamp, so those are compared by what is IN them.
def configLines = { File f ->
    f.readLines().findAll { !it.startsWith("timestamp\t") && !it.startsWith("open_method\t") }
}
def zipEntries = { File f ->
    def z = new java.util.zip.ZipFile(f)
    try { return z.entries().collect { it.getName() + ":" + it.getSize() }.sort() }
    finally { z.close() }
}
def differing = []
listing(outImp).each { String n ->
    if (n == "batch_summary.tsv" || n == "batch_params.txt") return   // seconds, and the mode itself
    def a = new File(outImp, n), b = new File(outRdr, n)
    boolean agrees
    if (n.endsWith("_config.txt"))  { agrees = (configLines(a) == configLines(b)) }
    else if (n.endsWith(".zip"))    { agrees = (zipEntries(a) == zipEntries(b)) }
    else                            { agrees = java.util.Arrays.equals(a.getBytes(), b.getBytes()) }
    if (!agrees) {
        differing << n
        // "They differ" is not a useful failure. Name the first line that does.
        if (!n.endsWith(".zip")) {
            def la = a.readLines(), lb = b.readLines()
            int i = (0..<Math.min(la.size(), lb.size())).find { la[it] != lb[it] }
            println "         " + n + " first differs at line " + ((i == null) ? "(length only)" : (i + 1))
            if (i != null) {
                println "           importer: " + la[i].take(160)
                println "           reader  : " + lb[i].take(160)
            }
        }
    }
}
check("every output file agrees",              differing, [])

// Named individually, so a failure says WHICH thing drifted rather than that
// something did.
def cfgImp = RC.parse(new File(outImp, "E1_config.txt").getText("UTF-8"))
def cfgRdr = RC.parse(new File(outRdr, "E1_config.txt").getText("UTF-8"))
["image_title", "image_width", "image_height", "image_slices", "image_channels",
 "pixel_width", "pixel_height", "pixel_depth", "pixel_unit"].each { String k ->
    check("  " + k + " agrees",                cfgRdr[k], cfgImp[k])
}
check("the image is calibrated, not in pixels", cfgRdr.pixel_unit, "micron")
check("open_method is recorded",               cfgRdr.open_method, "reader")
check("...and blank when nothing opened it",   RC.PROVENANCE_KEYS.contains("open_method"), true)

tmp.deleteDir()
println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} batch check(s) failed")
