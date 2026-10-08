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
    [series_id: "A", path: "one.ome.tif",     series_index: 0, include: "true",  size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
    [series_id: "B", path: "missing.ome.tif", series_index: 0, include: "true",  size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
    [series_id: "C", path: "two.ome.tif",     series_index: 0, include: "true",  size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
    [series_id: "D", path: "two.ome.tif",     series_index: 0, include: "false", size_x: 200, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"],
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
      ["series_id", "t", "path", "series_index", "status", "open_method",
       "threshold", "mask_pct", "n_nucleus",
       "n_nucleolus", "seconds", "message"])
check("A is ok",                               sum.find { it.series_id == "A" }.status, "ok")
check("A counted its nuclei",                  sum.find { it.series_id == "A" }.n_nucleus, "6")
check("B is failed",                           sum.find { it.series_id == "B" }.status, "failed")
check("B says what went wrong",                sum.find { it.series_id == "B" }.message.contains("missing.ome.tif"), true)
check("D is excluded",                         sum.find { it.series_id == "D" }.status, "excluded")
check("a failed row has no counts",            sum.find { it.series_id == "B" }.n_nucleus, "")

// The threshold and the coverage, per row. Every _config.txt carries them too;
// the columns exist so that finding the handful of rows where the threshold
// went wrong does not mean opening a thousand files. On a slide that scans
// across empty sections that is the common case, not the rare one.
def rowA = sum.find { it.series_id == "A" }
check("A records the range it thresholded at",
      rowA.threshold ==~ /\d+-\d+/, true)
check("...and the percent of pixels it selected",
      rowA.mask_pct ==~ /\d+\.\d\d/, true)
check("...which is neither nothing nor everything",
      (rowA.mask_pct as double) > 0.0d && (rowA.mask_pct as double) < 50.0d, true)
// Blank, not stale: a row that never ran must not show the previous row's
// numbers, which is exactly what a carried-over variable would do.
check("a failed row has no threshold",         sum.find { it.series_id == "B" }.threshold, "")
check("an excluded row has no coverage",       sum.find { it.series_id == "D" }.mask_pct, "")

println ""
println "=== the overview override refuses what it does not recognise ==="
// save_overview is a real parameter and a config carries it, so this dialog
// field is an OVERRIDE, not a setting: "(from config)" leaves the config alone.
// A Boolean could not express that -- it has no third state -- which is why it
// stopped being one.
check("the neutral value leaves the config alone",
      BR.overviewOverride("(from config)"), null)
check("blank means the same",                  BR.overviewOverride(""), null)
check("absent means the same",                 BR.overviewOverride(null), null)
check("yes overrides to true",                 BR.overviewOverride("yes"), true)
check("no overrides to false",                 BR.overviewOverride("no"), false)
check("surrounding space is tolerated",        BR.overviewOverride("  yes  "), true)

// THE ONE THAT MATTERS. A `#@ String` with choices={...} is NOT validated
// against those choices on the command line -- SciJava passes any string
// through. Measured: a script declaring {"(from config)","yes","no"} and run
// with pick=true receives the String "true".
//
// This switch WAS a Boolean. A caller carrying saveOverview=true from before
// the change would land in the override branch, compare "true" == "yes", and
// silently turn overviews OFF -- the opposite of what it says. Refusing is the
// only safe reading, because "true" plainly means yes to whoever wrote it and
// there is no way to honour that without guessing.
check("the OLD boolean value is refused, not read as 'no'",
      errOf { BR.overviewOverride("true") }?.contains("saveOverview must be one of"), true)
check("...and the message says why it changed",
      errOf { BR.overviewOverride("true") }?.contains("true/false switch before"), true)
check("false is refused too",                  errOf { BR.overviewOverride("false") } != null, true)
check("and anything else",                     errOf { BR.overviewOverride("maybe") } != null, true)

println ""
println "=== a bad threshold request costs ONE error, not one per row ==="
// The batch checks the threshold before the loop. Without that, "Manual with
// no range" would open a stack, blur it, and fail -- for every included row.
// The assertion is therefore not that it threw, but that it threw having
// written NOTHING: no summary, no per-image output, no directory content.
def outBad = new File(tmp, "out_badthresh")
String badErr = errOf {
    runner.run(rows, raw, params + [nucleus_threshold: "Manual", nucleus_threshold_range: ""], outBad)
}
check("Manual with no range is refused",       badErr?.contains("needs nucleus_threshold_range"), true)
check("...before any row ran",                 new File(outBad, "batch_summary.tsv").exists(), false)
check("...and nothing was written at all",     (outBad.exists() ? outBad.listFiles().size() : 0), 0)

def outBad2 = new File(tmp, "out_badmethod")
check("an unknown method is refused too",
      errOf { runner.run(rows, raw, params + [nucleus_threshold: "Banana"], outBad2) }
          ?.contains("unknown threshold method"), true)
check("...also before any row ran",            (outBad2.exists() ? outBad2.listFiles().size() : 0), 0)

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
    [series_id: "P", path: "one.ome.tif",    series_index: 0, include: "true", pixel_width: "0.25"],
    [series_id: "Q", path: "coarse.ome.tif", series_index: 0, include: "true", pixel_width: "0.5"],
]
check("two sizes are seen",                    BR.pixelSizes(mixed).keySet().sort(), ["0.25", "0.5"])
def res2 = runner.run(mixed, raw, params, new File(tmp, "out2"))
check("...and warned about",                   res2.warnings.any { it.contains("different pixel sizes") }, true)
check("the warning names the risk",            res2.warnings.any { it.contains("PIXELS") }, true)
check("both still ran",                        res2.ok, 2)
// One pixel size must NOT warn, or the warning means nothing.
def same = [[series_id: "P", path: "one.ome.tif", series_index: 0, include: "true", pixel_width: "0.25"],
            [series_id: "Q", path: "two.ome.tif", series_index: 0, include: "true", pixel_width: "0.25"]]
def res3 = runner.run(same, raw, params, new File(tmp, "out3"))
check("one pixel size is quiet",               res3.warnings.size(), 0)

println ""
println "=== a stale sheet is reported ==="
def stale = [[series_id: "S", path: "one.ome.tif", series_index: 0, include: "true",
              size_x: 999, size_y: 200, size_z: 3, size_c: 1, pixel_width: "0.25"]]
def res4 = runner.run(stale, raw, params, new File(tmp, "out4"))
check("the mismatch is warned",                res4.warnings.any { it.contains("does not match the image") }, true)
check("...naming the column",                  res4.warnings.any { it.contains("size_x") }, true)
// A warning, not a refusal: the image is what it is, and stopping would lose
// the run over a stale number that may not matter.
check("it still ran",                          res4.ok, 1)
def agree = [[series_id: "T", path: "one.ome.tif", series_index: 0, include: "true",
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
def guarded = [[series_id: "G", path: "one.ome.tif", series_index: 0, include: "true"]]
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
println "=== the sheet's series_id names the output, not the image ==="
// An id from the title would be "Series001" for both files, which is exactly
// the collision the sheet exists to prevent.
def two = [[series_id: "first_one",  path: "one.ome.tif", series_index: 0, include: "true"],
           [series_id: "second_two", path: "two.ome.tif", series_index: 0, include: "true"]]
def out6 = new File(tmp, "out6")
runner.run(two, raw, params, out6)
check("first output named from the sheet",     new File(out6, "first_one_nucleus_outline.txt").isFile(), true)
check("second output named from the sheet",    new File(out6, "second_two_nucleus_outline.txt").isFile(), true)
check("no Series001 collision on disk",        new File(out6, "Series001_nucleus_outline.txt").exists(), false)

println ""
println "=== a hand-edited id that cannot name a file fails its row only ==="
// series_id is the operator's to edit. One with a space would name files, and
// be written into the tab-separated outline table, as given -- so it is
// refused per row, before the image opens, and the other rows still run.
def edited = [[series_id: "my embryo",  path: "one.ome.tif", series_index: 0, include: "true"],
              [series_id: "fine_one",   path: "two.ome.tif", series_index: 0, include: "true"]]
def outEd = new File(tmp, "outEd")
def resEd = runner.run(edited, raw, params, outEd)
check("one row failed, one ran",               [resEd.failed, resEd.ok], [1, 1])
def edRow = resEd.summary.find { it.series_id == "my embryo" }
check("...the failure names the clean form",   edRow?.message?.toString()?.contains("'my_embryo'"), true)
check("...and nothing was written for it",
      outEd.list().findAll { it.startsWith("my") }.toList(), [])
check("the good row's output exists",          new File(outEd, "fine_one_nucleus_outline.txt").isFile(), true)

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
    [series_id: "E0", path: "multi.ome.tif", series_index: 0, include: "true"],
    [series_id: "E1", path: "multi.ome.tif", series_index: 1, include: "true"],
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

println ""
println "=== the analysis step refuses what the sheet step allowed ==="
// Make_SeriesSheet writes a sheet with duplicate series_ids on purpose, so they
// can be opened and fixed. Here they must be fatal BEFORE anything runs: the
// series_id names the output files, so two rows sharing one overwrite each other
// on disk and merge into a single sample in R.
def clash = [[series_id: "same", path: "one.ome.tif", series_index: 0, include: "true"],
             [series_id: "same", path: "two.ome.tif", series_index: 0, include: "true"]]
def clashErr = errOf { runner.run(clash, raw, params, new File(tmp, "out_clash")) }
check("a duplicate series_id stops the batch", clashErr?.contains("share a series_id"), true)
check("...naming both rows",                   clashErr?.contains("one.ome.tif[0]") && clashErr?.contains("two.ome.tif[0]"), true)
check("...before anything was written",        new File(new File(tmp, "out_clash"), "same_nucleus_outline.txt").exists(), false)
// An excluded duplicate writes nothing, so it is not a duplicate that matters.
def clashOff = [[series_id: "same", path: "one.ome.tif", series_index: 0, include: "true"],
                [series_id: "same", path: "two.ome.tif", series_index: 0, include: "false"]]
check("an EXCLUDED duplicate is not a clash",  errOf { runner.run(clashOff, raw, params, new File(tmp, "out_clashoff")) }, null)

// A sheet from before v0.7.0 has `prefix` where `series_id` now is. Without an
// up-front check every id reads blank, and the duplicate check above reports
// that two rows "share" one -- loud, about the wrong thing. The check has to
// fire FIRST, so this sheet would also trip the duplicate check if it got there.
def oldSheet = [[prefix: "A", path: "one.ome.tif", series_index: 0, include: "true"],
                [prefix: "B", path: "two.ome.tif", series_index: 0, include: "true"]]
def oldErr = errOf { runner.run(oldSheet, raw, params, new File(tmp, "out_old")) }
check("an old sheet is named as one",          oldErr?.contains("written before v0.7.0"), true)
check("...not reported as a shared id",        oldErr?.contains("share a series_id"), false)
check("...and nothing ran",                    new File(new File(tmp, "out_old"), "batch_summary.tsv").exists(), false)
// One included row reaches no duplicate check at all, so this is what stops it.
check("a one-row old sheet is refused too",
      errOf { runner.run(oldSheet.take(1), raw, params, new File(tmp, "out_old1")) }?.contains("written before v0.7.0"), true)

println ""
println "=== a results folder says which series it came from ==="
// Identity comes from content, not from the filename: the series_id should not
// have to be parsed back apart to answer "which series of which file was this?"
def provRows = [[series_id: "PV", path: "multi.ome.tif", series_index: 1, include: "true",
                 series_name: "S1"]]
def outProv = new File(tmp, "out_prov")
runner.run(provRows, raw, params, outProv)
def prov = RC.parse(new File(outProv, "PV_config.txt").getText("UTF-8"))
check("source_file recorded",                  prov.source_file, "multi.ome.tif")
check("series_index recorded",                 prov.series_index, "1")
check("series_name recorded",                  prov.series_name, "S1")
// ...and a config carrying them still feeds back in as a run's parameters.
check("they are provenance, not parameters",
      errOf { RC.readParams(new File(outProv, "PV_config.txt"), NP.PARAM_TYPES) }, null)


println ""
println "=== runEach: the shared loop, with the caller's own work ==="

// run() is now one caller of runEach(); Run_Overview_Batch.groovy is another.
// What is asserted here is the part they share -- include, the duplicate
// refusal, opening, closing, one failure not costing the rest, and a summary
// that is RECTANGULAR whatever happened to a row.
def outEach = new File(tmp, "each"); outEach.mkdirs()
def seen = []
def eachRes = runner.runEach(rows, raw, [open_mode: "auto"], outEach,
                             ["n_slices", "title"], null,
                             { src, prefix, si, openMethod, row ->
                                 seen << prefix
                                 return [n_slices: src.nSlices, title: src.title]
                             })
// A, B and C are included; B points at a file that is not there. So the work
// closure must see exactly the rows that OPENED -- naming them, because a
// count alone would pass if it ran the wrong two.
check("work sees only the rows that opened",  seen.sort(), ["A", "C"])
check("...counted as ok",                     eachRes.ok, 2)
check("...and the unopenable row is contained", eachRes.failed, 1)

def eachTsv = new File(outEach, "batch_summary.tsv")
check("runEach writes batch_summary.tsv",     eachTsv.isFile(), true)
def eachHdr = eachTsv.readLines()[0].split("\t").toList()
check("...with the caller's columns, in place",
      eachHdr, ["series_id", "t", "path", "series_index", "status", "open_method",
                "n_slices", "title", "seconds", "message"])

// THE point of `blanks`: an excluded or failed row has no work output, and a
// ragged table would make every reader of it wrong about which column is which.
def eachRows = TSV.read(eachTsv)
def widths = eachRows.collect { it.keySet().size() }.toSet()
check("every row has every column",           widths.size(), 1)
def exRow = eachRows.find { it.status == "excluded" }
check("an excluded row has blank work cells",
      (exRow == null) ? "no excluded row in fixture" : [exRow.n_slices, exRow.title],
      (exRow == null) ? "no excluded row in fixture" : ["", ""])

// A throwing row must be contained, not fatal -- and must still be countable.
def outThrow = new File(tmp, "eachthrow"); outThrow.mkdirs()
int called = 0
def throwRes = runner.runEach(rows, raw, [open_mode: "auto"], outThrow,
                              ["n_slices"], null,
                              { src, prefix, si, openMethod, row ->
                                  called++
                                  throw new IllegalStateException("deliberate")
                              })
check("every opened row still reached the work", called, 2)
check("...all three included rows failed",      throwRes.failed, 3)
check("...and none is counted ok",              throwRes.ok, 0)
def throwRows = TSV.read(new File(outThrow, "batch_summary.tsv"))
// Each failure keeps ITS OWN reason. Collapsing them to one message would hide
// that B failed for a different cause than A and C, which is the whole value
// of the message column on a long batch.
check("...each failure keeps its own reason",
      throwRows.findAll { it.status == "failed" }
               .collectEntries { [(it.series_id): it.message.contains("deliberate")] },
      [A: true, B: false, C: true])
check("...and B's reason is the missing file",
      throwRows.find { it.series_id == "B" }.message.contains("no such image file"), true)

// The pixel-size warning is now the CALLER's sentence, because whether a mixed
// batch matters depends on what the rows do. Without this the overview batch
// would warn about a blur sigma it never uses.
def outNote = new File(tmp, "eachnote"); outNote.mkdirs()
def noteRes = runner.runEach(mixed, raw, [open_mode: "auto"], outNote, [], null,
                             { src, prefix, si, openMethod, row -> [:] })
def mixedWarn = noteRes.warnings.find { it.contains("different pixel sizes") }
check("mixed pixel sizes still warn",         mixedWarn != null, true)
check("...with the generic note by default",
      mixedWarn == null ? "no warning" : mixedWarn.contains("nucleus_blur_sigma"), false)
// ...and run() supplies the nucleus one, which now names ALL three pixel settings.
def nucWarn = res2.warnings.find { it.contains("different pixel sizes") }
check("run() still gives the nucleus note",
      nucWarn == null ? "no warning" : nucWarn.contains("nucleus_blur_sigma"), true)
check("...naming nucleolus_erode_px too",
      nucWarn == null ? "no warning" : nucWarn.contains("nucleolus_erode_px"), true)

println ""
println "=== run_tag: several batches sharing one output directory ==="
// The SLURM array's tasks all write into one outdir. Each series is in one
// task, so a series' files cannot collide; the batch's own two files would,
// and a summary overwritten by another task reads as rows that never ran.
check("blank names the files as always",      BR.runFileName("batch_summary", ".tsv", ""), "batch_summary.tsv")
check("...and so does null",                  BR.runFileName("batch_summary", ".tsv", null), "batch_summary.tsv")
check("a tag goes before the extension",      BR.runFileName("batch_params", ".txt", "task_03"), "batch_params_task_03.txt")
check("a tag with a path separator is refused",
      errOf { BR.runFileName("batch_summary", ".tsv", "../x") }?.contains("runTag may hold only"), true)

def outShared = new File(tmp, "shared")
def tagWork = { src, sid, si, openMethod, row -> [n_slices: src.nSlices] }
def tagA = runner.runEach(rows.findAll { it.series_id == "A" }, raw, [open_mode: "auto", run_tag: "task_01"],
                          outShared, ["n_slices"], null, tagWork)
def tagC = runner.runEach(rows.findAll { it.series_id == "C" }, raw, [open_mode: "auto", run_tag: "task_02"],
                          outShared, ["n_slices"], null, tagWork)
// Each summary holds ITS task's row, named -- two files existing would pass
// if the second had been written over the first under both names.
check("task_01's summary holds A only",
      TSV.read(new File(outShared, "batch_summary_task_01.tsv")).collect { it.series_id }, ["A"])
check("task_02's summary holds C only",
      TSV.read(new File(outShared, "batch_summary_task_02.tsv")).collect { it.series_id }, ["C"])
check("...no untagged summary beside them",   new File(outShared, "batch_summary.tsv").exists(), false)
check("runEach returns the file it wrote",    tagC.summary_file.getName(), "batch_summary_task_02.tsv")

// A bad tag costs one message, before any row is opened.
int tagCalls = 0
def outBadTag = new File(tmp, "badtag")
check("a bad tag is refused",
      errOf { runner.runEach(rows, raw, [open_mode: "auto", run_tag: "a b"], outBadTag, [], null,
                             { src, sid, si, m, row -> tagCalls++; [:] }) }?.contains("runTag may hold only"), true)
check("...before any row ran",                tagCalls, 0)
check("...or the outdir was made",            outBadTag.exists(), false)

// run() names batch_params.txt by the same tag.
def outTagRun = new File(tmp, "tagrun")
runner.run(rows.findAll { it.series_id == "A" }, raw, params + [run_tag: "task_07"], outTagRun)
check("run() writes batch_params_<tag>.txt",  new File(outTagRun, "batch_params_task_07.txt").isFile(), true)
check("...and batch_summary_<tag>.tsv",      new File(outTagRun, "batch_summary_task_07.tsv").isFile(), true)
check("...and neither untagged name",
      [new File(outTagRun, "batch_params.txt").exists(), new File(outTagRun, "batch_summary.tsv").exists()],
      [false, false])
// run_tag names a file; it is not an analysis parameter, so it must not be
// written into a file meant to be fed back in as a config.
check("run_tag is not in batch_params",
      new File(outTagRun, "batch_params_task_07.txt").text.contains("run_tag"), false)
check("...nor in the series' own _config.txt",
      errOf { RC.readParams(new File(outTagRun, "A_config.txt"), NP.PARAM_TYPES) }, null)

tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} batch check(s) failed")
