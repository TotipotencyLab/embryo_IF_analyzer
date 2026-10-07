// Test_FeatureOverlay.groovy
//
// The overview TIFF and its feature overlay (analysis-oo_count-physical_blur):
// Overview.composite / saveTiff, FeatureOverlay, and the Run_Overview_Batch
// front end that uses them.
//
// Synthesised images, so no fixture. Built so the obvious wrong implementation
// fails: the pixels are ANISOTROPIC (0.5 x 0.4 um) and the image is not square,
// so swapping x and y -- in the calibration or in the frame -- moves every
// vertex; the two channels have different contrast windows, so one window
// applied to both is caught; and the PNG-only run is compared byte for byte
// with a run that also writes the TIFF, so "the PNGs are unchanged" is a
// measured fact rather than an intention.
//
// Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_FeatureOverlay.groovy

import ij.CompositeImage
import ij.IJ
import ij.ImagePlus
import ij.ImageStack
import ij.gui.OvalRoi
import ij.gui.PolygonRoi
import ij.gui.Roi
import ij.process.ByteProcessor
import java.awt.Color
import loci.formats.MetadataTools
import loci.formats.out.OMETiffWriter
import ome.units.UNITS
import ome.units.quantity.Length
import ome.xml.model.enums.DimensionOrder
import ome.xml.model.enums.PixelType

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "FeatureOverlay.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy/FeatureOverlay.groovy at " + LIBDIR)
}

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-62s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def errOf = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() ?: t.getClass().getSimpleName() } }

def gcl = new GroovyClassLoader(this.class.classLoader)
def OV = gcl.parseClass(new File(LIBDIR, "Overview.groovy"))
def FO = gcl.parseClass(new File(LIBDIR, "FeatureOverlay.groovy"))
def RX = gcl.parseClass(new File(LIBDIR, "RoiExport.groovy"))

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_featureoverlay_" + System.nanoTime())
tmp.mkdirs()

// 2 channels x 3 slices, 200 wide x 160 high, 0.5 x 0.4 um.
// ch1: a disc 200 on 50, on slice 2 only.  ch2: a horizontal ramp 0..199 on
// every slice -- a different histogram, so a different auto window.
int W = 200, H = 160
double PW = 0.5d, PH = 0.4d
def planes = { int ch, int z ->
    def ip = new ByteProcessor(W, H)
    if (ch == 1) {
        ip.setValue(50); ip.fill()
        if (z == 2) { ip.setValue(200); ip.fill(new OvalRoi(80, 60, 40, 40)) }
    } else {
        for (int x = 0; x < W; x++) for (int y = 0; y < H; y++) ip.set(x, y, x)
    }
    return ip
}
def makeImp = {
    def st = new ImageStack(W, H)
    (1..3).each { int z -> (1..2).each { int ch -> st.addSlice(planes(ch, z)) } }
    def imp = new ImagePlus("synthetic", st)
    imp.setDimensions(2, 3, 1)
    def cal = imp.getCalibration()
    cal.pixelWidth = PW; cal.pixelHeight = PH; cal.pixelDepth = 2.0d; cal.setUnit("micron")
    return imp
}
def planesEqual = { ImagePlus a, ImagePlus b ->
    a.getStackSize() == b.getStackSize() &&
        (1..a.getStackSize()).every { int i ->
            Arrays.equals((byte[]) a.getStack().getProcessor(i).getPixels(), (byte[]) b.getStack().getProcessor(i).getPixels())
        }
}

println "=== composite(): one image, every channel, pixels untouched ==="
def imp = makeImp()
def proj = OV.project(imp, null, "max", null)
def blueGreen = OV.channelColours("blue,green")
def comp = OV.composite(proj, [contrast: "auto", saturated: 0.35d, colours: blueGreen])
ImagePlus ci = comp.image
check("2 channels -> a CompositeImage",                 ci instanceof CompositeImage, true)
check("channels x slices x frames",                     [ci.getNChannels(), ci.getNSlices(), ci.getNFrames()], [2, 1, 1])
check("pixels are the projection's, value for value",   planesEqual(ci, proj), true)
check("ch1 is the max over z (the slice-2 disc reaches 200)", ci.getStack().getProcessor(1).get(100, 80), 200)
check("calibration kept, x and y apart",                [ci.getCalibration().pixelWidth, ci.getCalibration().pixelHeight], [PW, PH])
check("ch1 coloured blue",                              new Color(((CompositeImage) ci).getChannelLut(1).getRGB(255)), Color.BLUE)
check("ch2 coloured green",                             new Color(((CompositeImage) ci).getChannelLut(2).getRGB(255)), Color.GREEN)
def pngWin = [1, 2].collect { int c -> def v = OV.prepare(proj, c, [contrast: "auto", saturated: 0.35d]); [v.lo, v.hi] }
def tifWin = comp.ranges.collect { [it[0], it[1]] }
check("each channel's window = the PNG's window for it",  tifWin, pngWin)
check("...and the two windows differ (so one-for-both would fail)", tifWin[0] != tifWin[1], true)
check("the windows are on the LUTs",
      [1, 2].collect { [((CompositeImage) ci).getChannelLut(it).min, ((CompositeImage) ci).getChannelLut(it).max] }, tifWin)
check("default colours: red, green",
      [1, 2].collect { new Color(((CompositeImage) OV.composite(proj, [:]).image).getChannelLut(it).getRGB(255)) },
      [Color.RED, Color.GREEN])
check("one colour for two channels is refused",
      errOf { OV.composite(proj, [colours: OV.channelColours("blue")]) }?.contains("give one per channel"), true)
check("an unknown colour name is refused before any image",
      errOf { OV.channelColours("blue,grene") }?.contains("unknown colour 'grene'"), true)
def proj2 = OV.project(imp, null, "max", [2])
def one = OV.composite(proj2, [contrast: "auto", saturated: 0.35d, colours: OV.channelColours("green")])
check("1 channel -> a plain image, window set",
      [one.image instanceof CompositeImage, one.image.getDisplayRangeMin(), one.image.getDisplayRangeMax()],
      [false, one.ranges[0][0], one.ranges[0][1]])

println ""
println "=== footprints: the outline table's um, back to this image's pixels ==="
// An ROI with integer vertices, exported the way the nucleus batch exports it,
// then turned into a one-ROI footprint (its own union) as R would write it.
def poly = new PolygonRoi([20, 150, 150, 90, 20] as int[], [30, 30, 120, 140, 110] as int[], 5, Roi.POLYGON)
def outlineFile = new File(tmp, "S1_nucleus_outline.txt")
RX.saveOutlineCoords(imp, [poly], ["nucleus_0002-0001-0085"], [2], "S1", outlineFile.getAbsolutePath())
def outl = outlineFile.readLines().drop(1).collect { it.split("\t", -1) }
def fpRows = ["name\tfeature_id\tpart\tring\tx\ty"]
outl.each { fpRows << ["S1", "nucleus_1", "1", "0", it[3], it[4]].join("\t") }
// nucleus_2: two parts, the first with a hole.
[[1, 0, [[10, 10], [40, 10], [40, 40], [10, 40]]],
 [1, 1, [[20, 20], [30, 20], [30, 30], [20, 30]]],
 [2, 0, [[160, 10], [190, 10], [190, 20]]]].each { def p ->
    p[2].each { def v -> fpRows << ["S1", "nucleus_2", p[0], p[1], v[0] * PW, v[1] * PH].join("\t") } }
[["invalid_nucleus_3", 100], ["invalid_contrast_nucleus_4", 120], ["failed_nucleus_overlap", 140]].each { def p ->
    [[p[1], 140], [p[1] + 8, 140], [p[1] + 8, 150]].each { def v -> fpRows << ["S1", p[0], 1, 0, v[0] * PW, v[1] * PH].join("\t") } }
def fpDir = new File(tmp, "fp"); fpDir.mkdirs()
def fpFile = new File(fpDir, "S1_nucleus_footprint.txt")
fpFile.setText(fpRows.join("\n") + "\n", "UTF-8")

def rois = FO.readFootprints(fpFile, "S1", imp.getCalibration(), W, H)
check("one polygon per (feature, part, ring)",          rois.size(), 7)
def r1 = rois.find { it.getName() == "nucleus_1" }
def fp1 = r1.getFloatPolygon()
def back = (0..<fp1.npoints).collect { [Math.round(fp1.xpoints[it]) as int, Math.round(fp1.ypoints[it]) as int] }
def orig = (0..<poly.getPolygon().npoints).collect { [poly.getPolygon().xpoints[it], poly.getPolygon().ypoints[it]] }
check("ROI -> outline.txt -> footprint -> polygon: same vertices", back, orig)
double maxErr = (0..<fp1.npoints).collect { Math.max(Math.abs(fp1.xpoints[it] - back[it][0]), Math.abs(fp1.ypoints[it] - back[it][1])) }.max()
check("...to within the outline's 3 decimals (< 0.01 px)", maxErr < 0.01d, true)
println "    (largest deviation " + String.format("%.5f", maxErr) + " px)"
check("nucleus_2: three rings, parts and rings as properties",
      rois.findAll { it.getName() == "nucleus_2" }.collect { [it.getProperty("part"), it.getProperty("ring")] },
      [["1", "0"], ["1", "1"], ["2", "0"]])
check("status from the id",
      rois.collectEntries { [(it.getName()): it.getProperty("status")] },
      [nucleus_1: "counted", nucleus_2: "counted", invalid_nucleus_3: "invalid",
       invalid_contrast_nucleus_4: "invalid", failed_nucleus_overlap: "failed"])

def bad = { String name, String text -> def f = new File(fpDir, name); f.setText(text, "UTF-8"); f }
check("a table for another series is refused",
      errOf { FO.readFootprints(fpFile, "S2", imp.getCalibration(), W, H) }?.contains("is for 'S1', not 'S2'"), true)
// The same footprints on an image half the size: vertices fall outside it.
check("footprints that do not fit the image are refused",
      errOf { FO.readFootprints(fpFile, "S1", imp.getCalibration(), 100, 80) }?.contains("not from this series' segmentation"), true)
check("a table without the footprint columns is refused",
      errOf { FO.readFootprints(bad("x1.txt", "name\troi\tz\tx\ty\n"), "S1", imp.getCalibration(), W, H) }?.contains("not a footprint table"), true)
check("a ring of two vertices is refused",
      errOf { FO.readFootprints(bad("x2.txt", "name\tfeature_id\tpart\tring\tx\ty\nS1\tnucleus_1\t1\t0\t1\t1\nS1\tnucleus_1\t1\t0\t2\t2\n"), "S1", imp.getCalibration(), W, H) }?.contains("has 2 vertices"), true)
check("a header-only table is a series with no features",
      FO.readFootprints(bad("x3.txt", "name\tfeature_id\tpart\tring\tx\ty\n"), "S1", imp.getCalibration(), W, H).size(), 0)
check("a missing table says which step makes it",
      errOf { FO.readFootprints(new File(fpDir, "nope.txt"), "S1", imp.getCalibration(), W, H) }?.contains("feature_footprint_cli.r"), true)

println ""
println "=== draw(): which statuses, in which colour ==="
def colours = [counted: Color.YELLOW, invalid: Color.CYAN, failed: Color.MAGENTA]
def drawOn = { String mode -> def im = new ImagePlus("d", new ByteProcessor(W, H)); [FO.draw(im, rois, FO.statusesDrawn(mode), colours), im] }
def (nNone, imNone) = drawOn("none")
check("none: counted only",                             nNone, [counted: 4, invalid: 0, failed: 0])
check("...and nothing else is on the overlay",          imNone.getOverlay().size(), 4)
check("invalid: + invalid_* (contrast-dropped included)", drawOn("invalid")[0], [counted: 4, invalid: 2, failed: 0])
def (nAll, imAll) = drawOn("all")
check("all: + failed_*",                                nAll, [counted: 4, invalid: 2, failed: 1])
def ov = imAll.getOverlay()
check("each outline named by its feature id",
      (0..<ov.size()).collect { ov.get(it).getName() }.unique(),
      ["nucleus_1", "nucleus_2", "invalid_nucleus_3", "invalid_contrast_nucleus_4", "failed_nucleus_overlap"])
check("coloured by status",
      (0..<ov.size()).collect { ov.get(it) }.collectEntries { [(it.getProperty("status")): it.getStrokeColor()] },
      colours)
check("stroke width 0 (one screen pixel at any zoom)",   (0..<ov.size()).collect { ov.get(it).getStrokeWidth() }.unique(), [0.0f])
check("the caller's ROIs are not modified",             rois[0].getStrokeColor() == Color.YELLOW, false)
check("an unknown drawRejected is refused",             errOf { FO.statusesDrawn("rejected") }?.contains("drawRejected must be one of"), true)

println ""
println "=== saveTiff(): the overlay and the windows survive the file ==="
FO.draw(ci, rois, FO.statusesDrawn("invalid"), colours)
def tifFile = OV.saveTiff(ci, new File(tmp, "S1_overview.tif").getAbsolutePath())
def re = IJ.openImage(tifFile.getAbsolutePath())
check("reopens as a 2-channel composite",               [re instanceof CompositeImage, re.getNChannels()], [true, 2])
check("pixels as written",                              planesEqual(re, proj), true)
check("calibration as written",                         [re.getCalibration().pixelWidth, re.getCalibration().pixelHeight, re.getCalibration().getUnit()],
      [PW, PH, "micron"])
check("each channel's window as written",
      [1, 2].collect { [((CompositeImage) re).getChannelLut(it).min, ((CompositeImage) re).getChannelLut(it).max] }, tifWin)
check("channel colours as written",
      [1, 2].collect { new Color(((CompositeImage) re).getChannelLut(it).getRGB(255)) }, [Color.BLUE, Color.GREEN])
def reOv = re.getOverlay()
check("overlay: 6 outlines (4 counted + 2 invalid)",    reOv?.size(), 6)
check("overlay names as written",
      reOv ? (0..<reOv.size()).collect { reOv.get(it).getName() }.unique() : null,
      ["nucleus_1", "nucleus_2", "invalid_nucleus_3", "invalid_contrast_nucleus_4"])
def reN1 = reOv ? (0..<reOv.size()).collect { reOv.get(it) }.find { it.getName() == "nucleus_1" } : null
def reBack = reN1 ? (0..<reN1.getFloatPolygon().npoints).collect {
    [Math.round(reN1.getFloatPolygon().xpoints[it]) as int, Math.round(reN1.getFloatPolygon().ypoints[it]) as int] } : null
check("nucleus_1's vertices survive the file",          reBack, orig)
re.close(); re.flush()

println ""
println "=== validateBatch(): refused before any image is opened ==="
def okArgs = [savePng: true, saveTiff: true, tiffColors: "", footprintDir: fpDir.getAbsolutePath(), roiDir: "",
              roiMode: "merged", feature: "nucleus", drawRejected: "none",
              countedColor: "yellow", invalidColor: "cyan", failedColor: "magenta"]
def v = FO.validateBatch(okArgs, OV)
check("valid settings normalise",                       [v.source, v.statuses, v.channelColours], ["footprint", ["counted"] as Set, null])
check("both outputs off",                               errOf { FO.validateBatch(okArgs + [savePng: false, saveTiff: false], OV) }?.contains("nothing to write"), true)
check("footprints AND an ROI zip folder",               errOf { FO.validateBatch(okArgs + [roiDir: tmp.getAbsolutePath()], OV) }?.contains("not both"), true)
check("an overlay without the TIFF",                    errOf { FO.validateBatch(okArgs + [saveTiff: false], OV) }?.contains("saveTiff is off"), true)
check("a footprint folder that does not exist",         errOf { FO.validateBatch(okArgs + [footprintDir: "/no/such/dir"], OV) }?.contains("no such folder"), true)
check("drawRejected with ROI zips (they carry no status)",
      errOf { FO.validateBatch(okArgs + [footprintDir: "", roiDir: tmp.getAbsolutePath(), drawRejected: "invalid"], OV) }?.contains("needs footprints"), true)
check("a bad outline colour",                           errOf { FO.validateBatch(okArgs + [invalidColor: "cyna"], OV) }?.contains("invalidColor"), true)
check("a bad channel colour",                           errOf { FO.validateBatch(okArgs + [tiffColors: "blue,grene"], OV) }?.contains("grene"), true)

println ""
println "=== the batch front end, end to end on files ==="
// One image file; four sheet rows:
//   A  footprints with a counted and an invalid feature, and an ROI zip
//   B  a header-only footprint table (no features), _config.txt count 0
//   C  no footprint table at all  -> failed when footprints are asked for
//   D  include=false              -> excluded
def raw = new File(tmp, "raw"); raw.mkdirs()
def seg = new File(tmp, "seg"); seg.mkdirs()
def fpb = new File(tmp, "fpb"); fpb.mkdirs()
def meta = MetadataTools.createOMEXMLMetadata()
MetadataTools.populateMetadata(meta, 0, "S", false, DimensionOrder.XYZCT.toString(),
                               PixelType.UINT8.toString(), W, H, 3, 2, 1, 1)
meta.setPixelsPhysicalSizeX(new Length(PW, UNITS.MICROMETER), 0)
meta.setPixelsPhysicalSizeY(new Length(PH, UNITS.MICROMETER), 0)
meta.setPixelsPhysicalSizeZ(new Length(2.0d, UNITS.MICROMETER), 0)
def w = new OMETiffWriter(); w.setMetadataRetrieve(meta); w.setId(new File(raw, "img.ome.tif").getAbsolutePath())
(1..2).each { int ch -> (1..3).each { int z -> w.saveBytes((z - 1) + 3 * (ch - 1), (byte[]) planes(ch, z).getPixels()) } }
w.close()
new File(fpb, "A_nucleus_footprint.txt").setText(
    fpRows.findAll { it.startsWith("name") || it.contains("\tnucleus_1\t") || it.contains("\tinvalid_nucleus_3\t") }
          .collect { it.replaceFirst(/^S1\t/, "A\t") }.join("\n") + "\n", "UTF-8")
new File(fpb, "B_nucleus_footprint.txt").setText("name\tfeature_id\tpart\tring\tx\ty\n", "UTF-8")
RX.saveRoiZip([poly], ["nucleus_0002-0001-0085"], new File(seg, "A_nucleus_outline_ROIs.zip").getAbsolutePath())
new File(seg, "B_config.txt").setText("parameter\tvalue\nnucleus_count\t0\n", "UTF-8")
def sheet = new File(tmp, "samples.tsv")
sheet.setText("prefix\tinclude\tpath\tseries_index\n" +
              "A\ttrue\timg.ome.tif\t0\nB\ttrue\timg.ome.tif\t0\nC\ttrue\timg.ome.tif\t0\nD\tfalse\timg.ome.tif\t0\n", "UTF-8")

def runScript = new File(LIBDIR, "Run_Overview_Batch.groovy")
def src = runScript.text.readLines().findAll { !it.trim().startsWith("#@") }.join("\n")
def runBatch = { File out, Map over ->
    def args = ["javax.script.filename": runScript.getAbsolutePath(),
                sheetFile: sheet, outdir: out, imageRoot: raw.getAbsolutePath(), zSpec: "", channelsCsv: "",
                method: "max", contrast: "auto", saturated: 0.35d, outWidth: 100, outHeight: 0, openMode: "auto",
                savePng: true, saveTiff: false, tiffColors: "", footprintDir: "", roiDir: "", roiMode: "merged",
                feature: "nucleus", drawRejected: "none", countedColor: "yellow", invalidColor: "cyan",
                failedColor: "magenta"] + over
    return errOf { new GroovyShell(this.class.classLoader, new Binding(args)).evaluate(src) }
}
def summary = { File out ->
    def f = new File(out, "batch_summary.tsv")
    if (!f.isFile()) return [header: null, rows: [:]]
    def lines = f.readLines()
    def hdr = lines[0].split("\t", -1) as List
    [header: hdr, rows: lines.drop(1).collectEntries { def p = it.split("\t", -1); [(p[0]): [hdr, p as List].transpose().collectEntries()] }]
}
def bytesOf = { File f -> f.isFile() ? f.bytes : null }

def outPng = new File(tmp, "out_png")
check("PNG-only run (the old behaviour) runs",          runBatch(outPng, [:]), null)
def sPng = summary(outPng)
check("PNG-only summary: the columns it always had",    sPng.header,
      ["prefix", "path", "series_index", "status", "open_method", "channels", "png_size", "display_range", "seconds", "message"])
check("PNG-only: no TIFF",                              outPng.listFiles().findAll { it.getName().endsWith(".tif") }.size(), 0)

def outFp = new File(tmp, "out_fp")
check("PNG + TIFF with footprints runs",
      runBatch(outFp, [saveTiff: true, tiffColors: "blue,green", footprintDir: fpb.getAbsolutePath(), drawRejected: "invalid"]), null)
def sFp = summary(outFp)
check("statuses: A ok, B ok, C failed, D excluded",     sFp.rows.collectEntries { k, r -> [(k): r.status] }, [A: "ok", B: "ok", C: "failed", D: "excluded"])
check("summary gains tiff and overlay, before seconds", sFp.header?.subList(5, 10), ["channels", "png_size", "display_range", "tiff", "overlay"])
check("A's overlay: one counted, one invalid",          sFp.rows.A?.overlay, "counted:1 invalid:1")
check("B's overlay: header-only footprints draw nothing", sFp.rows.B?.overlay, "counted:0 invalid:0")
check("C's message names the missing step",             sFp.rows.C?.message?.contains("feature_footprint_cli.r"), true)
check("C failed before writing anything (no half-written row)",
      outFp.listFiles().findAll { it.getName().startsWith("C_") }.collect { it.getName() }, [])
check("the PNGs are byte-identical to the PNG-only run",
      ["A_overview_ch1.png", "A_overview_ch2.png", "B_overview_ch1.png"].collect { Arrays.equals(bytesOf(new File(outPng, it)), bytesOf(new File(outFp, it))) },
      [true, true, true])
check("...and so is everything the summary had before", ["A", "B"].collect { k -> ["channels", "png_size", "display_range"].collect { sFp.rows[k]?.get(it) } },
      ["A", "B"].collect { k -> ["channels", "png_size", "display_range"].collect { sPng.rows[k]?.get(it) } })
def tA = IJ.openImage(new File(outFp, "A_overview.tif").getAbsolutePath())
check("A's TIFF: full resolution, 2 channels, calibrated",
      tA ? [tA.getWidth(), tA.getHeight(), tA.getNChannels(), tA.getCalibration().pixelWidth, tA.getCalibration().pixelHeight] : null,
      [W, H, 2, PW, PH])
check("A's TIFF: pixels are the projection of the file",  tA ? planesEqual(tA, proj) : null, true)
check("A's TIFF: overlay named as the footprints",
      tA?.getOverlay() ? (0..<tA.getOverlay().size()).collect { tA.getOverlay().get(it).getName() } : null,
      ["nucleus_1", "invalid_nucleus_3"])
check("A's TIFF title",                                 tA?.getTitle(), "A_overview.tif")
tA?.close(); tA?.flush()

def outRoi = new File(tmp, "out_roi")
check("TIFF only, ROI zips, every ROI",
      runBatch(outRoi, [savePng: false, saveTiff: true, roiDir: seg.getAbsolutePath(), roiMode: "all"]), null)
def sRoi = summary(outRoi)
check("A: the zip's one ROI; B: count 0 is not a failure; C: no zip, no config -> failed",
      ["A", "B", "C"].collect { [sRoi.rows[it]?.status, sRoi.rows[it]?.overlay] },
      [["ok", "roi_all:1"], ["ok", "roi_all:0"], ["failed", ""]])
check("no PNGs when savePng is off",                    outRoi.listFiles().findAll { it.getName().endsWith(".png") }.size(), 0)
check("display_range still reported, = the PNG run's",  sRoi.rows.A?.display_range, sPng.rows.A?.display_range)
check("png_size blank when no PNG was written",         sRoi.rows.A?.png_size, "")

check("nothing to write is refused before any row",
      runBatch(new File(tmp, "out_none"), [savePng: false])?.contains("nothing to write"), true)
check("...and wrote no summary",                        new File(tmp, "out_none").exists() && new File(tmp, "out_none/batch_summary.tsv").exists(), false)

tmp.deleteDir()
println ""
println "passed: ${passed}   FAILED: ${failed}"
