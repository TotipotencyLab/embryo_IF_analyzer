// Test_RoiContrast.groovy
//
// RoiContrast (inside vs ring, per ROI, on its own slice) and its batch front
// end, Run_RoiContrast_Batch.groovy. analysis-oo_count-physical_blur.
//
// Synthesised images, so no fixture. Every check is built so that the obvious
// wrong implementation fails it: the measured slice and channel hold values no
// other slice or channel holds, the ring's width is checked against a second
// pixel size, an object INSIDE the 3 um gap must not move the ring mean while
// one IN the band must, and an ROI at the edge must not count pixels outside
// the image as zeros.
//
// Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_RoiContrast.groovy

import ij.ImagePlus
import ij.ImageStack
import ij.gui.OvalRoi
import ij.process.ByteProcessor
import loci.formats.MetadataTools
import loci.formats.out.OMETiffWriter
import ome.units.UNITS
import ome.units.quantity.Length
import ome.xml.model.enums.DimensionOrder
import ome.xml.model.enums.PixelType

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "RoiContrast.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy/RoiContrast.groovy at " + LIBDIR)
}

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-58s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def errOf = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() ?: t.getClass().getSimpleName() } }

def gcl = new GroovyClassLoader(this.class.classLoader)
def RCT = gcl.parseClass(new File(LIBDIR, "RoiContrast.groovy"))
def RX  = gcl.parseClass(new File(LIBDIR, "RoiExport.groovy"))

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_roicontrast_" + System.nanoTime())
tmp.mkdirs()

// 2 channels x 3 slices, 200 x 200. Slice 2 is the one the ROI is on:
// ch1 disc 200 on 50, ch2 disc 100 on 25. Slices 1 and 3 are flat 10, so
// measuring the wrong slice reads 10 everywhere.
int CX = 100, CY = 100, R = 15
def planes = { int ch, int z ->
    def ip = new ByteProcessor(200, 200)
    if (z != 2) { ip.setValue(10); ip.fill(); return ip }
    ip.setValue(ch == 1 ? 50 : 25); ip.fill()
    ip.setValue(ch == 1 ? 200 : 100); ip.fill(new OvalRoi(CX - R, CY - R, 2 * R, 2 * R))
    return ip
}
def makeImp = { double pw, Closure extra = null ->
    def st = new ImageStack(200, 200)
    (1..3).each { int z -> (1..2).each { int ch ->       // c fastest, as setDimensions(2, 3, 1) implies
        def ip = planes(ch, z); if (extra) extra(ip, ch, z); st.addSlice(ip) } }
    def imp = new ImagePlus("synthetic", st)
    imp.setDimensions(2, 3, 1)
    imp.getCalibration().pixelWidth = pw; imp.getCalibration().pixelHeight = pw
    imp.getCalibration().setUnit("micron")
    return imp
}
def roiAt = { int cx, int cy, String name ->
    def r = new OvalRoi(cx - R, cy - R, 2 * R, 2 * R); r.setName(name); return r
}

println "=== sliceOf: from the ROI name, as every ROI here is named ==="
check("single frame <feature>_SSSS-NNNN-YYYY",  RCT.sliceOf(roiAt(50, 50, "nucleus_0007-0003-0050")), 7)
check("multi-frame TTTT- prefix: still the slice", RCT.sliceOf(roiAt(50, 50, "nucleus_0002-0007-0003-0050")), 7)
def unnamed = roiAt(50, 50, "free"); unnamed.setPosition(3)
check("an ROI named otherwise falls back to its position", RCT.sliceOf(unnamed), 3)
check("...and with neither, it refuses",
      errOf { RCT.sliceOf(roiAt(50, 50, "free")) }?.contains("cannot tell which slice"), true)

println ""
println "=== inside and ring, on the ROI's own slice and channel ==="
def imp05 = makeImp(0.5d)
def rows = RCT.measure(imp05, [roiAt(CX, CY, "nucleus_0002-0001-0100")], "S1", [1, 2], 3.0d, 12.0d)
def c1 = rows.find { it.ch == 1 }, c2 = rows.find { it.ch == 2 }
check("one row per ROI per channel",              rows.size(), 2)
check("ch1 inside is the disc (slice 2)",         c1.inside_mean as double, 200.0d)
check("ch1 ring is the background around it",    c1.ring_mean as double, 50.0d)
check("ch2 inside is ch2's disc, not ch1's",      c2.inside_mean as double, 100.0d)
check("ch2 ring",                                  c2.ring_mean as double, 25.0d)
check("the row names the series and the ROI",    [c1.name, c1.roi, c1.z], ["S1", "nucleus_0002-0001-0100", 2])
// 3-12 um at 0.5 um/px is 6-24 px outside a 15 px disc: pi*(39^2 - 21^2) = 3393.
int ring05 = c1.ring_area_px as int
check("ring area ~ pi*(39^2-21^2) px at 0.5 um/px (within 5%)",
      Math.abs(ring05 - 3393) < 0.05 * 3393, true)
println "    (ring area " + ring05 + " px)"

println ""
println "=== the ring is set in um: a finer pixel makes it wider in pixels ==="
// At 0.25 um/px the same 3-12 um is 12-48 px: pi*(63^2-27^2) = 10179 px, 3x.
def imp025 = makeImp(0.25d)
def ring025 = RCT.measure(imp025, [roiAt(CX, CY, "nucleus_0002-0001-0100")], "S1", [1], 3.0d, 12.0d)[0].ring_area_px as int
check("0.25 um/px ring ~ 3x the 0.5 um/px one (within 5%)",
      Math.abs(ring025 / (double) ring05 - 3.0d) < 0.15d, true)
println "    (ratio " + String.format("%.3f", ring025 / (double) ring05) + ")"

println ""
println "=== what the ring sees: the 3 um gap is excluded, the band is not ==="
// A bright 4x4 square 2 um (4 px) outside the edge, in the gap.
def gapImp = makeImp(0.5d) { ip, ch, z -> if (z == 2 && ch == 1) { ip.setValue(255); ip.setRoi(CX + R + 2, CY - 2, 4, 4); ip.fill(); ip.resetRoi() } }
def gapRing = RCT.measure(gapImp, [roiAt(CX, CY, "nucleus_0002-0001-0100")], "S1", [1], 3.0d, 12.0d)[0].ring_mean as double
check("an object in the 3 um gap leaves the ring at 50", gapRing, 50.0d)
// The same square 8 um (16 px) outside the edge, in the band.
def bandImp = makeImp(0.5d) { ip, ch, z -> if (z == 2 && ch == 1) { ip.setValue(255); ip.setRoi(CX + R + 15, CY - 2, 4, 4); ip.fill(); ip.resetRoi() } }
def bandRing = RCT.measure(bandImp, [roiAt(CX, CY, "nucleus_0002-0001-0100")], "S1", [1], 3.0d, 12.0d)[0].ring_mean as double
check("an object in the band raises the ring mean", bandRing > 50.0d, true)
println "    (ring mean " + String.format("%.3f", bandRing) + ")"

println ""
println "=== an ROI at the image edge: the ring is clipped, not padded with zeros ==="
def edgeImp = makeImp(0.5d) { ip, ch, z ->
    if (z == 2) { ip.setValue(ch == 1 ? 200 : 100); ip.fill(new OvalRoi(2, CY - R, 2 * R, 2 * R)) } }
def edge = RCT.measure(edgeImp, [roiAt(2 + R, CY, "nucleus_0002-0002-0100")], "S1", [1], 3.0d, 12.0d)[0]
check("edge ROI: ring area smaller than the full ring", (edge.ring_area_px as int) < ring05, true)
check("edge ROI: ring mean still the background",  edge.ring_mean as double, 50.0d)

println ""
println "=== refusals ==="
def uncal = makeImp(0.5d); uncal.getCalibration().setUnit("pixel")
check("an image not in micrometres",
      errOf { RCT.measure(uncal, [roiAt(CX, CY, "nucleus_0002-0001-0100")], "S1", [1], 3.0d, 12.0d) }?.contains("calibrated in micrometres"), true)
check("an ROI on a slice the image does not have",
      errOf { RCT.measure(imp05, [roiAt(CX, CY, "nucleus_0009-0001-0100")], "S1", [1], 3.0d, 12.0d) }?.contains("were these ROIs found on this image"), true)
check("a channel the image does not have",
      errOf { RCT.measure(imp05, [roiAt(CX, CY, "nucleus_0002-0001-0100")], "S1", [3], 3.0d, 12.0d) }?.contains("channel 3"), true)
check("a ring with outer <= inner",
      errOf { RCT.measure(imp05, [roiAt(CX, CY, "nucleus_0002-0001-0100")], "S1", [1], 5.0d, 5.0d) }?.contains("inner < outer"), true)

println ""
println "=== the batch front end, end to end on files ==="
// One 2-channel image file; three sheet rows pointing at it:
//   A  has an ROI zip                      -> measured
//   B  no zip, _config.txt says count 0    -> ok, header-only table
//   C  no zip, no _config.txt              -> failed (wrong segDir looks like this)
def raw = new File(tmp, "raw"); raw.mkdirs()
def seg = new File(tmp, "seg"); seg.mkdirs()
def out = new File(tmp, "out")
def meta = MetadataTools.createOMEXMLMetadata()
MetadataTools.populateMetadata(meta, 0, "S", false, DimensionOrder.XYZCT.toString(),
                               PixelType.UINT8.toString(), 200, 200, 3, 2, 1, 1)
meta.setPixelsPhysicalSizeX(new Length(0.5d, UNITS.MICROMETER), 0)
meta.setPixelsPhysicalSizeY(new Length(0.5d, UNITS.MICROMETER), 0)
meta.setPixelsPhysicalSizeZ(new Length(2.0d, UNITS.MICROMETER), 0)
def w = new OMETiffWriter(); w.setMetadataRetrieve(meta); w.setId(new File(raw, "img.ome.tif").getAbsolutePath())
(1..2).each { int ch -> (1..3).each { int z -> w.saveBytes((z - 1) + 3 * (ch - 1), (byte[]) planes(ch, z).getPixels()) } }
w.close()
RX.saveRoiZip([roiAt(CX, CY, "x")], ["nucleus_0002-0001-0100"], new File(seg, "A_nucleus_outline_ROIs.zip").getAbsolutePath())
new File(seg, "B_config.txt").setText("parameter\tvalue\nnucleus_count\t0\n", "UTF-8")
def sheet = new File(tmp, "samples.tsv")
sheet.setText("prefix\tinclude\tpath\tseries_index\n" +
              "A\ttrue\timg.ome.tif\t0\nB\ttrue\timg.ome.tif\t0\nC\ttrue\timg.ome.tif\t0\nD\tfalse\timg.ome.tif\t0\n", "UTF-8")

def runScript = new File(LIBDIR, "Run_RoiContrast_Batch.groovy")
def src = runScript.text.readLines().findAll { !it.trim().startsWith("#@") }.join("\n")
def b = new Binding(["javax.script.filename": runScript.getAbsolutePath(),
                     sheetFile: sheet, segDir: seg, outdir: out, imageRoot: raw.getAbsolutePath(),
                     feature: "nucleus", channels: "1,2", innerUm: 3.0d, outerUm: 12.0d, openMode: "auto"])
def runErr = errOf { new GroovyShell(this.class.classLoader, b).evaluate(src) }
check("the batch script runs",                     runErr, null)
def summ = out.isDirectory() && new File(out, "batch_summary.tsv").isFile() ?
    new File(out, "batch_summary.tsv").readLines().drop(1).collectEntries { def p = it.split("\t", -1); [(p[0]): p[3]] } : [:]
check("A ok, B ok, C failed, D excluded",          summ, [A: "ok", B: "ok", C: "failed", D: "excluded"])
def tA = new File(out, "A_nucleus_contrast.txt")
def linesA = tA.isFile() ? tA.readLines() : []
check("A's table: header + 2 channel rows",       linesA.size(), 3)
check("A's header is RoiContrast.COLUMNS",         linesA ? linesA[0] : null, RCT.COLUMNS.join("\t"))
def a1 = linesA.size() > 1 ? linesA[1].split("\t", -1) : null
check("A's ch1 row: inside 200, ring 50, read from the file",
      a1 ? [a1[0], a1[3], a1[5] as double, a1[7] as double] : null, ["A", "1", 200.0d, 50.0d])
def tB = new File(out, "B_nucleus_contrast.txt")
check("B (count 0): a header-only table",          tB.isFile() ? tB.readLines() : null, [RCT.COLUMNS.join("\t")])
check("C (no zip, no config): no table",           new File(out, "C_nucleus_contrast.txt").exists(), false)
def msgC = new File(out, "batch_summary.tsv").isFile() ?
    new File(out, "batch_summary.tsv").readLines().find { it.startsWith("C\t") } : ""
check("...and its message says why",               msgC?.contains("is segDir the right folder"), true)
def prm = new File(out, "contrast_params.txt")
check("contrast_params.txt records the ring",
      prm.isFile() && prm.text.contains("ring_inner_um\t3.0") && prm.text.contains("ring_outer_um\t12.0"), true)

tmp.deleteDir()
println ""
println "passed: ${passed}   FAILED: ${failed}"
