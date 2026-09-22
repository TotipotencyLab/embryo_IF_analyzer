// Test_Overview.groovy
//
// Fixture-free checks for Overview.groovy. Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_Overview.groovy
//
// How the synthetic image makes each check able to fail: every plane is filled
// with ONE value that encodes where it came from,
//
//     value(c, z) = 10 * c + z        e.g. channel 2, z 4  ->  24
//
// so a projection result says exactly which planes went into it. Take the wrong
// plane, the wrong channel, or mix channels, and the number is different.

import ij.*
import ij.gui.Roi
import ij.process.ByteProcessor
import ij.process.FloatProcessor
import ij.process.ImageProcessor
import ij.process.ShortProcessor

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "Overview.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def OV = new GroovyClassLoader().parseClass(new File(LIBDIR + "/Overview.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-50s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def throwsWith = { String what, String fragment, Closure body ->
    String msg = null
    try { body() } catch (IllegalArgumentException e) { msg = e.getMessage() }
    boolean ok = msg != null && msg.contains(fragment)
    println String.format("  %-6s %-50s threw=%s", ok ? "ok" : "FAILED", what, msg ?: "(nothing)")
    ok ? passed++ : failed++
}

// nC channels x nZ slices, each plane constant at 10*c + z.
// Hyperstack order: the channel varies FASTEST, so z is the outer loop.
def synth = { int nC, int nZ ->
    def st = new ImageStack(8, 8)
    (1..nZ).each { z -> (1..nC).each { c ->
        def ip = new ByteProcessor(8, 8); ip.setValue(10 * c + z); ip.fill()
        st.addSlice("c${c}z${z}", ip)
    } }
    def imp = new ImagePlus("synth", st)
    imp.setDimensions(nC, nZ, 1)
    imp.setOpenAsHyperStack(true)
    imp.getCalibration().pixelWidth = 0.25
    imp.getCalibration().setUnit("micron")
    imp
}
// Value of projection plane i (all pixels are equal, so read one).
def val = { ImagePlus p, int i -> p.getStack().getProcessor(i).getPixelValue(3, 3) as double }

println "=== Overview.project ==="

def img = synth(2, 5)

// --- the test image itself -----------------------------------------------------
// Checked through ImageJ's own indexing, independently of Overview. The first
// version of this test built the stack channel-major -- the same mistake as the
// code it was testing -- so the fixture has to be verified on its own terms.
def at = { ImagePlus im, int c, int z -> im.getStack().getProcessor(im.getStackIndex(c, z, 1)).getPixelValue(0, 0) as double }
check("fixture: (c1,z1) = 11",                 at(img, 1, 1), 11.0d)
check("fixture: (c2,z4) = 24",                 at(img, 2, 4), 24.0d)
check("fixture: (c1,z5) = 15",                 at(img, 1, 5), 15.0d)

// --- the projection takes the right planes ---------------------------------
def p = OV.project(img, null, "max", null)
check("all z, max: ch1 = 10+5",                val(p, 1), 15.0d)
check("all z, max: ch2 = 20+5",                val(p, 2), 25.0d)
check("one plane per channel",                 [p.getNChannels(), p.getNSlices()], [2, 1])

// Gapped range under a MEAN. Uses its own stack, because ZProjector returns the
// mean of 8-bit data as 8-bit (rounded): with the 10*c+z planes, 12.667 becomes
// 13 -- the same as the contiguous 1-5 mean, so the check could not tell a gap
// from no gap. Planes of 30, 60, 90, 120, 150 average to whole numbers:
//   {1,2,5} -> 80     contiguous 1-5 -> 90     1-2 -> 45
def wide = { ->
    def st = new ImageStack(8, 8)
    [30, 60, 90, 120, 150].each { v -> def ip = new ByteProcessor(8, 8); ip.setValue(v); ip.fill(); st.addSlice("", ip) }
    new ImagePlus("wide", st)
}
p = OV.project(wide(), [1, 2, 5] as Set, "mean", null)
check("gapped z {1,2,5}, mean = 80 (not 90)",  val(p, 1), 80.0d)

// Pinned, because prepare() depends on it: a later Fiji may change this.
check("8-bit mean stays 8-bit (rounded)",      OV.project(img, null, "mean", [1]).getBitDepth(), 8)
check("sum becomes 32-bit",                    OV.project(img, null, "sum",  [1]).getBitDepth(), 32)

p = OV.project(img, [2, 4] as Set, "min", [1])
check("z {2,4}, min = 12 (not 11)",            val(p, 1), 12.0d)
p = OV.project(img, [2, 4] as Set, "max", [1])
check("z {2,4}, max = 14 (not 15)",            val(p, 1), 14.0d)

// --- channels ----------------------------------------------------------------
p = OV.project(img, null, "max", [2])
check("channel [2] only: one plane",           p.getStackSize(), 1)
check("channel [2] only: value from ch2",      val(p, 1), 25.0d)
check("channel [2] only: labelled ch2",        OV.projectedChannels(p), [2])

p = OV.project(img, null, "max", [2, 1])
check("channel order follows the request",     OV.projectedChannels(p), [2, 1])
check("...and so do the values",               [val(p, 1), val(p, 2)], [25.0d, 15.0d])

// The trap: several channels, ONE z. ZProjector would take the max across the
// channels and return a single plane of 23.
p = OV.project(img, [3] as Set, "max", [1, 2])
check("single z, 2 channels: not mixed",       [val(p, 1), val(p, 2)], [13.0d, 23.0d])
check("single z, 2 channels: 2 planes",        p.getStackSize(), 2)

// A plain z-stack (one channel) goes through the same path.
p = OV.project(synth(1, 4), null, "max", null)
check("one-channel stack, max = 10+4",         val(p, 1), 14.0d)

// --- method names ------------------------------------------------------------
check("'avg' and 'mean' agree",
      val(OV.project(img, null, "avg", [1]), 1), val(OV.project(img, null, "mean", [1]), 1))
check("method name is case-insensitive",       val(OV.project(img, null, "MAX", [1]), 1), 15.0d)
check("sum = 11+12+13+14+15",                  val(OV.project(img, null, "sum", [1]), 1), 65.0d)

// --- bad input fails loudly ----------------------------------------------------
throwsWith("unknown method is rejected",       "unknown projection") { OV.project(img, null, "brightest", null) }
throwsWith("z out of range is rejected",       "z out of range")     { OV.project(img, [0, 6] as Set, "max", null) }
throwsWith("channel out of range is rejected", "channel out of range") { OV.project(img, null, "max", [3]) }

// --- side effects and metadata -----------------------------------------------
def before = (1..img.getStackSize()).collect { img.getStack().getProcessor(it).getPixelValue(0, 0) }
OV.project(img, [1, 3] as Set, "sum", [1, 2])
def after  = (1..img.getStackSize()).collect { img.getStack().getProcessor(it).getPixelValue(0, 0) }
check("source pixels untouched",               after, before)
check("source still 10 planes",                img.getStackSize(), 10)

p = OV.project(img, null, "max", null)
check("calibration kept",
      [p.getCalibration().pixelWidth, p.getCalibration().getUnit()], [0.25d, "micron"])

// ============================================================================
println ""
println "=== Overview.prepare ==="

// A one-plane "projection" labelled like project() output, from any processor.
def asProj = { ImageProcessor ip, int c, double pw = 0.25d ->
    def st = new ImageStack(ip.getWidth(), ip.getHeight()); st.addSlice("ch${c}", ip)
    def im = new ImagePlus("p", st)
    im.getCalibration().pixelWidth = pw; im.getCalibration().pixelHeight = pw
    im.getCalibration().setUnit("micron")
    im
}
// 200 x 100, 8-bit, a left-to-right ramp from 10 to 40: a DIM image, so an
// automatic stretch has something to do and "none" visibly does not.
def ramp8 = { ->
    def ip = new ByteProcessor(200, 100)
    for (int x = 0; x < 200; x++) for (int y = 0; y < 100; y++) ip.set(x, y, 10 + Math.round(30 * x / 199.0d) as int)
    ip
}
def dims = { v -> [v.image.getWidth(), v.image.getHeight()] }
def range = { v -> [v.image.getProcessor().getMin(), v.image.getProcessor().getMax()] }

// --- channel is looked up by its ORIGINAL number --------------------------------
def proj3 = OV.project(synth(3, 2), null, "max", [2, 3])   // planes are ch2, ch3
def v = OV.prepare(proj3, 3)
check("channel 3 -> ch3 values (30+2)",        v.image.getProcessor().getPixelValue(0, 0) as double, 32.0d)
check("View remembers its channel",            v.channel, 3)
throwsWith("channel not projected is rejected", "not in the projection") { OV.prepare(proj3, 1) }

// --- output size ---------------------------------------------------------------
def pr = asProj(ramp8(), 1)
v = OV.prepare(pr, 1)
check("blank size -> original 200x100",        dims(v), [200, 100])
check("...scale 1x1",                          [v.sx, v.sy], [1.0d, 1.0d])
v = OV.prepare(pr, 1, [width: 50])
check("width 50 -> 50x25 (aspect kept)",       dims(v), [50, 25])
check("...scale 0.25x0.25",                    [v.sx, v.sy], [0.25d, 0.25d])
v = OV.prepare(pr, 1, [height: 50])
check("height 50 -> 100x50 (aspect kept)",     dims(v), [100, 50])
v = OV.prepare(pr, 1, [width: 40, height: 40])
check("both 40 -> 40x40 (stretched)",          dims(v), [40, 40])
check("...unequal scale 0.2x0.4",              [v.sx, v.sy], [0.2d, 0.4d])
v = OV.prepare(pr, 1, [width: "", height: 0])
check("'' and 0 both mean 'not given'",        dims(v), [200, 100])
throwsWith("non-numeric width is rejected",    "whole number") { OV.prepare(pr, 1, [width: "abc"]) }
throwsWith("negative height is rejected",      "negative")     { OV.prepare(pr, 1, [height: -5]) }

v = OV.prepare(pr, 1, [width: 50])
check("calibration follows the resize",        v.image.getCalibration().pixelWidth, 1.0d)

// The resized image must still be the same picture: left half 0, right half 200.
// A flipped, shifted or garbage resize fails this.
def halves = new ByteProcessor(200, 100)
halves.setValue(200); halves.setRoi(100, 0, 100, 100); halves.fill(); halves.resetRoi()
v = OV.prepare(asProj(halves, 1), 1, [width: 50, contrast: "none"])
check("resize keeps the picture (0 | 200)",
      [v.image.getProcessor().getPixelValue(5, 12), v.image.getProcessor().getPixelValue(45, 12)], [0.0f, 200.0f])

// --- contrast ------------------------------------------------------------------
v = OV.prepare(pr, 1, [contrast: "none"])
check("8-bit 'none' -> full 0-255",            range(v), [0.0d, 255.0d])
v = OV.prepare(pr, 1)
def (lo8, hi8) = range(v)
check("8-bit 'auto' stays inside the data 10-40", lo8 >= 10 && hi8 <= 40, true)
println "         (auto range was ${lo8}-${hi8})"
check("'auto' leaves pixel values alone",      v.image.getProcessor().getPixelValue(100, 50), pr.getProcessor().getPixelValue(100, 50))

def r16 = new ShortProcessor(200, 100)
for (int x = 0; x < 200; x++) for (int y = 0; y < 100; y++) r16.set(x, y, 1000 + 10 * x)
check("16-bit 'none' -> full 0-65535",         range(OV.prepare(asProj(r16, 1), 1, [contrast: "none"])), [0.0d, 65535.0d])
def (lo16, hi16) = range(OV.prepare(asProj(r16, 1), 1))
check("16-bit 'auto' stays inside 1000-2990",  lo16 >= 1000 && hi16 <= 2990, true)

def r32 = new FloatProcessor(200, 100)
for (int x = 0; x < 200; x++) for (int y = 0; y < 100; y++) r32.setf(x, y, 5 * x as float)
check("32-bit 'none' -> the data's own 0-995", range(OV.prepare(asProj(r32, 1), 1, [contrast: "none"])), [0.0d, 995.0d])
def (lo32, hi32) = range(OV.prepare(asProj(r32, 1), 1))
// A sum projection lands here; its range must not be clipped to 8-bit's 255.
check("32-bit 'auto' not clipped at 255",      hi32 > 255 && hi32 <= 995, true)

// On a smooth ramp 'auto' clips nothing, so for 32-bit it returns the same range
// as 'none' -- the check above would pass even if 'auto' did nothing. The case
// that tells them apart is the one auto-contrast exists for: a few HOT PIXELS.
// 3 pixels of 100000 are 0.015% of the image, under the 0.35% that 'auto' clips.
def hot = r32.duplicate()
[[10, 10], [150, 60], [190, 90]].each { xy -> hot.setf(xy[0], xy[1], 100000f) }
check("hot pixels: 'none' stretches to them",  range(OV.prepare(asProj(hot, 1), 1, [contrast: "none"]))[1], 100000.0d)
def hiHot = range(OV.prepare(asProj(hot, 1), 1))[1]
check("hot pixels: 'auto' ignores them",       hiHot <= 995, true)
println "         (auto max with hot pixels was ${hiHot})"

def hot8 = ramp8()
[[10, 10], [150, 60], [190, 90]].each { xy -> hot8.set(xy[0], xy[1], 255) }
check("8-bit hot pixels: 'auto' max stays ~40", range(OV.prepare(asProj(hot8, 1), 1))[1] <= 40, true)

// saturated: 0 means "clip nothing", so the hot pixels are back in range. With a
// careless `opts.saturated ?: 0.35`, 0 counts as false, becomes 0.35, and the
// hot pixels are still ignored -- which this check catches.
// The top lands on a histogram bin edge (see the 32-bit note in prepare()), so
// it is 99609, not exactly 100000. What matters is ~99609 vs the ~781 of the bug.
def hiSat0 = range(OV.prepare(asProj(hot, 1), 1, [saturated: 0]))[1]
check("saturated 0 clips nothing",             hiSat0 > 99000, true)
println "         (saturated 0 max was ${hiSat0}; with the Elvis bug it was 781)"
throwsWith("saturated 100 is rejected",        "saturated")      { OV.prepare(pr, 1, [saturated: 100]) }

throwsWith("unknown contrast is rejected",     "unknown contrast") { OV.prepare(pr, 1, [contrast: "vivid"]) }

// --- the projection is not modified ---------------------------------------------
OV.prepare(proj3, 2, [width: 3, contrast: "auto"])
check("projection keeps its size and planes",
      [proj3.getWidth(), proj3.getHeight(), proj3.getStackSize()], [8, 8, 2])
check("projection keeps its values",           val(proj3, 1), 22.0d)

// ============================================================================
println ""
println "=== Overview.addOutlines + savePng ==="
// Checked on the SAVED PNG -- the file a person opens -- by reading colours back.

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_overview_" + System.nanoTime())
int nSaved = 0
// NB: closure parameters must not reuse a name already declared in the script
//     (`view`, `v`), or the script fails to compile.
def saveAndOpen = { def vw ->
    def f = OV.savePng(vw, new File(tmp, "out${nSaved++}.png").getPath())
    IJ.openImage(f.getPath())
}
def rgb = { ImagePlus im, int x, int y -> def c = im.getProcessor().getPixel(x, y); [(c >> 16) & 255, (c >> 8) & 255, c & 255] }
def GREY = [50, 50, 50], YELLOW = [255, 255, 0], MAGENTA = [255, 0, 255]

// A flat grey 200x100 source. Shown at width 100, so the view is half size.
def grey = { -> def ip = new ByteProcessor(200, 100); ip.setValue(50); ip.fill(); ip }
def halfView = { -> OV.prepare(asProj(grey(), 1), 1, [width: 100, contrast: "none"]) }
def rect = { int x, int y, int w, int h, int pos = 0 -> def r = new Roi(x, y, w, h); if (pos) r.setPosition(pos); r }

// --- outlines are scaled into the view --------------------------------------------
// Roi at (60,20) 40x40 in the source -> (30,10) 20x20 in the half-size view.
def view = halfView()
check("one outline drawn",                     OV.addOutlines(view, [rect(60, 20, 40, 40)]), 1)
def png = saveAndOpen(view)
check("saved PNG has the view's size",         [png.getWidth(), png.getHeight()], [100, 50])
check("saved PNG is RGB",                      png.getType(), ImagePlus.COLOR_RGB)
check("outline at the SCALED edge (30,20)",    rgb(png, 30, 20), YELLOW)
// Where the outline would be had it not been scaled: must be untouched.
check("nothing at the UNSCALED edge (60,40)",  rgb(png, 60, 40), GREY)
check("inside is not filled (40,20)",          rgb(png, 40, 20), GREY)

// --- line width is in output pixels ---------------------------------------------
view = halfView(); OV.addOutlines(view, [rect(60, 20, 40, 40)], [lineWidth: 1])
check("width 1: 2 px inside the edge is bare", rgb(saveAndOpen(view), 32, 20), GREY)
view = halfView(); OV.addOutlines(view, [rect(60, 20, 40, 40)], [lineWidth: 5])
check("width 5: 2 px inside the edge is drawn", rgb(saveAndOpen(view), 32, 20), YELLOW)

// --- colours, and later calls on top -----------------------------------------------
view = halfView(); OV.addOutlines(view, [rect(60, 20, 40, 40)], [color: "#00ff00"])
check("hex colour",                            rgb(saveAndOpen(view), 30, 20), [0, 255, 0])
view = halfView()
OV.addOutlines(view, [rect(20, 20, 40, 40)], [color: "yellow"])   // view (10,10)-(30,30)
OV.addOutlines(view, [rect(60, 20, 40, 40)], [color: "magenta"])  // view (30,10)-(50,30)
png = saveAndOpen(view)
check("first feature keeps its colour",        rgb(png, 10, 20), YELLOW)
check("second feature has its own colour",     rgb(png, 50, 20), MAGENTA)
check("shared edge: later call is on top",     rgb(png, 30, 20), MAGENTA)
throwsWith("unknown colour is rejected",       "unknown colour") { OV.addOutlines(halfView(), [rect(0, 0, 4, 4)], [color: "sparkly"]) }
throwsWith("negative line width is rejected",  "positive")       { OV.addOutlines(halfView(), [rect(0, 0, 4, 4)], [lineWidth: -1]) }
// 0 is FALSE in Groovy: a careless `opts.lineWidth ?: 1` turns 0 into 1 silently.
throwsWith("zero line width is rejected",      "positive")       { OV.addOutlines(halfView(), [rect(0, 0, 4, 4)], [lineWidth: 0]) }

// --- modes ---------------------------------------------------------------------------
// Two overlapping squares, view coords (10,10)-(30,30) and (20,10)-(40,30).
// Square B's left edge at x=20 lies INSIDE square A: drawn in "all", gone in "merged".
def overlapping = { -> [rect(20, 20, 40, 40), rect(40, 20, 40, 40)] }
view = halfView()
check("'all' draws every ROI",                 OV.addOutlines(view, overlapping(), [mode: "all"]), 2)
check("'all': inner edge is drawn",            rgb(saveAndOpen(view), 20, 20), YELLOW)
view = halfView()
check("'merged' draws one outline",            OV.addOutlines(view, overlapping(), [mode: "merged"]), 1)
png = saveAndOpen(view)
check("'merged': inner edge is gone",          rgb(png, 20, 20), GREY)
check("'merged': outer edge is kept",          rgb(png, 10, 20), YELLOW)
check("'merged' keeps separate ROIs separate",
      OV.addOutlines(halfView(), overlapping() + [rect(120, 20, 20, 20)], [mode: "merged"]), 2)
view = halfView()
check("'none' draws nothing",                  OV.addOutlines(view, overlapping(), [mode: "none"]), 0)
check("'none': image is bare",                 rgb(saveAndOpen(view), 10, 20), GREY)
check("empty ROI list draws nothing",          OV.addOutlines(halfView(), []), 0)
throwsWith("unknown mode is rejected",         "unknown outline mode") { OV.addOutlines(halfView(), overlapping(), [mode: "outline"]) }

// --- detection ROIs carry a slice position ------------------------------------------
view = halfView(); OV.addOutlines(view, [rect(60, 20, 40, 40, 7)])
check("ROI from slice 7 is still drawn",       rgb(saveAndOpen(view), 30, 20), YELLOW)

// --- the caller's ROIs are not changed ---------------------------------------------
def mine = [rect(20, 20, 40, 40, 7), rect(40, 20, 40, 40, 9)]
def snap = { -> mine.collect { [it.getBounds().x, it.getBounds().width, it.getPosition(), it.getStrokeColor(), it.getStrokeWidth()] } }
def beforeRois = snap()
OV.addOutlines(halfView(), mine, [mode: "all", color: "magenta", lineWidth: 4])
OV.addOutlines(halfView(), mine, [mode: "merged"])
check("caller's ROIs untouched",               snap(), beforeRois)

// --- the display range is what gets saved --------------------------------------------
// prepare() only sets a display range; savePng() is where it must take effect.
// On the dim 10-40 ramp, the brightest column (40) is white with 'auto' and
// stays 40 with 'none'. If save ignored the range, both would read 40.
def edge = { vw -> rgb(saveAndOpen(vw), 199, 50)[0] }
check("'none' saves raw brightness (40)",      edge(OV.prepare(asProj(ramp8(), 1), 1, [contrast: "none"])), 40)
def autoEdge = edge(OV.prepare(asProj(ramp8(), 1), 1))
check("'auto' saves stretched brightness",     autoEdge >= 250, true)
println "         (auto brightest column saved as ${autoEdge})"
def e32 = edge(OV.prepare(asProj(r32, 1), 1, [contrast: "none"]))
check("32-bit saves to a visible 0-255",       e32 >= 250, true)

// --- files ---------------------------------------------------------------------------
def deep = new File(tmp, "a/b/c/deep.png")
OV.savePng(halfView(), deep.getPath())
check("missing directories are created",       deep.isFile(), true)
check("overview file name",
      OV.overviewPath("/data/out", "GRV_Position010", 1), "/data/out/GRV_Position010_overview_ch1.png")

tmp.deleteDir()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} Overview check(s) failed")
