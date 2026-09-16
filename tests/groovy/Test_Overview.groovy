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
import ij.process.ByteProcessor

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

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} Overview check(s) failed")
