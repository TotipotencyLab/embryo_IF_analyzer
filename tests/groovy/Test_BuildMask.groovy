// Test_BuildMask.groovy
//
// Fixture-free checks for RoiDetect.buildMask(). Run headless:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_BuildMask.groovy
//
// The images are synthesised here, so this needs no data and can run anywhere
// Fiji does. Prints one line per check and a final count; a non-zero FAILED
// count is the signal to act on. Do not read a bare "OK" as a pass -- every
// assertion below prints the value it actually saw.

import ij.*
import ij.process.ByteProcessor
import ij.gui.OvalRoi

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "RoiDetect.groovy").exists()) {
    throw new IllegalStateException("run this from the repository root; no scripts/groovy at " + LIBDIR)
}
def RD = new GroovyClassLoader().parseClass(new File(LIBDIR + "/RoiDetect.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-52s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}

// Count particles in a one-slice mask built from `ip`.
def countParticles = { ByteProcessor ip, boolean watershed ->
    def imp = new ImagePlus("t", ip)
    def mask = RD.buildMask(imp, 1, 0.0d, "Otsu", true, watershed)
    def rois = RD.detect(mask, "0-Infinity", "", [1] as Set, false, true)
    mask.close()
    return rois.size()
}

def disc = { int x, int y, int d, ByteProcessor ip -> ip.setColor(255); ip.fill(new OvalRoi(x, y, d, d)); ip }

println "=== RoiDetect.buildMask ==="

// Two discs that overlap threshold into a single blob. This is the case
// watershed exists for: a zygote's two pronuclei, or oocytes packed together.
def touching = { -> def ip = new ByteProcessor(200, 120); disc(20, 20, 80, ip); disc(80, 20, 80, ip); ip }
check("touching discs, watershed off -> one blob", countParticles(touching(), false), 1)
check("touching discs, watershed on  -> split",    countParticles(touching(), true),  2)

// Well-separated objects must be left alone: watershed should never invent
// splits where there is nothing to split.
def separate = { -> def ip = new ByteProcessor(240, 120); disc(20, 20, 70, ip); disc(150, 20, 70, ip); ip }
check("separate discs, watershed off",             countParticles(separate(), false), 2)
check("separate discs, watershed on  -> unchanged", countParticles(separate(), true),  2)

// A single object stays one object.
def single = { -> disc(30, 20, 80, new ByteProcessor(150, 120)) }
check("single disc, watershed off",                countParticles(single(), false), 1)
check("single disc, watershed on   -> unchanged",  countParticles(single(), true),  1)

// buildMask must not modify the image it was given.
def src = new ImagePlus("src", touching())
def before = src.getProcessor().getStatistics().mean
def built = RD.buildMask(src, 1, 2.0d, "Otsu", true, true)
def after = src.getProcessor().getStatistics().mean
built.close()
check("source image untouched", String.format("%.6f", after), String.format("%.6f", before))

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} buildMask check(s) failed")
