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
import ij.process.ShortProcessor
import ij.gui.OvalRoi
import ij.plugin.Duplicator

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
    def mask = RD.buildMask(imp, 1, 0.0d, "Otsu", true, watershed).mask
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
built.mask.close()
check("source image untouched", String.format("%.6f", after), String.format("%.6f", before))

println ""
println "=== the same mask the macro call made: the plugin is kept as the ORACLE ==="
// buildMask no longer calls IJ.run("Auto Threshold", ...). It calls the SAME
// plugin through exec(), which additionally returns the threshold it chose --
// the number that was previously thrown away. Nothing about the decision is
// supposed to change, and "nothing changed" is a claim that has to be measured
// rather than asserted. The macro call stays here, in the test, for exactly
// that: when Auto_Threshold is next updated, drift shows up as a failure here
// instead of as quietly different nuclei.
//
// 16-bit is covered deliberately. The repo fixture is 8-bit, so nothing else
// exercises it -- and 16-bit is where the two paths genuinely differ: exec()
// leaves a 0/65535 mask where the macro path converts to 0/255. That is what
// buildMask's to8BitMask() exists for, and it would be invisible in an 8-bit
// test.
def probe = { int bits ->
    def st = new ImageStack(120, 120)
    (1..4).each { int z ->
        def ip = (bits == 8) ? new ByteProcessor(120, 120) : new ShortProcessor(120, 120)
        int bg = (bits == 8) ? 10 : 800
        int fg = (bits == 8) ? (90 + 25 * z) : (7000 + 2000 * z)
        for (int y = 0; y < 120; y++) for (int x = 0; x < 120; x++) ip.set(x, y, bg + ((x * y) % 7))
        ip.setColor(fg); ip.fill(new OvalRoi(20, 20, 40, 40))
        ip.setColor(fg); ip.fill(new OvalRoi(70, 65, 30, 30))
        ip.set(0, 0, 0)                                  // a pure-black pixel, for ignore_black
        ip.set(1, 0, (bits == 8) ? 255 : 65535)          // a saturated one, for ignore_white
        st.addSlice(ip)
    }
    def imp = new ImagePlus("probe" + bits, st)
    imp.setDimensions(1, 4, 1)
    return imp
}
def bytesOf = { ImagePlus imp ->
    def out = new ByteArrayOutputStream()
    for (int z = 1; z <= imp.getStackSize(); z++) {
        def ip = imp.getStack().getProcessor(z)
        for (int y = 0; y < ip.getHeight(); y++)
            for (int x = 0; x < ip.getWidth(); x++) { out.write(ip.get(x, y) & 0xff); out.write(ip.get(x, y) >>> 8) }
    }
    return out.toByteArray()
}

["Huang2", "Otsu", "Default", "Triangle"].each { String m ->
    [8, 16].each { int bits ->
        def raw = probe(bits)
        // The old call, verbatim, including the two ignore_ flags that are now
        // named constants.
        def oracle = new Duplicator().run(raw, 1, 1, 1, raw.getNSlices(), 1, 1)
        IJ.run(oracle, "Auto Threshold",
               "method=${m} ignore_black ignore_white white stack use_stack_histogram")
        def now = RD.buildMask(raw, 1, 0.0d, m, false, false)
        check(m + " " + bits + "-bit: identical to the macro call",
              java.util.Arrays.equals(bytesOf(oracle), bytesOf(now.mask)), true)
        check("  ...and 8-bit whatever went in",   now.mask.getBitDepth(), 8)
        oracle.close(); now.mask.close(); raw.close()
    }
}

println ""
println "=== the reported range really is the range that was applied ==="
// The point of reporting lo-hi rather than the algorithm's bare number is that
// it can be copied straight into a manual threshold without anyone working out
// which end it was. That is only true if it is right, so it is checked against
// the mask rather than against the formula that produced it.
[8, 16].each { int bits ->
    def raw = probe(bits)
    def r   = RD.buildMask(raw, 1, 0.0d, "Default", false, false)
    int lo  = r.lo as int
    int wrongBelow = 0, wrongAbove = 0
    for (int z = 1; z <= raw.getStackSize(); z++) {
        def sp = raw.getStack().getProcessor(z)
        def mp = r.mask.getStack().getProcessor(z)
        for (int y = 0; y < 120; y++) for (int x = 0; x < 120; x++) {
            boolean on = (mp.get(x, y) != 0)
            if (sp.get(x, y) >= lo && !on) wrongAbove++
            if (sp.get(x, y) <  lo &&  on) wrongBelow++
        }
    }
    check(bits + "-bit: every pixel at or above lo is masked",  wrongAbove, 0)
    check(bits + "-bit: no pixel below lo is masked",           wrongBelow, 0)
    check(bits + "-bit: hi is the type's maximum",              r.hi, (bits == 8) ? 255 : 65535)
    r.mask.close(); raw.close()
}

println ""
println "=== coverage, the cheap signal that a threshold went wrong ==="
// Measured BEFORE fill holes, because the question is what the threshold chose.
// The two failures it catches look identical in an ROI count -- the size filter
// turns "selected nothing" and "selected the whole frame" both into no nuclei.
def flat = { int v -> def ip = new ByteProcessor(100, 100); ip.setColor(v); ip.fill(); return new ImagePlus("f", ip) }
def discOnly = new ImagePlus("d", disc(30, 30, 40, new ByteProcessor(100, 100)))
def cov = RD.buildMask(discOnly, 1, 0.0d, "Otsu", false, false)
// pi*20^2 = 1257 of 10000 px, and Analyze Particles is not involved at all.
check("a single 40 px disc covers ~12.6%",
      Math.abs((cov.coverage as double) - 12.57d) < 0.6d, true)
check("...and the same disc with holes filled is unchanged here",
      Math.abs((RD.buildMask(discOnly, 1, 0.0d, "Otsu", true, false).coverage as double)
               - (cov.coverage as double)) < 0.001d, true)
cov.mask.close()

// Directly: coveragePct on a stack that is entirely on, and entirely off.
def allOn  = new ImagePlus("on",  { -> def ip = new ByteProcessor(10, 10); ip.setColor(255); ip.fill(); ip }())
def allOff = new ImagePlus("off", new ByteProcessor(10, 10))
check("coveragePct: all on  -> 100",           RD.coveragePct(allOn),  100.0d)
check("coveragePct: all off -> 0",             RD.coveragePct(allOff), 0.0d)

println ""
println "=== the degenerate end: a frame with nothing to separate ==="
// Found in Test_BatchRunner's log, where a pure binary fixture reported
// "255-255". That is correct there -- the discs ARE 255, so "only 255 counts"
// is the answer -- but it shows lo can reach the ceiling, and lo > hi would be
// a range that selects nothing while being reported as if it selected
// something. So: what happens when there is genuinely nothing to separate?
def flatImp = { int v ->
    def ip = new ByteProcessor(40, 40); ip.setColor(v); ip.fill()
    return new ImagePlus("flat" + v, ip)
}
// ignore_black and ignore_white zero the two end bins before the algorithm
// runs, so a frame whose pixels are ALL pure black or all saturated leaves the
// plugin an empty histogram and it throws ArrayIndexOutOfBoundsException.
//
// The OLD path swallowed that. Measured: IJ.run() goes through ImageJ's
// Executer, which catches the exception, logs it and returns -- leaving the
// image unthresholded, after which buildMask handed back the raw pixels AS IF
// they were a mask. On an all-255 frame that is 1600/1600 pixels "on". The
// check below is that the new path does not do this, so the oracle comparison
// above cannot be used here: the oracle is the thing that was wrong.
[255, 0].each { int v ->
    ["Otsu", "Default", "Huang2"].each { String m ->
        def f = flatImp(v)
        def r = RD.buildMask(f, 1, 0.0d, m, false, false)
        check("flat " + v + ", " + m + ": reported as 'none'", r.threshold, "none")
        check("  ...and nothing is selected",      r.coverage, 0.0d)
        check("  ...no lo/hi to paste anywhere",   [r.lo, r.hi], [null, null])
        r.mask.close(); f.close()
    }
}
// The old failure, stated as an assertion so it cannot come back: the mask of a
// uniform frame must not be the frame.
def f255 = flatImp(255)
def r255 = RD.buildMask(f255, 1, 0.0d, "Otsu", false, false)
int on255 = 0
for (int y = 0; y < 40; y++) for (int x = 0; x < 40; x++) if (r255.mask.getProcessor().get(x, y) != 0) on255++
check("an all-255 frame does NOT come back as a full mask", on255, 0)
r255.mask.close(); f255.close()

// A uniform frame that is neither black nor white is NOT degenerate -- the
// histogram has a bin, the algorithm runs, and nothing is above the threshold.
// Distinguishing the two matters: one is a broken input, the other is an empty
// field, and only the first should say "none".
def fMid = flatImp(120)
def rMid = RD.buildMask(fMid, 1, 0.0d, "Otsu", false, false)
check("a flat mid-grey frame gets a real threshold",
      rMid.threshold ==~ /\d+-\d+/, true)
check("...and still selects nothing",          rMid.coverage, 0.0d)
rMid.mask.close(); fMid.close()

println ""
println "=== to8BitMask ==="
def wide = new ImagePlus("w", { -> def ip = new ShortProcessor(4, 4); ip.set(0, 0, 65535); ip.set(1, 1, 7); ip }())
RD.to8BitMask(wide)
check("a 16-bit mask becomes 8-bit",           wide.getBitDepth(), 8)
check("...any non-zero value becomes 255",     [wide.getProcessor().get(0, 0), wide.getProcessor().get(1, 1)], [255, 255])
check("...and zero stays zero",                wide.getProcessor().get(2, 2), 0)

println ""
println "=== the method vocabulary comes from the plugin, not from a list ==="
def names = RD.methodNames()
check("Huang2 is available",                   names.contains("Huang2"), true)
check("...and the menu spelling Default",      names.contains("Default"), true)
check("...and MinError(I)",                    names.contains("MinError(I)"), true)
check("...and Manual, which is ours",          names.contains("Manual"), true)
// bilevel sits among the statics and is not an algorithm; offering it would be
// a method name that cannot work.
check("bilevel is not offered",                names.contains("bilevel"), false)

println ""
println "=== validateThreshold: before an image is opened, not after ==="
def threw2 = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() } }
check("a known method with no range is fine",  threw2 { RD.validateThreshold("Huang2", "") }, null)
check("an unknown method is refused",
      threw2 { RD.validateThreshold("Banana", "") }?.contains("unknown threshold method"), true)
// The mistake the batch would otherwise make a thousand times.
check("Manual with no range is refused",
      threw2 { RD.validateThreshold("Manual", "") }?.contains("needs nucleus_threshold_range"), true)
check("...and says why there is no default",
      threw2 { RD.validateThreshold("Manual", "") }?.contains("raw"), true)
check("an unreadable range is refused",
      threw2 { RD.validateThreshold("Manual", "abc") }?.contains("must be lo-hi"), true)
check("a backwards range is refused",
      threw2 { RD.validateThreshold("Manual", "200-100") }?.contains("low end above"), true)
check("a good manual range passes",            threw2 { RD.validateThreshold("Manual", "100-200") }, null)

println ""
println "=== Manual: the range is applied, both ends of it ==="
// The upper bound is the half that has to be proved: a threshold that ignored
// it would still select the discs and look right. probe(8) plants a saturated
// pixel at (1,0) for exactly this.
def mraw = probe(8)
def man  = RD.buildMask(mraw, 1, 0.0d, "Manual", false, false, [range: "100-200"])
check("Manual reports the range it was given", man.threshold, "100-200")
check("...as lo and hi",                       [man.lo, man.hi], [100, 200])
check("...the discs are selected",             man.mask.getStack().getProcessor(1).get(40, 40), 255)
check("...the background is not",              man.mask.getStack().getProcessor(1).get(110, 10), 0)
// The upper bound doing its job: 255 is outside [100, 200].
check("...and the SATURATED pixel is excluded",
      man.mask.getStack().getProcessor(1).get(1, 0), 0)
// Contrast with an open top end, where the same pixel is kept. Without this the
// check above could pass because the pixel was never selected by anything.
def manOpen = RD.buildMask(mraw, 1, 0.0d, "Manual", false, false, [range: "100-Infinity"])
check("...while an open top end keeps it",
      manOpen.mask.getStack().getProcessor(1).get(1, 0), 255)
check("...and Infinity is reported as the type maximum", manOpen.threshold, "100-255")
man.mask.close(); manOpen.mask.close(); mraw.close()

// A manual range no pixel falls in must say so through the coverage, not by
// looking like a successful run.
def mraw2 = probe(8)
def manNone = RD.buildMask(mraw2, 1, 0.0d, "Manual", false, false, [range: "230-250"])
check("a manual range that matches nothing reads 0%", manNone.coverage, 0.0d)
check("...and still reports the range asked for",     manNone.threshold, "230-250")
manNone.mask.close(); mraw2.close()

println ""
println "=== per-slice histograms: one threshold per slice, and it shows ==="
// probe() ramps the disc brightness with z on purpose, so a per-slice threshold
// MUST differ between slices. If it did not, this test would pass whether or
// not the per-slice branch ran at all.
def praw = probe(8)
def perS = RD.buildMask(praw, 1, 0.0d, "Default", false, false, [stackHistogram: false])
check("reported as a spread, not a range",     perS.threshold.startsWith("per-slice "), true)
check("...with no lo/hi to paste anywhere",    [perS.lo, perS.hi], [null, null])
def lohi = perS.threshold.replace("per-slice ", "").split("\\.\\.")
check("...and the slices really disagreed",    lohi[0] != lohi[1], true)
println "         (" + perS.threshold + ")"
// Every slice must be a mask. exec() thresholds only the CURRENT slice, so a
// missing setSlice() would leave slices 2..n as raw pixels -- which is exactly
// the failure the whole-stack default hides.
int rawSlices = 0
for (int z = 1; z <= perS.mask.getStackSize(); z++) {
    def ip = perS.mask.getStack().getProcessor(z)
    def vals = new HashSet<Integer>()
    for (int y = 0; y < 120; y++) for (int x = 0; x < 120; x++) vals << ip.get(x, y)
    if (!(vals.size() <= 2 && vals.every { it == 0 || it == 255 })) rawSlices++
}
check("every slice is binary, not just the first", rawSlices, 0)
// And it is a different answer from the pooled default, or the option is inert.
def pooled = RD.buildMask(praw, 1, 0.0d, "Default", false, false)
check("per-slice differs from the pooled default",
      perS.threshold == pooled.threshold, false)
perS.mask.close(); pooled.mask.close(); praw.close()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} buildMask check(s) failed")
