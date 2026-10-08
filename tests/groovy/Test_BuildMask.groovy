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
println "=== the threshold path against exec(): every method, both histograms ==="
// buildMask chooses the threshold itself now (RoiDetect.chooseThreshold), from
// a histogram it builds, through the plugin's per-method statics -- exec()'s own
// sequence without exec()'s int overflows. exec() is kept here, called exactly
// as buildMask used to call it, as the oracle: on stacks this small nothing
// overflows, so every method must select the same pixels and report the same
// range. The method list is the plugin's menu -- every name exec() dispatches.
// A narrower 16-bit range than probe()'s: Huang's cost grows with the square
// of the histogram's span (exec() runs the same static), and probe()'s 16-bit
// span of ~14,000 levels takes minutes per case. This one spans ~1,300 -- still
// a 65536-bin histogram, still trimmed, still with both end bins present.
def probeN = { int bits ->
    def st = new ImageStack(120, 120)
    (1..4).each { int z ->
        def ip = (bits == 8) ? new ByteProcessor(120, 120) : new ShortProcessor(120, 120)
        int bg = (bits == 8) ? 10 : 300
        int fg = (bits == 8) ? (90 + 25 * z) : (1000 + 150 * z)
        for (int y = 0; y < 120; y++) for (int x = 0; x < 120; x++) ip.set(x, y, bg + ((x * y) % 7))
        ip.setColor(fg); ip.fill(new OvalRoi(20, 20, 40, 40))
        ip.setColor(fg); ip.fill(new OvalRoi(70, 65, 30, 30))
        ip.set(0, 0, 0)
        ip.set(1, 0, (bits == 8) ? 255 : 65535)
        st.addSlice(ip)
    }
    def imp = new ImagePlus("probeN" + bits, st)
    imp.setDimensions(1, 4, 1)
    return imp
}
def EXEC_METHODS = ["Default", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean",
                    "MinError(I)", "Minimum", "Moments", "Otsu", "Percentile", "RenyiEntropy",
                    "Shanbhag", "Triangle", "Yen"]
def AT = fiji.threshold.Auto_Threshold
def execStack = { ImagePlus raw, String m ->
    def dup = new Duplicator().run(raw, 1, 1, 1, raw.getNSlices(), 1, 1)
    String rep
    try {
        def o = AT.newInstance().exec(dup, m, true, true, true, false, false, true)
        rep = ((o[0] as int) + 1) + "-" + (raw.getBitDepth() == 16 ? 65535 : 255)
    } catch (ArrayIndexOutOfBoundsException e) {
        for (int z = 1; z <= dup.getStackSize(); z++) dup.getStack().getProcessor(z).setValue(0d)
        for (int z = 1; z <= dup.getStackSize(); z++) dup.getStack().getProcessor(z).fill()
        rep = "none"
    }
    RD.to8BitMask(dup)
    [mask: dup, threshold: rep]
}
def execPerSlice = { ImagePlus raw, String m ->
    def dup = new Duplicator().run(raw, 1, 1, 1, raw.getNSlices(), 1, 1)
    def los = []
    for (int z = 1; z <= dup.getStackSize(); z++) {
        dup.setSlice(z)
        try {
            def o = AT.newInstance().exec(dup, m, true, true, true, false, false, false)
            los << ((o[0] as int) + 1)
        } catch (ArrayIndexOutOfBoundsException e) {
            RD.blankSlice(dup, z)
        }
    }
    RD.to8BitMask(dup)
    [mask: dup, threshold: los ? ("per-slice " + los.min() + ".." + los.max()) : "none"]
}
def disagreeExec = [], divided = []
EXEC_METHODS.each { String m ->
    [8, 16].each { int bits ->
        [true, false].each { boolean pooled ->
            def raw = probeN(bits)
            long c0 = System.currentTimeMillis()
            def want = pooled ? execStack(raw, m) : execPerSlice(raw, m)
            long c1 = System.currentTimeMillis()
            def got = RD.buildMask(raw, 1, 0.0d, m, false, false, [stackHistogram: pooled])
            long c2 = System.currentTimeMillis()
            if (c2 - c0 > 2000) println String.format("    (slow: %s %d-bit %s: exec %d ms, now %d ms)",
                                                       m, bits, pooled ? "pooled" : "per-slice", c1 - c0, c2 - c1)
            boolean same = java.util.Arrays.equals(bytesOf(want.mask), bytesOf(got.mask)) &&
                           want.threshold == got.threshold
            String label = m + " " + bits + "-bit " + (pooled ? "pooled" : "per-slice")
            // Where the counts had to be divided, exec()'s int arithmetic
            // overflowed and it is not an oracle there -- see the next section.
            if ((got.divisor ?: 1L) as long > 1L) {
                divided << (label + ": exec " + want.threshold + ", now " + got.threshold +
                            " (divisor " + got.divisor + ")")
            } else if (!same) {
                disagreeExec << (label + ": exec " + want.threshold + ", now " + got.threshold)
            }
            want.mask.close(); got.mask.close(); raw.close()
        }
    }
}
println "    compared " + (EXEC_METHODS.size() * 4) + " cases (" + EXEC_METHODS.size() + " methods x 8/16-bit x pooled/per-slice)"
check("wherever nothing was divided: same mask and range as exec()", disagreeExec, [])
divided.each { println "    divided, so exec() overflowed: " + it }
// MinError(I) forms value^2 x count per bin in int, so on a 16-bit histogram
// even this 120x120x4 image overflows it: exec() is wrong here, not the oracle.
check("...and the cases that had to be divided are MinError(I) 16-bit",
      divided.collect { it.split(":")[0] }.every { it.startsWith("MinError(I) 16-bit") } && divided.size() > 0, true)

// The two static names exec() does NOT dispatch: it compares the menu
// spelling, so "IJDefault" and "MinErrorI" fell through to no method and a
// threshold of (first non-empty bin - 1). methodNames() accepts them; now they
// mean their methods.
[["IJDefault", "Default"], ["MinErrorI", "MinError(I)"]].each { pair ->
    def raw = probe(16)
    def a = RD.buildMask(raw, 1, 0.0d, pair[0], false, false), b = RD.buildMask(raw, 1, 0.0d, pair[1], false, false)
    def viaExec = execStack(raw, pair[0])
    check(pair[0] + " is " + pair[1] + " now (exec gave " + viaExec.threshold + ")", a.threshold, b.threshold)
    a.mask.close(); b.mask.close(); viaExec.mask.close(); raw.close()
}

println ""
println "=== overflow: the counts are divided only as far as the method needs ==="
// What was bug 1 of note/known_issue.md, on a synthetic histogram of the kind a blurred
// tile merge gives: a huge dark peak and a dim tail, ~10^9 voxels. Every count
// is a multiple of 4096, so dividing by any power of two up to that is exact,
// and the same histogram at counts x16 (b16) is small enough for exact int
// arithmetic -- the reference. `naive` is the int route exec() took: the
// static on the raw counts.
def MAXI = (long) Integer.MAX_VALUE
long[] base = new long[256]
[[1, 9000], [2, 60000], [3, 120000], [4, 30000], [5, 4000]].each { base[it[0]] = it[1] }
(60..120).each { int v -> base[v] = Math.round(2000d * Math.exp(-Math.pow((v - 90) / 12d, 2))) + 1 }
def times = { long[] h, long k -> def o = new long[h.length]; for (int i = 0; i < h.length; i++) o[i] = h[i] * k; o }
long[] big = times(base, 4096L), b16 = times(base, 16L)
long nBig = big.sum() as long
long vcBig = 0L; for (int i = 0; i < 256; i++) vcBig += i * big[i]
println String.format("    big: %.2e voxels, sum value x count %.2e (int max %.2e)", nBig as double, vcBig as double, MAXI as double)
def toInt = { long[] h -> int[] o = new int[h.length]; for (int i = 0; i < h.length; i++) o[i] = (int) h[i]; o }
def naiveOf = { long[] h, String m ->
    // exec()'s own sequence, on int counts: end bins zeroed, trimmed, + offset
    long[] d = h.clone(); d[0] = 0; d[d.length - 1] = 0
    int lo = (0..<d.length).find { d[it] > 0 }, hi = (0..<d.length).findAll { d[it] > 0 }.max()
    long[] b = new long[hi - lo + 1]; System.arraycopy(d, lo, b, 0, b.length)
    RD.callMethod(m, toInt(b)) + lo
}
["Huang", "IsoData", "Li", "MinError(I)", "Otsu", "Triangle", "Default", "Mean"].each { String m ->
    def now = RD.chooseThreshold(big, m), ref = RD.chooseThreshold(b16, m)
    int naive = naiveOf(big, m)
    println String.format("    %-12s naive int %4d   now %4d (divisor %5d)   reference %4d (divisor %d)",
                          m, naive, now.t, now.divisor, ref.t, ref.divisor)
    check(m + ": the reference needed no division",       ref.divisor, 1L)
    check(m + ": equals the reference",                   now.t, ref.t)
}
// The test could fail: the naive route is wrong for the three that sum in int.
check("naive int route is wrong for Huang, IsoData, Li here",
      ["Huang", "IsoData", "Li"].every { naiveOf(big, it) != RD.chooseThreshold(b16, it).t }, true)
check("a method with no int product is not divided (Otsu)",  RD.chooseThreshold(big, "Otsu").divisor, 1L)

// MinError(I) forms value^2 x count per bin, so on a 16-bit range it needs
// dividing long before any sum overflows. A histogram spanning 0..4095:
long[] meHist = new long[65536]
(100..4000).each { int v -> meHist[v] = 64L * (v < 1000 ? 40 : 3) }
long[] meHistRef = new long[65536]; (0..<65536).each { meHistRef[it] = meHist[it].intdiv(64) }
def wNow = RD.chooseThreshold(meHist, "MinError(I)"), wRef = RD.chooseThreshold(meHistRef, "MinError(I)")
println "    16-bit MinError(I): now " + wNow.t + " (divisor " + wNow.divisor + "), counts/64 " + wRef.t +
        " (divisor " + wRef.divisor + "), naive " + naiveOf(meHist, "MinError(I)")
check("16-bit MinError(I) is divided for its square term", (wNow.divisor as long) > 1L, true)
check("...and equals the same histogram at exact small counts", wNow.t, wRef.t)

println ""
println "=== nucleus_threshold_scope is checked before an image is opened ==="
def scopeErr = { String scope, String m, boolean pooled ->
    try { RD.validateScope(scope, m, pooled); return null } catch (IllegalArgumentException e) { return e.getMessage() } }
check("frame and series are the scopes",           RD.THRESHOLD_SCOPES, ["frame", "series"])
check("series with a pooled histogram is fine",    scopeErr("series", "Otsu", true), null)
check("an unknown scope is refused",               scopeErr("stack", "Otsu", true)?.contains("must be one of frame, series"), true)
check("series with per-slice thresholds is refused", scopeErr("series", "Otsu", false)?.contains("choose one"), true)
check("...but not for Manual, which has no histogram", scopeErr("series", "Manual", false), null)

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
println "=== memory: in-place applyRange / to8BitMask / blank ==="

// These three used to build a whole replacement stack while still holding the
// original, so both were live at once. On an 11344 x 9590 x 25 tile merge that
// third copy is what ran the heap out. The risk
// in fixing it is silent: "identical output" can mean "correctly unchanged" or
// "the new code never ran", so each case below asserts BOTH the pixels and
// that the intended branch was taken.

// The pre-change implementations, kept as oracles -- the same trick the Auto
// Threshold macro call gets above. If these and the library ever disagree, the
// library changed behaviour, not just allocation.
def oldApplyRange = { ImagePlus im, int lo, int hi ->
    def s = im.getStack()
    def o = new ImageStack(im.getWidth(), im.getHeight())
    for (int z = 1; z <= s.getSize(); z++) {
        def ip = s.getProcessor(z)
        def bp = new ByteProcessor(im.getWidth(), im.getHeight())
        for (int y = 0; y < im.getHeight(); y++) {
            for (int x = 0; x < im.getWidth(); x++) {
                int v = ip.get(x, y)
                if (v >= lo && v <= hi) bp.set(x, y, 255)
            }
        }
        o.addSlice(s.getSliceLabel(z), bp)
    }
    im.setStack(o)
    return im
}
def oldTo8Bit = { ImagePlus im ->
    if (im.getBitDepth() == 8) { return im }
    def s = im.getStack()
    def o = new ImageStack(im.getWidth(), im.getHeight())
    for (int z = 1; z <= s.getSize(); z++) {
        def ip = s.getProcessor(z)
        def bp = new ByteProcessor(im.getWidth(), im.getHeight())
        for (int y = 0; y < im.getHeight(); y++) {
            for (int x = 0; x < im.getWidth(); x++) {
                if (ip.get(x, y) != 0) bp.set(x, y, 255)
            }
        }
        o.addSlice(s.getSliceLabel(z), bp)
    }
    im.setStack(o)
    return im
}

// Pixels that differ between two stacks, as a count -- never a boolean, so a
// failure says how wrong it is.
def diffPx = { ImagePlus a, ImagePlus b ->
    if (a.getStackSize() != b.getStackSize()) { return -1 }
    int n = 0
    for (int z = 1; z <= a.getStackSize(); z++) {
        def pa = a.getStack().getProcessor(z), pb = b.getStack().getProcessor(z)
        for (int y = 0; y < a.getHeight(); y++) {
            for (int x = 0; x < a.getWidth(); x++) if (pa.get(x, y) != pb.get(x, y)) n++
        }
    }
    return n
}

// A stack with real structure AND mid-grey background, so a pixel left
// unwritten by a missing else branch is visibly not zero.
def noisy = { int depth, int nz ->
    def st = new ImageStack(60, 40)
    def rnd = new java.util.Random(11)
    int top = (depth == 8) ? 255 : 65535
    for (int z = 1; z <= nz; z++) {
        def ip = (depth == 8) ? new ByteProcessor(60, 40) : new ShortProcessor(60, 40)
        for (int y = 0; y < 40; y++) {
            for (int x = 0; x < 60; x++) ip.set(x, y, 1 + rnd.nextInt(top))
        }
        ip.setColor(top); ip.fillOval(10 + z, 8, 20, 20)
        st.addSlice("z" + z, ip)
    }
    return new ImagePlus("n" + depth, st)
}

[8, 16].each { int depth ->
    int lo = (depth == 8) ? 200 : 50000
    int hi = (depth == 8) ? 255 : 65535

    def mine = noisy(depth, 3)
    def oracle = noisy(depth, 3)
    def keptStack = mine.getStack()
    RD.applyRange(mine, lo, hi)
    oldApplyRange(oracle, lo, hi)

    check("applyRange ${depth}-bit: same pixels as before", diffPx(mine, oracle), 0)
    check("applyRange ${depth}-bit: result is 8-bit",        mine.getBitDepth(), 8)
    check("applyRange ${depth}-bit: slices kept",            mine.getStackSize(), 3)

    // THE assertion that tells "unchanged" from "never ran". In place means the
    // stack instance survives; the old code always replaced it. Without this, an
    // early return on a mistyped bit-depth check would pass everything above.
    check("applyRange ${depth}-bit: in place iff 8-bit",
          mine.getStack().is(keptStack), depth == 8)

    // Only 0 and 255 may survive. This is the else-branch guard: drop the `: 0`
    // and the background keeps its original grey, which detect() would happily
    // threshold at 128-255 into plausible-looking ROIs.
    def seen = new HashSet<Integer>()
    for (int z = 1; z <= mine.getStackSize(); z++) {
        def ip = mine.getStack().getProcessor(z)
        for (int y = 0; y < 40; y++) for (int x = 0; x < 60; x++) seen << ip.get(x, y)
    }
    check("applyRange ${depth}-bit: only 0 and 255 remain", seen.sort(), [0, 255])
    mine.close(); oracle.close()
}

// to8BitMask: the 16-bit path must match the old one, and the 8-bit early
// return must still leave the stack untouched rather than forcing 255.
def wide16 = noisy(16, 2)
def wide16o = noisy(16, 2)
def wideKept = wide16.getStack()
RD.to8BitMask(wide16)
oldTo8Bit(wide16o)
check("to8BitMask 16-bit: same pixels as before",  diffPx(wide16, wide16o), 0)
check("to8BitMask 16-bit: now 8-bit",              wide16.getBitDepth(), 8)
check("to8BitMask 16-bit: stack replaced",         wide16.getStack().is(wideKept), false)
wide16.close(); wide16o.close()

def eight = noisy(8, 2)
def eightKept = eight.getStack()
int greyBefore = eight.getStack().getProcessor(1).get(0, 0)
RD.to8BitMask(eight)
check("to8BitMask 8-bit: returns early, untouched",
      eight.getStack().getProcessor(1).get(0, 0), greyBefore)
check("to8BitMask 8-bit: same stack instance",     eight.getStack().is(eightKept), true)
eight.close()

// blank(): 8-bit zeroes in place, 16-bit becomes an 8-bit empty mask.
[8, 16].each { int depth ->
    def bl = noisy(depth, 2)
    def blKept = bl.getStack()
    RD.blank(bl)
    long nonZero = 0
    for (int z = 1; z <= bl.getStackSize(); z++) {
        def ip = bl.getStack().getProcessor(z)
        for (int y = 0; y < 40; y++) for (int x = 0; x < 60; x++) if (ip.get(x, y) != 0) nonZero++
    }
    check("blank ${depth}-bit: nothing left on",     nonZero, 0L)
    check("blank ${depth}-bit: 8-bit result",        bl.getBitDepth(), 8)
    check("blank ${depth}-bit: slices kept",         bl.getStackSize(), 2)
    check("blank ${depth}-bit: in place iff 8-bit",  bl.getStack().is(blKept), depth == 8)
    bl.close()
}

println ""
println "=== memory: per-slice Gaussian blur ==="

// buildMask blurs one plane at a time rather than with IJ.run(..., "stack"),
// which parallelises over slices and holds one float plane per thread. "stack"
// mode is this same 2D blur applied per plane, so the pixels must not move.
double sg = 3.0d
def braw = noisy(8, 4)
def bref = noisy(8, 4)
IJ.run(bref, "Gaussian Blur...", "sigma=${sg} stack")       // the old path, by hand
def viaLib = RD.buildMask(braw, 1, sg,   "Manual", false, false, [range: "150-255"])
def viaRun = RD.buildMask(bref, 1, 0.0d, "Manual", false, false, [range: "150-255"])
check("per-slice blur == IJ.run stack blur", diffPx(viaLib.mask, viaRun.mask), 0)

// ...and the blur is not quietly skipped: with sigma off the mask must differ.
def bnone = noisy(8, 4)
def viaNone = RD.buildMask(bnone, 1, 0.0d, "Manual", false, false, [range: "150-255"])
check("sigma=0 gives a different mask",
      diffPx(viaLib.mask, viaNone.mask) > 0, true)
viaLib.mask.close(); viaRun.mask.close(); viaNone.mask.close()
braw.close(); bref.close(); bnone.close()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} buildMask check(s) failed")
