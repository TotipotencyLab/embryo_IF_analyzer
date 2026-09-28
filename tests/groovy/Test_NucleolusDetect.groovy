// Test_NucleolusDetect.groovy
//
// The nucleolus threshold, which now asks Landini's Auto Threshold for its
// algorithms instead of ImageJ's own ij.process.AutoThresholder enum.
//
// The point of this file is the ORACLE section: the enum is kept here, in the
// test, and the two are required to agree. That is the whole basis for claiming
// the switch changes no existing result -- the nucleus path and the nucleolus
// path had two different method vocabularies, and unifying them must not be
// paid for with different numbers.
//
// Images and histograms are synthesised, so this needs no fixture. Run headless
// from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_NucleolusDetect.groovy

import ij.ImagePlus
import ij.ImageStack
import ij.gui.OvalRoi
import ij.process.ByteProcessor
import ij.process.AutoThresholder

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "NucleolusDetect.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def ND = new GroovyClassLoader().parseClass(new File(LIBDIR + "/NucleolusDetect.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-56s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}

println "=== ij.process.AutoThresholder is kept as the ORACLE ==="
// Three histogram shapes, because one would not be evidence. The weak one is
// the documented real case: restricted to a single nucleus the histogram stops
// being strongly bimodal, which is why Otsu and Triangle misbehave there.
def hists = [:]
def gauss = { int[] h, double centre, double width, double height ->
    (0..255).each { int i -> h[i] += (int) (height * Math.exp(-Math.pow((i - centre) / width, 2))) }
    return h
}
hists["bimodal"] = gauss(gauss(new int[256], 60d, 14d, 900d), 165d, 26d, 2600d)
hists["weak"]    = gauss(gauss(new int[256], 95d, 18d, 260d), 150d, 34d, 2400d)
def rnd = new Random(42)
def flatH = new int[256]
(0..255).each { int i -> flatH[i] = 100 + rnd.nextInt(40) }
hists["flat"] = flatH

// Exactly the methods the dialog offered BEFORE the switch. Those are the ones
// a previous run could have used, so those are the ones that must not move.
["Default", "Otsu", "Triangle", "Huang", "IsoData"].each { String m ->
    hists.each { String hname, int[] h ->
        int oracle = new AutoThresholder().getThreshold(AutoThresholder.Method.valueOf(m), h)
        check(m + " on the " + hname + " histogram", ND.landiniBin(m, h), oracle)
    }
}

println ""
println "=== and the gap that the switch closes ==="
// Huang2 is the NUCLEUS default. Before this, putting it in nucleolus_threshold
// threw from Method.valueOf() -- the same word meant something in one field and
// nothing in the other.
def enumHasHuang2 = AutoThresholder.Method.values().any { it.toString() == "Huang2" }
check("Huang2 is absent from ImageJ's enum",   enumHasHuang2, false)
int h2 = ND.landiniBin("Huang2", hists["bimodal"])
check("...and is now a usable nucleolus method", h2 > 0 && h2 < 256, true)
def threw = { Closure c -> try { c(); return null } catch (Throwable t) { return t.getMessage() } }
check("an unknown method names itself",
      threw { ND.landiniBin("Banana", hists["flat"]) }?.contains("Banana"), true)
check("...and says what to use instead",
      threw { ND.landiniBin("Banana", hists["flat"]) }?.contains("Relative"), true)

println ""
println "=== Relative computes no histogram at all ==="
// The default, and the one that transfers across datasets. It must stay
// independent of the algorithm dispatch above.
def ip = new ByteProcessor(60, 60)
ip.setColor(200); ip.fill()
ip.setColor(100); ip.fill(new OvalRoi(20, 20, 20, 20))
def roi = new OvalRoi(5, 5, 50, 50)
double t = ND.nucleolusThreshold(ip, roi, "Relative", 0.6d)
def stats = ij.process.ImageStatistics.getStatistics(ip, ij.measure.Measurements.MEAN, null)
ip.setRoi(roi)
def roiStats = ij.process.ImageStatistics.getStatistics(ip, ij.measure.Measurements.MEAN, null)
check("Relative is mean x fraction",
      String.format("%.4f", t), String.format("%.4f", roiStats.mean * 0.6d))
check("...and a different fraction moves it",
      ND.nucleolusThreshold(ip, roi, "Relative", 0.3d) < t, true)

println ""
println "=== buildNucleolusMask: dark inside a nucleus, nothing outside it ==="
// A bright disc with a dark blob in it, and a second bright disc with none.
// Nucleoli are DARK in DAPI, so the dark blob is what must be selected -- and
// nothing beyond the ROI, which is the failure the macro version had.
def st = new ImageStack(160, 80)
def dna = new ByteProcessor(160, 80)
dna.setColor(30);  dna.fill()                                  // background
dna.setColor(200); dna.fill(new OvalRoi(10, 10, 60, 60))       // nucleus 1
dna.setColor(90);  dna.fill(new OvalRoi(30, 30, 20, 20))       // its nucleolus
dna.setColor(200); dna.fill(new OvalRoi(90, 10, 60, 60))       // nucleus 2, solid
st.addSlice(dna)
def imp = new ImagePlus("dna", st)
def nuc1 = new OvalRoi(10, 10, 60, 60), nuc2 = new OvalRoi(90, 10, 60, 60)
def mask = ND.buildNucleolusMask(imp, [nuc1, nuc2], [1, 1], 0.0d, "Relative", 0.6d)
def mp = mask.getProcessor()
check("the dark blob is selected",             mp.get(40, 40), 255)
check("the nucleoplasm around it is not",      mp.get(18, 40), 0)
check("the background is never touched",       mp.get(80, 5), 0)
check("a nucleus with no dark region stays empty",
      (0..59).every { int dy -> (0..59).every { int dx -> mp.get(90 + dx, 10 + dy) == 0 } }, true)

// The same image through an ALGORITHM rather than Relative, to prove the
// dispatch is reachable from the real entry point and not only from landiniBin.
def maskD = ND.buildNucleolusMask(imp, [nuc1], [1], 0.0d, "Huang2", 0.6d)
check("Huang2 runs end to end",                maskD.getProcessor().get(40, 40), 255)
mask.close(); maskD.close(); imp.close()

println ""
println "passed: ${passed}   FAILED: ${failed}"
if (failed > 0) throw new AssertionError("${failed} nucleolus check(s) failed")
