// Test_TiffAssembler.groovy
//
// Manifest rows -> TIFF. Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_TiffAssembler.groovy
//
// No data is needed; sources are synthesised by LuxFixture.

import ij.IJ
import ij.io.FileSaver

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "TiffAssembler.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def gcl = new GroovyClassLoader()
def TA  = gcl.parseClass(new File(LIBDIR + "/TiffAssembler.groovy"))
def LS  = gcl.parseClass(new File(LIBDIR + "/LuxendoScan.groovy"))
def SS  = gcl.parseClass(new File(LIBDIR + "/SampleSheet.groovy"))
def FIX = gcl.parseClass(new File("tests/groovy/LuxFixture.groovy"))
def asm = TA.load(LIBDIR)
def scanner = LS.load(LIBDIR)
def sheet = SS.load(LIBDIR)

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-54s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def checkNear = { String what, Double got, double want, double tol = 1e-5 ->
    boolean ok = (got != null) && Math.abs(got - want) <= tol
    println String.format("  %-6s %-54s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def throwsWith = { String what, String fragment, Closure body ->
    String msg = null
    try { body() } catch (Throwable e) { msg = e.getMessage() }
    boolean ok = msg != null && msg.contains(fragment)
    println String.format("  %-6s %-54s threw=%s", ok ? "ok" : "FAILED", what,
                          msg ? msg.readLines()[0] : "(nothing)")
    ok ? passed++ : failed++
}

def tmp = new File(System.getProperty("java.io.tmpdir"), "test_tiffassembler_" + System.nanoTime())
tmp.mkdirs()

println "=== the classic TIFF ceiling is the Bio-Formats constant, not 2^32 ==="
check("CLASSIC_TIFF_MAX", TA.CLASSIC_TIFF_MAX, 4183818240L)
check("2^32 would be wrong", TA.CLASSIC_TIFF_MAX < 4294967296L, true)
check("just under fits",   TA.fitsClassicTiff(4183818239L), true)
check("exactly at does not", TA.fitsClassicTiff(4183818240L), false)

println "\n=== size is predicted from the manifest, no trial write ==="
// One real Luxendo position, all four time points in one file, is the case the
// plan sizes: 2048 x 2048 x 39 x 3 channels x 2 bytes.
def big = (0..2).collect { [size_x: 2048, size_y: 2048, size_z: 39, channel: it] }
check("3 channels of 2048x2048x39", TA.predictBytes(big, 100), 2L * 2048 * 2048 * 39 * 3)
check("that fits classic TIFF",     TA.fitsClassicTiff(TA.predictBytes(big, 100)), true)
check("half scale is a quarter",    TA.predictBytes(big, 50), TA.predictBytes(big, 100) / 4)
check("scaled dimension",           TA.scaled(2048, 50), 1024)
// ROUNDED, not floored: 33% of 2048 is 675.84, which is 676 pixels.
check("scale is rounded, not floored", TA.scaled(2048, 33), 676)
check("scaled never reaches zero",  TA.scaled(3, 1), 1)
check("100% is untouched",          TA.scaled(2047, 100), 2047)

println "\n=== the scale is validated in code, as a percentage of the original ==="
check("blank is full resolution", TA.checkScalePercent(null), 100)
check("empty is full resolution", TA.checkScalePercent(""), 100)
check("50 is 50",                 TA.checkScalePercent("50"), 50)
throwsWith("zero refuses",     "between 1 and 100", { TA.checkScalePercent(0) })
throwsWith("negative refuses", "between 1 and 100", { TA.checkScalePercent(-5) })
// Refusing to ENLARGE is worth stating on its own: an upscaled output would be
// invented pixels wearing a real calibration.
throwsWith("above 100 refuses", "does not enlarge", { TA.checkScalePercent(200) })
throwsWith("nonsense refuses",  "must be a number", { TA.checkScalePercent("half") })

println "\n=== the format name is validated in code, not by the dialog ==="
check("default",  TA.checkFormat(null), "tiff")
check("bigtiff",  TA.checkFormat("BigTIFF"), "bigtiff")
throwsWith("an unknown format refuses", "format must be one of", { TA.checkFormat("ome") })
// A #@ String with choices={} is NOT validated on the command line, so a stale
// caller passing "true" must not quietly select a format.
throwsWith("a stale boolean refuses", "format must be one of", { TA.checkFormat("true") })

println "\n=== output naming says when the pixels are not full resolution ==="
check("plain",        TA.outputName("a.tif", 100, "tiff"),    "a.tif")
check("bigtiff ext",  TA.outputName("a.tif", 100, "bigtiff"), "a.ome.tif")
check("scale token",  TA.outputName("a.tif", 25, "tiff"),     "a_downscale25pc.tif")

println "\n=== a real assembly round trip ==="
def root = FIX.buildTree(new File(tmp, "acq"),
    [[stack: 0, desc: "pos1", nz: 3], [stack: 1, desc: "pos2", nz: 1]],
    [[index: 0, name: "BF"], [index: 1, name: "GFP"]], 2, 16, 12)
def scanRes = scanner.scan(root) { }
def rows = scanRes.sources
def out  = new File(tmp, "out")
def sums = asm.assembleAll(rows, root, out, [verify: true]) { }
check("one summary per output", sums.size(), 4)
check("all written",            sums.collect { it.status }.unique(), ["written"])
check("all verified",           sums.collect { it.verified }.unique(), ["yes"])

def s0 = sums.find { it.series_id.contains("pos1") && it.t == 0 }
def r0 = sheet.inspect(new File(out, s0.output_path))[0]
check("channels",    r0.size_c, 2)
check("slices",      r0.size_z, 3)
check("width",       r0.size_x, 12)
check("height",      r0.size_y, 16)
checkNear("pixel width", r0.pixel_width as Double, 0.208d)
checkNear("z step",      r0.pixel_depth as Double, 5.0d)

println "\n=== a single-plane position keeps a blank z step ==="
def sFlat = sums.find { it.series_id.contains("pos2") && it.t == 0 }
def rFlat = sheet.inspect(new File(out, sFlat.output_path))[0]
check("slices",      rFlat.size_z, 1)
check("pixel_depth", rFlat.pixel_depth, null)

println "\n=== resizing scales the CALIBRATION, not only the pixels ==="
// Halve the pixels and the pixel size must double, or every area is out by the
// square of the factor while the image looks perfect.
def outR = new File(tmp, "out_ds")
def sR = asm.assembleAll(rows, root, outR, [scalePercent: 50]) { }
def sR0 = sR.find { it.series_id.contains("pos1") && it.t == 0 }
def rR = sheet.inspect(new File(outR, sR0.output_path))[0]
check("width halved",   rR.size_x, 6)
check("height halved",  rR.size_y, 8)
check("slices unchanged", rR.size_z, 3)
checkNear("pixel width DOUBLED", rR.pixel_width as Double, 0.416d)
checkNear("z step unchanged",    rR.pixel_depth as Double, 5.0d)
check("physical width preserved",
      Math.abs((rR.size_x * (rR.pixel_width as double)) - (r0.size_x * (r0.pixel_width as double))) < 1e-6, true)
check("filename says it is downscaled", sR0.output_path.contains("_downscale50pc"), true)
check("and the checksum differs from full resolution", sR0.checksum != s0.checksum, true)

println "\n=== channels go in by their metadata index, not directory order ==="
def imp = IJ.openImage(new File(out, s0.output_path).getAbsolutePath())
check("stack is c-fastest", imp.getStack().getSliceLabel(1).contains("BF"), true)
check("second slice is channel 2", imp.getStack().getSliceLabel(2).contains("GFP"), true)
imp.close(); imp.flush()

println "\n=== over the limit is a per-output failure, not an aborted run ==="
// A manifest claiming a stack too large for classic TIFF. No pixels are read:
// the refusal happens on the prediction, before anything is opened.
def huge = (0..2).collect { c ->
    [series_id: "huge", t: 0, channel: c,
     channel_name: "c" + c, source_path: "nope.lux.h5", size_x: 4096, size_y: 4096,
     size_z: 100, pixel_width: 0.1, pixel_height: 0.1, pixel_depth: 1.0,
     pixel_unit: "micron"]
}
def hugeSum = asm.assembleOne(huge, root, out, [format: "tiff"])
check("status",        hugeSum.status, "failed")
check("reason names the fix", hugeSum.reason.contains("format=bigtiff"), true)
check("nothing written", new File(out, "huge.tif").exists(), false)
// and the rest of the run still happens
def mixed = rows + huge
def mixedSums = asm.assembleAll(mixed, root, new File(tmp, "mixed"), [:]) { }
check("one failure does not stop the others",
      mixedSums.count { it.status == "written" }, 4)
check("the failure has its own row", mixedSums.count { it.status == "failed" }, 1)
check("summary is rectangular",
      mixedSums.collect { it.keySet() }.unique().size(), 1)

println "\n=== bigtiff carries the same pixels and keeps its calibration ==="
def outB = new File(tmp, "out_big")
def sB = asm.assembleAll(rows, root, outB, [format: "bigtiff"]) { }
def sB0 = sB.find { it.series_id.contains("pos1") && it.t == 0 }
check("same pixel checksum as classic", sB0.checksum, s0.checksum)
def rB = sheet.inspect(new File(outB, sB0.output_path))[0]
check("channels", rB.size_c, 2)
check("slices",   rB.size_z, 3)
checkNear("pixel width", rB.pixel_width as Double, 0.208d)
checkNear("z step",      rB.pixel_depth as Double, 5.0d)

println "\n=== include is honoured, and a half-included output is a question ==="
// include is a SERIES property now, handed in as a map, so "some channels of
// this output are excluded" is no longer a state that can be expressed.
def offSum = asm.assembleAll(rows, root, new File(tmp, "off"),
                             [includeBySeries: [(s0.series_id): "false"]]) { }
check("skipped", offSum.find { it.output_path == s0.output_path }.status, "skipped")
check("reason",  offSum.find { it.output_path == s0.output_path }.reason, "include=false")
check("the others still ran", offSum.count { it.status == "written" }, sums.size() - 1)
// A series absent from the map defaults to included, so a trimmed series table
// cannot silently skip everything.
def noMapSum = asm.assembleAll(rows, root, new File(tmp, "nomap"), [:]) { }
check("absent from the map means included",
      noMapSum.collect { it.status }.unique(), ["written"])

println "\n=== gathering every time frame of a position into one file ==="
// The MANIFEST decides this, not the assembler -- so the gathered plan comes
// from a second scan, and the assembler just builds what the table describes.
def gRows = scanner.scan(root, [gatherFrames: true]) { }.sources
check("same number of sources",     gRows.size(), rows.size())
check("one output per position",    gRows.collect { it.series_id }.unique().size(), 2)
check("the series id carries no t", gRows[0].series_id.contains("_t0"), false)
check("but the rows still do",      gRows.collect { it.t }.unique().sort(), [0, 1])

def outG = new File(tmp, "out_gather")
def sG = asm.assembleAll(gRows, root, outG, [verify: true]) { }
check("one output per position", sG.size(), 2)
check("all written",             sG.collect { it.status }.unique(), ["written"])
check("all verified",            sG.collect { it.verified }.unique(), ["yes"])
def sG0 = sG.find { it.series_id.contains("pos1") }
check("the summary counts the frames", sG0.frames, 2)

def rG = sheet.inspect(new File(outG, sG0.output_path))[0]
check("channels", rG.size_c, 2)
check("slices",   rG.size_z, 3)
check("FRAMES",   rG.size_t, 2)
checkNear("pixel width survives", rG.pixel_width as Double, 0.208d)
checkNear("z step survives",      rG.pixel_depth as Double, 5.0d)

// The pixels must be the SAME pixels, in the same order, as the per-timepoint
// files hold -- a gathered stack of the right shape built from the wrong planes
// would pass every check above. TA.toBytes is the assembler's own serialiser,
// so this compares what the two files contain, not how they were written.
def crcOf = { File f ->
    def crc = new java.util.zip.CRC32()
    def imp2 = IJ.openImage(f.getAbsolutePath())
    try {
        def stk = imp2.getStack()
        for (int i = 1; i <= stk.getSize(); i++) {
            crc.update(TA.toBytes((short[]) stk.getProcessor(i).getPixels()))
        }
    } finally { imp2.close(); imp2.flush() }
    return crc.getValue()
}
def joined = crcOf(new File(outG, sG0.output_path))
def expectCrc = new java.util.zip.CRC32()
sums.findAll { it.series_id.startsWith(sG0.series_id) }.sort { it.t }.each { st ->
    def imp2 = IJ.openImage(new File(out, st.output_path).getAbsolutePath())
    try {
        def stk = imp2.getStack()
        for (int i = 1; i <= stk.getSize(); i++) {
            expectCrc.update(TA.toBytes((short[]) stk.getProcessor(i).getPixels()))
        }
    } finally { imp2.close(); imp2.flush() }
}
check("gathered pixels are the per-timepoint pixels, in order",
      Long.toHexString(joined), Long.toHexString(expectCrc.getValue()))

// A position whose z changes between timepoints cannot be one hyperstack, and
// that is FATAL when gathering where it is only a warning when not.
def ragged = gRows.collect { new LinkedHashMap(it) }
ragged.findAll { it.series_id.contains("pos1") && it.t == 1 }.each { it.size_z = 2 }
def raggedSeries = ragged.collect { it.series_id }.unique().collect { [prefix: it] }
throwsWith("z changing between frames refuses", "disagree on dimensions",
           { scanner.validate(raggedSeries, ragged) { } })

println "\n=== skipExisting resumes a run without redoing it ==="
def skipDir = new File(tmp, "skip")
def firstPass = asm.assembleAll(rows, root, skipDir, [skipExisting: true]) { }
check("nothing there yet, so all written", firstPass.collect { it.status }.unique(), ["written"])
def secondPass = asm.assembleAll(rows, root, skipDir, [skipExisting: true]) { }
check("second pass skips them all", secondPass.collect { it.status }.unique(), ["skipped"])
check("and says why",               secondPass.collect { it.reason }.unique(), ["already assembled"])
// Off, it does the work again -- the proof that the skip is what changed the
// behaviour and not some other refusal.
def forced = asm.assembleAll(rows, root, skipDir, [skipExisting: false]) { }
check("off, they are written again", forced.collect { it.status }.unique(), ["written"])
check("byte-identical the second time",
      forced.collect { it.checksum }, firstPass.collect { it.checksum })

// A TIFF without its provenance file is what a run killed mid-write leaves.
// Keeping that would be keeping a file nobody can vouch for.
def halfDone = secondPass[0]
def orphaned = new File(skipDir, halfDone.output_path.replace(".tif", "_gather.txt"))
check("provenance was there", orphaned.delete(), true)
def resumed = asm.assembleAll(rows, root, skipDir, [skipExisting: true]) { }
check("the unvouched-for output is redone",
      resumed.find { it.output_path == halfDone.output_path }.status, "written")
check("the complete ones are still skipped",
      resumed.count { it.status == "skipped" }, secondPass.size() - 1)

// The skip is keyed on the name actually written, and the name carries the
// scale -- so asking for a downscale in a directory full of full-resolution
// files must not skip them all.
def dsPass = asm.assembleAll(rows, root, skipDir, [skipExisting: true, scalePercent: 50]) { }
check("a downscale is not mistaken for the full-size file",
      dsPass.collect { it.status }.unique(), ["written"])
check("and lands under its own name",
      dsPass.every { it.output_path.contains("_downscale50pc") }, true)

println "\n=== verification catches a changed pixel ==="
def victim = new File(out, s0.output_path)
def vImp = IJ.openImage(victim.getAbsolutePath())
def proc = vImp.getStack().getProcessor(1)
proc.set(0, 0, proc.get(0, 0) + 1)
new FileSaver(vImp).saveAsTiffStack(victim.getAbsolutePath())
vImp.close(); vImp.flush()
def v = asm.verifyOne(out, s0)
check("one pixel changed is caught", v.ok, false)
check("and the reason names the checksum", v.reason.contains("pixel checksum"), true)

println "\n=== provenance travels with the output ==="
def prov = new File(out, s0.output_path.replace(".tif", "_gather.txt"))
check("written beside the image", prov.exists(), true)
def pm = [:]
prov.eachLine { l -> def p = l.split("\t", 2); if (p.size() == 2) pm[p[0]] = p[1] }
check("names its own output", pm.output_path, s0.output_path)
check("records the checksum",  pm.pixel_checksum, s0.checksum)
check("records the version",   pm.gatherer_version != null && !pm.gatherer_version.isEmpty(), true)
check("names every source",    (0..1).every { pm["channel_${it}_source"]?.endsWith(".lux.h5") }, true)
check("records the scale",     pm.scale_percent, "100")
check("records the frame count", pm.frames, "1")

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
