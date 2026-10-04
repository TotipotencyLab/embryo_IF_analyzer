// Test_SeriesSource.groovy
//
// A series handed over one frame at a time, and the batch reading Luxendo
// through it. Run headless from the repo root:
//
//   /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
//     --run tests/groovy/Test_SeriesSource.groovy
//
// No data is needed: the acquisition is synthesised by LuxFixture, with discs
// to segment. They move frame by frame and the second channel brightens, so a
// frame read or measured as another cannot pass.
import ij.IJ
import ij.ImagePlus
import ij.ImageStack
import ij.gui.OvalRoi
import ij.io.FileSaver
import ij.process.ShortProcessor

def LIBDIR = new File("scripts/groovy").getAbsolutePath()
if (!new File(LIBDIR, "SeriesSource.groovy").exists()) {
    throw new IllegalStateException("run from the repository root; no scripts/groovy at " + LIBDIR)
}
def gcl = new GroovyClassLoader()
def SS  = gcl.parseClass(new File(LIBDIR + "/SeriesSource.groovy"))
def LFC = gcl.parseClass(new File(LIBDIR + "/LuxendoFile.groovy"))
def LS  = gcl.parseClass(new File(LIBDIR + "/LuxendoScan.groovy"))
def TA  = gcl.parseClass(new File(LIBDIR + "/TiffAssembler.groovy"))
def BR  = gcl.parseClass(new File(LIBDIR + "/BatchRunner.groovy"))
def NP  = gcl.parseClass(new File(LIBDIR + "/NucleusPipeline.groovy"))
def TSV = gcl.parseClass(new File(LIBDIR + "/Tsv.groovy"))
def FIX = gcl.parseClass(new File("tests/groovy/LuxFixture.groovy"))

int passed = 0, failed = 0
def check = { String what, Object got, Object want ->
    boolean ok = (got == want)
    println String.format("  %-6s %-58s got=%s want=%s", ok ? "ok" : "FAILED", what, got, want)
    ok ? passed++ : failed++
}
def throwsWith = { String what, String fragment, Closure body ->
    String msg = null
    try { body() } catch (Throwable e) { msg = e.getMessage() }
    boolean ok = msg != null && msg.contains(fragment)
    println String.format("  %-6s %-58s threw=%s", ok ? "ok" : "FAILED", what,
                          msg ? msg.readLines()[0] : "(nothing)")
    ok ? passed++ : failed++
}
def tmp = new File(System.getProperty("java.io.tmpdir"), "test_seriessource_" + System.nanoTime())
tmp.mkdirs()

// --- the acquisition ----------------------------------------------------------
// One position, 3 slices, 2 channels, 3 time points, 120 x 160. Channel 0 (DNA)
// holds two discs that move 5 px a frame; channel 1 is flat at 100(t+1) + 10z,
// so its Mean names the frame and the slice that were measured.
int NY = 120, NX = 160
def inDisc = { int y, int x, int cy, int cx, int r -> (y - cy) * (y - cy) + (x - cx) * (x - cx) <= r * r }
def pixel = { Map pos, Map ch, int t, int z, int y, int x ->
    if ((ch.index as int) == 0) {
        return (inDisc(y, x, 50, 40 + 5 * t, 18) || inDisc(y, x, 70 + 5 * t, 115, 18)) ? 2000 : 100
    }
    return 100 * (t + 1) + 10 * z
}
def root = FIX.buildTree(new File(tmp, "acq"), [[stack: 0, desc: "pos1", nz: 3]],
                         [[index: 0, name: "DNA"], [index: 1, name: "GFP"]], 3, NY, NX, true,
                         [pixel   : pixel,
                          metaData: [triggers: [[type: "StartIntervalRepeats", interval_s: 1800, repeats: 3]]]])
def scanRes = LS.load(LIBDIR).scan(root) { }
def seriesRows = scanRes.series
def sid = seriesRows[0].series_id as String
def srcRows = LS.withSeriesId(scanRes.sources, seriesRows).sources

println "=== a Luxendo series, one frame at a time ==="
def src = SS.ofLuxendo(SS.typed(srcRows), root, sid, LFC)
check("frames are the sources' t, from 1",        src.frameList, [1, 2, 3])
check("nFrames",                                   src.nFrames, 3)
check("channels, slices",                          [src.nChannels, src.nSlices], [2, 3])
check("size",                                      [src.width, src.height], [NX, NY])
check("titled with the series id",                 src.title, sid)
check("pixel width from the sources table",        src.calibration.pixelWidth, 0.208d)
check("z step from the sources table",             src.calibration.pixelDepth, 5.0d)
check("frame interval from the acquisition JSON",  src.frameInterval, 1800d)
check("...in seconds",                             src.frameUnit, "sec")
check("method",                                    src.method, "luxendo")

def f2 = src.frame(2)
check("a frame is one frame",                      [f2.getNChannels(), f2.getNSlices(), f2.getNFrames()], [2, 3, 1])
check("...labelled as a one-time-point TIFF is",   f2.getStack().getSliceLabel(1), "c:1/2 z:1/3 - DNA")
// t = 2 is the file Cam_long_00001 (from 0): GFP there is 100 * 2 + 10 z.
check("frame 2 is time point 2's pixels",
      (1..3).collect { z -> f2.getStack().getProcessor(f2.getStackIndex(2, z, 1)).get(5, 5) }, [200, 210, 220])
check("...calibrated",                             f2.getCalibration().pixelWidth, 0.208d)

// The same frame through Make_LuxendoTiff's writer: same planes, same labels,
// in the same order. Two readers of one format agreeing is the guard against
// the fork the plan warns of.
def tifOut = new File(tmp, "tif"); tifOut.mkdirs()
def asm = TA.load(LIBDIR)
def sT = asm.assembleAll(srcRows, root, tifOut, [frames: "2", verify: true]) { }
def tif = IJ.openImage(new File(tifOut, sT[0].output_path).getAbsolutePath())
def planes = { ImagePlus imp -> (1..imp.getStackSize()).collect { i ->
    [imp.getStack().getSliceLabel(i), java.util.Arrays.hashCode((short[]) imp.getStack().getPixels(i))] } }
check("frame 2 equals Make_LuxendoTiff's _t0002, plane by plane", planes(f2), planes(tif))
tif.close(); tif.flush()
src.release(f2)

throwsWith("a frame the series lacks is named",    "has no frame t=5", { src.frame(5) })
check("choose: a selection, kept to what exists",  src.choose([2, 7]), [frames: [2], absent: [7]])
check("choose: blank is every frame",              src.choose(null).frames, [1, 2, 3])
check("describeFrames",                            [SS.describeFrames([1, 2, 3, 4]), SS.describeFrames([2, 11, 21])],
                                                    ["1-4", "2, 11, 21"])

// A frame missing a channel fails ON ITS OWN, not the series.
def ragged = SS.typed(srcRows).findAll { !((it.t as int) == 3 && (it.channel as int) == 2) }
def srcR = SS.ofLuxendo(ragged, root, sid, LFC)
check("the series still opens",                    srcR.frameList, [1, 2, 3])
throwsWith("the frame without its channel refuses", "must bring every channel", { srcR.frame(3) })
def ok1 = srcR.frame(1)
check("...and the others are unaffected",          ok1.getNChannels(), 2)
srcR.release(ok1)

def shaped = SS.typed(srcRows).collect { new LinkedHashMap(it) }
shaped.find { (it.t as int) == 2 }.size_z = 4
throwsWith("one series, one volume shape",         "disagree on (x, y, z)",
           { SS.ofLuxendo(shaped, root, sid, LFC) })

println "\n=== an open image is a source too ==="
def st = new ImageStack(20, 10)
(1..2).each { t -> st.addSlice("p" + t, new ShortProcessor(20, 10)) }
def two = new ImagePlus("two", st); two.setDimensions(1, 1, 2)
def srcI = SS.ofImage(two, "", false)
def one = srcI.frame(1)
check("a frame of a multi-frame image is a copy",  one.is(two), false)
check("...titled as the image, not DUP_",          one.getTitle(), "two")
srcI.release(one)
def single = new ImagePlus("single", new ShortProcessor(20, 10))
def srcS = SS.ofImage(single, "", false)
check("a single-frame image is handed over as itself", srcS.frame(1).is(single), true)
srcS.release(single)
check("...and release never closes the caller's image", single.getProcessor() != null, true)
check("a single frame ignores a selection",        srcS.choose([4]).frames, [1])

println "\n=== the batch reads Luxendo through the sources table ==="
// One Luxendo row and one TIFF row in ONE sheet: membership decides which is
// which. The TIFF is frame 1 of the acquisition, written as a file.
def tifRow = [series_id: "tif1", include: "true", alias: "tif1", series_index: "0", series_name: "tif1",
              path: new File(tifOut, sT[0].output_path).getAbsolutePath()]
def sheet = seriesRows.collect { new LinkedHashMap(it) } + [tifRow]
def params = NP.fromConfig([:]) + [
    script_name: "Test_SeriesSource", dna_channel: 1, channels_measured: "1,2",
    nucleus_blur_sigma: 0.0d, nucleus_threshold: "Otsu", nucleus_particle_size: "5-Infinity",
    nucleoli_enabled: false, save_overview: false, open_mode: "auto"]
// The scan's rows as text, as the front end hands them over after reading sources.tsv.
params.sources = scanRes.sources.collect { r -> r.collectEntries { k, v -> [k, v == null ? "" : v.toString()] } }
def runner = BR.load(LIBDIR)
def out = new File(tmp, "batch")
def res = runner.run(sheet, null, params + [frames: "2-3"], out) { }
def sum = TSV.read(new File(out, "batch_summary.tsv"))
check("summary: one row per frame, and the TIFF's",
      sum.collect { [it.series_id, it.t, it.status, it.open_method] },
      [[sid, "2", "ok", "luxendo"], [sid, "3", "ok", "luxendo"], ["tif1", "1", "ok", "importer"]])
check("summary columns",                           sum[0].keySet().toList(),
      ["series_id", "t", "path", "series_index", "status", "open_method",
       "threshold", "mask_pct", "n_nucleus", "n_nucleolus", "seconds", "message"])
check("two discs x 3 slices in each frame",        sum.collect { it.n_nucleus }, ["6", "6", "6"])

def readTsv = { File f ->
    def lines = f.readLines()
    def head = lines[0].split("\t", -1).toList()
    lines.drop(1).collect { l -> def v = l.split("\t", -1); def m = [:]
        head.eachWithIndex { h, i -> m[h] = (i < v.length ? v[i] : "") }; m }
}
def oL = readTsv(new File(out, sid + "_nucleus_outline.txt"))
check("t is the TIME POINT, not the frame's place in the run",
      oL.collect { it.t }.unique(), ["2", "3"])
check("ids carry TTTT-, the time point",
      oL.collect { (it.roi =~ /^nucleus_(\d{4})-/)[0][1] }.unique(), ["0002", "0003"])
def rL = readTsv(new File(out, sid + "_nucleus_res.txt"))
// GFP at time point t, slice z (both from 1) is 100 t + 10 (z - 1).
check("each frame measured its own GFP",
      rL.findAll { it.ch == "2" }.every { (it.Mean as double) == 100d * (it.t as int) + 10d * ((it.z as int) - 1) }, true)
check("res rows renumbered across the join",
      rL.collect { it[" "] as int }, (1..rL.size()).toList())
def cfg = [:]
new File(out, sid + "_config.txt").eachLine { l -> def p = l.split("\t", 2); if (p.size() == 2) cfg[p[0]] = p[1] }
check("config: the frames these results hold",    cfg.frames_analysed, "2 3")
check("config: the series' own frame count",      cfg.image_frames, "3")
check("config: the interval",                     [cfg.frame_interval, cfg.frame_unit], ["1800.0", "sec"])
check("config: titled with the series id",        cfg.image_title, sid)
check("config: opened as luxendo",                cfg.open_method, "luxendo")
check("config: calibrated",                       [cfg.pixel_width, cfg.pixel_depth, cfg.pixel_unit],
                                                  ["0.208", "5.0", "micron"])
def ts = readTsv(new File(out, sid + "_threshold_stats.tsv"))
check("threshold stats: one row per frame, by t",  ts.collect { it.t }, ["2", "3"])
check("nothing left staged",                       new File(out, ".staging").exists(), false)

// The streamed frame and the same frame from its TIFF find the same objects
// in the same place: the TIFF row is time point 2.
def cfgT = [:]
new File(out, "tif1_config.txt").eachLine { l -> def p = l.split("\t", 2); if (p.size() == 2) cfgT[p[0]] = p[1] }
check("the TIFF opened as 2 channels, 3 slices, 1 frame",
      [cfgT.image_channels, cfgT.image_slices, cfgT.image_frames], ["2", "3", "1"])
def oT = readTsv(new File(out, "tif1_nucleus_outline.txt"))
// The streamed ids carry TTTT- (the series has 3 frames) and the TIFF's do not
// (it has one); with that field taken off, the rest must agree.
def noT = { String roi -> roi.replaceFirst(/^nucleus_\d{4}-(\d{4}-\d{4}-\d{4})$/, 'nucleus_$1') }
def shape = { rows -> rows.collect { [noT(it.roi), it.z, it.x, it.y] } }
check("streamed t=2 outlines = the TIFF's outlines", shape(oL.findAll { it.t == "2" }), shape(oT))
def rT = readTsv(new File(out, "tif1_nucleus_res.txt"))
def meas = { rows -> rows.collect { [noT(it.roi), it.ch, it.Area, it.Mean] } }
check("...and the same measurements",             meas(rL.findAll { it.t == "2" }), meas(rT))

println "\n=== one frame failing costs that frame, not the series ==="
// Time point 3's GFP file replaced by something that is not HDF5.
def gfp3 = srcRows.find { (it.t as int) == 3 && (it.channel as int) == 2 }
def bad = new File(root, gfp3.source_path as String)
def keep = new File(bad.getPath() + ".keep"); bad.renameTo(keep)
bad.setText("not an HDF5 file")
def outF = new File(tmp, "batch_fail")
def resF = runner.run(sheet.findAll { it.series_id == sid }, null, params, outF) { }
def sumF = TSV.read(new File(outF, "batch_summary.tsv"))
check("frames 1, 2 ok and 3 failed",
      sumF.collect { [it.t, it.status] }, [["1", "ok"], ["2", "ok"], ["3", "failed"]])
check("...the failure says which file",
      sumF.find { it.t == "3" }.message.contains("Cam_long_00002") ||
      sumF.find { it.t == "3" }.message.toLowerCase().contains("hdf5"), true)
check("the row still counts as ok",                [resF.ok, resF.failed, resF.frames_failed], [1, 0, 1])
def cfgF = [:]
new File(outF, sid + "_config.txt").eachLine { l -> def p = l.split("\t", 2); if (p.size() == 2) cfgF[p[0]] = p[1] }
check("the results hold the frames that worked",   cfgF.frames_analysed, "1 2")
check("...and so does the outline table",
      readTsv(new File(outF, sid + "_nucleus_outline.txt")).collect { it.t }.unique(), ["1", "2"])
check("nothing left staged after a partial series", new File(outF, ".staging").exists(), false)
bad.delete(); keep.renameTo(bad)

println "\n=== membership decides, never the look of a path ==="
// The Luxendo row WITHOUT the sources table: its path is a directory, which is
// not an image file -- and the message says so, rather than trying to open it.
def outM = new File(tmp, "batch_nosrc")
def resM = runner.run(sheet.findAll { it.series_id == sid }, null, params.findAll { k, v -> k != "sources" }, outM) { }
def sumM = TSV.read(new File(outM, "batch_summary.tsv"))
check("without its sources, the row fails",        [sumM[0].status, sumM[0].t], ["failed", ""])
check("...naming the missing file",                sumM[0].message.contains("no such image file"), true)
throwsWith("a sources table from before v0.7.0 is named",
           "before v0.7.0", { runner.loadSources([[source_path: "x", series_id: "s", channel: "1", t: "1"]], sheet) })

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
if (failed > 0) throw new AssertionError("${failed} series-source check(s) failed")
