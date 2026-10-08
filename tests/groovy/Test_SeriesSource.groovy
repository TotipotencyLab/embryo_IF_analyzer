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

println "\n=== a rerun resumes a series an earlier batch left unfinished ==="
// The earlier batch dies at the JOIN, after every frame is staged -- the way a
// heap too small for the overview TIFFs ends one. Provoked by a directory where
// the join writes its temporary file.
def lxRows = sheet.findAll { it.series_id == sid }
def outR = new File(tmp, "batch_ref")
runner.run(lxRows, null, params, outR) { }
def dieAtJoin = { File o, Map ps ->
    def block = new File(o, sid + "_nucleus_outline.txt.part"); block.mkdirs()
    new File(block, "x").setText("x")
    def r = runner.run(lxRows, null, ps, o) { }
    block.deleteDir()
    return r
}
def outJ = new File(tmp, "batch_resume")
def resJ = dieAtJoin(outJ, params)
def sumJ = TSV.read(new File(outJ, "batch_summary.tsv"))
check("the first batch's row failed at the join",  [resJ.failed, sumJ[0].status], [1, "failed"])
check("...leaving all three frames staged",
      new File(outJ, ".staging/" + sid).list().findAll { it ==~ /t\d{4}/ }.sort(), ["t0001", "t0002", "t0003"])
def resJ2 = runner.run(lxRows, null, params, outJ) { }
def sumJ2 = TSV.read(new File(outJ, "batch_summary.tsv"))
check("the rerun resumed them: ok, no time, and why",
      sumJ2.collect { [it.t, it.status, it.seconds, it.message] },
      (1..3).collect { [it.toString(), "ok", "", NP.RESUMED_MESSAGE] })
check("...with the counts the frames recorded",    sumJ2.collect { it.n_nucleus }, ["6", "6", "6"])
def tables = { File o -> ["_nucleus_outline.txt", "_nucleus_res.txt", "_threshold_stats.tsv"]
                         .collect { new File(o, sid + it).getText("UTF-8") } }
check("...and wrote the uninterrupted batch's tables", tables(outJ) == tables(outR), true)
check("...nothing left staged",                    new File(outJ, ".staging").exists(), false)

// redo_all: the same leftover, analysed again.
def outK = new File(tmp, "batch_restart")
dieAtJoin(outK, params)
runner.run(lxRows, null, params + [existing_output: "redo_all"], outK) { }
def sumK = TSV.read(new File(outK, "batch_summary.tsv"))
check("redo_all analyses every frame again",
      sumK.collect { [it.status, it.message, it.seconds != ""] }, (1..3).collect { ["ok", "", true] })
check("...to the same tables",                     tables(outK) == tables(outR), true)

// Other settings: the row is refused, the batch goes on, the staging is kept.
def outL = new File(tmp, "batch_other")
dieAtJoin(outL, params)
def resL = runner.run(lxRows, null, params + [nucleus_particle_size: "6-Infinity"], outL) { }
def sumL = TSV.read(new File(outL, "batch_summary.tsv"))
println "  message: " + sumL[0].message
check("other settings fail the row",               [resL.failed, sumL[0].status, sumL[0].t], [1, "failed", ""])
check("...saying what differs",
      sumL[0].message.contains("nucleus_particle_size '5-Infinity' then, '6-Infinity' now"), true)
check("...and keep the staged frames",
      new File(outL, ".staging/" + sid).list().findAll { it ==~ /t\d{4}/ }.size(), 3)
// skip_finished resumes an unfinished series too: it is not finished.
def resL2 = runner.run(lxRows, null, params + [nucleus_particle_size: "6-Infinity", existing_output: "skip_finished"], outL) { }
check("skip_finished refuses the same leftover",   resL2.failed, 1)
// choices= is not enforced on the command line: a value the option does not
// have is refused before any row, rather than read as some other choice.
throwsWith("an unknown existingOutput is refused",
           "existingOutput must be one of resume_unfinished, skip_finished, redo_all; got >>>restart<<<",
           { runner.run(lxRows, null, params + [existing_output: "restart"], new File(tmp, "batch_bad")) { } })

println "\n=== skip_finished: a finished series is not opened again ==="
def readCfgS = { File f -> def m = [:]; f.eachLine { l -> def q = l.split("\t", 2); if (q.size() == 2) m[q[0]] = q[1] }; m }
// The Luxendo series (3 frames) and the single-frame TIFF, finished once.
def outS = new File(tmp, "batch_skip")
def resS0 = runner.run(sheet, null, params, outS) { }
def sumS0 = TSV.read(new File(outS, "batch_summary.tsv"))
def written = { File o -> o.listFiles().findAll { it.isFile() && it.getName() != "batch_summary.tsv" &&
                                                  it.getName() != "batch_params.txt" }
                          .sort { it.getName() }.collectEntries { [(it.getName()): [it.lastModified(), it.getText("ISO-8859-1")]] } }
def before = written(outS)
// Proof that a skipped series is not OPENED, not merely that its files came
// out the same: the TIFF is moved away and a Luxendo frame made unreadable.
// Either would fail its row if anything tried to read it.
def tifFile = new File(tifRow.path), tifAway = new File(tifRow.path + ".away")
tifFile.renameTo(tifAway)
bad.renameTo(keep); bad.setText("not an HDF5 file")
Thread.sleep(1100)   // so a rewritten file would carry a later lastModified
def resS = runner.run(sheet, null, params + [existing_output: "skip_finished"], outS) { }
def sumS = TSV.read(new File(outS, "batch_summary.tsv"))
check("both rows skipped, neither failed",         [resS.ok, resS.skipped, resS.failed], [2, 2, 0])
check("...summary: every frame, ok, no time, and why",
      sumS.collect { [it.series_id, it.t, it.status, it.seconds, it.message] },
      sumS0.collect { [it.series_id, it.t, "ok", "", runner.FINISHED_MESSAGE] })
check("...with the earlier run's numbers and open method",
      sumS.collect { [it.threshold, it.mask_pct, it.n_nucleus, it.n_nucleolus, it.open_method] },
      sumS0.collect { [it.threshold, it.mask_pct, it.n_nucleus, it.n_nucleolus, it.open_method] })
check("...and no result file touched",             written(outS) == before, true)
bad.delete(); keep.renameTo(bad); tifAway.renameTo(tifFile)

// Other settings: analysed again, not refused -- nothing is being mixed.
def logS2 = []
def resS2 = runner.run(sheet, null, params + [existing_output: "skip_finished", nucleus_particle_size: "6-Infinity"], outS) { logS2 << it }
println "  log: " + logS2.find { it.contains("earlier results differ") }
check("...the log names what differs",
      logS2.count { it.contains("earlier results differ in nucleus_particle_size -- analysed again") }, 2)
def sumS2 = TSV.read(new File(outS, "batch_summary.tsv"))
check("other settings: both analysed again",       [resS2.ok, resS2.skipped, sumS2.count { it.seconds != "" }], [2, 0, 4])
check("...and the config says so",                 readCfgS(new File(outS, sid + "_config.txt")).nucleus_particle_size, "6-Infinity")

// Other frames: the Luxendo results hold 1-3, this run asks for 2-3, so it is
// analysed again; the single-frame TIFF ignores `frames` and is still finished.
def resS3 = runner.run(sheet, null, params + [existing_output: "skip_finished", nucleus_particle_size: "6-Infinity",
                                              frames: "2-3"], outS) { }
def sumS3 = TSV.read(new File(outS, "batch_summary.tsv"))
check("other frames: Luxendo redone, the TIFF skipped",
      sumS3.collect { [it.series_id, it.t, it.message] },
      [[sid, "2", ""], [sid, "3", ""], ["tif1", "1", runner.FINISHED_MESSAGE]])

// A failed frame: the results lack it, so the series is not finished.
def outSF = new File(tmp, "batch_skip_failed")
bad.renameTo(keep); bad.setText("not an HDF5 file")
runner.run(lxRows, null, params, outSF) { }
bad.delete(); keep.renameTo(bad)
check("(the earlier run lost frame 3)",            readCfgS(new File(outSF, sid + "_config.txt")).frames_analysed, "1 2")
def resSF = runner.run(lxRows, null, params + [existing_output: "skip_finished"], outSF) { }
def sumSF = TSV.read(new File(outSF, "batch_summary.tsv"))
check("a series with a failed frame is done again", [resSF.skipped, sumSF.collect { [it.t, it.status, it.message] }],
      [0, [["1", "ok", ""], ["2", "ok", ""], ["3", "ok", ""]]])

// No _config.txt, nothing to go on: everything is analysed, and it says why.
def outSN = new File(tmp, "batch_skip_noconfig")
runner.run(lxRows, null, params + [save_config: false], outSN) { }
def logSN = []
def resSN = runner.run(lxRows, null, params + [save_config: false, existing_output: "skip_finished"], outSN) { logSN << it }
check("save_config off: nothing skipped",          resSN.skipped, 0)
check("...and a warning says why",                 logSN.any { it.startsWith("WARNING: skip_finished needs save_config") }, true)

println "\n=== membership decides, never the look of a path ==="
// The Luxendo row WITHOUT the sources table: its path is a directory, which is
// not an image file -- and the message says so, rather than trying to open it.
def outM = new File(tmp, "batch_nosrc")
def resM = runner.run(sheet.findAll { it.series_id == sid }, null, params.findAll { k, v -> k != "sources" }, outM) { }
def sumM = TSV.read(new File(outM, "batch_summary.tsv"))
check("without its sources, the row fails",        [sumM[0].status, sumM[0].t], ["failed", ""])
check("...naming the missing sources table",       sumM[0].message.contains("is a directory, not an image file; a Luxendo series is read through the sources table -- give sourcesFile"), true)
// With a sources table that has no rows for it (they belong to another,
// excluded, row of the sheet), the message says the table was read and did not
// match, not that none was given.
def outN = new File(tmp, "batch_othersrc")
def otherSrc = params.sources.collect { it + [alias: "elsewhere"] }
def mine = sheet.find { it.series_id == sid }
def otherRow = mine + [series_id: "elsewhere_row", alias: "elsewhere", include: "false"]
runner.run([mine, otherRow], null, params + [sources: otherSrc], outN) { }
def sumN = TSV.read(new File(outN, "batch_summary.tsv"))
println "  message: " + sumN[0].message
check("...or that the table has no rows for it",  sumN[0].message.contains("its (alias, series_index) has none"), true)
throwsWith("a sources table from before v0.7.0 is named",
           "before v0.7.0", { runner.loadSources([[source_path: "x", series_id: "s", channel: "1", t: "1"]], sheet) })

println "\n=== a multi-frame series' overview: one TIFF per channel, a page per frame ==="
def ovParams = params + [save_overview: true, overview_width: 0, overview_height: 0,
                         overview_contrast: "auto", overview_saturated: 0.35d, overview_method: "max"]
def readCfg = { File f -> def m = [:]; f.eachLine { l -> def q = l.split("\t", 2); if (q.size() == 2) m[q[0]] = q[1] }; m }
def outO = new File(tmp, "batch_ov")
runner.run(sheet, null, ovParams + [frames: "1-3"], outO) { }
def ovT = IJ.openImage(new File(outO, sid + "_overview_ch1.tif").getPath())
check("a page per frame, as frames",               [ovT.getNChannels(), ovT.getNSlices(), ovT.getNFrames()], [1, 1, 3])
check("...labelled with their t",                  (1..3).collect { ovT.getStack().getSliceLabel(it) }, ["t=1", "t=2", "t=3"])
check("...8-bit, at the image's size",             [ovT.getBitDepth(), ovT.getWidth(), ovT.getHeight()], [8, NX, NY])
check("...with the acquisition's frame interval",  ovT.getCalibration().frameInterval, 1800d)
def ovO = IJ.openImage(new File(outO, sid + "_overview_ch1_overlay.tif").getPath())
check("and an overlay TIFF, RGB, a page per frame", [ovO.getBitDepth(), ovO.getNFrames()], [24, 3])
// The discs move 5 px a frame, so frame 1's outline is not where frame 3's is:
// a page drawing another frame's ROIs would show it.
def coloured = { ImagePlus im, int page, int x, int y ->
    int v = im.getStack().getProcessor(page).getPixel(x, y); ((v >> 16) & 255) != (v & 255) }
// Disc 1's left edge at y = 50: x = 40 + 5(t - 1) - 18, the fixture counting
// its time points from 0.
check("page t's outline is at frame t's disc",
      (1..3).collect { int t -> (1..3).collect { int u -> (-1..1).any { dx -> coloured(ovO, u, 17 + 5 * t + dx, 50) } } },
      [[true, false, false], [false, true, false], [false, false, true]])
def cfgO = readCfg(new File(outO, sid + "_config.txt"))
println "         (display range recorded: ${cfgO.overview_display_range})"
check("the config says what was written",
      [cfgO.overview_saved, cfgO.overview_channels, cfgO.frames_analysed], ["true", "1,2", "1 2 3"])
check("...and one display range per channel",      cfgO.overview_display_range ==~ /ch1:[\d.]+-[\d.]+ ch2:[\d.]+-[\d.]+/, true)
check("no multi-frame PNG",                        new File(outO, sid + "_overview_ch1.png").exists(), false)
check("the single-frame row keeps its PNGs",
      ["tif1_overview_ch1.png", "tif1_overview_ch1_overlay.png"].every { new File(outO, it).isFile() }, true)
check("...and records its range too",              readCfg(new File(outO, "tif1_config.txt")).overview_display_range ==~ /ch1:[\d.]+-[\d.]+ ch2:[\d.]+-[\d.]+/, true)
check("nothing left staged",                       new File(outO, ".staging").exists(), false)
// Channel 2 brightens 100 a frame; one range for the series keeps that visible.
def pageMean = { int page -> ovT.getStack().getProcessor(page).getStatistics().mean }
def ov2 = IJ.openImage(new File(outO, sid + "_overview_ch2.tif").getPath())
def means2 = (1..3).collect { ov2.getStack().getProcessor(it).getStatistics().mean }
println "         (ch2 page means ${means2.collect { IJ.d2s(it, 1) }})"
check("ch2 brightens across the pages",            means2[0] < means2[1] && means2[1] < means2[2], true)

// The two routes to one frame: time point 2 streamed from the sources as a
// series of one chosen frame, and the same frame as a TIFF file (tif1). Its
// one page has the range its PNG has, so the pixels must be the PNG's.
// Channel 1 only: channel 2 is FLAT in x-y here, and with nothing to stretch
// ImageJ leaves whatever range the processor carried -- for the PNG, one left
// on the projection's stack (ch1's) -- where the series uses the type's full
// range. A real channel always has something to stretch.
def out2 = new File(tmp, "batch_ov_t2")
runner.run(sheet, null, ovParams + [frames: "2"], out2) { }
[1].each { int c ->
    def page = IJ.openImage(new File(out2, sid + "_overview_ch" + c + ".tif").getPath())
    def png  = IJ.openImage(new File(out2, "tif1_overview_ch" + c + ".png").getPath())
    def red  = ((ij.process.ColorProcessor) png.getProcessor()).getChannel(1, null).getPixels() as byte[]
    check("ch" + c + ": streamed t=2's page = the TIFF route's PNG", page.getProcessor().getPixels() as byte[] == red, true)
    def pageO = IJ.openImage(new File(out2, sid + "_overview_ch" + c + "_overlay.tif").getPath())
    def pngO  = IJ.openImage(new File(out2, "tif1_overview_ch" + c + "_overlay.png").getPath())
    check("ch" + c + ": ...and its overlay page = the overlay PNG",
          pageO.getProcessor().getPixels() as int[] == pngO.getProcessor().getPixels() as int[], true)
}
def rng2 = { File f -> readCfg(f).overview_display_range.split(" ") }
check("ch1's ranges agree as recorded",            rng2(new File(out2, sid + "_config.txt"))[0], rng2(new File(out2, "tif1_config.txt"))[0])
println "         (flat ch2: series ${rng2(new File(out2, sid + "_config.txt"))[1]}, PNG ${rng2(new File(out2, "tif1_config.txt"))[1]})"
check("a flat channel's series range is the type's full range", rng2(new File(out2, sid + "_config.txt"))[1], "ch2:0.0-65535.0")

// Run_Overview_Batch, the cheap look, through the same code: its TIFF must be
// the nucleus batch's byte for byte, as its PNGs always have been.
println "\n=== Run_Overview_Batch draws a Luxendo series ==="
def ovScript = new File(LIBDIR, "Run_Overview_Batch.groovy")
def sheetF = new File(tmp, "sheet_ov.tsv"); TSV.write(sheet, sheetF, sheet[0].keySet().toList())
def srcF = new File(tmp, "sources_ov.tsv"); TSV.write(params.sources, srcF, params.sources[0].keySet().toList())
def outB = new File(tmp, "overview_batch"); outB.mkdirs()
def bind = new Binding([sheetFile: sheetF, outdir: outB, imageRoot: "", zSpec: "", channelsCsv: "1,2",
                        method: "max", contrast: "auto", saturated: 0.35d, outWidth: 0, outHeight: 0,
                        openMode: "auto", sourcesFile: srcF, frames: "1-3", runTag: "",
                        "javax.script.filename": ovScript.getAbsolutePath()])
new GroovyShell(this.class.classLoader, bind).evaluate(
    ovScript.readLines().findAll { !it.startsWith("#@") }.join("\n"), "Run_Overview_Batch.groovy")
[1, 2].each { int c ->
    def a = new File(outB, sid + "_overview_ch" + c + ".tif"), b = new File(outO, sid + "_overview_ch" + c + ".tif")
    check("ch" + c + ": the overview batch's TIFF = the nucleus batch's, byte for byte",
          a.isFile() && java.util.Arrays.equals(a.bytes, b.bytes), true)
}
check("...a PNG for the single-frame row, = the nucleus batch's",
      java.util.Arrays.equals(new File(outB, "tif1_overview_ch1.png").bytes, new File(outO, "tif1_overview_ch1.png").bytes), true)
check("...and no overlay: it draws no outlines",   new File(outB, sid + "_overview_ch1_overlay.tif").exists(), false)
def sumB = TSV.read(new File(outB, "batch_summary.tsv"))
check("its summary has a row per frame",
      sumB.findAll { it.series_id == sid }.collect { [it.t, it.status, it.channels] },
      [["1", "ok", "1,2"], ["2", "ok", "1,2"], ["3", "ok", "1,2"]])
check("...and nothing left staged",                new File(outB, ".staging").exists(), false)

tmp.deleteDir()
println "\n=== ${passed} passed, ${failed} FAILED ==="
if (failed > 0) throw new AssertionError("${failed} series-source check(s) failed")
