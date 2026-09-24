#@ File    (persist=false, label="Image file (.lif, .tif, .czi, ...)", style="file") imageFile
#@ String  (persist=false, label="Series: blank = all, '1030-1069', '3 7 9', or 'name:<series name>'", value="") seriesSpec
#@ Boolean (persist=false, label="Show physical pixel size", value=true) showCalibration
#@ Boolean (persist=false, label="Show stage position", value=false) showStage
#@ String  (persist=false, label="Check pixels: none | middle | all", choices={"none","middle","all"}) checkPixels
#@ Boolean (persist=false, label="Group repeated names and judge whether they are copies", value=true) groupByName

// Inspect_ImageFile.groovy
//
// List what is inside an image file, and optionally look at the pixels,
// WITHOUT opening it in the GUI.
//
// A Leica .lif holds many series (imaging positions). Opening one to find out
// what is inside it costs minutes and gigabytes; reading the metadata costs
// milliseconds, because Bio-Formats can parse the header alone -- 3 s for a
// 200 GiB file with 1563 series, which will not fit in the importer's dialog
// at all.
//
// ---------------------------------------------------------------------------
// Two things this exists to answer, beyond "what is in here":
//
//   1. WHY DOES THE SAME NAME APPEAR MANY TIMES? A run of consecutive series
//      sharing one name is a Leica tile scan or Mark-and-Find: one named
//      acquisition, many fields. They are NOT copies, and a sample sheet needs
//      the series index in the prefix to tell them apart.
//
//   2. IS THERE ANYTHING IN IT? `checkPixels` reads planes and reports the
//      percentage that is non-zero. An acquisition that was set up and never
//      run, or a file that was copied incompletely, reads as a perfectly
//      well-formed series full of zeros -- and every step downstream will
//      happily segment nothing out of it.
//
// ---------------------------------------------------------------------------
// This script does NOT use loci.plugins.in.ImporterOptions, on purpose.
// ImporterOptions is backed by ImageJ preferences and the importer saves them,
// so touching it here would change how the operator's Fiji behaves afterwards
// -- setWindowless(true) once, and drag-and-drop stops offering the series
// chooser. Everything below goes through ImageReader directly, which has no
// such side effect. Use Open_LifFile.groovy when you actually want a window.
//
// ---------------------------------------------------------------------------
// Groovy notes, since this doubles as a reading script:
//
//   * The `#@` lines above are SciJava script parameters. Fiji turns them into
//     a dialog, and they are also settable from the command line. They are not
//     Groovy syntax -- they are a comment to Groovy and a directive to Fiji.
//     https://imagej.net/scripting/parameters
//     `persist=false` stops Fiji remembering the last value: handy in a dialog,
//     but a run whose behaviour depends on what somebody typed last week is not
//     reproducible.
//
//   * `def` declares a variable of unstated type. Groovy is dynamically typed
//     by default, but you may write the Java type instead when it helps the
//     reader (`IMetadata meta = ...` below).
//
//   * `"${expr}"` is a GString: string interpolation. Single quotes do not
//     interpolate.
//
//   * Groovy accepts Java syntax almost verbatim, which is why Javadoc examples
//     and the Recorder's "Java" output can be pasted in nearly unchanged.
//
// Headless:
//
//   ImageJ-macosx --headless --console --mem=3000m \
//     --run scripts/groovy/Inspect_ImageFile.groovy \
//     "imageFile='/path/big.lif',seriesSpec='1030-1069',checkPixels='middle'"

import loci.formats.ImageReader
import loci.formats.MetadataTools
import loci.formats.meta.IMetadata

def path = imageFile.getAbsolutePath()

// The reader parses the file header. setMetadataStore() must be called BEFORE
// setId(), or the store is never populated -- the metadata is read as the file
// is opened, not on demand.
def reader = new ImageReader()
IMetadata meta = MetadataTools.createOMEXMLMetadata() // empty metadata container (not linked to any file)
reader.setMetadataStore(meta) // wire `reader` and `meta` together.
long t0 = System.currentTimeMillis()
reader.setId(path) // Actual read of the file metadata - the file got opened and parsed. The information also populates to the `meta` object.
long headerMs = System.currentTimeMillis() - t0

int n = reader.getSeriesCount()
println "File   : ${path}"
println "Format : ${reader.getFormat()}"
println "Size   : ${String.format('%.2f GiB', new File(path).length() / 1073741824.0d)}"
println "Series : ${n}   (header read in ${headerMs} ms, no pixels)"
println ""

def names = (0..<n).collect { meta.getImageName(it) ?: "" }

// --- which series ----------------------------------------------------------
// Parsed by SeriesSpec.groovy, shared with Open_LifFile.groovy. A second copy
// of this would be a second set of edge cases to get wrong, and the two scripts
// must accept the same answers or the workflow "inspect these, now open those"
// breaks at the join.
def resolveLibDir = {
    def cands = []
    try { cands << binding.variables["javax.script.filename"] } catch (ignored) {}
    try { cands << binding.variables["org.scijava.script.ScriptModule"]?.getInfo()?.getPath() } catch (ignored) {}
    for (c in cands) {
        if (c) { def f = new File(c.toString()); if (f.exists() && f.getParentFile() != null) return f.getParentFile() }
    }
    return null
}
def libDir = resolveLibDir()
if (libDir == null || !new File(libDir, "SeriesSpec.groovy").exists()) {
    println "Cannot locate SeriesSpec.groovy -- run this script from scripts/groovy/"
    reader.close()
    return
}
def SS = new GroovyClassLoader(this.class.classLoader)
             .parseClass(new File(libDir, "SeriesSpec.groovy"))

def wanted
try {
    wanted = SS.parse(seriesSpec, names, n)
} catch (Exception e) {
    println "Series selection: " + e.getMessage()
    reader.close()
    println "Done: nothing to show"
    return
}
println "Selected: ${SS.describe(wanted)}   (${wanted.size()} series)"
println ""

// A small helper function to extract the physical (not pixel) size of the image.
// `?:` is the Elvis operator, returning the right side when the left is null or
// falsy -- safe here because a physical size of 0 is meaningless anyway.
def physical = { int series, String axis ->
    if (!showCalibration) return ""
    try {
        // switch-like code structure: `(condition) ? (value) : (condition2) ? (value2) : (default value)`
        def len = (axis == "X") ? meta.getPixelsPhysicalSizeX(series)
                : (axis == "Y") ? meta.getPixelsPhysicalSizeY(series)
                : (axis == "Z") ? meta.getPixelsPhysicalSizeZ(series)
                                : null
        // `?.` is safe navigation: it yields null instead of throwing when the
        // receiver is null, which physical sizes often are -- physicalSizeZ is
        // null on every single-plane series.
        return (len == null) ? "-" : String.format("%.4f", len.value().doubleValue())
    } catch (Exception e) {
        return "-"
    }
}

// Stage position, plane 0: every plane of a series shares it. Where the reader
// populates it, a tile scan shows a different position per series. Where it
// does not -- the Leica files here leave it at the origin -- it is silent, and
// the pixels have to answer instead.
def stageOf = { int s ->
    try {
        def x = meta.getPlanePositionX(s, 0)
        def y = meta.getPlanePositionY(s, 0)
        if (x == null && y == null) return null
        return [x?.value()?.doubleValue() ?: 0d, y?.value()?.doubleValue() ?: 0d]
    } catch (Exception e) {
        return null
    }
}

// --- pixels ----------------------------------------------------------------
// openBytes() off the reader, addressed with getIndex(z, c, t). NOT an
// ImagePlus, and not a cropped ImporterOptions read: an earlier version cropped
// with setZBegin/setCBegin and got an all-zero plane back from a series that
// plainly had data.
def planeStats = { int s, int z, int c ->
    byte[] raw = reader.openBytes(reader.getIndex(z, c, 0))
    long total = 0L
    int nonZero = 0, maxV = 0
    for (int i = 0; i < raw.length; i++) {
        int v = raw[i] & 0xFF
        total += v
        if (v > 0) nonZero++
        if (v > maxV) maxV = v
    }
    return [mean: total / (double) raw.length,
            max: maxV,
            pctNonZero: raw.length ? (100.0d * nonZero / raw.length) : 0d,
            crc: { def k = new java.util.zip.CRC32(); k.update(raw); k.getValue() }()]
}

// Per series: middle plane of channel 0, or every plane of every channel.
def dataOf = { int s ->
    if (checkPixels == "none") return null
    reader.setSeries(s)
    int nz = reader.getSizeZ(), nc = reader.getSizeC()
    if (checkPixels == "middle") {
        def st = planeStats(s, (int)(nz / 2), 0)
        return [pct: st.pctNonZero, mean: st.mean, max: st.max, crc: st.crc,
                planes: 1, withSignal: st.max > 0 ? 1 : 0]
    }
    long crcAll = 0L
    double pctSum = 0d, meanSum = 0d
    int maxAll = 0, withSignal = 0, planes = 0
    for (int c = 0; c < nc; c++) {
        for (int z = 0; z < nz; z++) {
            def st = planeStats(s, z, c)
            pctSum += st.pctNonZero; meanSum += st.mean
            if (st.max > maxAll) maxAll = st.max
            if (st.max > 0) withSignal++
            crcAll = crcAll * 31 + st.crc
            planes++
        }
    }
    return [pct: planes ? pctSum / planes : 0d, mean: planes ? meanSum / planes : 0d,
            max: maxAll, crc: crcAll, planes: planes, withSignal: withSignal]
}

// --- the listing ------------------------------------------------------------
def dataCache = [:]
if (wanted.size() <= 500) {
    // Print information of each series
    println String.format("%-6s %-34s %6s %6s %5s %4s %4s  %-9s %-17s %s",
                          "idx", "name", "X", "Y", "Z", "C", "T", "type",
                          showCalibration ? "pixel X, Y, Z (um)" : "",
                          checkPixels == "none" ? (showStage ? "stage x, y" : "")
                                                : "data%   max   planes w/signal")
    println "-" * 145
    wanted.each { int s ->
        reader.setSeries(s)                      // all getSizeN() below refer to this series
        def name = names[s] ?: "(unnamed)"
        if (name.length() > 34) name = "..." + name.substring(name.length() - 31)
        def d = dataOf(s)
        if (d != null) dataCache[s] = d
        def tail
        if (d != null) {
            tail = String.format("%6.2f%% %5d   %d/%d", d.pct, d.max, d.withSignal, d.planes)
            if (d.max == 0) tail += "   <-- EMPTY"
        } else {
            tail = showStage ? (stageOf(s) == null ? "-"
                     : String.format("%.1f, %.1f", stageOf(s)[0], stageOf(s)[1])) : ""
        }
        println String.format("%-6d %-34s %6d %6d %5d %4d %4d  %-9s %-17s %s",
                              s, name,
                              reader.getSizeX(), reader.getSizeY(), reader.getSizeZ(),
                              reader.getSizeC(), reader.getSizeT(),
                              loci.formats.FormatTools.getPixelTypeString(reader.getPixelType()),
                              showCalibration ? [physical(s, "X"), physical(s, "Y"), physical(s, "Z")].join(", ") : "",
                              tail)
    }
    println ""
} else {
    println "(${wanted.size()} series selected -- listing suppressed; narrow seriesSpec to see it)"
    println ""
    if (checkPixels != "none") {
        wanted.each { int s -> dataCache[s] = dataOf(s) }
    }
}

// --- empty series -----------------------------------------------------------
if (checkPixels != "none") {
    def empties = wanted.findAll { dataCache[it]?.max == 0 }
    println "=== pixel content ==="
    println "${wanted.size() - empties.size()} of ${wanted.size()} series contain data; ${empties.size()} are entirely zero."
    if (empties) {
        def runs = []
        empties.sort().each { int s ->
            if (runs && runs[-1][1] == s - 1) { runs[-1][1] = s } else { runs << [s, s] }
        }
        println "  empty ranges: " + runs.collect { it[0] == it[1] ? "${it[0]}" : "${it[0]}-${it[1]}" }.join(", ")
        println "  A well-formed series full of zeros segments to nothing without complaining."
        println "  If the empty ones are one contiguous tail, suspect the FILE rather than the"
        println "  acquisition: a copy that preallocated the full size and stopped part way"
        println "  leaves exactly this shape. Compare the file size against its source."
    }
    println ""
}

// --- repeated names ---------------------------------------------------------
if (groupByName) {
    def groups = wanted.groupBy { names[it] }.findAll { k, v -> v.size() > 1 }
    println "=== repeated names ==="
    if (groups.isEmpty()) {
        println "No name is repeated among the selected series."
    } else {
        println String.format("%-32s %5s %-15s %-10s %s", "name", "n", "indices", "contiguous", "verdict")
        println "-" * 120
        groups.sort { it.key }.each { nm, idx ->
            def sorted = idx.sort()
            boolean contiguous = (sorted.last() - sorted.first() + 1) == sorted.size()
            String verdict
            if (checkPixels != "none") {
                // The only evidence that settles it. Distinct pixel content
                // means distinct fields, whatever the name says.
                def crcs = sorted.collect { dataCache[it]?.crc }.findAll { it != null }
                def distinct = crcs.unique(false).size()
                def allEmpty = sorted.every { dataCache[it]?.max == 0 }
                verdict = allEmpty ? "ALL EMPTY -- no pixels to compare"
                        : distinct == crcs.size() ? "DIFFERENT FIELDS -- every one has its own content"
                        : distinct == 1 ? "IDENTICAL CONTENT -- really the same image repeated"
                        : "MIXED -- ${distinct} distinct among ${crcs.size()}"
            } else {
                def stages = sorted.collect { stageOf(it) }.findAll { it != null }
                def distinctPos = stages.collect {
                    [Math.round(it[0] * 10) / 10.0, Math.round(it[1] * 10) / 10.0]
                }.unique(false)
                // All at the origin is ABSENT data, not one shared position.
                // Reading it as "these are copies" would be a confident wrong
                // answer, which is worse here than no answer.
                boolean atOrigin = distinctPos.every { Math.abs(it[0]) < 1.0 && Math.abs(it[1]) < 1.0 }
                verdict = (stages.size() < 2 || (distinctPos.size() == 1 && atOrigin))
                        ? "no usable stage data -- set checkPixels to decide"
                        : distinctPos.size() == stages.size() ? "TILES -- every stage position differs"
                        : "same stage position -- set checkPixels to confirm"
            }
            def label = nm.length() > 32 ? ("..." + nm.substring(nm.length() - 29)) : nm
            println String.format("%-32s %5d %-15s %-10s %s", label, sorted.size(),
                                  "${sorted.first()}-${sorted.last()}",
                                  contiguous ? "yes" : "NO", verdict)
        }
        println ""
        println "A run of CONSECUTIVE series under one name is a tile scan or Mark-and-Find:"
        println "one acquisition, many fields. The name cannot tell them apart, so a sample"
        println "sheet prefix has to carry the series index."
    }
    println ""
}

// Always close the reader: it holds an open file handle.
reader.close()

println "To open one series into a window: Open_LifFile.groovy, or"
println "  loci.plugins.BF.openImagePlus(new loci.plugins.in.ImporterOptions(...))"
println "Done: ${wanted.size()} series inspected"
