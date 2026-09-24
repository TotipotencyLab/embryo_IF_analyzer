#@ File    (persist=false, label="Image file (.lif, .czi, ...)", style="file") imageFile
#@ String  (persist=false, label="Series: '5', '1 3', '1,4, 6-8', or 'name:<series name>'", value="0") seriesSpec
#@ Boolean (persist=false, label="Autoscale display", value=true) autoscale
#@ Boolean (persist=false, label="Open as virtual stack (big series, no copy in RAM)", value=true) virtualStack
#@ Integer (persist=false, label="Refuse to open more than this many windows at once", value=12) maxWindows

// Open_LifFile.groovy
//
// Open one or several chosen series into Fiji windows, by index or by name.
//
// The Bio-Formats importer dialog lists every series with a preview, which is
// the right tool until the file is big: a .lif with 1563 series does not fit in
// that list usefully, and building the previews is slow. This asks for indices
// instead. Use Inspect_ImageFile.groovy first to find out which ones you want
// -- it reads the header only, so it is instant even on 200 GiB.
//
// Series forms are shared with the inspector (SeriesSpec.groovy):
//
//   5                  one
//   1 3 7   /  1,3,7   several
//   6-8                an inclusive range
//   1,4, 6-8           mixed
//   name:slide20/O1_2  every series of that name -- a tile scan is many series
//                      under one name, so this can be a lot of windows
//
// maxWindows is a deliberate floor under the obvious accident: `name:` on a
// tiled acquisition, or a range typed with one digit too many, would otherwise
// try to open forty stacks at once and take the session with it.
//
// GUI script. Headless it will open the images and have nowhere to show them.
//
// ---------------------------------------------------------------------------
// ⚠️ ImporterOptions IS BACKED BY IMAGEJ PREFERENCES.
//
// The importer calls saveOptions() after a successful open, so whatever is set
// on an ImporterOptions instance becomes the operator's default. Setting
// windowless(true) once leaves `.bioformats.windowless=true` in IJ_Prefs.txt,
// and from then on dragging a .lif onto Fiji silently opens the first series
// instead of offering the chooser -- the dialog never comes back on its own.
//
// Every preference this script touches is therefore put back afterwards,
// whatever happens. Same discipline as forcing Set Measurements and
// Prefs.blackBackground rather than inheriting them: a global preference must
// not be quietly redecorated by a script.
//
// If a previous run has already changed it, this puts it right:
//
//   ij.Prefs.set("bioformats.windowless", false); ij.Prefs.savePreferences()

import ij.IJ
import ij.Prefs
import loci.formats.FormatTools
import loci.formats.ImageReader
import loci.formats.MetadataTools
import loci.formats.meta.IMetadata
import loci.plugins.BF
import loci.plugins.in.ImporterOptions

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
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
def SS = new GroovyClassLoader(this.class.classLoader)
             .parseClass(new File(libDir, "SeriesSpec.groovy"))

def path = imageFile.getAbsolutePath()

// --- resolve the series, from the header alone -----------------------------
def reader = new ImageReader()
IMetadata meta = MetadataTools.createOMEXMLMetadata()
reader.setMetadataStore(meta)
reader.setId(path)
int n = reader.getSeriesCount()
def names = (0..<n).collect { meta.getImageName(it) ?: "" }

def wanted
try {
    wanted = SS.parse(seriesSpec, names, n)
} catch (Exception e) {
    reader.close()
    IJ.error("Series selection\n\n" + e.getMessage())
    return
}

if (wanted.size() > maxWindows) {
    reader.close()
    IJ.error("That asks for " + wanted.size() + " series (" + SS.describe(wanted) + ").\n\n" +
             "A repeated name is a tile scan -- one acquisition, many fields -- so `name:` can\n" +
             "select dozens. Narrow the selection, or raise the window limit above " + maxWindows + ".")
    return
}

// --- what is about to be opened, before any of it is read ------------------
double totalMiB = 0d
def empties = []
IJ.log("=== " + new File(path).getName() + ": opening " + wanted.size() + " series (" +
       SS.describe(wanted) + ") ===")
wanted.each { int s ->
    reader.setSeries(s)
    long px = (long) reader.getSizeX() * reader.getSizeY() * reader.getSizeZ() * reader.getSizeC()
    double mib = (px * FormatTools.getBytesPerPixel(reader.getPixelType())) / 1048576.0d
    totalMiB += mib
    // Is there anything in it? One middle plane, straight off the reader. A
    // series full of zeros opens as a perfectly good black window and looks
    // like a display problem rather than an empty acquisition.
    String note = ""
    try {
        byte[] raw = reader.openBytes(reader.getIndex((int)(reader.getSizeZ() / 2), 0, 0))
        int mx = 0
        for (int i = 0; i < raw.length; i++) { int v = raw[i] & 0xFF; if (v > mx) mx = v }
        if (mx == 0) { note = "   <-- middle plane is EMPTY"; empties << s }
    } catch (Throwable t) {
        note = "   (could not pre-check pixels: " + t.getMessage() + ")"
    }
    IJ.log(String.format("  [%d] %-34s %dx%d z=%d c=%d  %.0f MiB%s",
                         s, (names[s] ?: "(unnamed)").take(34),
                         reader.getSizeX(), reader.getSizeY(),
                         reader.getSizeZ(), reader.getSizeC(), mib, note))
}
reader.close()

IJ.log(String.format("  total %.0f MiB%s", totalMiB, virtualStack ? " (virtual -- not held in RAM)" : " in RAM"))
if (!virtualStack && totalMiB > 4096) {
    IJ.log("  NOTE: " + String.format("%.1f GiB", totalMiB / 1024.0d) +
           " is a lot to hold at once. Tick the virtual stack option if Fiji stalls.")
}
if (empties) {
    IJ.log("  WARNING: series " + empties + " have an entirely zero middle plane.")
    IJ.log("  If those windows come up black, the data really is absent -- it is not the display.")
}

// --- open, with every preference restored afterwards -----------------------
boolean prevWindowless = Prefs.get("bioformats.windowless", false)
boolean prevAutoscale  = Prefs.get("bioformats.autoscale", true)
boolean prevVirtual    = Prefs.get("bioformats.virtual", false)
boolean prevOpenAll    = Prefs.get("bioformats.openAllSeries", false)
try {
    def opt = new ImporterOptions()
    opt.setId(path)
    // windowless: the series were already chosen in our own dialog, so the
    // importer's chooser would be a second question about the same thing.
    opt.setWindowless(true)
    opt.setAutoscale(autoscale)
    opt.setVirtual(virtualStack)
    opt.setOpenAllSeries(false)
    // One call for the whole selection: Bio-Formats returns an ImagePlus per
    // series and opens the file once, rather than once per window.
    opt.clearSeries()
    wanted.each { int s -> opt.setSeriesOn(s, true) }

    def imps = BF.openImagePlus(opt)
    if (imps == null || imps.length == 0) {
        IJ.error("Bio-Formats returned no image for " + SS.describe(wanted))
        return
    }
    imps.each { it.show() }
    IJ.log("Done: opened " + imps.length + " window(s)")
} finally {
    // The whole point of the block. Restored even if the open threw.
    Prefs.set("bioformats.windowless", prevWindowless)
    Prefs.set("bioformats.autoscale", prevAutoscale)
    Prefs.set("bioformats.virtual", prevVirtual)
    Prefs.set("bioformats.openAllSeries", prevOpenAll)
}
