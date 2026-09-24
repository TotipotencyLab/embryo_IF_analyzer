#@ File    (persist=false, label="Image file (.lif, .czi, ...)", style="file") imageFile
#@ String  (persist=false, label="Series index, or 'name:<series name>'", value="0") seriesSpec
#@ Boolean (persist=false, label="Autoscale display", value=true) autoscale
#@ Boolean (persist=false, label="Open as virtual stack (big series, no copy in RAM)", value=false) virtualStack

// Open_LifFile.groovy
//
// Open ONE chosen series into a Fiji window, by index or by name.
//
// The Bio-Formats importer dialog lists every series with a preview, which is
// the right tool until the file is big: a .lif with 1563 series does not fit in
// that list usefully, and building the previews is slow. This asks for an index
// instead. Use Inspect_ImageFile.groovy first to find out which index you want
// -- it reads the header only, so it is instant even on 200 GiB.
//
// GUI script. Headless it will open the image and have nowhere to show it.
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
import loci.formats.ImageReader
import loci.formats.MetadataTools
import loci.formats.meta.IMetadata
import loci.plugins.BF
import loci.plugins.in.ImporterOptions

def path = imageFile.getAbsolutePath()

// --- resolve the series, from the header alone -----------------------------
def reader = new ImageReader()
IMetadata meta = MetadataTools.createOMEXMLMetadata()
reader.setMetadataStore(meta)
reader.setId(path)
int n = reader.getSeriesCount()

int series = -1
def spec = (seriesSpec ?: "").trim()
if (spec.toLowerCase().startsWith("name:")) {
    def want = spec.substring(5).trim()
    def hits = (0..<n).findAll { (meta.getImageName(it) ?: "") == want }
    if (hits.isEmpty()) {
        reader.close()
        IJ.error("No series named '" + want + "' in " + new File(path).getName() +
                 "\n\nRun Inspect_ImageFile.groovy to list what is in there.")
        return
    }
    if (hits.size() > 1) {
        // A repeated name is a tile scan, not a mistake -- but this can only
        // open one, so say which and why rather than picking silently.
        IJ.log("'" + want + "' names " + hits.size() + " series: " + hits.take(12) +
               (hits.size() > 12 ? " ..." : ""))
        IJ.log("  That is one acquisition with many fields. Opening the first, " + hits[0] +
               "; give an index to choose another.")
    }
    series = hits[0]
} else {
    try {
        series = spec as int
    } catch (Exception e) {
        reader.close()
        IJ.error("Series must be a number, or 'name:<series name>'. Got: " + spec)
        return
    }
}
if (series < 0 || series >= n) {
    reader.close()
    IJ.error("Series " + series + " is out of range: this file has 0.." + (n - 1))
    return
}

reader.setSeries(series)
def label = meta.getImageName(series) ?: ("series " + series)
long px = (long) reader.getSizeX() * reader.getSizeY() * reader.getSizeZ() * reader.getSizeC()
int bpp = loci.formats.FormatTools.getBytesPerPixel(reader.getPixelType())
double mib = (px * bpp) / 1048576.0d

IJ.log("Opening [" + series + "] " + label)
IJ.log("  " + reader.getSizeX() + "x" + reader.getSizeY() +
       "  z=" + reader.getSizeZ() + " c=" + reader.getSizeC() + " t=" + reader.getSizeT() +
       "  " + String.format("%.0f MiB", mib) + (virtualStack ? " (virtual)" : " in RAM"))

// Is there anything in it? One middle plane, straight off the reader -- a
// series full of zeros opens as a perfectly good black window and looks like a
// display problem rather than an empty acquisition.
try {
    byte[] raw = reader.openBytes(reader.getIndex((int)(reader.getSizeZ() / 2), 0, 0))
    int mx = 0
    for (int i = 0; i < raw.length; i++) { int v = raw[i] & 0xFF; if (v > mx) mx = v }
    if (mx == 0) {
        IJ.log("  WARNING: the middle plane of channel 1 is entirely zero.")
        IJ.log("  If the window comes up black, the series really is empty -- it is not the display.")
    }
} catch (Throwable t) {
    IJ.log("  (could not pre-check the pixels: " + t.getMessage() + ")")
}
reader.close()

if (!virtualStack && mib > 4096) {
    IJ.log("  NOTE: " + String.format("%.1f GiB", mib / 1024.0d) +
           " is a lot to hold in RAM. Tick the virtual stack option if Fiji stalls.")
}

// --- open, with every preference restored afterwards -----------------------
boolean prevWindowless = Prefs.get("bioformats.windowless", false)
boolean prevAutoscale  = Prefs.get("bioformats.autoscale", true)
boolean prevVirtual    = Prefs.get("bioformats.virtual", false)
boolean prevOpenAll    = Prefs.get("bioformats.openAllSeries", false)
try {
    def opt = new ImporterOptions()
    opt.setId(path)
    // windowless: we have already chosen the series in our own dialog, so the
    // importer's chooser would be a second question about the same thing.
    opt.setWindowless(true)
    opt.setAutoscale(autoscale)
    opt.setVirtual(virtualStack)
    opt.setOpenAllSeries(false)
    opt.clearSeries()
    opt.setSeriesOn(series, true)

    def imps = BF.openImagePlus(opt)
    if (imps == null || imps.length == 0) {
        IJ.error("Bio-Formats returned no image for series " + series)
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
