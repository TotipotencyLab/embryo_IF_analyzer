#@ File (label="Image file (.lif, .tif, .czi, ...)", style="file") imageFile
#@ Boolean (label="Show physical pixel size", value=true) showCalibration

// Inspect_ImageFile.groovy
//
// List every series inside an image file WITHOUT loading any pixels.
//
// A Leica .lif holds many series (imaging positions). Opening one to find out
// what is inside it costs minutes and gigabytes; reading the metadata costs
// milliseconds, because Bio-Formats can parse the header alone.
//
// ---------------------------------------------------------------------------
// Groovy notes, since this doubles as a reading script:
//
//   * The `#@` lines above are SciJava script parameters. Fiji turns them into
//     a dialog, and they are also settable from the command line. They are not
//     Groovy syntax -- they are a comment to Groovy and a directive to Fiji.
//     https://imagej.net/scripting/parameters
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
reader.setId(path) // Actual read of the file metadata - the file got opened and parsed. The information also populates to the `meta` object.

// A small helper function to extract the physical (not pixel) size of the image.
// In Groovy a closure is a value: `{ args -> body }` assigned to
// a variable and invoked like a method. `?:` is the Elvis operator, returning
// the right side when the left is null or falsy.
def physical = { int series, String axis ->
    if (!showCalibration) return ""
    try {
        // switch-like code structure: `(condition) ? (value) : (condition2) ? (value2) : (default value)`
        def len = (axis == "X") ? meta.getPixelsPhysicalSizeX(series)
                : (axis == "Y") ? meta.getPixelsPhysicalSizeY(series)
                : (axis == "Z") ? meta.getPixelsPhysicalSizeZ(series)
                                : null
        // `?.` is safe navigation: it yields null instead of throwing when the
        // receiver is null, which physical sizes often are.
        return (len == null) ? "-" : String.format("%.4f %s", len.value().doubleValue(),
                                                   len.unit().getSymbol())
    } catch (Exception e) {
        return "-"
    }
}

int n = reader.getSeriesCount()
println "File   : ${path}"
println "Format : ${reader.getFormat()}"
println "Series : ${n}"
println ""
// Print information of each series
println String.format("%-5s %-34s %6s %6s %5s %4s %4s  %-9s %s",
                      "idx", "name", "X", "Y", "Z", "C", "T", "type", "pixel size (X, Y, Z)")
println "-" * 118

for (int s = 0; s < n; s++) {
    reader.setSeries(s)                      // all getSizeN() below refer to this series
    def name = meta.getImageName(s) ?: "(unnamed)"
    if (name.length() > 34) name = "..." + name.substring(name.length() - 31)

    println String.format("%-5d %-34s %6d %6d %5d %4d %4d  %-9s %s",
                          s, name,
                          reader.getSizeX(), reader.getSizeY(), reader.getSizeZ(),
                          reader.getSizeC(), reader.getSizeT(),
                          loci.formats.FormatTools.getPixelTypeString(reader.getPixelType()),
                          [physical(s, "X"), physical(s, "Y"), physical(s, "Z")].join(", "))
}

// Always close the reader: it holds an open file handle.
reader.close()

println ""
println "To open one series: Bio-Formats Importer, or"
println "  loci.plugins.BF.openImagePlus(new loci.plugins.in.ImporterOptions(...))"
