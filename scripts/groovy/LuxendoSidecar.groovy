// LuxendoSidecar.groovy
//
// One Luxendo `.json` sidecar -> the facts that place and size its `.lux.h5`.
//
// WHY THE SIDECAR AND NOT THE HDF5
//   Luxendo writes `Cam_long_00000.json` beside every `Cam_long_00000.lux.h5`,
//   and it holds the same `processingInformation` as the file's own /metadata
//   PLUS the dimensions and the voxel size. Measured over a samba mount on a
//   4032-file acquisition:
//
//     HDF5 open + dataset header   392 ms/file   ->  26 min
//     HDF5 open + /metadata        322 ms/file   ->  22 min
//     this sidecar                  88 ms/file   ->   6 min
//
//   The scan used to do the first two, so it opened every file twice and took
//   ~48 minutes before printing its first count. The same calls on a local SSD
//   are 4.5 ms, so this is latency per file, not bandwidth -- it scales with
//   how MANY files there are and barely at all with how big they are.
//
//   Checked on 25 files spread across the tree: the sidecar agrees with the
//   HDF5 /metadata AND with the real dimensions in 25 of 25. So identity still
//   comes from content, as the repo requires -- just from a cheaper file's
//   content. See note/time_series_plan.md §5.6.
//
// PAIRING IS THE INDEX-FILE TEST, FOR FREE
//   A row needs BOTH the sidecar and its `.lux.h5`. That way a stray `.json`
//   from somebody else's analysis in the same tree cannot invent a row, and
//   `main_raw.lux.h5` -- which is link-only, has no /Data and no /metadata --
//   is excluded because it has no sidecar. The old content test (open the HDF5,
//   look for /Data) is gone rather than moved.

import groovy.json.JsonSlurper

class LuxendoSidecar {

    static final String SUFFIX_H5   = ".lux.h5"
    static final String SUFFIX_JSON = ".json"

    /** The `.json` that would describe `h5`, whether or not it exists. */
    static File sidecarFor(File h5) {
        def n = h5.getName()
        if (!n.toLowerCase().endsWith(SUFFIX_H5)) return null
        return new File(h5.getParentFile(), n.substring(0, n.length() - SUFFIX_H5.length()) + SUFFIX_JSON)
    }

    /** The `.lux.h5` that `json` would describe, whether or not it exists. */
    static File imageFor(File json) {
        def n = json.getName()
        if (!n.toLowerCase().endsWith(SUFFIX_JSON)) return null
        return new File(json.getParentFile(), n.substring(0, n.length() - SUFFIX_JSON.length()) + SUFFIX_H5)
    }

    /** A pair is a `.lux.h5` and its sidecar, both present. Neither alone counts. */
    static boolean isPaired(File h5) {
        if (h5 == null || !h5.isFile()) return false
        def side = sidecarFor(h5)
        return side != null && side.isFile()
    }

    String  sourcePath          // as read, for messages only
    Integer stack
    String  stackDescription
    Integer channel
    String  channelDescription
    Integer timePoint
    Integer sizeX
    Integer sizeY
    Integer sizeZ
    Double  pixelWidth
    Double  pixelHeight
    Double  pixelDepth         // null for a single plane -- see below
    String  objective
    String  camera
    String  imageId

    /**
     * Read one sidecar.
     *
     * Throws rather than guesses. The three fields that PLACE a file -- stack,
     * channel, time_point -- are not optional: a file that cannot be placed
     * must stop the scan, not be quietly dropped or inferred from its path.
     */
    static LuxendoSidecar read(File json) {
        if (!json.isFile()) {
            throw new IllegalArgumentException("No such sidecar: " + json.getAbsolutePath())
        }
        def root
        try {
            root = new JsonSlurper().parse(json, "UTF-8")
        } catch (Throwable e) {
            throw new IllegalArgumentException(
                json.getName() + ": not readable as JSON (" + e.getMessage() + ")")
        }
        def pi = root?.processingInformation
        if (pi == null) {
            throw new IllegalArgumentException(
                json.getName() + ": no processingInformation; not a Luxendo sidecar")
        }
        def s = new LuxendoSidecar()
        s.sourcePath = json.getAbsolutePath()

        // Luxendo writes these as JSON STRINGS ("0", not 0), so they are parsed
        // rather than cast -- a cast of "0" to Integer throws in Groovy.
        s.stack     = requireInt(pi, "stack", json)
        s.channel   = requireInt(pi, "channel", json)
        s.timePoint = requireInt(pi, "time_point", json)

        s.stackDescription   = str(pi.stack_description)
        s.channelDescription = str(pi.channel_description)
        s.objective          = str(pi.objective)
        s.camera             = str(pi.camera)
        s.imageId            = str(pi.image_id)

        def sz = pi.image_size_vx
        if (sz == null || sz.width == null || sz.height == null || sz.depth == null) {
            throw new IllegalArgumentException(
                json.getName() + ": no image_size_vx; the dimensions are not optional")
        }
        s.sizeX = (sz.width  as Number).intValue()
        s.sizeY = (sz.height as Number).intValue()
        s.sizeZ = (sz.depth  as Number).intValue()
        if (s.sizeX < 1 || s.sizeY < 1 || s.sizeZ < 1) {
            throw new IllegalArgumentException(
                json.getName() + ": image_size_vx is " + s.sizeX + "x" + s.sizeY + "x" + s.sizeZ)
        }

        // Voxel size is optional -- a file with none gets blank calibration
        // rather than a made-up 1.0.
        def vx = pi.voxel_size_um
        if (vx != null) {
            s.pixelWidth  = dbl(vx.width)
            s.pixelHeight = dbl(vx.height)
            // BLANK, never 1.0, for a single plane. Same rule as _config.txt's
            // pixel_depth and LuxendoFile's: a z step that does not exist must
            // not arrive as a usable-looking number, because something
            // downstream multiplies by it.
            s.pixelDepth  = (s.sizeZ > 1) ? dbl(vx.depth) : null
        }
        return s
    }

    private static Integer requireInt(Object pi, String key, File json) {
        def v = pi[key]
        if (v == null || !v.toString().trim()) {
            throw new IllegalArgumentException(
                json.getName() + ": no " + key +
                " (stack, channel and time_point are what place a file); refusing to guess from the path")
        }
        try {
            return Integer.parseInt(v.toString().trim())
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(
                json.getName() + ": " + key + " is >>>" + v + "<<<, not a whole number")
        }
    }

    private static String str(Object v) {
        def t = (v == null) ? "" : v.toString().trim()
        return t ?: null
    }

    private static Double dbl(Object v) {
        if (v == null) return null
        def t = v.toString().trim()
        if (!t) return null
        try { return Double.parseDouble(t) } catch (NumberFormatException e) { return null }
    }

    /**
     * The time point encoded in a Luxendo filename, or null.
     *
     * `Cam_long_00001.lux.h5` -> 1. Used ONLY by quickScan, and only after the
     * mapping has been confirmed against a real sidecar in the same directory
     * -- see LuxendoScan.readSampled. Identity otherwise never comes from a
     * path in this repo, and this is the single narrow exception, made safe by
     * being checked rather than assumed.
     */
    static Integer timePointFromName(File h5) {
        def m = (h5.getName() =~ /(?i)_(\d+)\.lux\.h5$/)
        return m ? Integer.parseInt(m[0][1] as String) : null
    }

    /** A copy of this sidecar standing for another time point of the same stack. */
    LuxendoSidecar atTimePoint(int tp) {
        def c = new LuxendoSidecar()
        c.sourcePath = sourcePath; c.stack = stack; c.stackDescription = stackDescription
        c.channel = channel; c.channelDescription = channelDescription
        c.timePoint = tp
        c.sizeX = sizeX; c.sizeY = sizeY; c.sizeZ = sizeZ
        c.pixelWidth = pixelWidth; c.pixelHeight = pixelHeight; c.pixelDepth = pixelDepth
        c.objective = objective; c.camera = camera; c.imageId = imageId
        return c
    }

    /** The shape and calibration, for comparing one sidecar against another. */
    List shape() { return [sizeX, sizeY, sizeZ, pixelWidth, pixelHeight, pixelDepth] }

    @Override
    String toString() {
        return "LuxendoSidecar(stack=" + stack + " ch=" + channel + " t=" + timePoint +
               " " + sizeX + "x" + sizeY + "x" + sizeZ + ")"
    }
}
