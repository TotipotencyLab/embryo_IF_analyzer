// LuxendoFile.groovy
//
// One Luxendo `.lux.h5` file, opened for reading.
//
// Bio-Formats CANNOT read these -- `ImageReader.setId()` throws
// UnknownFormatException on both a raw Cam_long_*.lux.h5 and on main_raw.lux.h5.
// The only reader that opens anything in a Luxendo tree is BDVReader on
// bdv.xml, and on real data it returns the pixels of ONE stack for every series
// it reports (verified with a freshly constructed reader per series; the links
// inside bdv.h5 are correct, so the bug is Bio-Formats'). So this reads the
// HDF5 directly through JHDF5, which Fiji already ships.
//
// See note/luxendo_file_format.md for the format itself.

import ch.systemsx.cisd.hdf5.HDF5Factory
import ch.systemsx.cisd.hdf5.IHDF5Reader
import groovy.json.JsonSlurper

class LuxendoFile implements Closeable {

    /** The only two objects a .lux.h5 contains. */
    static final String DATA = "/Data"
    static final String META = "/metadata"

    File file
    private IHDF5Reader reader

    int sizeZ, sizeY, sizeX

    /**
     * Calibration, in micrometres.
     *
     * pixelDepth is NULL for a single plane rather than a number. A z step that
     * does not exist must not arrive as a usable-looking value -- something
     * downstream would multiply by it. Same rule `_config.txt` follows.
     */
    Double pixelWidth, pixelHeight, pixelDepth

    /** The acquisition JSON, byte-identical to the sidecar .json. */
    String metadataJson

    /** The subset of processingInformation the gatherer needs. */
    Map info = [:]

    /**
     * Does this file hold pixels, or is it one of the link-only index files?
     *
     * A Luxendo tree contains `main_raw.lux.h5` beside the real stacks: a few
     * hundred KB of HDF5 external links with no `/Data` at its root. It matches
     * *.lux.h5 like everything else, so a scan has to tell them apart by
     * content -- which is the same rule the rest of this milestone follows.
     */
    static boolean holdsPixels(File f) {
        if (!f.isFile()) return false
        def r = null
        try {
            r = HDF5Factory.openForReading(f)
            return r.object().exists(DATA) &&
                   r.object().getDataSetInformation(DATA).getDimensions().length == 3
        } catch (Throwable ignored) {
            return false
        } finally {
            if (r != null) { try { r.close() } catch (ignored2) { } }
        }
    }

    static LuxendoFile open(File f) {
        if (!f.isFile()) throw new IllegalArgumentException("No such .lux.h5: " + f.getAbsolutePath())
        def lf = new LuxendoFile(file: f)
        lf.reader = HDF5Factory.openForReading(f)
        lf.readHeader()
        return lf
    }

    private void readHeader() {
        if (!reader.object().exists(DATA)) {
            throw new IllegalArgumentException(
                file.getName() + " has no " + DATA + " dataset; not a Luxendo .lux.h5")
        }
        def dims = reader.object().getDataSetInformation(DATA).getDimensions()
        if (dims.length != 3) {
            throw new IllegalArgumentException(
                file.getName() + ": " + DATA + " has rank " + dims.length + ", expected 3 (z, y, x)")
        }
        // [z][y][x] -- NOT [x][y][z].
        sizeZ = dims[0] as int
        sizeY = dims[1] as int
        sizeX = dims[2] as int

        // element_size_um is [z, y, x] -- the ImageJ HDF5 convention, and the
        // reverse of the order the JSON states voxel_size_um in. Getting this
        // backwards swaps a 5 um z step for a 0.208 um one and every volume is
        // out by a factor of 24 with nothing on screen to say so.
        def el = null
        try {
            el = reader.float32().getArrayAttr(DATA, "element_size_um")
        } catch (ignored) { }
        if (el != null && el.length >= 3) {
            pixelDepth  = (sizeZ > 1) ? (el[0] as double) : null
            pixelHeight = el[1] as double
            pixelWidth  = el[2] as double
        }

        if (reader.object().exists(META)) {
            metadataJson = reader.string().read(META)
            info = parseInfo(metadataJson)
        }

        // The JSON carries the same calibration the other way round. Where both
        // are present they must agree: a disagreement means one of the two was
        // written by something that did not understand the other, and guessing
        // which to believe is how a run silently measures in the wrong units.
        def vox = info?.voxel_size_um
        if (vox != null && pixelWidth != null) {
            checkAgrees("pixel_width",  pixelWidth,  vox.width  as Double)
            checkAgrees("pixel_height", pixelHeight, vox.height as Double)
            if (sizeZ > 1) checkAgrees("pixel_depth", pixelDepth, vox.depth as Double)
        }
    }

    /** Float32 attribute against a double from JSON: compare at float precision. */
    private void checkAgrees(String what, Double attr, Double json) {
        if (attr == null || json == null) return
        if (Math.abs(attr - json) > 1e-4 * Math.max(1.0d, Math.abs(json))) {
            throw new IllegalStateException(
                file.getName() + ": " + what + " disagrees between element_size_um (" +
                attr + ") and the JSON voxel_size_um (" + json + ")")
        }
    }

    /** The fields of processingInformation this repo uses; the rest is left in the JSON. */
    static Map parseInfo(String json) {
        if (!json) return [:]
        def pi = new JsonSlurper().parseText(json)?.processingInformation
        if (pi == null) return [:]
        return [
            image_id           : pi.image_id,
            stack              : pi.stack,
            stack_description  : pi.stack_description,
            channel            : pi.channel,
            // absent for at least one channel in real data, so never assume it
            channel_description: pi.channel_description,
            time_point         : pi.time_point,
            objective          : pi.objective,
            camera             : pi.camera,
            voxel_size_um      : pi.voxel_size_um,
            image_size_vx      : pi.image_size_vx,
        ]
    }

    /**
     * One z plane as raw uint16, row-major, y then x.
     *
     * Read a plane at a time rather than the whole stack: a Luxendo stack is
     * 312 MiB and the assembler holds one output stack already. Measured, the
     * chunking ([nz, 64, 64], uncompressed) makes plane-by-plane about 5x
     * slower than one bulk read -- 2.5 s against 465 ms for 39 planes -- which
     * is the right trade against 312 MiB of extra heap.
     */
    short[] plane(int z) {
        if (z < 0 || z >= sizeZ) {
            throw new IllegalArgumentException(
                file.getName() + ": z " + z + " out of range 0.." + (sizeZ - 1))
        }
        return reader.uint16()
                     .readMDArrayBlockWithOffset(DATA, [1, sizeY, sizeX] as int[], [z, 0, 0] as long[])
                     .getAsFlatArray()
    }

    /**
     * Every plane, read in strips of rows rather than plane by plane.
     *
     * For a caller that wants the whole stack anyway -- the batch, which
     * analyses a frame at a time. /Data is chunked [nz, 64, 64]: every chunk
     * spans ALL slices, so reading plane by plane reads each chunk once per
     * plane, and over a network mount that is the cost of a frame. A strip of
     * rows as tall as a chunk, across every slice, reads each chunk exactly
     * once. Holds no more than the planes it returns plus one strip
     * (nz x 64 rows: ~10 MB for a 2048-wide, 39-slice stack).
     *
     * @return planes[z], each as plane(z) returns it
     */
    short[][] volume() {
        short[][] planes = new short[sizeZ][]
        for (int z = 0; z < sizeZ; z++) planes[z] = new short[sizeY * sizeX]
        int strip = chunkRows()
        for (int y0 = 0; y0 < sizeY; y0 += strip) {
            int h = Math.min(strip, sizeY - y0)
            short[] block = reader.uint16()
                                  .readMDArrayBlockWithOffset(DATA, [sizeZ, h, sizeX] as int[], [0, y0, 0] as long[])
                                  .getAsFlatArray()
            // block is [z][h][x]; each z's h rows land at row y0 of its plane.
            for (int z = 0; z < sizeZ; z++) {
                System.arraycopy(block, z * h * sizeX, planes[z], y0 * sizeX, h * sizeX)
            }
        }
        return planes
    }

    /** Rows per chunk of /Data, or 64 when it is not chunked (64 is what Luxendo writes). */
    int chunkRows() {
        def cs = null
        try { cs = reader.object().getDataSetInformation(DATA).tryGetChunkSizes() } catch (ignored) { }
        return (cs != null && cs.length == 3 && cs[1] > 0) ? (cs[1] as int) : 64
    }

    /** Bytes of pixel data this file holds. uint16, so two per voxel. */
    long payloadBytes() { return 2L * sizeZ * sizeY * sizeX }

    @Override
    void close() {
        if (reader != null) { try { reader.close() } catch (ignored) { } }
        reader = null
    }

    String toString() {
        return file.getName() + " [" + sizeX + "x" + sizeY + "x" + sizeZ + "]" +
               (pixelWidth != null ? (" " + pixelWidth + " um") : " uncalibrated")
    }
}
