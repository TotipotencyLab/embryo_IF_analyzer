// LuxFixture.groovy
//
// Synthetic Luxendo files for the tests. NOT part of the shipped library --
// it lives in tests/groovy because nothing outside the tests should be making
// .lux.h5 files.
//
// JHDF5 writes HDF5 as well as reading it, so the tests need no data at all,
// which is the rule the rest of tests/groovy already follows. It also makes
// this file the written-down statement of the format: if a future Luxendo
// release moves /Data or renames element_size_um, the expectation that has to
// change is here, rather than in somebody's memory of what they once saw.

import ch.systemsx.cisd.hdf5.HDF5Factory
import ch.systemsx.cisd.base.mdarray.MDShortArray
import groovy.json.JsonOutput

class LuxFixture {

    /**
     * One synthetic .lux.h5, shaped exactly as a TruLive3D emits:
     * /Data uint16 [z][y][x], an element_size_um attribute in [z, y, x] order,
     * and /metadata holding the acquisition JSON.
     *
     * Pixel value at (z, y, x) is z*10000 + y*100 + x, plus an optional
     * per-file `bias`. A transposed axis or an off-by-one plane therefore shows
     * up as a WRONG NUMBER rather than as a plausible one.
     *
     * opts: elementSize (bool), metadata (bool), el (float[3] z,y,x),
     *       vox (map), stack, stackDesc, channel, chanDesc, tp, bias
     */
    static File writeLux(File f, int nz, int ny, int nx, Map opts = [:]) {
        f.getParentFile()?.mkdirs()
        f.delete()
        int bias = (opts.bias ?: 0) as int
        def w = HDF5Factory.open(f)
        try {
            short[] flat = new short[nz * ny * nx]
            for (int z = 0; z < nz; z++)
                for (int y = 0; y < ny; y++)
                    for (int x = 0; x < nx; x++)
                        flat[(z * ny + y) * nx + x] = (short) (z * 10000 + y * 100 + x + bias)
            w.uint16().writeMDArray("/Data", new MDShortArray(flat, [nz, ny, nx] as int[]))
            if (opts.get("elementSize", true)) {
                w.float32().setArrayAttr("/Data", "element_size_um",
                                         (opts.el ?: [5.0f, 0.208f, 0.208f]) as float[])
            }
            if (opts.get("metadata", true)) {
                def vox = opts.containsKey("vox") ? opts.vox
                                                  : [width: 0.208, height: 0.208, depth: 5.0]
                w.string().write("/metadata", JsonOutput.toJson([
                    processingInformation: [
                        image_id           : "2026-09-10T17:47:37.268Z-test",
                        stack              : String.valueOf(opts.get("stack", 0)),
                        stack_description  : (opts.stackDesc ?: "L26A pos1"),
                        channel            : String.valueOf(opts.get("channel", 0)),
                        channel_description: opts.containsKey("chanDesc") ? opts.chanDesc : "BF",
                        time_point         : String.valueOf(opts.get("tp", 0)),
                        objective          : "bottom",
                        camera             : "long",
                        voxel_size_um      : vox,
                        image_size_vx      : [width: nx, height: ny, depth: nz],
                    ]
                ]))
            }
        } finally {
            w.close()
        }
        return f
    }

    /** An index file: valid HDF5, a .lux.h5 name, no /Data. What main_raw.lux.h5 is. */
    static File writeIndex(File f) {
        f.getParentFile()?.mkdirs()
        f.delete()
        def w = HDF5Factory.open(f)
        try { w.string().write("/timepoint_0/note", "links would live here") } finally { w.close() }
        return f
    }

    /**
     * A miniature acquisition tree, laid out as Luxendo does it.
     *
     * @param positions list of [stack: n, desc: "...", nz: n] maps
     * @param channels  list of [index: n, name: "..."] maps (name null = unnamed)
     * @param nT        time points per position
     */
    static File buildTree(File root, List positions, List channels, int nT,
                          int ny = 6, int nx = 4) {
        def raw = new File(root, "raw")
        positions.each { pos ->
            channels.each { ch ->
                def dirName = "stack_${pos.stack}-${pos.desc}_channel_${ch.index}" +
                              (ch.name ? "-${ch.name}" : "") + "_obj_bottom"
                (0..<nT).each { int t ->
                    writeLux(new File(new File(raw, dirName), String.format("Cam_long_%05d.lux.h5", t)),
                             (pos.nz as int), ny, nx,
                             [stack: pos.stack, stackDesc: pos.desc,
                              channel: ch.index, chanDesc: ch.name,
                              tp: t, bias: ((pos.stack as int) * 1000 + (ch.index as int) * 100 + t)])
                }
            }
        }
        writeIndex(new File(root, "main_raw.lux.h5"))
        return root
    }
}
