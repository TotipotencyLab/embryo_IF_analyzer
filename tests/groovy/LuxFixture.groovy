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
     * Luxendo also writes a `.json` SIDECAR beside every image, holding the
     * same processingInformation. The scan reads that rather than the HDF5
     * (LuxendoSidecar says why), and pairing the two is what tells an image
     * apart from an index file -- so the fixture writes both by default. Pass
     * `sidecar: false` to leave the image unpaired, which is how the tests
     * check that an unpaired file is reported and skipped.
     *
     * opts: elementSize (bool), metadata (bool), sidecar (bool), el (float[3] z,y,x),
     *       vox (map), stack, stackDesc, channel, chanDesc, tp, bias
     */
    static File writeLux(File f, int nz, int ny, int nx, Map opts = [:]) {
        f.getParentFile()?.mkdirs()
        f.delete()
        int bias = (opts.bias ?: 0) as int
        def info = [
            image_id           : "2026-09-10T17:47:37.268Z-test",
            stack              : String.valueOf(opts.get("stack", 0)),
            stack_description  : (opts.stackDesc ?: "L26A pos1"),
            channel            : String.valueOf(opts.get("channel", 0)),
            channel_description: opts.containsKey("chanDesc") ? opts.chanDesc : "BF",
            time_point         : String.valueOf(opts.get("tp", 0)),
            objective          : "bottom",
            camera             : "long",
            voxel_size_um      : (opts.containsKey("vox") ? opts.vox
                                                          : [width: 0.208, height: 0.208, depth: 5.0]),
            image_size_vx      : [width: nx, height: ny, depth: nz],
        ]
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
                w.string().write("/metadata", JsonOutput.toJson([processingInformation: info]))
            }
        } finally {
            w.close()
        }
        if (opts.get("sidecar", true)) {
            writeSidecar(sidecarPath(f), info)
        }
        return f
    }

    /** The `.json` Luxendo writes beside an image. */
    static File sidecarPath(File h5) {
        def n = h5.getName()
        return new File(h5.getParentFile(), n.replaceAll(/(?i)\.lux\.h5$/, "") + ".json")
    }

    /** Write a sidecar from an explicit processingInformation map. */
    static File writeSidecar(File json, Map info) {
        json.getParentFile()?.mkdirs()
        json.setText(JsonOutput.prettyPrint(JsonOutput.toJson([processingInformation: info])), "UTF-8")
        return json
    }

    /**
     * An index file: valid HDF5, a .lux.h5 name, no /Data -- AND no sidecar,
     * which is what main_raw.lux.h5 actually is. Verified on the real
     * acquisition: its top level is [timepoint_0..3], it has no /metadata, and
     * there is no main_raw.json. So "has a .json sibling" excludes it for free.
     */
    static File writeIndex(File f) {
        f.getParentFile()?.mkdirs()
        f.delete()
        def w = HDF5Factory.open(f)
        try { w.string().write("/timepoint_0/note", "links would live here") } finally { w.close() }
        return f
    }

    static String dirName(Map pos, Map ch) {
        return "stack_${pos.stack}-${pos.desc}_channel_${ch.index}" + (ch.name ? "-${ch.name}" : "") + "_obj_bottom"
    }

    /**
     * A miniature acquisition tree, laid out as Luxendo does it.
     *
     * @param positions list of [stack: n, desc: "...", nz: n] maps
     * @param channels  list of [index: n, name: "..."] maps (name null = unnamed)
     * @param nT        time points per position
     * @param bdv       also write the bdv.h5 + bdv.xml index, as Luxendo does at
     *                  the end of an acquisition (default true)
     */
    static File buildTree(File root, List positions, List channels, int nT,
                          int ny = 6, int nx = 4, boolean bdv = true) {
        def raw = new File(root, "raw")
        positions.each { pos ->
            channels.each { ch ->
                (0..<nT).each { int t ->
                    writeLux(new File(new File(raw, dirName(pos, ch)), String.format("Cam_long_%05d.lux.h5", t)),
                             (pos.nz as int), ny, nx,
                             [stack: pos.stack, stackDesc: pos.desc,
                              channel: ch.index, chanDesc: ch.name,
                              tp: t, bias: ((pos.stack as int) * 1000 + (ch.index as int) * 100 + t)])
                }
            }
        }
        writeIndex(new File(root, "main_raw.lux.h5"))
        if (bdv) {
            def spec = bdvSpec(positions, channels, nT, ny, nx)
            writeBdv(root, spec.setups, spec.links)
        }
        return root
    }

    /**
     * What Luxendo's bdv.xml + bdv.h5 hold for a tree built by buildTree.
     *
     * ⚠️ SETUPS ARE NUMBERED IN TEXT ORDER OF THE STACK, as on the real
     * acquisition -- st:0, st:1, st:10, st:11, ..., st:2 -- and `tile` is that
     * ordinal. So with a stack of 10 or more, `tile` stops being the stack
     * number, which is the hazard a test has to be able to show.
     *
     * Returned rather than written, so a test can damage it first.
     */
    static Map bdvSpec(List positions, List channels, int nT, int ny = 6, int nx = 4) {
        def ordered = positions.sort(false) { a, b -> a.stack.toString() <=> b.stack.toString() }
        def setups = [], links = []
        ordered.eachWithIndex { pos, int tile ->
            channels.each { ch ->
                int id = setups.size()
                setups << [id: id, name: "ch:${ch.index}_st:${pos.stack}_ang:h0-v90_obj:bottom_cam:long".toString(),
                           channel: ch.index, tile: tile, nx: nx, ny: ny, nz: pos.nz,
                           vox: [0.208, 0.208, 5.0]]
                (0..<nT).each { int t ->
                    links << [t: t, setup: id,
                              target: "raw/" + dirName(pos, ch) + "/" + String.format("Cam_long_%05d.lux.h5", t)]
                }
            }
        }
        return [setups: setups, links: links]
    }

    /**
     * bdv.h5 (one external link per (time point, setup) at /tNNNNN/sNN/0/cells,
     * plus a non-link dataset per setup as the real one has) and bdv.xml.
     */
    static void writeBdv(File root, List<Map> setups, List<Map> links) {
        def h5 = new File(root, "bdv.h5")
        h5.delete()
        def w = HDF5Factory.open(h5)
        try {
            setups.each { s ->
                w.float64().writeMatrix(String.format("/s%02d/resolutions", s.id as int), [[1d, 1d, 1d]] as double[][])
            }
            links.each { l ->
                def group = String.format("/t%05d/s%02d/0", l.t as int, l.setup as int)
                if (!w.object().exists(group)) w.object().createGroup(group)
                w.object().createExternalLink(l.target as String, "Data", group + "/cells")
            }
        } finally {
            w.close()
        }
        def nT = links.collect { it.t as int }.max()
        def sb = new StringBuilder()
        sb << '<?xml version="1.0" encoding="UTF-8"?>\n<SpimData version="0.2">\n'
        sb << '  <BasePath type="relative">.</BasePath>\n  <SequenceDescription>\n'
        sb << '    <ImageLoader format="bdv.hdf5"><hdf5 type="relative">bdv.h5</hdf5></ImageLoader>\n'
        sb << '    <ViewSetups>\n'
        setups.each { s ->
            sb << "      <ViewSetup>\n        <id>${s.id}</id>\n        <name>${s.name}</name>\n"
            sb << "        <size>${s.nx} ${s.ny} ${s.nz}</size>\n"
            sb << "        <voxelSize><unit>micrometer</unit><size>${s.vox.join(' ')}</size></voxelSize>\n"
            sb << "        <attributes><channel>${s.channel}</channel><angle>0</angle><tile>${s.tile}</tile></attributes>\n"
            sb << "      </ViewSetup>\n"
        }
        sb << '    </ViewSetups>\n'
        sb << "    <Timepoints type=\"range\"><first>0</first><last>${nT}</last></Timepoints>\n"
        sb << '  </SequenceDescription>\n</SpimData>\n'
        new File(root, "bdv.xml").setText(sb.toString(), "UTF-8")
    }
}
