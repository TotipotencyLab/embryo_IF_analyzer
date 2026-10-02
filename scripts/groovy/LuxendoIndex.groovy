// LuxendoIndex.groovy
//
// The BigDataViewer index Luxendo writes beside raw/ -> which `.lux.h5` holds
// each (setup, time point), WITHOUT walking raw/.
//
// WHY
//   Listing raw/ is what a scan costs over a network mount: ~8000 directory
//   entries for the 800 GB acquisition. `bdv.h5` already holds that list, as
//   one HDF5 external link per (time point, setup), and `bdv.xml` holds each
//   setup's size and voxel size. Measured over samba on the 4032-file
//   acquisition, in one session (note/luxendo_file_format.md §7):
//
//     v0.6.0 directory walk (quickScan)       175-231 s
//     this index, copied local and parsed       ~1 s
//
// WHAT IT DOES NOT HOLD, AND SO WHAT IT IS NOT
//   Not the position label ("L26A pos1"), not the channel name, and the stack
//   number only inside a setup NAME (`ch:0_st:10_...`). So the index is a FILE
//   LIST and a cross-check, never the source of identity: that still comes from
//   the sidecars, one per directory, exactly as on the walk.
//
// ⚠️ `<tile>` IS NOT THE STACK NUMBER. Luxendo numbers tiles in TEXT order of
//   the stack -- 0, 1, 10, 11, 12, 13, 2, ... -- so on the real 14-position
//   acquisition `tile` equals the stack for 6 of 42 setups. Keying on it would
//   give one position another's identity with nothing looking wrong. Nothing
//   here reads `tile`.
//
// ⚠️ IT IS WRITTEN AT THE END OF THE ACQUISITION -- 83 s after the last raw file
//   on the one acquisition timed. A run that crashed, or is still going, may
//   have no index or a stale one, and a file the index does not list is
//   invisible without a walk. LuxendoScan's `listing=walk` is the check for that.
//
// Read from a LOCAL COPY. HDF5 parsing is many small random reads, which is the
// access pattern network latency punishes: 5.7-6.2 s parsing bdv.h5 in place
// over samba, against 0.7 s to copy it and parse the copy.

import ch.systemsx.cisd.hdf5.HDF5Factory
import groovy.xml.XmlSlurper
import java.nio.file.Files
import java.nio.file.StandardCopyOption

class LuxendoIndex {

    static final String H5  = "bdv.h5"
    static final String XML = "bdv.xml"

    /** Level-0 image of one setup at one time point; other levels are pyramids. */
    static final String LINK_PATH = /^\/t(\d+)\/s(\d+)\/0\/cells$/

    /** [source_path (relative to the acquisition directory), setup, t], one per link. */
    List<Map> entries = []

    /** setup id -> [name, stack, channel, sizeX, sizeY, sizeZ, pixelWidth, pixelHeight, pixelDepth, unit] */
    Map<Integer, Map> setups = [:]

    /** Both files present. Either alone is not an index. */
    static boolean present(File dir) {
        return new File(dir, H5).isFile() && new File(dir, XML).isFile()
    }

    static LuxendoIndex read(File dir) {
        if (!present(dir)) {
            throw new IllegalArgumentException(
                "No " + H5 + " + " + XML + " in " + dir.getAbsolutePath())
        }
        def ix = new LuxendoIndex()
        def tmp = Files.createTempDirectory("luxendo_index").toFile()
        try {
            def h5  = new File(tmp, H5)
            def xml = new File(tmp, XML)
            Files.copy(new File(dir, H5).toPath(),  h5.toPath(),  StandardCopyOption.REPLACE_EXISTING)
            Files.copy(new File(dir, XML).toPath(), xml.toPath(), StandardCopyOption.REPLACE_EXISTING)
            ix.entries = readLinks(h5)
            ix.setups  = readSetups(xml)
        } finally {
            tmp.deleteDir()
        }
        if (!ix.entries) {
            throw new IllegalArgumentException(
                H5 + " in " + dir.getAbsolutePath() + " holds no external links to image files")
        }
        def unknown = ix.entries.collect { it.setup }.unique() - ix.setups.keySet()
        if (unknown) {
            throw new IllegalArgumentException(
                H5 + " links setup(s) " + unknown.sort() + " that " + XML + " does not describe")
        }
        return ix
    }

    /**
     * Every level-0 external link, WITHOUT dereferencing it -- dereferencing
     * would open the target, which is exactly the per-file cost this exists to
     * avoid. Link targets are relative to bdv.h5, which sits in the acquisition
     * directory, so they are already the sources table's `source_path`.
     */
    static List<Map> readLinks(File h5) {
        def r = HDF5Factory.openForReading(h5)
        def out = []
        try {
            def walk
            walk = { String p ->
                r.object().getAllGroupMemberInformation(p, true).each { info ->
                    def type = info.getType().toString()
                    if (type == "GROUP") {
                        walk(info.getPath())
                    } else if (type == "EXTERNAL_LINK") {
                        def m = (info.getPath() =~ LINK_PATH)
                        if (!m) return
                        out << [source_path: targetFile(info.tryGetSymbolicLinkTarget()),
                                t          : Integer.parseInt(m[0][1] as String),
                                setup      : Integer.parseInt(m[0][2] as String)]
                    }
                }
            }
            walk("/")
        } finally {
            r.close()
        }
        return out
    }

    /** "EXTERNAL::raw/x/Cam_long_00000.lux.h5::Data" -> "raw/x/Cam_long_00000.lux.h5" */
    static String targetFile(String target) {
        def t = (target ?: "").replaceFirst(/^EXTERNAL::/, "")
        int at = t.lastIndexOf("::")
        return (at >= 0 ? t.substring(0, at) : t).replace('\\', '/')
    }

    static Map<Integer, Map> readSetups(File xml) {
        def doc = new XmlSlurper().parse(xml)
        def out = [:]
        doc.SequenceDescription.ViewSetups.ViewSetup.each { vs ->
            def size = vs.size.text().trim().split(/\s+/)
            def vox  = vs.voxelSize.size.text().trim()
            def v    = vox ? vox.split(/\s+/).collect { Double.parseDouble(it) } : null
            def name = vs.name.text().trim()
            def st   = (name =~ /(?:^|_)st:(\d+)(?:_|$)/)
            def ch   = vs.attributes.channel.text().trim()
            int nz   = Integer.parseInt(size[2])
            out[Integer.parseInt(vs.id.text().trim())] = [
                name       : name,
                // From the NAME, because nothing structured carries it -- see
                // the header on <tile>. A cross-check only; identity is the
                // sidecar's.
                stack      : st ? Integer.parseInt(st[0][1] as String) : null,
                channel    : ch ? Integer.parseInt(ch) : null,
                sizeX      : Integer.parseInt(size[0]),
                sizeY      : Integer.parseInt(size[1]),
                sizeZ      : nz,
                pixelWidth : v ? v[0] : null,
                pixelHeight: v ? v[1] : null,
                // Blank for a single plane, as everywhere else in the repo.
                pixelDepth : (v && nz > 1) ? v[2] : null,
                unit       : vs.voxelSize.unit.text().trim(),
            ]
        }
        return out
    }
}
