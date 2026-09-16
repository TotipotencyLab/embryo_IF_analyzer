// Overview.groovy
//
// Quick-look images for checking a detection run: a z-projection of the planes
// that detection actually used, optionally with the detected outlines drawn on.
//
// Library only -- no `#@` parameters, no dialog. Load it the same way as the
// other modules:
//
//   def OV = new GroovyClassLoader().parseClass(new File(LIBDIR + "/Overview.groovy"))
//
// Steps, each its own function so a caller can use only what it needs:
//
//   project()      collapse the chosen z-planes, per channel      <- this file so far
//   prepare()      pick a channel, set contrast, resize            (next)
//   addOutlines()  draw ROIs onto an overlay, scaled to the view   (next)
//   savePng()      burn the overlay into pixels and write a PNG    (next)
//
// Why not just call ZProjector: it projects one continuous range. Detection
// accepts gapped ranges ("1-20,35-40"), and an overview has to show what
// detection saw, so the chosen planes are gathered into a substack first.

import ij.*
import ij.plugin.ZProjector

class Overview {

    // Accepted projection names -> the strings ZProjector.run() understands.
    static final Map<String, String> METHODS = [
        max: "max", min: "min", sum: "sum", sd: "sd", median: "median",
        mean: "avg", avg: "avg"
    ]

    /**
     * Z-project the given planes of the given channels.
     *
     * @param imp       source image (a hyperstack, or a plain z-stack)
     * @param zSlices   z indices to include, 1-based; null or empty = all
     * @param method    max | mean | sum | sd | median | min
     * @param channels  channels to keep, 1-based; null or empty = all
     * @return a new image with one plane per requested channel, in the order
     *         given. Each plane is labelled "ch<c>" with its ORIGINAL channel
     *         number, so a caller can find channel 3 even if it is the only one.
     *
     * Only the first time frame is used; time-lapse data is not handled.
     */
    static ImagePlus project(ImagePlus imp, Set<Integer> zSlices, String method,
                             List<Integer> channels) {
        String zpMethod = METHODS[method?.toLowerCase()]
        if (zpMethod == null) {
            throw new IllegalArgumentException(
                "unknown projection '${method}'; use one of ${METHODS.keySet().join(', ')}")
        }

        int nC = imp.getNChannels(), nZ = imp.getNSlices()
        // NB: `(1..nZ)` is a Range -- a List, but a READ-ONLY one, so sort() on it
        //     throws UnsupportedOperationException. toList() makes a real copy.
        List<Integer> zs = (zSlices ? zSlices.toList() : (1..nZ).toList()).sort()
        List<Integer> cs = channels ? channels : (1..nC).toList()

        // Fail with a message naming the problem, rather than an index error from
        // deep inside ImageStack.
        def badZ = zs.findAll { it < 1 || it > nZ }
        if (badZ) throw new IllegalArgumentException("z out of range 1-${nZ}: ${badZ}")
        def badC = cs.findAll { it < 1 || it > nC }
        if (badC) throw new IllegalArgumentException("channel out of range 1-${nC}: ${badC}")
        if (imp.getNFrames() > 1) {
            IJ.log("Overview.project: ${imp.getNFrames()} frames; using frame 1 only")
        }

        // Gather the chosen planes in HYPERSTACK ORDER, where the channel varies
        // fastest: c1z1, c2z1, c1z2, c2z2, ...  So z is the OUTER loop. Nesting
        // it the other way builds a stack that setDimensions() then silently
        // misreads, and ZProjector mixes channels into each other.
        //
        // NB: the processors are SHARED with the source, not copied. Nothing
        //     below writes to them -- ZProjector allocates its own output -- and
        //     sharing avoids duplicating half a gigabyte for a large section.
        //     Anything that ever modifies `sub` in place must copy first.
        def src = imp.getStack()
        def sub = new ImageStack(imp.getWidth(), imp.getHeight())
        zs.each { int z ->
            cs.each { int c ->
                sub.addSlice("c${c}_z${z}", src.getProcessor(imp.getStackIndex(c, z, 1)))
            }
        }

        def gathered = new ImagePlus(imp.getTitle() + "_sub", sub)
        gathered.setDimensions(cs.size(), zs.size(), 1)
        gathered.setCalibration(imp.getCalibration().copy())

        ImagePlus proj
        if (zs.size() == 1) {
            // Nothing to collapse. Must not go through ZProjector: with one z it
            // sees a plain stack of CHANNELS and would project across them.
            proj = gathered.duplicate()
        } else {
            // Flag it as a hyperstack so ZProjector projects each channel
            // separately. Without this a multi-channel stack is treated as one
            // long z-stack and every channel is mixed into one plane.
            gathered.setOpenAsHyperStack(cs.size() > 1)
            proj = ZProjector.run(gathered, zpMethod)
        }

        proj.setDimensions(cs.size(), 1, 1)
        proj.setCalibration(imp.getCalibration().copy())
        cs.eachWithIndex { int c, int i -> proj.getStack().setSliceLabel("ch${c}", i + 1) }
        proj.setTitle(imp.getTitle() + "_" + method.toLowerCase())
        return proj
    }

    /** Original channel numbers of a projection, read from its "ch<c>" labels. */
    static List<Integer> projectedChannels(ImagePlus proj) {
        (1..proj.getStackSize()).collect { int i ->
            def label = proj.getStack().getSliceLabel(i) ?: ""
            def m = label =~ /^ch(\d+)$/
            if (!m) throw new IllegalStateException("plane ${i} is not labelled ch<c>: '${label}'")
            m[0][1] as Integer
        }
    }
}
