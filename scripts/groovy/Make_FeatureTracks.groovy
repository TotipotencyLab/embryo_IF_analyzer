#@ String  (visibility=MESSAGE, value="Link annotate's features across time with TrackMate", required=false) help_title
#@ File    (persist=false, label="annotate output directory", description="annotate_features_cli.r's --outdir: every <series>_feature_centroids.tsv in it is tracked, one series per file, each on its own. The tracks are written beside them.", style="directory") inputDir
#@ String  (persist=false, label="Feature type to track", description="As annotate reports it -- after --rename, the new name. One type per run.", value="") featureType
#@ String  (persist=false, label="Use z in the distance (true / false)", description="true: the full calibrated 3-D distance. false: x-y only. No default: with a 5 um z step one slice of centroid wobble costs 5 um, which can break the track of a nucleus that never moved (note/time_series_plan.md 5.4). Choose it for the data.", value="") useZ
#@ Double  (persist=false, label="Linking max distance", description="Frame to frame, in the centroid table's units (um). A link must be strictly shorter.", value=15.0) linkingMaxDistance
#@ Integer (persist=false, label="Max frame gap", description="Frames a link may span when a feature goes missing: 2 bridges one missing frame, 1 bridges none. Counted in the series' time points, so with frames 1,10,20 one step is ten time points.", value=2) maxFrameGap
#@ Double  (persist=false, label="Gap-closing max distance", value=15.0) gapClosingMaxDistance
#@ Double  (persist=false, label="Splitting (division) max distance", value=15.0) splittingMaxDistance
#@ Boolean (persist=false, label="Allow merging", description="Off: two features becoming one ends one track there (a segmentation merge shows as a break). On: both lead into the merged feature -- the honest record when two objects really are segmented as one for a while, as pronuclei are.", value=false) allowMerging
#@ Double  (persist=false, label="Merging max distance", value=15.0) mergingMaxDistance

// Make_FeatureTracks.groovy
//
// annotate's centroid tables -> TrackMate -> one table of links per series.
// A `#@` block and one call; the work is in FeatureTracks.groovy so it can be
// tested without a dialog.
//
// Writes, beside each <stem>_feature_centroids.tsv, for the type tracked:
//
//   <stem>_<type>_tracks.tsv        one row per link (feature_id <- prev_feature_id),
//                                   and a start row for every feature nothing leads to
//   <stem>_<type>_track_params.txt  the settings used, and what they made
//   <stem>_<type>_track_edits.tsv   hand corrections: seeded empty, kept once it holds edits
//
// Tracks are NOT copied into the feature table. R joins them at use
// (join_tracks()), which is also where track and branch ids are numbered.
//
// Headless:
//
//   ImageJ-macosx --headless --console --run scripts/groovy/Make_FeatureTracks.groovy \
//     "inputDir='/path/R/feature',featureType='nucleus',useZ='false'"

import ij.IJ

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
if (libDir == null || !new File(libDir, "FeatureTracks.groovy").exists()) {
    IJ.error("Cannot locate the Groovy library files.\n\nSave this script into scripts/groovy/ and run it from there.")
    return
}
def LIBDIR = libDir.getAbsolutePath()

def FT = new GroovyClassLoader(this.class.classLoader)
             .parseClass(new File(LIBDIR, "FeatureTracks.groovy")).load(LIBDIR)

IJ.log("=== Feature tracks (TrackMate) ===")
IJ.log("  input : " + inputDir?.getAbsolutePath())
long t0 = System.currentTimeMillis()
def res = FT.trackDirectory(inputDir,
    [feature_type            : featureType,
     use_z                   : useZ,
     linking_max_distance    : linkingMaxDistance,
     max_frame_gap           : maxFrameGap,
     gap_closing_max_distance: gapClosingMaxDistance,
     splitting_max_distance  : splittingMaxDistance,
     allow_merging           : allowMerging,
     merging_max_distance    : mergingMaxDistance]) { IJ.log(it) }

def edits = res.series.countBy { it.edits }
IJ.log("")
IJ.log("  edits tables: " + edits.collect { k, v -> v + " " + k }.join(", "))
IJ.log("  took " + String.format("%.1f", (System.currentTimeMillis() - t0) / 1000.0d) + " s")
IJ.log("Done: " + res.series.size() + " series tracked")
