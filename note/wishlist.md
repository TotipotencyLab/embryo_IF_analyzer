# Wishlist features

- Watershed for nuclear detection (and for future deferred particle detection)
- `z_process`: make z-projection of each image, then save to a small file. Optionally overlay with ROI (e.g., detected nucleus)
- Groovy study scripts:
  - A script for image metadata inspection (how many series it contains, dimension of each (x,y,z), etc.)
  - A script that inspect the current GUI session - what is currently opened, how many window, working with ROI, etc.
- Introduce sample table (TSV) and config (YAML)
- `--parent_z_pad` for containment: let an inner feature match a parent a slice
  or two *beyond* the parent's own z-range (nucleus at z 5-8 would accept a
  nucleolus at z 4 or 9). Deliberately not implemented -- matching is strictly
  inside `[z_min, z_max]`, and `test-relate_features.R` has the test that would
  have to change.
- Use the parent to *split* child groups. `define_feature_group()` groups
  nucleoli on z-overlap alone, before any parent is known, so two nucleoli in
  adjacent nuclei could in principle merge into one feature. Parent assignment
  currently happens after grouping and cannot undo that. Not built until we see
  it actually happen.
- IF quantification CLI: needs the background measurement question settled
  first (empty space vs cytoplasm, and nucleus vs cytoplasm signal).
- Count an oocyte only where its **nucleolus** is visible, as the way to stop
  one oocyte being counted once per serial section it appears in. A nucleolus is
  roughly one per nucleus and much smaller than the oocyte, so it falls in a
  single section — which sidesteps aligning outlines across sections entirely.
  That alignment is the thing to avoid: a follicle's outline in the next section
  is not a deformed copy of this one, it is a different chord through the object,
  so shape registration is solving a harder problem than the count needs.
  Preferred shape is a **standalone feature-filtering script** rather than a flag
  on the annotate CLI, so the rule is visible in the command that applied it.
  The machinery exists — `relate_features.r --within 'nucleolus=nucleus'` already
  assigns nucleoli to parents. Known bias: a nucleolus straddling a cut is
  counted twice, and the classical corrections for that (Abercrombie, or a
  physical disector over a section pair) need section thickness and order, which
  is what a specimen/section grouping column would have to carry. Deferred.
- `features_config.txt`: the R side's equivalent of Fiji's `_config.txt`. The
  annotate CLI already stamps a `run_id` on its output and `join_feature_table()`
  refuses a join across two runs — but the id is a 10-character hash, so the
  refusal says *that* two tables disagree and nothing about *how*. A sidecar
  written next to `features.rds`, holding the resolved inputs and the effective
  parameters the hash was taken over, turns an opaque mismatch into a diff and
  lets a results directory explain itself months later. The guard works without
  it; this is the diagnosis half. Deferred, not rejected.
- **Blur sigma in physical units** (`nucleus_blur_sigma`, and the nucleolus
  blur and erode, which have the same problem). Today they are pixels, so one
  value is a different physical blur on every pixel size — the batch already
  warns about exactly this in a mixed-size batch. On the oocyte data the
  default 8 px was 1.8 µm on the 0.223 µm series and 3.6 µm on the 0.446 µm
  ones, and the manual threshold then meant different things on each, because
  blur lowers a small object's peak far more than a large one's. Tuning
  (2026-10-06, 10 hand-annotated sections) found 1.8 µm with Manual 32-255 best,
  and blur and threshold trade off — a smaller blur behaves like a lower
  threshold — so the two must be tuned together and a changed blur needs its
  threshold found again. Design questions for when it is built: a unit key
  (`nucleus_blur_unit = px|um`, default `px` so every old config means what it
  meant) or a second key; record the pixel sigma actually applied as
  provenance; anisotropic pixels (`blurGaussian` takes separate x and y sigmas);
  and the mixed-pixel-size warning, which should stop naming a parameter that
  is no longer in pixels. A situational version may exist on an SSD-checkout
  branch of v0.5.1 for the oocyte analysis — look there first.
- **Run the Groovy tests in one Fiji session.** Each `tests/groovy/Test_*.groovy`
  is its own headless launch, and the launch is most of the cost: 12–24 s per
  test, ~2.5 min for the eight, when the tests themselves take a second or two
  (measured 2026-10-06; `Test_RoiExport`, 25 checks on tiny synthetic images,
  still took 12 s). Per launch: JVM start, the SciJava context scanning ~1100
  jars' plugin index, compiling the test and its libraries through
  `GroovyClassLoader`, and Bio-Formats' own start-up where a test reads a file.
  Then the JVM does not exit, so a caller has to watch for the
  `passed: N   FAILED: M` line and kill it. A single runner script that
  evaluates each test file in turn in one session would pay the start-up once
  (estimate ~30–40 s for the set), and could print one combined summary and
  `System.exit` at the end, which would also remove the wait-and-kill. The cost
  to check: tests sharing a session can leak state into each other — ImageJ
  preferences (`blackBackground`, Set Measurements, Bio-Formats'
  `windowless`) persist within a session, and some tests assert exactly those
  preferences are left as found. Keep the one-file-per-launch route working
  for running a single test.

This means the Groovy scripts may have to defined into different levels:
- lowest level: utility function - for small individual step
- interactive session macro: the script that can be run on the current active image window. 
- High throughput analysis: This is the one that is based on the sample table and the config. This can be run headlessly in principle.



- **Detect N named features, not nucleus + nucleolus.** Raised 2026-10-08. The
  pipeline is built around two features with fixed names: `NucleusPipeline`
  runs nucleus then nucleolus, the parameters are `nucleus_*`/`nucleolus_*`,
  and `_threshold_stats.tsv`, `batch_summary.tsv` (`n_nucleus`, `n_nucleolus`)
  and `_config.txt` (`nucleus_count`, …) carry the names in their columns. The
  R side is mostly feature-agnostic already (`feature_type`, the `roi` prefix,
  `relate_features.r --within 'child=parent'`), but about ten R files still
  name the two, most of them in `define_feature_group.r` and
  `annotate_features_cli.r` (counted by grep, not yet read line by line).
  The idea: **a feature is a named block of settings, and the names come from
  the config**, so `nucleus`, `nucleolus`, `embryo`, `oocyte` are all just
  entries. The brightfield detection below is the first feature that is
  neither. Proposal and the questions it raises:
  - **Config format.** Several features each with their own settings do not fit
    a two-column table. Both YAML and JSON are readable on both sides with
    nothing installed: Fiji ships `snakeyaml-2.3` and `groovy-json`, and R has
    `yaml` and `jsonlite`. YAML is the one people edit by hand, and the existing
    entry "sample table (TSV) and config (YAML)" above points the same way.
    Undecided: whether the *record* each result folder carries (`_config.txt`)
    becomes YAML too, or stays a flat two-column file with dotted keys
    (`feature.nucleolus.threshold`). The flat form keeps `read.table` and
    `RunConfig` working and the round trip simple; the nested form is the same
    format as the input. The four-place agreement in `CLAUDE.md` (`PARAM_TYPES`
    ↔ dialog ↔ template ↔ what is written) would become one per-feature schema
    applied to every block. **Old configs must still read:** every results
    folder carries a flat `_config.txt`, and GUI-tune-then-batch depends on
    feeding one back in, so `nucleus_*`/`nucleolus_*` keys translate to two
    feature blocks.
  - **A feature block** would hold: name; channel; an optional pre-processing
    step (none | local SD | SD over z, as tested below); z handling (per slice,
    as now, or once on a projection — a 2-D feature); blur; threshold
    (method/range/stack histogram); size and circularity filters; fill holes;
    watershed; overlay colour; and optionally a **parent**.
  - **Dependency between features.** Today the nucleolus is not independent: it
    is thresholded per nucleus per slice, inside the nucleus ROIs. That becomes
    the general case: a feature with `parent: nucleus` and a per-parent
    threshold scope looks only inside its parent's ROIs, and one without a
    parent is thresholded on the whole frame. Features are detected in
    dependency order, and a cycle is refused. In Fiji, the parent only says
    *where to look*; which child belongs to which parent stays R's job
    (`relate_features.r`), as now.
  - **Output files.** Either keep one set per feature
    (`<series_id>_<feature>_outline.txt`, …), which is today's contract, so the
    R globbing by feature and every older results folder still work, but N
    features means 3N files. Or one combined table per kind with the feature in
    a column (the `roi` prefix already carries it), which means fewer files but
    a contract change, and the measurement columns could differ between
    features. Leaning: keep the per-feature files and change the per-image
    summary tables (`_threshold_stats.tsv`, the batch summary's counts) to long
    format with a `feature` column, since those are the ones that hard-code
    names in column headers.
  - **A 2-D feature is a new shape for R.** An embryo found on a projection has
    no `z`. R's grouping and `relate_features.r` containment both work slice by
    slice, so a parent without z needs a defined meaning, most likely "every
    slice", and "every axis counts from 1" needs a rule for an axis that is
    absent (blank, like `pixel_depth` for a single plane).
  - **R side:** stop defaulting to `nucleus`/`nucleolus` wherever a feature name
    is assumed, and read the feature list from the run's config.
  - **Sequencing.** After `time_axis` (agreed), and after `QoL` (likely). On
    `container` vs this: **this first.** `container` is last on purpose: "see
    the shape of a working pipeline before building a container for it". This
    refactor changes that shape: the config record the container's `series`
    grain reads, the per-feature summary tables, and the 2-D features its `roi`
    grain would hold. Built the other way round, the container would encode
    two-feature assumptions and be reworked right after. `tracking` is less
    affected: it links whichever feature it is pointed at, so making it take a
    feature name, not `nucleus`, from the start is enough.
  - Version: a contract change, so MINOR (below 1.0, `CLAUDE.md` § Versioning).
    Also see "Blur sigma in physical units" above: per-feature settings in
    pixels repeat that problem N times, and an embryo detector's scales (below)
    are pixel numbers tuned on one pixel size.

- **Segment embryos from brightfield** (Luxendo acquisitions have a BF channel;
  most IF data does not). It is one feature config in the entry above, not a
  script of its own. **Feasibility probe, 2026-10-08**, on the FUCCI
  acquisition's extracted TIFFs (`fucci_s0000_L26A_pos1`, t = 1, 25, 75;
  2048×2048×39, 0.208 µm/px, 5 µm z step; channel 1 = BF). The field holds four
  touching round objects about 80 µm across, judging by size embryos, which have
  cleaved by t = 75:
  - **Intensity thresholding the BF directly fails.** Otsu on the blurred mid
    slice follows the illumination falloff across the field, not the objects:
    the mask is 50–57% of the field, and 98% at t = 75. So the current pipeline
    pointed at the BF channel cannot work.
  - **A texture step first works.** The recipe: local standard deviation
    (`RankFilters` VARIANCE, r = 6 px ≈ 1.2 µm, then sqrt) on every slice, the
    **maximum over z** (each object at its own focal plane), Gaussian σ ≈ 8–11 px,
    Otsu (objects bright), fill holes, watershed. At t = 25 this gave exactly the
    four objects, 4,800–6,100 µm² with circularity 0.71–0.86; at t = 1 it split
    one object in two. Without watershed, the touching objects come out as one
    blob of ~21,000 µm².
  - Alternatives tried: local SD on the mid slice alone separates the objects
    cleanly but loses any object out of focus at that slice; SD across z
    (`ZProjector` "sd") finds every object but merges touching ones, and
    watershed splits them less cleanly than the recipe above.
  - **Where it breaks: after cleavage.** At t = 75 watershed cut along the
    blastomere boundaries as well, giving 6 objects, because a contact between
    blastomeres looks like a contact between embryos. Fixes to try: a size or
    shape assumption (expected embryo diameter: merge watershed fragments
    below it, or fit circles of that radius with a Hough transform), or a
    trained model (Cellpose with a fixed diameter, which here would need
    TrackMate-Cellpose and a conda environment, since `Fiji-Cellpose` needs
    Java 21; or a Weka/Labkit pixel classifier).
  - Debris below the embryos is picked up (~900 µm²); a ~2,000 µm² size filter
    removes it.
  - **The case for it:** at t = 75 the FUCCI fluorescence is almost gone (the
    nucleus run on this data found a 0.93% mask there), while the BF still shows
    every embryo. So an embryo outline from BF is the parent that survives when
    the reporter does not, and the natural `--within 'nucleus=embryo'` parent.
  - Not measured: run time per frame (39 per-slice rank filters on 2048²),
    other acquisitions or magnifications, and the zona as a separate outline.
- **Bridge features in time**, as a `--bridge_feature` option (raised
  2026-10-08, planning `tracking`). The time version of `--bridge_roi`, and
  named to match it: what bridges is a rejected feature, handed to TrackMate as
  its centroid. In short: features that R-side filters reject
  (`invalid_<feature>_NNNN`, e.g. below `min_z_span`) are still handed to
  TrackMate, as spots that may keep a track continuous but are never counted
  or reported as track members. The motivating case is mitosis: at nuclear
  envelope breakdown the DNA stops looking like a nucleus, and a frame or two
  of it rejected breaks the track between a mother and her daughters.
  - It needs those features in the centroid table, flagged, which they can be:
    R grouped them, so they have geometry and a centroid. After tracking, a
    bridge spot is dropped and the links through it kept, as a gap-closed
    link.
  - ⚠️ **A bridge can steal a link** (raised by the user). TrackMate's LAP
    tracker minimises the total cost of all links, so a bridge centroid
    (debris, a fragment) that happens to sit nearer a moving nucleus's next
    position than the nucleus itself takes the link — breaking the real
    track, or, once the bridge spot is dropped, joining two unrelated objects.
    A quality-difference penalty (`LINKING_FEATURE_PENALTIES` on `QUALITY`)
    would make real→bridge links dearer, but leaves bridge→bridge ones cheap.
    The safer design is **two passes**: track the real features alone, so
    every real-to-real link is settled first; then offer a bridge only to
    fill a gap between one track's end and another's start, near both. A
    bridge then never competes with a real feature. Test with a decoy: a
    bridge placed nearer than the real next feature must not take its link.
  - It cannot recover what Fiji itself rejects. `nucleus_circularity` filters
    inside `Analyze Particles`, so those ROIs never reach `_outline.txt`
    (CLAUDE.md) and have no centroid to hand over.
  - Not built until data shows TrackMate's own gap closing is not enough;
    `tracking` PR 2's synthetic division-after-a-gap test is the first
    evidence either way.
- **The overlap-core refactor, and an overlap linker to cross-check TrackMate**
  (moved out of `tracking` 2026-10-08). `find_overlap_roi_features()` scores
  an overlap against the *smaller* of the two areas, so its ratio means
  "fraction of the child inside the parent" only when one object is much
  larger than the other. It also throws its scores away, lets one failing
  slice kill a match, never resolves matches one-to-one, and does not keep
  time points apart. `relate_features.r` already gets containment right. The
  fix is to extract that core into one shared function -- a directional
  denominator, the scores kept, one-to-one matching with ties recorded, and
  the partition keys (`z`, `t`) as an argument -- and retire the old one. The
  review, R1-R5, and the callers (`test-spatial.R` and PLA scripts; PLA is
  published, so edit those for future work only) are in
  `time_series_plan.md` §7.
  - Its second caller was to be a frame-to-frame linker matching features by
    how much their outlines overlap: the in-house alternative to TrackMate.
    With TrackMate doing the linking, that is worth building only as a
    **cross-check** (low priority), and this refactor is its prerequisite.
  - Until then it is a standalone cleanup with one real caller. It would also
    delete the old function's `if(F){...}` block, a test scratchpad with a
    hardcoded `/Volumes/pool-toti-imaging/...` path.
- **A real time-lapse fixture for tracking** (2026-10-08). `tracking` can only
  be verified on synthesised data: the FUCCI acquisition's nuclear channel is
  sparse and dim at later time points, and its per-frame segmentation is
  already poor, so it cannot say whether z should count in the linking
  distance (`time_series_plan.md` §9, question 1), i.e. what `useZ` should
  default to. Either a new acquisition with a stable nuclear marker, or this
  one segmented from brightfield (the entry above), would do. The FUCCI series
  is still worth one tracking run, to see whether it can stand in for now.
