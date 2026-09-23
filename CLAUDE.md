# embryo_IF_analyzer — working notes

A collection of imaging-analysis scripts, not a single pipeline. It began with
immunofluorescence of preimplantation embryos ("IF") and has fanned out to other
assays (PLA, oocyte counting in tissue sections). Expect per-assay entry points
sharing a common library, not one program.

`README.md` is for people *using* the repo. This file is for working *on* it.

## Workflow

1. **Orient.** Check the current state against the repo, not against memory or
   this file. Both go stale; the code does not.
2. **Branch.** Never commit to `main`.
3. **Change** one thing. If a new assay needs behaviour the library lacks, add
   the option to the library — do not fork a script.
4. **Verify.** This is the step that earns its keep here, because the
   characteristic failure in this repo is *silent*: measurements stay correct
   while a join matches nothing, a filter switches units, or a mask looks
   plausible and is wrong.
   - Changing anything that writes the output files ⇒ re-run a reference and
     **diff**. The ROI counts agreeing is not the check; the files agreeing is.
   - A new option that should change nothing when off ⇒ prove it changes nothing
     when off, and *separately* prove it does something when on. Identical
     output can mean "correctly did nothing" or "silently never ran" — those
     look the same and must be told apart, if necessary on a case you
     synthesise (`tests/groovy/Test_BuildMask.groovy`).
   - Never report a pass that could not have failed. Print the value observed,
     not the word "OK".
   - Say which R ran; the suite's counts differ between 4.4 and 4.6.
5. **Commit** with the reasoning, not the diff. Why it was wrong and how the
   failure showed up is what is worth reading in a year. Note explicitly what
   was *not* verified.
6. **PR, squash merge**, tag if releasing (bump `VERSION` in the tagged commit).
7. **Doc-sync.** Two different things, and they are not handled the same way.

   **a. Format documentation travels with the change — write it, do not
   propose it.** If the work altered any table this repo reads or writes — a
   column, a filename pattern, a `_config.txt` field, a sample sheet rule, a
   `feature_id` prefix — then `note/data_formats.md` is now wrong and must be
   corrected in the same commit, along with `README.md` and
   `config/*template*` where they are affected. `note/data_formats.md` is the
   authoritative description of the shapes; `CLAUDE.md` explains why some are
   dangerous; `README.md` covers the subset a user needs. A fact belongs in one
   of them, referenced from the others — duplicating it guarantees drift.

   `tests/testthat/test-data_formats.R` asserts the documented columns against
   the code, so a format change that skips the doc shows up as a test failure
   rather than as a surprise months later. Update the test with the doc.

   **b. Captured knowledge — propose and stop.** Anything the work *revealed*
   rather than changed: a gotcha worth recording, a stale claim in `CLAUDE.md`
   or `.claude/skills/`, a new note, a new skill worth having. **Raise it as a
   proposal and stop.** Draft the edit only once it has been agreed; what is
   worth writing down is not the author's call alone.

Before deleting anything, confirm git actually holds it — `**/tmp/` and
`**/data/` mean "it is on disk" and "it is in history" are different questions.

## The output contract

Fiji writes, per feature per image:

```
<prefix><image id>_<feature>_outline.txt        name, roi, z, x, y   (one row per polygon vertex)
<prefix><image id>_<feature>_outline_ROIs.zip   ImageJ ROIs
<prefix><image id>_<feature>_res.txt            measurements, one row per ROI per channel
<prefix><image id>_config.txt                   every parameter used for that run
```

**This is a contract, not an implementation detail.** The field-level
description lives in `note/data_formats.md`; what follows is why it is
dangerous. Two things depend on it and break silently if changed:

`_config.txt` records `image_width` and `image_height` in **pixels** alongside
`pixel_width`/`pixel_height`. The outline tables are in calibrated units, so
these are what lets the R side reconstruct the image frame — the bounding box of
the detected objects is not the frame, and a QC panel drawn without them is
cropped differently from the Fiji overview PNG it is meant to sit beside.
`montage_qc_cli.r` warns loudly rather than producing a misaligned montage.

- `scripts/R/read_fiji_result.r` identifies the roi column by matching
  `\d{4}-\d{4}-\d{4}$` and joins measurements to outlines through the roi id
  **embedded in the `Label` column**. The measurement numbers can be perfectly
  correct while the join yields nothing.
- The measurement columns come from `Set Measurements`, which is a *persistent
  Fiji user preference*. The Groovy scripts force it explicitly:
  `area mean standard min centroid shape integrated median stack display`.
  Never rely on the operator's Fiji settings.

ROI names are `<feature>_SSSS-NNNN-YYYY` — slice, per-slice index, y-centre of the
ROI bounds. Reproduced in Groovy so output stays compatible after moving off the
ROI Manager.

When changing anything that writes these files, verify against a reference run
rather than by eye. See the `fiji-headless-testing` skill; it describes the whole
loop, including how to diff.

## Which scripts are current

- **`scripts/groovy/` is the supported Fiji path.** `Run_NucleusSelector.groovy`
  is the main entry point. `NucleolusDetect`, `RoiExport` and `RoiDetect` are the
  shared library, split by role (detect / export / orchestrate) rather than by
  experiment. `Inspect_*.groovy` are read-only diagnostics, written to be read as
  well as run — they carry the Groovy/ImageJ API notes.
- `Overview.groovy` builds the quick-look PNGs (project → prepare → addOutlines →
  savePng, each usable alone). Its `merged` outline mode unions ROIs **in the 2D
  projection**, so objects overlapping in x-y share one outline: it is a picture,
  not a count. On the fixture it draws 5 outlines where R groups 6 nuclei.
- Mask building lives in `RoiDetect.buildMask()`, not inline in the runner, so a
  new assay configures it rather than copying it. Watershed is an option there
  and is **off by default**: with it off the mask step is exactly what it was
  before extraction, verified against the fixture.
- **`scripts/fiji/` is reference only.** The IJ1 macros are kept deliberately, for
  code study and comparison. `PLA.ijm` and `measure_manual_selection.ijm` are
  still the only implementations of those two workflows.
- `scripts/R/` holds functions `source()`d by analysis scripts, not a package.
  `plot_features_topView()` / `union_features()` / `flip_y_image()` are the
  geom_sf-based plotting path; `plot_outline_topView()` is the older
  geom_polygon one and **cannot draw a union** (it flattens geometry to x/y, so
  holes and MULTIPOLYGONs come out wrong, silently).
- **`scripts/R_cli/` is the R command-line path**: `annotate_features_cli.r`
  (outlines → features, containment, + QC plot), `count_features_cli.r`
  (features → tidy counts, the oocyte deliverable), `feature_stat_cli.r`
  (per-feature statistics and their distributions), `feature_scatter_cli.r`
  (two statistics against each other) and `montage_qc_cli.r` (the 3-panel
  check).

  **`feature_stat_cli.r` is the threshold-finding step**, and the unit is the
  feature, not the ROI — adjacent z-slices of one object share signal through
  the point-spread function, so ROIs are not replicates. It joins the Fiji
  `_res.txt` onto features and aggregates per channel, **area-weighted by
  default**: a plain mean lets an object's small tapering end slices vote as
  loudly as its equator. The measurement tables are located by the **`roi`
  column's prefix**, not by `feature_type`, so the lookup survives `--rename`.
  Not finding them warns loudly rather than quietly producing a table with no
  signal in it. `note/if_quantification.md` covers what these numbers do and do
  not yet support — there is no background correction, so intensities are not
  comparable between images.

  `feature_scatter_cli.r` reads that CLI's **`feature_stats.tsv`**, not the
  `.rds`: the expensive work happens once and the exploration end stays cheap to
  re-run. Its `--threshold` is keyed to a **column, not a panel** — a cut-off is
  a fact about a variable, so it is drawn wherever that variable appears, as a
  vline when it is x and an hline when it is y. That deliberately removes the
  whole question of matching lines to plots by position, and with it the recycle
  / skip / off-by-one failures that come with it.
  `cli_helpers.r` is shared by all three. Conventions — the testable
  `<name>_cli(args)` function, the run guard, argparser's traps — are in the
  `r-cli-convention` skill. The IF quantification CLI is deliberately deferred:
  the background-measurement question is unsettled. `note/if_quantification.md`
  records what that decision will involve.

  **Identity comes from the file's content, not its name.** The `name` column
  holds the sample, the `roi` prefix holds the feature; the filename only has to
  select the right files. The previous filename parse used a greedy prefix and
  so mis-split any feature name containing `_` — `S1_growing_oocyte_outline.txt`
  became sample `S1_growing`, feature `oocyte`, silently. A greedy prefix is
  right when reading the feature out of an *ROI id*, whose tail is anchored and
  fixed-shape, and wrong for a filename, which has no such anchor.

  **A pre-grouping ROI filter can split one object into two.** `--min_circularity`
  and `--roi_area` drop ROIs before the overlap graph is built, so removing
  interior slices opens a z-gap that `max_z_dist` cannot bridge. Observed on real
  data: a circularity cut removed an oocyte's widest cross-sections and one
  object was counted as two. `--feature_area` acts after grouping and cannot do
  this — prefer it. Any statistic computed after such a filter is also a
  *truncated* one, so thresholds tuned against it are not portable to a run with
  a different cut.

  `relate_features.r` places inner features inside outer ones. **The biology is
  declared, never hardcoded**: `--within 'nucleolus=nucleus'`. Containment is
  the fraction of the *child* inside the parent, summed slice by slice — not on
  the flattened 2D union, which would misassign a child when two parents
  overlap in x-y. When the parent was not detected on a slice the child
  occupies, the match falls back to the parent's 2D footprint **only inside the
  parent's own z-range**, and is recorded as `gap_filled` rather than `direct`.
  A run leaning heavily on that is telling you the *parent* detection needs
  work. Gap-filling never modifies the parent — relating features must not
  rewrite them. Orphans are kept with `parent_feature_id = NA` unless
  `--require_parent`, because an orphaned nucleolus is evidence about nucleus
  detection and dropping it destroys the evidence.

  R's per-`feature_id` union is **z-aware**, so it keeps objects separate that
  Fiji's projection union merges. On the fixture: 70 nucleus ROIs → 6 nuclei in
  R, 5 outlines from Fiji, because one pair overlaps in x-y while sitting 30
  slices apart. Two further nuclei are visible but fail `min_z_span` and are
  reported as `invalid_`, not dropped — hence 8 visible, 6 counted.

The macros were forked per experiment because the IJ1 macro language has no import
mechanism — that is the problem the Groovy split exists to solve. **Do not add a
fourth fork.** A new assay should be a new configuration of the shared library.

## Standing decisions

- **Watershed forces `Prefs.blackBackground = true`.** It reads that preference
  to decide which phase is object; left to the operator's setting it erodes the
  background instead of splitting objects, and produces a plausible-looking mask
  while doing it. Same reasoning as forcing Set Measurements — and it likewise
  persists. Holes are filled before splitting, or watershed cuts through an
  unfilled hole and shatters one object into a ring of fragments.
- **Nucleolus thresholding:** `Default` and `Relative` work on real embryo DAPI.
  `Otsu` and `Triangle` mask essentially the whole nucleus — once the histogram is
  restricted to a single nucleus it is no longer strongly bimodal.
- **Nucleoli are thresholded per nucleus, per slice**, on the un-inverted image.
  Inverting made the extranuclear background the brightest thing present, which
  pulled the auto-threshold away from the nucleolus/nucleoplasm boundary.
- **`nucleolus_restrict_to_nucleus` in `nucleus_selector.ijm` is known-broken and
  disabled.** It can only compute one threshold from one arbitrary slice over the
  z-flattened union of nuclei. Kept, commented, as the record of why that work
  moved to Groovy.
- **`Run_NucleolusDetect.groovy` intentionally still uses the ROI Manager.** It is
  the "nucleoli only, from ROIs already in the manager" entry point, which is
  inherently interactive.
- **PLA is published.** The analysis lives in `PLA_analysis/`; the scripts used for
  the paper are committed in a separate repository. `PLA_analysis/` may be edited
  for *future* PLA work, but it is the worked example of the R side end to end.

## Data and fixtures

Nothing image-sized is tracked. `.gitignore` specifics worth knowing:

- `**/data/` ignores bulk data everywhere, then `!fixture/*/data/` un-excludes
  the fixture ones so their text tables can be tracked. The order matters and so
  does the directory: git will not look inside an excluded directory, so a
  negation on files alone never gets the chance to match.
- `*Position[0-9]*.txt` ignores raw Fiji dumps. **macOS sets
  `core.ignorecase=true`, so this matches lowercase `position...` too** — it once
  silently hid the entire fixture directory. `!fixture/**/*.txt` re-includes
  fixture text.
- `CLAUDE.local.md` is per-machine and untracked.

`fixture/if_data/data/` holds the Groovy output for `Position010` and **is
tracked** — five text tables, 1.1 MB, the input for the R tests. Getting them
tracked needed `!fixture/*/data/`, because git does not descend into a directory
excluded by `**/data/` and so a negation on the files alone can never fire. The
TIFF stacks in `fixture/*/raw_data/` and the ROI zips stay out via `*.tif` /
`*.zip`.

## Tests

`Rscript tests/run_tests.R` (`tests/testthat/`, testthat). The suite asserts the
output contract itself — that the roi id is recovered for **every** measurement
row, and that reading a table neither adds nor drops rows — because the numbers
can all be right while the join silently matches nothing.

The spatial tests need a working `sf`. `helper-setup.R` probes it **in a child
process**, since a broken `units` aborts R outright rather than raising, which
would take the whole run down; when it cannot load they skip rather than fail.
Note which R ran: the count differs. See `CLAUDE.local.md` for this machine.
Under R 4.6 with the full package set the suite is **417 passed / 0 skipped**.
`test-data_formats.R` pins the documented column sets against the code, so a
format change that skips `note/data_formats.md` fails a test.

⚠️ The suite runs **testthat edition 2** (no package `DESCRIPTION` to declare
edition 3), where `expect_warning()` returns the expression's **value**, not the
condition — `conditionMessage()` on the result fails. Assert warning text
through the `regexp` argument.

On the Fiji side, `tests/groovy/` synthesises its images, so those tests need no
data: `Test_BuildMask` (watershed), `Test_Overview` (projection, contrast,
resize, outlines, PNG), `Test_RoiExport` (ROI zip round trip) and
`Test_RunConfig` (the run config, including that `Run_NucleusSelector.groovy`
still compiles — it is parsed with its `#@` lines stripped, since those are
SciJava directives and not Groovy).

```
/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
  --run tests/groovy/Test_Overview.groovy
```

Read the FAILED count, never a bare "OK" — every check prints the value it saw.

## Versioning

Versions are **git tags / GitHub releases**, not per-file headers. Do not
reintroduce a `// version x.y.z` line when editing a script.

One exception, and it is deliberate: `VERSION` at the repo root holds the same
string the tag names, and `RoiExport.repoVersion()` reads it at run time so that
`_config.txt` can record which code produced a results directory. Provenance has
to travel with the output — a results folder is often read long after, on
another machine. Bump `VERSION` in the same commit you tag.

A script copied out of the repo still runs; it records `unknown`.

## Open items

- The R fixture covers one image (`Position010`) and one assay. Nothing pins the
  PLA or oocyte paths.
- `scripts/tmp/define_nucleus.r` exists on disk but `**/tmp/` ignores it, so it
  is **invisible to git and has never been committed** — there is no blob for it
  in any branch. It is an early graph-based (`igraph`/`tidygraph`) prototype of
  the ROI-grouping idea that became `define_feature_group.r`, not a copy of it.
  Deleting it destroys it. Promote it or delete it deliberately; do not assume
  git holds a copy.
- `Run_*.groovy` resolve their library directory from the SciJava script binding,
  so they must be *saved* and run from `scripts/groovy/` — an unsaved Script
  Editor buffer has no path and will fail with an explicit message.
