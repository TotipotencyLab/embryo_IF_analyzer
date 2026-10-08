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
   A milestone that changes the contract over several PRs may have a **home
   branch** instead (`time_axis`): its PRs are `<home>-<what>` branches
   squash-merged into it, it carries `VERSION` `<next>-dev` so its output cannot
   pass for the last release, and it is merged into `main` — a normal merge —
   once all of them are in, with the release `VERSION` as its last commit and
   the merge commit tagged. `main` then never holds a half-changed contract.
7. **Doc-sync.** Two different things, and they are not handled the same way.

   **a. Format documentation travels with the change — write it, do not
   propose it.** If the work altered any table this repo reads or writes — a
   column, a filename pattern, a `_config.txt` field, a series table rule, a
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
<series_id>_<feature>_outline.txt        name, roi, t, z, x, y   (one row per polygon vertex)
<series_id>_<feature>_outline_ROIs.zip   ImageJ ROIs
<series_id>_<feature>_res.txt            measurements, one row per ROI per channel per frame
<series_id>_config.txt                   every parameter used for that run
<series_id>_threshold_stats.tsv          what the nucleus threshold did, one row per frame
```

An image may have several frames; each is analysed as an image of its own, and
**every image axis this repo writes counts from 1** — channel, z and t, as
ImageJ shows them (`note/fiji_vocabulary.md`). A single frame is `t = 1`.

⚠️ **The file stem and the `name` column are one string.** The R side reads the
series id out of `name` and finds `_config.txt` and `_res.txt` by it, so a
prefix in the file names but not in `name` hides both — no `volume`, no signal
columns, and only a warning. That is why v0.7.0 *retired* `output_prefix` rather
than taking it out of `name` alone: an interactive run's id is the `Series id`
field, or the image title when that is blank.

**This is a contract, not an implementation detail.** The field-level
description lives in `note/data_formats.md`; what follows is why it is
dangerous. Two things depend on it and break silently if changed:

`_config.txt` records `image_width` and `image_height` in **pixels** alongside
`pixel_width`/`pixel_height`/`pixel_depth`. The outline tables are in calibrated units, so
these are what lets the R side reconstruct the image frame — the bounding box of
the detected objects is not the frame, and a QC panel drawn without them is
cropped differently from the Fiji overview PNG it is meant to sit beside.
`montage_qc_cli.r` warns loudly rather than producing a misaligned montage.
`pixel_depth` is blank for a single plane rather than ImageJ's default 1.0 — a
z step that does not exist must not arrive as a usable-looking number. The file
is also **readable back in** as a run's parameters (`RunConfig.groovy`), which
is why an unknown key there is an error rather than a shrug.

**Every `PARAM_TYPES` key must be written back, and a test says so.** An absent
key is *not* an error — `readParams` rejects unknown keys, not missing ones — so
a parameter the run forgot to write silently becomes its DEFAULT on the next
run. That is the round trip failing while looking like it worked, and it
happened three times: the `overview_*` keys, `nucleus_circularity`, and the five
`save_*` switches.

Four things must agree whenever a parameter is added. Three already had a
set-difference assertion and have never drifted; the fourth is the one that kept
breaking, and now has one too:

| must agree | asserted in |
|---|---|
| `PARAM_TYPES` ↔ the `#@` dialog variables | `Test_RunConfig` |
| the dialog's literal `value=` ↔ `DEFAULTS` | `Test_RunConfig` |
| `PARAM_TYPES` ↔ `config/nucleus_config_template.txt` | `Test_RunConfig` |
| `PARAM_TYPES` ↔ what `saveRunConfig()` actually writes | `Test_NucleusPipeline` |

Do not replace these with a checklist. A checklist is something a person has to
remember to read; a set difference is something that fails. There is no
exclusion: `output_prefix` was the one, and is retired. The series id names the
output rather than deciding the analysis — the same category as `outdir` — so
it is not a parameter at all, and is written as provenance. A retired key in an
older config is skipped, not refused (`RunConfig.RETIRED_KEYS`): every
`_config.txt` before v0.7.0 carries `position_pattern`.

**The series table does not need any of this, and the difference is the point.**
`schema/sheet_columns.tsv` is read at *run time* by `SheetSchema.groovy` and by
`cli_helpers.r`, so there is one source of truth and nothing to keep in sync.
The run config has no equivalent: its schema is `PARAM_TYPES`, a Groovy map the
R side cannot read, and the template is a generated duplicate of it. That is why
one needs tests where the other does not — and why making the sheet look more
like the config would be a step backwards.

- Measurements join outlines on **`(roi, t)`**. Since v0.8.0 `_res.txt` carries
  `roi`, `z`, `t` and `ch` written by Fiji; `read_fiji_result.r` takes them as
  given and stops if the `Label` disagrees. For an older table it still digs
  the roi id **out of the `Label` column** — the measurement numbers can be
  perfectly correct while that join yields nothing.
- The measurement columns come from `Set Measurements`, which is a *persistent
  Fiji user preference*. The Groovy scripts force it explicitly:
  `area mean standard min centroid shape integrated median display`.
  Never rely on the operator's Fiji settings. ⚠️ **Not `stack`**: ImageJ's
  `Ch`/`Slice`/`Frame` mean different things on different image shapes
  (`Slice` is the *time* on a 1-channel, 1-slice time course), and beside our
  own `ch` they collide once R lower-cases the names.

ROI names are `<feature>_SSSS-NNNN-YYYY` — slice, per-slice index, y-centre of the
ROI bounds — with a leading `TTTT-` (the frame) when the image has several
frames: four frames of one object would otherwise share a name, which the ROI
zip refuses. Every reader takes both shapes. Reproduced in Groovy so
single-frame output stays compatible after moving off the ROI Manager.

When changing anything that writes these files, verify against a reference run
rather than by eye. See the `fiji-headless-testing` skill; it describes the whole
loop, including how to diff.

## Which scripts are current

- **`scripts/groovy/` is the supported Fiji path.** `NucleolusDetect`,
  `RoiExport` and `RoiDetect` are the shared library, split by role (detect /
  export / orchestrate) rather than by experiment.
  **`NucleusPipeline.groovy` is everything that happens to one open image** —
  detect, export, measure, overview, run config — and `Run_NucleusSelector.groovy`
  is a `#@` block plus one call into it. Same division as the R side
  (`scripts/R/` holds the work, `<name>_cli.r` holds the parsing), pushed
  further: a `#@` script can only be tested by stripping its parameter lines and
  injecting a `Binding`, so anything worth testing belongs in the class. A second
  caller — the batch runner — is the reason it was split out.
- **`schema/` is internal, `config/` is yours.** `schema/sheet_columns.tsv` is
  read at run time by both languages and says what the columns of `files.tsv`,
  `series.tsv` and `sources.tsv` are and who owns each (`machine` overwritten on
  regeneration, `seeded` written once then yours, `user` never touched). It is
  not in `config/` because that folder's contract is "copy one out and edit it;
  nothing here is read automatically".
- Script name prefixes are a contract, but a loose one — **none of these verbs
  has a strict meaning, and trying to give them one is how you end up with a
  name nobody would choose.**

  - **`Make_*`** — *you can expect these files at the end.* The name says what
    comes out. Usually the pipeline then consumes it (`Make_SeriesSheet`), but
    it does not have to: `Make_LuxendoTiff` makes TIFFs you may simply look at
    and be happy with. The promise is the artifact, not the consumer.
  - **`Run_*`** — *executes something.* The vaguest term here, and deliberately
    so: it began as "a wrapper that runs some library function", and it is what
    a script gets called when naming its output would undersell it.
    `Run_NucleusSelector` could have been `Make_NuclearOutline` — it does write
    outlines — but it does more than that, and the narrower name would be a
    worse description.
  - **`Inspect_*`** — read-only diagnostics, written to be read as well as run;
    they carry the Groovy/ImageJ API notes.
  - **`Open_*`** — opens one thing into a window for a human to look at.
    Inherently interactive, never part of a batch.

  The line between `Make_` and `Run_` is judgement, not rule. Do not invent a
  fifth verb without adding it here.
- `Inspect_ImageFile.groovy` lists what is inside a file without opening it —
  series, dimensions, calibration — and with `checkPixels` reports the
  percentage of non-zero pixels per series. That last one matters: a series that
  was allocated but never written reads as a perfectly well-formed stack of
  zeros, and segments to nothing without complaining. It also groups repeated
  series names and says whether they are separate fields of a tile scan or
  genuinely the same image, which the name alone cannot tell you.
  `Open_LifFile.groovy` opens one chosen series into a window, by index or name.
- **The Luxendo path is two scripts and two tables.** Bio-Formats cannot read
  `.lux.h5` correctly — `BDVReader` returns the wrong specimen's pixels — so
  `LuxendoFile.groovy` reads the HDF5 directly through JHDF5 for pixels, and
  `LuxendoSidecar.groovy` reads the `.json` Luxendo writes beside every image
  for everything else.

  **A series is the whole position — x, y, z, c and t.** That is Bio-Formats'
  own meaning of "series", and how it presents `bdv.xml` — always, since v0.7.0
  removed `gatherFrames`; a time point is taken out at use, as
  `Make_LuxendoTiff`'s `frames`. The consequence to know before touching the batch: a 96-frame
  position is ~94 GB against a ~9 GB heap, so nothing may hold a Luxendo series
  whole. `SeriesSource.groovy` hands the batch one frame at a time: each frame is
  analysed, staged under `.staging/<series_id>/` and released before the next is
  read, and the series' files are joined from the staged frames at the end.
  **A rerun resumes from them**: finished frames are not read again, and
  `settings.txt` beside them makes a rerun under other settings stop rather than
  join frames analysed two ways. The skipped frames' ROIs come back from the
  staged zip, which is why it is staged even with `save_roi_zips` off. What a
  rerun does with output already there — resume, also skip finished series
  without opening them, or redo — is the batch's `existingOutput`
  (`note/data_formats.md`).

  **`Make_LuxendoSheets.groovy` writes `series.tsv` + `sources.tsv`.** Two
  tables because Luxendo breaks the assumption every other format here
  satisfies: one series spread **across** several files (one per channel, one
  per time point) rather than one or more series **inside** one file. A series
  row cannot hold more than one `path`, so the file facts go in the second
  table. `series.tsv` is the same series table every format writes, so every R
  CLI reads it unchanged, and it is where `include` lives. ⚠️ Its `path` is the
  **acquisition directory**, not a file: `path`, `series_index`, `series_name`
  and `alias` are defined so they are true for every format — `series_index` is
  "the index the container addresses a series by", the stack here.

  ⚠️ **`sources.tsv` joins the series table on `(alias, series_index)`, never on
  `series_id`.** `series_id` is yours to edit for readability; a join on it would
  orphan every source row the moment you did. Both key columns are machine-owned,
  and the alias half keeps two acquisitions in one table from colliding on a
  stack number. There is one join, `LuxendoScan.withSeriesId()` — use it.

  **`Make_LuxendoTiff.groovy` is for tuning and drag-and-drop, not a required
  step.** The batch reads the `.lux.h5` through the same two tables
  (`sourcesFile`) and never touches what this writes — deliberately: the sources
  *are* the pixels and TIFF does not compress them, so a mandatory conversion
  would mean holding two copies of an 800 GB acquisition. A row whose series is
  in the sources table is read from the sources, any other is opened as a file,
  so one sheet can hold both. For `Make_LuxendoTiff`, set `include=false` on all
  but a few series first, and choose time points with `frames` — each becomes
  its own `<series_id>_t<TTTT>` file unless `oneFile` gathers them into one.
  ⚠️ The two routes do not give identical files for the same time points: the
  batch's `t` is the acquisition's time point where a gathered TIFF's counts
  from 1 again, and the TIFF stores the pixel size as float32, so calibrated
  measurements (`Area`, `X`, `Y`, `IntDen`) differ by up to 1 part in 10^5.
  Outlines and pixel statistics agree exactly.

  ⚠️ **The series id carries the alias**, built with the repo's own
  `composeSeriesId()` rather than a second copy of the rule. `s<NNNN>_<stack
  description>` repeats between acquisitions — two real ones shared all 14 stack
  identities — so without it two runs overwrite each other's results in a shared
  output directory. The alias is the operator's, defaulting to the folder name.

  **The file list comes from `bdv.h5` + `bdv.xml`** when Luxendo wrote them
  (`listing=auto`): one external link per (time point, setup), so `raw/` is not
  walked — ~1 minute against ~3–4 over samba. The index is a file list and a
  cross-check, never identity, and every placed file is checked against it. ⚠️
  **`bdv.xml`'s `<tile>` is not the stack number** — it is the setup's ordinal in
  *text* order of the stack (0, 1, 10, …, 2) and matches for 6 of 42 setups.
  The index is written at the end of an acquisition and can be stale;
  `listing=walk` is the only route that sees a file it does not list.

  Identity comes from the sidecars, never from a path. On the walk a row needs
  **both** the `.lux.h5` and its `.json` — which is also the index-file test,
  since `main_raw.lux.h5` has no sidecar; on the index route with `quickScan`,
  only one sidecar per directory is read, so the others are not checked for.
  The one narrow exception is `quickScan`, which takes the *time point* from
  the filename after confirming
  the mapping against a real sidecar in the same directory, and falls back to
  reading every sidecar when they disagree. Whether a position's time points
  become one file or many is settled in the tables, at scan time, not by the
  assembler. Downscaling is a **percentage of the original**, and the
  calibration is scaled by the ratio *achieved* rather than requested, because
  the pixel count is rounded. `note/luxendo_file_format.md` is what the format
  is; `note/data_formats.md` §1 is the two tables.
- **`Run_Overview_Batch.groovy` is the cheap look at a dataset**: overview PNGs
  for every included sheet row and nothing else — a TIFF per channel, a page
  per frame, for a multi-frame series, with `sourcesFile`/`frames` as the
  nucleus batch takes them. It exists because deciding what
  a slide contains should not cost a segmentation run — detection needs a tuned
  config you cannot write until you have seen the images. Its PNGs carry the
  same `<series_id>_overview_ch<N>.png` names the nucleus path writes, and are
  byte-identical for the same settings, so nothing downstream needs to know
  which runner made one. The multi-frame TIFF is drawn at **one display range
  per channel across every frame** (`overview_display_range`), decided at the
  join from staged per-frame histograms — per-frame `auto` would make a cell
  appear to brighten because the stretch moved.
- **`BatchRunner.runEach()` is the loop; `run()` is one caller of it.** Include,
  the duplicate-`series_id` refusal, resolving and opening the image, the pixel-size
  warning, closing the stack on both paths, one row's failure not costing the
  other hundred and ninety-nine, and `batch_summary.tsv` are the same for any
  batch and are not worth a second copy — which is what a forked runner would
  be. A caller supplies the per-row work and names the columns that work
  reports; those columns are written blank for excluded and failed rows, so the
  summary is rectangular whatever happened. Whether a mixed-pixel-size batch
  *matters* is also the caller's to say: the detection is shared, the sentence
  is not, because warning an overview batch about a blur sigma it never uses is
  noise, and noise is what stops warnings being read.
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
  **`scripts/R/montage_grid.r` is the compositing both montage CLIs share** —
  cell, grid, title, scale bar, and `mg_write()`. Two things it settles. Its
  `full_width` is *given*, never inferred from the first cell: the group montage
  passes `cell * ncol` so a group of one still comes out the width of a full
  grid, while `montage_qc_cli.r` passes nothing, because its three panels are
  three renderings of one image scaled to a common height and padding them apart
  would put gaps into a strip meant to be read across. And `mg_write()` states
  the bit depth, because magick composes in 16 bits and appending a title band
  promoted the whole montage to a 16-bit PNG — twice the file, every flat grey
  shifted by 1/255, for no visible gain. The inputs are 8-bit; the output says
  so rather than depending on whether a band happened to be added.

  **`group_montage_cli.r` tiles one group's images into a single picture**, for
  the case where a count has to be made or checked by eye — an ovary is cut into
  serial sections imaged as separate series, so one specimen's sections are
  scattered across the sheet. Two things there are not preferences. A **missing
  image still occupies a labelled cell**, because counting from a grid that
  silently dropped a panel gives a wrong answer that looks like a right one. And
  panels are scaled by **physical** size, never pixel size: identical pixel
  dimensions can be different physical sizes, so equal-pixel scaling draws one
  follicle at two sizes in one picture, which is exactly the misjudgement a
  by-eye count makes. That needs `size_x`/`pixel_width`, which are `machine`
  columns `.cli_read_series_sheet()` drops by default — hence its
  `keep_machine=`, which names them one at a time so a caller carrying a machine
  column into its output has had to say which and why. `montage_index.tsv` is
  itself a valid input to the next run, which is the provenance answer instead
  of writing resolved paths back into `series.tsv`. Cells are laid out as a
  **table** — each column as wide as its widest cell, each row as tall as its
  tallest — because one cell size for a group spends the difference on blank
  space, and a column holding no cell at all is worth no width. So a montage is
  as wide as the columns it fills, and two montages no longer match in size.
  That costs nothing: padding never carried size information, the drawn pixels
  do, at a scale the scale bar states.

  `cli_helpers.r` is shared by all three. Conventions — the testable
  `<name>_cli(args)` function, the run guard, argparser's traps — are in the
  `r-cli-convention` skill. The IF quantification CLI is deliberately deferred:
  the background-measurement question is unsettled. `note/if_quantification.md`
  records what that decision will involve.

  **The series table is shared with the Fiji side, and is read the same way at
  both ends.** `.cli_read_series_sheet()` honours `include` with exactly the
  vocabulary `BatchRunner.isIncluded()` accepts, and applies it *before* the
  duplicate-`series_id` check so that setting `include=false` on all but one of a
  colliding pair works — which is what the Groovy error tells you to do. It then
  drops `include` (a control column, never metadata) and the machine columns,
  which it reads from `schema/sheet_columns.tsv` rather than listing again: a
  generated sheet carries sixteen, and joining them through would put `size_x`
  and `file_size` on every feature row. Dropped columns are reported.
  `feature_stat_cli.r --z_step` defaults to `pixel_depth` from each series' own
  `_config.txt` — per series, because pixel size varies fourfold inside one
  `.lif` here. Missing or blank means no `volume` column, never a default of 1.

  **Identity comes from the file's content, not its name.** The `name` column
  holds the series id, the `roi` prefix holds the feature; the filename only has to
  select the right files. The previous filename parse used a greedy prefix and
  so mis-split any feature name containing `_` — `S1_growing_oocyte_outline.txt`
  became series `S1_growing`, feature `oocyte`, silently. A greedy prefix is
  right when reading the feature out of an *ROI id*, whose tail is anchored and
  fixed-shape, and wrong for a filename, which has no such anchor.

  **A pre-grouping ROI filter can split one object into two.** `--min_circularity`
  and `--roi_area` drop ROIs before the overlap graph is built, so removing
  interior slices opens a z-gap that `max_z_dist` cannot bridge. Observed on real
  data: a circularity cut removed an oocyte's widest cross-sections and one
  object was counted as two. `--feature_area` acts after grouping and cannot do
  this — prefer it. Fiji's own `nucleus_circularity` (0.3.0, default
  `0.00-1.00` = off) is the same hazard with **no cure**: it filters inside
  `Analyze Particles`, so the rejects never reach `_outline.txt` and
  `--bridge_roi` has nothing to promote. Only the count survives, as
  `nucleus_circ_rejected`. Any statistic computed after such a filter is also a
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

- **The generated `series_id` always carries the series index**, as
  `sanitise(<alias>_s<NNNN>_<series_name>)`, never only where names happen to
  collide. Series names repeat freely — a tile scan is many series under one
  name, `Series001` is a Leica default — so with `alias` unique across files and
  the index unique within one, the id is unique *by construction*. Padding is
  a fixed four digits: deriving it from the file's series count would re-pad
  every id in a file that grew from 999 to 1001, which is the same
  instability as disambiguating only today's collisions. **It stays
  hand-editable**, for readability — so nothing may *join* on it that the
  operator cannot see change (hence the Luxendo sources key, above), and a
  regeneration matches rows on `(path, series_index)`, refusing outright when
  that key now names a different `series_name` (a file re-exported in another
  series order) rather than carrying the row's metadata onto the wrong image.
- **Duplicate `series_id`s are written by `Make_SeriesSheet` and refused by the
  batch.** The asymmetry is deliberate. The sheet is a draft for a person to
  read, and a duplicate you cannot open the table to see is one you cannot fix —
  the old behaviour threw before writing, so the error message was the only
  artifact of the run. It now writes the sheet and *then* fails (headless runs do
  not exit, so the log is the exit status: an exception and no `Done:` line).
  `allowDuplicateId` downgrades that. The batch refuses outright for included
  rows, because there two rows sharing an id overwrite each other's output;
  `.cli_read_series_sheet()` refuses on the R side too.
- **The batch opens an image one of two ways, and records which.**
  `BF.openImagePlus` — the importer, the code behind the series-chooser dialog —
  prepares a description of *every* series in the file before returning the one
  asked for, so its cost is O(series in the file) **per call**: 182 s on a
  1563-series `.lif` against 1.5 s on a 15-series one, paid once per row. The
  `reader` path holds one reader open per file and assembles the `ImagePlus`
  directly — 0.11 s for the same series. `auto` keeps the importer at or below
  16 series and switches above it. **Both are kept.** The importer handles format
  corners the hand-built path does not, and keeping both is what lets
  `Test_BatchRunner` assert the two produce identical bytes — the only guard
  against silent drift when Bio-Formats is next upgraded. That path reimplements
  calibration, plane order, window title and slice labels by hand, and three of
  those four were wrong at first in a way that changed the output while every
  measured number stayed correct.
- **Bio-Formats `ImporterOptions` writes ImageJ preferences.** The importer
  saves them after a successful open, so `setWindowless(true)` in a script
  leaves `.bioformats.windowless=true` behind and the operator's Fiji stops
  offering the series chooser on drag-and-drop. Prefer `ImageReader.openBytes()`
  when only reading — it touches no preferences — and where a real `ImagePlus`
  is needed, save and restore the keys in a `finally`. `Test_BatchRunner`
  asserts the batch leaves `windowless` as it found it, in both directions.
- **Headless `#@` scripts declare `persist=false`.** SciJava remembers what a
  script parameter was last set to and reuses it when a run does not supply one,
  so values typed into a GUI dialog leak into later headless runs. Observed: a
  batch given neither `outPrefix` nor `saveOverview` ran with `outPrefix=test_`
  and wrote overview PNGs, both left over from an interactive session. (The
  batch's `outPrefix` field has since been removed — it could never take effect,
  because the sheet's `series_id` named the output outright; v0.7.0 retired
  `output_prefix` altogether. The observation is kept: it is the evidence for
  the rule, not a description of today's dialog.) Same
  reasoning as forcing Set Measurements and `blackBackground` — a persistent
  user preference must never decide what a run does. `Run_NucleusSelector.groovy`
  keeps persistence deliberately: it is the tuning entry point and a human is
  looking at the dialog. **Three fields there are the exception** and reset
  every run: `Series id`, which names one image, so inheriting it would put the
  next image's output under this one's name; `nucleus_threshold_range`, a raw
  pixel value that is meaningless on a different bit depth or exposure; and
  `nucleus_stack_histogram`, the riskier of the two histogram modes. None should
  be inherited by the next image because it was used once on this one.

  `nucleus_threshold` itself **does** persist, like every other tuning field —
  and it is safe precisely because the range does not. Leaving the method on
  `Manual` means the next run starts with a blank range, which
  `validateThreshold()` refuses before the image is opened. A loud failure, not
  a stale number silently reused.

  All three are still written to `_config.txt`, so GUI-tune-then-batch is
  unaffected: `persist=false` means "do not remember into the next *dialog*",
  not "do not record".
- **Nothing holds two whole copies of the image, and `close()` does not help.**
  A tile merge is 5 GB of pixels and every intermediate is another copy, against
  a usable heap of ~8.9-9.6 GB — so `buildMask`'s helpers (`applyRange`,
  `to8BitMask`, `blank`) **mutate the stack they are given** rather than building
  a replacement beside it. That is safe only because the caller is always
  `buildMask` and the stack is always its own `Duplicator` copy; do not call them
  on an image you did not just duplicate. Where the type must change, each source
  plane is released as it is read (`setPixels(null, z)`) — `ImageStack`'s own
  `setProcessor()` cannot be used, it silently *converts* rather than swapping.
  The blur runs per slice for the same reason: `IJ.run(..., "stack")` holds one
  float plane per thread.

  And **`close()` must always be followed by `flush()`**. `close()` detaches a
  window; headless there is none, so while the variable is in scope it releases
  nothing — measured, 0 MB of 768 MB. Four intermediates in `NucleusPipeline`
  were closed and not flushed, and the nucleus mask stayed live through the
  overview projection, which is what exhausted the heap *after* the mask helpers
  were fixed. `Test_NucleusPipeline` asserts no library file has a bare
  `close()`, because this is exactly the kind of thing a person stops noticing.

  None of it changes a pixel: the mask helpers are checked against the previous
  implementations kept inline as oracles, and the per-slice blur against
  `IJ.run`. The test that separates "correctly unchanged" from "never ran" is the
  stack-identity assertion — in place means the instance survives.
- **Watershed forces `Prefs.blackBackground = true`.** It reads that preference
  to decide which phase is object; left to the operator's setting it erodes the
  background instead of splitting objects, and produces a plausible-looking mask
  while doing it. Same reasoning as forcing Set Measurements — and it likewise
  persists. Holes are filled before splitting, or watershed cuts through an
  unfilled hole and shatters one object into a ring of fragments.
- **One threshold vocabulary, two histograms.** Both features take their
  algorithms from Fiji's Auto Threshold plugin (`fiji.threshold.Auto_Threshold`).
  The nucleus chooses its threshold in `RoiDetect.chooseThreshold()`: the
  plugin's per-method statics, in `exec()`'s own sequence, on a `long`
  histogram whose counts are divided only as far as the method's `int`
  arithmetic needs (`nucleus_histogram_divisor`). `exec()` itself overflowed on
  large stacks and is kept as `Test_BuildMask`'s oracle. The nucleolus calls the
  same statics on a histogram it builds itself. Before this the nucleolus used
  ImageJ's own `ij.process.AutoThresholder` enum, which has no `Huang2` — the
  nucleus default — so the same word meant something in one field and threw in
  the other. The two implementations were measured as identical on every method
  the dialog offered before the switch, and `Test_NucleolusDetect` keeps the
  enum as the oracle.
  The **histograms stay different on purpose**: the nucleus pools one over the
  stack and drops its end bins (`ignore_black`/`ignore_white`), while the
  nucleolus builds one per nucleus per slice and drops nothing — inside a single
  nucleus the darkest pixels are the thing being looked for.
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
data: `Test_BuildMask` (thresholding — including the Auto Threshold macro call kept
as an oracle — watershed, manual ranges, per-slice histograms, and the
in-place mask helpers against the stacks-beside-stacks versions they
replaced),
`Test_NucleolusDetect` (the nucleolus threshold, with `ij.process.AutoThresholder`
as its oracle), `Test_Overview` (projection, contrast,
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

**What the numbers mean here** is keyed to the output contract, because that is
what the version is *for*: `_config.txt` records it so a results folder can name
the code that made it. The rule is deliberately loose while the repo has one
user — the point is that the number not be misleading, not that it be derivable:

| | when |
|---|---|
| **PATCH** | bug fixes; new parameters or columns appear and nothing existing breaks; a small addition to the contract |
| **MINOR** | a schema change of limited scope, or a key new capability in the pipeline — and, below 1.0, a *breaking* schema change too (see below) |
| **MAJOR** | a milestone for the repo as a whole — a complete end-to-end IF analysis pipeline would be one, and we are not there |

**Below 1.0 a breaking schema change is a MINOR, not a major.** With one user,
migrating a sheet is re-running `Make_SeriesSheet`; spending the major on that
would leave nothing to mark the milestone the row above reserves it for. What
makes this tolerable is that a stale sheet *stops* rather than half-loading:
both sides name the cause before reading anything else from the sheet —
`.cli_read_series_sheet()` in R, `SheetSchema.requireId()` in the batch and
`Make_LuxendoTiff`. ⚠️ That up-front check is not optional for a renaming
migration: without it the Groovy side reads every id as blank and its
duplicate check reports rows "sharing" an id — loud but **misleading** — and a
one-row sheet gets further still. A future rename adds its old name to the
check. Say so in the release notes, which is where someone looks when a sheet
stops loading. The `vocab` milestone
(`prefix` → `series_id`, `samples.tsv` → `series.tsv`) is the worked example, as
0.7.0. Revisit at 1.0, when there is an installed base for a major to protect.

`readParams` rejects unknown keys but tolerates missing ones, so adding a
parameter leaves every older config runnable — which is why that is not a major.

⚠️ **The rule says nothing about command-line surface.** Renaming a CLI flag
breaks a *command*, not an output, so it falls through to patch. That is
tolerable here — argparser fails loudly on an unknown flag, and there is one
caller — but it is a gap, not a decision. `--cell_height` → `--cell_max_px` in
0.5.1 is the worked example.

⚠️ **v0.4.0 and v0.5.0 were tagged under a stricter predecessor of this rule**,
which had new parameters as a minor rather than a patch. Under the rule above
v0.5.0 would have been 0.4.1. They are left as they are — a published tag is
not worth rewriting — so the history does not read consistently with this table
before 0.5.1.

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
