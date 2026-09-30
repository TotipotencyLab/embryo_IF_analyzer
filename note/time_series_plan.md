# Implementation plan: time-series support

**Status: design complete, nothing implemented.**
Delete this file when all milestones land — but migrate the surviving decisions
first, into `note/data_formats.md` (shapes) or `note/luxendo_file_format.md`
(that format's facts). `CLAUDE.md` only for the few that are standing hazards
someone must know *before* touching the code.

## How to read this document

| mark | meaning |
|---|---|
| 🔒 | **Design locked with the user.** Implement as written. |
| ⚠️ | A hazard: something that fails silently if got wrong. |
| ❓ | Open — decide before the milestone it sits in. |

🔒 decisions were made with evidence on the table. If the data contradicts one
at implementation time, **flag it or ask — never quietly implement something
else.** A locked decision that turns out wrong is a conversation, not a silent
substitution. Everything under "Facts established" was measured; re-measure
rather than trust it if the environment has moved.

---

## 1. Goal and scope

Make the repo able to ask **time-resolved questions** — how one nucleus's
volume, cross-sectional area, circularity or signal intensity changes over time
— rather than analysing each timepoint as an unrelated image.

🔒 **The driver is the shape of the data, not one dataset.** Luxendo TruLive3D
is the first instance to arrive. Nothing may be written so that it only works
for a TruLive3D.

**In scope:** reading Luxendo output into a format the pipeline accepts; a time
axis through Fiji and R; linking a feature to itself across time; the identity
and vocabulary cleanup that all of the above depends on.

**Not in scope:** background correction and absolute intensity comparison (see
`note/if_quantification.md`); any new segmentation method; tracking across
positions or across files that are not one time course.

---

## 2. Sequence

Each milestone has a short **alias**. Refer to them by alias, not by number —
the number says only where it sits in the queue, the alias says what it is. The
alias is also the branch prefix: `luxendo-<what>`, `time_axis-<what>`.

| # | alias | what | version bump | why here |
|---|---|---|---|---|
| 1 | **`luxendo`** | Luxendo input transform | **MINOR** | urgent — a user reads `.lux.h5` by hand today. Independent of the contract. |
| 2 | **`vocab`** | identity and vocabulary | **MAJOR** | before any *other* schema change; `time_axis` changes the schema too, and one migration beats two. |
| 3 | **`time_axis`** | time axis through the pipeline | **MINOR** | the groundwork every later step stands on — see below. |
| 4 | **`tracking`** | linking features across time | **MINOR** | needs `t`. Small, because TrackMate does the linking. |
| 5 | **`QoL`** | inspection round trip | **PATCH** | convenience. Lowest priority. |
| 6 | **`container`** | the R container | **MINOR** | last: see the shape of a working pipeline before building a container for it. |

**Why `time_axis` stays at 3, despite not being needed for the Luxendo path.**
A review pass established that under one file per (position, timepoint), `t`
reaches feature rows from the series table with no Fiji change at all, so
`tracking` could run without it. That is true and is *not* the reason to defer
it. The input shape is not fixed — a multi-frame `.lif` will arrive — and if
`time_axis` comes later, `tracking` has to support **two sources of `t`**: a
sheet column now, an outline column afterwards. With `time_axis` first, `t` is
always a column on the feature table whatever the input shape, and `tracking`
has one code path instead of two.

⚠️ **Consequence for testing:** the primary Luxendo workflow will never exercise
the multi-frame path, so nothing real will catch a regression in it. Every
multi-frame behaviour needs a synthesised test — `tests/groovy/` already
synthesises its images — and per `CLAUDE.md` the pair must be told apart:
prove it changes nothing on a single-frame image, and *separately* prove it does
something on a synthesised multi-frame one.

### Version bumps

`VERSION` is read at run time by `RoiExport.repoVersion()` and recorded in
`_config.txt`, so the number's job is to let a reader of an old results
directory know what made it. **Bump `VERSION` in the same commit you tag**, and
judge the class by what a reader of an old config or results folder would find —
not by how much code moved.

| alias | class | why |
|---|---|---|
| `luxendo` | **MINOR** | a key new capability — a second instrument's data becomes readable — but it touches no existing output. Every old config still runs and every old results directory still reads. |
| `vocab` | **MAJOR** | existing series tables stop working (`prefix` dropped outright, `samples.tsv` renamed) and the `name` column in existing results changes meaning. This is the definition of a schema change large enough to break existing data. |
| `time_axis` | **MINOR** | new columns appear and the ROI id gains a field, but only on multi-frame input; single-frame output is unchanged and absent `t` still means one frame, so nothing old breaks. Not a patch: a `_config.txt` and an `_outline.txt` from the new code carry fields the tagged version never wrote. |
| `tracking` | **MINOR** | a key new capability, additive only — a new table and a new column. |
| `QoL` | **PATCH** | three new scripts, no contract change, nothing existing breaks. |
| `container` | **MINOR** | new persisted tables and a changed CLI input contract, of limited scope. ⚠️ Note `CLAUDE.md` records that the rule says nothing about command-line surface, so the CLI change alone would fall through to patch; the new tables are what make this minor. |

⚠️ If `time_axis` were ever to land **before** `vocab`, reconsider: on its own
the 4-field ROI id makes `feature_roi_prefix()` return `NA` for new data read by
old code, which is closer to a break. The order above avoids the question.

Two constraints cut across the order:

- 🔒 **Do not run the full 33 GB conversion until `time_axis`'s contract is settled**, or
  it gets done twice. `luxendo` is written and tested on a few positions first.
- 🔒 **`luxendo`'s manifest uses `series_id` from day one**, anticipating `vocab`, so `vocab` does
  not revisit it.

---

## 3. The contract after this work

Everything in this section is a change to files the repo reads or writes, so
`note/data_formats.md`, `tests/testthat/test-data_formats.R`,
`schema/sheet_columns.tsv`, `README.md` and `config/*template*` move with it
(`CLAUDE.md` workflow step 7a — write it, do not propose it).

### 3.1 Identity

🔒 Five identities, each unique in a stated scope. The scopes differ, and that
is not an accident — see §6.2 for why.

| id | means | unique within | written by |
|---|---|---|---|
| `series_id` | one image series — one row of the series table | the series table | `Make_SampleSheet` / the gatherer |
| `position_id` | one physical field of view — **the same across timepoints** | the series table | the gatherer (seeded) |
| `t` | which timepoint, within a position | a `position_id` | the gatherer (seeded) |
| `feature_id` | one object **at one timepoint** | a `series_id` | `define_feature_group()` |
| `track_id` | one object **through time** | a `position_id` | `tracking` (TrackMate) |

🔒 `feature_id` = `<feature_type>_<NNNN>`, numbered **globally within a series**
across all timepoints it contains. Four-digit padding.
🔒 `track_id` = `<feature_type>_track_<NNNN>`, `NA` when unlinked.

⚠️ `feature_id` and `track_id` are keyed in **different scopes**, so
`feature` → `track` joins through the series table. It is the one join in the
design that crosses scopes; assert it rather than assume it.

### 3.2 The series table

🔒 `samples.tsv` → **`series.tsv`**. The prose stops saying "sample sheet"; it
is the series table. `files.tsv` is unchanged.

🔒 `prefix` → `series_id`. **Dropped outright, no alias, no warning** — the
sheet postdates the repo's only user, so there are no third-party sheets to keep
working, and an alias is a reserved word carried forever for a migration nobody
needs. An old sheet fails on a missing required column, which is correct.

New columns, both `seeded` (the gatherer writes them; a person may correct them
for data it did not produce):

| column | type | notes |
|---|---|---|
| `position_id` | string | which field of view |
| `t` | integer | timepoint index within that position |

🔒 `(position_id, t)` must be unique, refused on duplicates exactly as a
duplicate `series_id` is.

### 3.3 `_outline.txt`

🔒 Gains a `t` column: `name, roi, t, z, x, y`.

### 3.4 `_res.txt`

🔒 Gains explicit `roi`, `z`, `t`, `ch` columns, **written by us**, not parsed
back out of anything. `measureRois()` already knows the true channel and
`slices[i]`, and once it loops, the frame; `rt` is our own `ResultsTable`.

⚠️ `z` is present for a human reading the table. It must not reach a join —
see §3.6.

⚠️ **The Groovy and R changes land in the same PR.** `read_fiji_result()` ends
with `left_join(res_df, res_label_info, by = "label")` and `res_label_info`
already carries a `roi` column; a `roi` column in `_res.txt` becomes
`roi.x`/`roi.y` and every downstream reference silently disappears. Verified.

### 3.5 ROI ids

🔒 `<feature>_TTTT-SSSS-NNNN-YYYY` when the image has more than one frame,
`<feature>_SSSS-NNNN-YYYY` otherwise. Conditional, like the `t` column.

Forced, not cosmetic: `saveRoiZip()` uses the ROI name as the zip entry name, so
four frames of one object give four identical names and `ZipOutputStream` throws.

### 3.6 Joins

🔒 Measurements join outlines on **`(roi, t)`**. Both are keys, so no non-key
column is duplicated.

🔒 **The outline table is authoritative for `z`.** Before dropping the
measurement table's `z`, **assert the two agree on the overlap**, then drop, and
**report the drop** the way `.cli_read_sample_sheet()` already reports dropped
columns. Worth a small shared helper — `t` will want the same treatment as soon
as a second table carries it.

### 3.7 Backward compatibility

🔒 **By absence, not by flag.** A table with no `t` column is one time frame.
Every existing IF and oocyte output keeps working untouched and no caller opts
in. Same shape as `readParams` tolerating missing keys.

### 3.8 Where `t` comes from, and recording it

🔒 Two ways, and the run records which: frames inside the stack, or the series
table's `t` column when the gatherer wrote one file per timepoint (the default).
`_config.txt` says which, for the same reason it records `open_mode`.

---

## 4. Milestones

### `luxendo` — Luxendo input transform

Read `.lux.h5` and write files the existing pipeline accepts. Format facts are
in `note/luxendo_file_format.md`; do not duplicate them here.

- [ ] Read `.lux.h5` with **JHDF5** (`ch.systemsx.cisd.hdf5`). Bio-Formats
      cannot read them at all, and `BDVReader` on `bdv.xml` returns the wrong
      specimen silently — both verified, both in the format note.
- [ ] **Assembly manifest**: one row per *output file*, naming every source file
      and plane feeding it. Identity column `series_id`; also `position_id`, `t`.
- [ ] 🔒 **Manifest gets a `schema/` entry**, like `sheet_columns.tsv` — read at
      run time by both languages, two readers from day one.
- [ ] **Generic assembler**: manifest → TIFF.
- [ ] 🔒 **Three entry modes**: (i) end to end from a Luxendo directory,
      (ii) **manifest only**, (iii) assemble from an existing manifest.
      (ii) is what lets a person see the plan before committing to 33 GB.
- [ ] 🔒 **One file per (position, timepoint), classic TIFF by default**;
      `--format bigtiff` as an explicit alternative. See §5.1 and §6.1.
- [ ] ⚠️ 🔒 **`setCanDetectBigTiff(false)`, always.** Otherwise Bio-Formats
      silently upgrades to BigTIFF above the ceiling and the drag-and-drop
      guarantee is gone with no error. See §5.1.
- [ ] Predict output size from the manifest and **warn before writing**.
      Over-limit for the chosen format is a **per-row failure recorded in the
      summary, not an abort** — `BatchRunner.runEach()`'s contract. The message
      names `--format bigtiff`.
- [ ] 🔒 **One assembler with a `resize` option**, not two entry points. Two
      gatherers differing only in scale is the fork `CLAUDE.md` forbids.
      `resize` and `gatherFrames` are independent, both non-persistent.
- [ ] ⚠️ **Resizing must scale the calibration.** Halve the pixels and
      `pixel_width`/`pixel_height` must double, or every area is wrong by the
      square of the factor while the image looks perfect. Test: assemble one
      position at 1x and 2x, assert the physical extent matches.
- [ ] A non-default `resize` puts a token in the output filename.
- [ ] Per-output **provenance record**: source paths, checksums, gatherer
      version, resize factor.
- [ ] Honour `include`, with exactly `BatchRunner.isIncluded()`'s vocabulary.
- [ ] **Verification mode**: re-read output planes and compare to source by
      checksum. "39 slices, 3 channels" passes happily while channels are
      transposed — this is the only check that can fail correctly.
- [ ] ⚠️ Assert z uniform **across timepoints of one position**. It is *not*
      uniform across positions (16..39, one at z=1).
- [ ] 🔒 `raw/` — the Luxendo acquisition directory — **is never modified or
      deleted.** Converted TIFFs are derived and may be regenerated.

**Verification.** Assemble two positions including `L26A pos3` (z=1), checksum
against source, and open one in Fiji by drag-and-drop.

### `vocab` — identity and vocabulary

One schema migration, so §3.1 and §3.2 land together.

- [ ] `series_id` as the one word; `sample` and `prefix` both retired.
- [ ] 🔒 **`Run_NucleusSelector` gains a `series_id` source control**, because
      the interactive path has no series table to read an id from. One `String`
      field whose meaning depends on the mode:

      | mode | the String means | behaviour |
      |---|---|---|
      | `derive` | a pattern to find in the slice label or title | today's `resolveImageId()`, minus the `output_prefix` |
      | `explicit` | the `series_id` itself | used verbatim |

      ⚠️ **Validate the mode in code, not in the dialog.** A `#@ String` with
      `choices={...}` is *not* validated on the command line — SciJava passes any
      string straight through, as `BatchRunner.OPEN_MODES` already documents.
      ⚠️ **`explicit` must not persist.** It names one image, so inheriting it
      into the next run would silently mislabel that run's output — the same
      category as `nucleus_threshold_range`, and the same treatment.
- [ ] 🔒 **`position_id` and `t` reach the series table from `files.tsv`**, one
      row per output file, seeded onto each series row.
      ⚠️ They must be declared **`seeded`**, not `machine`.
      `.cli_read_sample_sheet()` *drops* machine columns, so declaring them
      machine would silently remove them before they ever reach a feature row,
      and `tracking` would have nothing to track on.
- [ ] ⚠️ **Seed them explicitly in `SampleSheet.build()`, the way `include`
      already is.** Do not rely on the default `inherit`. `Make_SampleSheet`
      computes its default as *"every `files.tsv` column that is not already a
      declared series column"*
      ([Make_SampleSheet.groovy:95](../scripts/groovy/Make_SampleSheet.groovy)),
      so the moment `position_id` and `t` are added to
      `schema/sheet_columns.tsv` the default `inherit` **stops carrying them**
      and they arrive blank. Declared-and-explicitly-seeded is exactly the
      pattern `include` follows
      ([SampleSheet.groovy:270](../scripts/groovy/SampleSheet.groovy)); follow it.
      A run-time `inherit` list would also work but puts the guarantee in the
      operator's hands, where it will eventually be forgotten.
- [ ] `samples.tsv` → `series.tsv`.
- [ ] `position_id`, `t` added to the series table.
- [ ] `feature_id` numbered globally within a series; `track_id` reserved.
- [ ] `schema/sheet_columns.tsv`, `SheetSchema.groovy`,
      `.cli_read_sample_sheet()`, `note/data_formats.md`,
      `tests/testthat/test-data_formats.R`, R CLI flags, `README.md`,
      `config/*template*`.

⚠️ **This changes an output file.** The interactive path stops writing
`output_prefix` into `name`.

**Verification.** Re-run the reference and assert the old and new outputs are
identical **after dropping `name`**, and that `name` differs only by the removed
`output_prefix`. That fails if anything else moved. Regenerate the fixture
deliberately, and say so in the commit.

### `time_axis` — time axis through the pipeline

- [ ] Groovy: frame loop in `RoiDetect` / `NucleusPipeline`. Frames are
      hardcoded to 1 in three places: [NucleusPipeline.groovy:314](../scripts/groovy/NucleusPipeline.groovy),
      [RoiDetect.groovy:162](../scripts/groovy/RoiDetect.groovy) (both
      `Duplicator().run(..., 1, 1)`) and
      [RoiExport.groovy:121](../scripts/groovy/RoiExport.groovy)
      (`imp.setPosition(ch, slices[i], 1)`).
- [ ] `_outline.txt` gains `t`; `_res.txt` gains `roi, z, t, ch`; ROI ids gain
      `TTTT-` when frames > 1.
- [ ] R: `t` honoured in grouping; absent `t` means one frame.
- [ ] ⚠️ `define_feature_group()` has **no partition argument today**
      ([define_feature_group.r:72-83](../scripts/R/define_feature_group.r)).
      Add one, **and make the numbering global within the series**, or every
      timepoint emits `nucleus_1`.
- [ ] ⚠️ **`feature_stats.r` must be updated in the same change.**
      [:134](../scripts/R/feature_stats.r) dedups on `(roi, ch)` and
      [:141](../scripts/R/feature_stats.r) joins `by = "roi"`. Under time data
      with a 3-field ROI id it keeps the first timepoint, warns, and reports
      t=0's numbers as the feature's. `keep` gains `t`; dedup key becomes
      `(roi, ch, t)`; join becomes `by = c("roi", "t")`.
- [ ] ⚠️ `saveRoiZip()` leaves a **189-byte partial zip** when it throws.
      Write to a temp path and rename on success.
- [ ] ⚠️ 🔒 **Any new run parameter must land in four places at once**, and four
      set-difference assertions enforce it: `PARAM_TYPES` ↔ the `#@` dialog
      variables, the dialog's literal `value=` ↔ `DEFAULTS`, `PARAM_TYPES` ↔
      `config/nucleus_config_template.txt` (all three in `Test_RunConfig`), and
      `PARAM_TYPES` ↔ what `saveRunConfig()` actually writes
      (`Test_NucleusPipeline`). `CLAUDE.md` records this breaking three times;
      a parameter the run forgets to write silently becomes its DEFAULT on the
      next run. The time source of §3.8 is such a parameter.

**Verification.** Two runs that must be told apart: a single-frame image
produces byte-identical output to before (correctly did nothing), and a
synthesised 4-frame stack produces four times the ROIs with distinct ids
(actually ran). `tests/groovy/` synthesises its own images, so this needs no
data.

### `tracking` — linking across time

🔒 **Outsourced to TrackMate.** See §5.2.

- [ ] `Make_FeatureTracks.groovy`: feature centroids per
      `(series_id, feature_id, t)` → TrackMate LAP tracker → `tracks.tsv`
      carrying `(position_id, series_id, feature_id, t, track_id)`.
- [ ] R joins `tracks.tsv` onto the feature table.
- [ ] Record the tracker settings used, as `_config.txt` records everything else.
- [ ] 🔒 **Calibrated units, never pixels.** In pixels `LINKING_MAX_DISTANCE`
      stops meaning a physical distance, and pixel size varies fourfold inside
      one `.lif` here, so a cut-off tuned on one dataset would be wrong on the
      next. See §5.4.
- [ ] ❓ **PENDING DECISION — does `z` take part in the distance?** Cannot be
      settled without real data; revisit at implementation and **ask rather than
      pick**. First thing to try: `z = 0`, link on xy. See §5.4.
- [ ] 🔒 Time-linking belongs in **`annotate_features_cli.r`** — another
      annotation on the feature table, alongside containment, not a separate
      product.
- [ ] Extract `relate_features.r`'s containment core and give it a directional
      denominator, retained scores, one-to-one resolution with ties recorded,
      and partition keys as an argument. See §7.
- [ ] Retire `find_overlap_roi_features()`. Two callers first:
      `PLA_analysis/test_PLA.R` and `tests/testthat/test-spatial.R`.
- [ ] *Low priority:* a primitive in-house overlap linker as a cross-check.

**Verification.** Synthesise two objects moving apart over four frames; assert
two tracks, not one and not four. Then a dividing object; assert the split is
recorded rather than becoming two unrelated tracks.

### `QoL` — inspection round trip

🔒 `Open_*` is the verb — `Open_LifFile.groovy` already establishes it as "opens
something into the Fiji GUI for a human", interactive-only by nature. No fourth
verb needed.

⚠️ The ROI Manager needs a GUI, so the **logic** (join, filter, rename) lives in
a library class and the `Open_*` script is a thin caller — same division as
`NucleusPipeline` vs `Run_NucleusSelector`, and the only way any of it is tested.

- [ ] `Inspect_AnnotatedFeatures.groovy` — inputs: series table, R feature
      table, ROI directory. No side effect; prints `filename`, `series_name`,
      `series_index`, `position_id`, `t`, `n_<feature>`. After `vocab` the join is
      `series_id` on both sides with no prefix to strip.
- [ ] `Open_AnnotatedFeatures.groovy` — same inputs plus a series selector
      (number or `series_id`, default 1, `persist=false`) and an optional
      feature filter (blank = all). Opens the series via `BatchRunner`'s open
      path, loads the zip with `RoiExport.loadRoiZip()`, filters, renames with
      the `feature_id` prefix, adds to the ROI Manager.
- [ ] `Open_SeriesRow.groovy` — open row N of a series table. (Named for
      `series.tsv`, not the retired "sample sheet".)
      🔒 **1-based** (it is a table row; `series_index` stays 0-based because
      Bio-Formats owns it — the dialog says which is which).
      🔒 **Does not honour `include`**, so the row number indexes the
      **unfiltered** table, and the script **prints** the row's `include` value.

### `container` — the R container

🔒 **S3, not S4.** A plain list, so `$` keeps working and nothing downstream
changes on day one — a CLI can build one and ignore it, which makes it adoptable
incrementally rather than as a rewrite. No methods required up front;
`print.image_region()` and `[.image_region` can arrive later. S4 would be the only
object system in a repo whose idiom is tables and functions.

🔒 **One element per grain.** Mixing grains is what produces the duplicated-row
failures this repo keeps meeting.

```r
structure(list(
  series  = <tibble>,  # per series_id — from _config.txt: pixel sizes,
                       #   image_width/height, VERSION, open_mode, t source,
                       #   plus position_id and t
  roi     = <sf tbl>,  # per (series_id, roi) — t, z, geometry, area,
                       #   feature_id, feature_type
  measure = <tibble>,  # per (series_id, roi, ch) — the _res.txt measurements
  feature = <tibble>,  # per (series_id, feature_id) — t, feature_type,
                       #   parent_feature_id, track_id, z_span, containment,
                       #   match_kind, per-channel stats
  track   = <tibble>,  # per (position_id, track_id)  <- NOT series
                       #   n_frames, t_first, t_last, split/merge flags
  meta    = <list>     # run_id (run_id_from() exists), input fingerprint,
                       #   join report, dropped columns
), class = "image_region")
```

The constructor asserts, once, in one place:

- [ ] no duplicate keys within a grain — the assertion that would have caught
      the `feature_id` collision
- [ ] every `measure` key exists in `roi` — the join that "can be perfectly
      correct while matching nothing"
- [ ] every non-`NA` `roi$feature_id` exists in `feature`
- [ ] ⚠️ **the two identity scopes agree**: every `feature` row's `track_id`
      resolves in `track` via the `position_id` of its series
- [ ] **referential integrity both ways**: every `series_id` in `roi`,
      `measure` or `feature` exists in `series`, and a `series` row with no ROIs
      is *reported* rather than assumed empty. A stray `series_id` is the
      signature of a partial read — one `_config.txt` missing from a results
      directory.
- [ ] row counts reported, not assumed

🔒 **Persistence is TSV; `.rds` is at most a cache.** `QoL`'s Groovy scripts cannot
read an R serialisation, and a format only one language opens would undo what
`schema/sheet_columns.tsv` exists to guarantee. Precedent:
`feature_scatter_cli.r` deliberately reads `feature_stats.tsv`, not the `.rds`.

The object is **multi-series** — `series_id` is a column in every element —
because that is how the CLIs already work. This changes the CLI contract, and
that is most of the benefit: after the first annotation step a CLI takes *one*
input instead of a directory plus pattern flags.

---

## 5. Facts established

All measured on this machine (Fiji 2.16.0 / ImageJ 1.54p, Bio-Formats 8.1.1,
JHDF5 19.04.1, TrackMate 7.14.0; R 4.6.1, dplyr 1.2.1). Re-measure rather than
trust if the environment moves.

### 5.1 The classic-TIFF ceiling, and a silent format switch

**The ceiling is `4,183,818,240` bytes of pixel payload** — 3.896 GiB, reserving
~106 MiB below 2^32 for IFDs. It is a literal in
`loci.formats.out.TiffWriter`, not 2^32.

Predictable from the manifest, no trial write:

```
bytes = size_x * size_y * size_z * size_c * size_t * bytes_per_pixel
fits classic TIFF  <=>  bytes < 4183818240
```

⚠️ **`canDetectBigTiff` defaults to `true`** on both `TiffWriter` and
`OMETiffWriter`. Above the ceiling they log *"Switching to BigTIFF (by file
size)"* and carry on, so a run that asked for classic TIFF gets a BigTIFF and
the drag-and-drop guarantee vanishes with no error. There is also a *"by file
extension"* path for `.tf2`/`.tf8`/`.btf`. Hence `setCanDetectBigTiff(false)`:
the writer then raises *"File is too large; call setBigTiff(true)"*. Same family
as forcing `Set Measurements` and `blackBackground` — a library default must not
decide what a run produces.

At one file per (position, timepoint) every Luxendo output is ~981 MB, a quarter
of the ceiling. All four timepoints in one file would also have fitted, at
3.656 GiB — but with 6.2% margin, about two and a half more z slices.

BigTIFF itself works: written via `setBigTiff(true)` it carries magic 43,
Bio-Formats reads it with calibration intact, and `IJ.openImage()` opens it as a
calibrated 3c/5z/4t hyperstack.

### 5.2 TrackMate is drivable without an image

**There is no CSV spot importer in bundled TrackMate 7.14.0** — only
`CSVExporter` and `TGMMImporter`. "Hand it a table" is not the path.

What works, verified end to end on synthetic spots (two objects, three frames →
6 vertices, 4 correct edges): `new SpotCollection()`,
`new Spot(x, y, z, radius, quality, name)`, `sc.add(spot, t)`,
`sc.setVisible(true)`, `SparseLAPTrackerFactory().create(sc, settings)`,
`tracker.getResult()`. No image, no detection, no new dependency.

Settings from `getDefaultSettings()`:

```
LINKING_MAX_DISTANCE          GAP_CLOSING_MAX_DISTANCE    MAX_FRAME_GAP
ALLOW_GAP_CLOSING             ALLOW_TRACK_SPLITTING       ALLOW_TRACK_MERGING
SPLITTING_MAX_DISTANCE        MERGING_MAX_DISTANCE        BLOCKING_VALUE
LINKING_FEATURE_PENALTIES     GAP_CLOSING_FEATURE_PENALTIES
SPLITTING_FEATURE_PENALTIES   MERGING_FEATURE_PENALTIES
ALTERNATIVE_LINKING_COST_FACTOR   CUTOFF_PERCENTILE
```

🔒 **`ALLOW_TRACK_SPLITTING` is why this is outsourced.** Nuclei divide;
division is a split in the track graph. A bespoke linker needs it as a special
case, here it is a setting. Gap closing likewise covers a frame where detection
drops an object.

TrackMate is a Java library, so this is a Groovy `Make_*` step: reads a table,
writes a table the pipeline consumes.

### 5.3 ImageJ's stack-position columns cannot be trusted

Which of `Ch` / `Slice` / `Frame` appear, and what they contain, depends on
image shape:

| shape | hyper? | `Ch` | `Slice` | `Frame` |
|---|---|---|---|---|
| 1c 5z 1t | no | – | z | – |
| 3c 5z 1t | yes | ✓ | z | – |
| 3c 5z 4t | yes | ✓ | z | ✓ |
| 1c 5z 4t | yes | – | z | ✓ |
| **1c 1z 4t** | **no** | – | **t, reported as `Slice`** | – |
| **3c 1z 4t** | yes | ✓ | **absent** | ✓ |
| **3c 1z 1t** | **no** | **absent** | channel, as `Slice` | – |

`Slice` means z, or t, or the channel. The bottom three rows are shapes the
Luxendo data contains. This is why §3.4 writes our own columns.

A string column added to our `ResultsTable` survives `rt.save()`; ours append
after ImageJ's.

### 5.4 Coordinate units change which tracks survive

Two spots, one nucleus, xy-stationary, z centroid moving by one slice between
frames. Luxendo calibration: xy 0.208 µm, z 5.0 µm.

```
cut-off 3 units, one slice of z wobble:
  calibrated µm (z step 5.0)     dz=5.000  -> NOT LINKED    <- the track breaks
  slice index  (z step 1)        dz=1.000  -> linked

real xy motion of 2 µm, no z change:
  calibrated µm                  dx=2.000  -> linked
  pixels (9.6 px at 0.208 µm)    dx=9.600  -> NOT LINKED
```

Pixels fail the second case, so 🔒 calibrated.

But calibrated exposes the anisotropy: **24x between xy and z**. One slice of
z-centroid wobble costs 5 µm, which swamps a cut-off tuned for a couple of µm of
real xy motion and breaks the track of a nucleus that never moved. With ~20 µm
nuclei at 5 µm steps the z centroid is resolved to about four samples, so much
of that wobble is sampling noise rather than motion.

Three ways out, in the order to try them:

1. **`z = 0`, link on xy only.** Likeliest right here — xy is the informative
   axis and z is coarse.
2. Full 3D with `LINKING_MAX_DISTANCE` at or above the z step. Safe, but the xy
   tolerance becomes >= 5 µm too, which may over-link a dense field.
3. Scale z by the anisotropy before passing it. Tunable, and arbitrary.

❓ Which one is a **pending decision** needing a real dataset. Do not pick
silently.

### 5.5 Regex behaviour under a 4-field ROI id

| id | `feature_roi_prefix()` |
|---|---|
| `nucleus_0001-0001-0433` | `"nucleus"` |
| `nucleus_0001-0002-0003-0433` | `NA` — **fails loudly** |
| `nucleus_t0001_0002-0001-0433` | `"nucleus_t0001"` — **silently wrong** |

The column-identifying regex `\d{4}-\d{4}-\d{4}$` still matches the 4-field id's
tail correctly, since the last three fields remain z, index, y.

---

## 6. Alternatives considered and rejected

Recorded so they are not re-proposed, and so a reversal is a decision rather
than a drift.

### 6.1 One file per position, all timepoints inside (BigTIFF)

**Rejected: loses drag-and-drop.** It would have removed the `position_id` axis
entirely and kept every key inside a series, which is genuinely tidier. BigTIFF
was verified to work through Bio-Formats *and* `IJ.openImage`. But a format
every tool opens without thinking is worth more than saving a column: the column
is paid once, the friction is paid on every look at the data.

Cost accepted: `track_id` is scoped to `position_id` while `feature_id` is
scoped to `series_id`, so the container carries two identity scopes.

Bonus retained: per-timepoint files allow analysing t=0 while t=3 is still
acquiring, and a corrupt timepoint costs one file rather than a position.

### 6.2 Time as independent sheet rows, no contract change

**Rejected: cannot link.** Cheapest option, gives per-timepoint statistics, and
requires nothing in this document. But a feature cannot be followed to itself
across time, which is the goal.

### 6.3 `image_id` instead of `series_id`

**Rejected: the literal argument wins.** A row of that table *is* a Bio-Formats
series — `Make_SampleSheet` builds it by enumerating series. The objection that
three `series_*` names crowd one table does not survive inspection, because they
are not redundant: `series_index` and `series_name` are unique *within a file*
(which is exactly why neither can be the identity across a batch — a Leica
`Series001` recurs in every file), while `series_id` is unique across the table.

Note the underlying problem `vocab` fixes is not naming: **one column holds two
compositions today.** Batch passes the sheet's `prefix` as `basename`, so `name`
= the per-series id with no `output_prefix`; interactive falls back to
`(output_prefix ?: "") + resolveImageId(...)`
([NucleusPipeline.groovy:212](../scripts/groovy/NucleusPipeline.groovy)). The
fixture is from the interactive path. The docs disagree too: `CLAUDE.md`'s
contract writes `<prefix><image id>_…` while `note/data_formats.md` says the
`name` column *is* the image id.

### 6.4 Deriving `t` from the ROI id instead of a column

**Rejected: the redundancy is load-bearing.** With `t` a column on both sides
and the join on `(roi, t)`, a disagreement drops rows, and the suite already
asserts that reading a table neither adds nor drops rows. Derive it and a
mismatch is undetectable. `TTTT` is also conditional, so a parser would need two
id shapes; and the outline table already made this split for z.

### 6.5 `<feature>_t0001_SSSS-NNNN-YYYY` instead of `TTTT-`

**Rejected: fails silently.** See §5.5. Prefer the id that returns `NA` and
warns over the one that returns a plausible wrong feature type.

### 6.6 No `z` in `_res.txt`

**Rejected: it is useful for eyeballing and costs nothing**, provided §3.6's
assert-then-drop exists. The original reason — a join collision — was real but
is solved by naming which table wins rather than by omitting the column.

Assert-then-drop was preferred over making `z` a join key: a key also catches
disagreement, but as a *quietly short table* rather than a message naming the
column.

### 6.7 A `track_id_alias` column

**Rejected: an alias is only worth its cost when the canonical id is
unreadable**, and `nucleus_track_0007` is not. A second thing to keep in sync
against a repo rule that a fact belongs in one place. Revisit only if a *stable*
id is later needed — one surviving re-runs with different parameters, which
would have to be content-derived and genuinely unreadable.

### 6.8 An S4 container

**Rejected: ceremony without benefit.** A validated constructor over an
S3-classed list gets the whole value — one object, keys checked in one place.

### 6.9 Separate gatherers for analysis and for eyeballing

**Rejected: it is a fork.** `CLAUDE.md` says add the option to the library
rather than fork a script. The safeguard moves to the provenance record, a
filename token, and the calibration test in `luxendo`.

### 6.10 Building `container` before `tracking`

**Rejected: see the shape of a working pipeline first.** Not for the reason the
review gave — the `BatchRunner.runEach()` precedent is a weak analogy, since
nothing was anticipated then because nothing was planned that far ahead. The
real merit is narrower: whatever the container holds should be what `tracking` turned
out to need, not what it was guessed to need.

---

### 6.11 `feature_set` / `analysis_set` / `particle_feature` as the class name

**Rejected in favour of `image_region`.** `feature_set` names the container
after one of its own grains, which is confusing exactly where the object is
meant to remove confusion. `particle_feature` reproduces that flaw — it contains
`feature`, a grain — even though "particle" is good, familiar vocabulary from
`Analyze Particles`. `analysis_set` avoids every collision but says nothing: it
could hold anything.

`image_region` says what the object is a collection of, collides with no grain,
and is singular in the way R class names conventionally are (`data.frame`,
`tbl_df`, `sf`).

⚠️ **This is the class name only.** The `roi` grain keeps its name. `roi` is in
the output contract — the `_outline.txt` column, the `_res.txt` column after
`time_axis`, the `\d{4}-\d{4}-\d{4}` id — and in `RoiExport`, `RoiDetect`,
`loadRoiZip` and the ROI Manager. It is also standard ImageJ vocabulary. Renaming
it on the R side alone would create a second word for one concept, which is what
`vocab` exists to remove.

## 7. `find_overlap_roi_features()` — review, and what replaces it

303 lines, used by `PLA_analysis/test_PLA.R` and `tests/testthat/test-spatial.R`,
by no CLI. **The geometry is sound; the scoring is the defect.**

- **R1 — the denominator is symmetric.** `min_area = pmin(area_1, area_2)`, so
  `int_ratio` is "fraction of child inside parent" only by accident, when the
  child is always smaller. For two features of *similar* size — the same nucleus
  at t and t+1 — `pmin` flips between pairs and the ratio stops being comparable.
  `relate_features.r` divides by the child's area. **This is the core defect.**
- **R2 — `min_ratio_roi_overlap = 1` is all-or-nothing.** One failing slice
  kills a link. The test block probing `min_intersect_ratio=1` then `0.99999` is
  the symptom.
- **R3 — the output discards the evidence**, returning only the id pair while
  `int_ratio` and `ratio_roi_ovl` are computed and thrown away.
- **R4 — no one-to-one resolution.** Right for containment, wrong for tracking.
- **R5 — unsafe on a time-aware table**: `z_1 == z_2` is the only partition, so
  two ROIs at the same z in different timepoints would count as overlapping.
- Performance: dense `st_intersects(sparse=FALSE)` matrix; the `z` filter
  applied *after* the geometric predicate; a linear scan inside the per-pair loop.

**Not a defect:** `reframe()` where `summarise()` might be used. Tested — for
scalar expressions they return identical values and grouping. The only
difference is that `summarise()` *errors* on an unexpectedly non-scalar
expression where `reframe()` silently returns extra rows. Mild preference, given
this repo's hazard is silent row multiplication.

**Housekeeping:** the `if(F){...}` block carries a hardcoded
`/Volumes/pool-toti-imaging/...` path. Confirmed a live-testing scratchpad —
delete it.

🔒 **`tracking` extracts `relate_features.r`'s containment core** and gives it a
directional denominator, retained scores, one-to-one resolution with ties
recorded, and partition keys as an argument. One core, two callers
(parent/child and frame-to-frame). `find_overlap_roi_features()` is then
retired; leaving a second overlap implementation is how a third gets written.

---

## 8. Hazards

**H1 — `roi` column collision on join.** §3.4. Verified: the join yields
`roi.x`/`roi.y` and no bare `roi`.

**H2 — a pre-grouping filter can split one object, and time makes it worse.**
Already true in z (`CLAUDE.md`). Across t the same mechanism removes a frame and
breaks a track, which is harder to see than a split object.

**H3 — dimensions vary per position, and a single-plane position is the sharp
edge.** z is 16..39 across the Luxendo positions; x, y, c, t may differ too.
Nothing may size a run from its first row. `pixel_depth` — the z step, a
`_config.txt` field and a series-table column — must be **blank** for a single
plane, never ImageJ's default 1.0, because something downstream would multiply
by it. `L26A pos3` (z=1) lands on the worst rows of §5.3 and should be the first
position tested.

**H4 — channel identity.** `channel_2` has no `channel_description`, and the
`.ims` files permute channel order relative to the directory names. Identity
comes from the manifest, never from an index alone.

**H5 — `feature_stats.r` silently keeps only the first timepoint.** the `time_axis` milestone.

**H6 — a failed ROI-zip write leaves a partial file.** Verified: 189 bytes on
disk after the exception, which `loadRoiZip` would read. the `time_axis` milestone.

---

## 9. Open questions

1. ❓ **PENDING DECISION — does `z` take part in the TrackMate distance?**
   Calibrated units are locked (§5.4); whether `z` participates cannot be
   settled without a real dataset. Try `z = 0` first, and **ask rather than
   pick**. (`tracking`)

That is the only one left. The container class name was settled as
🔒 **`image_region`** — see §6.11.
