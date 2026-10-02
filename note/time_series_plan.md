# Implementation plan: time-series support

**Status: `luxendo` done (v0.6.0, amended after it — §4); `vocab` done
(v0.7.0, both PRs, §4); nothing after it implemented.** The design was revised on 2026-10-02 (**a series is a
whole position, time included**; §3.1, §6.17) and the sections below say so
where it changed.
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

**In scope:** reading Luxendo output into a format the pipeline accepts **and
into the sheets the batch runner already loops**; a time axis through Fiji and R;
linking a feature to itself across time; the identity and vocabulary cleanup
that all of the above depends on.

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
| 2 | **`vocab`** | identity and vocabulary | **MINOR** (0.7.0) | before any *other* schema change; `time_axis` changes the schema too, and one migration beats two. Below 1.0 a breaking schema change is a minor — `CLAUDE.md` § Versioning. |
| 3 | **`time_axis`** | time axis through the pipeline | **MINOR** | the groundwork every later step stands on — see below. |
| 4 | **`tracking`** | linking features across time | **MINOR** | needs `t`. Small, because TrackMate does the linking. |
| 5 | **`QoL`** | inspection round trip | **PATCH** | convenience. Lowest priority. |
| 6 | **`container`** | the R container | **MINOR** | last: see the shape of a working pipeline before building a container for it. |

🔒 **`luxendo` grew a second part** after a design review (§4, §6.12–§6.17): the
scan emits a *series* table as well as the sources table, because the batch
runner loops series and that is where `include` lives. The **PR boundary is
"building the input files"** — `Make_LuxendoSheets` and `Make_LuxendoTiff` — and
`Make_OverviewStack` sits in `QoL`, since it is a convenience that nothing in
the analysis depends on and it reads finished outputs rather than producing them.

**`time_axis` is now on the Luxendo critical path.** Before 2026-10-02 the
Luxendo series was one (position, time point), so `t` could reach feature rows
from the series table with no Fiji change, and `time_axis` was justified only by
keeping `tracking` to one source of `t`. With a series now a whole position
(§3.1, §6.17), the time axis is *inside* every Luxendo series: **no Luxendo
analysis runs until `time_axis` lands**, and it must stream frames, because a
position (~95 GB) never fits in memory (§5.10). Accepted by the user: the data
can wait.

It also changes the testing picture for the better. Previously the primary
Luxendo workflow would never exercise the multi-frame path; now it is the
*only* path Luxendo takes, so real data exercises it. Synthesised tests are
still required — per `CLAUDE.md` the pair must be told apart: prove it changes
nothing on a single-frame image, and *separately* prove it does something on a
synthesised multi-frame one.

### Version bumps

`VERSION` is read at run time by `RoiExport.repoVersion()` and recorded in
`_config.txt`, so the number's job is to let a reader of an old results
directory know what made it. **Bump `VERSION` in the same commit you tag**, and
judge the class by what a reader of an old config or results folder would find —
not by how much code moved.

| alias | class | why |
|---|---|---|
| `luxendo` | **MINOR** | a key new capability — a second instrument's data becomes readable — but it touches no existing output. Every old config still runs and every old results directory still reads. |
| `luxendo`, amended | **PATCH** | the index file list, `Make_LuxendoTiff`'s `frames`, and a changed *default* (`gatherFrames` on). No column changes; the per-time-point layout is one switch away. |
| `vocab` | **MINOR** (0.7.0) | existing series tables stop working (`prefix` dropped outright, `samples.tsv` renamed) and the `name` column in existing results changes meaning. Below 1.0 a breaking schema change is a minor, because a stale sheet *stops* rather than half-loading — `CLAUDE.md` § Versioning; it was a major in this plan before that rule. Release notes must say so. |
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
  (Largely moot since conversion stopped being a required step, §6.12.)
- 🔒 **`luxendo`'s manifest uses `series_id` from day one**, anticipating `vocab`, so `vocab` does
  not revisit it.

---

## 3. The contract after this work

Everything in this section is a change to files the repo reads or writes, so
`note/data_formats.md`, `tests/testthat/test-data_formats.R`,
`schema/sheet_columns.tsv`, `README.md` and `config/*template*` move with it
(`CLAUDE.md` workflow step 7a — write it, do not propose it).

### 3.1 Identity

🔒 **A series is one 5D image — x, y, z, c and t.** That is Bio-Formats' meaning
of "series" in every format, not LIF's: a LIF time-lapse holds its frames inside
one series, and a Luxendo **stack** is one series with its time points spread
across files — which is exactly how Bio-Formats itself presents `bdv.xml` (14
series, one per position). Revised 2026-10-02; until then a Luxendo series was
one (position, time point), and that is what §6.17 records reversing.

🔒 Four identities, each unique in a stated scope:

| id | means | unique within | written by |
|---|---|---|---|
| `series_id` | one series — one row of the series table | the series table | `Make_SeriesSheet` / `Make_LuxendoSheets` |
| `t` | which time point, within a series | a `series_id` | the frame index, written by Fiji into the output tables (`time_axis`) |
| `feature_id` | one object **at one time point** | a `series_id` | `define_feature_group()` |
| `track_id` | one object **through time** | a `series_id` | `tracking` (TrackMate) |

🔒 `feature_id` = `<feature_type>_<NNNN>`, numbered **globally within a series**
across all time points it contains. Four-digit padding.
🔒 `track_id` = `<feature_type>_track_<NNNN>`, `NA` when unlinked.

**No `position_id`.** It was planned as a column so a position could span
several series rows; with a series being the whole position it would always
equal `series_id`. Only a format that genuinely splits one field of view across
rows would bring it back. Consequence: `feature_id` and `track_id` now share one
scope, and the cross-scope join the earlier design warned about is gone.

### 3.2 The series table

🔒 `samples.tsv` → **`series.tsv`**. The prose stops saying "sample sheet"; it
is the series table. `files.tsv` is unchanged.

🔒 `prefix` → `series_id`. **Dropped outright, no alias, no warning** — the
sheet postdates the repo's only user, so there are no third-party sheets to keep
working, and an alias is a reserved word carried forever for a migration nobody
needs. An old sheet fails on a missing required column, which is correct.

🔒 **No new columns.** The `position_id` and `t` once planned here are gone
with §3.1's revision: `t` is a column of the output tables, not of the series
table, and `(path, series_index)` is unique again without it.

🔒 **The word stays `series`** — `series_id`, `series_index`, `series.tsv`.
`stack_id`, `position_id` and `image_id` were considered and rejected (§6.19).
`note/data_formats.md` gains a two-line glossary: a Luxendo *stack* is a series;
an ImageJ *stack* is the planes of one frame.

🔒 **`series_index` means "the index the container's own reader addresses this
series by"** — the Bio-Formats series for a file, the `stack` for a Luxendo
acquisition. True for both, so the column stops being a borrowed meaning.

🔒 **`alias` stays required, and keeps its name.** It is never read today, but
on a Luxendo table it is the only record of the alias a rescan must reuse —
without it a forgotten alias falls back to the folder name and every id and
hand edit is orphaned. A rename would have to happen on `files.tsv` too, or one
concept gets two words. What changes is its description: "short handle for the
container `series_index` counts within — a file for Bio-Formats, an acquisition
directory for Luxendo; unique across a series table".

🔒 **One series sheet for every format.** Luxendo does not get its own sheet:
with the definitions above nothing is borrowed any more, and a second sheet
type would make the batch and every R CLI learn two shapes. The address differs
between formats and that is the resolver's business (§3.2b). If a format arrives
whose address is not `(path, index)` — an OME-Zarr plate's row/column/field —
the answer is another route table like `sources`, not another series sheet. The
schema's sheet name follows: `samples` → `series` (one literal in
`cli_helpers.r`).

🔒 **`series_id` stays hand-editable** (`seeded`), for readability; decided
2026-10-02. An edited id must still pass the duplicate check. Editing it after
a run means the results carry the old id, so the analysis must be re-run to
match — the operator's choice to make. (Making it derived-only was proposed and
declined.)

⚠️ **So nothing may JOIN on it that the operator cannot see change.** Today
`sources.tsv` joined the series table on `series_id`, and an edit would have
orphaned every source row. Since `vocab` the sources table is keyed on
**`(alias, series_index)`** — the acquisition and the stack, both machine
columns the operator never edits, unique together now that a series is the
whole position (§3.2b), and safe when two acquisitions share one table —
and `series_id` is looked up from the series row (`LuxendoScan.withSeriesId()`). The regeneration key was already
`(path, series_index)`, so edits survive a rescan.

### 3.2b The sources table — when one series comes from many files

🔒 **The friction this resolves.** Every format the repo read before Luxendo put
one or more *series inside one file*. Luxendo puts **one series across several
files** — one per channel, one per timepoint. The series table cannot absorb
that: a row would need more than one `path`.

🔒 **So a second table carries it**, one row per source file, keyed to
`series_id`. This is not a new pattern: `files.tsv` → `samples.tsv` is already
"a file table and a series table", and this is the same pair with the cardinality
reversed. Declared in `schema/sheet_columns.tsv` under sheet `manifest`, read at
run time by both languages.

🔒 **`include` moves to the series table**, one value per series. Carrying it
per source made "rows of this output disagree on include" a representable state,
which was a real source of confusion in use. Moving it makes that state
**unrepresentable** rather than better handled.

🔒 **`target_output_path` is dropped outright, not moved.** It was only ever
`series_id + ".tif"`, so it is derived rather than stored: the output name comes
from `series_id`, plus `_t<TTTT>` when one time point is taken out of a
multi-frame series, with the scale token and extension added as before. A stored copy of a
derived value is a second thing to keep in step.

The sources table is therefore: `source_path, series_id, channel, channel_name,
t, size_x, size_y, size_z, pixel_width, pixel_height, pixel_depth, pixel_unit,
source_bytes`. `channel_name` stays because it is per channel and so cannot live
on a one-row-per-series table; everything else the series table already carries.

🔒 **The series table needs no new columns.** `samples` already declares
everything, once the four columns of §3.2b are read the Luxendo way:
`size_c` is the channel count, `size_t` the frame count, `file_size` the sum of
the sources, `pixel_type` `uint16`.

#### 🔒 The four columns that presume one-file addressing

`samples` declares `path`, `series_index`, `series_name` and `alias` all
`required=yes`. Read the Luxendo way, every one of them is now true:

| column | Luxendo meaning |
|---|---|
| `path` | the acquisition directory — the root the sources table resolves against |
| `series_index` | the `stack` number — the series index, by §3.2's definition |
| `series_name` | `stack_description`, before sanitising |
| `alias` | a short handle for the acquisition, **the operator's**, defaulting to the folder name |

🔒 **`alias` is a parameter, not the folder name.** The folder name is only its
default, exactly as `files.tsv`'s alias defaults to the basename — the point of
the column is that a run need not be named after whatever the camera called the
directory. `series_id` carries it, built by `SeriesSheet.composeSeriesId()` rather
than a second copy of the rule, which is what makes a series id unique
**across** acquisitions and not merely within one. Measured: two real
acquisitions shared all 14 stack identities, so all 56 of the smaller one's
series ids collided before the alias was added, and 0 after.

#### `series_index` was not unique — resolved by folding time into the series

Until 2026-10-02 a Luxendo series was one (position, time point), so the stack
number repeated once per time point — 56 rows, 14 values — and
`SeriesSheet.mergeKey()`, which is `(path, series_index)`, collapsed 56 rows to
14. The plan then was two new columns, `position_id` and `t`. Making the series
the whole position fixes it at the root instead: one row per stack, so
`(path, series_index)` is unique and stable, `mergeKey` needs no change, and H10
is gone. It persists only for tables made with `gatherFrames` off (§4).

🔒 **A running counter per row stays ruled out**, for the record: it restores
uniqueness and destroys stability — an acquisition grows, and every counter
after the new time points shifts, orphaning results folders. `CLAUDE.md`
settled the same shape for the series id's index padding, and the comment on `SeriesSheet.INDEX_FORMAT` records
what it cost last time.

🔒 **A rescan must refuse a renumbering.** The stack number is written into
every file's sidecar at acquisition and cannot change on a rescan, but nothing
verifies that a regenerated row is the series it replaces. So: the same
`(path, series_index)` with a **different `series_name`** is a renumbering, and
the merge stops rather than carrying hand-edited metadata onto the wrong series.
Today `merge()` overwrites `series_name` silently, as a machine column — which
would also mis-carry metadata on a LIF re-exported in a different series order.
In `vocab`, for both formats.

#### 🔒 One resolver, three callers

How a row becomes pixels is now needed by the batch runner, by
`Make_LuxendoTiff` and by `Make_OverviewStack`. Three copies is how the fork
happens quietly, so it is **one resolver** that all three call, reporting which
route it took the way `BatchRunner` already records `open_method`.

⚠️ **For a multi-frame series it hands back one frame at a time**, never the
whole series: a Luxendo position is ~95 GB against a ~9 GB heap (§5.10). That
makes the resolver part of `time_axis`, not of `luxendo`, and it is why it is
not built yet.

⚠️ **The rule is membership, not plausibility.** A series resolves from the
sources table *if the table has rows for its `series_id`*; otherwise from
`path`; **neither is an error naming the series_id.** Sniffing whether `path`
"looks like a real file" is the class of heuristic this repo has already been
bitten by (the greedy filename prefix; `Slice` meaning three different things) —
a stale path or a moved mount silently takes the wrong branch and surfaces
somewhere else. Membership also lets one sheet mix Luxendo and non-Luxendo rows.

### 3.3 `_outline.txt`

🔒 Gains a `t` column, **always** — one frame is `t = 1`: `name, roi, t, z, x, y`.
Decided 2026-10-02: one file shape for every image, rather than a column that
comes and goes. Reading by absence (§3.7) remains for files written before.

🔒 **Every image axis this repo writes counts from 1** — channel, z and t, as
ImageJ shows them. Settled 2026-10-02 and recorded in
`note/fiji_vocabulary.md`; the instrument's own names (`Cam_long_0005`) and
`series_index` (a Bio-Formats address, not an axis) keep their own count.

### 3.4 `_res.txt`

🔒 Gains explicit `roi`, `z`, `t`, `ch` columns, **always**, **written by us**,
not parsed back out of anything. ImageJ's own `Ch`/`Slice`/`Frame` go: `stack`
leaves the forced Set Measurements list (decided 2026-10-02), because §5.3
shows they mean different things on different shapes, and beside our `ch` they
would be two channel columns that can disagree — and collide once R
lower-cases the names. `measureRois()` already knows the true channel and
`slices[i]`, and once it loops, the frame; `rt` is our own `ResultsTable`.

⚠️ `z` is present for a human reading the table. It must not reach a join —
see §3.6.

⚠️ **The Groovy and R changes land in the same PR.** `read_fiji_result()` ends
with `left_join(res_df, res_label_info, by = "label")` and `res_label_info`
already carries a `roi` column; a `roi` column in `_res.txt` becomes
`roi.x`/`roi.y` and every downstream reference silently disappears. Verified.

### 3.5 ROI ids

🔒 `<feature>_TTTT-SSSS-NNNN-YYYY` when the image has more than one frame,
`<feature>_SSSS-NNNN-YYYY` otherwise. Conditional — unlike the `t` column, on
purpose: the join is on `(roi, t)`, so a single-frame id needs no frame field,
and adding one would change every single-frame ROI id for nothing. Readers
need both shapes anyway, for old data. `TTTT` counts from 1, like `SSSS`.

Forced, not cosmetic: `saveRoiZip()` uses the ROI name as the zip entry name, so
four frames of one object give four identical names and `ZipOutputStream` throws.

### 3.6 Joins

🔒 Measurements join outlines on **`(roi, t)`**. Both are keys, so no non-key
column is duplicated.

🔒 **The outline table is authoritative for `z`.** Before dropping the
measurement table's `z`, **assert the two agree on the overlap**, then drop, and
**report the drop** the way `.cli_read_series_sheet()` already reports dropped
columns. Worth a small shared helper — `t` will want the same treatment as soon
as a second table carries it.

### 3.7 Backward compatibility

🔒 **By absence, not by flag.** A table with no `t` column is one time frame.
Every existing IF and oocyte output keeps working untouched and no caller opts
in. Same shape as `readParams` tolerating missing keys. New files always carry
`t` (§3.3); absence is now only how an *older* file reads.

### 3.8 Where `t` comes from, and recording it

🔒 **One way: the frame index inside the series, counted from 1.** There used to be two — frames,
or a series-table `t` for per-time-point Luxendo files — and the series-table
one went with §3.1's revision. A table made with `gatherFrames` off still
produces single-frame series, which this design analyses as unrelated images;
and is why the option is removed in `vocab`.

The **frame interval** is the time calibration, as `pixel_depth` is for z, and
belongs in `_config.txt` — as **provenance**, like `pixel_depth`, not as a
parameter: it is measured off the image, never chosen. Luxendo
records it in every sidecar (`metaData.triggers[].interval_s`, 1800 s on the
real acquisition), Bio-Formats exposes it for LIF as `frameInterval`. Blank when
unknown — never a default of 1, by the rule for `pixel_depth`.

---

## 4. Milestones

### `luxendo` — Luxendo input transform

Read `.lux.h5` and write files the existing pipeline accepts. Format facts are
in `note/luxendo_file_format.md`; do not duplicate them here.

#### Part 1 — reading the format, and the manifest  ✅ done

- [x] Read `.lux.h5` with **JHDF5** (`ch.systemsx.cisd.hdf5`). Bio-Formats
      cannot read them at all, and `BDVReader` on `bdv.xml` returns the wrong
      specimen silently — both verified, both in the format note.
- [x] **Assembly manifest**: one row per *source file*, carrying the output it
      feeds — a single row with a variable-length list of sources would not
      survive being a TSV. Identity column `series_id`; also `t`. (A `position_id` was planned here and never shipped; dropped, §6.20.)
- [x] 🔒 **Manifest gets a `schema/` entry**, like `sheet_columns.tsv` — read at
      run time by both languages, two readers from day one.
- [x] **Generic assembler**: manifest → TIFF.
- [x] 🔒 **Three entry modes**: (i) end to end from a Luxendo directory,
      (ii) **manifest only**, (iii) assemble from an existing manifest.
      (ii) is what lets a person see the plan before committing to 33 GB.
- [x] 🔒 **One file per (position, timepoint), classic TIFF by default**;
      `--format bigtiff` as an explicit alternative. See §5.1 and §6.1.
- [x] ⚠️ 🔒 **`setCanDetectBigTiff(false)`, always.** Otherwise Bio-Formats
      silently upgrades to BigTIFF above the ceiling and the drag-and-drop
      guarantee is gone with no error. See §5.1.
- [x] Predict output size from the manifest and **warn before writing**.
      Over-limit for the chosen format is a **per-row failure recorded in the
      summary, not an abort** — `BatchRunner.runEach()`'s contract. The message
      names `--format bigtiff`.
- [x] 🔒 **One assembler with a scale option**, not two entry points. Two
      gatherers differing only in scale is the fork `CLAUDE.md` forbids.
      Scale and `gatherFrames` are independent, both non-persistent. Built as
      `scalePercent`, 1..100 **percent of the original**, not a divisor: a
      percentage needs no explaining in the dialog, and the divisor's unbounded
      top end was a footgun (`resize=100000` produced a 1x1 image) rather than a
      capability.
- [x] ⚠️ **Resizing must scale the calibration.** Halve the pixels and
      `pixel_width`/`pixel_height` must double, or every area is wrong by the
      square of the factor while the image looks perfect. Test: assemble one
      position at 100% and 50%, assert the physical extent matches.
      **By the ratio ACHIEVED, not the one requested** — the pixel count is
      rounded, so 33% of 2048 is 676 px (ratio 3.0296, not 3.0303). Measured on
      the real acquisition: achieved reproduces 425.98402 um to five decimals,
      requested would have recorded 426.08488.
- [x] A non-default scale puts a token in the output filename:
      `_downscale<PC>pc`, which cannot be misread as a divisor.
- [x] Per-output **provenance record**: source paths, checksums, gatherer
      version, scale. Keyed per (timepoint, channel) when gathered, or three of
      a gathered position's twelve sources would be the only ones named.
- [x] Honour `include`, with exactly `BatchRunner.isIncluded()`'s vocabulary.
- [x] **Verification mode**: re-read output planes and compare to source by
      checksum. "39 slices, 3 channels" passes happily while channels are
      transposed — this is the only check that can fail correctly.
- [x] ⚠️ Assert z uniform **across timepoints of one position**. It is *not*
      uniform across positions (16..39, one at z=1). A **warning** when writing
      one file per timepoint, where the run is still well defined; **fatal**
      under `gatherFrames`, where there is no single volume shape to build the
      hyperstack from.
- [x] 🔒 `raw/` — the Luxendo acquisition directory — **is never modified or
      deleted.** Converted TIFFs are derived and may be regenerated.

#### Part 2 — the two sheets, after the design review

A long design pass (recorded in §6.12–§6.17) established that the manifest alone
is not enough: the batch runner needs a **series** table to loop over, because
that is what carries `include`, `prefix` and the operator's metadata. So the
scan emits both, from the one pass, and the milestone grows these items.

🔒 **The analysis workflow this produces**, end to end:

```
1. Make_LuxendoSheets      scan -> series table + sources table
   (edit the series table: include=, condition=, genotype=)
2. Make_LuxendoTiff        a FEW series, ONE time point each (frames=0), FOR TUNING ONLY
3. Run_NucleusSelector     tune the threshold on one of those
4. Run_NucleusSelector_Batch   detection + measurement, reading .lux.h5 directly,
                               streaming frames                  (needs time_axis)
5. Make_OverviewStack      per series, z-projected, + overlay   (the QoL milestone)
6. the R CLIs              unchanged
```

Since 2026-10-02 a series is a whole position, so step 4 cannot run until
`time_axis` gives the batch a frame loop that never holds a whole series.

🔒 **Step 2 is small and optional, and that is the point.** Step 4 assembles
channels in memory and never reads step 2's output, so **the dataset is never
duplicated on disk.** Converting everything first was rejected for exactly this
reason (§6.12): the sources are already the pixels, so a required conversion
step means permanently holding two copies of an 800 GB acquisition.

⚠️ **Step 1 → step 4 has a human edit in between**, and it is where a two-table
design first goes wrong. Regeneration must match on `series_id`, never
re-propagate a seeded column silently, and report every carry-over — the
discipline `Make_SeriesSheet` already has. A join matching nothing must be a
loud error: a silent empty run is the failure this repo is built around.

- [x] 🔒 **`Make_LuxendoSheets.groovy`** — one scan, two tables. Named for what
      it makes; `Metadata` was considered and dropped because "metadata" in this
      repo means the operator's columns, which this script does not write.
- [x] 🔒 **The scan reads the sidecar `.json`, not the HDF5** — §5.6. Pair on
      the JSON *and* require the `.lux.h5` sibling, so a stray JSON from
      someone else's analysis cannot invent a row, and `main_raw.lux.h5` (which
      has no sidecar) is excluded for free. `holdsPixels` is then deleted
      rather than moved.
- [x] 🔒 **`quickScan` on by default**: one sidecar per *directory* for the
      dimension columns rather than one per file, since they are constant within
      a channel directory. ⚠️ Verified against sibling file sizes — within a
      directory they vary by **8 bytes** while one z-plane is 8,388,608, so a
      deviating timepoint stands out by a factor of a million. That turns
      "sampled one file" into "sampled one file and checked the other 95".

      ⚠️ **The time point is the one fact a sampled sidecar cannot supply**, because
      it is the axis a channel directory runs along. Reusing the sampled
      sidecar's `time_point` gave every file `t=0`, which the duplicate-source
      check then correctly rejected — caught by the tests, not by inspection. It
      now comes from the filename suffix, the single place in this repo where
      identity touches a path, and only after the mapping has been **confirmed**
      against the sampled file's real `time_point`; a directory where they
      disagree is read in full and says so.
- [x] `Make_LuxendoTiff` **narrowed**: takes the two sheets instead of scanning,
      and is for tuning and drag-and-drop only. Basenames from `series_id`, plus
      `_t<TTTT>` for a time point taken out of a multi-frame series (Part 3).
- [x] 🔒 `include` **moves to the series table**; `target_output_path` is
      **dropped** and derived. See §3.2b.
- [x] ⚠️ **The series id carries the alias**, and the alias is a parameter.
      `s<NNNN>_<stack_description>` is unique only within one acquisition:
      measured on two real ones, all 14 stack identities were identical, so all
      56 of the smaller run's series ids collided and two runs in one output
      directory would have overwritten each other's results. Built with
      `SeriesSheet.composeSeriesId()`, so the prefix rule stays in one place.
      This was a bug in Part 2 as first written — `CLAUDE.md`'s "unique by
      construction" rule was broken without anyone noticing.

Two questions Part 2 left open for `vocab` are now settled in §3.2:
`series_index` stopped being the position index by becoming the series index
(a series is the position), and `alias` stays required with its name, with a
description that says what it is unique within.

**Moved to `time_axis`**, where it now belongs (§3.2b): **the one resolver** —
membership in the sources table decides, never a guess about `path` — and the
batch runner's **one optional sources parameter**. Forking
`Run_NucleusSelector_Luxendo_Batch` stays rejected (§6.13). The resolver has to
hand back frames one at a time, which is the `time_axis` design.

**Verification of Part 2.** Both tables written for the 800 GB acquisition,
measured at ~8 minutes rather than ~48 (§5.6). Re-measured on 2026-10-02 at
175–231 s — network conditions vary — and superseded by Part 3's index route.

#### Part 3 — after v0.6.0: the index file list, and one series per position  ✅ done

Branch `luxendo-index_scan`. No column changed; the default layout did.

- [x] 🔒 **The file list comes from `bdv.h5` + `bdv.xml` when both are there**
      (`listing=auto`), not from walking `raw/`. Identity still comes from one
      sidecar per directory; every placed file is checked against the index
      (time point, channel, stack, size, voxel) and a disagreement stops the
      scan. `listing=walk` keeps the v0.6.0 route and, with an index present,
      reports any difference between the two file sets — the only way to see a
      file the index does not list. `LuxendoIndex.groovy`;
      `note/luxendo_file_format.md` §7 has the facts.
- [x] ⚠️ **`<tile>` is never read**: it is the setup's ordinal in *text* order of
      the stack and equals the stack for 6 of 42 setups. The stack is compared
      through the setup name's `st:N`.
- [x] 🔒 **Option A: every listed file is stat-ed.** It is ~all of the route's
      cost (51–89 s of ~1 min over samba for 4032 files) and buys `source_bytes`,
      proof the file is there, and quickScan's size check. **Option B** — skip
      the stat, ~3–6 s — is recorded, not built: a missing file would surface
      only when the batch opens it, `source_bytes` would become optional or
      verify-only, and a time point of a different depth would rest on
      `bdv.xml`'s one size per setup. Revisit only if a minute becomes too long.
- [x] 🔒 **`gatherFrames` on by default** — one series per position, §3.1. Off
      still gives the v0.6.0 per-time-point layout. **Removed in `vocab`**
      (decided 2026-10-02): `Make_LuxendoSheets` will write only the folded
      layout.
- [x] `Make_LuxendoTiff` gains **`frames`** (`0`, `0,47,95`, `0-3`; blank =
      every frame in one file, as before): each chosen time point of a
      multi-frame series is its own file, `<series_id>_t<TTTT>` — the same name
      and the same pixels the per-time-point layout gives that frame (asserted).
- [x] ⚠️ **An output too big for the heap is a FAILED row, decided from the
      tables.** `assembleOne` builds the whole stack before writing, and
      `bigtiff` has no size cap, so a 96-frame position (~94 GB) would otherwise
      read ~8 GB over the network and die of `OutOfMemoryError`. Limit: 75% of
      `maxMemory()`.

**Verification, as carried out.** Groovy: `Test_LuxendoScan` 105, `Test_TiffAssembler`
116, `Test_LuxendoFile` 37, `Test_LuxendoSidecar` 49 — all passed, 0 failed;
`LuxFixture` now writes a `bdv.h5` + `bdv.xml` laid out as Luxendo's, setups in
text order of the stack. "Index and walk agree" could also mean "the index route
never ran", so the tests separate the two: an index that omits one file gives
one row fewer on the index route and a warning on the walk; an index naming a
missing file, a wrong size, or a swapped time point stops the scan. Real data,
the 800 GB acquisition over samba: `Make_LuxendoSheets` with the new defaults
in 77 s, **`sources.tsv` byte-identical to v0.6.0's** and `series.tsv`
identical apart from the hand-edited `include`; the per-time-point layout
likewise (78.6 s). `listing=walk` on the same acquisition: 231 s, "the walk
and bdv.h5 list the same files", tables byte-identical to the index route's. Not verified: an acquisition with no index on a real mount
(the walk route there is v0.6.0's, unchanged), and any Luxendo software version
other than Embedded v3.17.3.

#### Part 1's verification, as carried out

Assemble two positions including `L26A pos3` (z=1), checksum against source, and
open one in Fiji by drag-and-drop.

**Done**, on branch `luxendo-input_transform`. 175 Groovy checks across
`Test_LuxendoFile`, `Test_LuxendoScan` and `Test_TiffAssembler`, all synthesising
their own `.lux.h5` through `tests/groovy/LuxFixture.groovy`, plus the real
33 GB acquisition end to end. Two things were added that this list did not ask
for and that the work showed were needed: `skipExisting`, which resumes an
interrupted run and requires the provenance file as well as the image so a
half-written output is redone; and the manifest being written **before** any
pixels and unconditionally, because a plan that only survives a successful run
is not a plan.

### `vocab` — identity and vocabulary

One schema migration, **two PRs, one release (v0.7.0)**. Both open questions
were settled on 2026-10-02 (§3.2: `series_id` stays editable; `gatherFrames` is
removed), so there is no separate decisions PR.

🔒 **The split follows what the code forces.** `schema/sheet_columns.tsv` is read
at run time by both languages, so renaming the sheet or its id column breaks R
unless the R sheet reader changes in the same PR. Inputs and outputs, on the
other hand, separate cleanly. **Release after PR 2, not between them**: in
between, `main` is consistent but speaks two vocabularies (sheets say
`series_id`, outputs say `sample`).

#### PR 1 — `vocab-series_table`: the sheets, in both languages  ✅ done

Everything that reads or writes `files.tsv`, `series.tsv` and `sources.tsv`.

- [x] `schema/sheet_columns.tsv`: sheet `samples` → `series`, column `prefix` →
      `series_id`, the §3.2 descriptions of `series_index` and `alias`.
      `SheetSchema.SERIES` / `ID_COLUMN` name them once on the Groovy side.
- [x] `samples.tsv` → `series.tsv` everywhere it is named;
      `config/series_sheet_template.tsv` → `config/series_template.tsv`.
- [x] Groovy: `SeriesSheet`, `Make_SeriesSheet` (`allowDuplicatePrefix` →
      `allowDuplicateId`), `BatchRunner` (and the `series_id` column of
      `batch_summary.tsv`), `Run_Overview_Batch`, `SheetSchema`, `LuxendoScan`,
      `Make_LuxendoTiff`.
- [x] ⚠️ **`sources.tsv` keyed on `(alias, series_index)`**, not `series_id` —
      refined from "`series_index`" at implementation, so two acquisitions in one
      table cannot collide on a stack number. One join,
      `LuxendoScan.withSeriesId()`.
- [x] `gatherFrames` removed; asking for it is refused, naming `frames`.
- [x] R: `.cli_read_series_sheet()` (default `id_column = "series_id"`, the
      `sheet == "series"` literal), the CLIs' `--id_column` default,
      `group_montage_cli.r` (and `montage_index.tsv`'s `series_id` column, since
      that file is read back in as a sheet).
- [x] 🔒 **`merge()` refuses a renumbering**: same `(path, series_index)`,
      different `series_name`; nothing is written.
- [x] ⚠️ **An old sheet is named as one**, on both sides (`SheetSchema.requireId`,
      `.cli_read_series_sheet()`), before the duplicate check can misreport it;
      `Make_SeriesSheet` pointed at an old sheet renames `prefix` in place,
      edits kept (`SeriesSheet.migrateOldId`).
- [x] The glossary, `note/data_formats.md` §1, `tests/testthat/test-data_formats.R`,
      `README.md`, `config/`.

**Code identifiers followed, in the same PR** (asked for at review): the old
word was gone from the data but not from the code, so `Make_SampleSheet` →
`Make_SeriesSheet`, `SampleSheet` → `SeriesSheet` (and `Test_SeriesSheet`),
`composePrefix()` → `composeSeriesId()`, `duplicatePrefixes`/`checkPrefixes` →
`duplicateIds`/`checkIds`, `.cli_read_sample_sheet()` / `.cli_apply_sample_sheet()`
→ `.cli_read_series_sheet()` / `.cli_apply_series_sheet()`, the R CLIs'
`--sample_sheet` → `--series_sheet`, and the schema's `manifest` sheet → `sources`.
⚠️ `--series_sheet` breaks every saved CLI command — fine below 1.0 with one
caller (`CLAUDE.md` § Versioning records that a flag rename is a command
change, not an output one), and argparser fails loudly on the old flag. The
`sample` column of R's outputs is PR 2.

**Verification, as carried out.** R 4.6.1: 964 passed, 0 failed (main: 955; the
one warning is the same on main). Groovy, every file: `Test_SeriesSheet` 106,
`Test_BatchRunner` 131, `Test_LuxendoScan` 111, `Test_TiffAssembler` 117,
`Test_LuxendoFile` 37, `Test_LuxendoSidecar` 49, `Test_RunConfig` 90,
`Test_NucleusPipeline` 62, `Test_Overview` 126, `Test_RoiExport` 28,
`Test_BuildMask` 108, `Test_NucleolusDetect` 26 — 0 failed. Real data, read
only, outputs in scratch:
- the rnf4 oocyte project's `files.tsv` (two tile-merged `.lif`, 111 series):
  `main`'s and this branch's `Make_SeriesSheet` agree on every row and cell,
  only the id column's header differs;
- its hand-edited `samples.tsv`, migrated by `Make_SeriesSheet`: identical apart
  from the renamed header and an `ovary` column the merge adds because
  `files.tsv` has it and the old sheet did not (behaviour already on `main`);
  every `use`, `section_id` and `include` edit kept;
- the same old sheet handed to `Run_NucleusSelector_Batch`: refused naming
  v0.7.0, nothing written;
- the 800 GB Luxendo acquisition: `series.tsv` and `sources.tsv` equal to
  v0.6.0's `per_pos_*` in all 14 and 4032 rows once `series_id` is translated to
  `(alias, series_index)` — 0 differing cells; 85.5 s;
- the 33 GB acquisition with one `series_id` hand-edited to `my_embryo_A`:
  `Make_LuxendoTiff frames=0` found its sources, wrote and verified
  `my_embryo_A_t0000.tif`, skipped the 13 excluded series.

#### PR 2 — `vocab-output_identity`: what the analysis writes  ✅ done

- [x] 🔒 **One `Series id` field** in `Run_NucleusSelector` (and `Run_Overview`,
      so its PNG names keep matching), `persist=false`, blank by default. Blank
      takes the id from the image title (`RoiExport.seriesIdFromTitle`); typed,
      it is used as given — **refused, not rewritten**, if `sanitize()` would
      change it (`checkSeriesId`), which the batch now also applies to a
      hand-edited sheet id, failing that row only. Not a `PARAM_TYPES` key — a
      config setting it would name every image of a batch alike — and written
      as the `series_id` provenance field, replacing `output_basename`.
      **Revised at implementation** from the derive/explicit mode planned here:
      with one field holding either a pattern or an id, persistence has no safe
      setting (remembered, an explicit id mislabels the next image; reset, so
      does the pattern), and a mode `choices` list is not validated headless.
      A pre-filled default as Fiji's Duplicate dialog has is not possible in a
      `#@` script (no initializer for scripts in SciJava 2.99).
- [x] **`output_prefix` retired, not just taken out of `name`**, and with it
      `position_pattern`. Found at implementation: the R side finds
      `<name>_config.txt` and `<name>_<roi>_res.txt` from the outline table's
      `name`, so a prefix kept in the file names but not in `name` would hide
      both — no `volume`, no signal, a warning only. The file stem and `name`
      are now one string everywhere. Both keys are `RunConfig.RETIRED_KEYS`:
      skipped with a log line rather than refused, because every pre-v0.7.0
      `_config.txt` carries `position_pattern`.
- [x] R outputs: `sample` → `series_id` in the features `.rds`/`.tsv`,
      `feature_stats.tsv`, `feature_rejects.tsv`, `feature_counts.tsv`, and
      `n_sample` → `n_series` in the counts summary; the `--group_by`,
      `--color_by` and `--facet` defaults that named it; internal identifiers.
      An R output written before v0.7.0 is refused, naming the version
      (`.cli_require_series_id`), by every CLI that reads one.
- [x] ~~Regenerate the fixture~~ — **kept**, deliberately. With the prefix
      retired, `GRV_Position010` is simply a typed id, so nothing in it changes
      meaning. Its v0.2.0 `_config.txt` carries `position_pattern`, which now
      exercises the retired-key path. `note/data_formats.md` §2, §3, §5.

**Verification, as carried out.** R 4.6.1: 973 passed, 0 failed (main 964).
Groovy: `Test_RoiExport` 33, `Test_RunConfig` 105, `Test_NucleusPipeline` 66,
`Test_BatchRunner` 135, the rest unchanged — 0 failed. Real image, headless,
outputs in scratch:
- the fixture TIFF through `main` (`output_prefix=GRV_`, `position_pattern=Position`)
  and this branch (`series_id=GRV_Position010`), the fixture's own config: all
  13 files the same names; outlines, measurements and the six PNGs
  byte-identical; ROI zips identical in entry names and bytes (only the zip
  timestamps differ); `_config.txt` differs in `timestamp`, `script`, and
  `output_basename`+`position_pattern` → `series_id`. Nothing else moved.
- the same run with a blank id: named
  `20241216_dkD_DAPI_EGFP_Klf5_Nr5a2_forrep3.lif-Position010`, and identical
  to the above in every table apart from the id.
- the tracked fixture, made interactively in v0.2.0, against today's headless
  run: outlines byte-identical; `_res.txt` identical once the `-1` the
  duplicated window added to the title in `Label` is removed.
- the R CLIs (annotate → feature_stat → count with a sheet and `--group_by`) on
  the fixture under `main` and this branch: every table identical in every row;
  the headers differ by `sample` → `series_id` and `n_sample` → `n_series`
  only; the `.rds` equal in data and geometry.
- R on this branch's own Fiji output, both ids: `volume` present (found
  `<series_id>_config.txt`) and `ch<N>_signal` present (found the `_res.txt`).

**Then release v0.7.0**, with release notes saying old sheets stop loading and
are regenerated with `Make_SeriesSheet` / `Make_LuxendoSheets`.

**Moved to `time_axis`** (2026-10-02): `feature_id` as `<feature_type>_<NNNN>`,
numbered globally within a series. Today it is `nucleus_1` — unpadded, per
image — and the global numbering only means something once
`define_feature_group()` gains its time partition, which is `time_axis` work;
changing the id once rather than twice.

### `time_axis` — time axis through the pipeline

🔒 **A series is never held whole.** A Luxendo position is ~95 GB against a
~9 GB heap (§5.10), and an HPC node does not change the design: it must still
run on a 16 GB machine. So this milestone is "stream the frames of an image that
cannot be opened", not "loop over the frames of an open one". HPC buys
parallelism — one job per series, §5.8 — not a different design.

🔒 **Branching (decided 2026-10-02).** `time_axis` is the milestone's home
branch, at `VERSION` `0.8.0-dev` so that output made from it cannot claim to be
0.7.0. Each PR below is a `time_axis-<what>` branch, squash-merged into
`time_axis`; `time_axis` is merged (a normal merge) into `main` when all five
are in, with `VERSION` 0.8.0 as its last commit, and the merge commit is tagged.
`main` never carries a half-changed contract.

🔒 **Five PRs, one release (0.8.0).** The order follows what each needs:

| # | branch | scope | needs |
|---|---|---|---|
| 1 | `time_axis-contract` | the contract on an image that is already open: frame loop, `t`/`roi`/`z`/`ch` columns, `TTTT-`, `_threshold_stats.tsv`, frame interval, the 1-based rule on the Luxendo side, the `(roi, t)` join, `feature_stats` keyed on `t`, the ROI-zip fix | synthesised images |
| 2 | `time_axis-features` | R grouping: the time partition, global numbering, `<feature_type>_<NNNN>` | PR 1 |
| 3 | `time_axis-stream` | the one resolver, frames streamed, per-frame staging joined at the end, `batch_summary.tsv` per frame, the overview TIFF | PR 1; one real Luxendo position |
| 4 | `time_axis-resume` | skip finished frames, refuse a resume under other settings, clean up, the stop-at-frame-k test | PR 3 |
| 5 | `time_axis-threshold_scope` | `nucleus_threshold_scope`, the two-pass histogram, choosing its default on real data | PR 3; real data |

#### PR 1 — `time_axis-contract`

- [ ] 🔒 **The frame loop processes one single-frame image per `t`.** An open
      multi-frame image is one source of frames (a `Duplicator` copy of one
      frame at a time); PR 3 adds the streaming source and reuses the same
      per-frame code rather than restructuring it. A single-frame image is
      passed through as itself — no copy. Every whole-stack step (the pooled
      histogram, the `Duplicator` of the full z range) is then per frame for
      free: the three places frames were hardcoded to 1
      ([NucleusPipeline.groovy:312](../scripts/groovy/NucleusPipeline.groovy),
      [RoiDetect.groovy:162](../scripts/groovy/RoiDetect.groovy),
      [RoiExport.groovy:121](../scripts/groovy/RoiExport.groovy)) only ever
      see one frame.
- [ ] `_outline.txt` gains `t`; `_res.txt` gains `roi, z, t, ch`; `stack` leaves
      Set Measurements; ROI ids gain `TTTT-` when frames > 1. Results are
      written once, at the end, from all frames — the ROIs and table rows are
      small; only pixels are not.
- [ ] 🔒 **`<series_id>_threshold_stats.tsv`, always**: one row per frame —
      `t`, `nucleus_threshold_used`, `nucleus_mask_pct`,
      `nucleus_circ_rejected`, `nucleus_count`, `nucleolus_count`. The
      nucleolus threshold is not in it (per nucleus per slice). `_config.txt`
      keeps the totals; its `nucleus_threshold_used` and `nucleus_mask_pct`
      are unchanged for one frame and the literal `per-frame` for several.
- [ ] `_config.txt` gains `image_frames`, and `frame_interval` + `frame_unit`
      (provenance; blank for one frame or when unknown — never 1).
- [ ] A multi-frame image writes **no overview** in PR 1, and says so in the
      log; its overview is PR 3's TIFF. A single frame's PNGs are unchanged.
- [ ] ⚠️ `saveRoiZip()` leaves a **189-byte partial zip** when it throws.
      Write to a temp path and rename on success.
- [ ] 🔒 **The 1-based rule on the Luxendo side**: `sources.tsv`'s `channel`
      and `t`, `Make_LuxendoTiff`'s `frames=`, the `_t<TTTT>` names and the
      `_gather.txt` keys count from 1. ⚠️ An older `sources.tsv` would be off
      by one *silently* — but every one of them has `t = 0` and `channel = 0`
      rows, so "both ≥ 1" refuses every old table, naming v0.8.0.
- [ ] R: the outline and measurement readers take `t` (absent means 1);
      measurements join outlines on `(roi, t)`; the explicit columns win over
      the `Label` parse, after **asserting they agree**; `feature_stats.r`
      keys on `(roi, ch, t)` (H5). Annotating a table with more than one `t`
      is **refused** until PR 2 — grouping across frames would merge one
      object's frames into one feature.
- [ ] ⚠️ 🔒 **Any new run parameter lands in four places at once** (CLAUDE.md
      § The output contract). PR 1 adds none: `image_frames`,
      `frame_interval` and `frame_unit` are provenance.

**Verification.** Single-frame: the fixture's columns that existed are
byte-identical apart from the dropped `Ch`/`Slice`, and the new `roi`, `z`,
`ch` equal what R parses out of `Label` today. Multi-frame: a synthesised
4-frame stack gives four times the ROIs with distinct ids, each frame's
measurements equal to the same frame analysed alone, and `t` counted 1–4.

#### PR 2 — `time_axis-features`

- [ ] ⚠️ `define_feature_group()` has **no partition argument today**
      ([define_feature_group.r:72-83](../scripts/R/define_feature_group.r)).
      Add one, **and make the numbering global within the series**, or every
      timepoint emits `nucleus_1`. 🔒 And change the id format here, once:
      `<feature_type>_<NNNN>`, four-digit padding (moved from `vocab`). The
      fixture's R expectations, `note/data_formats.md` §5 and the R tests move
      with it. Lifts PR 1's refusal of multi-frame tables.

#### PR 3 — `time_axis-stream`

- [ ] 🔒 **The one resolver** (§3.2b): membership in the sources table decides,
      and a multi-frame series is handed over **one frame at a time**.
      `Make_LuxendoTiff` moves onto it.
- [ ] 🔒 **The unit of work is the frame, not the row.** `batch_summary.tsv`
      gains one row per `(series_id, t)`; a failed frame is recorded and the
      rest of the series continues, as `runEach()` already does for rows.
- [ ] 🔒 **Per-frame staging**: each frame's outputs are written to staging
      files under a temporary name and renamed when complete, then joined in
      `t` order when the series finishes. Without it, a crash at frame 90 of
      96 loses all 90. It is also what makes PR 4 cheap: a frame is done
      exactly when its staged files exist.
- [ ] 🔒 **The overview of a multi-frame series is DATA, written by the batch**:
      one 16-bit, z-projected, downscaled TIFF per series, channels and frames
      inside, appended frame by frame, no contrast decided. Single-frame series
      keep the PNG unchanged. Rendering is `QoL`'s `Make_OverviewStack` (H8).

**Verification.** One real Luxendo position end to end, run at a heap that
could not hold it whole — the proof that nothing loads the series.

#### PR 4 — `time_axis-resume`

Decided 2026-10-02: record the frames that succeeded, and resume from them.

- [ ] A rerun skips frames whose staged files exist and deletes leftover
      temporary files; `batch_summary.tsv` marks them as done earlier.
- [ ] ⚠️ **Refuse to resume under different settings**: a fingerprint of the
      run's parameters is kept with the staging, and a rerun whose parameters
      differ stops — otherwise one series' results mix frames analysed two ways.
      A `restart` option discards the staging instead.
- [ ] **The test**: stop a synthesised run deliberately at frame k, resume,
      and assert the final files are byte-identical to an uninterrupted run.

#### PR 5 — `time_axis-threshold_scope`

- [ ] 🔒 **`nucleus_threshold_scope = frame | series`**, a run parameter (so the
      four-place rule applies). `series` is a two-pass read — stream every frame
      into one histogram (tiny: 65,536 bins), choose, stream again — so it
      costs I/O, not memory. It needs the per-algorithm statics on a hand-built
      histogram, as the nucleolus already does, with `exec()` on a small image
      as the oracle, the way `Test_NucleolusDetect` keeps `AutoThresholder`.
      ⚠️ Per series is not obviously better: bleaching and changing
      expression dim a live time course, a fixed raw threshold slowly
      under-segments the late frames, and a per-frame auto threshold follows
      the drift. Compare both on real data before choosing the default. The
      existing workaround stands meanwhile: tune interactively, then batch with
      `Manual` and the range.

### `tracking` — linking across time

🔒 **Outsourced to TrackMate.** See §5.2.

- [ ] `Make_FeatureTracks.groovy`: feature centroids per
      `(series_id, feature_id, t)` → TrackMate LAP tracker → `tracks.tsv`
      carrying `(series_id, feature_id, t, track_id)` — one scope, since a
      series is the whole time course (§3.1).
- [ ] R joins `tracks.tsv` onto the feature table.
- [ ] Record the tracker settings used, as `_config.txt` records everything else.
- [ ] 🔒 **Calibrated units, never pixels.** In pixels `LINKING_MAX_DISTANCE`
      stops meaning a physical distance, and pixel size varies fourfold inside
      one `.lif` here, so a cut-off tuned on one dataset would be wrong on the
      next. See §5.4.
- [ ] ❓ **PENDING DECISION — does `z` take part in the distance?** Cannot be
      settled without real data; revisit at implementation and **ask rather than
      pick**. See §5.4. ⚠️ Try full calibrated 3D alongside `z = 0`, not after
      it: in an embryo, nuclei stacked in z are normal — the fixture has a pair
      that overlaps in x-y while sitting 30 slices apart — and xy-only linking
      would take them for one object. Neighbouring nuclei are ~one diameter
      (~20 µm) apart, so a 3D cut-off a little above the 5 µm z step may not
      over-link.
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

Also the current home of the **gathered overview**.

❓ **Its position in the sequence is deliberately not settled.** It is filed here
because nothing in the analysis depends on it, but it **cannot precede
`time_axis`** — it needs `t` on the outline tables — and on an 800 GB
acquisition "can I see what this contains over time" may not be a convenience at
all. **Decide during or after `time_axis`**, once the absence has been felt.

🔒 **Overviews, revised 2026-10-02.** A single-frame series keeps its PNG,
unchanged. A multi-frame series gets the 16-bit projection TIFF the batch
writes as data (`time_axis`), and this milestone **renders** it:
`<series_id>_overview_ch<N>.tif` — same name rule, the extension says it holds
frames — 8-bit, one display range per (series, channel) across all frames,
range recorded. R never makes a contrast decision, which keeps §6.14's point;
`magick` reads a PNG as one frame and a TIFF as many, so `img[t + 1]` serves
both. The earlier plan of a frame selection on the batch's PNG overview
(`0,47,95`) is subsumed: the selection moves to the render step.

- [ ] `montage_qc_cli.r` gains an optional **`--t`**: which frames of the
      overview to use (default all), and the filter that keeps the feature rows
      to the same frame. The panel extent stays the series' `_config.txt` one —
      z, x and y are uniform across a series' frames.
- [ ] A multi-frame QC montage is written as a **multi-frame 8-bit TIFF**, to
      scroll through time in Fiji; `mg_write()`'s 8-bit guard carries over.

🔒 `Open_*` is the verb — `Open_LifFile.groovy` already establishes it as "opens
something into the Fiji GUI for a human", interactive-only by nature. No fourth
verb needed.

⚠️ The ROI Manager needs a GUI, so the **logic** (join, filter, rename) lives in
a library class and the `Open_*` script is a thin caller — same division as
`NucleusPipeline` vs `Run_NucleusSelector`, and the only way any of it is tested.

- [ ] `Inspect_AnnotatedFeatures.groovy` — inputs: series table, R feature
      table, ROI directory. No side effect; prints `filename`, `series_name`,
      `series_index`, `t`, `n_<feature>`. After `vocab` the join is
      `series_id` on both sides with no prefix to strip.
- [ ] `Open_AnnotatedFeatures.groovy` — same inputs plus a series selector
      (number or `series_id`, default 1, `persist=false`) and an optional
      feature filter (blank = all). Opens the series via `BatchRunner`'s open
      path, loads the zip with `RoiExport.loadRoiZip()`, filters, renames with
      the `feature_id` prefix, adds to the ROI Manager.
- [ ] 🔒 **`Make_OverviewStack.groovy`** — one z-projected hyperstack per
      series, channels and timepoints inside, downscaled, with the detected
      outlines attached as an **overlay**. Built from the batch's projection
      TIFF, so it reads megabytes, not the 800 GB of source.

      🔒 **A `Make_`, not a mode of a batch.** `Run_Overview_Batch` drives
      `BatchRunner.runEach()`, which is strictly per row; a per-position stack
      needs per-group accumulation. Running it as a **second pass over finished
      outputs** removes the accumulation entirely — which is also why holding
      overviews in memory during the batch was rejected (§6.15).

      🔒 **Not Luxendo-named**, because the artifact is not Luxendo-specific:
      the same step is how IF QC could move to TIFF later. It resolves rows
      through the one resolver of §3.2b, so it reads `.lux.h5` or an ordinary
      file without caring which.

      🔒 **The overlay is attached after segmentation, to the written file.**
      Measured (§5.7): an ImageJ TIFF carries an `Overlay`, per-frame
      `tPosition` survives the round trip, and the pixels stay untouched 16-bit.
      So the assembler never needs to know about outlines, and the outlines stay
      vector and toggleable instead of burned in. Run it before segmentation for
      the plain stack, again afterwards to attach — same output path, idempotent.

      ⚠️ **It is a picture, not a count.** ROIs are per z-slice, so a projected
      stack needs them unioned in 2D, and `CLAUDE.md` already records that this
      merges objects overlapping in x-y: 5 outlines where R counts 6 nuclei on
      the fixture. Label it as an eyeball artifact wherever it is written.

      ⚠️ **One display range per (series, channel), not per frame.**
      `Overview.prepare()` computes `lo`/`hi` per image; applied per frame, a
      cell appears to brighten because the stretch moved. Over time that artifact
      looks like biology.

      ⚠️ The overlay is **ImageJ-specific TIFF metadata** — Fiji shows it, other
      tools silently ignore it. Fine for inspection, not an interchange format.
- [ ] `Open_LuxendoSeries.groovy` — one Luxendo series at one time point, all
      channels, calibrated, into a window, through `LuxendoFile` and the two
      sheets (or the one resolver once `time_axis` has built it). Drag-and-drop
      cannot do this — Bio-Formats has no Luxendo reader — and the HDF5 import
      opens one channel per file (`note/luxendo_file_format.md` §5). Added
      2026-10-02.
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
                       #   image_width/height, VERSION, open_mode,
                       #   frame interval
  roi     = <sf tbl>,  # per (series_id, roi) — t, z, geometry, area,
                       #   feature_id, feature_type
  measure = <tibble>,  # per (series_id, roi, ch) — the _res.txt measurements
  feature = <tibble>,  # per (series_id, feature_id) — t, feature_type,
                       #   parent_feature_id, track_id, z_span, containment,
                       #   match_kind, per-channel stats
  track   = <tibble>,  # per (series_id, track_id)
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
- [ ] every non-`NA` `feature$track_id` resolves in `track` within its series
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

### 5.6 The sidecar JSON, and what a network mount costs

Measured on an 800 GB acquisition over a samba mount, 4032 `.lux.h5` in 42
directories (14 positions x 3 channels x 96 timepoints):

| | per file | over 4032 files |
|---|---|---|
| `holdsPixels` (HDF5 open + dataset header) | 392 ms | **26 min** |
| `LuxendoFile.open` + `/metadata` | 322 ms | **22 min** |
| sidecar `.json` | 88 ms | 6 min |
| `File.length()` | 9.5 ms | 38 s |

The same calls on a local SSD were **4.5 ms** — so this is latency, not
bandwidth, and it scales with file *count*, not dataset size. The first scan of
this dataset therefore cost **~48 minutes** before printing its first count,
because the scan opened every file **twice**: once to test for `/Data`, once for
the metadata.

🔒 **There is a `.json` sidecar beside every `.lux.h5`** — 4032 of 4032 — and it
holds the complete manifest: `stack`, `stack_description`, `channel`,
`channel_description`, `time_point`, `voxel_size_um` and `image_size_vx`.
Checked against the HDF5 `/metadata` *and* the real dimensions on 25 spread
files: **25 of 25 agree exactly.** So identity still comes from content; it is
simply a cheaper file's content.

Measured end to end afterwards: **480 s** for the 800 GB acquisition against the
old path's ~48 minutes, a 6x improvement rather than the 48x the per-file
figures suggest — the directory walk, not the file reads, is what is left. On
the 33 GB acquisition it is 2.0 s with `quickScan` and 3.3 s without, and the
two produce **byte-identical** tables.

`main_raw.lux.h5` has **no sidecar**, which makes "has a `.json` sibling" a free
index-file test. Its top level is `[timepoint_0..3]` and it has no `/metadata`
at all — so dropping the content check *without* the sidecar swap would have
made the scan **throw on every real Luxendo tree**, not merely go faster.

The directory names also parse cleanly (`stack_<n>-<name>_channel_<c>-<name>_obj_<obj>`,
checked on 404 spread files, 0 disagreements), and that was the user's first
proposal. Rejected in §6.16.

⚠️ **Re-measured 2026-10-02: the walk took 175–231 s**, not 480 s, across three
runs on the same acquisition. Network conditions vary; treat both as a range,
and compare routes only within one session. §5.9 has the index route that
replaced it.

### 5.7 An ImageJ TIFF carries a per-frame overlay

Written with four frames and one `OvalRoi` per frame, then reopened:

```
reopened: c=1 z=1 t=4
overlay survived: true   rois = 4
roi 0 tPosition=1   roi 1 tPosition=2   roi 2 tPosition=3   roi 3 tPosition=4
pixels intact: 1000 (want 1000), bitDepth=16
```

So outlines can be attached to an **already written** stack, per frame, without
touching the pixels. `Overview.addOutlines()` already draws to an overlay and
scales ROIs with `RoiScaler` from the view's `sx`/`sy`; the burn-in happens only
in `savePng` via `flatten()`.

Also measured: R's `magick` (2.9.1, already a dependency) reads a multi-frame
TIFF and indexes single frames — 5 written, 5 read back, `b[3]` addressable. So
moving the overview to TIFF is *possible* on the R side. Rejected for now in
§6.14.

### 5.8 What a parallel run would cost, and why splitting the sheet is better

`runEach()` iterates **sheet rows in sheet order**; a row's identity is
`series_id` and its address is `(path, series_index)`.

It cannot be threaded as written, for five reasons, and four of them are shared
global state: the **single-entry reader cache** (one thread's `closeReader()`
can close a reader another is mid-read on, and Bio-Formats readers are not
thread-safe), `IJ.run("Set Measurements...")`, `Prefs.blackBackground` before
every watershed, and the saved/restored `bioformats.windowless` preference. The
fifth is `summary <<`, which would also lose sheet order.

⚠️ Memory is the real ceiling regardless: one assembled Luxendo series is
**981 MB** and a tile merge is 5 GB against ~9 GB usable heap, and "nothing
holds two whole copies" is a standing decision — so N threads means N copies.

🔒 **The answer is to split the series table into chunks and submit parallel
jobs**, each its own JVM with its own ImageJ globals. Two things to handle when
that arrives:

- ⚠️ the **duplicate-`series_id` refusal becomes per-chunk**, so a collision split
  across two chunks escapes both checks and the jobs overwrite each other's
  output. It must be checked before the split, not inside each job.
- `batch_summary.tsv` arrives in N pieces; they concatenate cleanly **only if
  every chunk was given the same `extraCols`**, which the rectangularity
  contract guarantees.

Where threading *would* pay is the I/O, which is latency-bound and touches no
ImageJ state: assembling row *n+1*'s channels while row *n* is segmented. One
image's extra memory, summary order preserved. Not needed yet.

Since a series became a whole position (§3.1), chunking by series gives at
most one job per position — 14 for the real acquisition — and the duplicate-id
check before the split (H9) is unchanged.

### 5.9 `bdv.h5` + `bdv.xml` are a complete, cheap file list

The facts — what the index holds and does not, `<tile>` ≠ stack, written at the
end of the acquisition, the cost of each step — live in
`note/luxendo_file_format.md` §7, which is that format's note. The headline:
`Make_LuxendoSheets` on the 4032-file acquisition over samba in **77–79 s** via
the index against **175–231 s** via the walk, with byte-identical tables; and of
those 77 s, **51–89 s** is one `length()` per file (option A, §4 `luxendo` Part 3).

### 5.10 A series does not fit in memory, and a histogram does

One Luxendo (position, time point) is ~981 MB; a 96-frame position is therefore
~94 GB of pixels (`file_size` on its series row: 94,248,045,152 bytes for the
largest), against ~8.9–9.6 GB usable heap on the 16 GB machine. Not a TIFF
limit and not fixable by format — the reason the batch must stream frames
(`time_axis`). A whole-series *threshold* needs no such memory: a 16-bit
histogram is 65,536 counters, so it is a second pass over the data, not a
second copy of it.

---

## 6. Alternatives considered and rejected

Recorded so they are not re-proposed, and so a reversal is a decision rather
than a drift.

### 6.1 One file per position, all timepoints inside (BigTIFF)

*Superseded in part, 2026-10-02.* The rejection below was of a **TIFF** holding a
whole position. It still stands for TIFF. But its premise — that the analysis
reads converted files — fell with §6.12, and a series is now the whole position
read from the HDF5 directly (§3.1, §6.17). The "cost accepted" paragraph —
`track_id` and `feature_id` in two scopes — no longer applies.

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
series — `Make_SeriesSheet` builds it by enumerating series. The objection that
three `series_*` names crowd one table does not survive inspection, because they
are not redundant: `series_index` and `series_name` are unique *within a file*
(which is exactly why neither can be the identity across a batch — a Leica
`Series001` recurs in every file), while `series_id` is unique across the table.

Note the underlying problem `vocab` fixes is not naming: **one column holds two
compositions today.** Batch passes the sheet's `prefix` as `basename`, so `name`
= the per-series id with no `output_prefix`; interactive falls back to
`(output_prefix ?: "") + resolveImageId(...)`
(`NucleusPipeline.groovy`, before v0.7.0). The fixture is from the interactive
path. Settled in `vocab` PR 2 by retiring `output_prefix`: one id, one stem. The docs disagree too: `CLAUDE.md`'s
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

### 6.12 Convert everything to TIFF first, then analyse

**Rejected: it doubles the dataset, permanently.** The `.lux.h5` files already
*are* the pixels and TIFF does not compress them, so making conversion a
required step means holding two copies of an 800 GB acquisition for as long as
the analysis exists. The friction between steps was the stated objection; the
storage is the decisive one. Conversion survives as a small optional step for
tuning and drag-and-drop (§4, step 2).

### 6.13 One sheet row per file, or a forked Luxendo batch runner

Two separate rejections of the same shape.

**One row per file: rejected, it breaks a cross-language invariant.** `prefix`
would stop being unique per row, and three things enforce that it is —
`BatchRunner.runEach()`'s duplicate refusal, `.cli_read_series_sheet()`'s on the
R side, and the standing decision that `prefix` is unique *by construction*. So
it does not merely make `include` confusing: **every R CLI stops loading the
sheet**, which is the whole downstream half of the repo.

**A forked `Run_NucleusSelector_Luxendo_Batch.groovy`: rejected.** Tracing
`runEach()`, the only thing Luxendo changes is *how a row becomes an
`ImagePlus`* — everything before and after (include, the duplicate refusal, the
pixel-size warning, closing on both paths, failure isolation,
`batch_summary.tsv`) is format-agnostic, and `CLAUDE.md` names that exact list
as "not worth a second copy". The library already has this seam as `open_mode`;
a third route is **adding an option to the library**, which is what the workflow
prescribes. Cost of avoiding the fork: one optional parameter, blank for
everyone not using Luxendo.

### 6.14 A TIFF option on the batch's overview parameter

**Rejected: it turns the interactive runner into a batch runner.**
`Run_NucleusSelector` handles one open image and has no concept of a group, so
giving it the gathering capability means giving it a loop. The gathered stack
also wants one display range per (position, channel), which cannot be known
until every frame has been seen — so it is not a per-row artifact at all.

Moving the overview from PNG to TIFF *generally* was also considered, and R was
verified able to read multi-frame TIFF and index frames (§5.7). Rejected for
now on three costs: `<prefix>_overview_ch<N>.png` is byte-identical between the
nucleus path and `Run_Overview_Batch` **on purpose**, so nothing downstream need
know which runner made one; `mg_write()` pins 8-bit because magick composes in
16 bits and a title band once promoted a whole montage; and a 16-bit TIFF
carries no display range, so R would inherit a decision Fiji currently makes and
records in `display_range`. Frame selection solves the file-count problem
without any of it.

### 6.15 Gathering overview PNGs, or holding overviews in memory

**Writing every overview PNG then gathering them: rejected, the contrast is
already baked in.** `prepare()` with `contrast=auto` fits `lo`/`hi` per image, so
a stack of per-frame PNGs shows a cell brightening because the stretch moved.
Over a time course that artifact looks like biology, which is worse than no
picture.

**Holding reduced overviews in memory until a group completes: rejected.**
`prepare()` keeps 16-bit (the 8-bit conversion is in `savePng`), so one
position's worth is **1.15 GB** at 1000 px wide or **302 MB** at 512, competing
with a ~1 GB working image. Worse, it would require the series table to be
**sorted by position** for correctness — and that table is one a person is
invited to edit and re-sort, so the requirement fails silently into several
partial files. A second pass over finished outputs needs neither the memory nor
the ordering.

### 6.16 Parsing the directory names for identity

**Rejected, though it works.** Checked on 404 spread files: the pattern
`stack_<n>-<name>_channel_<c>-<name>_obj_<obj>` plus the filename's timepoint
suffix agrees with the metadata in every case, and all 42 directories match.

It is the wrong lever for two reasons. The path **cannot supply `size_x`,
`size_y`, `size_z` or the voxel size**, and the assembler needs all four — to
build the processor, to predict bytes, to write the calibration. A path-only
manifest is blank in exactly the columns that do the work, so the
predict-before-writing warning stops working, which is the reason the manifest
step exists. And it saves nothing: reading one sidecar JSON per directory costs
**~3 s** against **~16 s** for the directory listing it needs anyway. Identity
stays content-derived for free.

### 6.17 Dropping "one `series_id` per timepoint" for Luxendo — ADOPTED, 2026-10-02

*Rejected at first, then reversed.* The original rejection, kept for the
record, weighed three costs against "collapsing the two identity scopes into
one, which is genuinely tidier":

- **failure isolation drops from timepoint to position** — a transient read at
  t=73 would cost all 96;
- **resumability goes with it** — a partly-done position has no clean marker;
- **it re-scopes `feature_id`** to `(series_id, t)`.

Why it was reversed:

- **The premise was conversion.** One series per time point was chosen while
  the plan still expected TIFFs, where a position does not fit the classic
  limit. Once the analysis reads the HDF5 directly (§6.12) that reason is gone,
  and a Luxendo stack is one series in Bio-Formats' own sense (§3.1).
- **The third cost was wrong.** §3.1 already defined `feature_id` as numbered
  across every time point within a series, which is exactly this design;
  nothing re-scopes.
- **The first two are real, and are met inside the row**: the unit of work
  becomes the frame, with a summary row per `(series_id, t)` (`time_axis`).
  They were never avoided by splitting rows either — a row was already three
  files, and the work is per frame whichever way rows are cut; the batch has
  to be loud about it either way.
- **What it buys:** `series_index` honest with no redefinition and H10 gone;
  no `position_id`, no series-table `t`, one identity scope; Luxendo takes the
  same multi-frame path a LIF time-lapse does, so real data exercises it.

**What it costs, accepted:** no Luxendo analysis until `time_axis` lands, and
`time_axis` must stream (§5.10). `include` can no longer exclude a single bad
time point, since there is no row for it — a frame selection has to do that.

### 6.18 A running counter for Luxendo's `series_index`

**Rejected: it trades uniqueness for instability.** It would make
`(path, series_index)` unique again, which is tempting because that pair is
`mergeKey()`. But a counter is derived from the collection, and a Luxendo
acquisition grows while it is being worked on — so every prefix after the
inserted timepoints changes on a rescan, orphaning results folders and pointing
`mergeKey` at the wrong rows. `CLAUDE.md` already rejected the same shape for
the series id's index padding, and the comment on `SeriesSheet.INDEX_FORMAT` records what it cost last time.
Full reasoning in §3.2b.

### 6.19 `stack_id`, `position_id` or `image_id` instead of `series`

**Rejected; the word was right, splitting the series by `t` was the mistake.**

- `stack` collides with ImageJ and with this repo, where a stack is the planes
  of one image: `nucleus_stack_histogram` — a `_config.txt` key — means pooled
  over z. "Per-stack versus per-series threshold" is exactly the `time_axis`
  decision, and naming the row a stack would make it ambiguous where it
  matters. It is also Luxendo's word, not a general one.
- `position` implies a stage position, which is wrong for a LIF tile scan
  (a merged tile image spans many).
- `image_id` was rejected in §6.3, and Luxendo's own JSON has an `image_id`
  meaning a per-*file* UUID — one channel at one time point.

### 6.20 `position_id` and a series-table `t`

**Dropped with §6.17.** Planned as `seeded` columns so a position could span
rows; with the series being the position, `position_id` would always equal
`series_id`, and `t` belongs to the output tables. The design work that went
into making them seeded rather than machine (`.cli_read_series_sheet()` drops
machine columns) is moot rather than wrong — bring it back if a format ever
splits one field of view across rows.

### 6.21 An 8-bit overview TIFF rendered by the batch

**Rejected: the batch streams frames, so it would stretch each one on its own**
— H8, a cell brightening because the range moved. The series-wide range is not
known until every frame has been seen. Hence data (16-bit projection) from the
batch and pictures from a render step (§4 `QoL`).

### 6.22 Skipping the per-file stat on the index route (option B)

**Deferred, not rejected.** ~3–6 s instead of ~1 minute for 4032 files over
samba, at the price of `source_bytes`, the proof each file is there, and the
size check (§4 `luxendo` Part 3). A minute is cheap beside a run that reads
800 GB. Revisit if the stat ever dominates.

### 6.23 Taking the stack number from `bdv.xml`'s `<tile>`

**Rejected: it is the setup's ordinal in text order of the stack** — 0, 1, 10,
11, 12, 13, 2, … — and equals the stack for 6 of 42 setups.

---

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

**H5 — `feature_stats.r` silently keeps only the first timepoint.** The
`time_axis` milestone.

**H6 — a failed ROI-zip write leaves a partial file.** Verified: 189 bytes on
disk after the exception, which `loadRoiZip` would read. The `time_axis`
milestone.

**H7 — the two tables can disagree.** The series and sources tables join on
`(alias, series_index)` — both machine-owned, so editing a `series_id` cannot
orphan its sources — and a person edits the series table between the scan and
the run. Regeneration matches rows and refuses a renumbering rather than
re-propagating a seeded column onto the wrong image; a join matching nothing
is a **loud error**, not an empty run. Resolved in `luxendo` and `vocab`
(`LuxendoScan.withSeriesId()`).

**H8 — an auto display range fitted per frame makes a time course lie.** §6.15,
§6.21. One range per (series, channel), or a moving stretch reads as changing
signal. The `QoL` milestone.

**H9 — a per-chunk duplicate-`series_id` check misses a cross-chunk collision.**
§5.8. Splitting the series table for parallel jobs puts each half's refusal in a
different JVM, and two jobs then overwrite each other's output. Check before the
split. Whenever HPC arrives.

**H10 — `(path, series_index)` not unique for Luxendo.** *Resolved 2026-10-02*
by one series per position (§3.2b). Survives only in tables made with
`gatherFrames` off, where `mergeKey` collapses 56 rows to 14 — do not build a
rescan that preserves edits on such a table.

**H11 — `bdv.xml`'s `<tile>` looks like the stack number and is not.** §6.23.
Equal for 6 of 42 setups. Nothing reads it; a future reader of the index must
not start.

**H12 — the index can be stale.** Written at the end of an acquisition, so a
crashed run may have none or an incomplete one, and a file it does not list is
invisible to `listing=index`. A listed file that is missing, or that disagrees
with its sidecar, stops the scan; an *unlisted* file is caught only by
`listing=walk`, which compares the two. The `luxendo` milestone.

---

## 9. Open questions

1. ❓ **Does `z` take part in the TrackMate distance?** Calibrated units are
   locked (§5.4). Needs a real dataset; try full 3D alongside `z = 0` (§4
   `tracking`), and **ask rather than pick**. (`tracking`)
2. ~~Resume granularity~~ — **settled 2026-10-02: per frame**, and its own PR
   (§4 `time_axis` PR 4).
3. ❓ **Default `nucleus_threshold_scope`** — `frame` or `series`; compare on
   real data first (§4 `time_axis`). (`time_axis`)
4. ❓ **Where `QoL` sits** — not before `time_axis`; decide once its absence has
   been felt. (§4 `QoL`)

Settled on 2026-10-02 and recorded where they apply: a series is the whole
position (§3.1, §6.17); the word stays `series` (§6.19); `series_index` and
`alias` keep their names with new definitions, and `alias` stays required
(§3.2); one series sheet for every format (§3.2); no `position_id` (§6.20); the
overview splits into data and render (§4 `time_axis`, `QoL`); the threshold
scope is a parameter (§4 `time_axis`); the index file list with option A
(§4 `luxendo` Part 3); `series_id` stays editable, with the sources keyed on
`(alias, series_index)` (§3.2); `gatherFrames` removed and `feature_id`'s format moved
to `time_axis` (§4 `vocab`). The container class name is 🔒 **`image_region`** (§6.11).
