# Data formats

Every table this repo reads or writes, in one place. **This file is the
authoritative description of the shapes**; `CLAUDE.md` explains why some of them
are dangerous to change, and `README.md` covers the subset a user needs to start
a project. When a format changes, this file changes with it — the doc-sync step
in `CLAUDE.md` exists to check that.

Column names below are exact, including case.

---

## 1. The sheets

`files.tsv` (one row per **file**) → `Make_SeriesSheet.groovy` → `series.tsv`
(one row per **series**) → the batch runner, and the R CLIs' `--series_sheet`.

`series.tsv` is the **series table** the R side reads. It can be written by hand
with `series_id` plus your metadata, or generated, with machine-read columns
alongside. Luxendo acquisitions get the same table from `Make_LuxendoSheets`.

**A series** is one 5D image — x, y, z, c and t — in Bio-Formats' sense, for
every format: one series of a `.lif`, or one Luxendo *stack* with all its time
points. Not to be confused with an ImageJ *stack*, which is the planes of one
image (`nucleus_stack_histogram` pools over z).

⚠️ **Renamed in v0.7.0:** `samples.tsv` → `series.tsv`, and its id column
`prefix` → `series_id`. An old sheet is refused with a message naming the
version, on both sides, rather than read with every id blank.
`Make_SeriesSheet` pointed at an old sheet renames the column in place, keeping
your edits.

The column list both are checked against is
[`schema/sheet_columns.tsv`](../schema/sheet_columns.tsv) — internal, read by
both languages, and the reason there is no second copy to drift. Its first
column says which sheet a row describes — `files`, `series`, and `sources`
for Luxendo's `sources.tsv` at the end of this section — so one file declares
them all.

### Who owns a column

| owner | on regeneration | examples |
|---|---|---|
| `machine` | **overwritten** — facts about the file | `series_index`, `series_name`, `size_*`, `pixel_*`, `file_size` |
| `seeded` | **written once, then yours** | `series_id`, `include`, inherited `condition`/`genotype` |
| `user` | **never touched** | anything you add |

A seeded value does **not** re-propagate when `files.tsv` changes — predictable
beats clever, and a fix there must not silently rewrite rows you have edited.
`reseed` (space-separated column names) forces it; `reseedAll` covers every
seeded column and therefore rewrites `series_id` from `alias`. Every reseed
reports what it changed.

Rows are matched across regenerations on **`path` + `series_index`**, never on
`series_id` — you may edit that, and editing `alias` rewrites it. A match whose
`series_name` has changed is a **renumbering** — the file re-exported in another
series order — and the merge stops, writing nothing, rather than carry your
metadata onto the wrong image.

### `files.tsv` — you write this

| Column | Required | Meaning |
|---|---|---|
| `path` | **yes** | image file; absolute, or relative to the image root given at run time |
| `alias` | no | short handle for the container the series index counts within — here, this file. Builds every `series_id` from it; unique across the table. Defaults to the basename without its extension |
| `include` | no | seeds the `include` of every series read from this file. Default true |
| anything else | no | seeded onto every series of that file — typed once instead of per series |

Template: [`config/files_template.tsv`](../config/files_template.tsv). Scan mode
writes a skeleton for you.

### `series.tsv` — generated, then yours to edit

`series_id` is generated as `sanitise(<alias>_s<NNNN>_<series_name>)` — the
series index, zero-padded to four digits — and the invariant everything rests
on is:

> **the `series_id` column == the output filename stem == the `name` column
> of the outline table.**

Since v0.7.0 nothing is put in front of it (`output_prefix` is retired), so
that holds for an interactive run as well as a batch — §2.

**You may edit it**, for readability; it must stay unique. Results already
written carry the old id, so edit before a run, or re-run after.

That is why the *sanitised* value is written into the sheet, not the raw one: a
sheet saying `my run` while the disk says `my_run` fails the R join with nothing
visibly wrong.

**The index is always present, not added only where names collide.** Series
names repeat freely — a tile scan is many series under one name, and `Series001`
is a Leica default — so with `alias` unique across files and the index unique
within one, the id is unique *by construction*. Padding is a fixed four
digits and widens past 9999 rather than being derived from the file's series
count, which would re-pad every id in a file that grew from 999 series to
1001.

A duplicate `series_id` can therefore only be introduced **by editing the
column**. `Make_SeriesSheet` writes the sheet anyway and then fails, so the table
can be opened and corrected — the error message must not be the only artifact of
the run. `allowDuplicateId` downgrades that to a warning. The batch refuses
outright, for included rows, because there two rows sharing an id overwrite
each other's output files; `.cli_read_series_sheet()` on the R side refuses too.

Column **order** is presentation only — every reader on both sides works by
column name, and the merge matches on `path` + `series_index` — so the sheet is
written in the order a person reads it: `series_id`, `include`, then your own
metadata columns, then the file's facts. Rearranging it breaks nothing.

Machine columns are `alias`, `path`, `series_index`, `series_name`, `size_x`,
`size_y`, `size_z`, `size_c`, `size_t`, `pixel_type`, `pixel_width`,
`pixel_height`, `pixel_depth`, `pixel_unit` and `file_size`. `pixel_depth` is
**blank for a single plane** — there is no z axis to measure, and Bio-Formats
reports none. `file_size` with the basename is the fingerprint that spots a
copied or replaced file, and what `skipExplored` compares against before
deciding a file is unchanged.

**Passing the same sheet to both stages is the ordinary case.**
`annotate_features_cli.r` binds the sheet's metadata onto every feature row, so
a column such as `genotype` is already in the `_features.rds` by the time
`feature_stat_cli.r` sees it. That step therefore **reconciles rather than
re-joins**: a shared column whose values agree per series is skipped (a
`left_join` would produce `genotype.x`/`genotype.y`, after which `--group_by
genotype` resolves to neither), and one whose values *disagree* is a hard error
— the features were annotated from a different sheet, and silently preferring
either copy would attach the wrong metadata to real numbers.

**The R side reads this sheet too, and reads it the same way.** `include` is
honoured by `.cli_read_series_sheet()` with exactly the vocabulary
`BatchRunner.isIncluded()` accepts (`true/yes/1`, `false/no/0`, blank or absent
= included); a word neither side agrees on is an error rather than a guess. It
is applied **before** the duplicate-id check, so setting `include=false` on
all but one of a colliding pair works on both ends — which is what the Groovy
error tells you to do. The column is then dropped: it says whether to analyse a
row, and would otherwise land on every output row as a column that is `TRUE`
everywhere by construction.

The **machine columns are dropped too**, read from `schema/sheet_columns.tsv`
rather than listed again in R. They are facts about the image file that
`Make_SeriesSheet` rewrites on every regeneration, and a generated sheet carries
sixteen of them — joined through, they would put `size_x` and `file_size` on
every feature row as though somebody had typed them as metadata. The run reports
which were dropped. A CLI copied out of the repo finds no schema, keeps
everything, and still runs.

⚠️ A metadata column may not be named after one the CLIs write themselves —
`roi`, `z`, `area`, `is_bridge`, `geometry`, `series_id`, `run_id`, `feature_id`,
`feature_type`, `parent_*`, `feature_class`, `n_detected`, `n_invalid`,
`n_failed`, `n_roi`. The sheet is rejected with the offending name rather than
the column being silently renamed to `area...7`. (The `--id_column` itself is
exempt: it is the key, not metadata.)

Template: [`config/series_template.tsv`](../config/series_template.tsv).

### What is checked, and how hard

| check | severity | catches |
|---|---|---|
| `path` unique | **error** | the same file listed twice |
| `alias` unique | **error** | two files claiming one name |
| basename repeated across paths | warning | a filename reused for different data |
| basename **and** size repeated | warning | one file copied elsewhere — what `alias` cannot see |
| composed `series_id` unique | **error** | the collision the parts do not show |

The last is not implied by the others. Alias `A` with series `B_C` composes to
the same string as alias `A_B` with series `C`; and `sanitise()` collapses
whitespace, so `Image005 Denoised` and `Image005_Denoised` are one filename.
Both are checked on the sanitised composite, which is what reaches the disk.

### `include`

One column, one meaning. The file-level `include` is a **default generator**:
each series row inherits its file's value and can then be flipped individually.
There is no two-level logic at run time.

`series.tsv` stays a **complete inventory** — an excluded file still gets its
rows, marked off. A record that silently omits things is worse than one listing
them as off, the same reasoning that keeps an orphan feature rather than
dropping it.

### Behaviour when a sheet is given to the R CLIs

- rows naming a `series_id` with no matching file → **warning**, listed
- files whose series id is not in the sheet → dropped, reported as a message
- no overlap at all → **error** (a silent empty run is the failure mode this
  repo is built to avoid)

Accepted extensions: `.tsv` / `.txt` (tab-delimited), `.csv`, `.xlsx` / `.xls`.

### The Luxendo pair — `series.tsv` and `sources.tsv`

`Make_LuxendoSheets.groovy` writes **two** tables, because Luxendo breaks the
assumption every other format here satisfies.

> Every format this repo read before Luxendo put one or more **series inside one
> file**. Luxendo puts **one series across several files** — one per channel, one
> per time point.

A series row cannot absorb that: it would need more than one `path`. So the file
facts live in a second table, which is not a new pattern — `files.tsv` →
`series.tsv` is already a file table and a series table, and this is the same
pair with the cardinality reversed.

| | rows | sheet in `schema/sheet_columns.tsv` |
|---|---|---|
| `series.tsv` | one per series — one per **stack**, every time point inside | `series` — **the same table every format writes** |
| `sources.tsv` | one per `.lux.h5` | `sources` |

`series.tsv` being the ordinary series table is the point: the R CLIs read it
unchanged, and it is where `include` lives and where you add your own columns.

**One series per stack, always.** v0.6.0 split a stack into one series per time
point by default (`gatherFrames`); that option was removed in v0.7.0, and asking
for it is refused. To take out one time point, use `Make_LuxendoTiff`'s
`frames`.

#### `series.tsv` — four columns, read the Luxendo way

`path`, `series_index`, `series_name` and `alias` are **required**, and each is
defined so that it is true for every format, Luxendo included:

| column | Luxendo meaning |
|---|---|
| `path` | ⚠️ the **acquisition directory** — the root `source_path` resolves against, not a file |
| `series_index` | the `stack` number — the index the container addresses a series by |
| `series_name` | `stack_description`, before sanitising |
| `alias` | a short handle for the acquisition, **the operator's**, defaulting to the folder name |

Everything else follows from the sources: `size_c` is the channel count,
`size_t` the number of time points, `file_size` the sum of the sources,
`pixel_type` `uint16`.

⚠️ **`series_id` carries the alias, and it has to.** It is generated as
`sanitise(<alias>_s<NNNN>_<stack_description>)` — the repo's own
`composeSeriesId()`, not a second copy of the rule.
`s<NNNN>_<stack_description>` is unique only *within* one acquisition:
measured on two real acquisitions, **all 14 stack identities were identical**
(`stack_0-L26A pos1` in both), so without the alias both runs produce the same
ids, and two runs landing in one output directory would silently overwrite each
other's `_outline.txt`, `_res.txt` and `_config.txt`. With the alias, 0 collide.

The alias is a **parameter** on `Make_LuxendoSheets`, blank meaning the folder
name, exactly as `files.tsv`'s alias defaults to the basename. The point of the
column is that a run need not be named after whatever the camera called the
directory.

#### `sources.tsv` — one row per file

| Column | Required | Meaning |
|---|---|---|
| `source_path` | **yes** | the `.lux.h5`, relative to the acquisition directory in the series row's `path` |
| `alias` | **yes** | the acquisition this source belongs to |
| `series_index` | **yes** | the stack. With `alias`, the join to `series.tsv` — rows sharing the pair are one series: its channels and its time points |
| `channel` | **yes** | 0-based channel index **in the output** |
| `channel_name` | no | `channel_description`; blank when unnamed. Here and not on the series table because it is per channel |
| `t` | **yes** | 0-based time point within the series |
| `size_x`, `size_y`, `size_z` | **yes** | pixels |
| `pixel_width`, `pixel_height`, `pixel_depth` | no | physical sizes; `pixel_depth` **blank for a single plane** |
| `pixel_unit` | no | unit of those three |
| `source_bytes` | **yes** | with the basename, the fingerprint that spots a replaced source |

⚠️ **The join is `(alias, series_index)`, never `series_id`.** You may edit
`series_id` for readability; a join on it would orphan every source row the
moment you did. Both halves of the key are machine columns on the series table,
so no edit can break the join, and two acquisitions in one table cannot collide
on a stack number. One implementation, `LuxendoScan.withSeriesId()`, is what
every consumer uses; a source with no series row stops the run, a series with
no sources is reported.

**There is no `target_output_path`.** It was only ever `series_id + ".tif"`, so
the output name is **derived** from the series row's `series_id` (as it stands,
so an edited id names the file), plus `_t<TTTT>` when one time point is taken
out, `_downscale<PC>pc`, and the format's extension. A stored copy of a derived
value is a second thing to keep in step.

**There is no `include` here either.** It is a property of the series, so it
lives on `series.tsv` — one value, which makes "the channels of this output
disagree about `include`" a state that cannot be written down rather than one
that has to be handled.

#### Where the file list comes from — `listing`

| `listing` | file list | |
|---|---|---|
| `auto` (default) | the index when `bdv.h5` **and** `bdv.xml` are both in the acquisition directory, else the walk | |
| `index` | the external links in `bdv.h5`; refuses if there is no index | no directory walk |
| `walk` | every directory under the acquisition, pairing `.lux.h5` with `.json` | the v0.6.0 route |

The scan logs which it took. On the 800 GB, 4032-file acquisition over samba the
index route took **77–79 s** end to end against **175–231 s** for the walk, and
the tables were byte-identical. `note/luxendo_file_format.md` §7 has the
measurements and what the index does and does not hold.

On the index route **every listed file is stat-ed** — for `source_bytes`, to prove
it is on disk, and for the size check below — and a file the index lists that is
missing stops the scan. Every placed file is then checked against the index:
time point, channel, stack (read from the setup *name*, `st:N`, never from
`<tile>`), size and voxel size. A disagreement stops the scan.

⚠️ **The index is written at the end of an acquisition**, so a run that crashed
may have none or a stale one, and a file the index does not list is invisible to
the index route. `listing=walk` is the check: with an index present it compares
the two file sets and warns on any difference.

#### Identity comes from the `.json` sidecar

Luxendo writes `Cam_long_00000.json` beside every `Cam_long_00000.lux.h5`, and
it holds the same `processingInformation` as the image's own `/metadata` **plus**
the dimensions and voxel size. Verified on 25 spread files: it agrees with the
HDF5 and with the real dimensions in 25 of 25.

**On the walk, a row needs both halves.** A `.lux.h5` with no sidecar is skipped
and reported; a `.json` with no image is ignored. That pairing *is* the
index-file test — `main_raw.lux.h5` is link-only and has no sidecar — so nothing
has to be opened to recognise it. The index route never sees `main_raw.lux.h5`,
and with `quickScan` it reads only the sampled sidecars, so an unsampled file's
`.json` is not checked for there.

⚠️ **`quickScan` (default on) reads one sidecar per *directory*.** The
dimensions and voxel size are constant within a channel directory, and the one
fact that is not — the **time point**, which is the axis the directory runs
along — comes from the filename suffix, *after* the mapping has been confirmed
against the sampled file's real `time_point`. A directory whose names disagree
with its sidecar is read in full and says so. Every file's size is also compared
with its siblings', and anything more than half a z-plane away is read in full:
sizes within a directory vary by **8 bytes** where one plane is 8,388,608, so a
truncated time point cannot hide. On the real acquisition the sampled and full
scans produce **byte-identical** tables.

#### What the conversion writes

```
gather_summary.tsv        one row per OUTPUT: status, reason, bytes, checksum, verified
<output>_gather.txt       per output: its sources, checksum and VERSION
```

`gather_summary.tsv` is **rectangular whatever happened** — same contract as
`batch_summary.tsv` — with `output_path, series_id, t, frames, channels, format,
scale_percent, status, reason, bytes, checksum, verified`. A `status` of
`written` / `skipped` / `failed` with a `reason`, so a half-completed run is
legible. `frames` is the number of time points in that output.

**`frames` on `Make_LuxendoTiff` chooses time points** — `0`, `0,47,95`, `0-3`.
Each chosen time point of a multi-frame series becomes **its own file**, named
`<series_id>_t<TTTT>`, which is the same name and the same pixels the
per-time-point layout gives that frame. A series holding one frame keeps its own
name. Blank writes every frame of a series into one file, as before — which for
a 96-frame position is ~94 GB, so it fails as a row: over the classic TIFF limit
for `tiff`, and over 75% of the heap for either format, both decided from the
tables before anything is read.

`checksum` is a CRC32 over the pixels **as written** — after any downscale, so
it cannot pass a scaling bug off as a correct downscale — and `verified=yes`
means the file was read back off disk and its pixels recomputed to the same
value. ⚠️ **It verifies the writer, not the reader.** Both sides come from the
same read, so a plane fetched wrongly would match itself. What it catches is the
round trip: a truncated write, or channels, slices and frames transposed.

`_gather.txt` names **every** source: one `channel_<c>_source` line per channel,
or `t<N>_channel_<c>_source` when the output gathered several frames.

⚠️ **The TIFFs are not a required step.** The batch runner is to read the
`.lux.h5` through the same two tables and assemble channels in memory, so it
never reads them. (That route — one resolver shared by the batch,
`Make_LuxendoTiff` and the overview — is planned, not built: today the batch
opens files only.) Converting is for tuning a threshold interactively and for drag-and-drop.
The sources *are* the pixels and TIFF does not compress them, so a mandatory
conversion would mean holding two copies of the acquisition.

---

## 2. Fiji output — `scripts/groovy/` writes this

Per feature, per image. Every file is named after the image's **series id** —
the batch takes it from the series table, an interactive run from its `Series
id` field or, left blank, from the image title (see below).

```
<series_id>_<feature>_outline.txt        polygon vertices
<series_id>_<feature>_outline_ROIs.zip   ImageJ ROIs
<series_id>_<feature>_res.txt            measurements
<series_id>_config.txt                   every parameter used
<series_id>_overview_ch<c>.png           quick-look projection      (optional)
<series_id>_overview_ch<c>_overlay.png   the same, outlines drawn   (optional)
```

⚠️ **The file stem and the outline tables' `name` column are the same string,
and the R side depends on it**: it reads the series id out of `name`, then
finds `<series_id>_config.txt` and `<series_id>_<feature>_res.txt` by that
name. Until v0.7.0 an interactive run put `output_prefix` in front of both;
that parameter is retired rather than moved out of `name` alone, because a
prefix in the file names but not in `name` would hide both files from R — no
`volume`, no signal columns, a warning and not an error.

`<feature>` is `nucleus` or `nucleolus` today.

### Where the R side gets the series id and the feature

**From the file's content, not its name.** Both identities are already in the
table:

| Identity | Comes from | Example |
|---|---|---|
| series id | the `name` column | `GRV_Position010` |
| feature | the `roi` column's prefix | `nucleus_0001-0001-0433` → `nucleus` |

The filename only has to get the right files onto the list; `--input`'s glob and
`--input_pattern` **select**, they no longer **parse**. This is what lets a
feature name contain an underscore (`growing_oocyte`), and it means renaming a
file cannot silently change what the R side thinks is in it.

⚠️ The ROI id's tail is fixed-shape and anchored (`_SSSS-NNNN-YYYY`), which is
why a greedy prefix is correct when reading the feature *out of an ROI id* and
was wrong when reading it out of a *filename* — a filename has no such anchor,
so `S1_growing_oocyte_outline.txt` parsed as series `S1_growing`, feature
`oocyte`. Fixed, and pinned in `test-feature_identity.R`.

Two rules that are errors, not warnings, because a silent pick would file half a
table under the wrong name:

- one file may hold **exactly one** feature type
- one file may hold **exactly one** series id

`.cli_parse_contract()` remains as a fallback for tables written without a
`name` column, and a run says how many files it had to fall back for.

### The series id of an interactive run

`Run_NucleusSelector.groovy` and `Run_Overview.groovy` have one field for it,
`Series id`, **blank by default and never remembered**: it names one image, so
inheriting it would put the next image's output under this one's name.

| `Series id` | the id used |
|---|---|
| blank | the image title, less the `<file>.lif - ` part Bio-Formats puts before a series name, through `RoiExport.sanitize()` — `260909_IHC.lif - Image005 Denoised` gives `Image005_Denoised`. Logged with the raw title |
| typed | exactly what was typed — **refused, not rewritten**, if `sanitize()` would change it (whitespace, `/ \ : * ? " < > \|`, a trailing image extension), with the accepted form in the message |

The batch checks a hand-edited sheet id the same way, failing that row only.
It **never contains whitespace**: the id is written into the `name` column of
the tab-separated outline table as well as into filenames.

⚠️ **Retired in v0.7.0: `output_prefix` and `position_pattern`.** The first
became the typed id (or another output directory, to keep runs apart); the
second searched the slice label for a token such as `Position`, a guess at
where one acquisition software put the name. A `_config.txt` carrying either
still reads — the key is skipped and the batch logs that it was
(`RunConfig.RETIRED_KEYS`); an unknown key is still an error. A run of the
fixture image without a typed id now gets the TIFF's title,
`20241216_dkD_DAPI_EGFP_Klf5_Nr5a2_forrep3.lif-Position010`; its tracked output
is reproduced byte for byte by typing `GRV_Position010`.

### Overview PNGs — quick-look only, never an input to a measurement

Written by `Run_NucleusSelector.groovy` (when "Save overview PNG" is on) and by
`Run_Overview.groovy`. The name is decided in one place,
`Overview.overviewPath(dir, basename, channel, suffix)`:

```
<series_id>_overview_ch<c><suffix>.png
```

| Suffix | Contents |
|---|---|
| *(empty)* | the bare z-projection |
| `_overlay` | the same projection with detected outlines drawn on |

**The suffix is what makes the two coexist.** They shared one name until now, so
writing either destroyed the other — and `montage_qc_cli.r` needs both at once,
as `--projection` and `--overlay`.

`Run_NucleusSelector` writes both, for the DNA channel **plus every channel in
`channels_measured`** — the outlines come from DNA, so drawing them over the
other channels is how a signal is checked against its compartment. Which files
exist is recorded in `_config.txt` as `overview_channels` and
`overview_overlay_suffix`. `Run_Overview` derives its suffix from whether it
actually drew anything, and takes an override.

**The projection, the output size and the contrast are parameters**, since
0.3.0 — `overview_method`, `overview_width`, `overview_height`,
`overview_contrast` and `overview_saturated`, all recorded in `_config.txt`
whether or not a PNG was written. They were fixed at `max` and 500 px, with
`Run_Overview.groovy` as the way to vary them; that is no good for a merged
tile scan, where 500 px of a 20000 px mosaic diagnoses nothing and re-running a
second script over a whole batch is not a quick visual check.

`overview_width`/`overview_height` take **0 for "the original size"**, and one
given makes the other follow the aspect ratio, so one number handles any tile
geometry. ⚠️ On a large batch that is a lot of disk: six PNGs per series, at
the full frame.

⚠️ `overview_contrast = auto` **stretches each picture to full range on its
own**, so two projections of genuinely different data can render to identical
bytes. Measured on a synthetic stack: a flat disc at 220 (`max`) and the same
disc at 100 (`min`) both save as 0–255 and the files are byte-identical. The
display range each PNG settled on is logged for exactly this reason. Use
`contrast = none` when the pictures are meant to be compared to each other.

⚠️ Outlines are drawn in `merged` mode: ROIs are unioned **in the 2D
projection**, so two objects overlapping in x-y share one outline however far
apart they are in z. It is a picture, not a count — that is the whole reason
the montage puts it beside R's z-aware union.

⚠️ Contrast is `auto`, which stretches whatever is present. **A channel holding
only noise gets that noise stretched to full brightness and saves a convincing
picture of nothing.** Both runners log the display range per channel for this
reason (`display 10.0-19.0` is noise; `display 10.0-240.0` is signal). The one
case the tell misses is a *perfectly* constant channel, where the histogram is
degenerate and ImageJ falls back to the type's full range — pinned in
`Test_Overview`.

### `_outline.txt` — one row per polygon vertex

| Column | Type | Notes |
|---|---|---|
| `name` | chr | the series id, the same string as the file stem — **never contains whitespace**, see §2 |
| `roi` | chr | ROI id, `SSSS-NNNN-YYYY` |
| `z` | int | 1-based slice |
| `x`, `y` | dbl | **calibrated units (µm), not pixels** |

Vertices are in ring order and the ring is *not* closed — R repeats the first
point when building the polygon.

### `_res.txt` — Fiji's Results table, one row per ROI per channel

Written with an unnamed first column (Fiji's row numbers), which
`read_fiji_result()` drops. Columns come from `Set Measurements`, which the
Groovy scripts force explicitly to:

```
area mean standard min centroid shape integrated median stack display
```

giving `Label, Area, Mean, StdDev, Min, Max, X, Y, Circ., IntDen, Median,
RawIntDen, Ch, Slice, AR, Round, Solidity`.

**`Label` is the join key.** It packs image, ROI id, channel and slice into one
string, e.g.

```
20241216_dkD.lif-Position010-1.tif:nucleus_0001-0001-0433:c:1/4 z:1/50 - .../Position010
```

`read_fiji_result()` parses it and returns the columns lower-cased and
de-punctuated, plus four derived ones:

```
label area mean stddev min max x y circ intden median rawintden ch slice
ar round solidity filename roi pos z
```

### `_config.txt` — two columns, `parameter` and `value`

Provenance, read long after the run. **Eleven non-test files across both
languages touch it**, which is most of the reason a field name is expensive to
change — renaming one from the Groovy side would not naturally lead anyone to
grep the R CLIs:

| | |
|---|---|
| **writes** | `NucleusPipeline` (via `RoiExport.saveRunConfig()`) — the only writer |
| **reads as parameters** | `RunConfig.readParams()`, called by `Run_NucleusSelector_Batch.groovy`. Unknown key ⇒ error; absent key ⇒ the default |
| **reads individual fields** | `montage_qc_cli.r` (`image_width`/`image_height`, `pixel_width`/`pixel_height`) · `feature_stat_cli.r` (`pixel_depth` ⇒ `--z_step`) · `cli_helpers.r` (`.cli_read_config()`) |
| **schema** | `NucleusPipeline.PARAM_TYPES` (parameters) + `RunConfig.PROVENANCE_KEYS` (everything else) |
| **template** | `config/nucleus_config_template.txt`, generated from `NucleusPipeline.DEFAULTS` |

Unlike the series table, there is **no run-time schema file** both languages
read — `schema/sheet_columns.tsv` has no counterpart here. The R readers above
therefore know these field names by hard-coded string, and nothing checks them
against the writer. Changing a field name means grepping this table.

Fields that other code depends on:

| Field | Used by |
|---|---|
| `image_width`, `image_height` | **pixels.** `montage_qc_cli.r`, to draw its panel over the same frame as the Fiji PNG |
| `pixel_width`, `pixel_height`, `pixel_unit` | the same, to convert that frame to µm |
| `pixel_depth` | the z step, in `pixel_unit`. **Blank when the image is a single plane** — ImageJ defaults the calibration to 1.0 with no z axis and Bio-Formats reports no physical size, so a written 1.0 would be a plausible number for a distance that does not exist. Written since 0.2.0, and `feature_stat_cli.r` now **defaults `--z_step` to it**, per series, from the config beside the inputs |
| `script` | records the repo `VERSION` that produced the directory |
| `source_file`, `series_index`, `series_name` | which series of which file produced this directory. Written by the batch, **blank in the interactive runner** where the image was already open and nothing told it. Identity comes from content, not from the filename, so the series id should not have to be parsed apart to answer this |
| `series_id` | the id this run's files are named after — the `name` column's value. **Provenance, not a parameter**: a config setting it would give every image of a batch one name. Replaced `output_basename` in v0.7.0, which an older config still carries and a reader ignores |
| `open_method` | which reader opened the image: `importer` (Bio-Formats' own) or `reader` (one held open across the file). **Blank when the image was already open**, i.e. the interactive runner, where the operator opened it however they liked. The two are asserted to produce byte-identical output, but two runs that used different ones must not be indistinguishable afterwards |
| `overview_channels`, `overview_overlay_suffix` | which overview PNGs exist, so a results folder can be read later without guessing. Blank when none were written |
| `nucleus_threshold_used` | the pixel range the threshold **selected**, as `lo-hi`; `per-slice <lo>..<hi>` (note the `..`) when `nucleus_stack_histogram` is off, since there were as many thresholds as slices and none of them is the answer. Not the algorithm's bare number: for bright objects that number is the *bottom* of the range and the top is the type's maximum, so the pair is what can be copied into a manual threshold without working out which end it was. The literal **`none`** when the frame had nothing to separate (see below) — a word, not a range, so it cannot be pasted anywhere by mistake |
| `nucleus_mask_pct` | percent of pixels the threshold selected, **before** fill holes and watershed, because the question it answers is what the threshold chose. The cheap signal that one went wrong in either direction: `0.00` selected nothing (a blank field), a number in the tens selected the frame rather than the objects in it. Neither shows up in an ROI count — the size filter turns both into "no nuclei" |
| `nucleus_circ_rejected` | how many ROIs `nucleus_circularity` deleted. **Blank when the filter was off** — a `0` would claim a filter ran and found nothing to remove. Unlike the R side's `--min_circularity`, which marks a row and leaves it in the table, this filter drops ROIs before anything is written, so this number is the only surviving evidence that they existed |

Everything else is a record of the run's parameters. A key that is absent must
be handled, not assumed: configs written before a field existed are still valid
input (`montage_qc_cli.r` warns and degrades rather than failing).

⚠️ **That tolerance is also the trap.** Because an absent key is legal, a
parameter the writer forgets silently becomes its *default* on the next run
rather than an error. `Test_NucleusPipeline` asserts every `PARAM_TYPES` key is
written, against a config that was actually produced.

There is **no exception** to that since v0.7.0. `output_prefix` was one — it
named the output rather than deciding the analysis — and it is retired (§2).
The series id is not a parameter at all and is written as provenance.

**This file can be read back in as the parameters of another run.**
`RunConfig.groovy` parses exactly the shape `RoiExport.saveRunConfig()` writes,
so "tune one image in the GUI, take its config, run the batch with it" needs no
converter. Three rules make that safe:

- an **unknown** key is an error — silently ignoring `nucleus_sigma` when the
  parameter is `nucleus_blur_sigma` is how a typo becomes a default nobody sees;
- the **provenance** fields above (`timestamp`, `image_*`, `nucleus_count`, …)
  are ignored on purpose, so a whole `_config.txt` can go back in unedited.
  `nucleus_threshold_used` is one of them, deliberately: `nucleus_threshold` is
  the *request* and reads back in, while the value it produced is a fact about
  one image. A config fed forward re-derives the threshold for the image it is
  given rather than freezing the one it came from — **pinning a threshold is a
  deliberate act**, copying the range into a manual setting, never a side
  effect of reusing a config;
- a value that will not coerce is an error naming the key. Booleans especially:
  in Groovy a non-empty string is truthy, so `"false"` read from a file would
  otherwise *enable* what it guards.

`#` comments and blank lines are allowed, and the header is optional. One value
is translated on the way back: a blank `z_spec` is written as the readable
`(all)`, and `NucleusPipeline.fromConfig()` maps it back to blank — without
that, feeding a run its own config fails on the one field nobody set.

---

### Batch output — `Run_NucleusSelector_Batch.groovy` adds two files

Per image it writes exactly what the interactive runner writes; the outline,
`_res.txt`, ROI zip and `_config.txt` are the same contract. Two extra files
describe the batch as a whole.

`batch_summary.tsv` — one row per **sheet row**, excluded ones included, so it
is a complete record of what the run did rather than of what succeeded:

| Column | Meaning |
|---|---|
| `series_id` | the series, as the sheet named it (`prefix` before v0.7.0) |
| `path`, `series_index` | which image it came from |
| `status` | `ok`, `failed`, or `excluded` |
| `open_method` | `importer` or `reader`, whichever actually opened it; blank for `excluded` rows |
| `threshold` | the pixel range the nucleus threshold **selected**, `lo-hi`, same syntax the manual threshold takes; `none` when the frame had nothing to separate. Blank when the row did not run |
| `mask_pct` | percent of pixels inside that range, measured **before** fill holes and watershed. Blank when the row did not run |
| `n_nucleus`, `n_nucleolus` | counts, blank when the row did not run |
| `seconds` | wall time for that image |
| `message` | for `failed`, the exception, flattened to one line |

A row failing does not stop the batch. On a long run this file, not the log, is
what says which images need attention.

### Overview-only batch — `Run_Overview_Batch.groovy`

Projections and nothing else: no threshold, no particles, no measurement, no
ROI files. It writes

```
<series_id>_overview_ch<N>.png
```

— the **same names** the nucleus path writes for its raw overview, and that is
load-bearing rather than a coincidence. Both go through
`Overview.overviewPath()`, and for the same projection, size and contrast
settings the two produce byte-identical PNGs (verified on the fixture, both
channels). A file from either runner is interchangeable to anything downstream,
so a montage or a QC panel need not know which one made it. No suffix parameter
is offered: the suffix exists to mark that outlines were drawn, and nothing here
draws any.

**`batch_summary.tsv` has a fixed frame and a variable middle.** Every batch
writes `series_id`, `path`, `series_index`, `status`, `open_method` first and
`seconds`, `message` last, with the same meanings as the table above; between
them sit the columns that *that* batch's work reports. Blank in those columns
means the row did not run, and every row carries every column — an `excluded`
or `failed` row is padded rather than shortened, so the table stays rectangular
whatever happened to it.

For this runner the middle is:

| Column | Meaning |
|---|---|
| `channels` | the channels projected, comma separated, in the order written |
| `png_size` | `<width>x<height>` of the PNG actually written, after any resize |
| `display_range` | per channel, `ch<N>:lo-hi` — the display window the contrast setting chose |

`display_range` is there because `auto` contrast stretches whatever it is given:
a channel holding only noise has that noise stretched to full range and saves a
convincing picture of nothing. A narrow range beside a wide one on another
channel is the tell — but only if it is written down, and opening every PNG to
find the few that went wrong is what this column exists to avoid.

### Group montage — `group_montage_cli.r` writes two things

`<prefix><group>_montage.png`, one per group, and one index describing all of
them.

`montage_index.tsv` — one row per **sheet row that was laid out**, including the
ones whose image was missing, because the montage draws those as placeholder
cells and a count taken from the picture has to be reconcilable with the table:

| Column | Meaning |
|---|---|
| `group` | the value of the `--group_by` column |
| `series_id` | the series, as the sheet named it (`prefix` before v0.7.0) |
| `image_path` | the file used, whether built from `--image_dir`/`--image_suffix` or taken from `--image_path_by` |
| `status` | `ok`, or `missing` when the image was not there |
| `row`, `col` | 1-based position in the grid, so a cell in the picture can be named |
| `um_per_px` | micrometres per output pixel for that montage; blank under `--scale pixel` |
| `cell_px_w`, `cell_px_h` | that cell's own slot, in output pixels — **per cell, not per montage**: the layout is a table, so column `j` is as wide as its widest cell and row `i` as tall as its tallest |
| `width_um`, `height_um` | the series' physical size, `size_x * pixel_width` |
| `montage` | the file this row was drawn into |

**It is designed to be read back in.** It carries `group`, `series_id` and
`image_path`, so re-running exactly what you looked at is
`--series_sheet montage_index.tsv --image_path_by image_path --group_by group`.
That is the provenance answer rather than writing resolved paths back into
`series.tsv`: the sheet is shared with the Fiji side and its columns have
declared owners in `schema/sheet_columns.tsv`, so an R CLI adding one would need
a schema entry and `Make_SeriesSheet` would have to know about it.

**Panels are scaled by physical size.** `size_x * pixel_width` is the extent;
the PNG's own dimensions are not, because Fiji has usually already resized the
overview. Two series with identical pixel dimensions can be different physical
sizes — this dataset holds 0.2227 and 0.4456 µm pixels — and drawing them equal
makes one follicle look like two different ones. A row with no usable
`pixel_width` is an error rather than a quiet fall back to pixel scaling.

`--cell_max_px` bounds a panel's **longest** side. Cells are then laid out as a
**table**: column `j` takes the width of its widest cell, row `i` the height of
its tallest, and each cell is padded top-left to its own column and row. A
column holding no cell at all — `--ncol 3` with two panels — is worth no width,
so a montage is as wide as the columns it actually fills and two montages of
different sizes no longer match. A cell whose image is **missing** takes the
slot its siblings occupy, not one predicted from the sheet: the sheet's shape
and the file's shape need not agree, and a placeholder sized from the sheet came
out three times too tall beside a real panel. Padding never carried size information: the
drawn pixels do, at a scale that is constant across the montage and stated by
the scale bar.

`--um_per_px group` (the default) fits each group's largest panel to the cell;
`run` uses one scale for every montage, so two of them can be compared with each
other. Because `group` means two montages are **not** comparable, the scale bar
is drawn by default and the value is recorded in the index and in the title.

### How the nucleus threshold is chosen

`nucleus_threshold` names a method from the Auto Threshold plugin — the full
list, `Huang2` (the default) through `Yen` — **or the literal `Manual`**.

| parameter | |
|---|---|
| `nucleus_threshold` | the method, or `Manual` |
| `nucleus_threshold_range` | `lo-hi` in **raw pixel values**, read *only* when the method is `Manual`, exactly as `nucleolus_rel_fraction` is read only for `Relative`. `Infinity` as the top end means the type's maximum |
| `nucleus_stack_histogram` | `true` (default): one threshold from the pooled histogram of every slice. `false`: one per slice |

Both ends of the manual range are applied: a pixel is object when
`lo ≤ v ≤ hi`, so an upper bound excludes saturated pixels. An auto method
supplies only the bottom end and pins the top at the type's maximum, which is
why `nucleus_threshold_used` is reported as a pair — the pair is what pastes
back into `nucleus_threshold_range` without anyone working out which end the
algorithm's number was.

⚠️ **A manual threshold is a raw pixel value, and does not travel.** It is
meaningless on a different bit depth, a different exposure, or — in a batch —
on a `.lif` whose series were acquired differently. `nucleus_mask_pct` is what
catches that: a manual value above everything present reads `0.00`.

⚠️ **`nucleus_stack_histogram = false` lets an empty slice's noise become
objects.** With nothing bimodal on that slice, the method still returns a
threshold and finds "objects" in the noise. The pooled default is the safe one;
turn it off for a stack with strong z-dependent illumination falloff, where one
threshold under-segments the deep slices.

`nucleus_threshold_range` and `nucleus_stack_histogram` are **not remembered
between runs of the interactive dialog** (`persist=false`) although every other
field is, including the method itself. The method is safe to remember precisely
because the range is not: leaving it on `Manual` means the next run starts with
a blank range and is refused before the image opens, rather than silently
reusing a pixel value from a different image. All three are still written to
`_config.txt`, so GUI-tune-then-batch is unaffected. See `CLAUDE.md`.

⚠️ **A uniform frame has no threshold, and says so.** `ignore_black` and
`ignore_white` zero the two end bins before the algorithm runs, so a frame whose
pixels are *all* pure black or all saturated leaves an empty histogram and the
Auto Threshold plugin throws. A field of blank mounting medium in a tile scan is
exactly this. It is now caught: the mask is empty, `threshold` reads `none` and
`mask_pct` reads `0.00`.

Before that it was **silent and wrong**. `IJ.run()` routes through ImageJ's
`Executer`, which catches the plugin's exception, logs it and returns — leaving
the image *unthresholded*, after which the mask step handed back the raw pixels
as if they were a mask. Measured on an all-255 frame: 1600 of 1600 pixels "on",
looking exactly like a successful run. **Results written by v0.3.0 and earlier**
from sections that include uniform fields are suspect for that reason.

The batch chooses between two ways of opening an image, and `open_method`
records which one ran. `importer` is Bio-Formats' own `BF.openImagePlus` — the
code behind the series-chooser dialog, which prepares a description of **every**
series in the file before returning the one asked for. Its cost is therefore
O(series in the file) *per call*, and the batch calls it once per row: 182 s per
row on a 1563-series `.lif`, against 1.5 s on a 15-series one. `reader` holds a
single reader open for the whole file and assembles the `ImagePlus` directly,
which reads the same series in 0.11 s.

`auto` (the default) picks `importer` at or below 16 series in the file and
`reader` above it. Both are kept rather than one replacing the other: the
importer handles format corners the hand-built path does not, and keeping both
is what lets `Test_BatchRunner` assert they produce the same bytes — the only
guard against silent drift when Bio-Formats is next upgraded.

`batch_params.txt` — the parameters actually used, in the same
`parameter`/`value` shape as `_config.txt`, holding only the re-feedable subset
(no `script_name`, no results). It can be passed straight back as the config of
another run.

⚠️ The `Label` column of `_res.txt` differs between the two runners, and
harmlessly. `IJ.openImage()` and Bio-Formats build the slice label differently,
so the text after the roi id is not the same — but the **roi id itself is**, and
that is what `read_fiji_result.r` joins on. Verified: the same images through
both runners give identical per-feature statistics and identical channel signals.

---

## 3. R CLI output

### `annotate_features_cli.r`

```
<outdir>/<series_id>_features.rds     sf, one row per ROI      <- canonical
<outdir>/<series_id>_features_qc.png  only with --qc_plot
<outdir>/<output_prefix>features.tsv  the same, geometry dropped
```

The `.rds` is the canonical object; the TSV is the same table for anything that
cannot read R. Columns, in both:

| Column | Type | Notes |
|---|---|---|
| `roi` | chr | ROI id from Fiji |
| `z` | int | slice |
| `area` | dbl | µm², from the polygon |
| `is_bridge` | lgl | `TRUE` if this ROI only forms graph edges, see §5 |
| `feature_id` | chr | group this ROI belongs to, see §5 |
| `feature_type` | chr | `nucleus`, `nucleolus`, … |
| `series_id` | chr | from the outline table's `name` column. **`sample` before v0.7.0**: an older output is refused by every CLI that reads one, naming the version, rather than failing later or joining on nothing |
| `run_id` | chr | 10 hex characters identifying the annotate run, see §5 |
| `parent_feature_id` | chr | `NA` unless `--within` was given |
| `parent_feature_type` | chr | |
| `parent_containment` | dbl | 0–1, fraction of the child inside the parent |
| `parent_match` | chr | `direct`, `gap_filled`, or `NA` |
| *metadata* | | every non-`series_id` series-table column, when supplied |
| `geometry` | sfc | `.rds` only |

⚠️ The `.rds` is a registered `sf` object, but the tibble that
`define_feature_group()` returns is **not** — see the `sf` primer note.

### `count_features_cli.r`

```
<outdir>/<output_prefix>feature_counts.tsv          one row per series per feature type
<outdir>/<output_prefix>feature_counts_summary.tsv  only with --group_by
<outdir>/<output_prefix>feature_counts.png          only with --plot
```

`feature_counts.tsv`: `series_id`, `feature_class`, the `--feature_class_by`
component columns (`feature_type` by default), `n_detected`, `n_invalid`,
`n_failed`, `n_roi`, plus metadata columns.

#### `--feature_class_by` — what is being counted

Three flags sit next to each other and are easy to confuse, so the difference
is behavioural, not just wording:

| flag | takes | does |
|---|---|---|
| `--feature` | **values** | which feature types to include |
| `--feature_class_by` | **columns** | joined into `feature_class`, the identity of the thing counted |
| `--group_by` | **columns** | kept separate — the strata counts are broken down by |

```bash
--feature_table stats/feature_stats.tsv --feature_class_by class feature_type
```

Defaults to `feature_type`, so a run without it is unchanged apart from a
`feature_class` column that equals `feature_type`.

The component columns are kept **beside** the composite, so nothing downstream
has to split the label apart — which is where the separator would bite. A value
that already contains the separator is a **warning**, and `--class_sep` picks
another. An absent class becomes `unclassified`, never `NA` (see §5).

**The annotation stays the spine and the stats table is joined onto it**, so
`n_invalid` and `n_failed` survive. Counting from the stats table alone cannot
produce them: it holds only valid features, and that accounting is what
separates "few objects here" from "most of them failed a filter". The join is
scoped so that `invalid_*` and `failed_*` rows are not reported as misses —
they are legitimately absent from a per-feature table.

⚠️ The join is guarded by `run_id`; see §5. Joining a stats table from a
different annotate run would otherwise match at ~100% and be wrong.

`n_detected` is the count. The other three are why it is trustworthy: a series
whose objects mostly failed the z-span filter must not look like a series that
genuinely has few objects.

`feature_counts_summary.tsv`: the `--group_by` columns, `feature_type`,
`n_series` (`n_sample` before v0.7.0), `mean_detected`, `sd_detected`,
`total_detected`.

### `feature_stat_cli.r`

```
<outdir>/<output_prefix>feature_stats.tsv     one row per detected FEATURE
<outdir>/<output_prefix>feature_rejects.tsv   what did not become one
<outdir>/<output_prefix>feature_stats.pdf     one page per statistic, unless --no_plot
```

The unit is the feature, not the ROI: adjacent z-slices of one object share
signal through the point-spread function, so ROIs are not replicates. Only rows
naming a real feature are summarised; `invalid_*` and `failed_*` go to the
rejects table instead of being folded in.

| Column | Notes |
|---|---|
| `series_id`, `feature_type`, `feature_id` | the key |
| `n_roi`, `n_z` | **seed** ROIs in the feature, and distinct slices — bridges excluded |
| `z_min`, `z_max`, `z_span` | extent; `z_span` = max − min + 1 |
| `z_gaps` | `z_span − n_z` — slices inside the object's range where it was not detected |
| `n_roi_all`, `n_z_all` | every ROI in the feature and every slice it touches, bridges included; equal `n_roi` / `n_z` when nothing bridged |
| `n_bridge`, `frac_bridge` | bridge ROIs in the feature, and their share of `n_roi_all`; `0` unless `--bridge_roi` was used |
| `max_roi_per_z` | most **seed** ROIs the feature has on any one slice. **`> 1` means it spans objects sitting side by side**, not one object followed through z. Bridges are excluded: a bridge often lies *over* what it connects, so counting them would read `2` on a good rescue |
| `area_med`, `area_mean`, `area_max`, `area_sum` | per-slice ROI area, µm². `area_sum` is the shape-free size measure — see below |
| `volume` | `area_sum × z_step`, µm³. Present when a z step is **known**: `--z_step` if given, otherwise `pixel_depth` read from each series' `_config.txt`. The run says which, and says so when there is none — an absent column is otherwise indistinguishable from a missing feature |
| `circ_med`, `circ_min` | only when the `_res.txt` was found |
| `ch<N>_signal` | one column per channel measured; only when the `_res.txt` was found |
| `class` | the `--class` a feature matched, or `unclassified`; only when `--class` was given |
| *metadata* | every non-`series_id` series-table column, when supplied. `is_bridge` is **not** carried through: it is an ROI-level fact that varies within a feature, and `n_bridge` / `frac_bridge` are the feature-level answer |

⚠️ Every statistic above **excludes bridge ROIs** (`n_roi` and `n_z`
included). That is deliberate: `define_feature_group()` tests `--min_z_span`
and `--feature_area` on the ordinary ROIs alone, so if these numbers counted
bridges, a threshold read off one of these plots would not mean the same thing
as the same number handed to the filter. `frac_bridge` is how you see how much
of a feature is resting on rejected ROIs — a high value is evidence that the
*filter* needs adjusting, not a result to trust. See §5.

⚠️ **`area_med` is only a size measure while the object is a sphere.** A
median cross-section stands in for size when every section is a circle of
predictable radius, which holds for immature and fully grown oocytes but *not*
for growing ones, which are visibly irregular. `area_sum` — summed
cross-sectional area — is the shape-free alternative, and `--z_step` turns it
into a real volume by the Cavalieri estimate (`area_sum × z_step`).

`--z_step` no longer has to be supplied. Fiji's `_config.txt` has recorded
`pixel_depth` since 0.2.0, and `feature_stat_cli.r` reads it **per series**,
from the config beside that series' tables — not once for the run, because
pixel size varies within a single `.lif` here (0.4456 / 0.2227 / 0.1098 µm) and
one z step for all of them would be wrong for most. An explicit `--z_step` wins
everywhere. Two cases still yield no `volume`, and both are said aloud rather
than defaulted: results produced before 0.2.0 have no `pixel_depth` in the file
at all, and a single-plane image leaves it **blank on purpose**, because there
is no z axis to measure. `volume` is also an *undercount*
wherever the object was missed on a slice inside its own range — nothing is
interpolated, and `z_gaps` is the column that says how much is missing.

`ch<N>_signal` is aggregated by `--channel_stat`, default **`wmean`** — the mean
weighted by ROI area. A plain mean lets a feature's small tapering end slices
vote as loudly as its equator. See `note/if_quantification.md`.

⚠️ The measurement tables are found as `<series_id>_<roi prefix>_res.txt`, where
the prefix comes from the **`roi` column**, not from `feature_type`. After a
`--rename` those differ, and the file on disk carries the original. Not finding
them is a **loud warning**, never a silent run without signal.

`--log_scale` works here exactly as it does in `feature_scatter_cli.r` — names
or globs, matched against the columns present, nothing logged unless named, and
a pattern matching nothing warns. When it is absent the run lists the columns
whose span is wide enough that a log axis may help, restricted to the
statistics that actually get a panel. A logged panel says `(log10)` on its
axis, and one that would have to drop a zero falls back to linear **with a
warning**.

#### `--class` — naming the kinds of object, from their own statistics

```bash
--class 'growing:area_med=600:Inf' 'growing:circ_med=0:0.7' 'small:area_med=0:600'
```

Each token is `class:column=lo:hi`, bounds **inclusive**. The name is everything
before the *first* colon, which is unambiguous because a column name cannot
contain one while a range always does — a token with no name is refused rather
than read as a class called `area_med=400`.

Several tokens naming the same class are **ANDed**. Naming the same column
twice for one class is an error, not a silent replacement.

**Priority is the flag order.** A feature matching several classes takes the
first, and the run says how many did:

```
Warning: 6 feature(s) matched more than one class and took the first.
  The current class priority is: big > round
  Reorder the --class flags to change it.
```

Taking the first is defensible; doing so invisibly is not. Swapping two
`--class` flags is expected to change the answer.

**`NA` never matches.** A feature whose `ch1_signal` is `NA` because no
measurement table was found has not been shown to lie inside the range, and
treating unknown as a match would invent class members.

A feature matching no class is an **orphan**: `class` is the literal string
`unclassified` and it is kept,
the same reasoning as a parentless nucleolus in `relate_features.r` — an object
that fits no class is evidence about the *classes*, and dropping it destroys
the evidence. `--drop_orphan_feature` removes them once you have looked.

⚠️ **`unclassified` is a string, not `NA`, and this is deliberate.** With
`NA` there, `table(stats$class)` — the obvious way to count a classified table
— drops those rows without saying so and comes out short. Measured on real
data: 17 features, `table()` reporting 15.

**`unclassified` and `other` are reserved class names.** `--class 'other:...'`
is an error. `unclassified` is what a feature matching no class is called, and
`other` is the group the plots fold unmapped classes into — a user class of
either name would put two different things under one label with nothing to say
which was which. Both are settable in `--color_map`.

The biology is declared, never hardcoded. Nothing in the code knows what an
oocyte is; it knows a class is a set of ranges. Because `class` is an ordinary
column, `--group_by class` groups the distribution panels by it and
`feature_scatter_cli.r --color_by class` colours by it.

⚠️ A class boundary read off `feature_stats.tsv` is only portable to runs with
the **same pre-grouping filters**. `--min_circularity` and `--roi_area` truncate
the statistics they act on, so a cut tuned against one is not the same cut
against another. See §5.

`feature_rejects.tsv`: `series_id`, `feature_type`, `bucket`, `n_roi`, where
`bucket` is `feature`, `invalid`, `failed` or `unassigned`. It exists so that a
thin distribution can be read as either "few objects here" or "most of them
failed a filter".

### `feature_scatter_cli.r`

```
<outdir>/<output_prefix>feature_scatter.pdf
```

Reads `feature_stats.tsv` — **not** the `.rds`. The expensive work is done once
upstream; this is the cheap end you re-run while trying pairs.

- `--plot 'x:y'`, optionally named `'id=x:y'`. The id only labels the page;
  unnamed pairs get `p1..pN` by position, and an explicit id colliding with an
  auto-assigned one is an error rather than a silent shadow.
- `--threshold 'column=value'` draws a guide line. **Keyed to the column, not
  to the plot**: a cut-off is a fact about a variable, so it appears on every
  panel where that variable does — vertical when it is x, horizontal when it is
  y. Nothing is matched by position, so nothing can drift out of step, and the
  same number cannot be stated two different ways on two panels. Repeat a key
  for a band: `'area_med=100' 'area_med=400'`.
- `--facet none | both | <column>`; `both` (default) writes a pooled page and a
  faceted one per pair.
- `--facet_keep` / `--facet_keep_file` choose which levels get broken out. **The
  pooled page always uses every row** — the point of the pair is to see the whole
  population once and a readable subset of it beside that, not to answer both
  questions from the same reduced set. The file form takes one value per line and
  allows `#` comments.
- `--facet_max` (default 16) skips the faceted page when it would exceed that
  many panels, with a warning naming `--facet_keep`. A page of fifty panels is
  not a figure.
- `--legend_max` (default 12) drops the colour key past that many levels — it
  becomes unreadable and ggplot shrinks the plot panel to make room for it. The
  colour mapping stays; only the key goes, and the subtitle says so. The key is
  also dropped when `--color_by` equals the facet column, since the strip above
  each panel already names it.
- `--show_avail_stats` lists the plottable columns with their non-NA counts,
  ranges and spans, then exits. It needs `--input` (which channels exist depends on the
  data) but not `--outdir`.

⚠️ **Nothing is drawn on a log axis unless you ask.** `--log_scale` names the
columns, as names or globs, and applies wherever they appear:

```bash
--log_scale volume 'area_*'
```

Quote a glob so the shell does not expand it against filenames. A pattern that
matches no column is a **warning**, not a silent linear plot.

Keyed to the **column, not the axis** — the same reasoning as `--threshold`:
whether a quantity wants a log scale is a fact about the quantity, not about
which axis it happened to land on.

Two automatic rules were tried and removed, both because the reader could not
see them. Keyed on the *name*, `area_sum` was logged and `volume` was not —
although `volume` **is** `area_sum × z_step`, so a change of units silently
flipped the scale and identical data read as two different results. Keyed on
the *data* (log when the span exceeds ~20×) it is at least unit-invariant, but
on real data it logs `z_min`, a slice index spanning 42× that means nothing
logged.

`--show_avail_stats` prints each column's **span** (max/min over positive
values) and lists the wide ones, so the choice is informed rather than guessed.
Span is the right statistic for it precisely because it does not change with
the units. A logged axis says `(log10)` in its label. A log axis that would
drop a zero or negative value falls back to linear **with a warning** rather
than silently losing the point.

`--smooth` and `--corr` are off by default on purpose: per-series n here is
single digits, where a fit is noise with a ribbon around it and a coefficient
invites more confidence than the data supports.

Several `--input` tables gain a `source_file` column automatically, so runs with
different settings can be compared with `--facet source_file`.

### `montage_qc_cli.r`

Panel (iii) takes the same `--feature_table` / `--feature_class_by` /
`--class_sep` as `count_features_cli.r`, so an outline can be coloured by
`class` rather than by `feature_type`.

`--color_map 'growing=red' 'small=blue'` highlights the classes under
inspection. Everything else — including orphans — is drawn as a single `other`
group in **`grey30`**, so invalid and unclassified features are still visible
without competing for attention. It is that dark on purpose: the per-ROI
outlines underneath are `grey80`, and a lighter `other` was indistinguishable
from them.

Both reserved names are settable here, and they are the exception to the
"names a class that is not present" warning — `other` never appears in the
data, being the group this step invents:

```bash
--color_map 'growing=red' 'other=grey60' 'unclassified=gold'
```

Naming `unclassified` also **pulls it out of `other`** into its own level, for
when the orphans are what you want to look at.

⚠️ **The colour map is always complete.** Measured on ggplot2 4.0.3, a level
missing from `scale_colour_manual(values=)` is drawn in `na.value` grey
*and dropped from the legend*, so a real class vanishes from the figure's
account of itself. `class_palette()` therefore **recodes** the unmapped classes
into one `other` level rather than letting them fall through, and puts `other`
**first** so it is drawn underneath. See `.claude/skills/r-ggplot`.

The panel has **no legend on purpose** — it is drawn to the image frame so it
lines up with the Fiji PNGs beside it, and a legend would steal width and break
that. The key goes in the panel caption instead, which also names what is
inside `other`:

```
R union, z-aware (17) | growing=red | other(1): small
```

A `--color_map` colour that is not an R colour name or `#RRGGBB` is refused
before ggplot sees it; one naming a class that is not present warns.



One PNG, panels left to right: raw z-projection, Fiji overlay, R union. The
first two are the Fiji overview pair described in §2 — `--projection` takes the
unsuffixed PNG and `--overlay` the `_overlay` one, both for the same channel.

---

## 4. Argument conventions, shared by every CLI

- Multi-value arguments are **space-separated**: `--feature nucleus nucleolus`.
- Per-feature settings are `key=value` tokens, with `default=` as the fallback:
  `--min_z_span 'default=5' 'nucleolus=2'`.
- Containment is `child=parent`: `--within 'nucleolus=nucleus'`.
- Renaming is `old=new`: `--rename 'nucleus=oocyte'`.
- Axis pairs are `x:y`, optionally named: `--plot 'p1=area_med:ch1_signal'`.
- ⚠️ Most `key=value` flags **reject a repeated key** as a mistake. `--threshold`
  is the exception and collects them, because two guide lines on one variable
  (a band) is its normal case.
- **Ranges are `key=lo:hi`**, with either end omittable: `--roi_area
  'nucleus=80:Inf'`, `'nucleus=80:'` and `'nucleus=:100'` are all valid. A colon
  rather than a dash, because `80--5` is ambiguous; and never a comma, see below.
- ⚠️ **A comma is reserved.** argparser splits a multi-value argument on commas
  even when the shell delivered it as one word, so a comma can never be a
  separator here — and a file path containing one is silently split in half.
  Never write a comma into a value or a filename.

---

## 5. Naming vocabulary

### ROI ids — Fiji assigns these

```
<feature>_SSSS-NNNN-YYYY
          │    │    └── y-centre of the ROI bounds, zero-padded
          │    └─────── index within that slice
          └──────────── slice number
```

e.g. `nucleus_0001-0001-0433`. Reproduced in Groovy so output stays compatible
with the older macros. `read_fiji_result()` finds the id by matching
`\d{4}-\d{4}-\d{4}$` inside `Label`.

### `feature_id` — R assigns these, per image

| Pattern | Meaning |
|---|---|
| `<feature>_N` | a real detected feature — **this is what gets counted** |
| `invalid_<feature>_N` | a group that failed `min_z_span` or `min_avg_area` |
| `failed_<feature>_<reason>` | an ROI that never reached grouping; `<reason>` is `excluded`, `name`, `area`, `overlap` or `bridge` |
| `NA` | an ROI that reached no group at all |

`N` is sequential within an image and carries **no meaning across images** —
`nucleus_1` in two series are unrelated objects. Always group by
`series_id` + `feature_id`.

`<feature>` here is the **reporting** name, which `--rename 'nucleus=oocyte'`
changes. The `roi` column keeps Fiji's original prefix either way, because that
is what the grouping step matches on — so after a rename a row reads
`feature_id = oocyte_1`, `roi = nucleus_0001-0001-0433`. That is not a
mismatch; it is provenance.

To test whether a row is a real detected feature, compare against the row's own
`feature_type` (`startsWith(feature_id, paste0(feature_type, "_"))`), never
against a hardcoded list of names — `.cli_valid_rows()` does this.

### `run_id` — which annotate run produced this

Ten hex characters, hashed from the effective parameters, the repo `VERSION`,
and a fingerprint of the resolved input files (basename and size). Written by
`annotate_features_cli.r` onto every row, and carried through by
`feature_stat_cli.r`.

It exists because **`feature_id` is sequential within an image and means
nothing across runs.** Two annotate runs over the same images produce the same
series ids *and* the same `nucleus_1`, `nucleus_2`, … So joining one run's
per-feature table onto another run's annotation matches on
`series_id` + `feature_id` at essentially **100%** and attaches every value to the
wrong object. The obvious guard — reporting the match rate — reads *perfect*
in exactly the case that is broken.

`join_feature_table()` compares the two `run_id`s and **stops** when they
disagree, naming both. `--force` overrides it and downgrades the stop to a
warning. A table written before `run_id` existed joins with a warning saying
the check could not be made.

Identical parameters over identical inputs give an identical id, so a re-run
joins cleanly. What it does **not** catch: an input file edited in place to the
same byte length. Hashing the outline tables' contents would, at the cost of
reading every input on a large batch.

### `is_bridge` — an ROI that holds an object together but is not evidence of it

A pre-grouping ROI filter (`--min_circularity`, `--roi_area`) drops ROIs
*before* the overlap graph is built, so removing an object's interior slices
opens a z-gap that `--max_z_dist` cannot span and **one object is counted as
two**. Observed on real oocyte data: a circularity cut removed an oocyte's
widest cross-sections, because the equator of a large object is the least
circular part of it.

⚠️ **Fiji's `nucleus_circularity` carries the same hazard and no cure.** It
runs inside `Analyze Particles`, so a rejected ROI is never written to
`_outline.txt` at all — there is no row for `--bridge_roi` to promote, and no
record beyond the count in `nucleus_circ_rejected`. It defaults to
`0.00-1.00`, which filters nothing. Turn it on to remove imaging artefacts that
the size filter cannot catch (a reflection off a section edge thresholds like
an object and is the wrong shape for one), and prefer the R-side filter when
the run is feeding a full analysis, because that one is reversible.

`--bridge_roi circularity roi_area` (or `all`) keeps those rejects in the graph
as **edge-formers only**. A bridge ROI:

- **can** link two ROIs across the gap it sits in,
- **cannot** seed a feature — a group containing no ordinary ROI is never valid,
- **does not** count toward `--min_z_span`,
- **does not** contribute to the mean area that `--feature_area` tests.

So bridging can rescue an object that a filter cut in half; it can never
assemble one out of rejects.

`--input`'s own name filter is **not** bridgeable, and this is not an
oversight. The other two reject on *quality*, and a low-quality ROI of the
right feature is still that feature. The name filter rejects on *identity* — an
ROI of a different feature type — and letting one form edges would glue two
unrelated objects into a single feature.

**A bridge may not merge two components that share a z-slice.** One object
contributes one ROI per slice, so a feature holding two seeds on one slice is
two objects fused, not one object followed through z. That is the difference
between the two things a bridge can do: rescuing a pinched object joins
components that are *disjoint* in z, while a merged mask joins components that
*coexist* on the same slices. The refusal is counted as a delta rather than as
a presence, because the seed pass can legitimately leave a component already
stacked and a later bridge must not be blamed for a collision it did not cause.
Seed edges are processed before bridge edges so the guard decides about
finished components; a run with no bridges is unaffected.

⚠️ **The guard only separates objects that sit side by side.** Two distinct
objects stacked in *z* are indistinguishable, by topology alone, from one
pinched object — measured on real data, a small oocyte pair 4 slices apart
stays merged. Telling those apart needs geometry, not graph structure: whether
the merged object's z-extent is plausible for its cross-sectional area, which
is what `--z_step` and `volume` exist to make answerable.

`is_bridge` is present in the output whether or not anything bridges, so a
reader never has to test for the column before using it. `feature_stat_cli.r`
reports `n_bridge` and `frac_bridge` per feature: a high fraction means the
count is resting on ROIs that the filter rejected, which is evidence that the
*filter* needs adjusting rather than a result to trust.

---

## 6. What is safe to change

| Change | Cost |
|---|---|
| adding a `_config.txt` field | cheap — readers look up by key |
| adding a column to a CLI output | cheap |
| adding a series-table column | free — non-`series_id` columns are passed through |
| renaming an output column | **breaks the next stage silently**; grep the CLIs first |
| changing the `_outline.txt` filename pattern | cheap now — identity comes from the file's content, so the pattern only has to still *select* the files (`--input_pattern`) |
| renaming a feature (`--rename`) | free downstream; the `roi` column keeps the original prefix on purpose |
| changing `Set Measurements` | breaks the `Label` join; the measurements stay correct while the join returns nothing |
| changing the ROI name format | **more expensive than it was**: it breaks the roi-id regex in `read_fiji_result()` *and* the feature name the R side now reads from the prefix |
| changing the overview `_overlay` suffix | cheap, but `--projection`/`--overlay` are passed by hand, so an old results folder keeps the old names |

The rows in bold-adjacent territory share one property: **the numbers stay
right while the join matches nothing**. Verify a format change by diffing a
reference run, never by reading.
