# Data formats

Every table this repo reads or writes, in one place. **This file is the
authoritative description of the shapes**; `CLAUDE.md` explains why some of them
are dangerous to change, and `README.md` covers the subset a user needs to start
a project. When a format changes, this file changes with it — the doc-sync step
in `CLAUDE.md` exists to check that.

Column names below are exact, including case.

---

## 1. Sample sheet — you write this

Optional everywhere. Supplies experimental metadata and, where given, restricts
which files are processed. It never supplies paths: `--input` always answers
"where is the data".

Read by `.cli_read_sample_sheet()`. Accepted extensions: `.tsv` / `.txt`
(tab-delimited), `.csv`, `.xlsx` / `.xls`.

| Column | Required | Meaning |
|---|---|---|
| `prefix` | **yes** | the file stem Fiji wrote, e.g. `GRV_Position010` — everything before `_<feature>_outline.txt`. Must be unique; whitespace is trimmed. Rename the column with `--id_column`. |
| anything else | no | carried through verbatim onto every output row, and usable in `--group_by` |

⚠️ A metadata column may not be named after one the CLIs write themselves —
`roi`, `z`, `area`, `geometry`, `sample`, `feature_id`, `feature_type`,
`parent_*`, `n_detected`, `n_invalid`, `n_failed`, `n_roi`. The sheet is
rejected with the offending name rather than the column being silently renamed
to `area...7`. (The `--id_column` itself is exempt: it is the key, not
metadata.)

Template: [`config/sample_sheet_template.tsv`](../config/sample_sheet_template.tsv).

Behaviour when it is supplied:

- rows naming a `prefix` with no matching file → **warning**, listed
- files whose sample is not in the sheet → dropped, reported as a message
- no overlap at all → **error** (a silent empty run is the failure mode this
  repo is built to avoid)

---

## 2. Fiji output — `scripts/groovy/` writes this

Per feature, per image. `<prefix>` is the operator's `--output_prefix`,
`<image id>` is resolved from the image title (see `RoiExport.resolveImageId`).

```
<prefix><image id>_<feature>_outline.txt        polygon vertices
<prefix><image id>_<feature>_outline_ROIs.zip   ImageJ ROIs
<prefix><image id>_<feature>_res.txt            measurements
<prefix><image id>_config.txt                   every parameter used
<prefix><image id>_overview_ch<c>.png           quick-look projection      (optional)
<prefix><image id>_overview_ch<c>_overlay.png   the same, outlines drawn   (optional)
```

`<feature>` is `nucleus` or `nucleolus` today.

### Where the R side gets the sample and the feature

**From the file's content, not its name.** Both identities are already in the
table:

| Identity | Comes from | Example |
|---|---|---|
| sample | the `name` column | `GRV_Position010` |
| feature | the `roi` column's prefix | `nucleus_0001-0001-0433` → `nucleus` |

The filename only has to get the right files onto the list; `--input`'s glob and
`--input_pattern` **select**, they no longer **parse**. This is what lets a
feature name contain an underscore (`growing_oocyte`), and it means renaming a
file cannot silently change what the R side thinks is in it.

⚠️ The ROI id's tail is fixed-shape and anchored (`_SSSS-NNNN-YYYY`), which is
why a greedy prefix is correct when reading the feature *out of an ROI id* and
was wrong when reading it out of a *filename* — a filename has no such anchor,
so `S1_growing_oocyte_outline.txt` parsed as sample `S1_growing`, feature
`oocyte`. Fixed, and pinned in `test-feature_identity.R`.

Two rules that are errors, not warnings, because a silent pick would file half a
table under the wrong name:

- one file may hold **exactly one** feature type
- one file may hold **exactly one** sample name

`.cli_parse_contract()` remains as a fallback for tables written without a
`name` column, and a run says how many files it had to fall back for.

### `<image id>` — how it is resolved

`RoiExport.resolveImageId(imp, positionPattern)`, and it **never contains
whitespace**: `sanitize()` collapses runs of whitespace to `_`, because the id
is written into the `name` column of the tab-separated outline table as well as
into filenames.

ImageJ prefixes a hyperstack slice label with the plane's coordinates, and
**those slashes are not path separators**:

```
"c:1/3 - Image002"
"c:1/4 z:1/50 - Lightning 001/Mark_and_Find 001/Position010"
"c:1/3 z:12/56 - Series001"
```

So the coordinate prefix is stripped first, then the `/`-segment containing
`positionPattern` is found, then everything before the pattern is dropped —
`56 - Series001` becomes `Series001`. If the pattern matches nothing in the
label, the title is tried the same way; Bio-Formats titles a series
`<file>.lif - <series name>`, and the file part is removed. The raw title and
slice label are logged on every run, so a wrong id can be diagnosed from the
log alone.

### Overview PNGs — quick-look only, never an input to a measurement

Written by `Run_NucleusSelector.groovy` (when "Save overview PNG" is on) and by
`Run_Overview.groovy`. The name is decided in one place,
`Overview.overviewPath(dir, basename, channel, suffix)`:

```
<prefix><image id>_overview_ch<c><suffix>.png
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
| `name` | chr | the image id — **never contains whitespace**, see §2 |
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

Provenance, read long after the run. Fields that other code depends on:

| Field | Used by |
|---|---|
| `image_width`, `image_height` | **pixels.** `montage_qc_cli.r`, to draw its panel over the same frame as the Fiji PNG |
| `pixel_width`, `pixel_height`, `pixel_unit` | the same, to convert that frame to µm |
| `script` | records the repo `VERSION` that produced the directory |
| `overview_channels`, `overview_overlay_suffix` | which overview PNGs exist, so a results folder can be read later without guessing. Blank when none were written |

Everything else is a record of the run's parameters. A key that is absent must
be handled, not assumed: configs written before a field existed are still valid
input (`montage_qc_cli.r` warns and degrades rather than failing).

---

## 3. R CLI output

### `annotate_features_cli.r`

```
<outdir>/<sample>_features.rds        sf, one row per ROI      <- canonical
<outdir>/<sample>_features_qc.png     only with --qc_plot
<outdir>/<output_prefix>features.tsv  the same, geometry dropped
```

The `.rds` is the canonical object; the TSV is the same table for anything that
cannot read R. Columns, in both:

| Column | Type | Notes |
|---|---|---|
| `roi` | chr | ROI id from Fiji |
| `z` | int | slice |
| `area` | dbl | µm², from the polygon |
| `feature_id` | chr | group this ROI belongs to, see §5 |
| `feature_type` | chr | `nucleus`, `nucleolus`, … |
| `sample` | chr | the `prefix` |
| `parent_feature_id` | chr | `NA` unless `--within` was given |
| `parent_feature_type` | chr | |
| `parent_containment` | dbl | 0–1, fraction of the child inside the parent |
| `parent_match` | chr | `direct`, `gap_filled`, or `NA` |
| *metadata* | | every non-`prefix` sample sheet column, when supplied |
| `geometry` | sfc | `.rds` only |

⚠️ The `.rds` is a registered `sf` object, but the tibble that
`define_feature_group()` returns is **not** — see the `sf` primer note.

### `count_features_cli.r`

```
<outdir>/<output_prefix>feature_counts.tsv          one row per sample per feature type
<outdir>/<output_prefix>feature_counts_summary.tsv  only with --group_by
<outdir>/<output_prefix>feature_counts.png          only with --plot
```

`feature_counts.tsv`: `sample`, `feature_type`, `n_detected`, `n_invalid`,
`n_failed`, `n_roi`, plus metadata columns.

`n_detected` is the count. The other three are why it is trustworthy: a sample
whose objects mostly failed the z-span filter must not look like a sample that
genuinely has few objects.

`feature_counts_summary.tsv`: the `--group_by` columns, `feature_type`,
`n_sample`, `mean_detected`, `sd_detected`, `total_detected`.

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
| `sample`, `feature_type`, `feature_id` | the key |
| `n_roi`, `n_z` | ROIs in the feature, and distinct slices |
| `z_min`, `z_max`, `z_span` | extent; `z_span` = max − min + 1 |
| `z_gaps` | `z_span − n_z` — slices inside the object's range where it was not detected |
| `area_med`, `area_mean`, `area_max`, `area_sum` | per-slice ROI area, µm² |
| `circ_med`, `circ_min` | only when the `_res.txt` was found |
| `ch<N>_signal` | one column per channel measured; only when the `_res.txt` was found |
| *metadata* | every non-`prefix` sample sheet column, when supplied |

`ch<N>_signal` is aggregated by `--channel_stat`, default **`wmean`** — the mean
weighted by ROI area. A plain mean lets a feature's small tapering end slices
vote as loudly as its equator. See `note/if_quantification.md`.

⚠️ The measurement tables are found as `<sample>_<roi prefix>_res.txt`, where
the prefix comes from the **`roi` column**, not from `feature_type`. After a
`--rename` those differ, and the file on disk carries the original. Not finding
them is a **loud warning**, never a silent run without signal.

`feature_rejects.tsv`: `sample`, `feature_type`, `bucket`, `n_roi`, where
`bucket` is `feature`, `invalid`, `failed` or `unassigned`. It exists so that a
thin distribution can be read as either "few objects here" or "most of them
failed a filter".

### `montage_qc_cli.r`

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
| `failed_<feature>_<reason>` | an ROI that never reached grouping; `<reason>` is `excluded`, `name`, `area` or `overlap` |
| `NA` | an ROI that reached no group at all |

`N` is sequential within an image and carries **no meaning across images** —
`nucleus_1` in two samples are unrelated objects. Always group by
`sample` + `feature_id`.

`<feature>` here is the **reporting** name, which `--rename 'nucleus=oocyte'`
changes. The `roi` column keeps Fiji's original prefix either way, because that
is what the grouping step matches on — so after a rename a row reads
`feature_id = oocyte_1`, `roi = nucleus_0001-0001-0433`. That is not a
mismatch; it is provenance.

To test whether a row is a real detected feature, compare against the row's own
`feature_type` (`startsWith(feature_id, paste0(feature_type, "_"))`), never
against a hardcoded list of names — `.cli_valid_rows()` does this.

---

## 6. What is safe to change

| Change | Cost |
|---|---|
| adding a `_config.txt` field | cheap — readers look up by key |
| adding a column to a CLI output | cheap |
| adding a sample sheet column | free — non-`prefix` columns are passed through |
| renaming an output column | **breaks the next stage silently**; grep the CLIs first |
| changing the `_outline.txt` filename pattern | cheap now — identity comes from the file's content, so the pattern only has to still *select* the files (`--input_pattern`) |
| renaming a feature (`--rename`) | free downstream; the `roi` column keeps the original prefix on purpose |
| changing `Set Measurements` | breaks the `Label` join; the measurements stay correct while the join returns nothing |
| changing the ROI name format | **more expensive than it was**: it breaks the roi-id regex in `read_fiji_result()` *and* the feature name the R side now reads from the prefix |
| changing the overview `_overlay` suffix | cheap, but `--projection`/`--overlay` are passed by hand, so an old results folder keeps the old names |

The rows in bold-adjacent territory share one property: **the numbers stay
right while the join matches nothing**. Verify a format change by diffing a
reference run, never by reading.
