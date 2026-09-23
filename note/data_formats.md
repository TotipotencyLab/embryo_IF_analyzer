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
```

`<feature>` is `nucleus` or `nucleolus` today. The R side derives both the
sample and the feature from this filename, so the pattern is load-bearing —
`--input_pattern` overrides it, but whatever replaces it must still let
`<sample>` and `<feature>` be recovered from the basename.

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

### `montage_qc_cli.r`

One PNG, panels left to right: raw z-projection, Fiji overlay, R union.

---

## 4. Argument conventions, shared by every CLI

- Multi-value arguments are **space-separated**: `--feature nucleus nucleolus`.
- Per-feature settings are `key=value` tokens, with `default=` as the fallback:
  `--min_z_span 'default=5' 'nucleolus=2'`.
- Containment is `child=parent`: `--within 'nucleolus=nucleus'`.
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

---

## 6. What is safe to change

| Change | Cost |
|---|---|
| adding a `_config.txt` field | cheap — readers look up by key |
| adding a column to a CLI output | cheap |
| adding a sample sheet column | free — non-`prefix` columns are passed through |
| renaming an output column | **breaks the next stage silently**; grep the CLIs first |
| changing the `_outline.txt` filename pattern | breaks sample/feature recovery; needs `--input_pattern` and a doc update here |
| changing `Set Measurements` | breaks the `Label` join; the measurements stay correct while the join returns nothing |
| changing the ROI name format | breaks the roi-id regex in `read_fiji_result()` |

The rows in bold-adjacent territory share one property: **the numbers stay
right while the join matches nothing**. Verify a format change by diffing a
reference run, never by reading.
