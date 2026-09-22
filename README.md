# embryo_IF_analyzer

Image analysis scripts for fluorescence microscopy of preimplantation embryos and
related tissue. Started with immunofluorescence (hence "IF") and has since grown to
cover other assays.

The pipeline is two-stage:

```
Leica .lif  ──▶  Fiji: segment + measure  ──▶  .txt  ──▶  R: 3D reconstruction, stats, plots
```

Fiji writes one ROI-outline table and one measurement table per feature per image.
R reads those, merges per-slice ROIs into 3D objects, and does the analysis.

## Setting up an analysis project

1. **Make a sample sheet.** Copy
   [`config/sample_sheet_template.tsv`](config/sample_sheet_template.tsv) and
   edit it. Only `prefix` is required — the file stem Fiji wrote, everything
   before `_<feature>_outline.txt`:

   | prefix | genotype | timepoint |
   |---|---|---|
   | `GRV_Position010` | wt | E3.5 |

   Every other column is your own metadata. It is carried onto the outputs and
   can be used to group the results. The sheet is optional: the CLIs also run
   sample-unaware.

2. **Run the Fiji side** to produce the outline and measurement tables
   (see below).

3. **Run the R side:**

   ```bash
   # outlines -> features, with nucleoli placed inside their nuclei
   scripts/R_cli/annotate_features_cli.r \
       --input raw_measurements/ --feature nucleus nucleolus \
       --outdir results/ --sample_sheet config/samples.tsv \
       --max_z_dist 'default=3' 'nucleolus=1' \
       --min_z_span 'default=5' 'nucleolus=2' \
       --within 'nucleolus=nucleus' --qc_plot

   # features -> counts, grouped by your metadata
   scripts/R_cli/count_features_cli.r \
       --input results/ --outdir results/ \
       --sample_sheet config/samples.tsv --group_by genotype --plot

   # eyeball it: raw projection | Fiji outline | R union
   scripts/R_cli/montage_qc_cli.r \
       --features results/GRV_Position010_features.rds \
       --projection results/GRV_Position010_overview_ch1.png \
       --overlay results/GRV_Position010_overview_ch1_overlay.png \
       --output results/GRV_Position010_montage.png
   ```

**Every table these read and write is described in
[`note/data_formats.md`](note/data_formats.md)** — required and optional
columns, what each output means, and which changes break the next stage.

Two conventions worth knowing before you start:

- Multi-value options are **space-separated**, and per-feature settings are
  `key=value` tokens with `default=` as the fallback.
- **Never put a comma in a value or a filename.** The argument parser splits on
  commas even inside a quoted word, so a path containing one is silently cut in
  half.

## Fiji side

**Use the Groovy scripts in [`scripts/groovy/`](scripts/groovy/).** They are the
supported version. The IJ1 macros in [`scripts/fiji/`](scripts/fiji/) are kept for
reference and comparison but are no longer the primary path.

| script | purpose |
|---|---|
| `Run_NucleusSelector.groovy` | main entry point: nucleus + nucleolus detection, export, measurement |
| `Run_NucleolusDetect.groovy` | nucleoli only, using nucleus ROIs already in the ROI Manager |
| `NucleolusDetect.groovy` | per-nucleus, per-slice nucleolus thresholding |
| `RoiExport.groovy` | outline `.txt`, ROI `.zip`, measurement `.txt` |
| `RoiDetect.groovy` | mask building (blur, threshold, fill holes, watershed) and particle detection |
| `Run_Overview.groovy` | quick-look PNG: z-projection with detected outlines drawn on |
| `Overview.groovy` | projection, contrast, resize, outline drawing, PNG export |
| `Inspect_ImageFile.groovy` | list the series in a file and their dimensions, without loading pixels |
| `Inspect_Session.groovy` | report open images, the active image, ROI Manager and measurement settings |

To run: open the image in Fiji, then `File › Open…` the script and press **Run**.
Parameters are `#@` script parameters, so they appear as a dialog — no file editing.
Save the `Run_*` script inside `scripts/groovy/` so it can locate its libraries.

Output per feature, written to the chosen directory:

```
<prefix><image id>_<feature>_outline.txt        name, roi, z, x, y
<prefix><image id>_<feature>_outline_ROIs.zip   ImageJ ROIs
<prefix><image id>_<feature>_res.txt            measurements, one row per ROI per channel
<prefix><image id>_config.txt                   every parameter used for this run
```

`Set Measurements` is forced by the script, so the output columns do not depend on
the operator's Fiji preferences. The `_config.txt` records the full parameter set,
image calibration and ROI counts, so a set of results always carries the settings
that produced it.

Analysis can be restricted to chosen z-slices (e.g. `1-20,35-40`), which is useful
for cutting small fixtures out of a full stack.

`Run_NucleusSelector` can also write **overview PNGs** (off by default), for a
quick check that detection behaved. Two per channel — the bare z-projection and
the same projection with nuclei and nucleoli outlined:

```
GRV_Position010_overview_ch1.png           raw
GRV_Position010_overview_ch1_overlay.png   with outlines
```

They are written for the DNA channel plus every channel being measured, since
the outlines come from DNA and drawing them over the other channels is how you
check a signal against the compartment it should be in. Both are wanted at once
by `montage_qc_cli.r`, which is why they are separate files.

The log records the display range used per channel (`display 2.0-207.0`).
Contrast is automatic, so a channel holding only noise has that noise stretched
to full brightness and saves a convincing picture of nothing — a narrow range is
the warning.

`Run_Overview.groovy` does the same with every setting exposed, and can take its
outlines from saved `*_outline_ROIs.zip` files — so overviews can be regenerated
later without re-running detection.

Touching nuclei can be split with **watershed** (off by default). Turn it on for
objects that threshold into one blob but are two things — a zygote's two
pronuclei, or oocytes packed together in a section.

Detection uses `ParticleAnalyzer` directly rather than the ROI Manager, so the
whole pipeline also runs headless:

```bash
/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
  --run scripts/groovy/Run_NucleusSelector.groovy
```

## R side

[`scripts/R/`](scripts/R/) holds the downstream functions, `source()`d from an
analysis script:

| file | purpose |
|---|---|
| `read_fiji_result.r` | parse a Fiji measurement table, extract position/z from the label |
| `FnGroup_roi_2_polygons.r` | outline coordinates → `sf` polygons |
| `find_ROI_z_intersect.r` | merge per-slice ROIs across z into 3D objects |
| `find_overlap_roi_features.r` | overlap between feature types (e.g. spots within nuclei) |
| `define_feature_group.r` | group ROIs into features |
| `relate_features.r` | place inner features inside outer ones (nucleolus in nucleus) |
| `plot_outline_topView.r`, `brewer_pal_2.r` | plotting helpers |

[`scripts/R_cli/`](scripts/R_cli/) wraps these as command-line entry points —
`annotate_features_cli.r`, `count_features_cli.r` and `montage_qc_cli.r`. Each
takes `--help`.

[`PLA_analysis/`](PLA_analysis/) contains the proximity ligation assay analysis
(published separately) and serves as a worked example of the R side end to end.

## Tests

```bash
Rscript tests/run_tests.R
```

Fiji-side checks (no data needed):

```bash
for t in Test_BuildMask Test_Overview Test_RoiExport; do
  /Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
    --run "tests/groovy/$t.groovy"
done
```

The R suite covers the Fiji→R boundary: that the roi id is recovered for every measurement
row, that reading a table neither adds nor drops rows, and that outlines become
valid polygons and group into features across z. Tests needing `sf` skip
themselves if it cannot be loaded, so the rest still run.

## Layout

```
scripts/groovy/   Fiji scripts (primary)
scripts/fiji/     IJ1 macros (reference; PLA + manual-selection still used here)
scripts/R/        downstream analysis functions
tests/            R test suite (testthat)
fixture/          small test inputs and reference outputs
PLA_analysis/     published PLA analysis
```

## Versioning

Releases are git tags; see the repository's releases page. The `VERSION` file at
the repo root carries the same string, and each run records it in `_config.txt`
so a set of results says which version produced it.

## Requirements

- **Fiji** with Bio-Formats (bundled). Groovy scripting is built in.
- **R** with `sf`, `sp`, `dplyr`, `tidyr`, `tibble`, `stringr`, `ggplot2`,
  `ggpubr`, `ggbeeswarm`, `cowplot`, `RColorBrewer`, `writexl`.

Image data is not tracked in this repository; see [`.gitignore`](.gitignore).
