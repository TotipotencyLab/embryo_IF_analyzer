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

To run: open the image in Fiji, then `File › Open…` the script and press **Run**.
Parameters are `#@` script parameters, so they appear as a dialog — no file editing.
Save the `Run_*` script inside `scripts/groovy/` so it can locate its libraries.

Output per feature, written to the chosen directory:

```
<prefix><image id>_<feature>_outline.txt        name, roi, z, x, y
<prefix><image id>_<feature>_outline_ROIs.zip   ImageJ ROIs
<prefix><image id>_<feature>_res.txt            measurements, one row per ROI per channel
```

`Set Measurements` is forced by the script, so the output columns do not depend on
the operator's Fiji preferences.

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
| `plot_outline_topView.r`, `brewer_pal_2.r` | plotting helpers |

[`PLA_analysis/`](PLA_analysis/) contains the proximity ligation assay analysis
(published separately) and serves as a worked example of the R side end to end.

## Layout

```
scripts/groovy/   Fiji scripts (primary)
scripts/fiji/     IJ1 macros (reference; PLA + manual-selection still used here)
scripts/R/        downstream analysis functions
fixture/          small test inputs and reference outputs
PLA_analysis/     published PLA analysis
```

## Requirements

- **Fiji** with Bio-Formats (bundled). Groovy scripting is built in.
- **R** with `sf`, `sp`, `dplyr`, `tidyr`, `tibble`, `stringr`, `ggplot2`,
  `ggpubr`, `ggbeeswarm`, `cowplot`, `RColorBrewer`, `writexl`.

Image data is not tracked in this repository; see [`.gitignore`](.gitignore).
