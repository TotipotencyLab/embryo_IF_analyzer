# embryo_IF_analyzer — working notes

A collection of imaging-analysis scripts, not a single pipeline. It began with
immunofluorescence of preimplantation embryos ("IF") and has fanned out to other
assays (PLA, oocyte counting in tissue sections). Expect per-assay entry points
sharing a common library, not one program.

`README.md` is for people *using* the repo. This file is for working *on* it.

## The output contract

Fiji writes, per feature per image:

```
<prefix><image id>_<feature>_outline.txt        name, roi, z, x, y   (one row per polygon vertex)
<prefix><image id>_<feature>_outline_ROIs.zip   ImageJ ROIs
<prefix><image id>_<feature>_res.txt            measurements, one row per ROI per channel
<prefix><image id>_config.txt                   every parameter used for that run
```

**This is a contract, not an implementation detail.** Two things depend on it and
break silently if changed:

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
  experiment.
- **`scripts/fiji/` is reference only.** The IJ1 macros are kept deliberately, for
  code study and comparison. `PLA.ijm` and `measure_manual_selection.ijm` are
  still the only implementations of those two workflows.
- `scripts/R/` holds functions `source()`d by analysis scripts, not a package.

The macros were forked per experiment because the IJ1 macro language has no import
mechanism — that is the problem the Groovy split exists to solve. **Do not add a
fourth fork.** A new assay should be a new configuration of the shared library.

## Standing decisions

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

- `**/data/` — bulk images live in `fixture/*/data/`, deliberately ignored.
- `*Position[0-9]*.txt` ignores raw Fiji dumps. **macOS sets
  `core.ignorecase=true`, so this matches lowercase `position...` too** — it once
  silently hid the entire fixture directory. `!fixture/**/*.txt` re-includes
  fixture text.
- `CLAUDE.local.md` is per-machine and untracked.

`fixture/if_data/` currently holds `res_fiji/`, `res_groovy/` and `res_headless/`
— outputs of the macro, GUI-Groovy and headless-Groovy runs of the same image,
kept as the evidence that the port is equivalent. They are untracked and large.

## Open items

- No R unit tests yet. Cutting a small fixture (the z-range option exists for
  this) and pinning current behaviour is the next step; it should come before any
  further R refactoring.
- `scripts/R/find_ROI_z_intersect.r:83` has a marked, unfixed bug: it errors when
  the overlap table has zero rows, which is reachable — the checkpoints above only
  `warning()` and fall through.
- `scripts/fiji/static_versions/` duplicates what git history already provides and
  is slated for removal; tag the commits first if the versions should stay
  addressable.
- `scripts/tmp/define_nucleus.r` exists on disk but `**/tmp/` ignores it, so it is
  invisible to git. Promote it or delete it; do not leave real code there.
- `Run_*.groovy` resolve their library directory from the SciJava script binding,
  so they must be *saved* and run from `scripts/groovy/` — an unsaved Script
  Editor buffer has no path and will fail with an explicit message.
