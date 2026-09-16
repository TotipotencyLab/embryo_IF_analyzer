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
  experiment. `Inspect_*.groovy` are read-only diagnostics, written to be read as
  well as run — they carry the Groovy/ImageJ API notes.
- Mask building lives in `RoiDetect.buildMask()`, not inline in the runner, so a
  new assay configures it rather than copying it. Watershed is an option there
  and is **off by default**: with it off the mask step is exactly what it was
  before extraction, verified against the fixture.
- **`scripts/fiji/` is reference only.** The IJ1 macros are kept deliberately, for
  code study and comparison. `PLA.ijm` and `measure_manual_selection.ijm` are
  still the only implementations of those two workflows.
- `scripts/R/` holds functions `source()`d by analysis scripts, not a package.

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

On the Fiji side, `tests/groovy/Test_BuildMask.groovy` synthesises its images, so
it needs no data:

```
/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console \
  --run tests/groovy/Test_BuildMask.groovy
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
