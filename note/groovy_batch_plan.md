# Groovy batch pipeline — design, decisions and roadmap

**Temporary.** This file exists to carry the design across working sessions while
the feature is built. **Delete it once the batch path is delivered**; anything in
it that is still true then belongs in `note/data_formats.md`, `CLAUDE.md` or a
skill, not here.

Status: **A, B and C done.** D not started; E not started. Last updated 2026-09-24.

---

## 1. The problem

The R side runs in high throughput; the Groovy side does not. `Run_NucleusSelector.groovy`
begins with `#@ ImagePlus imp`, which SciJava fills from the *active image* — so it
requires a human to open each series in Fiji first. The goal is to segment every
series of every file from one invocation, interactively or headless, with the R
side unchanged downstream.

---

## 2. Measured facts

Re-measuring these costs a Fiji launch and an external SSD, so they are recorded
rather than reasoned about. Probed 2026-09-23/24, Fiji ImageJ 2.16.0 / 1.54p.

### Headless `#@` parameter passing — works

```
/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console --mem=6000m \
  --run script.groovy "strParam='hello world',intParam=7,boolParam=true,csvParam='1,2,3'"
```

| observation | result |
|---|---|
| `String` / `Integer` / `Double` / `Boolean` / `File` injection | all correct and typed |
| space inside a quoted value | survives |
| **comma inside a quoted value** | **survives** — SciJava is not argparser |
| `GraphicsEnvironment.isHeadless()` | `true` |
| `new RoiManager(false)` | `java.awt.HeadlessException` |
| `imp.show()` | returns without throwing |

So `addToRoiManager` must be forced false headless; `imp.show()` is harmless but
should still be guarded.

⚠️ The probe process **exited cleanly**, which does not match the "headless runs
finish and never exit" note in `CLAUDE.local.md`. Whatever causes that hang is
something the real pipeline does, not headless mode as such. Plan for it
(sentinel line, poll the log, kill matching `--headless`) rather than assuming
it is gone.

### Libraries already on Fiji's classpath

SnakeYAML, Gson, Jackson, org.json, `java.util.Properties`,
`loci.formats.out.OMETiffWriter`, `loci.formats.services.OMEXMLService`.
Config format is therefore a design choice, not an availability constraint.

### A real Leica `.lif`

`/Volumes/toti-ssd-SR/imaging/rnf4_project/oocyte_count/raw/260909_IHC_DDx4p23_sA7A568.lif`,
8.9 GB, 15 series. Metadata read: **887 ms**. Opening a 2-slice crop of a
2048x2048x56x3 series: **2.9 s**.

```
SERIES 0   Image002            1024x1024  Z=1   physX=0.4456 um  physZ=null
SERIES 1-6 Image003..Image006 Denoised    Z=1   physX=0.2227 um  physZ=null
SERIES 7   Series001           2048x2048  Z=56  physX=0.2227 um  physZ=0.9999
SERIES 14  Series006 Denoised  4152x4152  Z=24  physX=0.1098 um  physZ=0.9999
```

Four facts that drive the design:

1. **Series names are unique within a file but not across files.** `Series001` is
   a Leica default and will recur in every `.lif`.
2. **`physZ` is null on single-plane series.** `pixel_depth` must be nullable
   everywhere it appears.
3. **Pixel size varies 4x within one file** (0.4456 / 0.2227 / 0.1098 um). See
   §6 — this breaks "run the batch with the same setting".
4. **The first seven series are single-plane snapshots**, and several are
   `Denoised` duplicates of a raw series. Roughly half the rows of this file must
   be excluded before a 3D batch means anything. This is what `include` is for.

### What Bio-Formats hands ImageJ

```
title      = "260909_IHC_DDx4p23_sA7A568.lif - Series001"
sliceLabel = "c:1/3 z:1/56 - Series001"
cal.pixelDepth = 0.9999285454545455 micron     <- ALREADY on the ImagePlus
```

`cal.pixelDepth` is available in `Run_NucleusSelector.groovy` today and simply is
not written to `_config.txt`. One line.

`RoiExport.stripFileTitle()` deliberately discards the `<file>.lif - ` half of the
title, so `resolveImageId()` returns `Series001` — **the collision is by
construction**, not bad luck.

---

## 3. What already exists and must not be re-derived

- **`samples.tsv` IS the existing sample sheet.** `config/sample_sheet_template.tsv`
  and `note/data_formats.md` §1 already define it; `.cli_read_sample_sheet()`
  already reads it, already requires a unique `prefix`, already rejects columns
  that collide with ones the CLIs write. Only `files.tsv` is genuinely new.
- **Identity comes from file content, not the filename.** `.cli_scan_inputs()`
  reads `sample` from the outline table's `name` column and `feature` from the
  roi id prefix; `.cli_parse_contract()` is a fallback that fires only when the
  content path fails. A verbose `<alias>_<series>` prefix full of underscores is
  therefore safe, and a series named after a feature is a non-issue (there is a
  passing test: `nucleus_test_nucleus_outline.txt` -> sample `nucleus_test`).
- **`run_input_fingerprint()`** in `scripts/R/feature_join.r` already uses
  `basename:size` to detect a changed input. The sheet generator reuses the idea.
- **`sanitize()`** collapses whitespace to `_`, so `Image006 Denoised` becomes
  `Image006_Denoised` on disk.

---

## 4. The design

### Artifact chain

```
files.tsv  --[Make_SampleSheet]-->  samples.tsv  --[Run_NucleusSelector_Batch]-->  outputs  -->  R CLIs
 path                                prefix, alias, path
 alias                               series_index, series_name
 include                             dims, pixel_w/h/depth, file_size
 + per-file metadata                 include, + inherited metadata, + your columns
```

`files.tsv` is written by the user (`--scan <dir>` emits a skeleton). Per-file
metadata is typed once and seeded onto every series of that file — the main
ergonomic reason there are two sheets rather than one.

### The invariant that makes the chain join

> **the `prefix` column == the output filename prefix == the `name` column in the
> outline table.**

Consequence: the generator writes the **sanitised** value into the sheet, and
warns when sanitisation changed something. A sheet saying `my run` while the disk
says `my_run` fails the R join with nothing visibly wrong.

### Column ownership

| owner | on regeneration | examples |
|---|---|---|
| `machine` | **overwritten** — facts about the file | `series_index`, `series_name`, `size_*`, `pixel_*`, `file_size` |
| `seeded` | **written once, then preserved** | `prefix`, `alias`, `include`, inherited `condition` / `genotype` |
| `user` | **never touched** | anything you add |

Blank in `files.tsv` means "differs per series" and seeds nothing.

A seeded value **does not re-propagate** when `files.tsv` changes — predictable
beats clever. `--reseed <cols>` (space-separated) forces it; `--reseed_all`
covers every seeded column. **Every reseed reports what it changed, per column.**
Reseeding `alias` recomputes `prefix`, which orphans output already on disk under
the old names — that needs an explicit warning listing the changed prefixes.

### Uniqueness checks

| check | level | severity | catches |
|---|---|---|---|
| `path` unique | files | error | same file listed twice |
| `alias` unique | files | error | two files claiming one name |
| basename duplicated across paths | files | **warn, naming both paths** | reused filename for a different dataset |
| `basename:size` identical across paths | files | **warn** | the same file copied elsewhere — what alias cannot see |
| sanitised `<alias>_<series>` unique | samples | error | the composed collision |

The composed check is **not** redundant: alias `A_Series` + series `001` collides
with alias `A` + series `Series001`. Unique parts, colliding composite — the same
class as the `S1_growing_oocyte` filename mis-split in `CLAUDE.md`. It must run on
the *sanitised* string, or two series differing only in whitespace pass a check
and then collide on disk.

### `include`

One column, one meaning, in `samples.tsv`. File-level `include` is a **default
generator**, not a runtime filter: each series row inherits its file's value and
can then be flipped individually. No two-level AND logic anywhere.

`samples.tsv` stays a **complete inventory** — an excluded file still gets its
rows, marked off. A record that silently omits things is worse than one listing
them as off (same reasoning as keeping orphan features).

R-side contract: column absent -> everything included (backwards compatible);
`TRUE`/`FALSE`/`1`/`0` accepted; anything else is an **error**, never silently
false; `NA` is an error naming the row; duplicate `prefix` stays an error even
among excluded rows. R honouring it is what lets a sample be dropped **without
re-segmenting**.

### Merge / regeneration

Key on **`path` + `series_index`** — never on `prefix`, which the user may edit.

- no `--sheet` -> create; `--sheet <existing>` -> merge
- default: re-read every file (887 ms each; correctness beats the second)
- `--skip_explored`: skip files already present **whose recorded `file_size` still
  matches**; a changed size re-reads anyway and says so. Skip means "skip the
  unchanged", never "trust blindly"
- missing/unreadable file -> rows **kept and reported**, never dropped
- rows whose `path` left `files.tsv` -> reported, kept unless `--prune`

### Config file format

**The same two-column `parameter<TAB>value` table `_config.txt` already is**,
plus `#` comments and an optional header. (Recorded because the design
discussion above described this as `key = value`, which it never was — the
principle "read back exactly what we write" was right, the syntax was
misremembered. A line without a tab is refused with a message saying so,
since `key = value` is the mistake a reader is most likely to make.)
Chosen over YAML (which is available — SnakeYAML is on the classpath) because:

1. it closes the tune-then-batch loop with **no converter** — every run already
   writes this format;
2. both sides read it with **no new dependency** (R already parses `_config.txt`;
   YAML would mean the `yaml` package);
3. flat naming is already the idiom (`nucleus_blur_sigma`), so nesting buys
   nothing.

This overrules the "config (YAML)" line in `note/wishlist.md`. Low-regret because
flat keys map onto YAML mechanically later.

`_config.txt` mixes parameters with results (`nucleus_count`, `timestamp`,
`image_width`). Rather than reshape a file the output contract protects, the batch
writes its **effective parameters** once as a re-feedable file, and the reader
also accepts a full `_config.txt` by ignoring a known list of provenance keys.
**Unknown keys are an error** — silently ignoring `nucleus_sigma` for
`nucleus_blur_sigma` is how a typo becomes a default nobody notices.

### Entry points — two, not three modes

SciJava builds the `#@` dialog from the declarations **before the script body
runs**, so a config file chosen in that dialog cannot repopulate the fields beside
it. Anything that tries produces "which one won?" ambiguity.

- `Run_NucleusSelector.groovy` — one active image, full dialog. This is *tuning*.
  Writes its effective params.
- `Run_NucleusSelector_Batch.groovy` — sheet + config + root + outdir. Runs in the
  GUI **and** headless, identically. Its dialog asks for four paths, not twenty
  parameters.

The "mixture" case is served by editing the params file, not by a hybrid dialog.
(Untested: SciJava initializer/callback machinery might be able to prefill a
dialog. Only worth exploring if the mixture case turns out to matter.)

### Script naming

`Run_*` (does the analysis), `Inspect_*` (read-only diagnostic), `Make_*` (writes
a table the pipeline consumes). Record this in `CLAUDE.md`'s script inventory so
the next verb is not invented ad hoc.

---

## 5. Layering — the R analogy, sharpened

| R side | Groovy equivalent |
|---|---|
| `scripts/R/*.r`, sourced | `RoiDetect`, `RoiExport`, `Overview`, **new `NucleusPipeline`** |
| `<name>_cli(args)` — the testable unit | `NucleusPipeline.run(imp, params, outDir, basename)` |
| argparser block | the `#@` block in `Run_*.groovy` |
| `cli_helpers.r` | new `RunConfig` — read/write config, read sheets |

**Push more into the library than the R side does.** In R the CLI function is
directly testable because `args` is a parameter; a Groovy `#@` script can only be
tested by stripping the `#@` lines and injecting a `Binding`. The front end should
be parameter declaration, coercion, and one call.

---

## 6. Known trap: "the same setting" is not well defined

`nucSize` is in calibrated units^2 and transfers across series. `nucSigma` goes to
`IJ.run(mask, "Gaussian Blur...", "sigma=${sigma} stack")` — **pixels**. With pixel
size varying 4x inside one file, `sigma=8` is 1.78 um on Series001 and 0.88 um on
Series006 Denoised, and nothing warns.

Decision: **warn loudly when one batch spans multiple pixel sizes** (goes in the
batch runner, next to the loop that would otherwise do the wrong thing quietly),
and later add an opt-in `nucleus_blur_sigma_um` that converts per image.
Reinterpreting the existing pixel `sigma` would silently change every existing
result, so it must be a new key.

Per-row parameter overrides in the sheet were considered and **deferred** — the
sheet says *which* images, the config says *how*. A problematic series is re-run
from the GUI, and the difference is captured in its own `_config.txt`.

---

## 7. Roadmap

| | PR | contents | verification |
|---|---|---|---|
| ~~**A**~~ | **DONE.** `NucleusPipeline.groovy`; `Run_NucleusSelector.groovy` 206 -> 97 lines; `basename` override added for D | | reference diff: all six output files byte-identical, `_config.txt` identical bar the timestamp. Groovy suite 187 passed / 0 failed; R 677/0/0 under 4.6.1 |
| ~~**B**~~ | **DONE.** `RunConfig.groovy` (parse/format/coerce, unknown-key error, provenance ignored); `NucleusPipeline.PARAM_TYPES`/`DEFAULTS`/`fromConfig`; `pixel_depth` in `_config.txt`, blank for a single plane | | reference diff: measurement files byte-identical, `_config.txt` differs by exactly the one added row. Groovy 233 passed / 0 failed; R 682/0/0 |
| ~~**C**~~ | **DONE.** `schema/sheet_columns.tsv` + `SheetSchema`/`Tsv`/`SampleSheet` · `Make_SampleSheet.groovy` (scan + build + merge, headless) · `config/files_template.tsv` · `data_formats.md` §1 rewritten · `Inspect_ImageFile.groovy` committed | | real LIF: 15 rows, prefixes unique and sanitised, dims match §2; rerun over a hand-edited sheet kept every edit (0 added, 15 updated). Groovy 123 on the new/changed tests; R 692/0/0 |
| **D** | `Run_NucleusSelector_Batch.groovy` — loop, per-row try/catch, `batch_summary.tsv`, mixed-pixel-size warning, `addToRoiManager` forced off, images closed each iteration | | one-row sheet over Position010 reproduces the fixture byte-for-byte; then a 2-row sheet on the LIF |
| **E** | R honours `include`; `--z_step` defaults from `pixel_depth`; consumes the schema file | independent of A–D | existing suite + a sheet with `include=FALSE` |

Batch robustness requirements for D, easy to forget:

- per-row try/catch — an exception on series 57 must not cost the other 143
- `batch_summary.tsv`: prefix, series, status, nucleus count, duration, error text
- close each image and reader every iteration. 16 GB machine; one 1400x1400x61
  stack wanted ~6 GB. A leak dies around series 12 with an OOM that looks like a
  code bug
- cross-check sheet dimensions against the opened image; warn on mismatch (catches
  a sheet generated from a different version of the file)

### Test fixture

A multi-series OME-TIFF for the batch tests, with **the generator script tracked
in git rather than the TIFF** (user's call). `loci.formats.out.OMETiffWriter` is
on the classpath. Cross-check its behaviour against the real `.lif` above.

---

## 8. Still open

1. `--reseed_all` and `alias`: settled that it may overwrite `alias`, with a
   warning listing changed prefixes. Confirm the warning is enough versus
   requiring a separate opt-in.
2. Folder name for the schema file — `schema/` proposed; `internal/` the
   alternative.
3. Whether `Make_SampleSheet` should refuse, rather than warn, on a duplicated
   input basename.
