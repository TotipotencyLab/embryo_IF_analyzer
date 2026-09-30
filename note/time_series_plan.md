# Plan: time-series support, driven by the Luxendo case

**Status: design. Nothing implemented.**

Scrutinised end to end on 2026-09-30; the findings are folded in rather than
appended, so the decisions below are post-review. The largest was that
`feature_id` and "the object through time" were the same word — see "Identity:
what is unique, and in what scope".

Working note for tracking progress and holding decisions that must survive a
context compaction. **Delete when all milestones land — but migrate the
surviving decisions first** (into `CLAUDE.md`, `note/data_formats.md` or
`note/luxendo_file_format.md`), because the rationale is the part worth keeping
and deleting the file is how it gets lost.

Format facts about the input live in `note/luxendo_file_format.md`, not here.

## Goal

Make the repo able to ask time-resolved questions — how one nucleus's volume,
cross-sectional area, circularity or signal intensity changes over time —
rather than analysing each timepoint as an unrelated image. Luxendo TruLive3D
is the first dataset that needs it and the driving case throughout.

The alternative considered and rejected: treat each (position, timepoint) as an
independent sheet row and change nothing in the output contract. Cheaper, gives
per-timepoint statistics, but cannot link a feature to itself across time, so
every time-resolved question stays out of reach. The linking is the point.

## Sequence

Agreed order, with the reason each sits where it does. Numbers are the order of
work, not of importance.

| # | work | why here |
|---|---|---|
| **1** | **M1** Luxendo transform | urgent — a user is reading `.lux.h5` by hand today. Independent of the contract. |
| **2** | **V** identity/vocabulary unification | before any *other* schema change: M2 changes the schema too, and two migrations is worse than one. M4 and the S3 both key on this column. |
| **3** | **M2** time axis | the contract decisions. |
| **4** | **M4** inspection scripts | small, and they give a way to *eyeball* M2's output — worth something at the verify step. |
| **5** | **S3** container | after `t` exists. Position relative to M3 is **unsettled** — see below. |
| **6** | **M3** linking across time | needs `t`. Much smaller than first scoped — TrackMate does the linking. |

Two constraints that cut across the order:

- **Do not run the full 33 GB conversion until M2's contract is settled**, or it
  gets done twice. M1 can be *written* and tested on a few positions first.
- **M1's manifest should use `image_id` from day one**, anticipating V, so that V
  does not have to revisit it. M1 is otherwise unaffected by V because it writes
  its own table and feeds `Make_SampleSheet` unchanged.

⚠️ **Open: S3 before or after M3?** The plan puts the container first because M3
consumes it. The scrutiny pass called that a contradiction of the repo's own
precedent — `BatchRunner.runEach()` was extracted *because a second caller
appeared*, not in anticipation of one — and argued for writing M3 against plain
tables and extracting the container once its shape is known. That argument got
stronger when M3 shrank to a TrackMate call. Not yet decided.

## Milestones

### M1 — Luxendo input transform

- [ ] Read `.lux.h5` via JHDF5 (`ch.systemsx.cisd.hdf5`), not Bio-Formats.
- [ ] Build an assembly manifest from a Luxendo run folder: one row per
      **output file**, naming every source file and plane that feeds it.
      Identity column named `image_id` from the start.
- [ ] Generic assembler: manifest -> TIFF. One timepoint per file by default.
- [ ] Luxendo wrapper running both end to end.
- [ ] Per-output provenance record (source paths, checksums, gatherer version).
- [ ] Honour `include`, with exactly `BatchRunner.isIncluded()`'s vocabulary.
- [ ] Verification mode: re-read output planes, compare to source by checksum.
- [ ] Assert z uniform across timepoints of one position; it is NOT uniform
      across positions (16..39, one position at z=1).

### V — identity and vocabulary unification

See "The identity problem" and "Identity: what is unique, and in what scope"
below. This is not only a rename; it is the one schema migration, so the
`image_id`, `feature_id` and `track_id` decisions all ride together.

- [ ] Decide `image_id` as the one word, retire `sample`.
- [ ] Make both entry points write the same thing into it.
- [ ] `samples.tsv`: `prefix` -> `image_id`, with `prefix` accepted as a
      deprecated alias that warns.
- [ ] `feature_id` numbered globally within an image, across all timepoints.
- [ ] `track_id` reserved in the schema; M3 populates it.
- [ ] `schema/sheet_columns.tsv`, `note/data_formats.md`,
      `tests/testthat/test-data_formats.R`, R CLI flags, `config/*template*`.

**Kept against the review's advice, deliberately.** The scrutiny pass argued V
does not advance the goal and changes an existing output file for a naming
preference. Overruled on the grounds that the repo has one user and few analysed
datasets, so the break is cheap now and expensive later, and that the confusion
`sample` causes is already real on the ovary cross-section data. Recorded so the
trade-off is visible rather than forgotten.

### M2 — time axis through the pipeline

- [ ] Groovy: frame loop in `RoiDetect` / `NucleusPipeline`.
- [ ] `t` column in `_outline.txt`.
- [ ] `roi`, `t`, `ch` as **explicit columns** in `_res.txt`; no `z` there
      (decision 4).
- [ ] ROI id gains `TTTT-` when frames > 1 (decision 5) — forced by the ROI zip.
- [ ] R: `t` honoured in grouping; absent `t` means one frame.
- [ ] Join on `(roi, t)` (decision 4).

### M4 — inspection round trip

- [ ] `Inspect_AnnotatedFeatures.groovy` — read-only feature-count table.
- [ ] `Open_AnnotatedFeatures.groovy` — one image's ROIs into the ROI Manager.
- [ ] `Open_SampleSheetRow.groovy` — open row N (1-based, unfiltered) of a sheet.

### S3 — the container

See "The S3 container" below.

### M3 — linking features across time

**Outsourced to TrackMate.** See "Linking: TrackMate, verified" below.

- [ ] `Make_FeatureTracks.groovy`: feature centroids per (image_id, feature_id,
      t) -> TrackMate LAP tracker -> `tracks.tsv` (image_id, feature_id, t,
      track_id).
- [ ] R joins `tracks.tsv` onto the feature table by (image_id, feature_id).
- [ ] Record the tracker settings used, the same way `_config.txt` records
      everything else.
- [ ] *Low priority:* a primitive in-house overlap linker as a fallback and a
      cross-check. Not on the critical path now.

## Design decisions

**1. One object contributes one ROI per z-slice *per time frame*.**
That is the correct statement of `define_feature_group()`'s invariant. An ROI
from t=0 must never fuse with one from t=1 during feature annotation. Grouping
partitions by `t` before building the overlap graph.

⚠️ `define_feature_group()` has no partition argument today
([define_feature_group.r:72-83](../scripts/R/define_feature_group.r)) — this is
a new parameter, **and** the numbering must become global within the image, or
every timepoint emits `nucleus_1`. See "Identity: what is unique, and in what
scope".

**2. Backward compatibility is by absence, not by flag.** A table with no `t`
column is one time frame. Every existing IF and oocyte output keeps working
untouched and no caller opts in. Same shape as `readParams` tolerating missing
keys.

**3. Time arrives two ways and the run must record which.** Either as frames in
the image stack, or as a sample-sheet column when the gatherer wrote one file
per timepoint (the default). Both must work. `_config.txt` should say which, for
the same reason it records `open_mode`.

**4. `_res.txt` gets explicit `roi`, `z`, `t`, `ch` columns written by us.**
Superseding an earlier idea of parsing `t` out of the slice label, which was
wrong for a reason worth recording.

Measured (2026-09-30) on synthesised hyperstacks — which of ImageJ's
`STACK_POSITION` columns appear depends on the image *shape*, and what they
contain changes with it:

| image shape | hyper? | `Ch` | `Slice` | `Frame` |
|---|---|---|---|---|
| 1c 1z 1t | no | – | 1 | – |
| 1c 5z 1t | no | – | z | – |
| 3c 5z 1t | yes | ✓ | z | – |
| 3c 5z 4t | yes | ✓ | z | ✓ |
| 1c 5z 4t | yes | – | z | ✓ |
| **1c 1z 4t** | **no** | – | **t, reported as `Slice`** | – |
| **3c 1z 4t** | yes | ✓ | **absent** | ✓ |
| **3c 1z 1t** | **no** | **absent** | channel, as `Slice` | – |

So `Slice` means z, or t, or the channel, depending on shape; `Ch` and `Frame`
vanish in cases the Luxendo data actually contains (`3c 1z 4t` is `L26A pos3`
gathered across time). Label parsing and stack-position columns are *both*
unreliable. `measureRois()` already knows the true `ch`, `slices[i]` and (once
it loops) the frame, and `rt` is our own `ResultsTable` — so writing them
ourselves is authoritative, shape-independent and about four lines.

Verified that a string column can be added and survives `rt.save()`:
headings came out as `[Label, ..., Ch, Slice, Frame, AR, Round, Solidity, roi]`,
with `roi` holding `nucleus_0003-0001-0030`. Our columns append after ImageJ's.

**Keep `t` as a column even though the ROI id carries `TTTT`.** Asked directly,
and the answer is not the parsimonious one. Three reasons:

- **The redundancy is load-bearing.** With `t` a column on both sides and the
  join on `(roi, t)`, a disagreement between the two tables drops rows — and
  `tests/testthat` already asserts that reading a table neither adds nor drops
  rows. Derive `t` instead and a mismatch is undetectable.
- **`TTTT` is conditional** (present only when frames > 1, decision 5), so a
  parser would need to handle two id shapes. A column that is simply *absent* is
  already the backward-compatibility signal (decision 2).
- **Do not make the id do double duty.** `TTTT` exists so ROI names are unique
  inside the zip; the column exists to carry the value. The outline table
  already made exactly this split for z — an explicit `z` column despite `SSSS`
  being in the id — and that precedent is the one to follow.

**No `z` in `_res.txt` at all.** Nothing on the measurement path groups by z —
`CLAUDE.md` is explicit that the unit is the feature, not the ROI, and adjacent
z-slices are not replicates — so it would be a column nothing reads. Its
agreement is also implied transitively, since the roi id encodes z and `roi` is
a key.

**The join is `by = c("roi", "t")`.** Both are keys, so nothing is a duplicated
non-key column and the worry about accumulating join columns goes away: the only
columns the two tables share are the ones they are joined on.

⚠️ `read_fiji_result()` ends with `left_join(res_df, res_label_info, by="label")`
and `res_label_info` already has a `roi` column. A `roi` column in `_res.txt`
becomes `roi.x`/`roi.y` and every downstream reference silently disappears. The
Groovy and R changes must land in the same PR.

**5. ROI id becomes `TTTT-SSSS-NNNN-YYYY` when frames > 1. This is forced, not
cosmetic.** `saveRoiZip()` uses the ROI name as the zip entry name
(`new ZipEntry(names[i] + ".roi")`). Four frames of the same object produce four
identical names, and `ZipOutputStream` throws `ZipException: duplicate entry`.
Loud, which is right, but it means the current 3-field id cannot survive a
multi-frame run.

Of the two ways to add `t`, the evidence favours the prefix:

- `TTTT-SSSS-NNNN-YYYY` — `feature_roi_prefix()`'s
  `^(.+)_\d{4}-\d{4}-\d{4}$` **fails to match** (four groups after the
  underscore, not three), returns `NA`, and per `CLAUDE.md` that path warns
  loudly. Column identification by `\d{4}-\d{4}-\d{4}$` still matches the
  correct tail, since the last three fields are still z, index, y.
- `<feature>_t0001_SSSS-NNNN-YYYY` — the greedy `(.+)` **matches** and returns
  `nucleus_t0001` as the feature type, so every timepoint becomes a different
  feature. Silently wrong.

Prefer the option that fails loudly.

**6. Container: TIFF, one timepoint per file.** Full resolution, lossless.
Classic TIFF's 4 GB cap is real — one position x 4 timepoints is 3.93 GB — and
per-timepoint files are ~981 MB. If combined files are ever wanted,
`OMETiffWriter.setBigTiff()` and its companion mode are both available; Zarr and
N5 are not (see the format note).

**7. Gathering for analysis and gathering for eyeballing stay separate.** The
analysis gather is lossless, full-resolution, checksum-verified. Any downsampled
browse-in-Fiji stack is a different entry point with different output naming, so
nobody can measure from a preview without noticing.

**8. Do not write a third overlap implementation.** See the review below.

**9. `Open_*` is the verb for the round trip — no fourth verb needed.**
`Open_LifFile.groovy` already establishes `Open_*` as "opens something into the
Fiji GUI for a human to look at", which is interactive-only by nature. That
resolves what was hazard H5. `Inspect_*` covers the read-only count table.

Because the ROI Manager needs a GUI, the *logic* (join, filter, rename) belongs
in a library class and the `Open_*` script stays a thin caller — same reasoning
as `NucleusPipeline` vs `Run_NucleusSelector`, and the only way any of it gets
tested.

**10. No S4 container, but the thing behind the request is worth having.**
A validated constructor returning an S3-classed list of the three tables, with
the join contract asserted once at construction, gets the actual benefit — one
object, keys checked in one place — at a fraction of S4's ceremony. The repo's
idiom is tables and functions; S4 would be the only object system in it. Revisit
only if the S3 version proves insufficient.

## The identity problem (milestone V)

**This is not only a naming drift. One column currently holds two different
compositions depending on which entry point ran.**

Measured in the code:

- **Batch**: `BatchRunner` passes the sheet's `prefix` as `basename`, and
  `basename` is what `saveOutlineCoords()` writes into `name`. So
  `name` = the sheet's per-series id, with **no** `output_prefix`.
- **Interactive**: `NucleusPipeline` falls back to
  `(p.output_prefix ?: "") + RX.resolveImageId(imp, ...)`. So
  `name` = `output_prefix` **+** an id dug out of the slice label.

The fixture is from the interactive path, which is why its `name` reads
`GRV_Position010` while a batch run would write `Position010` (or
`alias_s0000_series`).

The docs disagree with each other too. `CLAUDE.md`'s output contract writes
`<prefix><image id>_<feature>_outline.txt`, making "image id" the per-series
part; `note/data_formats.md` says the outline `name` column **is** "the image
id". Both cannot be right while the two entry points differ.

So there are three concepts and they need three words:

| concept | today | proposed |
|---|---|---|
| the operator's output tag (`GRV_`) | `output_prefix` | unchanged — already unambiguous |
| the per-series identity (`Position010`, `alias_s0000_series`) | `samples.tsv.prefix`, `basename` | **`image_id`** |
| what is actually written into `name` / called `sample` in R | `name`, `sample` | **`image_id`** — i.e. make it the same thing |

**Recommendation: `image_id`, and make the third concept *be* the second.**
Stop baking `output_prefix` into the identity. Then the join to `samples.tsv` is
direct with no prefix to strip, and `--output_prefix` goes back to doing only
what its name says — prefixing filenames.

Why `image_id` and not the alternatives:

- `sample` is actively misleading. The ovary cross-section data has many series
  per biological sample, which is the confusion that prompted this.
- `prefix` names a consequence (it ends up at the front of filenames), not the
  identity, and it already means `output_prefix` elsewhere — two meanings for
  one word.
- a bare `id` invites "id of what" in a repo that already has `feature_id`,
  `roi`, `parent_feature_id` and `run_id`.
- `image_id` is the word the prose docs already reach for.

The cost is the honest part: adopting it means the interactive path stops
writing `output_prefix` into `name`, which **changes an output file**. That is a
reference-run-and-diff change, and the diff is expected to be non-empty — the
one case where `CLAUDE.md`'s "re-run a reference and diff" is used to confirm a
change rather than to confirm the absence of one.

Migration: accept `prefix` in a sheet as a deprecated alias for `image_id` and
warn. Loud, not silent, and cheap.

If the behaviour change is unwanted, the fallback is to name the third concept
`output_id` and keep it distinct — but then every join has to know the prefix,
which is what `.cli_parse_contract()`'s greedy-prefix bug was about.

## The S3 container (milestone S3)

**One element per grain, and the constructor asserts each grain.** Mixing grains
is what produces the duplicated-row failures this repo keeps meeting; four
grains, four elements.

```r
structure(
  list(
    image   = <tibble>,  # one row per image_id
                         #   from _config.txt: pixel_width/height/depth,
                         #   image_width/height, VERSION, open_mode,
                         #   and where t came from (frames vs sheet column)
    roi     = <sf tbl>,  # one row per (image_id, roi)
                         #   t, z, geometry, area, feature_id, feature_type
    measure = <tibble>,  # one row per (image_id, roi, ch)
                         #   the _res.txt measurements
    feature = <tibble>,  # one row per (image_id, feature_id)
                         #   t, feature_type, parent_feature_id, track_id,
                         #   z_span, containment, match_kind,
                         #   per-channel stats
    track   = <tibble>,  # one row per (image_id, track_id)
                         #   n_frames, t_first, t_last, split/merge flags
    meta    = <list>     # run_id (run_id_from() already exists), input
                         #   fingerprint, join report, dropped columns
  ),
  class = "feature_set"   # name open
)
```

What the constructor checks, once, in one place — this is the whole point:

- no duplicate keys within a grain (each element really is the grain it claims)
  — this is the assertion that would have caught the `feature_id` collision
- every `measure` key exists in `roi` — the join that "can be perfectly correct
  while matching nothing"
- every non-`NA` `roi$feature_id` exists in `feature`
- row counts reported, not assumed

**S3 rather than S4**, and rather than nothing:

- a plain list, so `$` access keeps working and nothing downstream has to change
  on day one — the CLIs can build one and ignore it at first, which is what
  makes it adoptable incrementally instead of as a rewrite
- no methods required up front; `print.feature_set()` and `[.feature_set` can
  arrive later without committing to them now
- S4 would be the only object system in a repo whose idiom is tables and
  functions, and its validity machinery buys little that the constructor above
  does not

The object is **multi-image** — `image_id` is a column in every element — because
that is already how the CLIs work (read many files, bind, aggregate). One object
per image would push the binding back onto every caller.

## Identity: what is unique, and in what scope

The scrutiny pass found the plan's largest hole: `feature_id` and "the object
through time" were the same word, which broke three things at once. This section
is the fix.

### Two identities, because they are two things

| | means | scope | produced by |
|---|---|---|---|
| `feature_id` | one object **at one timepoint** | unique within an `image_id` | `define_feature_group()` |
| `track_id` | one object **through time** | unique within an `image_id` | M3 (TrackMate) |

Conflating them is what made the collision invisible. A nucleus at t=0 and the
same nucleus at t=1 are two `feature_id`s and one `track_id`.

### Uniqueness is by composite key, not by longer strings

`feature_id` does **not** need to be unique across images, and should not be
made so by pasting `image_id` into it. The key is `(image_id, feature_id)`;
that is how tables work, and embedding the image id would duplicate a column
into every value and make ids unreadable for no gain. The S3 container's
constructor asserts the composite key, which is where the guarantee belongs.

### The numbering fix

`define_feature_group()` numbers with
`paste0(feature_prefix, seq_along(valid_feature_group))`
([define_feature_group.r:410](../scripts/R/define_feature_group.r)) — from 1,
per call. Partition by `t` and call it per partition and every timepoint emits
`nucleus_1`, `nucleus_2`, …

**Number globally within an image, across all timepoints, in one call that
partitions internally.** Then uniqueness needs no `t` segment in the string,
because `t` is already a column on every row.

- `feature_id` = `<feature_type>_<NNNN>`
- `track_id`  = `<feature_type>_track_<NNNN>`, `NA` when unlinked — orphans are
  kept, same reasoning as `relate_features.r` keeping an orphaned nucleolus
  because it is evidence about detection.

Padding to four digits is a deliberate small break from today's `nucleus_1`:
unpadded ids sort `nucleus_10` before `nucleus_2`, and the repo already has a
standing decision for fixed four-digit padding on the sample prefix. Accepted
because the output contract may break at this stage.

`feature_stats.r`'s validity test
`startsWith(feature_id, paste0(feature_type, "_"))` keeps working under both
ids, which is what makes this a rename-and-renumber rather than a redesign.

### On `track_id_alias` — recommend not having one

The proposal was to keep `nucleus_1` as a human-friendly alias of `track_id`.
**An alias is only worth its cost when the canonical id is unreadable**, and
`nucleus_track_0007` is not. An alias column is a second thing to keep in sync,
against a repo rule that a fact belongs in one place and duplicating it
guarantees drift.

Revisit only if a *stable* id is later needed — one that survives re-running
with different parameters, which would have to be content-derived (a hash, like
`run_id_from()`) and genuinely unreadable. Then an alias earns its place.

## Linking: TrackMate, verified

Outsourcing agreed. What follows was measured, because the obvious integration
does not exist.

**There is no CSV spot importer in the bundled TrackMate 7.14.0.** It ships
`fiji.plugin.trackmate.io.CSVExporter` (export only) and `TGMMImporter`. The
`TrackMate-CSV` importer is a separate update site. So "hand TrackMate a table
of centroids" is not the path.

**The path that works: build a `SpotCollection` and call the tracker directly.**
No image, no detection, no new dependency. Verified end to end on synthetic
spots — two objects drifting over three frames:

```
tracker ran OK: vertices=6 edges=4
  link: A_t0 -> A_t1    link: A_t1 -> A_t2
  link: B_t0 -> B_t1    link: B_t1 -> B_t2
```

via `new SpotCollection()`, `new Spot(x, y, z, radius, quality, name)`,
`sc.add(spot, t)`, `sc.setVisible(true)`, then
`SparseLAPTrackerFactory().create(sc, settings)` and `tracker.getResult()`.

Available settings, from `getDefaultSettings()`:

```
LINKING_MAX_DISTANCE          GAP_CLOSING_MAX_DISTANCE    MAX_FRAME_GAP
ALLOW_GAP_CLOSING             ALLOW_TRACK_SPLITTING       ALLOW_TRACK_MERGING
SPLITTING_MAX_DISTANCE        MERGING_MAX_DISTANCE        BLOCKING_VALUE
LINKING_FEATURE_PENALTIES     GAP_CLOSING_FEATURE_PENALTIES
SPLITTING_FEATURE_PENALTIES   MERGING_FEATURE_PENALTIES
ALTERNATIVE_LINKING_COST_FACTOR   CUTOFF_PERCENTILE
```

**`ALLOW_TRACK_SPLITTING` is the reason this is the right call.** Nuclei divide,
and division is a split in the track graph. A bespoke overlap linker would need
that as a special case; here it is a setting. Gap closing likewise covers the
frame where detection drops an object — the failure mode hazard H2 describes.

Because TrackMate is a Java library, this lives on the Groovy side, which makes
it a `Make_*` step by the repo's existing verb contract: reads a table, writes a
table the pipeline consumes. R stays the place features are defined; Groovy
stays the place Fiji libraries are called.

Open: does the tracker want centroids in calibrated units (it should — the
`Spot` constructor takes doubles and radius, and distances are in those units),
and does `z` go in as the calibrated z or as the slice index? Must be settled
with a real dataset before trusting `LINKING_MAX_DISTANCE`.

## Review: `scripts/R/find_overlap_roi_features.r`

Asked for directly. 303 lines, used by `PLA_analysis/test_PLA.R` and
`tests/testthat/test-spatial.R`, by no CLI. Verdict: **the geometry is sound;
the scoring is why it never felt satisfying.** Five findings, roughly in order
of how much they matter.

**R1 — the denominator is symmetric, so the score is not a consistent
quantity.** `min_area = pmin(area_1, area_2)` and
`int_ratio = int_area / min_area`. For nucleolus-in-nucleus the child is always
the smaller, so this is accidentally "fraction of child inside parent" and
behaves. For two features of *similar* size — which is exactly the same nucleus
at t and t+1 — `pmin` picks whichever happens to be smaller on that slice, so
the denominator flips from pair to pair and the ratio stops being comparable
across pairs. `relate_features.r` fixes this by always dividing by the child's
area. **This is the core defect and the reason to reuse that core rather than
this one.**

**R2 — `min_ratio_roi_overlap = 1` is all-or-nothing.**
`ratio_roi_ovl = pmin(n_roi_1_ovl, n_roi_2_ovl) / min_roi`, so with the default
every ROI of the smaller feature must overlap. One failing slice kills the whole
link. The test block at the bottom probing `min_intersect_ratio=1` then
`0.99999` is the symptom of a threshold with no usable middle.

**R3 — the output throws away the evidence.** It returns only
`feature_id_1, feature_id_2`. `int_ratio` and `ratio_roi_ovl` — the numbers you
would need to judge a marginal link or tune the cut — are computed and
discarded. Compare `relate_features.r`, which keeps containment and records
`direct` vs `gap_filled`.

**R4 — no one-to-one resolution.** A feature can pair with several and nothing
says which is best. Correct for containment (many nucleoli in one nucleus),
wrong for tracking, where you usually want at most one predecessor and want
competing candidates recorded rather than silently both returned.

**R5 — it is unsafe on a time-aware table today.** `z_1 == z_2` is the only
partition, so two ROIs at the same z in *different* timepoints would be treated
as overlapping. Not a current bug; it becomes one the moment a `t` column
exists, which makes it a migration hazard rather than a latent one.

Performance, lower priority but relevant to the wrapper: `st_intersects(sparse=FALSE)`
builds a dense `nrow(F1) x nrow(F2)` logical matrix; the `z_1 == z_2` filter is
applied *after* the geometric predicate rather than before it; and the
per-pair loop does a linear `F1_df$roi == ...` scan inside the loop. Fine at
70x70, quadratic-on-quadratic across four timepoints.

Also: `reframe()` is used where every expression is scalar and `summarise()` is
meant, and the `if(F){...}` block carries a hardcoded
`/Volumes/pool-toti-imaging/...` path while being the only documentation of
intended use.

**So M3 extracts `relate_features.r`'s containment core and gives it: a
directional denominator, retained scores, one-to-one resolution with ties
recorded, and partition keys (`t`) as an argument.** `find_overlap_roi_features()`
stays where it is for `PLA_analysis/`, or is retired once the new utility
covers its test.

## M4 — the inspection scripts, as specified

Both feasible; nothing here needs a capability the repo lacks.

`Inspect_AnnotatedFeatures.groovy` — inputs: sample sheet, R feature table, ROI
directory. Side effect: none, prints `filename`, `series_name`, `series_index`,
`n_<feature>`. All three inputs are text; the join is
`feature.sample` <-> `samples.prefix` (the R `name`/`sample` column *is* the
prefix — confirm at implementation, it is the one thing that could surprise).

`Open_AnnotatedFeatures.groovy` — same inputs plus a sample selector (one
number or name, default 1, `persist=false`) and an optional feature filter
(blank = all). Opens the series via `BatchRunner`'s existing open path, loads
the zip with `RoiExport.loadRoiZip()` (already headless-safe and already
restores names), filters, renames with the `feature_id` prefix, adds to the ROI
Manager.

`Open_SampleSheetRow.groovy` — open row N of a sheet.

- **1-based row number.** It is a table row, and every tool the user reads the
  sheet with is 1-based. `series_index` stays 0-based because Bio-Formats owns
  it. The dialog label must say which is which.
- **Does not honour `include`** — agreed, because the reason to open a row by
  hand is often to decide whether to include it. Two consequences: the row
  number must index the **unfiltered** sheet, or row N means different things in
  different runs; and the script should *print* the row's `include` value so an
  excluded row cannot be mistaken for an included one.

## Remaining open questions

1. Does the assembly manifest get a `schema/` entry like `sheet_columns.tsv`, or
   stay internal to the gatherer? A `schema/` entry means one source of truth
   read at run time by both languages, which is the repo's stated preference.
2. ~~Linking rule: overlap or nearest centroid?~~ **Resolved:** outsourced to
   TrackMate's LAP tracker, which does both cost models properly and handles
   splitting and gap closing. See "Linking: TrackMate, verified".
3. Does time-linking belong in `annotate_features_cli.r` or its own CLI?
4. Storage: conversion doubles 33 GB on the external SSD. Convert only
   `include=true` rows? Keep `raw/` afterwards?
5. ~~Does anything downstream read `res_df$z`?~~ **Resolved: no.**
   `summarise_feature_stats()` requires only `roi, ch, mean` from `res`
   ([feature_stats.r:128](../scripts/R/feature_stats.r)) and its `keep` vector
   discards everything else; `z` comes from the outline/feature side. Confirms
   decision 4's "no `z` in `_res.txt`".

## Hazards carried forward

**H1 — `roi` column collision on join.** See decision 4.

**H2 — a pre-grouping filter can split one object, and time makes it worse.**
Already true in z (`CLAUDE.md`). Across t the same mechanism removes a frame and
breaks a track, which is harder to see than a split object.

**H3 — z varies per position in the Luxendo data** (16..39, one at z=1). `z=1`
must produce a blank `pixel_depth`, and `3c 1z 4t` is the worst case in the
decision-4 table.

**H5 — `feature_stats.r` silently keeps only the first timepoint.**
[feature_stats.r:134](../scripts/R/feature_stats.r) dedups on `(roi, ch)` and
[:141](../scripts/R/feature_stats.r) joins `by = "roi"`. With a 3-field ROI id,
`(roi, ch)` repeats across timepoints: the dedup fires, warns, **keeps the
first**, and every per-channel statistic is then computed from t=0 while being
reported as the feature's. `keep` gains `t`, the dedup key becomes
`(roi, ch, t)`, the join becomes `by = c("roi", "t")`. **This is a stronger
argument for `TTTT-` than the zip one in decision 5** — the zip fails loudly,
this one warns and continues with wrong numbers.

**H6 — a failed ROI-zip write leaves a partial file.** Verified: `saveRoiZip`
with three identical names throws
`ZipException: duplicate entry: nucleus_0001-0001-0433.roi` **and leaves a
189-byte zip on disk**, which `loadRoiZip` would happily read. Write to a temp
path and rename on success. One line, and it is the repo's own rule that a
plausible-looking wrong artifact is worse than none.

**H4 — channel identity.** `channel_2` has no `channel_description`, and the
`.ims` files permute channel order relative to the directory names. Identity
must come from the manifest, never from an index alone.
