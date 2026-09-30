# Plan: time-series support, driven by the Luxendo case

**Status: design. Nothing implemented.**

Scrutinised end to end, then reviewed in line twice; every round is folded in
rather than appended, so the decisions below are current rather than a history.
Three shaped the plan as it now stands:

- `feature_id` and "the object through time" were the same word — now
  `feature_id` and `track_id`, at different scopes.
- the container question was reopened and settled for **classic TIFF and
  drag-and-drop**, accepting a `position_id` axis as the price.
- the identity column is **`series_id`**, and `samples.tsv` becomes
  `series.tsv`.

Working note for tracking progress and holding decisions that must survive a
context compaction. **Delete when all milestones land — but migrate the surviving decisions first.**
Default target is `note/data_formats.md` (shapes) or
`note/luxendo_file_format.md` (that format's facts). `CLAUDE.md` only for the
few that are standing hazards someone must know *before* touching the code —
most of what is here is situational and would just make that file longer.

Format facts about the input live in `note/luxendo_file_format.md`, not here.

## Goal

Make the repo able to ask time-resolved questions — how one nucleus's volume,
cross-sectional area, circularity or signal intensity changes over time —
rather than analysing each timepoint as an unrelated image.

**The driver is the shape of the data, not this dataset.** Luxendo TruLive3D is
the first instance to arrive; anything with a time axis should fall out of the
same design. Nothing here should be written so that it only works for a
TruLive3D.

The alternative considered and rejected: treat each (position, timepoint) as an
independent sheet row and change nothing in the output contract. Cheaper, gives
per-timepoint statistics, but cannot link a feature to itself across time, so
every time-resolved question stays out of reach. The linking is the point.

⚠️ **One file per timepoint means time lives *between* rows, not inside a
file** — so the series table needs `position_id` and `t` to say which rows form
one time course. That axis is now a decided part of the design; see
"`position_id`: decided, we keep it".

## Sequence

Agreed order, with the reason each sits where it does. Numbers are the order of
work, not of importance.

| # | work | why here |
|---|---|---|
| **1** | **M1** Luxendo transform | urgent — a user is reading `.lux.h5` by hand today. Independent of the contract. |
| **2** | **V** identity/vocabulary unification | before any *other* schema change: M2 changes the schema too, and two migrations is worse than one. M4 and the S3 both key on this column. |
| **3** | **M2** time axis | the contract decisions. |
| **4** | **M4** inspection scripts | small, and they give a way to *eyeball* M2's output — worth something at the verify step. |
| **5** | **M3** linking across time | needs `t`. Much smaller than first scoped — TrackMate does the linking. |
| **6** | **S3** container | last, deliberately: see the shape of a working pipeline before building a container for it. |

Two constraints that cut across the order:

- **Do not run the full 33 GB conversion until M2's contract is settled**, or it
  gets done twice. M1 can be *written* and tested on a few positions first.
- **M1's manifest should use `series_id` from day one**, anticipating V, so that V
  does not have to revisit it. M1 is otherwise unaffected by V because it writes
  its own table and feeds `Make_SampleSheet` unchanged.

✅ **Resolved: S3 comes after M3**, as the table above now shows.

The reason is *not* the one the scrutiny pass gave. It argued from the
`BatchRunner.runEach()` precedent — extracted because a second caller appeared —
but that is a weak analogy: nothing was anticipated back then because nothing
was planned that far ahead, whereas here there *is* a goal and thinking about
the problem up front is the point. The precedent describes how that extraction
happened to occur, not a rule against planning.

The real merit is narrower and still decisive: **see the shape of a pipeline
that works before building a container for it.** Writing M3 against plain tables
costs little, and whatever the container ends up holding will be what M3 turned
out to need rather than what it was guessed to need. That is over-engineering
avoidance, not precedent-following.

## Milestones

### M1 — Luxendo input transform

- [ ] Read `.lux.h5` via JHDF5 (`ch.systemsx.cisd.hdf5`), not Bio-Formats.
- [ ] Build an assembly manifest from a Luxendo run folder: one row per
      **output file**, naming every source file and plane that feeds it.
      Identity column named `series_id` from the start.
- [ ] Generic assembler: manifest -> TIFF, one file per (position, timepoint)
      by default; `--format bigtiff` as an explicit alternative.
- [ ] Predict the output size from the manifest and warn **before** writing —
      see "The 4 GB rule, precisely".
- [ ] `setCanDetectBigTiff(false)` always, so the chosen format is the written
      format.
- [ ] **Three entry modes, agreed:** (i) end to end from a Luxendo directory,
      (ii) **manifest only** — so a look at the plan costs nothing before
      committing to 33 GB, (iii) assemble from an existing manifest. (ii) is the
      one that earns the manifest its keep.
- [ ] Per-output provenance record (source paths, checksums, gatherer version).
- [ ] Honour `include`, with exactly `BatchRunner.isIncluded()`'s vocabulary.
- [ ] Verification mode: re-read output planes, compare to source by checksum.
- [ ] Assert z uniform across timepoints of one position; it is NOT uniform
      across positions (16..39, one position at z=1).

### V — identity and vocabulary unification

See "The identity problem" and "Identity: what is unique, and in what scope"
below. This is not only a rename; it is the one schema migration, so the
`series_id`, `feature_id` and `track_id` decisions all ride together.

`position_id` joins them: it is what makes several series one time course —
see "`position_id`: decided, we keep it".


- [ ] `series_id` as the one word; `sample` and `prefix` both retired.
- [ ] Make both entry points write the same thing into it.
- [ ] `samples.tsv` -> `series.tsv`; `prefix` dropped outright, no alias.
- [ ] `position_id` and `t` added to the series table.
- [ ] `feature_id` numbered globally within a series.
- [ ] `track_id` reserved in the schema, keyed on `(position_id, track_id)`;
      M3 populates it.
- [ ] `schema/sheet_columns.tsv`, `note/data_formats.md`,
      `tests/testthat/test-data_formats.R`, R CLI flags, `config/*template*`.

**Kept against the review's advice, deliberately.** The scrutiny pass argued V
does not advance the goal and changes an existing output file for a naming
preference. Overruled on the grounds that the repo has one user and few analysed
datasets, so the break is cheap now and expensive later, and that the confusion
`sample` causes is already real on the ovary cross-section data. Recorded so the
trade-off is visible rather than forgotten.

### M2 — time axis through the pipeline

*Agreed: the milestone that matters most; everything else is either input to it
or built on it.*

- [ ] Groovy: frame loop in `RoiDetect` / `NucleusPipeline`.
- [ ] `t` column in `_outline.txt`.
- [ ] `roi`, `z`, `t`, `ch` as **explicit columns** in `_res.txt` (decision 4);
      `z` is for human eyes and is safe because R takes `res` through an
      allow-list.
- [ ] ROI id gains `TTTT-` when frames > 1 (decision 5) — forced by the ROI zip.
- [ ] R: `t` honoured in grouping; absent `t` means one frame.
- [ ] Join on `(roi, t)` (decision 4).

### M4 — inspection round trip

- [ ] `Inspect_AnnotatedFeatures.groovy` — read-only feature-count table.
- [ ] `Open_AnnotatedFeatures.groovy` — one image's ROIs into the ROI Manager.
- [ ] `Open_SampleSheetRow.groovy` — open row N (1-based, unfiltered) of a sheet.

### M3 — linking features across time

**Outsourced to TrackMate.** See "Linking: TrackMate, verified" below.

- [ ] `Make_FeatureTracks.groovy`: feature centroids per (series_id, feature_id,
      t) -> TrackMate LAP tracker -> `tracks.tsv` (series_id, feature_id, t,
      track_id).
- [ ] R joins `tracks.tsv` onto the feature table by (series_id, feature_id).
- [ ] Record the tracker settings used, the same way `_config.txt` records
      everything else.
- [ ] *Low priority:* a primitive in-house overlap linker as a fallback and a
      cross-check. Not on the critical path now.

### S3 — the container

See "The S3 container" below.

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
gathered across time). Label parsing and stack-position columns are *both* unreliable. 
`measureRois()` already knows the true `ch`, `slices[i]` and (once
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

**`z` IS written, for eyeballing — and it costs nothing.** An earlier draft said
to leave it out, on the grounds that nothing on the measurement path groups by z
and a duplicated `z` would collide on the join. The first half is true; the
second turns out not to be a problem, because `summarise_feature_stats()`
already subsets `res` through an **allow-list**:

```r
keep <- c("roi", "ch", "mean", intersect(c("median", "circ"), colnames(res_df)))
```

Anything else in `_res.txt` is discarded before the join ever happens. So `z`
can sit in the file for a human reading it, with no join risk at all. Write it.

**But "it happens to be safe today" is not a mechanism, so give it one.**
`keep` is an allow-list inside one function; a second reader of `_res.txt`
joining it to outlines would hit `z.x` / `z.y` with nothing to stop it.

Of the two options — make `z` a join key, or let the outline table win — take
the second, **with an assertion in front of it**:

1. the **outline table is authoritative** for `z`. It is written per ROI from
   `slices[i]`, and it is the table the geometry is built from.
2. before dropping the measurement table's `z`, **assert the two agree** on the
   overlap. They are written in the same Groovy loop from the same variable, so
   a disagreement is a code bug, and a code bug should be loud.
3. **report the drop**, the way `.cli_read_sample_sheet()` already reports the
   columns it drops.

Assert-then-drop beats using `z` as a join key, which would also catch the
disagreement but by *dropping rows* — the failure would show up as a quietly
short table rather than as a message naming the column. It beats silent
dropping, which catches nothing.

Worth making it a small shared helper rather than a line in one function, since
`t` will want the same treatment the moment a second table carries it.

The rule this generalises to: extra columns in `_res.txt` are free **provided
the reader states which table wins**, because the R side takes what it names
rather than everything it finds.

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

**6. Container: one file per (position, timepoint), classic TIFF by default,
BigTIFF as an explicit choice.** Drag-and-drop into Fiji is worth more than the
axis it costs, so the default stays the format every tool opens without
thinking. BigTIFF is verified to work and is available by request; Zarr and N5
are not (see the format note).

The size rule is now exact rather than approximate — see "The 4 GB rule,
precisely" — and the assembler predicts it from the manifest before writing
anything.

**7. One assembler with a resize option — not two entry points.** Reversed from
an earlier draft, and the repo's own rule is the reason: *"If a new assay needs
behaviour the library lacks, add the option to the library — do not fork a
script."* Two gatherers differing only in scale is a fork.

So: `resize` (non-persistent, default off) and `gatherFrames` as independent
options, exactly as proposed.

The safeguard moves from "separate functions" to three cheaper things, and the
third is the one that actually matters:

- the resize factor is recorded in the per-output provenance record;
- a non-default resize puts a token in the output filename, so a downsampled
  file cannot be mistaken for full-res at a glance;
- ⚠️ **resizing must scale the calibration.** Halve the pixels and
  `pixel_width`/`pixel_height` must double. Get it wrong and every area is out
  by 4x while the image looks perfect — the exact silent failure this repo is
  built around. This is the real risk in merging the two, and it is a test, not
  a comment: assemble the same position at 1x and 2x and assert the physical
  extent matches.

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

*Agreed on both counts: one output file per image series is right — detection is
expensive and a long run must not risk one big file — and the identity must
travel as file **content**, not as a filename, with the two runners writing the
same thing. That second half is what this milestone fixes.*

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
| the per-series identity (`Position010`, `alias_s0000_series`) | `samples.tsv.prefix`, `basename` | **`series_id`** |
| what is actually written into `name` / called `sample` in R | `name`, `sample` | **`series_id`** — i.e. make it the same thing |


**Recommendation: `series_id`, and make the third concept *be* the second.**
Stop baking `output_prefix` into the identity. Then the join to `samples.tsv` is
direct with no prefix to strip, and `--output_prefix` goes back to doing only
what its name says — prefixing filenames.

Why not the alternatives:

- `sample` is actively misleading. The ovary cross-section data has many series
  per biological sample, which is the confusion that prompted this.
- `prefix` names a consequence (it ends up at the front of filenames), not the
  identity, and it already means `output_prefix` elsewhere — two meanings for
  one word.
- a bare `id` invites "id of what" in a repo that already has `feature_id`,
  `roi`, `parent_feature_id` and `run_id`.

### ✅ Decided: `series_id`

`series_id` was the earlier recommendation, on the grounds that after M1 the
analysed unit is a standalone TIFF and "series" imports a container concept.
**Overruled, and the literal argument wins:** a row of that table *is* an image
series, always — `Make_SampleSheet` builds it by enumerating series — so
`series_id` says what the row is rather than what it later becomes.

The objection that three `series_*` names would crowd one table does not hold on
inspection, because they are not redundant:

| column | says | unique within |
|---|---|---|
| `series_id` | **the identity** of this row | the whole series table |
| `series_index` | which series of its file, 0-based as Bio-Formats counts | one file |
| `series_name` | what the file calls it, before sanitising | one file (by design) |

`series_index` and `series_name` are unique *within a file*, which is exactly
why neither can be the identity across a batch — a Leica `Series001` recurs in
every file. `series_id` is the composite that is unique across the table. They
are one family of related facts, correctly named as one.

**The table is renamed with it.** `samples.tsv` becomes `series.tsv`, and the
prose stops calling it "the sample sheet" — it is the series table. `files.tsv`
is unchanged. This touches `schema/sheet_columns.tsv` (the `sheet` column's
values), `SheetSchema.groovy`, `.cli_read_sample_sheet()` and its callers,
`note/data_formats.md`, `README.md` and `config/*template*`.

The cost is the honest part: adopting it means the interactive path stops
writing `output_prefix` into `name`, which **changes an output file**. That is a
reference-run-and-diff change, and the diff is expected to be non-empty — the
one case where `CLAUDE.md`'s "re-run a reference and diff" is used to confirm a
change rather than to confirm the absence of one.

**And it can be checked better than "trust the author".** A byte-identical diff
is impossible, but the *expected* diff is exactly one column: re-run the
reference, then assert that the old and new outputs are identical after dropping
`name`, and that `name` differs only by the `output_prefix` that was removed.
That fails if anything else moved, which is the whole point. The fixture is then
regenerated deliberately, with the commit saying so.

Migration: **drop `prefix` outright, no alias and no warning.** Agreed — the
sample sheet postdates the repo's only user, so there are no third-party sheets
to keep working, and an alias would be a reserved word carried forever for a
migration nobody needs. An old sheet then fails on a missing required column,
which is loud and correct.

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
    series  = <tibble>,  # one row per series_id
                         #   from _config.txt: pixel_width/height/depth,
                         #   image_width/height, VERSION, open_mode,
                         #   and where t came from (frames vs sheet column)
    roi     = <sf tbl>,  # one row per (series_id, roi)
                         #   t, z, geometry, area, feature_id, feature_type
    measure = <tibble>,  # one row per (series_id, roi, ch)
                         #   the _res.txt measurements
    feature = <tibble>,  # one row per (series_id, feature_id)
                         #   t, feature_type, parent_feature_id, track_id,
                         #   z_span, containment, match_kind,
                         #   per-channel stats
    track   = <tibble>,  # one row per (position_id, track_id)   <- NOT series
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
- **the two identity scopes agree** — every `feature` row's `track_id` resolves
  in `track` via the `position_id` its series belongs to. This is the join the
  container exists to make safe, because it is the one that crosses scopes.
- **referential integrity in both directions** — asked for, and yes. Every
  `series_id` appearing in `roi`, `measure` or `feature` must exist in `series`,
  and a `series` row with no ROIs is reported rather than assumed empty. A
  stray `series_id` is the signature of a partial read — one `_config.txt`
  missing from a results directory — and it is exactly the kind of thing that
  otherwise shows up as a quietly short table.
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

The object is **multi-series** — `series_id` is a column in every element — because
that is already how the CLIs work (read many files, bind, aggregate). One object
per series would push the binding back onto every caller.

### Yes, it changes the CLI contract — and that is most of the benefit

Today every CLI re-scans an output directory and re-derives identity from
filenames. With one gathered object, everything after the first
annotation step takes *one* input instead of a directory plus a set of
pattern flags. That removes a whole class of "which files did it actually
pick up" ambiguity.

⚠️ **But persistence must not be `.rds`.** M4's `Inspect_*` and `Open_*` scripts
are Groovy and cannot read an R serialisation, and a format only one of the two
languages can open would undo the thing `schema/sheet_columns.tsv` exists to
guarantee.

**So: TSV is the artifact, `.rds` is at most a cache.** One TSV per grain
(`image`, `roi`, `measure`, `feature`, `track`) plus the constructor that binds
them. There is already precedent for exactly this split —
`feature_scatter_cli.r` deliberately reads `feature_stats.tsv` and not the
`.rds`, so that the expensive step happens once while the exploring end stays
cheap and re-runnable. Same reasoning, one layer up.

## Identity: what is unique, and in what scope

The scrutiny pass found the plan's largest hole: `feature_id` and "the object
through time" were the same word, which broke three things at once. This section
is the fix.

### Two identities, because they are two things

| | means | scope | produced by |
|---|---|---|---|
| `feature_id` | one object **at one timepoint** | unique within an `series_id` | `define_feature_group()` |
| `track_id` | one object **through time** | unique within a `position_id` | M3 (TrackMate) |

Conflating them is what made the collision invisible. A nucleus at t=0 and the
same nucleus at t=1 are two `feature_id`s and one `track_id`.

**Note the scopes differ, and that is forced by the container choice.** With one
file per (position, timepoint), each timepoint is its own series, so a track
spans series and cannot be keyed within one. `feature_id` is unique within a
`series_id`; `track_id` is unique within a `position_id`. See
"`position_id`: decided, we keep it".

### Uniqueness is by composite key, not by longer strings

`feature_id` does **not** need to be unique across images, and should not be
made so by pasting `series_id` into it. The key is `(series_id, feature_id)`;
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

## `position_id`: decided, we keep it

Scrutinised, and the answer came back the other way from the earlier draft.

### The decision

**One file per (position, timepoint), and the series table gains `position_id`
and `t`.** Drag-and-drop into Fiji is the deciding factor: BigTIFF works, but a
format that every tool opens without thinking is worth more than the axis it
costs. The axis is a column; losing drag-and-drop is friction on every single
look at the data.

| column | owner | says |
|---|---|---|
| `series_id` | seeded | identity of this row |
| `position_id` | seeded | which physical field of view — **the thing that is the same across timepoints** |
| `t` | seeded | which timepoint, within that position |

`seeded` rather than `machine`: the gatherer writes them, and a person may need
to correct them for data it did not produce. `(position_id, t)` should be
unique, and a duplicate is worth refusing the same way a duplicate `series_id`
is.

### What it costs, stated plainly

**`track_id` is keyed on `(position_id, track_id)`, not `(series_id, track_id)`.**
A track spans timepoints, and each timepoint is its own series, so a track
genuinely does span series. This is the real price of the decision: the
container carries two identity scopes, and `feature` (keyed within a series)
joins to `track` (keyed within a position) through the series table. It is
a join, not an ambiguity — but it is a join that has to be got right, and the
S3 constructor should assert it.

The alternative was one file per position with frames inside, keeping every key
inside a series. Rejected on drag-and-drop. Recorded so the cost is visible.

### And the axis pays for something else

Per-timepoint files allow analysing t=0 while t=3 is still being acquired, and a
corrupt timepoint costs one file rather than a position. That was the
counter-argument to gathering frames, and it now comes for free.

## The 4 GB rule, precisely

The limit is real but it is not 2^32, and the failure mode is not what it looks
like. Measured in `loci.formats.out.TiffWriter` (Bio-Formats 8.1.1):

**The ceiling is `4,183,818,240` bytes of pixel payload** — 3.896 GiB, reserving
~106 MiB below 2^32 for IFDs and metadata.

⚠️ **And Bio-Formats will silently change the format on you.** `TiffWriter` and
`OMETiffWriter` both default to `canDetectBigTiff = true`, and above the ceiling
they log *"Switching to BigTIFF (by file size)"* and carry on. A run that asked
for classic TIFF would produce a BigTIFF, and the drag-and-drop guarantee this
whole decision was made for would be gone without an error anywhere. There is
also a *"Switching to BigTIFF (by file extension)"* path for `.tf2`/`.tf8`/`.btf`.

**So the assembler must call `setCanDetectBigTiff(false)`.** Then the writer
raises *"File is too large; call setBigTiff(true)"* instead, and the format
written is the format chosen. Same family as forcing `Set Measurements` and
`blackBackground`: a library default must not be allowed to decide what a run
produces.

### Predicting it before writing

Exactly computable from the manifest, no trial write needed:

```
bytes = size_x * size_y * size_z * size_c * size_t * bytes_per_pixel
fits classic TIFF  <=>  bytes < 4183818240
```

For this dataset, at one file per (position, timepoint), every output is about
981 MB — a quarter of the ceiling, with no case near it. Gathering all four
timepoints into one file would still fit, but only just:

```
L26A pos1   z=39   3925868544 bytes   3.656 GiB   fits, margin 6.2%
D17 pos4    z=38   3825205248 bytes   3.563 GiB   fits
L26A pos3   z=1     100663296 bytes   0.094 GiB   fits
```

6.2% is about two and a half more z slices. That thin a margin is itself an
argument for the per-timepoint default: the same acquisition with a slightly
deeper stack would cross the line, and the crossing would have been silent.

### What happens when it does not fit

Warn at manifest time, where the user can still choose. At write time, a series
too large for the chosen format is a **per-row failure recorded in the summary,
not an abort** — exactly `BatchRunner.runEach()`'s contract, where one row's
failure does not cost the other hundred and ninety-nine. The message names the
fix (`--format bigtiff`), and the summary row says why it was skipped, so a
partial run is legible rather than mysterious.

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

*Asked: can the two overlap functions be merged into one? Yes — that is exactly
what the extraction at the end of this section is. One directional-containment
core, two callers (parent/child and frame-to-frame).*

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

**On `reframe()` — the nit was overstated, and the mental model is right.**
Asked directly, so it was tested rather than asserted (dplyr 1.2.1, R 4.6.1):

```
scalar expressions:   summarise -> 2 rows, 1 group   reframe -> 2 rows, 1 group
                      identical values? TRUE
non-scalar (range(x)): summarise -> ERROR
                       reframe   -> 4 rows, silently
```

So "`reframe` is `summarise` with automatic ungrouping" is accurate for scalar
expressions — the outputs are identical. The *only* difference that matters:
`summarise()` **errors** when an expression unexpectedly returns more than one
value, and `reframe()` silently returns extra rows. In a repo whose named hazard
is silent row multiplication, that argues mildly for `summarise()` where the
expressions are meant to be scalar. Mildly. It is not a bug in the existing
code.

Also: the `if(F){...}` block carries a hardcoded `/Volumes/pool-toti-imaging/...`
path. **Confirmed as a live-testing scratchpad — delete it** when the file is
next touched.

**So M3 extracts `relate_features.r`'s containment core and gives it: a
directional denominator, retained scores, one-to-one resolution with ties
recorded, and partition keys (`t`) as an argument.**

`find_overlap_roi_features()` is then **retired**. Two callers must be handled
first: `PLA_analysis/test_PLA.R` sources it, and `tests/testthat/test-spatial.R`
exercises it — the test migrates to the new utility, and the PLA script either
migrates or pins what it needs. Retiring it is the point; leaving a second
overlap implementation in place is how a third one eventually gets written.

## M4 — the inspection scripts, as specified

Both feasible; nothing here needs a capability the repo lacks.

*The S3 work may change what these read, so the inputs below are provisional.
The cross-language friction is already settled in that section: the persisted
artifact is TSV, never `.rds`, precisely so these Groovy scripts can read it.*

`Inspect_AnnotatedFeatures.groovy` — inputs: the series table, R feature table,
ROI directory. Side effect: none, prints `filename`, `series_name`,
`series_index`, `n_<feature>`, and after V also `position_id` and `t` so a time
course reads as a time course. All three inputs are text; after V the join is
`series_id` on both sides, with no prefix to strip — which is most of what V
buys here.

`Open_AnnotatedFeatures.groovy` — same inputs plus a series selector (one
number or `series_id`, default 1, `persist=false`) and an optional feature filter
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

1. ~~Does the assembly manifest get a `schema/` entry?~~ **Resolved: yes.** One
   source of truth read at run time by both languages, as `sheet_columns.tsv`
   already is — and the manifest is read by the assembler and written by the
   Luxendo wrapper, so it has two readers from day one.
2. ~~Linking rule: overlap or nearest centroid?~~ **Resolved:** outsourced to
   TrackMate's LAP tracker, which does both cost models properly and handles
   splitting and gap closing. See "Linking: TrackMate, verified".
3. ~~Does time-linking belong in `annotate_features_cli.r` or its own CLI?~~
   **Resolved: `annotate_features_cli.r`.** It is another annotation on the
   feature table, alongside containment — not a separate product.
4. ~~Convert only `include=true` rows? Keep `raw/`?~~ **Both resolved.** The
   gatherer honours `include` like everything else. And `raw/` — the Luxendo
   acquisition directory, the `.lux.h5` files straight off the camera — **is
   kept, always.** Raw instrument output is never modified or deleted by
   anything in this repo. The conversion doubles the storage and that is simply
   the cost; a converted TIFF is a derived artifact and derived artifacts are
   the ones that may be regenerated.
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

**H3 — z varies per position, and a single-plane position is the sharp edge.**
Rewritten, because the original was too terse to be useful.

`pixel_depth` is the **z step** — the physical distance between slices. It is a
field in `_config.txt` and a column in the series table
(`schema/sheet_columns.tsv` describes it), and the repo has a standing rule
about it: *blank for a single plane, never ImageJ's default of 1.0*, because a z
step that does not exist must not arrive as a usable-looking number. Something
downstream would multiply by it.

So the hazard is two things, not one:

1. **Outputs will differ in dimensions** — z is 16..39 across the Luxendo
   positions, and in general x, y, c and t can differ too. Nothing in the
   assembler or the batch may assume one shape for a run. That is what the
   sheet's per-row `size_*` columns are for, and the batch already tolerates it;
   the gatherer must not undo that by, say, sizing a stack from the first row.
2. **`L26A pos3` has z = 1**, so its `pixel_depth` must be written blank. And
   whichever container choice is made, that position lands on one of the rows of
   the decision-4 table where ImageJ's own columns go missing: `3c 1z 1t` is not
   a hyperstack and reports *the channel* in `Slice` with no `Ch` column at all,
   while `3c 1z 4t` has no `Slice` column. It is the single position most likely
   to break something quietly, and it is worth being the first one tested.

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
