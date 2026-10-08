# Known bugs

Open bugs found but not fixed, with the evidence and a proposed fix, so that
whoever picks one up does not have to rediscover it. Remove an entry when its
fix is merged.

**Bug 1** (auto-threshold methods silently wrong on large stacks: `exec()`'s
`int` histogram and the `int` arithmetic of Huang, IsoData, Li and MinError(I))
**was fixed in `time_axis` PR 5** and its entry removed: the threshold is now
chosen by `RoiDetect.chooseThreshold()` from a `long` histogram, with the counts
divided only as far as the method needs. The numbering of the others is kept.

Entries 2 and 3 were found on the oocyte dataset (`rnf4_project/oocyte_count`,
DDX4 confocal tile merges): 2 on 2026-10-05 during a threshold survey, 3
on 2026-10-05 during nucleus-detection tuning. All are present, with identical
code at every site, in **v0.5.1**, **`main` (`bbe02f1`, v0.7.0)** and
**`time_axis-stream` (`0306396`)**. Line numbers below were taken from
`time_axis-stream` and match the other two.

⚠️ PR 5 moved lines in `RoiDetect.groovy` and added one dialog line to
`Run_NucleusSelector.groovy` after the nucleus threshold, so line numbers cited
below for those two files are off by the insertions; search for the quoted code.

**Re-checked 2026-10-07 on `time_axis-overview` (`8ad4b05`, PR 3b): all three
still present.** No PR since has touched `RoiDetect`, `NucleolusDetect` or the
dialog's `choices`; every cited line is where it was, except
`BatchRunner`'s `catch (Throwable)`, now line 675. Bug 1 was re-measured on the
bundled plugin (below); bug 3 was not re-run, its code being unchanged.

---

## 2. `IJ_IsoData` is offered as a threshold method but does not exist

**Severity: low.** It fails loudly and produces no wrong output, but it is a
choice in the dialog that can only ever fail.

### Context and evidence

- `Run_NucleusSelector.groovy:11` (nucleus) and `:20` (nucleolus) both list
  `"IJ_IsoData"` in their `choices`.
- Auto_Threshold 1.18.0 has no such method under any name: its only `IJ*`
  static is `IJDefault`. `IJ_IsoData` is the vocabulary of ImageJ's own
  `ij.process.AutoThresholder` enum, which the nucleolus used before switching
  to the plugin (see `NucleolusDetect.landiniBin()`'s header comment).
- **Nucleus:** `buildMask()` calls `validateThreshold()` first, whose known
  names come from the plugin's statics (`RoiDetect.methodNames()`,
  `RoiDetect.groovy:64`). The run stops with "unknown threshold method".
- **Nucleolus:** `landiniBin()` (`NucleolusDetect.groovy:96–110`) maps only
  `Default → IJDefault` and `MinError(I) → MinErrorI`, finds no static, and
  throws "unknown nucleolus threshold method".
- **The raw plugin call** `exec(..., "IJ_IsoData", ...)` returns a threshold of
  **0 without an error**, which selects everything above 0 (measured on
  WT_ovary2_s0051). The repo never reaches this because of the validation above.
  Any new caller that skips `validateThreshold()` would.

### Proposed fix

- Remove `"IJ_IsoData"` from both `choices` lists.
- Add a **set-difference test**, in the spirit of the run-config tests: every
  entry in the `nucleusMethod` choices must be in `RoiDetect.methodNames()`,
  and every `nucleolusMethod` entry must be in `methodNames()` plus
  `"Relative"`. `Test_RunConfig` already parses the dialog with its `#@` lines
  stripped, which is where the choices can be read. A checklist would not have
  caught this; a set difference would have.

---

## 3. Watershed runs out of heap on a large stack, and the batch reports it as "Macro canceled"

**Severity: medium.** It fails loudly — the row is `failed` and the other rows
carry on — but the recorded reason names the wrong cause, and the option
cannot be used on the largest images at all. Watershed is off by default.

### Context

`RoiDetect.buildMask()` runs ImageJ's binary watershed on the whole mask stack:
`IJ.run(mask, "Watershed", "stack")`, at `RoiDetect.groovy:198` (Manual
threshold), `:233` (per-slice histogram) and `:302` (stack histogram). Stack
mode splits the slices among worker threads — the log names them
`Watershed 1-2`, `Watershed 6-8`, `Watershed 9-11`, … — and each works on its
own slices at the same time.

Everything else in `buildMask()` was made to hold at most one extra slice
(per-slice blur, the in-place mask helpers; see "Nothing holds two whole
copies of the image" in `CLAUDE.md`). The watershed is the one step still run
over the whole stack in parallel.

### Evidence

Tuning variant V3 (= the v0.5.1 oocyte batch config with
`nucleus_watershed = true`; Manual 40-255, blur 8 px), `--mem` default, Fiji
heap **8969 MB**:

| series | size (x × y × z × c, 8-bit) | watershed off | watershed on |
|---|---|---|---|
| `Rnf4_KI_ovary1_s0005` | 7742 × 7649 × 17 × 2 | ok | ok |
| `Rnf4_KI_ovary1_s0017` | 11348 × 9582 × 22 × 2 | ok (every variant) | **failed after 331 s** |

The s0017 log (`tuning/V3/fiji_px223/batch.log`):

```
Exception in thread "Watershed 9-11" Exception in thread "Watershed 17-19" ...
java.lang.OutOfMemoryError: Java heap space
Exception in thread "Watershed 1-2" java.lang.OutOfMemoryError: Java heap space
...
FAILED Rnf4_KI_ovary1_s0017: RuntimeException: Macro canceled
```

and `batch_summary.tsv` records only `RuntimeException: Macro canceled`. The
out-of-memory error is raised in the worker threads, so it never reaches
`BatchRunner.runEach()`'s `catch (Throwable)` (`BatchRunner.groovy:675` since
PR 3a's last commit; 667 before); what
arrives is the cancellation that follows.

The arithmetic: the open image is 11348 × 9582 × 22 × 2 ≈ 4.8 GB and the mask
another 2.4 GB, so ~7.2 GB of the 8.97 GB heap is taken before the watershed
starts. One slice is ~109 Mpx; a float distance map of one slice is ~435 MB, and
several threads each hold at least that. (The per-thread allocation is inferred
from the thread names and the heap left over, not measured.)

### Proposed fix

1. **Run the watershed slice by slice, as the blur already is**: loop over
   `z`, wrap each mask plane in a one-slice `ImagePlus`, `IJ.run(one,
   "Watershed", "")`. Binary watershed in stack mode treats every slice
   independently, so the result should be identical; peak memory drops to one
   slice's buffers. A smaller alternative is to cap the thread count around
   the call (`Prefs.setThreads(1)`, restored in a `finally`), which also bounds
   memory but keeps the whole-stack call.
2. **Make the cause visible.** When a worker thread runs out of memory, the
   batch summary should say so rather than "Macro canceled". One way: install
   a temporary `Thread.setDefaultUncaughtExceptionHandler` around the call
   that records an `OutOfMemoryError`, restore it in a `finally`, and rethrow
   as `IllegalStateException("watershed ran out of heap on <w>x<h>x<z>; raise
   --mem or turn watershed off")`. Untested idea — confirm the handler sees the
   ImageJ worker threads' errors before relying on it. Fix 1 removes this case;
   fix 2 is for the next step that does the same.

### Verifying the fix

- `Test_BuildMask` already checks watershed on a synthesised stack. Keep the
  whole-stack `IJ.run(mask, "Watershed", "stack")` as the **oracle** and assert
  the per-slice result is pixel-identical, on a stack with touching discs on
  several slices (so a no-op cannot pass), and assert the stack instance is
  the same object afterwards (in place, as the other mask helpers).
- Real data: rerun `Rnf4_KI_ovary1_s0017` with watershed on at the default
  heap — it must complete — and, on a series that fits either way (s0005),
  diff `_nucleus_outline.txt` and `_nucleus_res.txt` against the whole-stack
  run. Identical files, not identical counts.
- Not verified: whether `Fill Holes` in stack mode (the call just before) has
  the same problem on a larger image; it held on s0017.

### Note on whether to fix it

On the oocyte data the watershed did not help — it split large growing
oocytes (tuning, 6 WT test sections: 17 split oocytes with blur 4 px +
particle 50 + watershed, against 6 with the same settings and no watershed), and a
shape-based trigger (solidity) could not tell merged oocytes from large single
ones. So the fix matters for the option's other users and for not reporting a
misleading reason, not for the oocyte count.

---

## 4. Moments overflows on the grey level itself, on a wide 16-bit histogram

**Severity: low-to-medium when it applies.** Found 2026-10-08 reading the
Auto_Threshold 1.18.0 bytecode while fixing what was bug 1 (`time_axis` PR 5).
Not measured on real data.

### Mechanism

`Moments(int[])` forms `i * i` and `i * i * i` in `int` (`iload; iload; imul`,
then `i2d`), where `i` is the bin index of the histogram the method is handed,
the one `RoiDetect.chooseThreshold()` has already trimmed to the occupied range.
`i³` passes 2³¹ − 1 once the trimmed histogram spans more than **1,290 grey
levels**, and `i²` once it spans more than 46,340. An 8-bit histogram can never
reach that; a blurred 16-bit stack easily can.

It is an overflow in the grey level, not the counts, so dividing the counts,
which is what fixed bug 1, does not reach it. `exec()` has the same overflow, so
results before PR 5 were affected the same way.

### Proposed fix

Either compute Moments' sums in `double` in our own code, which means
reimplementing one method and checking it against the static on narrow
histograms, or bin a wide 16-bit histogram down before calling Moments, which
changes the answer by up to a bin width. Or refuse Moments on a trimmed
histogram wider than 1,290 levels, which is loud and cheap. Check upstream first:
a later Auto_Threshold may have widened it, as it did `Mean()`.
