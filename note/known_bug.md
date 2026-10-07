# Known bugs

Open bugs found but not fixed, with the evidence and a proposed fix, so that
whoever picks one up does not have to rediscover it. Remove an entry when its
fix is merged.

All entries below were found on the oocyte dataset (`rnf4_project/oocyte_count`,
DDX4 confocal tile merges): 1 and 2 on 2026-10-05 during a threshold survey, 3
on 2026-10-05 during nucleus-detection tuning. All are present, with identical
code at every site, in **v0.5.1**, **`main` (`bbe02f1`, v0.7.0)** and
**`time_axis-stream` (`0306396`)**. Line numbers below were taken from
`time_axis-stream` and match the other two.

**Re-checked 2026-10-07 on `time_axis-overview` (`8ad4b05`, PR 3b): all three
still present.** No PR since has touched `RoiDetect`, `NucleolusDetect` or the
dialog's `choices`; every cited line is where it was, except
`BatchRunner`'s `catch (Throwable)`, now line 675. Bug 1 was re-measured on the
bundled plugin (below); bug 3 was not re-run, its code being unchanged.

---

## 1. Auto-threshold methods give a wrong threshold on large stacks — silently

**Severity: high when it applies.** The mask is wrong, the run completes, and
nothing warns. It only applies to an auto method (anything but `Manual`) on a
large stack; the oocyte dataset uses `Manual` and is not affected.

### Context

`RoiDetect.buildMask()` thresholds the nucleus channel with Fiji's
Auto_Threshold plugin (`fiji.threshold.Auto_Threshold`, **1.18.0** as bundled
here), through `exec()`:

- `nucleus_stack_histogram = true` (the default): `RoiDetect.groovy:241`,
  `exec(..., doIstackHistogram=true)` — one histogram pooled over every slice.
- `false`: `RoiDetect.groovy:212`, `exec()` once per slice.

### Mechanism (two overflows, both in the plugin)

1. **The methods accumulate in `int`.** In the plugin source (1.18.0,
   `src/main/java/fiji/threshold/Auto_Threshold.java`):
   - `Huang`: `int sum_pix; ... sum_pix += ih * data[ih];`
   - `IsoData`: `int ... l, toth, h; ... l = l + (data[i] * i); h += (data[i]*i);`
   - `Li`: `int num_pixels, sum_back, sum_obj, num_back, num_obj;`

   These overflow once Σ value × count exceeds 2³¹−1 ≈ 2.1 × 10⁹, e.g. 10⁹
   voxels at a mean grey value of about 2. A blurred tile merge is
   10⁸–10⁹ voxels per stack. (Upstream already widened `Mean()` to `long`;
   these three were not.)

   **Re-measured 2026-10-07**, the statics called directly on a synthetic
   256-bin histogram of 10⁹ voxels (a dark peak at 3, a dim tail around 90;
   Σ value × count = 3.2 × 10¹⁰), against the same counts divided by 256 and
   by 1024:

   | method | real counts | ÷256 | ÷1024 |
   |---|---|---|---|
   | Huang | **1** | 27 | 27 |
   | IsoData | **−1** | 50 | 50 |
   | Li | **0** | 27 | 27 |
   | MinErrorI | **32** | 8 | 8 |
   | Mean, Otsu, Triangle, Yen, IJDefault | unchanged | | |

   So **MinError(I)'s own static overflows too** — which settles the open
   question below for overflow 1, though not whether s0017 also hit
   overflow 2 — and IsoData can return −1, which `lo = t + 1` would turn into
   "everything from 0".
2. **The stack histogram itself is `int[]`.** `exec()` sums each slice's
   `getHistogram()` into an `int[] data` (confirmed in the 1.18.0 bytecode:
   `newarray int`, `iaload`/`iastore`, no `long` anywhere in `exec`), so a single bin overflows once more
   than 2³¹ voxels fall in it. Reached by the largest stacks here: s0017 has
   2.39 × 10⁹ voxels, most of them near 0 after the blur.

### Evidence

A survey script mirrored `buildMask` up to the threshold (DDX4 channel, per
plane `GaussianBlur.blurGaussian(ByteProcessor, 8 px)`, one pooled histogram,
end bins zeroed as `IGNORE_BLACK`/`IGNORE_WHITE`) and ran every method on
three versions of the same histogram:

- **safe:** scaled to a 2²² total, so no sum can overflow;
- **half:** 2²¹, to see whether rounding alone moves the threshold;
- **full:** the real counts, i.e. what `exec()` sees.

**Oracle:** on one series small enough to hold whole (WT_ovary2_s0051, 3.4 ×
10⁸ voxels), `exec()` on the actual blurred stack agreed with **full** for 16
of 17 methods (Intermodes off by 1). So **full** is what a batch run would
have used.

Threshold `t` (the selected range is `t+1`–255), where full departs from safe
and half agrees with safe:

| series | voxels | method | safe | half | **full = batch** |
|---|---|---|---|---|---|
| Rnf4_KI_ovary1_s0005 | 1.01 × 10⁹ | Huang | 7 | 7 | **1** |
| | | Li | 14 | 14 | **0** |
| | | IsoData | 102 | 102 | **95** |
| Rnf4_KI_ovary1_s0017 | 2.39 × 10⁹ | Huang | 6 | 6 | **1** |
| | | Li | 6 | 6 | **0** |
| | | IsoData | 8 | 8 | **35** |
| | | MinError(I) | 2 | 2 | **5** |

A threshold of 0 or 1 selects essentially the entire image. At 2³⁰, an
earlier pass of the survey already showed Huang, IsoData and Li moving when
the counts were halved, which is the overflow signature.

The survey script and its outputs are in the oocyte project on the SSD, not
in this repo:

- `oocyte_count/embryo_IF_analyzer/sandbox/biapy_trial/Survey_Threshold.groovy`
  (with `07_threshold_survey.sh`), in the v0.5.1 checkout's ignored `sandbox/`;
- results in `oocyte_count/biapy_trial/threshold_survey/thresholds.tsv`.

### Why the tests don't catch it

`Test_BuildMask` and `Test_NucleolusDetect` synthesise small stacks, orders of
magnitude below the counts where either overflow starts.

### Proposed fix

Stop handing the plugin a raw stack. Build the histogram here and call the
per-method static on it:

1. Sum the per-slice histograms into a **`long[]`** (removes overflow 2).
2. Zero the end bins as now (`IGNORE_BLACK`/`IGNORE_WHITE`).
3. If Σ value × count exceeds `Integer.MAX_VALUE`, scale the counts down
   uniformly, **only as far as needed** (removes overflow 1). Do not scale
   further than that. Tail-sensitive methods move by a few levels when the
   histogram is shrunk hard: at 2²² vs real counts on s0051, Yen gave 116 vs
   118, RenyiEntropy 94 vs 98, Minimum 156 vs 198.
4. Run the method through the same static dispatch `NucleolusDetect.landiniBin()`
   uses, then select `lo = t + 1` with the existing `applyRange()`. This also
   drops the reliance on `exec()` thresholding the image in place.
5. Do the same for the per-slice path (`stackHistogram = false`). A single
   0.223 µm plane is ~1.1 × 10⁸ px, close enough to the limit for a bright
   slice.

Record when scaling happened. A scaled threshold is not bit-for-bit what an
unscaled one would be, so a provenance field in `_config.txt` (alongside
`nucleus_threshold_used`) is the honest place. It is provenance, not a
parameter, so it must not enter `PARAM_TYPES`; see the four-way agreement
table in `CLAUDE.md`.

### Verifying the fix

- **Proves the bug and the fix:** feed the histogram path a synthetic histogram
  whose Σ value × count exceeds 2³¹ (no need for a large image). Before the
  fix, Huang/Li/IsoData return the overflowed values; after, they equal the
  same histogram scaled down by a power of two.
- **Proves nothing else moved:** on stacks small enough not to overflow, the
  new path must equal `exec()` for every method (keep `exec()` as the oracle,
  as `Test_BuildMask` already does for the macro call).
- Not yet known: whether the s0017 MinError(I) shift is overflow 1 alone or
  both (its static does overflow on its own — re-measurement above), and
  whether a later Auto_Threshold release fixes either. Check upstream before
  writing a workaround.

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
