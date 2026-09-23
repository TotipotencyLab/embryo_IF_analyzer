# Quantifying immunofluorescence — working notes

Background for the measurement side of this repo: what the numbers Fiji writes
actually mean, how to turn per-ROI measurements into per-feature ones, and which
of the standard mistakes this pipeline is structurally exposed to.

This is a *reference* note, not a specification. `note/data_formats.md` is
authoritative for the table shapes. Where a claim comes from this project's own
data rather than general practice, it says so.

The IF quantification CLI is deliberately deferred (see `CLAUDE.md`) — the
background question in §3 is why. This note exists so that when it is written,
the decisions are made deliberately rather than by whichever default was easiest.

---

## 1. What Fiji measures

The Groovy scripts force `Set Measurements` explicitly, so the columns do not
depend on the operator's preferences:

```
area mean standard min centroid shape integrated median stack display
```

| Column | Meaning | Notes |
|---|---|---|
| `Area` | ROI area | **calibrated** (µm²) when the image is calibrated |
| `Mean` | mean pixel value in the ROI | the usual "signal" |
| `Median` | median pixel value | robust to specks and hot pixels |
| `StdDev` | spread within the ROI | texture, and a saturation warning sign |
| `Min`, `Max` | extremes | `Max` at the type ceiling (255 / 65535) means **clipping** |
| `IntDen` | Area × Mean | in calibrated units |
| `RawIntDen` | **sum of raw pixel values** | the honest "total signal" |
| `Circ.`, `AR`, `Round`, `Solidity` | shape | `Circ. = 4π·area/perimeter²` |

Two distinctions that cause most of the confusion:

- **`Mean` is a concentration, `RawIntDen` is an amount.** A big dim object and
  a small bright one can share a `Mean` while differing 10× in total signal.
  Which one answers the question depends on the question; say which you used.
- **`IntDen` vs `RawIntDen`.** `IntDen` is in calibrated units and so changes if
  the pixel size changes; `RawIntDen` is a pure pixel sum. For summing across
  ROIs, `RawIntDen` is the safer currency.

---

## 2. From ROIs to a feature

A feature here is a stack of ROIs across z. Collapsing them into one number is
where bias creeps in.

**Area-weight the mean.** A feature's ROIs are not equal: the slices at the top
and bottom of an object are small tapering cross-sections, the middle ones are
large. A plain `mean(roi_means)` gives the tiny end slices the same vote as the
equator, which pulls the result toward whatever the object's edges look like.

```
weighted:    sum(RawIntDen) / sum(Area_px)          <- preferred
equivalent:  weighted.mean(Mean, w = Area)
unweighted:  mean(Mean)                              <- biased toward end slices
```

The two differ most for large objects with a strong z-profile — exactly the
growing oocytes this repo is currently trying to count.

**Median for robustness, mean for totals.** If the question is "how much protein
is here", sum `RawIntDen`. If it is "how bright is this compartment", an
area-weighted mean or a median of medians is steadier.

**Report the z-span alongside.** A feature seen on 3 slices and one seen on 30
are not comparable as totals, and a total that silently depends on how many
slices happened to pass a filter is not a measurement.

---

## 3. Background — the unsettled part

Raw intensities are meaningless on their own: they carry camera offset, laser
power, exposure, detector gain, and whatever the sample autofluoresces at. Some
kind of background handling is mandatory before any threshold or comparison.

The usual options, roughly in increasing order of effort:

1. **Camera offset only.** Subtract the constant the detector adds with no
   light. Necessary but rarely sufficient.
2. **A modal or low-percentile estimate of the image.** The mode of a
   mostly-empty field approximates background. Fails when the field is crowded.
3. **A measured empty region.** An ROI the operator marks as "nothing here",
   measured in the same channel and the *same z-slice*. Most defensible, needs
   a region that is genuinely empty.
4. **Rolling-ball / morphological subtraction** before measurement. Handles
   uneven illumination, but it is a spatial filter: it changes the pixel values
   the segmentation then sees, so it must be applied consistently and recorded.

**Per slice, not per stack.** Signal attenuates with depth, so one background
number for a whole stack over- or under-corrects at the extremes.

> Measured in this project (`oocyte_count`, Series001/003, 2026-09-22): the mean
> DAPI inside detected ROIs falls from **22.0** in the top 10 slices to **6.4**
> at z 40–50, while the ch2 marker stays flat (28.7 → 28.0). So roughly a 3×
> depth gradient in one channel and almost none in another, in the same stack.
> A single absolute threshold applied across z would be substantially measuring
> depth. At the *feature* level the confound is weaker (Spearman ρ ≈ −0.23
> against mean z), because features span many slices and average it out.

Whatever is chosen, **record it in the output**, the way `_config.txt` records
the detection parameters. A background method that is not recorded makes every
number downstream unreproducible.

---

## 4. Normalisation, and what is safe to compare

Ranked by how much they can be trusted:

1. **Ratios within one feature** — e.g. nuclear ÷ cytoplasmic intensity of the
   same protein in the same cell. Illumination, exposure and depth largely
   cancel. This is the standard readout for translocation and the most robust
   thing on this list.
2. **Ratios within one image** — a feature against other features, or against a
   reference structure in the same field.
3. **Across images, same session, identical settings** — acceptable with
   background correction, provided nothing about the acquisition changed.
4. **Across sessions or experiments** — needs an explicit control: a reference
   sample, beads, or a stain included in every batch. Raw intensities across
   sessions are not comparable, and no amount of downstream statistics repairs
   that.

Other systematic effects worth keeping in mind: photobleaching over a long
acquisition, spectral bleed-through between channels (check with single-stain
controls), and non-linearity near saturation.

---

## 5. Compartments

Nuclear-vs-cytoplasmic quantification is the common case and has one trap that
dominates the rest.

**A "cytoplasm" ROI that contains the nucleus is not cytoplasm.** If the
cytoplasmic measurement is taken over the whole cell outline, nuclear signal
contaminates it, and the N/C ratio is compressed toward 1 — which looks like a
weak effect rather than a measurement error. The cytoplasmic region must be the
cell mask **minus** the nuclear mask (an annulus).

This repo already has the machinery for the relationship — `relate_features.r`
assigns a child feature to a parent by per-slice containment (`--within
'nucleolus=nucleus'`). Subtracting the child's geometry from the parent to make
a measurement region is a further step and is **not implemented**; when it is,
it should be done per slice, for the same reason containment is.

Other compartment notes:

- Erode the nuclear mask slightly before using it, or the nuclear rim picks up
  cytoplasmic signal through the point-spread function.
- A nucleolus is not "background" for the nucleoplasm; exclude it explicitly
  rather than letting it drag the nuclear mean down.

---

## 6. Traps this pipeline is specifically exposed to

- **You cannot filter on the channel you segmented with.** Every detected object
  is positive in the segmentation channel by construction, so its intensity
  there carries no information about whether the call was right. Filtering needs
  either a different channel, or a detection step that deliberately produces a
  superset including negatives.
- **Saturation.** `Max` at the type ceiling means the true value is unknown and
  every mean involving those pixels is an underestimate. Check before trusting.
- **Shape statistics are conditional on the filters above them.** If ROIs below
  a circularity cut are removed before grouping, the surviving population's
  median circularity is a truncated statistic — it cannot be compared against a
  run with a different cut. The same applies to area.
- **Pre-grouping ROI filters can split one object into two.** Removing interior
  slices opens a z-gap that the grouping step cannot bridge. Observed in this
  project: a circularity cut removed an oocyte's widest cross-sections and one
  object was counted as two. See `CLAUDE.md`.
- **Per-slice measurements are not independent.** Adjacent slices of one object
  share signal through the point-spread function, so treating slices as
  replicates inflates confidence. The feature, not the ROI, is the unit.

---

## 7. What this repo does today

- Fiji measures every ROI in every requested channel and writes `_res.txt`.
  `read_fiji_result()` parses it and recovers the ROI id from the `Label`
  column.
- `annotate_features_cli.r` reads **only the outline tables**; the measurements
  are not currently joined onto features. That join is the missing piece for
  both IF quantification and cross-channel filtering of detected features.
- No background estimation, no normalisation, no compartment subtraction.

Nothing above is implemented as policy yet — it is the list of decisions to make
deliberately when the measurement CLI is written.
