# Fiji vocabulary

Common Fiji terms in plain language, with the ones that cause bugs marked.
Verified against `fixture/if_data/raw_data/…Position010.tif` (4 channels ×
50 z × 1 frame, 8-bit).

## Image structure

- **Series** — one image inside a container file. A Leica `.lif` holds many; a
  `.tif` exported from one holds a single series. A series is a whole
  acquisition with its own channels, z and time — usually one microscope
  position, but a tile scan or timelapse is also one series. Bio-Formats reads
  the series list from the header without loading pixels
  (`Inspect_ImageFile.groovy`).
- **Channel (c)** — one detector / fluorophore. `getNChannels()`.
- **Slice (z)** — one focal plane of the z-stack. `getNSlices()`.
- **Frame (t)** — one timepoint. `getNFrames()`.
- **Stack** — any set of planes. **Hyperstack** — a stack that knows which axis
  is which (c, z, t). Ours are hyperstacks.

> ⚠️ **"Slice" means two different things.** `getNSlices()` is the z depth (50
> here). But `getStackSize()` and `getCurrentSlice()` use the *flat* index over
> every plane, c × z × t (200 here). At channel 2, z 7, `getCurrentSlice()`
> returns **26**, not 7 — it counts `c + (z-1) × nChannels`. Use `getZ()` /
> `getC()` / `getT()` when you mean an axis, and `setPosition(c, z, t)` to move.
> A ROI's `getPosition()` is likewise a flat index.

## Regions

- **ROI** — Region of Interest: a selection (here, one detected object on one
  slice). Held in memory, stored in the **ROI Manager**, saved as `.roi` or a
  `.zip` of them. Its **name** matters: it is written into the `.roi` file and
  into the measurement `Label` column, which is how the R side joins
  measurements to outlines.
- **Outline** — *our* term, not Fiji's: the polygon vertex coordinates of an
  ROI, written to `*_outline.txt` as one row per vertex (`name, roi, z, x, y`)
  in calibrated units. This is what R turns into polygons.
- **Mask** — a binary image (object = 255, background = 0) produced by
  thresholding, from which ROIs are detected.

## Key image metadata fields

| what | call | note |
|---|---|---|
| title | `imp.getTitle()` | filename-ish; fallback for the output id |
| size | `getWidth()`, `getHeight()` | pixels |
| dimensions | `getNChannels/NSlices/NFrames()` | c, z, t — see the warning above |
| flat stack size | `getStackSize()` | c × z × t |
| bit depth | `getBitDepth()` | 8 here; affects threshold ranges |
| pixel size | `getCalibration().pixelWidth` / `.pixelHeight` / `.pixelDepth` | **calibrated units, not pixels** |
| unit | `getCalibration().getUnit()` | e.g. `micron` |
| is it calibrated | `getCalibration().scaled()` | false ⇒ sizes are in pixels |
| slice label | `getStack().getSliceLabel(n)` | per-plane text; carries the position token |

> ⚠️ **Calibrated vs pixel units.** `ParticleAnalyzer` *reports* Area in
> calibrated units but *filters* in pixels. Mixing them silently changes how
> many objects you get. `pixelWidth` and `pixelHeight` can differ — scale y by
> `pixelHeight`.

Slice labels here look like:

```
c:1/4 z:1/50 - Lightning 001/Mark_and_Find 001/Position010
```

The trailing `Position010` is the token the scripts pull out to name outputs,
and `read_fiji_result.r` parses `z:` from the measurement `Label`.

## Processing terms

- **Threshold** — split pixels into object vs background. *Auto* methods (Otsu,
  Huang, Default…) pick the cut from the histogram; **stack histogram** uses one
  histogram for the whole stack instead of one per slice.
- **Fill Holes** — close enclosed background inside an object.
- **Watershed** — split touching objects that thresholded into one blob. Needs a
  binary image, and reads `Prefs.blackBackground` to decide which phase is the
  object.
- **Z-projection** — collapse z into one plane (max, sum, mean…).
