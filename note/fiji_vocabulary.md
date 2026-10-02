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

## Counting: what starts at 1, and what at 0

🔒 **Rule for this repo: every image axis it writes counts from 1** — channel,
z and t — because that is how ImageJ shows them, and ImageJ is where the
images are looked at. Decided 2026-10-02 (`time_axis`).

| | counts from | where you meet it |
|---|---|---|
| **channel** | **1** | ImageJ's C slider, `setPosition(c, …)`, slice labels `c:1/4`; ours: `dna_channel`, `channels_measured`, `ch` in `_res.txt`, `ch1_signal`, `_overview_ch1.png`, `sources.tsv`'s `channel` |
| **z** | **1** | the Z slider, `z:1/50`; ours: `z` in the outline and measurement tables, `SSSS` in ROI ids, `z_spec` |
| **t** | **1** | the T slider, `t:3/96`; ours: `t` in the outline and measurement tables, `TTTT` in ROI ids, `sources.tsv`'s `t`, `Make_LuxendoTiff`'s `frames=` and its `_t<TTTT>` names |
| pixel x, y | 0 | `ip.get(x, y)`, ROI coordinates (the outline table scales them to calibrated units) |
| row / list indices | 0 | Results-table rows, ROI Manager and overlay indices, Groovy and Java lists |
| **Bio-Formats** | **0** | series, and plane / z / c / t in its own API (`getIndex(z, c, t)`, `openBytes(plane)`) |
| **`series_index`** | **0** | ours, deliberately: it is the address Bio-Formats finds a series by, not an image axis, and it is inside every `series_id` (`s0000`) |
| **Luxendo** | **0** | its file names (`Cam_long_0005`), `time_point` and `channel_0` in the sidecars and directories — the instrument's records, read, never rewritten |

So the only place a 0-based count turns into ours is where we read Bio-Formats
or Luxendo: `t` = Luxendo `time_point` + 1, `channel` = Luxendo channel + 1.
⚠️ That conversion is an off-by-one that raises no error, so each one is pinned
by a test, and a table written before the rule (with `t = 0` / `channel = 0`)
is refused rather than read.

⚠️ R's own indexing also starts at 1 — which makes the rule cheap there, and
the Groovy side the one to watch.

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
| title | `imp.getTitle()` | filename-ish; the series id of an interactive run when `Series id` is left blank |
| size | `getWidth()`, `getHeight()` | pixels |
| dimensions | `getNChannels/NSlices/NFrames()` | c, z, t — see the warning above |
| flat stack size | `getStackSize()` | c × z × t |
| bit depth | `getBitDepth()` | 8 here; affects threshold ranges |
| pixel size | `getCalibration().pixelWidth` / `.pixelHeight` / `.pixelDepth` | **calibrated units, not pixels** |
| unit | `getCalibration().getUnit()` | e.g. `micron` |
| is it calibrated | `getCalibration().scaled()` | false ⇒ sizes are in pixels |
| slice label | `getStack().getSliceLabel(n)` | per-plane text; the measurement `Label` repeats it |

> ⚠️ **Calibrated vs pixel units.** `ParticleAnalyzer` *reports* Area in
> calibrated units but *filters* in pixels. Mixing them silently changes how
> many objects you get. `pixelWidth` and `pixelHeight` can differ — scale y by
> `pixelHeight`.

Slice labels here look like:

```
c:1/4 z:1/50 - Lightning 001/Mark_and_Find 001/Position010
```

The trailing `Position010` is the series name. Before v0.7.0 the scripts
searched the label for a token such as `Position` to name outputs; they now
take the title, or the typed `Series id`. `read_fiji_result.r` parses `z:` from
the measurement `Label` of files written before the explicit `z` column.

## Processing terms

- **Threshold** — split pixels into object vs background. *Auto* methods (Otsu,
  Huang, Default…) pick the cut from the histogram; **stack histogram** uses one
  histogram for the whole stack instead of one per slice.
- **Fill Holes** — close enclosed background inside an object.
- **Watershed** — split touching objects that thresholded into one blob. Needs a
  binary image, and reads `Prefs.blackBackground` to decide which phase is the
  object.
- **Z-projection** — collapse z into one plane (max, sum, mean…).
