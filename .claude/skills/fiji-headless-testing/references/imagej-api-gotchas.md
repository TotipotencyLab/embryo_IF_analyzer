# ImageJ API gotchas

Traps confirmed empirically in this repo. Each one produced a real bug that
looked plausible until the output was diffed against a reference run.

## ParticleAnalyzer filters in pixels, reports in calibrated units

The constructor's `minSize`/`maxSize` are **pixels**. The `Area` it reports is in
**calibrated units**. Verified: a 1257 px² / 12.57 µm² blob passes `minSize=100`
and fails `minSize=2000`.

The Analyze Particles *dialog* takes calibrated units, so a size range taken from
a user must be converted, or the filter is wrong by `1/pixelArea`:

```groovy
double pxArea = cal.pixelWidth * cal.pixelHeight
double minPx  = minSize / pxArea
```

Symptom when missed: far too many particles, and a minimum Area far below the
requested floor.

## Circularity belongs in the 7-arg constructor

```groovy
new ParticleAnalyzer(opts, meas, rt, minPx, maxPx, minCirc, maxCirc)
```

Filtering afterwards from `roi.getStatistics().area` (pixels) and
`roi.getLength()` (calibrated when an image is attached) mixes units and filters
on a meaningless number.

## setSlice() drops the threshold

Pass the processor explicitly instead of relying on the current slice:

```groovy
def ip = imp.getStack().getProcessor(z)
ip.setThreshold(128, 255, ImageProcessor.NO_LUT_UPDATE)
pa.analyze(imp, ip)              // not: imp.setSlice(z); pa.analyze(imp)
```

An unthresholded image makes ParticleAnalyzer try to trace enormous regions,
which presents as `OutOfMemoryError` and looks like a headless limitation.

## Roi.setName() must be set on the object

Keeping names in a parallel list is not enough. The name is encoded into the
`.roi` file **and** picked up into the `Label` column by `Analyzer`. Since
`read_fiji_result.r` joins outlines to measurements by matching the roi id inside
that Label, a missing name silently breaks the R join.

## ROI Manager auto-label format

`SSSS-NNNN-YYYY` = slice, per-slice index, **y-centre of the ROI bounds** (not x).
Derived empirically and confirmed by byte-identical output. `read_fiji_result.r`
identifies the roi column by matching `\d{4}-\d{4}-\d{4}$`, so the shape matters.
Reproduced in `RoiDetect.autoLabels()`.

## ImageStatistics.getHistogram() returns long[]

`AutoThresholder.getThreshold()` accepts only `int[]`. Convert before calling, or
it fails with `MissingMethodException` on 16-bit data.

`AutoThresholder` also returns a **bin index**, not a pixel value. For 16-bit data
map it back through the 256-bin scale:

```groovy
double binSize = (stats.histMax - stats.histMin) / 256.0
double t = stats.histMin + (bin + 1) * binSize
```

## setAutoThreshold() honours the selection; the Auto Threshold plugin does not

- `setAutoThreshold("Default")` — built-in macro function. Computes its histogram
  from the **active selection**. No `dark` modifier targets the **dark** end.
- `run("Auto Threshold", "method=Default white stack")` — the Fiji *plugin*.
  Different option syntax, ignores selections.

`dark` means *"the background is dark"*, i.e. select bright objects. This is the
usual source of confusion.

Restricting the histogram to a selection is often the fix when an auto-threshold
lands in the wrong place: a whole-frame histogram is dominated by whichever region
is largest, not by the boundary of interest.

## IJ1 macro: cannot return a nested string-returning call

```javascript
return sanitize_name(x);        // Error: "Numeric return value expected"
id = sanitize_name(x); return id;   // works
```

The interpreter cannot infer a string return type through a nested user-function
call. Applies to the IJ1 macro language only.

## IJ1 macro: newArray(n) makes an n-element zero array

`newArray(2)` is **not** a one-element array holding 2 — it is a 2-element array
of zeros. Writing `newArray(ch)` to mean "channel ch" silently changes the array
length, and any function keyed off `.length` then does the wrong thing.

## Script self-location

SciJava injects exactly two bindings: `javax.script.filename` (absolute path,
String) and `org.scijava.script.ScriptModule` (`.getInfo().getPath()`). Everything
else is empty — `codeSource.location` gives a useless `file:/groovy/script`, the
class resource is null, no relevant system properties. An unsaved Script Editor
buffer has no path at all, so always fail with an explicit message.

## Set Measurements is a persistent user preference

Not a per-script setting. A script that relies on it produces different output
columns on different machines. Issue it explicitly. This set reproduces the
column contract the R side expects:

```
area mean standard min centroid shape integrated median stack display
```
