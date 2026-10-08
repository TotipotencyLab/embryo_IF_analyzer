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

## "Slice" is two different indices

`getNSlices()` is the **z depth**. `getStackSize()`, `getCurrentSlice()`,
`setSlice()` and `Roi.getPosition()` all index the **flat c × z × t space**.

Verified on a 4-channel × 50-slice × 1-frame hyperstack:

```
getNSlices()                    = 50
getStackSize()                  = 200        // 4 * 50 * 1
setPosition(c=2, z=7, t=1)
  -> getCurrentSlice()          = 26         // c + (z-1) * nChannels
  -> getZ()                     = 7
```

So a slice number taken from `getCurrentSlice()` or `roi.getPosition()` is **not**
a z index — except on a single-channel, single-frame image, which is exactly what
a small test image usually is. The bug hides during development and appears on
real data.

Read an axis with `getZ()` / `getC()` / `getT()`; move with `setPosition(c, z, t)`.

Symptom when missed: z values correct on a one-channel test image and silently
scaled by the channel count on a real acquisition — plausible numbers, wrong
plane, and a downstream z-merge that quietly groups nothing.

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
`.roi` file **and** picked up into the `Label` column by `Analyzer`. Downstream
parsers commonly key on the roi id inside that Label to join measurements back to
outlines, so a missing name breaks the join silently — the numeric columns are all
still correct, which makes it easy to miss.

## ROI Manager auto-label format

`SSSS-NNNN-YYYY` = slice, per-slice index, **y-centre of the ROI bounds** (not x).
Derived empirically and confirmed by byte-identical output. Downstream parsers may
identify the roi column by matching `\d{4}-\d{4}-\d{4}$`, so the shape matters when
producing ROIs without the ROI Manager:

```groovy
def perSlice = [:]
rois.collect { roi ->
    int z = roi.getPosition()
    int idx = (perSlice[z] = (perSlice[z] ?: 0) + 1)
    def b = roi.getBounds()
    String.format("%04d-%04d-%04d", z, idx, b.y + (int) (b.height / 2))
}
```

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
columns on different machines, silently. Issue it explicitly, and treat the option
string as part of the output contract. For example:

```
area mean standard min centroid shape integrated median stack display
```

## ImagePlus.close() frees nothing while a reference is in scope

`close()` detaches a **window**. Headless there is no window, so on an image a
variable still points at it releases no pixels at all. `flush()` is what drops
them. Measured on a 768 MB stack:

| call | released |
|---|---|
| `close()`, variable out of scope and nulled | 763 MB |
| `close()`, variable still live | **0 MB** |
| `flush()` | 730 MB |

The middle row is the one that bites, because it is what every method-local
intermediate looks like: the variable stays in scope to the end of the method
however early you "closed" it.

```groovy
def mask = new Duplicator().run(imp, ...)
...
mask.close()                 // frees nothing -- `mask` is still in scope
mask.close(); mask.flush()   // this is the pair to write
```

On full-size intermediates this is gigabytes carried through every later stage,
and it is invisible until something else needs the heap. Pair the two calls
everywhere, and prefer a source-level assertion over remembering to.

## ImageStack.setProcessor() converts instead of swapping

It cannot be used to put a `ByteProcessor` into a 16-bit stack. It accepts the
call without complaint, allocates a new `short[]`, and converts:

```
before: pixels class = short[]
after setProcessor: pixels class = short[]      <- not byte[]
after setProcessor: is our array  = false       <- not the array handed in
stack bitDepth = 16
```

So a per-plane swap written that way saves nothing (it allocates a short plane
per slice) **and** leaves a 16-bit mask whose "on" value is 255 — which
`setThreshold(128, 255)` downstream still turns into plausible-looking ROIs.
Silent in both directions.

`setPixels(Object, int)` stores what it is given, including `null` and a
mismatched type, and `getProcessor()` then reads the type back off the array. To
rebuild a stack at a smaller type without holding two whole copies, write the new
plane into a new stack and release the source plane as you pass it:

```groovy
out.addSlice(src.getSliceLabel(z), bp)
src.setPixels(null, z)      // this plane is garbage from here on
```

Call `imp.setStack(out)` at the end: `ImagePlus` caches its bit depth.

## Gaussian Blur "stack" mode is per-plane, and parallel

`IJ.run(imp, "Gaussian Blur...", "sigma=S stack")` parallelises over slices
(`PARALLELIZE_STACKS`), and each worker converts its plane to float — on a
108 MP plane that is 415 MB per thread, eight at once. `blurGaussian(ip, sigma)`
in a loop holds one.

The pixels are identical, so this is a free swap when memory is tight: measured
on synthetic 8- and 16-bit stacks, **0 of 16384 pixels differ**. Worth pinning
with a test, since "stack" mode being per-plane is the assumption the swap rests
on.

## ShapeRoi.getRois() returns a lone ROI at (0,0)

A `ShapeRoi` built from one ROI and never combined with another hands that ROI
back from `getRois()` **at the origin** — right size, wrong place. Measured on
1.54p: `new ShapeRoi(new Roi(10, 8, 20, 16)).getRois()` gives bounds
`(0,0,20,16)`; after any `or()`, even with a disjoint ROI, every piece is
where it should be.

So a "union, then split into pieces" helper draws an image with exactly one
ROI in its top-left corner, and nothing errors. `Overview.union()` did this for
years unnoticed — real images always had several ROIs — and it surfaced once
frames were drawn one at a time, where a frame with one nucleolus is ordinary.
Return a lone ROI as itself (`clone()`); test the one-ROI case explicitly.

## Groovy: count{} on a primitive array is count(value)

`(int[] px).count { it > 0 }` compiles, runs, and returns **0** for every
closure: Groovy resolves `count` on a primitive array to `count(Object value)`
and counts the elements *equal to the closure object*. Same for `long[]`,
`byte[]`. Convert first — `(px as List).count { ... }` — or loop. A pixel
check written this way passes silently when it should be finding pixels, which
is the shape of a test that cannot fail.
