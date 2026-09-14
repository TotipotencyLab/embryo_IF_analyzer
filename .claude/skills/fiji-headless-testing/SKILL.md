---
name: fiji-headless-testing
description: Run and test Fiji/ImageJ code (Groovy or IJ1 macro) headlessly from the terminal on this machine, and verify a change against a reference run. Use whenever writing, debugging or porting anything under scripts/groovy/ or scripts/fiji/ — before claiming a script works, and before handing one over for the user to run in the GUI. Covers the working launcher, what does and does not work headless, driving a #@ script from a Binding, and diffing outputs against a known-good run.
---

# Fiji headless testing

Fiji code can be run and tested from the terminal here. Do that before handing a
script to the user — the GUI round-trip is slow for them and hides bugs that an
assertion would catch.

## Launcher

```bash
/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx --headless --console --run script.groovy
```

- **Use `Contents/MacOS/ImageJ-macosx`.** The newer `/Applications/Fiji.app/fiji`
  launcher fails with *"No matching Java installations found"* — it expects Java 21,
  and this Fiji bundles Java 8 (`java/macosx/adoptopenjdk-8.jdk`).
- `ImageJ-macosx` prints *"Unable to locate a Java Runtime"* to stderr and then
  works anyway. Ignore that line; filter it out of output greps.
- `--run` takes `.groovy` (and other script languages); `-macro` takes `.ijm`.
- Add `--mem=6000m` for real images. The default heap is small.
- The docs suggest `-Dimagej.updater.disableAutocheck=true` to skip the
  update-server ping. Not verified with this launcher — startup here is dominated
  by JVM/class loading, so it may not help much.

## Launches are slow — batch everything

A single launch costs roughly 30 s to several minutes, dominated by startup.
**Put every assertion for one question into one script.** Do not iterate one
assertion per launch.

Run in the background and poll the output file rather than blocking:

```bash
<launcher> ... > "$SP/out.txt" 2>&1        # run_in_background: true
until grep -qE "DONE|Exception" "$SP/out.txt"; do sleep 8; done
```

Always end the script with a sentinel line (`println "DONE"`) so the poll has
something to wait for, including on the failure path.

## What works headless

| | headless |
|---|---|
| Groovy scripting via `--run` | works |
| IJ1 macro via `-macro` | works |
| `ImageProcessor` / `ImageStatistics` arithmetic | works |
| `ParticleAnalyzer` **computing** particles | works |
| `Overlay` as a container (`Overlay.addSelection`, naming) | works |
| `ij.gui.Wand` tracing | works |
| `RoiManager` — **any** constructor | `HeadlessException` |
| `ParticleAnalyzer` with `SHOW_OVERLAY_OUTLINES` | `OutOfMemoryError` |
| `Analyze Particles ... show=Overlay` from a macro | silently yields 0 |

`RoiManager extends PlugInFrame extends java.awt.Frame`, so it cannot be
constructed without a display; imagej-legacy does not patch it
([imagej-legacy#153](https://github.com/imagej/imagej-legacy/issues/153), open).
`new RoiManager(false)` fails too.

**To get ROIs headless**, subclass `ParticleAnalyzer` and intercept before its
output routing — see `scripts/groovy/RoiDetect.groovy`:

```groovy
@Override protected void saveResults(ImageStatistics stats, Roi roi) {
    super.saveResults(stats, roi)
    def r = (Roi) roi.clone(); r.setPosition(currentSlice); found << r
}
```

## Driving a `#@` script

SciJava strips `#@` parameter lines before compiling; plain Groovy cannot parse
them. To exercise the *real* orchestrator rather than a copy, strip them and
inject the values through a `Binding`:

```groovy
def src = new File(SCRIPT).text.readLines().findAll { !it.trim().startsWith("#@") }.join("\n")
def b = new Binding([
    "javax.script.filename": SCRIPT,   // so resolveLibDir() still works
    imp: imp, outdir: new File(OUT), nucSigma: 8.0d, /* ... */
])
new GroovyShell(this.class.classLoader, b).evaluate(src)
```

Inject `javax.script.filename` or any script that resolves its own path will
fail. For a compile-only check, `parseClass(src)` the stripped source.

## Test hygiene

- **Never print PASS unconditionally.** Compute a boolean and print expected vs
  actual for every check. A test in this repo once printed `HEADLESS OVERLAY OK`
  directly above `size=0` and a `NullPointerException`.
- State the expected value inline: `println "got=$n (want 6)"`.
- **Kill stray headless processes afterwards.** They persist and hold GBs:

```bash
ps -eo pid,command | grep "[I]mageJ-macosx --headless" \
  | awk '{print $1}' | while read p; do kill "$p" 2>/dev/null; done
```

  Match on `--headless`: **the user's interactive Fiji has no such flag and must
  not be killed.** Check what survived before moving on:
  `ps -eo pid,etime,command | grep "[I]mageJ-macosx" | grep -v headless`

## Verifying a change against a reference run

The way bugs actually get found here: run the old and new paths on the same input
with the same settings, then diff. Counts alone are not enough — two of the three
bugs in the `RoiDetect` port produced plausible-looking counts.

1. **Use the same output prefix** for both runs, in different directories, so
   filenames match and `diff` is direct.
2. **Normalise incidental columns.** The outline `name` column carries the
   basename; `sed 's/^PREFIX/POS/'` both sides before diffing.
3. **ROI zips:** compare entry names and lengths, ignoring timestamps —
   `unzip -l f.zip | awk 'NR>3&&NF>=4{print $1,$4}' | sort`. A raw md5 always
   differs because ZIP entries store mtimes.
4. **When counts differ, compare distributions**, not just totals. `min/median/max`
   of the Area column localises it immediately: a min of `0.86` against an expected
   floor of `80` says the size filter is wrong, not the segmentation.
5. **A constant byte delta per entry points at a fixed-size field.** Every ROI
   being exactly 44 bytes short meant a missing 22-character name stored as UTF-16.

See `references/imagej-api-gotchas.md` for the specific API traps these uncovered.
