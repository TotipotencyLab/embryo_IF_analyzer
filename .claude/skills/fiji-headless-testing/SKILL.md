---
name: fiji-headless-testing
description: Run and test Fiji/ImageJ code (Groovy or IJ1 macro) headlessly from the terminal on this machine, and verify a change against a reference run. Use whenever writing, debugging or porting a Fiji script or ImageJ macro — before claiming it works, and before handing one over for the user to run in the GUI. Covers the working launcher, what does and does not work headless, driving a #@ script from a Binding, and diffing outputs against a known-good run.
---

# Fiji headless testing

Fiji code can be run and tested from the terminal here. Do that before handing a
script to the user — the GUI round-trip is slow for them and hides bugs that an
assertion would catch.

## Finding a working launcher

**Do not assume a path.** Which launcher works is installation-specific: a Fiji
that has been updated in place for years may still bundle Java 8 while its newer
`fiji` launcher expects Java 21, in which case `fiji` fails with *"No matching
Java installations found"* and the older platform launcher works instead. A fresh
install typically behaves the opposite way.

If `CLAUDE.local.md` records a verified launcher for this machine, use it. If not,
probe once and record the result there:

```bash
cat > /tmp/probe.groovy <<'EOF'
println "LAUNCHER_OK headless=" + java.awt.GraphicsEnvironment.isHeadless()
EOF

for L in /Applications/Fiji.app/Contents/MacOS/ImageJ-* \
         /Applications/Fiji.app/fiji \
         "$(command -v fiji 2>/dev/null)"; do
  [ -x "$L" ] || continue
  if "$L" --headless --console --run /tmp/probe.groovy 2>&1 | grep -q LAUNCHER_OK; then
    echo "WORKS: $L"; break
  else
    echo "fails: $L"
  fi
done
```

Then:

```bash
<launcher> --headless --console --run script.groovy      # .groovy
<launcher> --headless --console -macro  script.ijm       # IJ1 macro
```

- A launcher may print *"Unable to locate a Java Runtime"* to stderr and still
  work. Filter that line out of output greps rather than treating it as failure.
- Add `--mem=<N>m` for real images; the default heap is small and an OOM here
  looks exactly like a code bug. Size it against the image and free RAM.
- The docs suggest `-Dimagej.updater.disableAutocheck=true` to skip the
  update-server ping. Unverified; startup is dominated by JVM/class loading, so
  it may not help much.

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
output routing:

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
with the same settings, then diff. Counts alone are not enough: a wrong size
filter and a wrong circularity filter both produce plausible-looking counts.

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

## Verifying an option that should change nothing

Adding a flag that is off by default and behaviour-preserving needs **two**
proofs, and only the first is obvious:

1. **Off changes nothing** — reference diff, exactly as above.
2. **On does something** — without this, identical output is equally consistent
   with *the flag is never read*.

Step 2 is the one that gets skipped, because real data often has nothing for the
feature to act on. Both runs then legitimately agree, and a no-op that was never
wired up is indistinguishable from a feature working correctly. Observed: adding
an object-splitting step changed neither the counts nor a byte of the outlines on
a real image, because nothing in that image needed splitting.

Resolve it by synthesising the case the feature exists for. No fixture, seconds
to run:

```groovy
// two overlapping discs threshold into ONE blob
def ip = new ByteProcessor(200, 120)
ip.setColor(255)
ip.fill(new OvalRoi(20, 20, 80, 80))
ip.fill(new OvalRoi(80, 20, 80, 80))
// expect 1 particle with the option off, 2 with it on
```

Assert the negative too: that the option leaves alone what it must not — two
well-separated objects stay 2, one object stays 1 — and that the source image is
unmodified. An option that splits *everything* passes the positive check on its
own.

The general rule: **a check that cannot fail is not evidence.** Before accepting
a pass, ask what result would have revealed the bug, and confirm the test could
have produced it.
