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

## Passing `#@` parameters on the command line

A `#@`-parameterised script is fully drivable headless — the dialog is simply
never shown. Append one quoted string after the script path, `name=value` pairs
separated by **commas**:

```bash
<launcher> --headless --console --mem=6000m \
  --run script.groovy "strParam='hello world',intParam=7,dblParam=8.5,boolParam=true,fileParam='/abs/path.tif'"
```

Measured on ImageJ 2.16.0 / 1.54p, one launch, all six declared parameters:

```
#@ String  strParam   ->  strParam='hello world'   arrives as "hello world"
#@ Integer intParam   ->  intParam=7               arrives as Integer 7, not "7"
#@ Double  dblParam   ->  dblParam=8.5             arrives as Double
#@ Boolean boolParam  ->  boolParam=true           arrives as Boolean
#@ File    fileParam  ->  fileParam='/abs/path'    arrives as File, .exists() true
```

- Names are the **variable** names from the `#@` lines, not the `label=` text.
- Values are typed on arrival; do not re-parse them.
- An omitted parameter **takes its declared `value=`**, correctly typed —
  measured: `#@ Boolean (value=false) b` omitted arrives as Boolean `false`, and
  `#@ Integer (value=7) n` as `7`. An omitted `required=false` parameter is
  `null`.

⚠️ **A required parameter with no default HANGS the run.** Omit a `#@` parameter
that declares neither `value=` nor `required=false` and the headless process
blocks forever waiting for input that cannot arrive — no error, no timeout, just
a JVM sitting there looking like slow work. Measured on
`#@ Boolean (label="...") boolBare`. Give every parameter a default or mark it
optional, and treat an unexpectedly silent run as this until proven otherwise.

**`choices=` without `value=` is exactly this case, and it does not look like
it.** A dropdown reads as though it has a default — the first entry — and in the
GUI it does. Headless it is a required parameter with nothing to fall back on:

```groovy
#@ String (choices={"auto","importer","reader"}) mode              // HANGS if omitted
#@ String (value="auto", choices={"auto","importer","reader"}) mode  // fine
```

Measured: omitted, the first produced nothing in 90 s; the second ran with
`mode=auto`. Naming the first choice explicitly changes nothing in the dialog,
because that is already what SciJava picks — so there is no cost to doing it
everywhere, and one script gaining a new dropdown is otherwise enough to stop
every existing headless caller that does not know to pass it.

**What this failure looks like** is worth memorising, because it is not an
error. A 284-byte log holding nothing but the launcher's two *"Unable to locate
a Java Runtime"* lines — which are normal stderr noise on this machine — and a
JVM that never exits. The absence of output IS the symptom.

⚠️ **`choices=` is NOT enforced on the command line.** It constrains the GUI
dropdown and nothing else: headless, SciJava hands your script whatever string
was passed. Measured on a probe declaring
`choices={"(from config)","yes","no"}`:

```
pick=true    ->  "true"            accepted, and not in the list
pick='yes'   ->  "yes"
absent       ->  "(from config)"   the declared value=
```

So a `#@ String` with `choices=` is a **free-text parameter that happens to
render as a dropdown**, and the script must validate it. Reading the declaration
as though it were a contract is how a caller passing a stale value gets silently
mishandled rather than refused.

This bites hardest when a parameter **changes type between releases**. A
`Boolean saveOverview` became a three-way `String`
`{"(from config)","yes","no"}`, resolved as:

```groovy
if (saveOverview != "(from config)") params.save_overview = (saveOverview == "yes")
```

Every existing caller still passed `saveOverview=true`. That is not
`"(from config)"`, so it took the override branch; `"true" == "yes"` is false;
overviews were silently switched **off** — the opposite of what the caller
said, with no error, on a long run nobody watches to the end.

Validate against the same list the dropdown declares, and **refuse** rather than
coerce: a value the interface no longer has means the caller believed something
that is no longer true, and guessing at their intent is how the reversal stays
invisible.

```groovy
static final List<String> CHOICES = ["(from config)", "yes", "no"]
if (!CHOICES.contains(v)) throw new IllegalArgumentException(
    "saveOverview must be one of " + CHOICES.join(", ") + "; got >>>" + v + "<<<")
```

Put that check in a **library class, not the `#@` script**. A front end is
typically only ever compiled by a test suite, never run — so a check written
there cannot be tested, which is exactly how the bug above shipped.

⚠️⚠️ **`#@` parameter values PERSIST between runs — including from the GUI into
headless.** SciJava remembers what a parameter was last set to and reuses it when
the value is not supplied, so a headless batch can silently run with a string
somebody typed into a dialog weeks earlier. Observed here: a batch script that
was never given `outPrefix` or `saveOverview` ran with `outPrefix=test_` and
`saveOverview=true`, both left over from an interactive session — and wrote
overview PNGs nobody asked for. (Neither parameter still has that shape in this
repo — `outPrefix` was removed and `saveOverview` became a three-way string.
The observation is kept as the evidence for the rule, not as a description of
today's dialog.) It is the same trap as `Set Measurements` and
`Prefs.blackBackground`, which are also persistent user preferences.

```groovy
#@ String (persist=false, label="Output prefix", value="") outPrefix
```

Put `persist=false` on **every** parameter of any script whose output must
depend only on its inputs.

⚠️⚠️ **`ImporterOptions` is the same trap, and it reaches the GUI.**
`loci.plugins.in.ImporterOptions` is backed by ImageJ preferences, and the
importer calls `saveOptions()` after a successful open — so options set
programmatically become the operator's defaults. Measured, in one process:

```
A. unguarded   before: windowless=false   after: windowless=true    <- leaked
B. guarded     before: windowless=false   after: windowless=false
```

`setWindowless(true)` in a script leaves `.bioformats.windowless=true` in
`IJ_Prefs.txt`, and from then on **dragging a file onto Fiji silently opens the
first series instead of offering the series chooser** — the dialog does not come
back on its own. Reported by a user after a single run of an inspection script.

Two defences, in order of preference:

1. **Do not use `ImporterOptions` at all** when you only need to look. Going
   through `ImageReader` directly — `reader.openBytes(reader.getIndex(z,c,t))` —
   reads pixels with no preference involvement whatsoever.
2. When you genuinely need a window or a full `ImagePlus`, save and restore
   every preference you touch:

```groovy
boolean prev = ij.Prefs.get("bioformats.windowless", false)
try   { /* build options, BF.openImagePlus(opt) */ }
finally { ij.Prefs.set("bioformats.windowless", prev) }
```

To put a machine right afterwards:
`ij.Prefs.set("bioformats.windowless", false); ij.Prefs.savePreferences()`
 Persistence is a convenience for a dialog a human is
looking at; it is a correctness bug anywhere else. The tell is a run whose
recorded parameters do not match what you passed — which is a good reason for a
batch to write the parameters it actually used.

⚠️ **The comma separates parameters; a comma INSIDE a quoted value is kept.**
Measured: `csvParam='1,2,3'` arrives as the single string `1,2,3`. This is the
opposite of argparser on the R side, which splits an `nargs=Inf` value on commas
even when the shell delivered one element (see the `r-cli-convention` skill). Do
not carry that habit across — but do not rely on the difference either: quoting
is what preserves it.

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

Whether the JVM exits after the script finishes is not consistent: a small probe
that opens no images may exit cleanly while a full pipeline run sits there
forever. Do not infer one from the other — always judge completion by the output
and kill afterwards.

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
| `imp.show()` | **returns without throwing** — harmless, still worth guarding |
| Bio-Formats `ImageReader` + `MetadataTools` (no pixels) | works |
| `loci.plugins.in.ImporterOptions` + `BF.openImagePlus` | works, series selection included |

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

## Reading image files headless (Bio-Formats)

Both halves work headless: parsing metadata without touching pixels, and opening
a chosen series.

```groovy
def r = new loci.formats.ImageReader()
def m = loci.formats.MetadataTools.createOMEXMLMetadata()
r.setMetadataStore(m)        // BEFORE setId(), or the store is never populated
r.setId(path)
r.getSeriesCount(); r.setSeries(s); r.getSizeX() ...
m.getImageName(s); m.getPixelsPhysicalSizeZ(s)
r.close()                    // holds an open file handle
```

Metadata parsing is cheap enough to do on every file in a batch: **887 ms** for
an 8.9 GB Leica `.lif` with 15 series. Opening a 2-slice crop of one
2048x2048x56x3 series from that same file took **2.9 s**.

```groovy
def opt = new loci.plugins.in.ImporterOptions()
opt.setId(path); opt.setWindowless(true)
opt.clearSeries(); opt.setSeriesOn(idx, true)
opt.setZBegin(idx, 0); opt.setZEnd(idx, nZ - 1)
def imp = loci.plugins.BF.openImagePlus(opt)[0]
```

⚠️ `setZEnd` takes a **0-based index, not a count**. Passing `1` for a
single-plane series throws `IllegalArgumentException: Invalid Z index: 1/1`.
Clamp against the series' own `getSizeZ()`.

### ⚠️ The importer's cost is O(series in the FILE), per call

`BF.openImagePlus` prepares a description of **every** series in the file before
returning the one you asked for — that is what the series chooser needs, and
`setWindowless(true)` skips *showing* the dialog, not building it. Measured on
two real Leica files:

| | 15 series (8.3 GiB) | 1563 series (200 GiB) |
|---|---|---|
| `ImageReader.setId` (header only) | 1.2 s | 2.3 s |
| `BF.openImagePlus`, one series | 1.5 s | **181.9 s** |
| same pixels off a held-open reader | 0.22 s | **0.11 s** |

Requesting two series in one call cost 191 s against 182 s for one, confirming
the cost is per *call*, not per series requested. So **a loop that opens one
series per iteration multiplies it**: ~180 s per row on such a file.

Where many series are opened from one file, hold a single reader open and build
the `ImagePlus` yourself:

```groovy
def rd = new loci.plugins.util.ImageProcessorReader(
             new loci.formats.ChannelSeparator(new loci.formats.ImageReader()))
rd.setMetadataStore(meta); rd.setId(path)     // ONCE for the whole file
rd.setSeries(idx)                             // per image: moves a cursor
def st = new ij.ImageStack(rd.getSizeX(), rd.getSizeY())
for (t..) for (z..) for (c..)                 // c fastest: matches setDimensions
    st.addSlice(label, rd.openProcessors(rd.getIndex(z, c, t))[0])
def imp = new ij.ImagePlus(title, st); imp.setDimensions(nc, nz, nt)
```

**Everything the importer was doing for you is now yours to reproduce, and the
failures are silent** — each of these was wrong first time while every measured
number stayed correct:

- **Calibration.** A bare `ImagePlus` measures in *pixels*. Copy
  `getPixelsPhysicalSizeX/Y/Z` onto the `Calibration` by hand; skip Z where
  there is no z axis.
- **The unit string.** The importer writes `micron`; the metadata store's symbol
  is `µm`. Map it, or every `pixel_unit` you record differs by one word.
- **Slice labels.** Without them ImageJ falls back to the slice number, and
  anything parsing the `Label` column of a measurement table sees a different
  string in every row.
- **Plane order.** Add slices in the same order `setDimensions(c, z, t)` implies,
  or channels and slices transpose — with valid numbers attributed to the wrong
  channel.

Assert the two paths against each other rather than trusting either: build a
fixture whose series hold *different* content, so a reader that silently ignores
`setSeries` fails instead of passing every calibration check twice.

Things worth knowing about what comes back:

- `physicalSizeZ` is **null on single-plane series**. Anything recording a z step
  must treat it as nullable.
- **Pixel size can vary between series of one file** — 0.4456 / 0.2227 / 0.1098
  um were all present in one `.lif`. A parameter in pixels (a blur sigma) is
  therefore not comparable across those series, while one in calibrated units is.
- `imp.getTitle()` is `"<file>.lif - <series name>"` **only when the file holds
  more than one series**; a single-series file is titled with the bare filename.
  Bio-Formats disambiguates only when there is something to disambiguate. The
  file half is the only thing making a series name unique across files: Leica
  defaults like `Series001` recur in every file.
- The slice label is `"c:1/3 z:1/56 - <series name>"`, and **a component is
  omitted when its dimension has one entry** — `"z:1/3 - S0"` for a
  single-channel stack, `"- P"` for a single plane. Measured on all three
  shapes; do not assume the full form.
- `imp.getCalibration().pixelDepth` **is** populated by the Bio-Formats import.

## Test hygiene

- **Never print PASS unconditionally.** Compute a boolean and print expected vs
  actual for every check. A test in this repo once printed `HEADLESS OVERLAY OK`
  directly above `size=0` and a `NullPointerException`.
- State the expected value inline: `println "got=$n (want 6)"`.
- **Kill your headless process afterwards, by the PID you launched.** They
  persist and hold GBs:

```bash
<launcher> --headless --console --run t.groovy > t.log 2>&1 &
pid=$!
# ... wait on the log ...
kill "$pid" 2>/dev/null
```

  Not by matching `--headless`: **another session may be running its own
  headless Fiji** — observed, two agents on one machine at once — and the
  user's interactive Fiji, which has no such flag, must never be killed either.
  A match-all is only for clearing up strays you know are yours. Check what survived before moving on:
  `ps -eo pid,etime,command | grep "[I]mageJ-macosx" | grep -v headless`

## Memory on large images

A tile merge is not a bigger version of a test image; it is a different problem.
One 11344 x 9590 x 25 x 2 uint8 series is 5187 MB of pixels, and every
intermediate is another whole copy. The launcher's default heap here is
`-Xmx10000m` on a 16 GB machine, and `maxMemory()` reports **less** than that
and *grows during the run* (ParallelGC excludes a survivor space), so treat the
usable ceiling as ~8.9-9.6 GB and never as 10.

Two habits carry most of it, both in `references/imagej-api-gotchas.md`:

- **Pair `close()` with `flush()`.** `close()` releases nothing while the
  variable is in scope, which is every method-local intermediate.
- **Do not build a replacement stack while holding the original.** Mutate in
  place when the type is unchanged, and when it must change, release each source
  plane as you consume it.

Prefer a source-level assertion to remembering either one — a set difference
fails, a checklist is something a person has to read:

```groovy
// no bare close() on an image, in any library file
code =~ /\.close\(\)/ && !(code =~ /\.flush\(\)/)
```

### Measuring it

Inline the steps you suspect into a standalone script and print
`totalMemory() - freeMemory()` after each, so each allocation is visible as its
own step. Run it against the real series — synthetic images of this size cost
more to build than to measure.

```groovy
def rt = Runtime.getRuntime()
def MB = { long b -> String.format("%.0f", b / 1048576.0d) }
def mem = { String t -> println "MEM  " + t.padRight(34) + " used=" +
    MB(rt.totalMemory() - rt.freeMemory()) + "MB  max=" + MB(rt.maxMemory()) + "MB" }
```

⚠️ **A SciJava `--run` script has no `args`** — `MissingPropertyException: No
such property: args`. Pass inputs as `-D` system properties *before* `--run`:

```bash
ImageJ-macosx --headless --console -Dlif="$lif" -Dseries=14 --run Diag_Mem.groovy
```

⚠️ **Keep alive whatever the real pipeline keeps alive.** Dropping the source
image at the end of a diagnostic lets GC collect gigabytes the real run cannot,
the failure disappears, and you have measured a lighter run than the one that
failed. End the script with a line that forces it to stay reachable:

```groovy
println "REPORT imp_live=" + imp.getStackSize()
```

⚠️ **A margin that depends on GC timing is not a margin.** Transient
collectible garbage — a parallel filter's per-thread float planes — can put the
high-water mark above the ceiling and still pass, repeatedly, until the run that
matters. Judge a step by what it *demands*, not by whether it survived.

### Proving a memory fix

The failure mode is silent, so the reference diff is not enough on its own:

1. **Pixels unchanged**, against the pre-change implementation kept inline as an
   oracle — and print the differing pixel *count*, never a boolean.
2. **The new branch actually ran.** In place means the stack instance survives:
   `assert imp.getStack().is(before)`. Without this, an early `return` on a
   mistyped bit-depth check passes (1) perfectly.
3. **It fits where it did not.** Re-run the failing series at the **default**
   heap. The control is the original failure at that same heap; one row is
   enough. Raising `--mem` proves nothing about the fix.
4. **Do not assume the peak is where the OOM was.** Fixing the reported step
   moves the failure to the next one. The only evidence that a pipeline fits is
   a whole run finishing.

## Verifying a change against a reference run

The way bugs actually get found here: run the old and new paths on the same input
with the same settings, then diff. Counts alone are not enough: a wrong size
filter and a wrong circularity filter both produce plausible-looking counts.

1. **Give both runs the same series id** (`series_id` in the parameters, or the
   `Series id` field), in different directories, so filenames match and `diff`
   is direct. Before v0.7.0 this was the output prefix; a run of `main` from
   then needs `output_prefix` + `position_pattern` to produce the same name.
2. **Normalise incidental columns.** The outline `name` column carries the
   series id; if the two runs' ids differ, `sed` one into the other on both
   sides before diffing. `_res.txt`'s `Label` carries the image *title*, which
   an interactive run's duplicated window suffixes with `-1`.
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
