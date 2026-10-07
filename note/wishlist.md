# Wishlist features

- Watershed for nuclear detection (and for future deferred particle detection)
- `z_process`: make z-projection of each image, then save to a small file. Optionally overlay with ROI (e.g., detected nucleus)
- Groovy study scripts:
  - A script for image metadata inspection (how many series it contains, dimension of each (x,y,z), etc.)
  - A script that inspect the current GUI session - what is currently opened, how many window, working with ROI, etc.
- Introduce sample table (TSV) and config (YAML)
- `--parent_z_pad` for containment: let an inner feature match a parent a slice
  or two *beyond* the parent's own z-range (nucleus at z 5-8 would accept a
  nucleolus at z 4 or 9). Deliberately not implemented -- matching is strictly
  inside `[z_min, z_max]`, and `test-relate_features.R` has the test that would
  have to change.
- Use the parent to *split* child groups. `define_feature_group()` groups
  nucleoli on z-overlap alone, before any parent is known, so two nucleoli in
  adjacent nuclei could in principle merge into one feature. Parent assignment
  currently happens after grouping and cannot undo that. Not built until we see
  it actually happen.
- IF quantification CLI: needs the background measurement question settled
  first (empty space vs cytoplasm, and nucleus vs cytoplasm signal).
- Count an oocyte only where its **nucleolus** is visible, as the way to stop
  one oocyte being counted once per serial section it appears in. A nucleolus is
  roughly one per nucleus and much smaller than the oocyte, so it falls in a
  single section — which sidesteps aligning outlines across sections entirely.
  That alignment is the thing to avoid: a follicle's outline in the next section
  is not a deformed copy of this one, it is a different chord through the object,
  so shape registration is solving a harder problem than the count needs.
  Preferred shape is a **standalone feature-filtering script** rather than a flag
  on the annotate CLI, so the rule is visible in the command that applied it.
  The machinery exists — `relate_features.r --within 'nucleolus=nucleus'` already
  assigns nucleoli to parents. Known bias: a nucleolus straddling a cut is
  counted twice, and the classical corrections for that (Abercrombie, or a
  physical disector over a section pair) need section thickness and order, which
  is what a specimen/section grouping column would have to carry. Deferred.
- `features_config.txt`: the R side's equivalent of Fiji's `_config.txt`. The
  annotate CLI already stamps a `run_id` on its output and `join_feature_table()`
  refuses a join across two runs — but the id is a 10-character hash, so the
  refusal says *that* two tables disagree and nothing about *how*. A sidecar
  written next to `features.rds`, holding the resolved inputs and the effective
  parameters the hash was taken over, turns an opaque mismatch into a diff and
  lets a results directory explain itself months later. The guard works without
  it; this is the diagnosis half. Deferred, not rejected.
- **Blur sigma in physical units** (`nucleus_blur_sigma`, and the nucleolus
  blur and erode, which have the same problem). Today they are pixels, so one
  value is a different physical blur on every pixel size — the batch already
  warns about exactly this in a mixed-size batch. On the oocyte data the
  default 8 px was 1.8 µm on the 0.223 µm series and 3.6 µm on the 0.446 µm
  ones, and the manual threshold then meant different things on each, because
  blur lowers a small object's peak far more than a large one's. Tuning
  (2026-10-06, 10 hand-annotated sections) found 1.8 µm with Manual 32-255 best,
  and blur and threshold trade off — a smaller blur behaves like a lower
  threshold — so the two must be tuned together and a changed blur needs its
  threshold found again. Design questions for when it is built: a unit key
  (`nucleus_blur_unit = px|um`, default `px` so every old config means what it
  meant) or a second key; record the pixel sigma actually applied as
  provenance; anisotropic pixels (`blurGaussian` takes separate x and y sigmas);
  and the mixed-pixel-size warning, which should stop naming a parameter that
  is no longer in pixels. A situational version may exist on an SSD-checkout
  branch of v0.5.1 for the oocyte analysis — look there first.
- **Run the Groovy tests in one Fiji session.** Each `tests/groovy/Test_*.groovy`
  is its own headless launch, and the launch is most of the cost: 12–24 s per
  test, ~2.5 min for the eight, when the tests themselves take a second or two
  (measured 2026-10-06; `Test_RoiExport`, 25 checks on tiny synthetic images,
  still took 12 s). Per launch: JVM start, the SciJava context scanning ~1100
  jars' plugin index, compiling the test and its libraries through
  `GroovyClassLoader`, and Bio-Formats' own start-up where a test reads a file.
  Then the JVM does not exit, so a caller has to watch for the
  `passed: N   FAILED: M` line and kill it. A single runner script that
  evaluates each test file in turn in one session would pay the start-up once
  (estimate ~30–40 s for the set), and could print one combined summary and
  `System.exit` at the end, which would also remove the wait-and-kill. The cost
  to check: tests sharing a session can leak state into each other — ImageJ
  preferences (`blackBackground`, Set Measurements, Bio-Formats'
  `windowless`) persist within a session, and some tests assert exactly those
  preferences are left as found. Keep the one-file-per-launch route working
  for running a single test.

This means the Groovy scripts may have to defined into different levels:
- lowest level: utility function - for small individual step
- interactive session macro: the script that can be run on the current active image window. 
- High throughput analysis: This is the one that is based on the sample table and the config. This can be run headlessly in principle.


