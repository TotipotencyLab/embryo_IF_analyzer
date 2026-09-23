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
- `features_config.txt`: the R side's equivalent of Fiji's `_config.txt`. The
  annotate CLI already stamps a `run_id` on its output and `join_feature_table()`
  refuses a join across two runs — but the id is a 10-character hash, so the
  refusal says *that* two tables disagree and nothing about *how*. A sidecar
  written next to `features.rds`, holding the resolved inputs and the effective
  parameters the hash was taken over, turns an opaque mismatch into a diff and
  lets a results directory explain itself months later. The guard works without
  it; this is the diagnosis half. Deferred, not rejected.

This means the Groovy scripts may have to defined into different levels:
- lowest level: utility function - for small individual step
- interactive session macro: the script that can be run on the current active image window. 
- High throughput analysis: This is the one that is based on the sample table and the config. This can be run headlessly in principle.


