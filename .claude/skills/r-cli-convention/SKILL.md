---
name: r-cli-convention
description: House pattern for the R command-line scripts in scripts/R_cli/*_cli.r - the testable <name>_cli(args) function, the sourceable-and-runnable guard, argparser gotchas including multi-value arguments, resolving --input into a file list, required-argument checking, sourcing the sourced-not-installed scripts/R/ helpers, and the loud-failure rules this repo needs. Use when adding, editing or reviewing any R CLI here.
---

# R CLI convention

Adapted from the `r-cli-convention` skill in the lab's RNA-seq pipeline repo. The
argparser behaviour below was **re-measured on argparser 0.7.3 under R 4.6** in
this repo, not inherited.

## House style

Explicit `return()` on every exit, braces on single-line `if` bodies, named
arguments at call sites, and roxygen2 blocks on the helpers even though this is
not a package. Defaults live in one `dflt_args` list rather than inline at each
`.cli_param_for()` call.

## Layout

- CLIs live at **`scripts/R_cli/<name>_cli.r`**, flat.
- The functions they call live in **`scripts/R/`** — a directory of `.r` files
  that is **`source()`d, not installed**. It is not a package, so `pkg::fn` does
  not work and never will.
- The file defines **`<name>_cli(args)`** plus its private helpers. Nothing else
  runs at top level except the guard.
- `#!/usr/bin/env Rscript` shebang.

## The testable-function pattern

Put all parsing and orchestration inside
`<name>_cli(args = commandArgs(trailingOnly = TRUE))` and parse with
`parse_args(parser, argv = args)`. Because the argument vector is a parameter, a
test calls `annotate_features_cli(c("--input", f, "--outdir", d))` directly.

This is the only reason these CLIs are testable at all. A script that parses at
top level cannot be tested without running it as a subprocess.

## Run guard

```r
if (!interactive() && sys.nframe() == 0L) annotate_features_cli()
```

`sys.nframe() == 0L` is true only when the file is executed, not when it is
`source()`d — R's `if __name__ == "__main__"`. Tests source the file to get the
function without triggering a run.

## argparser gotchas

All measured on **0.7.3**; each line below was observed, not assumed.

- `arg_parser("...", hide.opts = TRUE)` drops the default `--opts` argument.
- **`help` is mandatory.** Omitting it fails inside `stopifnot` with
  `argument "help" is missing, with no default`, naming neither the argument nor
  the real problem.
- **Give every optional argument an explicit `short`.** With `--outdir` and
  `--output_prefix` registered, argparser raises no error, `-o` silently
  resolves to whichever was registered **first**, and the other is unreachable
  by short name. Capitals disambiguate (`-O`, `-P`).
- Booleans are `flag = TRUE`, default `FALSE`. For a default-TRUE option expose
  a **negating** flag (`--no_qc_plot`) rather than a positive one nobody can
  switch off. Prefer designing so the default is FALSE and the flag is positive.
- Unset character arguments come back as `NA` (logical, length 1).

### Multi-value (space-separated) arguments

`nargs = Inf` — **and the default must be `NULL`, `NA`, or absent.**

⚠️ **A default of length N silently pins the arity at N.** `add_argument()`
accepts it; only `parse_args()` complains, and it blames the extra *values*, not
the default that caused it:

```r
# WRONG - the length-1 default pins arity at 1
add_argument(p, "--feature", help="...", type="character", nargs=Inf, default="nucleus")
#   --feature nucleus nucleolus
#   -> Error: Extra arguments supplied: expecting 0 values but got 1 values: (nucleolus).

# RIGHT
add_argument(p, "--feature", help="...", type="character", nargs=Inf, default=NULL)
```

Measured shapes with `default = NULL`:

```
--words a b          -> c("a", "b")
--words a            -> "a"
--words ''           -> character(0)      <- length 0, NOT ""
(absent)             -> NA                <- length-1 LOGICAL
--words a b --flag   -> c("a", "b")       <- a trailing flag does not get eaten
--words -1 1         -> c("-1", "1")      <- a leading negative is fine
--words 'a*.txt'     -> "a*.txt"          <- globs arrive intact; see below
```

### ⚠️ A comma is RESERVED inside a value

argparser splits an `nargs = Inf` value on commas **even when the shell delivered
it as a single argv element**. Measured:

```
--words 'cell,type'      ->  cell | type          (two values from one element)
--words 'cell,type' b    ->  cell | type | b
--words 'cell type' b    ->  cell type | b        (a SPACE is fine)
```

Quoting does not help — a comma is shell-safe, so the split happens past the
shell, inside argparser. Consequences here:

- Never use a comma as a separator in any CLI value. Space-separate instead.
- Where a value needs internal structure, use `=` or `:`
  (`'default=3' 'nucleolus=1'`), never `,`.
- **A file path containing a comma will be split in half.** Paths like
  `Position010,rep2_nucleus_outline.txt` silently become two nonexistent paths.
  The input resolver must check existence and fail loudly.

### ⚠️ `Rscript -e` halves backslashes

Not argparser, but the same family of trap: it bites when driving these CLIs
from a shell script. **`Rscript -e` strips one level of backslash escaping
before R parses the string**, so a regex that is correct in a `.R` file is a
syntax error inline. Measured:

```bash
Rscript -e 'cat(grepl("a\.b",     "a.b"))'   # Error: '\.' is an unrecognized escape
Rscript -e 'cat(grepl("a\\.b",    "a.b"))'   # Error: '\.' is an unrecognized escape
Rscript -e 'cat(grepl("a\\\\.b",  "a.b"))'   # TRUE
Rscript file.R                               # TRUE with TWO backslashes in the file
```

Single-quoting does not help — the shell passes the string through untouched
and the stripping happens inside `Rscript`. The error names `<input>` and a line
number that refers to the `-e` text, not to any file, which is the tell.

- **Put anything containing a regex in a `.R` file** and call
  `Rscript path/to/file.R`. Do not count backslashes.
- If it must be inline, either double them again (`\\\\.`) or dodge the issue
  entirely with a character class: `"_features[.]rds$"` needs no backslash at
  all.

### The three "nothing given" shapes

```
absent  : class logical   len 1   all(is.na()) TRUE
''      : class character len 0   all(is.na()) TRUE   (vacuously)
unset   : NA
```

Normalise them in one shared helper, `.cli_resolve_arg()`, which also **refuses a
value beginning with `--`** — that means the preceding flag was rendered empty
and swallowed the next one.

## Required arguments

argparser has no `required=`. Validate from one list:

```r
required <- c("input", "outdir")
# all(): a multi-value argument holds 2+ elements, and is.na() on those returns a
# vector, which `if` refuses in R >= 4.2 ("the condition has length > 1").
missing <- required[vapply(required, function(a) all(is.na(argv[[a]])), logical(1))]
if (length(missing)) {
  stop("Missing required argument(s): ", paste0("--", missing, collapse = ", "), call. = FALSE)
}
```

## Resolving `--input` into a file list

One `--input` argument with `nargs = Inf` covers every form. Resolve each
element by what it is:

```
--input a/x_nucleus_outline.txt        explicit file
--input a/*_nucleus_outline.txt        shell expands; R receives many paths
--input 'a/*_nucleus_outline.txt'      quoted; Sys.glob() in R
--input a/                             directory; scan by the output contract
```

Rules, in order: a path that `dir.exists()` is scanned; an element containing
`*`, `?` or `[` goes through `Sys.glob()`; anything else must exist literally.
Then de-duplicate and sort.

⚠️ **Zero resolved files must be an error, never a quiet success.** The
characteristic failure in this repo is a silent no-op — a join that matches
nothing, a glob that expands to nothing — and it looks exactly like a clean run.
Every resolver reports how many files each element contributed.

## Warnings are the output

Set `options(warn = 1)` at the top of the CLI function and restore on exit:

```r
.warn_option <- options(warn = 1)
on.exit(options(.warn_option), add = TRUE)
```

Deferred, R prints "There were N warnings" and **hides the text past ten of
them** — so the run with the most wrong with it says the least. For a stage whose
warnings are its entire feedback channel (a feature that grouped to nothing, an
ROI dropped for too few vertices), that is backwards. Restoring on exit keeps a
test session as it found it.

## Sourcing `scripts/R/`

`scripts/R/` is sourced, not installed. Take the path as `--rlib_path` and
default to it relative to the running script:

```r
.source_rlib <- function(dir = NA) {
  if (exists("polygonize_roi_df", mode = "function")) return(invisible(NULL))  # tests pre-source
  if (is.na(dir)) {
    cmd_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", cmd_args, value = TRUE)
    if (!length(file_arg)) stop("cannot locate scripts/R; pass --rlib_path", call. = FALSE)
    dir <- file.path(dirname(normalizePath(sub("^--file=", "", file_arg[1]))), "..", "R")
  }
  if (!dir.exists(dir)) stop("scripts/R directory not found: ", dir, call. = FALSE)
  for (f in sort(list.files(dir, pattern = "[.][Rr]$", full.names = TRUE))) {
    sys.source(f, envir = globalenv())
  }
}
```

⚠️ **One global namespace.** Everything sourced lands in `globalenv()`, so a
CLI-private helper can silently replace a library function of the same name.
Prefix CLI-private helpers with `.` and keep them distinct.

⚠️ Files are sourced **alphabetically**, so a top-level constant that reads
another file's constant captures it before it exists. Make it a function instead.

⚠️ Several files in `scripts/R/` end in an `if(F){ ... }` test block. That is
inert, but it means sourcing pulls in their library expectations — `dplyr`,
`sf`, `stringr` must already be attached.

## Declared relationships, not hardcoded biology

Where one feature contains another, the caller declares it as `child=parent`
tokens (`--within 'nucleolus=nucleus'`) reusing `.cli_key_values()`. Nothing in
the code knows that a nucleolus belongs in a nucleus. Validate the spec as a
forest — no self-reference, no cycles, no child with two parents — and validate
it **once against the whole table**, not per sample: a sample legitimately
missing a feature type is a warning about the data, not a broken spec.

## Repo-specific rules

- **`sf` needs R 4.6 here**; 4.4's CRAN binary of `units` aborts the process.
  A CLI that touches geometry should fail with a clear message rather than a
  segfault — check `requireNamespace("sf")` early.
- **`polygonize_roi_df()` and `define_feature_group()` return a plain tibble
  with an `sfc` column, not an `sf` object.** `st_as_sf()` before any `sf`
  method. `summarise()` on the un-registered tibble drops geometry silently
  instead of unioning it.
- **Never write a comma into an output column** that another CLI reads back as
  a multi-value argument.
- **A relaxation must be recorded, not just applied.** Containment matching
  falls back to a parent's 2D footprint when the parent was not detected on a
  slice; the result carries `parent_match = "gap_filled"` so the amount of the
  answer resting on that fallback is visible. The same reasoning as printing
  the effective per-feature parameters rather than leaving defaults implicit.

## Testing

`tests/testthat/test-<name>_cli.r`, run by `tests/run_tests.R`. testthat sets
the working directory to the test file's own directory, so anchor every path on
the repo root:

```r
.repo <- function(...) {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  file.path(root, ...)
}
```

- Source the CLI once at file scope; the run guard means that does not execute it.
- ⚠️ **This repo runs testthat edition 2**, because it is a collection of scripts
  with no package `DESCRIPTION` to declare `Config/testthat/edition: 3`. Under
  edition 2 `expect_warning()` returns the **expression's value**, not the
  condition — `conditionMessage()` on the result fails with *"no applicable
  method for 'conditionMessage'"*. (Edition 3 returns the condition; the
  inherited version of this skill described that behaviour, which does not apply
  here.) Assert the text through the `regexp` argument instead, and take the
  value from a separate `suppressWarnings()` call:

  ```r
  expect_warning(f(), "matched nothing")
  got <- suppressWarnings(f())
  ```
- A CLI that emits progress `message()`s is not silent: use
  `expect_no_warning(suppressMessages(...))`, not `expect_silent()`.
- Write outputs to `withr::local_tempdir()`, never into the repo.
