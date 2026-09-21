# Argument handling for the CLIs in scripts/R_cli/.
#
# These assert the shapes argparser actually produces (measured on 0.7.3), not
# the shapes it would be convenient for it to produce. The three "nothing given"
# forms in particular are genuinely different objects.

source_cli("cli_helpers.r")

# --- .cli_multi ---------------------------------------------------------------

test_that(".cli_multi normalises every 'nothing given' shape to character(0)", {
  expect_identical(.cli_multi(NA), character(0))          # flag absent
  expect_identical(.cli_multi(character(0)), character(0)) # --flag ''
  expect_identical(.cli_multi(NULL), character(0))
  expect_identical(.cli_multi(c(NA, NA)), character(0))
  expect_identical(.cli_multi(""), character(0))
  expect_identical(.cli_multi("   "), character(0))
})

test_that(".cli_multi keeps real values and trims them", {
  expect_identical(.cli_multi(c("a", "b")), c("a", "b"))
  expect_identical(.cli_multi(c(" a ", "b")), c("a", "b"))
  expect_identical(.cli_multi(c("a", NA, "b")), c("a", "b"))
  # A value containing a space stays one element -- that is the whole reason
  # the convention is space-separated arguments rather than comma-joined ones.
  expect_identical(.cli_multi("cell type"), "cell type")
})

test_that(".cli_multi refuses a value that is really a flag", {
  # This happens when a preceding argument rendered empty and swallowed the
  # next flag. Accepting it silently turns a malformed command into a run.
  expect_error(.cli_multi(c("a", "--outdir")), "look like flags")
  expect_error(.cli_multi("--qc_plot", "--feature"), "--feature")
})

# --- .cli_key_values ----------------------------------------------------------

test_that(".cli_key_values parses key=value tokens", {
  expect_identical(.cli_key_values(c("default=3", "nucleolus=1")),
                   c(default = "3", nucleolus = "1"))
  # A bare token is the default
  expect_identical(.cli_key_values("5"), c(default = "5"))
  expect_identical(.cli_key_values(NA), character(0))
})

test_that(".cli_key_values rejects malformed token sets", {
  expect_error(.cli_key_values(c("a=1", "a=2")), "Duplicate key")
  expect_error(.cli_key_values("=3"), "Empty key")
})

test_that(".cli_key_values keeps a value containing '='", {
  expect_identical(.cli_key_values("note=a=b"), c(note = "a=b"))
})

# --- .cli_param_for -----------------------------------------------------------

test_that(".cli_param_for prefers the feature, then default, then the fallback", {
  kv <- c(default = "3", nucleolus = "1")
  expect_identical(.cli_param_for(kv, "nucleolus", 99), 1)
  expect_identical(.cli_param_for(kv, "nucleus", 99), 3)     # via default=
  expect_identical(.cli_param_for(character(0), "nucleus", 99), 99)
  expect_identical(.cli_param_for(c(nucleolus = "1"), "nucleus", 99), 99)
})

test_that(".cli_param_for refuses a non-numeric value", {
  expect_error(.cli_param_for(c(default = "many"), "nucleus", 1, "--min_z_span"),
               "not a number")
})

test_that(".cli_param_for accepts zero", {
  # Zero is a legitimate value for min_intersect_ratio. An Elvis-style
  # "value or default" test would silently replace it.
  expect_identical(.cli_param_for(c(default = "0"), "nucleus", 5), 0)
})

# --- .cli_resolve_input -------------------------------------------------------

make_input_tree <- function(){
  d <- withr::local_tempdir(.local_envir = parent.frame())
  for(f in c("S1_nucleus_outline.txt", "S2_nucleus_outline.txt",
             "S1_nucleolus_outline.txt", "notes.md")){
    writeLines("x", file.path(d, f))
  }
  d
}

test_that(".cli_resolve_input scans a directory by the output contract", {
  d <- make_input_tree()
  got <- .cli_resolve_input(d, "_(nucleus|nucleolus)_outline\\.txt$")
  expect_length(got, 3)
  expect_false(any(grepl("notes.md", got)))
})

test_that(".cli_resolve_input expands a quoted glob", {
  d <- make_input_tree()
  got <- .cli_resolve_input(file.path(d, "*_nucleus_outline.txt"),
                            "_(nucleus|nucleolus)_outline\\.txt$")
  expect_length(got, 2)
})

test_that(".cli_resolve_input accepts explicit files and de-duplicates", {
  d <- make_input_tree()
  f <- file.path(d, "S1_nucleus_outline.txt")
  got <- .cli_resolve_input(c(f, f), "_outline\\.txt$")
  expect_length(got, 1)
})

test_that(".cli_resolve_input fails loudly rather than resolving to nothing", {
  # The characteristic failure in this repo is the silent no-op: a run that
  # produces nothing and says it succeeded.
  d <- withr::local_tempdir()
  expect_error(.cli_resolve_input(file.path(d, "absent.txt"), "_outline\\.txt$"),
               "No such file")
  expect_error(suppressWarnings(
    .cli_resolve_input(file.path(d, "*_nucleus_outline.txt"), "_outline\\.txt$")),
    "No input files resolved")
  expect_error(.cli_resolve_input(NA, "x"), "No value given")
})

test_that(".cli_resolve_input warns when a glob matches nothing", {
  d <- make_input_tree()
  # NB: under testthat edition 2 -- which is what this repo runs, having no
  #     package DESCRIPTION to declare edition 3 -- expect_warning() returns the
  #     EXPRESSION'S VALUE, not the condition. So assert the text through the
  #     regexp argument; conditionMessage() on the result fails with
  #     "no applicable method".
  expect_warning(
    .cli_resolve_input(c(file.path(d, "S1_nucleus_outline.txt"),
                         file.path(d, "*_cell_outline.txt")),
                       "_outline\\.txt$"),
    "_cell_outline")
  got <- suppressWarnings(
    .cli_resolve_input(c(file.path(d, "S1_nucleus_outline.txt"),
                         file.path(d, "*_cell_outline.txt")),
                       "_outline\\.txt$"))
  expect_length(got, 1)   # the one real file still comes back
})

test_that(".cli_resolve_input explains a comma-split path", {
  d <- withr::local_tempdir()
  expect_error(.cli_resolve_input("S1,rep2_nucleus_outline.txt", "_outline\\.txt$"),
               "comma in a path")
})

# --- .cli_parse_contract ------------------------------------------------------

test_that(".cli_parse_contract splits sample and feature", {
  got <- .cli_parse_contract(
    c("/x/GRV_Position010_nucleus_outline.txt",
      "/x/GRV_Position010_nucleolus_outline.txt"),
    c("nucleus", "nucleolus"))
  expect_identical(got$sample, rep("GRV_Position010", 2))
  expect_identical(sort(got$feature), c("nucleolus", "nucleus"))
})

test_that(".cli_parse_contract does not silently drop unmatched files", {
  expect_warning(
    .cli_parse_contract(c("/x/S1_nucleus_outline.txt", "/x/S1_cell_outline.txt"),
                        "nucleus"),
    "not matching")
})

test_that(".cli_parse_contract errors when nothing names a requested feature", {
  expect_error(suppressWarnings(
    .cli_parse_contract("/x/S1_cell_outline.txt", "nucleus")),
    "name a requested feature")
})

test_that(".cli_parse_contract keeps a sample name containing the feature word", {
  # "nucleus_test" as a sample prefix must not confuse the split; the regex is
  # anchored on the LAST _<feature>_outline.txt.
  got <- .cli_parse_contract("/x/nucleus_test_nucleus_outline.txt", "nucleus")
  expect_identical(got$sample, "nucleus_test")
  expect_identical(got$feature, "nucleus")
})

# --- sample sheet -------------------------------------------------------------

write_sheet <- function(dir, df){
  p <- file.path(dir, "samples.tsv")
  write.table(df, p, sep = "\t", quote = FALSE, row.names = FALSE)
  p
}

test_that(".cli_read_sample_sheet requires the id column and rejects duplicates", {
  d <- withr::local_tempdir()
  ok <- write_sheet(d, data.frame(prefix = c("S1", "S2"), genotype = c("wt", "ko")))
  expect_identical(nrow(.cli_read_sample_sheet(ok)), 2L)

  bad <- write_sheet(d, data.frame(sample = "S1"))
  expect_error(.cli_read_sample_sheet(bad), "no 'prefix' column")

  dup <- write_sheet(d, data.frame(prefix = c("S1", "S1")))
  expect_error(.cli_read_sample_sheet(dup), "Duplicate")
})

test_that(".cli_apply_sample_sheet filters inputs and reports both mismatches", {
  contract <- data.frame(path = c("a", "b"), sample = c("S1", "S2"),
                         feature = "nucleus", stringsAsFactors = FALSE)
  sheet <- data.frame(prefix = c("S1", "S3"), stringsAsFactors = FALSE)

  expect_warning(suppressMessages(.cli_apply_sample_sheet(contract, sheet)),
                 "matched no input file")
  expect_warning(suppressMessages(.cli_apply_sample_sheet(contract, sheet)), "S3")

  got <- suppressWarnings(suppressMessages(.cli_apply_sample_sheet(contract, sheet)))
  expect_identical(got$sample, "S1")   # S2 dropped: not in the sheet
})

test_that(".cli_apply_sample_sheet errors when nothing overlaps", {
  contract <- data.frame(path = "a", sample = "S1", feature = "nucleus",
                         stringsAsFactors = FALSE)
  sheet <- data.frame(prefix = "OTHER", stringsAsFactors = FALSE)
  expect_error(suppressWarnings(suppressMessages(
    .cli_apply_sample_sheet(contract, sheet))), "no sample in common")
})
