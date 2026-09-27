# Argument handling for the CLIs in scripts/R_cli/.
#
# These assert the shapes argparser actually produces (measured on 0.7.3), not
# the shapes it would be convenient for it to produce. The three "nothing given"
# forms in particular are genuinely different objects.

source_cli("cli_helpers.r")

# --- .cli_resolve_arg ---------------------------------------------------------------

test_that(".cli_resolve_arg normalises every 'nothing given' shape to character(0)", {
  expect_identical(.cli_resolve_arg(NA), character(0))          # flag absent
  expect_identical(.cli_resolve_arg(character(0)), character(0)) # --flag ''
  expect_identical(.cli_resolve_arg(NULL), character(0))
  expect_identical(.cli_resolve_arg(c(NA, NA)), character(0))
  expect_identical(.cli_resolve_arg(""), character(0))
  expect_identical(.cli_resolve_arg("   "), character(0))
})

test_that(".cli_resolve_arg keeps real values and trims them", {
  expect_identical(.cli_resolve_arg(c("a", "b")), c("a", "b"))
  expect_identical(.cli_resolve_arg(c(" a ", "b")), c("a", "b"))
  expect_identical(.cli_resolve_arg(c("a", NA, "b")), c("a", "b"))
  # A value containing a space stays one element -- that is the whole reason
  # the convention is space-separated arguments rather than comma-joined ones.
  expect_identical(.cli_resolve_arg("cell type"), "cell type")
})

test_that(".cli_resolve_arg refuses a value that is really a flag", {
  # This happens when a preceding argument rendered empty and swallowed the
  # next flag. Accepting it silently turns a malformed command into a run.
  expect_error(.cli_resolve_arg(c("a", "--outdir")), "look like flags")
  expect_error(.cli_resolve_arg("--qc_plot", "--feature"), "--feature")
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

# --- .cli_resolve_input_path -------------------------------------------------------

make_input_tree <- function(){
  d <- withr::local_tempdir(.local_envir = parent.frame())
  for(f in c("S1_nucleus_outline.txt", "S2_nucleus_outline.txt",
             "S1_nucleolus_outline.txt", "notes.md")){
    writeLines("x", file.path(d, f))
  }
  d
}

test_that(".cli_resolve_input_path scans a directory by the output contract", {
  d <- make_input_tree()
  got <- .cli_resolve_input_path(d, "_(nucleus|nucleolus)_outline\\.txt$")
  expect_length(got, 3)
  expect_false(any(grepl("notes.md", got)))
})

test_that(".cli_resolve_input_path expands a quoted glob", {
  d <- make_input_tree()
  got <- .cli_resolve_input_path(file.path(d, "*_nucleus_outline.txt"),
                            "_(nucleus|nucleolus)_outline\\.txt$")
  expect_length(got, 2)
})

test_that(".cli_resolve_input_path accepts explicit files and de-duplicates", {
  d <- make_input_tree()
  f <- file.path(d, "S1_nucleus_outline.txt")
  got <- .cli_resolve_input_path(c(f, f), "_outline\\.txt$")
  expect_length(got, 1)
})

test_that(".cli_resolve_input_path fails loudly rather than resolving to nothing", {
  # The characteristic failure in this repo is the silent no-op: a run that
  # produces nothing and says it succeeded.
  d <- withr::local_tempdir()
  expect_error(.cli_resolve_input_path(file.path(d, "absent.txt"), "_outline\\.txt$"),
               "No such file")
  expect_error(suppressWarnings(
    .cli_resolve_input_path(file.path(d, "*_nucleus_outline.txt"), "_outline\\.txt$")),
    "No input files resolved")
  expect_error(.cli_resolve_input_path(NA, "x"), "No value given")
})

test_that(".cli_resolve_input_path warns when a glob matches nothing", {
  d <- make_input_tree()
  # NB: under testthat edition 2 -- which is what this repo runs, having no
  #     package DESCRIPTION to declare edition 3 -- expect_warning() returns the
  #     EXPRESSION'S VALUE, not the condition. So assert the text through the
  #     regexp argument; conditionMessage() on the result fails with
  #     "no applicable method".
  expect_warning(
    .cli_resolve_input_path(c(file.path(d, "S1_nucleus_outline.txt"),
                         file.path(d, "*_cell_outline.txt")),
                       "_outline\\.txt$"),
    "_cell_outline")
  got <- suppressWarnings(
    .cli_resolve_input_path(c(file.path(d, "S1_nucleus_outline.txt"),
                         file.path(d, "*_cell_outline.txt")),
                       "_outline\\.txt$"))
  expect_length(got, 1)   # the one real file still comes back
})

test_that(".cli_resolve_input_path explains a comma-split path", {
  d <- withr::local_tempdir()
  expect_error(.cli_resolve_input_path("S1,rep2_nucleus_outline.txt", "_outline\\.txt$"),
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

# --- include ------------------------------------------------------------------
# The same column is read by BatchRunner.isIncluded() on the Groovy side. These
# pin the vocabularies together: one sheet, two readers, and a word that means
# "run this" to one and not the other would show up only as a sample count.

test_that(".cli_is_included accepts exactly what the Groovy side accepts", {
  expect_identical(.cli_is_included(c("true", "yes", "1")), c(TRUE, TRUE, TRUE))
  expect_identical(.cli_is_included(c("false", "no", "0")), c(FALSE, FALSE, FALSE))
  expect_identical(.cli_is_included(c("TRUE", "False", " Yes ")), c(TRUE, FALSE, TRUE))
  # Blank, absent and NA are included: a sheet without the column must behave
  # exactly as it did before the column existed.
  expect_identical(.cli_is_included(c("", NA, "  ")), c(TRUE, TRUE, TRUE))
  # read.delim turns a column of TRUE/FALSE into logical, not character.
  expect_identical(.cli_is_included(c(TRUE, FALSE, NA)), c(TRUE, FALSE, TRUE))
  # ...and a column of 1/0 into integer.
  expect_identical(.cli_is_included(c(1L, 0L)), c(TRUE, FALSE))
  # "maybe" must not quietly mean one or the other.
  expect_error(.cli_is_included(c("true", "maybe")), "true/false")
})

test_that(".cli_read_sample_sheet honours include and does not carry it through", {
  d <- withr::local_tempdir()
  p <- file.path(d, "s.tsv")
  write.table(data.frame(prefix = c("A", "B", "C"),
                         include = c("true", "false", "true"),
                         condition = c("wt", "ko", "wt")),
              p, sep = "\t", quote = FALSE, row.names = FALSE)
  sheet <- suppressMessages(.cli_read_sample_sheet(p))
  expect_identical(sheet$prefix, c("A", "C"))
  # A control column, not metadata: left in, it would land on every output row
  # as a column that is TRUE everywhere by construction.
  expect_false("include" %in% colnames(sheet))
  expect_identical(sheet$condition, c("wt", "wt"))
})

test_that("an excluded row may duplicate an included one", {
  # The Groovy side refuses a duplicate prefix only among INCLUDED rows, and
  # its error says to set include=false on all but one. That advice has to work
  # here, or the two ends disagree about the same file.
  d <- withr::local_tempdir()
  p <- file.path(d, "s.tsv")
  write.table(data.frame(prefix = c("A", "A"), include = c("true", "false")),
              p, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_identical(nrow(suppressMessages(.cli_read_sample_sheet(p))), 1L)

  # Both included is still fatal.
  p2 <- file.path(d, "s2.tsv")
  write.table(data.frame(prefix = c("A", "A"), include = c("true", "true")),
              p2, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(suppressMessages(.cli_read_sample_sheet(p2)), "Duplicate")
})

test_that("a sheet with every row excluded is an error, not an empty run", {
  d <- withr::local_tempdir()
  p <- file.path(d, "s.tsv")
  write.table(data.frame(prefix = c("A", "B"), include = c("false", "no")),
              p, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(suppressMessages(.cli_read_sample_sheet(p)), "include=false")
})

# --- the schema file ----------------------------------------------------------

test_that("machine columns come from the schema and are not carried through", {
  machine <- .cli_sheet_machine_columns()
  skip_if(!length(machine), "schema/sheet_columns.tsv not found from here")
  # Read from schema/sheet_columns.tsv, not duplicated here: these are facts
  # about the image file that Make_SampleSheet rewrites on every regeneration.
  expect_true(all(c("size_x", "pixel_width", "file_size", "series_index") %in% machine))
  expect_false("prefix" %in% machine)

  d <- withr::local_tempdir()
  p <- file.path(d, "s.tsv")
  write.table(data.frame(prefix = "A", condition = "wt",
                         size_x = 2048L, pixel_width = 0.22, file_size = 99L),
              p, sep = "\t", quote = FALSE, row.names = FALSE)
  sheet <- suppressMessages(.cli_read_sample_sheet(p))
  expect_identical(colnames(sheet), c("prefix", "condition"))
  # Dropped loudly, not silently: somebody who wanted pixel_width should be
  # told where it went.
  expect_message(.cli_read_sample_sheet(p), "machine column")
})

# --- the Fiji _config.txt -----------------------------------------------------

test_that(".cli_read_config reads the two-column format, and blanks are NA", {
  d <- withr::local_tempdir()
  p <- file.path(d, "X_config.txt")
  writeLines(c("parameter\tvalue",
               "pixel_width\t0.2227",
               "pixel_depth\t0.9999",
               "output_prefix\t"), p)
  cfg <- .cli_read_config(p)
  expect_identical(cfg[["pixel_width"]], "0.2227")
  expect_equal(.cli_config_num(cfg, "pixel_depth"), 0.9999)
  # Absent and blank are both NA, never 0.
  expect_true(is.na(.cli_config_num(cfg, "nothing_here")))
  expect_true(is.na(.cli_config_num(cfg, "output_prefix")))

  expect_null(.cli_read_config(NA_character_))
  expect_null(.cli_read_config(file.path(d, "nope.txt")))
  # A file that is not this format is refused rather than half-read.
  q <- file.path(d, "other.txt")
  writeLines(c("a\tb", "1\t2"), q)
  expect_null(.cli_read_config(q))
})

test_that(".cli_find_config looks beside the tables, not only beside the .rds", {
  d <- withr::local_tempdir()
  sub <- file.path(d, "features"); dir.create(sub)
  writeLines(c("parameter\tvalue", "pixel_depth\t1"), file.path(d, "S_config.txt"))
  expect_true(is.na(.cli_find_config("S", sub)))
  expect_false(is.na(.cli_find_config("S", c(sub, d))))
  expect_true(is.na(.cli_find_config("missing", c(sub, d))))
})

test_that("the fixture predates pixel_depth, so no volume is inferred from it", {
  # Deliberately NOT regenerated: results written before 0.2.0 have no
  # pixel_depth, and the CLI has to degrade rather than invent a z step.
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  cfg <- .cli_read_config(.cli_find_config("GRV_Position010",
                                           dirname(fixture_file("nucleus", "outline"))))
  expect_false(is.null(cfg))
  expect_true(is.na(.cli_config_num(cfg, "pixel_depth")))
})
