# Feature identity read from a file's CONTENT, plus the area ranges and rename
# that depend on it.
#
# The theme: the outline table already knows what it holds -- the `name` column
# is the sample and the ROI id prefix is the feature -- so the filename does not
# have to be parsed for identity, only matched for selection. Everything here
# pins one consequence of that.

source_cli("cli_helpers.r")

# A minimal outline table in the real shape. Three vertices per ROI, so the
# polygon is valid; the caller varies the parts under test.
write_outline <- function(path, sample = "S1", feature = "nucleus",
                          rois = c("0001-0001-0433", "0002-0001-0433"),
                          z = NULL) {
  if (is.null(z)) z <- seq_along(rois)
  rows <- do.call(rbind, lapply(seq_along(rois), function(i) {
    data.frame(name = sample,
               roi  = paste0(feature, "_", rois[i]),
               z    = z[i],
               x    = c(0, 10, 10, 0),
               y    = c(0, 0, 10, 10),
               stringsAsFactors = FALSE)
  }))
  write.table(rows, path, sep = "\t", quote = FALSE, row.names = FALSE)
  return(path)
}

# --- the ROI id is the feature's home ------------------------------------------

test_that("the feature name is recovered from the ROI id, multi-word included", {
  expect_identical(.cli_feature_from_roi("nucleus_0001-0001-0433"), "nucleus")
  # The reason a GREEDY prefix is right here: the tail is fixed-shape and
  # anchored, so it cannot swallow part of the name.
  expect_identical(.cli_feature_from_roi("growing_oocyte_0012-0003-1884"), "growing_oocyte")
  expect_identical(.cli_feature_from_roi("a_b_c_d_9999-9999-9999"), "a_b_c_d")
})

test_that("ids that are not ROI ids are ignored rather than guessed at", {
  expect_identical(.cli_feature_from_roi(c("nonsense", "also-nonsense")), character(0))
  expect_identical(.cli_feature_from_roi(c("nucleus_0001-0001-0433", "junk")), "nucleus")
  expect_identical(.cli_feature_from_roi(character(0)), character(0))
  # Too few digits: not the contract, so not a match.
  expect_identical(.cli_feature_from_roi("nucleus_1-1-1"), character(0))
})

# --- identity from content ------------------------------------------------------

test_that("sample and feature come out of the file, not its name", {
  d <- withr::local_tempdir()
  p <- write_outline(file.path(d, "utterly_unrelated_name.txt"),
                     sample = "GRV_Position010", feature = "nucleus")
  id <- .cli_identify_outline(p)
  expect_true(id$ok)
  expect_identical(id$sample, "GRV_Position010")
  expect_identical(id$feature, "nucleus")
})

test_that("a file mixing feature types is an error, never a silent pick", {
  d <- withr::local_tempdir()
  p <- file.path(d, "mixed.txt")
  rows <- rbind(
    data.frame(name = "S1", roi = "nucleus_0001-0001-0433",   z = 1, x = 0, y = 0),
    data.frame(name = "S1", roi = "nucleolus_0001-0001-0433", z = 1, x = 1, y = 1))
  write.table(rows, p, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(.cli_identify_outline(p), "mixes feature types")
})

test_that("a file mixing sample names is an error too", {
  d <- withr::local_tempdir()
  p <- file.path(d, "mixed_sample.txt")
  rows <- rbind(
    data.frame(name = "S1", roi = "nucleus_0001-0001-0433", z = 1, x = 0, y = 0),
    data.frame(name = "S2", roi = "nucleus_0002-0001-0433", z = 2, x = 1, y = 1))
  write.table(rows, p, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(.cli_identify_outline(p), "mixes sample names")
})

test_that("a table without a usable name column reports why, rather than throwing", {
  d <- withr::local_tempdir()
  p <- file.path(d, "no_name.txt")
  rows <- data.frame(roi = "nucleus_0001-0001-0433", z = 1, x = 0, y = 0)
  write.table(rows, p, sep = "\t", quote = FALSE, row.names = FALSE)
  id <- .cli_identify_outline(p)
  expect_false(id$ok)
  expect_match(id$why, "name column")
})

# --- scanning inputs -------------------------------------------------------------

test_that("scan_inputs prefers content and reports the source", {
  d <- withr::local_tempdir()
  p <- write_outline(file.path(d, "anything.txt"), sample = "S1", feature = "nucleus")
  jobs <- .cli_scan_inputs(p, "nucleus")
  expect_identical(nrow(jobs), 1L)
  expect_identical(jobs$sample, "S1")
  expect_identical(jobs$feature, "nucleus")
  expect_identical(jobs$roi_prefix, "nucleus")
  expect_identical(jobs$from, "content")
})

test_that("a file holding an unrequested feature is dropped, and said so", {
  d <- withr::local_tempdir()
  a <- write_outline(file.path(d, "a.txt"), sample = "S1", feature = "nucleus")
  b <- write_outline(file.path(d, "b.txt"), sample = "S1", feature = "nucleolus")
  jobs <- expect_message(.cli_scan_inputs(c(a, b), "nucleus"), "not requested")
  expect_identical(nrow(jobs), 1L)
  expect_identical(jobs$roi_prefix, "nucleus")
})

test_that("asking for a feature no file holds names what was found instead", {
  d <- withr::local_tempdir()
  a <- write_outline(file.path(d, "a.txt"), sample = "S1", feature = "nucleus")
  expect_error(.cli_scan_inputs(a, "cytoplasm"), "Found instead: nucleus")
})

test_that("a file whose content cannot be read falls back to its name", {
  d <- withr::local_tempdir()
  p <- file.path(d, "S9_nucleus_outline.txt")
  # No `name` column, so the content probe cannot supply a sample.
  rows <- data.frame(roi = "nucleus_0001-0001-0433", z = 1, x = 0, y = 0)
  write.table(rows, p, sep = "\t", quote = FALSE, row.names = FALSE)
  jobs <- .cli_scan_inputs(p, "nucleus")
  expect_identical(jobs$from, "filename")
  expect_identical(jobs$sample, "S9")
})

# --- the bug this replaces --------------------------------------------------------

test_that("the filename parser no longer mis-splits a multi-word feature", {
  # Greedy (.*) filed S1_growing_oocyte_outline.txt under sample "S1_growing",
  # feature "oocyte" -- silently, and reordering the alternation did not help.
  got <- .cli_parse_contract("S1_growing_oocyte_outline.txt",
                             c("oocyte", "growing_oocyte"))
  expect_identical(got$sample, "S1")
  expect_identical(got$feature, "growing_oocyte")

  # The single-word case still parses as it always did.
  got2 <- .cli_parse_contract("GRV_Position010_nucleus_outline.txt", "nucleus")
  expect_identical(got2$sample, "GRV_Position010")
  expect_identical(got2$feature, "nucleus")
})

# --- rename -----------------------------------------------------------------------

test_that("rename changes the reporting name and leaves the ROI matcher alone", {
  jobs <- data.frame(path = "p", sample = "S1", roi_prefix = "nucleus",
                     from = "content", feature = "nucleus",
                     stringsAsFactors = FALSE)
  out <- expect_message(.cli_apply_rename(jobs, c(nucleus = "oocyte")), "nucleus -> oocyte")
  expect_identical(out$feature, "oocyte")
  # The ROI ids in the file still say "nucleus", so this must NOT move.
  expect_identical(out$roi_prefix, "nucleus")
})

test_that("renaming something absent warns instead of passing silently", {
  jobs <- data.frame(path = "p", sample = "S1", roi_prefix = "nucleus",
                     from = "content", feature = "nucleus",
                     stringsAsFactors = FALSE)
  expect_warning(.cli_apply_rename(jobs, c(cytoplasm = "cell")), "not present")
})

# --- ranges -------------------------------------------------------------------------

test_that("key=lo:hi ranges parse, including open ends", {
  r <- .cli_key_ranges(c("nucleus=80:Inf", "nucleolus=3:150"), "--roi_area")
  expect_equal(r$nucleus, c(80, Inf))
  expect_equal(r$nucleolus, c(3, 150))

  expect_equal(.cli_key_ranges("nucleus=80:", "--roi_area")$nucleus, c(80, Inf))
  expect_equal(.cli_key_ranges("nucleus=:100", "--roi_area")$nucleus, c(0, 100))
  # A bare token files under "default", like the other key=value flags.
  expect_equal(.cli_key_ranges("5:10", "--roi_area")$default, c(5, 10))
})

test_that("malformed ranges are rejected with the offending value", {
  expect_error(.cli_key_ranges("nucleus=80", "--roi_area"), "must be 'lo:hi'")
  expect_error(.cli_key_ranges("nucleus=a:b", "--roi_area"), "not numeric")
  expect_error(.cli_key_ranges("nucleus=100:10", "--roi_area"), "lo > hi")
  expect_error(.cli_key_ranges("nucleus=1:2:3", "--roi_area"), "exactly one")
})

test_that("a range falls back to 'default', then to nothing", {
  r <- .cli_key_ranges(c("default=1:2", "nucleus=3:4"), "--roi_area")
  expect_equal(.cli_range_for(r, "nucleus"), c(3, 4))
  expect_equal(.cli_range_for(r, "nucleolus"), c(1, 2))
  # NULL, not c(0, Inf): a caller must be able to tell "not given" from
  # "given as everything".
  expect_null(.cli_range_for(list(), "nucleus"))
  expect_null(.cli_range_for(.cli_key_ranges("nucleus=3:4", "--x"), "cytoplasm"))
})

# --- valid-row predicate ---------------------------------------------------------

test_that("valid rows are recognised for ANY feature name", {
  df <- data.frame(
    feature_type = c("oocyte", "oocyte", "oocyte", "growing_oocyte"),
    feature_id   = c("oocyte_1", "invalid_oocyte_1", "failed_oocyte_area",
                     "growing_oocyte_2"),
    stringsAsFactors = FALSE)
  keep <- .cli_valid_rows(df)
  expect_identical(keep$feature_id, c("oocyte_1", "growing_oocyte_2"))
})

test_that("an NA feature_id is not a valid feature", {
  df <- data.frame(feature_type = "oocyte", feature_id = NA_character_,
                   stringsAsFactors = FALSE)
  expect_identical(nrow(.cli_valid_rows(df)), 0L)
})

# --- the feature-level area range, against the fixture -----------------------------

n_valid <- function(rds, prefix) {
  f <- readRDS(rds)
  length(unique(f$feature_id[grepl(paste0("^", prefix, "_\\d+$"), f$feature_id)]))
}

run_annotate <- function(outdir, ...) {
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_file("nucleus", "outline"),
    "--outdir", outdir, "--feature", "nucleus", ...)))
  return(file.path(outdir, "GRV_Position010_features.rds"))
}

test_that("feature_area_range filters on the feature, and its UPPER bound works", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")

  base <- n_valid(run_annotate(withr::local_tempdir()), "nucleus")
  expect_gt(base, 0)

  # A lower bound keeps only the larger features...
  hi <- n_valid(run_annotate(withr::local_tempdir(),
                             "--feature_area", "nucleus=150:Inf"), "nucleus")
  expect_lt(hi, base)

  # ...and an upper bound is the case min_avg_area could not express at all.
  lo <- n_valid(run_annotate(withr::local_tempdir(),
                             "--feature_area", "nucleus=0:100"), "nucleus")
  expect_lt(lo, base)
})

test_that("roi_area acts BEFORE grouping, so it can change the feature count", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")

  base <- n_valid(run_annotate(withr::local_tempdir()), "nucleus")
  cut  <- n_valid(run_annotate(withr::local_tempdir(),
                               "--roi_area", "nucleus=100:Inf"), "nucleus")
  expect_false(identical(base, cut))
})

test_that("min_avg_area still works, and the two bounds combine", {
  skip_if_no_sf()
  source_r_scripts(c("FnGroup_roi_2_polygons.r", "find_ROI_z_intersect.r",
                     "define_feature_group.r"))
  o <- read.table(fixture_file("nucleus", "outline"), header = TRUE,
                  sep = "\t", stringsAsFactors = FALSE)
  o <- o[, c("roi", "z", "x", "y")]

  none <- define_feature_group(o, max_z_dist = 3, min_z_span = 5,
                               feature_prefix = "nucleus_")
  n_none <- length(unique(none$feature_id[grepl("^nucleus_\\d+$", none$feature_id)]))

  old <- define_feature_group(o, max_z_dist = 3, min_z_span = 5,
                              min_avg_area = 150, feature_prefix = "nucleus_")
  n_old <- length(unique(old$feature_id[grepl("^nucleus_\\d+$", old$feature_id)]))

  new <- define_feature_group(o, max_z_dist = 3, min_z_span = 5,
                              feature_area_range = c(150, Inf),
                              feature_prefix = "nucleus_")
  n_new <- length(unique(new$feature_id[grepl("^nucleus_\\d+$", new$feature_id)]))

  expect_lt(n_old, n_none)          # the old argument still bites
  expect_identical(n_new, n_old)    # and the range spells the same thing
})

test_that("a bad feature_area_range is refused rather than half-applied", {
  skip_if_no_sf()
  source_r_scripts(c("FnGroup_roi_2_polygons.r", "find_ROI_z_intersect.r",
                     "define_feature_group.r"))
  o <- read.table(fixture_file("nucleus", "outline"), header = TRUE,
                  sep = "\t", stringsAsFactors = FALSE)[, c("roi", "z", "x", "y")]
  expect_error(define_feature_group(o, feature_area_range = 150), "length-2")
})

# --- rename, end to end --------------------------------------------------------------

test_that("a renamed feature still matches its ROIs and relabels every id", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")

  plain <- run_annotate(withr::local_tempdir())
  ren   <- run_annotate(withr::local_tempdir(), "--rename", "nucleus=oocyte")

  # Same objects found: renaming must not change what was detected. If the ROI
  # matcher had followed the new name it would have matched nothing and this
  # would read 0.
  expect_identical(n_valid(ren, "oocyte"), n_valid(plain, "nucleus"))
  expect_gt(n_valid(ren, "oocyte"), 0)

  f <- readRDS(ren)
  expect_identical(unique(f$feature_type), "oocyte")
  expect_true(all(grepl("oocyte", f$feature_id[!is.na(f$feature_id)])))
  expect_false(any(grepl("nucleus", f$feature_id[!is.na(f$feature_id)])))
  # The ROI ids themselves are Fiji's and must be untouched.
  expect_true(all(grepl("^nucleus_", f$roi)))
})
