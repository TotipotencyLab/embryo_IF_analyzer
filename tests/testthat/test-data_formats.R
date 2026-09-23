# The documented formats, asserted against the code that reads and writes them.
#
# A format description in prose drifts silently; the point of these is that it
# cannot. Each one pins something note/data_formats.md claims.

source_cli("cli_helpers.r")

# --- the shipped template -----------------------------------------------------

test_that("the sample sheet template loads through the real reader", {
  tmpl <- file.path(repo_root(), "config", "sample_sheet_template.tsv")
  expect_true(file.exists(tmpl))

  sheet <- .cli_read_sample_sheet(tmpl)
  expect_true("prefix" %in% colnames(sheet))
  expect_gt(nrow(sheet), 0)
  expect_false(anyDuplicated(sheet$prefix) > 0)
})

test_that("the template's first sample is the fixture, so it can be run as-is", {
  tmpl <- file.path(repo_root(), "config", "sample_sheet_template.tsv")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))

  sheet <- .cli_read_sample_sheet(tmpl)
  fixture_sample <- sub("_nucleus_outline\\.txt$", "",
                        basename(fixture_file("nucleus", "outline")))
  expect_true(fixture_sample %in% sheet$prefix)
})

test_that("only prefix is required; other columns are free metadata", {
  d <- withr::local_tempdir()
  p <- file.path(d, "minimal.tsv")
  write.table(data.frame(prefix = c("A", "B")), p,
              sep = "\t", quote = FALSE, row.names = FALSE)
  expect_identical(nrow(.cli_read_sample_sheet(p)), 2L)
})

test_that("the sample sheet reader accepts tsv, txt and csv", {
  d <- withr::local_tempdir()
  df <- data.frame(prefix = "A", genotype = "wt")
  for (ext in c("tsv", "txt")) {
    p <- file.path(d, paste0("s.", ext))
    write.table(df, p, sep = "\t", quote = FALSE, row.names = FALSE)
    expect_identical(.cli_read_sample_sheet(p)$genotype, "wt", info = ext)
  }
  p <- file.path(d, "s.csv")
  write.csv(df, p, row.names = FALSE)
  expect_identical(.cli_read_sample_sheet(p)$genotype, "wt")
})

# --- the Fiji output contract -------------------------------------------------

test_that("the fixture matches the documented outline columns", {
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  hdr <- names(read.table(fixture_file("nucleus", "outline"),
                          header = TRUE, nrows = 1, stringsAsFactors = FALSE))
  expect_identical(hdr, c("name", "roi", "z", "x", "y"))
})

test_that("ROI ids match the documented <feature>_SSSS-NNNN-YYYY form", {
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  o <- read.table(fixture_file("nucleus", "outline"), header = TRUE,
                  stringsAsFactors = FALSE)
  expect_true(all(grepl("^nucleus_\\d{4}-\\d{4}-\\d{4}$", unique(o$roi))))
})

test_that("read_fiji_result returns the documented columns", {
  skip_if_no_fixture(fixture_file("nucleus", "res"))
  source_r_scripts("read_fiji_result.r")
  res <- read_fiji_result(fixture_file("nucleus", "res"))
  expect_identical(
    colnames(res),
    c("label", "area", "mean", "stddev", "min", "max", "x", "y", "circ",
      "intden", "median", "rawintden", "ch", "slice", "ar", "round",
      "solidity", "filename", "roi", "pos", "z"))
})

test_that("the config file is a two-column parameter/value table", {
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  cfg <- file.path(fixture_dir(), "GRV_Position010_config.txt")
  skip_if_no_fixture(cfg)
  d <- read.delim(cfg, stringsAsFactors = FALSE)
  expect_identical(colnames(d), c("parameter", "value"))
  # Fields other code looks up by name.
  for (k in c("pixel_width", "pixel_height", "pixel_unit", "script")) {
    expect_true(k %in% d$parameter, info = k)
  }
})

# --- the R CLI outputs --------------------------------------------------------

test_that("annotate writes the documented columns", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")

  out <- withr::local_tempdir()
  tsv <- suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "nucleolus",
    "--outdir", out,
    "--max_z_dist", "default=3", "nucleolus=1",
    "--min_z_span", "default=5", "nucleolus=2",
    "--within", "nucleolus=nucleus")))

  documented <- c("roi", "z", "area", "feature_id", "feature_type", "sample",
                  "parent_feature_id", "parent_feature_type",
                  "parent_containment", "parent_match")
  expect_identical(colnames(tsv), documented)

  rds <- readRDS(file.path(out, "GRV_Position010_features.rds"))
  expect_setequal(colnames(rds), c(documented, "geometry"))
  expect_s3_class(rds, "sf")

  # parent_match takes only the documented values.
  expect_true(all(is.na(tsv$parent_match) |
                    tsv$parent_match %in% c("direct", "gap_filled")))
})

test_that("feature_id uses the documented vocabulary", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")

  out <- withr::local_tempdir()
  tsv <- suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "nucleolus",
    "--outdir", out,
    "--max_z_dist", "default=3", "nucleolus=1",
    "--min_z_span", "default=5", "nucleolus=2")))

  ids <- unique(tsv$feature_id[!is.na(tsv$feature_id)])
  ok <- grepl("^(nucleus|nucleolus)_\\d+$", ids) |
    grepl("^invalid_(nucleus|nucleolus)_\\d+$", ids) |
    grepl("^failed_(nucleus|nucleolus)_(excluded|name|area|overlap)$", ids)
  expect_true(all(ok), info = paste(ids[!ok], collapse = ", "))
})

test_that("count writes the documented columns", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("count_features_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))

  out <- withr::local_tempdir()
  counts <- suppressMessages(count_features_cli(c("--input", feat, "--outdir", out)))
  expect_identical(colnames(counts),
                   c("sample", "feature_type", "n_detected", "n_invalid",
                     "n_failed", "n_roi"))

  d <- withr::local_tempdir()
  sheet <- file.path(d, "s.tsv")
  write.table(data.frame(prefix = "GRV_Position010", genotype = "wt"),
              sheet, sep = "\t", quote = FALSE, row.names = FALSE)
  out2 <- withr::local_tempdir()
  suppressMessages(count_features_cli(c(
    "--input", feat, "--outdir", out2,
    "--sample_sheet", sheet, "--group_by", "genotype")))
  s <- read.delim(file.path(out2, "feature_counts_summary.tsv"),
                  stringsAsFactors = FALSE)
  expect_identical(colnames(s),
                   c("genotype", "feature_type", "n_sample", "mean_detected",
                     "sd_detected", "total_detected"))
})

# --- whitespace in the image id ------------------------------------------------

test_that(".read_outline survives a name column containing spaces", {
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")

  # The `name` column holds the image id, and a Leica series called
  # "Image005 Denoised" put a space in every row. read.table() defaults to
  # splitting on ANY whitespace, so the row became six fields against a
  # five-column header: R silently took the first as row names and every
  # column shifted. Explicit sep = "\t" is what stops that.
  d <- withr::local_tempdir()
  p <- file.path(d, "S1_nucleus_outline.txt")
  writeLines(c(
    "name\troi\tz\tx\ty",
    "Image005 Denoised\tnucleus_0001-0001-0433\t1\t1.5\t2.5",
    "Image005 Denoised\tnucleus_0001-0001-0433\t1\t3.5\t4.5",
    "Image005 Denoised\tnucleus_0001-0001-0433\t1\t5.5\t0.5"), p)

  got <- .read_outline(p)
  expect_identical(colnames(got), c("roi", "z", "x", "y"))
  expect_identical(nrow(got), 3L)
  expect_identical(unique(got$roi), "nucleus_0001-0001-0433")
  expect_equal(got$x, c(1.5, 3.5, 5.5))
  expect_equal(got$z, c(1L, 1L, 1L))
})

test_that("the documented identity columns are the ones the code reads", {
  # note/data_formats.md §2 claims the sample comes from `name` and the feature
  # from the `roi` prefix. Assert that against the fixture and the real reader,
  # so the claim cannot rot.
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  o <- read.table(fixture_file("nucleus", "outline"), header = TRUE,
                  sep = "\t", stringsAsFactors = FALSE)
  expect_identical(unique(o$name), "GRV_Position010")
  expect_identical(.cli_feature_from_roi(o$roi), "nucleus")

  id <- .cli_identify_outline(fixture_file("nucleus", "outline"))
  expect_true(id$ok)
  expect_identical(id$sample, "GRV_Position010")
  expect_identical(id$feature, "nucleus")
})

test_that("the documented range syntax is what the parser accepts", {
  # §4 documents key=lo:hi with either end omittable.
  expect_equal(.cli_key_ranges("nucleus=80:Inf", "--roi_area")$nucleus, c(80, Inf))
  expect_equal(.cli_key_ranges("nucleus=80:",    "--roi_area")$nucleus, c(80, Inf))
  expect_equal(.cli_key_ranges("nucleus=:100",   "--roi_area")$nucleus, c(0, 100))
  # §4 also says a comma is reserved, so it must not work as a separator here.
  expect_error(.cli_key_ranges("nucleus=80,100", "--roi_area"), "lo:hi")
})

test_that("Groovy no longer emits an image id containing whitespace", {
  # sanitize() collapses whitespace, so the case above should not arise from
  # our own writer any more. Belt and braces: the reader is explicit anyway.
  gv <- file.path(repo_root(), "scripts", "groovy", "RoiExport.groovy")
  skip_if(!file.exists(gv), "RoiExport.groovy not present")
  src <- paste(readLines(gv), collapse = "\n")
  expect_match(src, "replaceAll\\(/\\\\s\\+/, \"_\"\\)",
               info = "sanitize() must collapse whitespace to underscore")
})
