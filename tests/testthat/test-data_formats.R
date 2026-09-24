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
  # The fixture predates pixel_depth and is deliberately NOT regenerated: an
  # older config must stay readable, and this is the case that proves it.
  expect_false("pixel_depth" %in% d$parameter)
})

test_that("the sheet column schema is readable and its owners are known", {
  # schema/sheet_columns.tsv exists so this list does NOT live twice, once in
  # Groovy and once in R. This is the R half reading it; Test_SampleSheet is
  # the Groovy half.
  f <- file.path(repo_root(), "schema", "sheet_columns.tsv")
  skip_if_not(file.exists(f), "schema/sheet_columns.tsv not found")
  d <- read.delim(f, stringsAsFactors = FALSE)
  expect_identical(colnames(d),
                   c("sheet", "column", "owner", "type", "required", "description"))
  expect_true(all(d$sheet %in% c("files", "samples")))
  expect_true(all(d$owner %in% c("machine", "seeded", "user")))
  expect_true(all(d$type %in% c("string", "integer", "double", "boolean")))
  expect_true(all(d$required %in% c("yes", "no")))

  # prefix is the join key the R side already requires, and it must be seeded
  # rather than machine: regeneration must not overwrite one you edited.
  pre <- d[d$sheet == "samples" & d$column == "prefix", ]
  expect_identical(nrow(pre), 1L)
  expect_identical(pre$owner, "seeded")
  expect_identical(pre$required, "yes")
  expect_identical(d$owner[d$sheet == "samples" & d$column == "series_index"], "machine")

  # Every samples column has to be described where people look for it.
  doc <- paste(readLines(file.path(repo_root(), "note", "data_formats.md"),
                         warn = FALSE), collapse = "\n")
  undocumented <- setdiff(d$column[d$sheet == "samples"],
                          unlist(regmatches(doc, gregexpr("[A-Za-z_]+", doc))))
  expect_identical(undocumented, character(0))
})

test_that("pixel_depth is written by the Groovy side and documented here", {
  # Cross-language, so it is a source check rather than a call: the writer is
  # Groovy and the documentation is Markdown, and the failure being guarded
  # against is one of them changing without the other.
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  skip_if_not(file.exists(np), "NucleusPipeline.groovy not found")
  src <- paste(readLines(np, warn = FALSE), collapse = "\n")
  expect_match(src, "pixel_depth", fixed = TRUE)
  # Blank for a single plane, not ImageJ's default 1.0 -- a z step that does
  # not exist must not arrive looking usable.
  expect_match(src, "getNSlices() > 1", fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  # The ROW in the _config.txt field table, not merely a mention of the name:
  # the first version of this assertion passed while the row was deleted,
  # because another paragraph elsewhere happened to say "pixel_depth".
  row <- grep("^\\|\\s*`pixel_depth`\\s*\\|", doc, value = TRUE)
  expect_length(row, 1L)
  expect_match(row, "single plane", fixed = TRUE)
})

test_that("open_method is written, readable back, and documented", {
  # Two ways of opening an image now exist and are asserted to produce identical
  # output; which one ran must still be recorded, or two runs that differed are
  # indistinguishable afterwards. Same reasoning as recording VERSION.
  br <- file.path(repo_root(), "scripts", "groovy", "BatchRunner.groovy")
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  rc <- file.path(repo_root(), "scripts", "groovy", "RunConfig.groovy")
  skip_if_not(all(file.exists(br, np, rc)), "Groovy library not found")

  br_src <- paste(readLines(br, warn = FALSE), collapse = "\n")
  expect_match(br_src, "OPEN_MODES", fixed = TRUE)
  # The summary column, so a long run says per row which reader opened it.
  expect_match(br_src, '"open_method"', fixed = TRUE)

  # Written into _config.txt ...
  expect_match(paste(readLines(np, warn = FALSE), collapse = "\n"),
               "open_method", fixed = TRUE)
  # ... and therefore readable back: an unknown key in a config is a hard error,
  # so a provenance field that is not registered makes a run's own _config.txt
  # unusable as the config of the next run.
  expect_match(paste(readLines(rc, warn = FALSE), collapse = "\n"),
               '"open_method"', fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  # The ROWS in the two field tables, not a passing mention: _config.txt and
  # batch_summary.tsv each document it.
  rows <- grep("^\\|\\s*`open_method`\\s*\\|", doc, value = TRUE)
  expect_length(rows, 2L)
  expect_true(any(grepl("importer", rows, fixed = TRUE)))
  expect_true(any(grepl("reader", rows, fixed = TRUE)))
})

test_that("the sample prefix carries the series index, and the doc says so", {
  # Series names repeat -- a tile scan is many series under one name -- so the
  # index is what makes the prefix unique WITHIN a file, as the alias does
  # across files. Forced always, never only where names happen to collide: a
  # prefix that depends on which other series share its name is a prefix that
  # changes when a file is re-acquired.
  ss <- file.path(repo_root(), "scripts", "groovy", "SampleSheet.groovy")
  skip_if_not(file.exists(ss), "SampleSheet.groovy not found")
  src <- paste(readLines(ss, warn = FALSE), collapse = "\n")
  # Fixed width, not derived from the series count.
  expect_match(src, 'INDEX_FORMAT = "s%04d"', fixed = TRUE)

  schema <- read.delim(file.path(repo_root(), "schema", "sheet_columns.tsv"),
                       stringsAsFactors = FALSE)
  prefix_row <- schema[schema$sheet == "samples" & schema$column == "prefix", ]
  expect_equal(nrow(prefix_row), 1L)
  expect_match(prefix_row$description, "s<NNNN>", fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  expect_true(any(grepl("sanitise(<alias>_s<NNNN>_<series_name>)", doc, fixed = TRUE)))
  # The old two-part form must be gone, not merely joined by the new one.
  expect_false(any(grepl("`sanitise(<alias>_<series_name>)`", doc, fixed = TRUE)))
})

test_that("a duplicate prefix is refused downstream, allowed at sheet time", {
  # The asymmetry is deliberate: the sheet step is a draft for a person to read,
  # so it writes the file and THEN fails -- a duplicate you cannot open the
  # table to see is one you cannot fix. The analysis step has no such excuse.
  ms <- file.path(repo_root(), "scripts", "groovy", "Make_SampleSheet.groovy")
  br <- file.path(repo_root(), "scripts", "groovy", "BatchRunner.groovy")
  skip_if_not(all(file.exists(ms, br)), "Groovy front ends not found")

  ms_src <- readLines(ms, warn = FALSE)
  # Anchored on the BUILD-mode write: scan mode writes with the same call prefix.
  write_at <- grep("TSV.write(rows, outSheet, sheet.columnOrder(rows))", ms_src, fixed = TRUE)
  throw_at <- grep("duplicated prefix", ms_src, fixed = TRUE)
  expect_length(write_at, 1L)
  expect_length(throw_at, 1L)
  # The ordering IS the feature.
  expect_lt(write_at, throw_at)
  expect_match(paste(ms_src, collapse = "\n"), "allowDuplicatePrefix", fixed = TRUE)

  expect_match(paste(readLines(br, warn = FALSE), collapse = "\n"),
               "share a prefix", fixed = TRUE)

  # R has refused one all along; this pins that it still does, since the
  # documented contract now leans on it.
  expect_match(paste(readLines(file.path(repo_root(), "scripts", "R_cli",
                                         "cli_helpers.r"), warn = FALSE),
                     collapse = "\n"),
               "Duplicate '", fixed = TRUE)
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

  documented <- c("roi", "z", "area", "is_bridge", "feature_id", "feature_type",
                  "sample", "run_id", "parent_feature_id", "parent_feature_type",
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
                   c("sample", "feature_class", "feature_type", "n_detected",
                     "n_invalid", "n_failed", "n_roi"))
  # With the default --feature_class_by the composite IS the feature type, so
  # the extra column is a rename of nothing rather than a change of meaning.
  expect_identical(counts$feature_class, counts$feature_type)

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

test_that("feature_stat writes the documented columns", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "nucleolus",
    "--outdir", feat, "--min_z_span", "default=5", "nucleolus=2",
    "--max_z_dist", "default=3", "nucleolus=1")))

  out <- withr::local_tempdir()
  st <- suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", out, "--res_dir", fixture_dir(), "--no_plot")))

  documented <- c("sample", "feature_type", "feature_id", "n_roi", "n_z",
                  "z_min", "z_max", "z_span", "area_med", "area_mean",
                  "area_max", "area_sum", "z_gaps", "circ_med", "circ_min")
  expect_true(all(documented %in% colnames(st)),
              info = paste(setdiff(documented, colnames(st)), collapse = ", "))

  # One signal column per channel Fiji measured, named ch<N>_signal.
  sig <- grep("^ch\\d+_signal$", colnames(st), value = TRUE)
  expect_gt(length(sig), 0)

  # z_gaps is defined as z_span - n_z; assert the arithmetic, not just the name.
  expect_equal(st$z_gaps, st$z_span - st$n_z)
  # And z_span as max - min + 1.
  expect_equal(st$z_span, st$z_max - st$z_min + 1L)

  rej <- read.delim(file.path(out, "feature_rejects.tsv"), stringsAsFactors = FALSE)
  expect_identical(colnames(rej), c("sample", "feature_type", "bucket", "n_roi"))
  expect_true(all(rej$bucket %in% c("feature", "invalid", "failed", "unassigned")))
})

test_that("the documented default aggregation really is area-weighted", {
  source_r_scripts("feature_stats.r")
  # §3 claims --channel_stat defaults to wmean. A tapering object is where that
  # differs from a plain mean, so the claim is testable rather than decorative.
  v <- c(0, 100, 0); w <- c(1, 98, 1)
  expect_equal(aggregate_roi_stat(v, w, "wmean"), 98)
  expect_false(isTRUE(all.equal(aggregate_roi_stat(v, w, "wmean"),
                                aggregate_roi_stat(v, w, "mean"))))
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
