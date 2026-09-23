# Per-feature statistics and their plots.
#
# The theme: the feature is the unit, not the ROI. Every assertion here pins
# something that would otherwise let a per-ROI quantity leak into a per-feature
# answer, or let a missing input produce a plausible-looking but empty result.

source_cli("cli_helpers.r")

# A per-ROI feature table in the shape annotate_features_cli.r writes. Areas
# vary per slice on purpose: a feature that tapers is what makes weighted and
# unweighted means differ.
make_feats <- function(sample = "S1", feature = "nucleus",
                       ids = c("nucleus_1", "nucleus_1", "nucleus_1"),
                       z = 1:3, area = c(10, 100, 10),
                       roi = NULL) {
  if (is.null(roi)) {
    roi <- sprintf("%s_%04d-0001-0433", feature, z)
  }
  data.frame(roi = roi, z = z, area = area,
             feature_id = ids, feature_type = feature, sample = sample,
             stringsAsFactors = FALSE)
}

make_res <- function(roi, ch = 1, mean = 50, circ = 0.9) {
  data.frame(roi = rep(roi, each = length(ch)),
             ch = rep(ch, times = length(roi)),
             mean = mean, circ = circ, stringsAsFactors = FALSE)
}

# --- aggregation ----------------------------------------------------------------

test_that("area weighting is not the same as a plain mean", {
  source_r_scripts("feature_stats.r")
  # A tapering object: the big middle slice should dominate.
  value <- c(0, 100, 0)
  weight <- c(1, 98, 1)
  expect_equal(aggregate_roi_stat(value, weight, "mean"), 100 / 3)
  expect_equal(aggregate_roi_stat(value, weight, "wmean"), 98)
  # The whole point: the two differ, and by a lot here.
  expect_gt(abs(aggregate_roi_stat(value, weight, "wmean") -
                  aggregate_roi_stat(value, weight, "mean")), 50)
})

test_that("every documented statistic is distinct and correct", {
  source_r_scripts("feature_stats.r")
  v <- c(1, 2, 6); w <- c(1, 1, 1)
  expect_equal(aggregate_roi_stat(v, w, "mean"), 3)
  expect_equal(aggregate_roi_stat(v, w, "median"), 2)
  expect_equal(aggregate_roi_stat(v, w, "min"), 1)
  expect_equal(aggregate_roi_stat(v, w, "max"), 6)
  expect_equal(aggregate_roi_stat(v, w, "sum"), 9)
  expect_equal(aggregate_roi_stat(v, w, "sd"), stats::sd(v))
})

test_that("degenerate inputs return NA rather than a wrong number", {
  source_r_scripts("feature_stats.r")
  expect_true(is.na(aggregate_roi_stat(c(NA, NA), c(1, 1), "mean")))
  # sd of one value is undefined; 0 would be a lie.
  expect_true(is.na(aggregate_roi_stat(5, 1, "sd")))
  # Zero total weight must fall back, not divide by zero.
  expect_equal(aggregate_roi_stat(c(2, 4), c(0, 0), "wmean"), 3)
  expect_error(aggregate_roi_stat(1, 1, "geometric"), "Unknown statistic")
})

# --- the roi prefix, which is how the _res.txt is found ---------------------------

test_that("the roi prefix survives a rename", {
  source_r_scripts("feature_stats.r")
  # feature_type says "oocyte" after --rename, but the file on disk is still
  # <sample>_nucleus_res.txt. The roi column is what knows that.
  expect_identical(feature_roi_prefix("nucleus_0001-0001-0433"), "nucleus")
  expect_identical(feature_roi_prefix("growing_oocyte_0012-0003-1884"), "growing_oocyte")
  expect_true(is.na(feature_roi_prefix("not-an-roi-id")))
})

# --- summarising ------------------------------------------------------------------

test_that("one row per feature, with the documented columns", {
  source_r_scripts("feature_stats.r")
  f <- make_feats()
  st <- summarise_feature_stats(f)
  expect_identical(nrow(st), 1L)
  for (col in c("sample", "feature_type", "feature_id", "n_roi", "n_z",
                "z_span", "z_gaps", "area_med", "area_mean", "area_max", "area_sum")) {
    expect_true(col %in% colnames(st), info = col)
  }
  expect_identical(st$n_roi, 3L)
  expect_identical(st$n_z, 3L)
  expect_equal(st$area_sum, 120)
  expect_equal(st$area_med, 10)
})

test_that("invalid_ and failed_ rows are excluded from the statistics", {
  source_r_scripts("feature_stats.r")
  f <- rbind(
    make_feats(ids = rep("nucleus_1", 3)),
    make_feats(ids = rep("invalid_nucleus_1", 3), z = 4:6, area = c(999, 999, 999)),
    make_feats(ids = rep("failed_nucleus_area", 3), z = 7:9, area = c(999, 999, 999)))
  st <- summarise_feature_stats(f)
  expect_identical(nrow(st), 1L)
  expect_identical(st$feature_id, "nucleus_1")
  # If the 999s had leaked in, this would not be 10.
  expect_equal(st$area_med, 10)
})

test_that("z_gaps counts slices the object skipped", {
  source_r_scripts("feature_stats.r")
  solid <- summarise_feature_stats(make_feats(z = 1:3, area = c(1, 1, 1)))
  expect_identical(solid$z_gaps, 0L)
  gappy <- summarise_feature_stats(make_feats(z = c(1L, 2L, 9L), area = c(1, 1, 1)))
  expect_identical(gappy$z_span, 9L)
  expect_identical(gappy$n_z, 3L)
  expect_identical(gappy$z_gaps, 6L)
})

test_that("channel signal is area-weighted per feature and named ch<N>_signal", {
  source_r_scripts("feature_stats.r")
  f <- make_feats(area = c(1, 98, 1))
  res <- make_res(f$roi, ch = 1, mean = c(0, 100, 0))
  st <- summarise_feature_stats(f, res = res, channel_stat = "wmean")
  expect_true("ch1_signal" %in% colnames(st))
  expect_equal(st$ch1_signal, 98)

  st2 <- summarise_feature_stats(f, res = res, channel_stat = "mean")
  expect_equal(st2$ch1_signal, 100 / 3)
})

test_that("several channels each get their own column", {
  source_r_scripts("feature_stats.r")
  f <- make_feats(area = c(1, 1, 1))
  res <- rbind(make_res(f$roi, ch = 1, mean = 10),
               make_res(f$roi, ch = 2, mean = 20))
  st <- summarise_feature_stats(f, res = res)
  expect_equal(st$ch1_signal, 10)
  expect_equal(st$ch2_signal, 20)
})

test_that("the channel join does not multiply the geometry", {
  source_r_scripts("feature_stats.r")
  f <- make_feats(area = c(10, 10, 10))
  # Two channels means two res rows per ROI. n_roi must stay 3, not become 6,
  # and area_sum must stay 30, not 60.
  res <- rbind(make_res(f$roi, ch = 1, mean = 10),
               make_res(f$roi, ch = 2, mean = 20))
  st <- summarise_feature_stats(f, res = res)
  expect_identical(st$n_roi, 3L)
  expect_equal(st$area_sum, 30)
})

test_that("a duplicated roi/channel row warns and is not counted twice", {
  source_r_scripts("feature_stats.r")
  f <- make_feats(area = c(1, 1, 1))
  res <- make_res(f$roi, ch = 1, mean = 10)
  res <- rbind(res, res[1, , drop = FALSE])
  res$mean[nrow(res)] <- 1000        # make the duplicate detectable
  st <- expect_warning(summarise_feature_stats(f, res = res), "duplicate")
  expect_equal(st$ch1_signal, 10)
})

test_that("a table with no valid features warns and returns nothing", {
  source_r_scripts("feature_stats.r")
  f <- make_feats(ids = rep("failed_nucleus_area", 3))
  st <- expect_warning(summarise_feature_stats(f), "No valid features")
  expect_identical(nrow(st), 0L)
})

test_that("a missing required column is named", {
  source_r_scripts("feature_stats.r")
  f <- make_feats()
  f$area <- NULL
  expect_error(summarise_feature_stats(f), "missing column\\(s\\): area")
})

test_that("reject counts separate 'few objects' from 'most of them failed'", {
  source_r_scripts("feature_stats.r")
  f <- rbind(make_feats(ids = rep("nucleus_1", 3)),
             make_feats(ids = rep("invalid_nucleus_1", 3), z = 4:6),
             make_feats(ids = rep("failed_nucleus_area", 3), z = 7:9))
  rc <- feature_reject_counts(f)
  got <- stats::setNames(rc$n_roi, rc$bucket)
  expect_identical(unname(got[["feature"]]), 3L)
  expect_identical(unname(got[["invalid"]]), 3L)
  expect_identical(unname(got[["failed"]]), 3L)
})

# --- plotting ----------------------------------------------------------------------

test_that("save_plot_list writes one page per plot", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_stats.r")
  suppressPackageStartupMessages(library(ggplot2))
  d <- withr::local_tempdir()
  p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point()
  f <- file.path(d, "out.pdf")
  save_plot_list(list(a = p, b = p, c = p), f, width = 4, height = 3)
  expect_true(file.exists(f))
  # The page count is in the PDF catalogue; read it rather than trusting
  # length(). Matched on RAW bytes: a PDF is binary, and readLines() on it
  # yields invalid UTF-8 that breaks testthat's own error formatting.
  bytes <- readBin(f, "raw", file.size(f))
  expect_gt(length(grepRaw(charToRaw("/Count 3"), bytes, all = TRUE)), 0)
})

test_that("a plot that fails to render does not leave the device open", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_stats.r")
  suppressPackageStartupMessages(library(ggplot2))
  d <- withr::local_tempdir()
  before <- grDevices::dev.cur()

  # A ggplot that only explodes at print time.
  bad <- ggplot(data.frame(x = 1), aes(x, nonexistent_column)) + geom_point()
  good <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point()
  f <- file.path(d, "out.pdf")
  suppressWarnings(suppressMessages(save_plot_list(list(bad = bad, good = good), f)))

  # Without dev.off() on exit the pdf device stays current and every later plot
  # in the session is silently written into this file.
  expect_identical(grDevices::dev.cur(), before)
  expect_true(file.exists(f))
})

test_that("an empty plot list says so instead of writing a broken file", {
  source_r_scripts("plot_feature_stats.r")
  d <- withr::local_tempdir()
  f <- file.path(d, "none.pdf")
  expect_warning(save_plot_list(list(), f), "No plots")
  expect_false(file.exists(f))
})

test_that("plot_feature_stat rejects unknown types and columns by name", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_stats.r")
  suppressPackageStartupMessages(library(ggplot2))
  st <- data.frame(sample = c("a", "b"), area_med = c(1, 2), stringsAsFactors = FALSE)
  expect_error(plot_feature_stat(st, "area_med", types = "swarm"), "Unknown plot type")
  expect_error(plot_feature_stat(st, "nope"), "No such column")
  expect_error(plot_feature_stat(st, "area_med", group_col = "nope"), "No such grouping")
})

test_that("a statistic that is entirely NA yields no panel rather than an empty one", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_stats.r")
  suppressPackageStartupMessages(library(ggplot2))
  st <- data.frame(sample = c("a", "b"), area_med = c(1, 2),
                   ch9_signal = c(NA_real_, NA_real_), stringsAsFactors = FALSE)
  expect_null(plot_feature_stat(st, "ch9_signal"))
  pl <- plot_feature_stat_list(st)
  expect_false("ch9_signal" %in% names(pl))
  expect_true("area_med" %in% names(pl))
})

# --- the CLI ---------------------------------------------------------------------

test_that("the CLI writes the stats table and a PDF from the fixture", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))

  out <- withr::local_tempdir()
  st <- suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", out, "--res_dir", fixture_dir())))

  expect_gt(nrow(st), 0)
  expect_true(file.exists(file.path(out, "feature_stats.tsv")))
  expect_true(file.exists(file.path(out, "feature_rejects.tsv")))
  expect_true(file.exists(file.path(out, "feature_stats.pdf")))
  # The measurement table was found, so there is signal.
  expect_true("ch1_signal" %in% colnames(st))
  expect_false(all(is.na(st$ch1_signal)))
})

test_that("no measurement table is a loud warning, not a quiet empty column", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_file("nucleus", "outline"),
    "--feature", "nucleus", "--outdir", feat)))

  out <- withr::local_tempdir()
  empty <- withr::local_tempdir()      # a directory with no _res.txt in it
  expect_warning(
    suppressMessages(feature_stat_cli(c(
      "--input", feat, "--outdir", out, "--res_dir", empty, "--no_plot"))),
    "NO channel signal")
})

test_that("--no_plot writes the table and no PDF", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  out <- withr::local_tempdir()
  suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", out, "--res_dir", fixture_dir(), "--no_plot")))
  expect_true(file.exists(file.path(out, "feature_stats.tsv")))
  expect_false(file.exists(file.path(out, "feature_stats.pdf")))
})

test_that("--group_by naming a column that is absent fails with the available ones", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  out <- withr::local_tempdir()
  expect_error(
    suppressMessages(feature_stat_cli(c(
      "--input", feat, "--outdir", out, "--res_dir", fixture_dir(),
      "--group_by", "genotype", "--no_plot"))),
    "not present")
})

test_that("is_bridge does not leak onto the per-feature table", {
  # It is an ROI-level fact and VARIES within a bridged feature, so carrying it
  # through as sample metadata made summarise_feature_stats() warn and then
  # take an arbitrary first value -- yielding an is_bridge column on a table
  # whose unit is the feature, which invites exactly the wrong filter.
  # n_bridge / frac_bridge are the feature-level answer.
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat,
    "--roi_area", "nucleus=100:Inf", "--bridge_roi", "roi_area")))

  # The flag must actually have bridged something, or this proves nothing.
  rds <- readRDS(list.files(feat, "_features[.]rds$", full.names = TRUE)[1])
  expect_true(any(rds$is_bridge))

  out <- withr::local_tempdir()
  st <- suppressMessages(feature_stat_cli(
    c("--input", feat, "--outdir", out, "--no_plot", "--res_dir", fixture_dir())))

  expect_false("is_bridge" %in% colnames(st))
  expect_true(all(c("n_bridge", "frac_bridge", "n_roi_all", "n_z_all") %in% colnames(st)))
  expect_gt(sum(st$n_bridge), 0)
})

test_that("volume appears only with --z_step, and is area_sum x z_step", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))

  out <- withr::local_tempdir()
  plain <- suppressMessages(feature_stat_cli(
    c("--input", feat, "--outdir", out, "--no_plot", "--res_dir", fixture_dir())))
  expect_false("volume" %in% colnames(plain))

  out2 <- withr::local_tempdir()
  with_z <- suppressMessages(feature_stat_cli(
    c("--input", feat, "--outdir", out2, "--no_plot", "--res_dir", fixture_dir(),
      "--z_step", "0.5")))
  expect_true("volume" %in% colnames(with_z))
  expect_equal(with_z$volume, with_z$area_sum * 0.5)
})
