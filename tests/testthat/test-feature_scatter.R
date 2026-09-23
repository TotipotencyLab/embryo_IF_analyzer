# Scatter plots of per-feature statistics.
#
# The central design claim is that a threshold belongs to a COLUMN, not to a
# panel, and so follows that column onto whichever axis it lands on. Most of
# what follows pins that, and the parsing that makes it possible.

source_cli("cli_helpers.r")

fake_stats <- function(n = 12) {
  set.seed(4)
  data.frame(
    sample       = rep(c("A", "B"), length.out = n),
    feature_type = "nucleus",
    feature_id   = paste0("nucleus_", seq_len(n)),
    area_med     = seq(100, 1200, length.out = n),
    circ_med     = seq(0.5, 0.95, length.out = n),
    ch1_signal   = seq(0.5, 25, length.out = n),
    stringsAsFactors = FALSE)
}

# --- repeated keys ---------------------------------------------------------------

test_that("a repeated threshold key collects instead of erroring", {
  # Two lines on one variable is the normal case: a band, small below and
  # growing above. .cli_key_values() rejects duplicates, which is why this
  # variant exists.
  got <- .cli_key_values_multi(c("area_med=100", "area_med=400", "circ_med=0.6"),
                               "--threshold")
  expect_identical(got$area_med, c("100", "400"))
  expect_identical(got$circ_med, "0.6")
  # Order follows the command line, not the alphabet.
  expect_identical(names(got), c("area_med", "circ_med"))
})

test_that("a threshold without '=' is rejected, naming the token", {
  expect_error(.cli_key_values_multi("400", "--threshold"), "'key=value'")
  expect_error(.cli_key_values_multi("=400", "--threshold"), "Empty key")
})

test_that("no tokens gives an empty list, not an error", {
  expect_identical(.cli_key_values_multi(NULL, "--threshold"), list())
})

# --- plot specs -------------------------------------------------------------------

test_that("'x:y' parses and gets an auto id", {
  s <- .cli_parse_plot_specs(c("area_med:ch1_signal", "circ_med:area_med"))
  expect_identical(s$x, c("area_med", "circ_med"))
  expect_identical(s$y, c("ch1_signal", "area_med"))
  expect_identical(s$id, c("p1", "p2"))
})

test_that("'id=x:y' keeps the given id", {
  s <- .cli_parse_plot_specs(c("sz=circ_med:area_med", "ch1_signal:ch2_signal"))
  expect_identical(s$id, c("sz", "p2"))
  expect_identical(s$x, c("circ_med", "ch1_signal"))
})

test_that("an explicit id colliding with an auto id is an error, not a shadow", {
  # 'p2=' on the first spec, and the second would auto-assign p2 by position.
  expect_error(.cli_parse_plot_specs(c("p2=a:b", "c:d")), "Duplicate plot id")
})

test_that("malformed pairs are rejected with the offending token", {
  expect_error(.cli_parse_plot_specs("area_med"), "exactly one colon")
  expect_error(.cli_parse_plot_specs("a:b:c"), "exactly one colon")
  expect_error(.cli_parse_plot_specs(":b"), "exactly one colon")
  expect_error(.cli_parse_plot_specs("a:"), "exactly one colon")
})

test_that("the same column on both axes is refused", {
  expect_error(.cli_parse_plot_specs("area_med:area_med"), "same column on both axes")
})

test_that("asking for no plot at all says what to pass", {
  expect_error(.cli_parse_plot_specs(NULL), "give --plot")
})

# --- log axes ----------------------------------------------------------------------

test_that("only wide-range columns are logged automatically", {
  source_r_scripts("plot_feature_scatter.r")
  expect_true(scatter_should_log("area_med"))
  expect_true(scatter_should_log("area_sum"))
  # Circularity is 0-1 and z_gaps can legitimately be 0; log would drop those.
  expect_false(scatter_should_log("circ_med"))
  expect_false(scatter_should_log("z_gaps"))
  expect_false(scatter_should_log("ch1_signal"))
})

test_that("the log setting overrides the automatic choice both ways", {
  source_r_scripts("plot_feature_scatter.r")
  expect_true(resolve_log("auto", "area_med"))
  expect_false(resolve_log("off", "area_med"))
  expect_true(resolve_log("on", "circ_med"))
  expect_false(resolve_log("auto", "circ_med"))
  expect_error(resolve_log("maybe", "area_med"), "auto, on or off")
})

# --- the design claim: a threshold follows its column ---------------------------------

test_that("one threshold lands on x in one panel and y in another", {
  source_r_scripts("plot_feature_scatter.r")
  th <- list(area_med = 400)

  # area_med on x  -> vertical line
  a <- .scatter_threshold_lines(th, x = "area_med", y = "ch1_signal",
                                log_x = FALSE, log_y = FALSE)
  expect_equal(a$v, 400)
  expect_identical(length(a$h), 0L)

  # the SAME threshold, with area_med now on y -> horizontal line
  b <- .scatter_threshold_lines(th, x = "circ_med", y = "area_med",
                                log_x = FALSE, log_y = FALSE)
  expect_identical(length(b$v), 0L)
  expect_equal(b$h, 400)

  # and nothing at all where the column does not appear
  c3 <- .scatter_threshold_lines(th, x = "circ_med", y = "ch1_signal",
                                 log_x = FALSE, log_y = FALSE)
  expect_identical(length(c3$v), 0L)
  expect_identical(length(c3$h), 0L)
})

test_that("a band of two thresholds on one column draws two lines", {
  source_r_scripts("plot_feature_scatter.r")
  got <- .scatter_threshold_lines(list(area_med = c("100", "400")),
                                  x = "area_med", y = "circ_med",
                                  log_x = FALSE, log_y = FALSE)
  expect_equal(got$v, c(100, 400))
})

test_that("a non-numeric threshold is named rather than silently dropped", {
  source_r_scripts("plot_feature_scatter.r")
  expect_error(.scatter_threshold_lines(list(area_med = "big"), "area_med", "y",
                                        FALSE, FALSE), "not numeric")
})

test_that("a threshold a log axis cannot show warns instead of vanishing", {
  source_r_scripts("plot_feature_scatter.r")
  got <- expect_warning(
    .scatter_threshold_lines(list(area_med = c("0", "400")), "area_med", "y",
                             log_x = TRUE, log_y = FALSE),
    "cannot be drawn")
  expect_equal(got$v, 400)
})

# --- the panel ------------------------------------------------------------------------

test_that("the threshold is drawn once, not once per data row", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  st <- fake_stats(20)
  p <- plot_feature_scatter(st, "area_med", "ch1_signal",
                            thresholds = list(area_med = 400, ch1_signal = 5))
  b <- ggplot2::ggplot_build(p)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_identical(nrow(b$data[[which(geoms == "GeomVline")]]), 1L)
  expect_identical(nrow(b$data[[which(geoms == "GeomHline")]]), 1L)
  expect_identical(nrow(b$data[[which(geoms == "GeomPoint")]]), 20L)
})

test_that("a non-numeric axis is refused and points at the right tool", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  expect_error(plot_feature_scatter(fake_stats(), "sample", "area_med"),
               "feature_stat_cli")
})

test_that("an unknown axis is named", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  expect_error(plot_feature_scatter(fake_stats(), "nope", "area_med"),
               "No such column")
})

test_that("a pair with nothing to draw yields NULL, not an empty panel", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  st <- fake_stats()
  st$ch9_signal <- NA_real_
  expect_null(plot_feature_scatter(st, "area_med", "ch9_signal"))
})

test_that("auto-logging steps aside when a value would be dropped", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  st <- fake_stats()
  st$area_med[1] <- 0                      # log10(0) is -Inf
  p <- expect_warning(plot_feature_scatter(st, "area_med", "ch1_signal"),
                      "Not logging x")
  # The point must still be there; silently losing it is the failure this avoids.
  b <- ggplot2::ggplot_build(p)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_identical(nrow(b$data[[which(geoms == "GeomPoint")]]), nrow(st))
})

# --- the colour legend ----------------------------------------------------------

test_that("the legend is dropped when it would be unreadable, and kept when not", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  legend_of <- function(p) as.character(p$theme$legend.position)

  few <- fake_stats(12)                       # 2 samples
  many <- data.frame(sample = rep(sprintf("S%02d", 1:20), each = 2),
                     area_med = seq(100, 900, length.out = 40),
                     ch1_signal = seq(1, 20, length.out = 40),
                     stringsAsFactors = FALSE)

  # Kept: this is the half that makes the others meaningful. Without it,
  # "dropped" could pass because the legend was never there.
  expect_identical(legend_of(plot_feature_scatter(few, "area_med", "ch1_signal",
                                                  color_by = "sample")), "right")
  # Dropped: past legend_max the key is unreadable and squeezes the panel.
  expect_identical(legend_of(plot_feature_scatter(many, "area_med", "ch1_signal",
                                                  color_by = "sample")), "none")
  # Dropped: the facet strip already names it.
  expect_identical(legend_of(plot_feature_scatter(few, "area_med", "ch1_signal",
                                                  color_by = "sample",
                                                  facet_by = "sample")), "none")
  # The threshold is a parameter, not a hardcoded 12.
  expect_identical(legend_of(plot_feature_scatter(few, "area_med", "ch1_signal",
                                                  color_by = "sample",
                                                  legend_max = 1)), "none")
})

test_that("dropping the legend is announced on the plot, not silent", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  p <- plot_feature_scatter(fake_stats(12), "area_med", "ch1_signal",
                            color_by = "sample", legend_max = 1)
  expect_match(p$labels$subtitle, "legend omitted")
  # ...but not when the facet strip makes it merely redundant.
  q <- plot_feature_scatter(fake_stats(12), "area_med", "ch1_signal",
                            color_by = "sample", facet_by = "sample")
  expect_false(grepl("legend omitted", q$labels$subtitle))
})

test_that("the colour mapping survives even when the key is dropped", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  p <- plot_feature_scatter(fake_stats(12), "area_med", "ch1_signal",
                            color_by = "sample", legend_max = 1)
  b <- ggplot2::ggplot_build(p)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # Two samples in fake_stats(), so two colours should still be in play.
  expect_gt(length(unique(b$data[[which(geoms == "GeomPoint")]]$colour)), 1)
})

# --- subsetting which levels get faceted --------------------------------------------

test_that("facet_keep subsets the faceted page and leaves the pooled one whole", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  st <- fake_stats(12)                        # samples A and B
  specs <- .cli_parse_plot_specs("area_med:ch1_signal")
  pl <- plot_feature_scatter_list(st, specs, facet = "both", facet_keep = "A")
  expect_length(pl, 2)

  n_points <- function(p) {
    b <- ggplot2::ggplot_build(p)
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    nrow(b$data[[which(geoms == "GeomPoint")]])
  }
  # Pooled keeps every feature; faceted keeps only sample A's.
  expect_identical(n_points(pl[[1]]), nrow(st))
  expect_identical(n_points(pl[[2]]), sum(st$sample == "A"))
})

test_that("facet_keep naming something absent warns", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  specs <- .cli_parse_plot_specs("area_med:ch1_signal")
  expect_warning(
    plot_feature_scatter_list(fake_stats(12), specs, facet = "both",
                              facet_keep = c("A", "ZZZ")),
    "not in sample")
})

test_that("facet_keep matching nothing is an error, not an empty page", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  specs <- .cli_parse_plot_specs("area_med:ch1_signal")
  expect_error(
    suppressWarnings(plot_feature_scatter_list(fake_stats(12), specs,
                                               facet = "both", facet_keep = "ZZZ")),
    "left no rows")
})

test_that("too many facets skips that page loudly and keeps the pooled one", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  many <- data.frame(sample = rep(sprintf("S%02d", 1:20), each = 2),
                     area_med = seq(100, 900, length.out = 40),
                     ch1_signal = seq(1, 20, length.out = 40),
                     stringsAsFactors = FALSE)
  specs <- .cli_parse_plot_specs("area_med:ch1_signal")
  pl <- expect_warning(
    plot_feature_scatter_list(many, specs, facet = "both", facet_max = 16),
    "past --facet_max")
  expect_length(pl, 1)                        # pooled only
  # Raising the cap brings it back, so the skip is the cap and not a bug.
  pl2 <- plot_feature_scatter_list(many, specs, facet = "both", facet_max = 50)
  expect_length(pl2, 2)
})

# --- the list file ----------------------------------------------------------------

test_that("a value-list file drops comments and blanks", {
  d <- withr::local_tempdir()
  p <- file.path(d, "keep.txt")
  writeLines(c("# samples of interest", "Series001", "", "  Series005  ",
               "Series012   # a trailing comment", "Series001"), p)
  expect_identical(.cli_read_value_list(p, "--facet_keep_file"),
                   c("Series001", "Series005", "Series012"))
})

test_that("a missing or empty list file is named", {
  d <- withr::local_tempdir()
  expect_error(.cli_read_value_list(file.path(d, "nope.txt"), "--facet_keep_file"),
               "No such file")
  p <- file.path(d, "empty.txt")
  writeLines(c("", "# nothing here"), p)
  expect_error(.cli_read_value_list(p, "--facet_keep_file"), "holds no values")
})

test_that("facet 'both' gives a pooled page and a faceted one per pair", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  specs <- .cli_parse_plot_specs(c("area_med:ch1_signal", "circ_med:ch1_signal"))
  expect_length(plot_feature_scatter_list(fake_stats(), specs, facet = "both"), 4)
  expect_length(plot_feature_scatter_list(fake_stats(), specs, facet = "none"), 2)
  expect_length(plot_feature_scatter_list(fake_stats(), specs, facet = "sample"), 2)
})

test_that("faceting on a column that is absent names it", {
  skip_if_no_pkg("ggplot2")
  source_r_scripts("plot_feature_scatter.r")
  suppressPackageStartupMessages(library(ggplot2))
  specs <- .cli_parse_plot_specs("area_med:ch1_signal")
  expect_error(plot_feature_scatter_list(fake_stats(), specs, facet = "genotype"),
               "not present")
})

# --- describing what is available -------------------------------------------------------

test_that("the available-stats table reports emptiness, which is the useful part", {
  source_r_scripts("plot_feature_scatter.r")
  st <- fake_stats()
  st$ch9_signal <- NA_real_
  d <- describe_feature_stats(st)
  expect_true("ch9_signal" %in% d$column)
  expect_identical(d$non_na[d$column == "ch9_signal"], paste0("0/", nrow(st)))
  expect_identical(d$range[d$column == "ch9_signal"], "(all NA)")
  expect_identical(d$type[d$column == "sample"], "chr")
})

# --- the CLI --------------------------------------------------------------------------

stats_file <- function(dir) {
  p <- file.path(dir, "feature_stats.tsv")
  write.table(fake_stats(), p, sep = "\t", quote = FALSE, row.names = FALSE)
  return(p)
}

test_that("--show_avail_stats works without --outdir and writes nothing", {
  skip_if_no_pkg("argparser")
  source_cli("feature_scatter_cli.r")
  d <- withr::local_tempdir()
  f <- stats_file(d)
  out <- capture.output(suppressMessages(
    feature_scatter_cli(c("--input", f, "--show_avail_stats"))))
  expect_true(any(grepl("area_med", out)))
  expect_true(any(grepl("Only numeric columns", out)))
  expect_false(file.exists(file.path(d, "feature_scatter.pdf")))
})

test_that("the CLI writes one PDF with a page per pair per view", {
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("feature_scatter_cli.r")
  d <- withr::local_tempdir()
  f <- stats_file(d)
  out <- withr::local_tempdir()
  suppressMessages(feature_scatter_cli(c(
    "--input", f, "--outdir", out,
    "--plot", "area_med:ch1_signal", "circ_med:ch1_signal",
    "--threshold", "area_med=400", "--facet", "both")))
  pdf <- file.path(out, "feature_scatter.pdf")
  expect_true(file.exists(pdf))
  bytes <- readBin(pdf, "raw", file.size(pdf))
  expect_gt(length(grepRaw(charToRaw("/Count 4"), bytes, all = TRUE)), 0)
})

test_that("a threshold no plot shows is a warning, not silence", {
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("feature_scatter_cli.r")
  d <- withr::local_tempdir()
  f <- stats_file(d)
  out <- withr::local_tempdir()
  expect_warning(
    suppressMessages(feature_scatter_cli(c(
      "--input", f, "--outdir", out, "--plot", "area_med:ch1_signal",
      "--threshold", "circ_med=0.6", "--facet", "none"))),
    "no --plot shows")
})

test_that("an unknown --plot column errors and lists what can be plotted", {
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("feature_scatter_cli.r")
  d <- withr::local_tempdir()
  f <- stats_file(d)
  out <- withr::local_tempdir()
  expect_error(
    suppressMessages(feature_scatter_cli(c(
      "--input", f, "--outdir", out, "--plot", "nope:ch1_signal"))),
    "plottable columns")
})

test_that("an unknown --threshold or --color_by column errors by name", {
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("feature_scatter_cli.r")
  d <- withr::local_tempdir()
  f <- stats_file(d)
  out <- withr::local_tempdir()
  expect_error(
    suppressMessages(feature_scatter_cli(c(
      "--input", f, "--outdir", out, "--plot", "area_med:ch1_signal",
      "--threshold", "nope=1"))),
    "not in the table")
  expect_error(
    suppressMessages(feature_scatter_cli(c(
      "--input", f, "--outdir", out, "--plot", "area_med:ch1_signal",
      "--color_by", "nope"))),
    "--color_by names a column")
})

test_that("several input tables gain a source_file column to tell them apart", {
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("feature_scatter_cli.r")
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  f1 <- file.path(d1, "runA_feature_stats.tsv")
  f2 <- file.path(d2, "runB_feature_stats.tsv")
  write.table(fake_stats(), f1, sep = "\t", quote = FALSE, row.names = FALSE)
  write.table(fake_stats(), f2, sep = "\t", quote = FALSE, row.names = FALSE)

  out <- withr::local_tempdir()
  st <- suppressMessages(feature_scatter_cli(c(
    "--input", f1, f2, "--outdir", out,
    "--plot", "area_med:ch1_signal", "--facet", "source_file")))
  expect_true("source_file" %in% colnames(st))
  expect_setequal(unique(st$source_file), c("runA", "runB"))
  expect_identical(nrow(st), 2L * nrow(fake_stats()))
})

test_that("a single input table gets no source_file column", {
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("feature_scatter_cli.r")
  d <- withr::local_tempdir()
  f <- stats_file(d)
  st <- NULL
  invisible(capture.output(
    st <- suppressMessages(feature_scatter_cli(c("--input", f, "--show_avail_stats")))))
  expect_false("source_file" %in% colnames(st))
})
