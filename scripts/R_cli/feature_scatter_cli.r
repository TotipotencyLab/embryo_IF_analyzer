#!/usr/bin/env Rscript

# feature_scatter_cli.r
#
# Two per-feature statistics against each other, one point per detected object.
#
# Reads the feature_stats.tsv that feature_stat_cli.r writes, NOT the .rds. The
# expensive work -- reading geometry, joining the Fiji measurements, summarising
# per feature -- is done once upstream; this is the cheap end you re-run a dozen
# times while trying pairs.
#
#   ./feature_scatter_cli.r --input stats/feature_stats.tsv --outdir stats/ \
#       --plot 'area_med:ch1_signal' 'circ_med:ch1_signal' \
#       --threshold 'area_med=400' --color_by sample
#
#   ./feature_scatter_cli.r --input stats/feature_stats.tsv --show_avail_stats
#
# Thresholds are keyed to the COLUMN, not to the plot: a cut-off decided once is
# a fact about a variable, so it is drawn on every panel where that variable
# appears, as a vertical line when it is x and a horizontal one when it is y.
# Nothing has to be matched up by position, and the same number cannot be stated
# two different ways on two panels.

suppressPackageStartupMessages({
  library(argparser)
})


# Directory of THIS file, resolved at source time. See annotate_features_cli.r
# for why this is a top-level assignment rather than a function called later.
.THIS_DIR <- (function() {
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    if (!is.null(f$ofile)){
      return(dirname(normalizePath(f$ofile, mustWork = FALSE)))
    }
  }
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)){
    return(dirname(normalizePath(sub("^--file=", "", a[1]), mustWork = FALSE)))
  }
  a <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) character(0))
  if (length(a)){
    return(dirname(a[1]))
  }
  return(NA_character_)
})()

.scatter_source_helpers <- function() {
  if (exists(".cli_resolve_arg", mode = "function")) return(invisible(NULL))
  if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
  sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
}

# ------------------------------------------------------------------------------

feature_scatter_cli <- function(args = commandArgs(trailingOnly = TRUE)) {

  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)

  .scatter_source_helpers()

  p <- arg_parser("Scatter plots of per-feature statistics", hide.opts = TRUE)
  p <- add_argument(p, "--input", short = "-i", type = "character", nargs = Inf, default = NULL,
                    help = "feature_stats.tsv file(s), globs, or directories to scan")
  p <- add_argument(p, "--outdir", short = "-o", type = "character",
                    help = "directory to write the PDF into")
  p <- add_argument(p, "--plot", short = "-p", type = "character", nargs = Inf, default = NULL,
                    help = "axis pair(s) as 'x:y', optionally named: 'p1=area_med:ch1_signal'")
  p <- add_argument(p, "--threshold", short = "-T", type = "character", nargs = Inf, default = NULL,
                    help = "guide line as 'column=value'; repeat a column for a band")
  p <- add_argument(p, "--facet", short = "-F", type = "character", default = "both",
                    help = "none, both, or a column name to facet by [default: both]")
  p <- add_argument(p, "--color_by", short = "-c", type = "character",
                    help = "column mapped to point colour")
  p <- add_argument(p, "--show_avail_stats", short = "-a", flag = TRUE,
                    help = "list the columns that can be plotted, then exit")
  p <- add_argument(p, "--log_x", short = "-x", type = "character", default = "auto",
                    help = "auto, on or off [auto: log only area_* columns]")
  p <- add_argument(p, "--log_y", short = "-y", type = "character", default = "auto",
                    help = "auto, on or off")
  p <- add_argument(p, "--smooth", short = "-s", flag = TRUE,
                    help = "add a linear fit (off by default; n per group is small)")
  p <- add_argument(p, "--corr", short = "-r", flag = TRUE,
                    help = "report Spearman rho (off by default; see --smooth)")
  p <- add_argument(p, "--output_prefix", short = "-X", type = "character", default = "",
                    help = "prefix for the output file")
  p <- add_argument(p, "--plot_width", short = "-W", type = "double", default = 7,
                    help = "PDF page width, inches")
  p <- add_argument(p, "--plot_height", short = "-H", type = "double", default = 5.5,
                    help = "PDF page height, inches")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character",
                    help = "path to scripts/R [default: alongside this script]")

  argv <- parse_args(p, argv = args)
  # --outdir is not needed to answer "what can I plot?"
  .cli_require(argv, if (argv$show_avail_stats) "input" else c("input", "outdir"))

  .cli_need(c("dplyr", "tibble"))
  suppressPackageStartupMessages({ library(dplyr); library(tibble) })
  if (!argv$show_avail_stats) {
    .cli_need("ggplot2")
    suppressPackageStartupMessages(library(ggplot2))
  }
  .source_rlib(argv$rlib_path, .THIS_DIR)

  # --- read ---------------------------------------------------------------------
  files <- .cli_resolve_input_path(argv$input, "feature_stats\\.tsv$")
  stats <- .read_stats_tables(files)
  message("Read ", nrow(stats), " feature(s) from ", length(files), " table(s)")

  if (argv$show_avail_stats) {
    .print_avail(stats)
    return(invisible(stats))
  }

  # --- specs and thresholds -------------------------------------------------------
  specs <- .cli_parse_plot_specs(argv$plot, "--plot")
  .check_columns(specs, stats)

  thresholds <- .cli_key_values_multi(argv$threshold, "--threshold")
  unknown <- setdiff(names(thresholds), colnames(stats))
  if (length(unknown)) {
    stop("--threshold names column(s) that are not in the table: ",
         paste(unknown, collapse = ", "),
         "\n  run with --show_avail_stats to list them", call. = FALSE)
  }
  # A threshold on a column that no plot shows draws nothing at all, which looks
  # exactly like a threshold that was drawn and happened to sit off-screen.
  shown <- unique(c(specs$x, specs$y))
  idle <- setdiff(names(thresholds), shown)
  if (length(idle)) {
    warning("--threshold names column(s) that no --plot shows, so nothing is ",
            "drawn for them: ", paste(idle, collapse = ", "), call. = FALSE)
  }

  color_by <- if (is.na(argv$color_by)) NULL else argv$color_by
  if (!is.null(color_by) && !color_by %in% colnames(stats)) {
    stop("--color_by names a column that is not present: ", color_by,
         "\n  run with --show_avail_stats to list them", call. = FALSE)
  }

  .report_plan(specs, thresholds, stats)

  # --- draw --------------------------------------------------------------------
  outdir <- argv$outdir
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(outdir)) stop("Could not create --outdir: ", outdir, call. = FALSE)

  plots <- plot_feature_scatter_list(
    stats, specs, facet = argv$facet,
    color_by = color_by, thresholds = thresholds,
    log_x = argv$log_x, log_y = argv$log_y,
    smooth = argv$smooth, corr = argv$corr)

  if (!length(plots)) {
    stop("Nothing could be drawn: every requested pair was empty after ",
         "dropping NA. Check --show_avail_stats.", call. = FALSE)
  }

  pdf_path <- file.path(outdir, paste0(argv$output_prefix, "feature_scatter.pdf"))
  save_plot_list(plots, pdf_path, width = argv$plot_width, height = argv$plot_height)
  message("  -> ", basename(pdf_path), " (", length(plots), " page(s))")

  return(invisible(stats))
}

# --- private helpers ----------------------------------------------------------

#' Read one or more feature_stats.tsv, labelling rows by their source
#'
#' `source_file` is added whenever more than one table is read, so several runs
#' (different settings, different segmentations) can be compared in one view via
#' --facet source_file or --color_by source_file.
.read_stats_tables <- function(files) {
  one <- lapply(files, function(f) {
    d <- utils::read.delim(f, stringsAsFactors = FALSE, check.names = FALSE)
    if (!nrow(d)) {
      warning("Empty stats table, skipping: ", basename(f), call. = FALSE)
      return(NULL)
    }
    d$source_file <- sub("_?feature_stats\\.tsv$", "", basename(f))
    if (!nzchar(d$source_file[1])) d$source_file <- basename(dirname(f))
    return(d)
  })
  one <- one[!vapply(one, is.null, logical(1))]
  if (!length(one)) stop("No usable rows in any input table.", call. = FALSE)

  out <- dplyr::bind_rows(one)
  if (length(files) == 1L) {
    out$source_file <- NULL
  }
  return(tibble::as_tibble(out))
}

#' Fail on an unknown axis, naming what IS available
.check_columns <- function(specs, stats) {
  want <- unique(c(specs$x, specs$y))
  missing <- setdiff(want, colnames(stats))
  if (length(missing)) {
    numeric_cols <- names(stats)[vapply(stats, is.numeric, logical(1))]
    stop("--plot names column(s) that are not in the table: ",
         paste(missing, collapse = ", "),
         "\n  plottable columns: ", paste(numeric_cols, collapse = ", "),
         "\n  (--show_avail_stats also reports how many values each one holds)",
         call. = FALSE)
  }
  return(invisible(NULL))
}

#' Print what can go on an axis, and how much of it is actually there
.print_avail <- function(stats) {
  d <- describe_feature_stats(stats)
  w <- max(nchar(d$column))
  cat("\nColumns available for --plot / --threshold / --color_by:\n\n")
  cat(sprintf("  %-*s  %-8s %-10s %s\n", w, "column", "type", "non-NA", "range"))
  for (i in seq_len(nrow(d))) {
    cat(sprintf("  %-*s  %-8s %-10s %s\n", w, d$column[i], d$type[i],
                d$non_na[i], d$range[i]))
  }
  empty <- d$column[grepl("^0/", d$non_na)]
  if (length(empty)) {
    cat("\n  Note: ", paste(empty, collapse = ", "),
        " hold no values at all -- plotting them gives an empty panel.\n", sep = "")
  }
  cat("\n  Only numeric columns can be a --plot axis.\n\n")
  return(invisible(NULL))
}

#' Say which threshold landed on which axis of which panel, before drawing
#'
#' The whole point of keying thresholds to a column is that they follow the
#' variable around; this makes that visible rather than something to infer from
#' the PDF.
.report_plan <- function(specs, thresholds, stats) {
  for (i in seq_len(nrow(specs))) {
    bits <- c()
    if (specs$x[i] %in% names(thresholds)) {
      bits <- c(bits, paste0("x=", paste(thresholds[[specs$x[i]]], collapse = ",")))
    }
    if (specs$y[i] %in% names(thresholds)) {
      bits <- c(bits, paste0("y=", paste(thresholds[[specs$y[i]]], collapse = ",")))
    }
    n_ok <- sum(!is.na(stats[[specs$x[i]]]) & !is.na(stats[[specs$y[i]]]))
    message("  ", specs$id[i], "  ", specs$y[i], " vs ", specs$x[i],
            "   n=", n_ok,
            if (length(bits)) paste0("   threshold ", paste(bits, collapse = " ")) else "")
  }
  return(invisible(NULL))
}

if (!interactive() && sys.nframe() == 0L) feature_scatter_cli()
