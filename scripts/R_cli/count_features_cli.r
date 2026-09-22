#!/usr/bin/env Rscript

# count_features_cli.r
#
# Annotated features -> tidy per-sample counts, ready to plot.
#
# Reads the *_features.rds written by annotate_features_cli.r, counts distinct
# features per sample per feature type, joins sample metadata, and writes a tidy
# table. This is the oocyte-counting deliverable.
#
#   ./count_features_cli.r --input out/ --sample_sheet samples.tsv \
#       --group_by genotype timepoint --outdir out --plot
#
# A count is the number of distinct feature_id values that carry a real feature
# prefix. ROIs in invalid_* or failed_* groups are reported separately rather
# than dropped quietly -- a sample whose nuclei mostly failed the z-span filter
# should look different from one that genuinely has few nuclei.

suppressPackageStartupMessages({
  library(argparser)
})


# Directory of THIS file, resolved at source time.
#
# NB: commandArgs("--file=") names the *running* script, which is the CLI only
#     when Rscript executed it directly. A test source()s the file instead, and
#     then --file= names the test runner -- so the library lookup silently
#     resolved to the wrong directory. source() keeps the path in `ofile` in its
#     own frame, and that frame is only on the stack WHILE the file is being
#     sourced, which is why this is a top-level assignment and not a function
#     called later.
.THIS_DIR <- (function() {
  # Test route
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    if (!is.null(f$ofile)){
      return(dirname(normalizePath(f$ofile, mustWork = FALSE)))
    }
  }
  # Rscript CLI route
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)){
    return(dirname(normalizePath(sub("^--file=", "", a[1]), mustWork = FALSE)))
  }
  # Interactive session via RStudio (while developing, not real use)
  a <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) character(0))
  if (length(a)){
    return(dirname(a[1]))
  }
  return(NA_character_)
})()

.count_source_helpers <- function() {
  if (exists(".cli_resolve_arg", mode = "function")) return(invisible(NULL))
  if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
  sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
}

# ------------------------------------------------------------------------------

count_features_cli <- function(args = commandArgs(trailingOnly = TRUE)) {

  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)

  .count_source_helpers()

  p <- arg_parser("Count annotated cellular features per sample", hide.opts = TRUE)
  p <- add_argument(p, "--input", short = "-i", type = "character", nargs = Inf, default = NULL,
                    help = "*_features.rds files, globs, or directories to scan")
  p <- add_argument(p, "--outdir", short = "-o", type = "character",
                    help = "directory to write results into")
  p <- add_argument(p, "--sample_sheet", short = "-S", type = "character",
                    help = "optional table with a 'prefix' column; filters inputs and joins metadata")
  p <- add_argument(p, "--id_column", short = "-I", type = "character", default = "prefix",
                    help = "sample sheet column holding the file prefix")
  p <- add_argument(p, "--group_by", short = "-g", type = "character", nargs = Inf, default = NULL,
                    help = "metadata column(s) to summarise over, e.g. genotype timepoint")
  p <- add_argument(p, "--feature", short = "-f", type = "character", nargs = Inf, default = NULL,
                    help = "restrict to these feature type(s) [default: all present]")
  p <- add_argument(p, "--output_prefix", short = "-P", type = "character", default = "",
                    help = "prefix for the output files")
  p <- add_argument(p, "--plot", short = "-p", flag = TRUE,
                    help = "also write a count plot")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character",
                    help = "path to scripts/R [default: alongside this script]")

  argv <- parse_args(p, argv = args)
  .cli_require(argv, c("input", "outdir"))

  .cli_need(c("dplyr", "tibble", "stringr", "sf"))
  suppressPackageStartupMessages({
    library(dplyr); library(tibble); library(stringr); library(sf)
  })

  files <- .cli_resolve_input_path(argv$input, "_features\\.rds$")
  samples_on_disk <- sub("_features\\.rds$", "", basename(files))

  sheet <- NULL
  if (!is.na(argv$sample_sheet)) {
    sheet <- .cli_read_sample_sheet(argv$sample_sheet, argv$id_column)
    jobs <- .cli_apply_sample_sheet(
      data.frame(path = files, sample = samples_on_disk, feature = NA_character_,
                 stringsAsFactors = FALSE),
      sheet, argv$id_column)
    files <- jobs$path
    samples_on_disk <- jobs$sample
  }

  keep_features <- .cli_resolve_arg(argv$feature, "--feature")
  group_by_cols <- .cli_resolve_arg(argv$group_by, "--group_by")

  outdir <- argv$outdir
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(outdir)) stop("Could not create --outdir: ", outdir, call. = FALSE)

  message("Counting features in ", length(files), " file(s)")

  per_sample <- list()
  for (k in seq_along(files)) {
    smp <- samples_on_disk[k]
    x <- readRDS(files[k])

    # NB: check the columns BEFORE st_as_sf(). A file that is not an annotate
    #     output fails inside sf with "no simple features geometry column
    #     present", which names neither the file nor what was expected.
    needed <- c("feature_id", "feature_type")
    absent <- setdiff(needed, colnames(x))
    if (length(absent)) {
      stop("Feature file ", basename(files[k]), " is missing column(s): ",
           paste(absent, collapse = ", "),
           "\n  (was it written by annotate_features_cli.r?)", call. = FALSE)
    }
    if (!inherits(x, "sf")) x <- sf::st_as_sf(x)
    tab <- sf::st_drop_geometry(x)
    if (length(keep_features)) tab <- tab[tab$feature_type %in% keep_features, , drop = FALSE]
    if (!nrow(tab)) {
      warning("No rows left for sample ", smp, " after --feature filtering", call. = FALSE)
      next
    }

    tab$.status <- .feature_status(tab$feature_id)

    counts <- tab |>
      dplyr::distinct(feature_type, feature_id, .status) |>
      dplyr::count(feature_type, .status, name = "n") |>
      tidyr::pivot_wider(names_from = ".status", values_from = "n", values_fill = 0)

    for (col in c("detected", "invalid", "failed")) {
      if (!col %in% colnames(counts)) counts[[col]] <- 0L
    }
    counts <- counts |>
      dplyr::mutate(sample = smp,
                    # unname(): vapply over a character vector names the result
                    # after its input, which would leak into the written TSV.
                    n_roi = unname(vapply(feature_type,
                                          function(ft) sum(tab$feature_type == ft), integer(1)))) |>
      dplyr::select(sample, feature_type, n_detected = detected,
                    n_invalid = invalid, n_failed = failed, n_roi)

    per_sample[[smp]] <- counts
    message("  ", smp, ": ",
            paste(sprintf("%s=%d", counts$feature_type, counts$n_detected), collapse = ", "))
  }

  if (!length(per_sample)) stop("Nothing was counted.", call. = FALSE)

  tidy <- dplyr::bind_rows(per_sample)

  if (!is.null(sheet)) {
    meta <- sheet
    names(meta)[names(meta) == argv$id_column] <- "sample"
    tidy <- dplyr::left_join(tidy, meta, by = "sample")
  }

  missing_group <- setdiff(group_by_cols, colnames(tidy))
  if (length(missing_group)) {
    stop("--group_by column(s) not found: ", paste(missing_group, collapse = ", "),
         "\n  available: ", paste(colnames(tidy), collapse = ", "),
         if (is.null(sheet)) "\n  (did you mean to pass --sample_sheet?)" else "",
         call. = FALSE)
  }

  counts_path <- file.path(outdir, paste0(argv$output_prefix, "feature_counts.tsv"))
  utils::write.table(tidy, counts_path, sep = "\t", quote = FALSE, row.names = FALSE)
  message("Wrote ", nrow(tidy), " row(s) -> ", counts_path)

  if (length(group_by_cols)) {
    summary_df <- tidy |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_by_cols, "feature_type")))) |>
      dplyr::summarise(n_sample = dplyr::n(),
                       mean_detected = mean(n_detected),
                       sd_detected = stats::sd(n_detected),
                       total_detected = sum(n_detected),
                       .groups = "drop")
    summary_path <- file.path(outdir, paste0(argv$output_prefix, "feature_counts_summary.tsv"))
    utils::write.table(summary_df, summary_path, sep = "\t", quote = FALSE, row.names = FALSE)
    message("Wrote ", nrow(summary_df), " summary row(s) -> ", summary_path)
  }

  if (argv$plot) {
    .cli_need("ggplot2")
    suppressPackageStartupMessages(library(ggplot2))
    png_path <- file.path(outdir, paste0(argv$output_prefix, "feature_counts.png"))
    .count_plot(tidy, group_by_cols, png_path)
    message("Wrote ", png_path)
  }

  return(invisible(tidy))
}

# --- private helpers ----------------------------------------------------------

.feature_status <- function(feature_id) {
  # annotate_features_cli names groups <feature>_N, invalid_<feature>_N and
  # failed_<feature>_<reason>. NA means an ROI that reached no group at all.
  out <- rep("detected", length(feature_id))
  out[is.na(feature_id)] <- "failed"
  out[!is.na(feature_id) & startsWith(feature_id, "invalid_")] <- "invalid"
  out[!is.na(feature_id) & startsWith(feature_id, "failed_")]  <- "failed"
  return(out)
}

#' Build the count plot
#'
#' Separate from saving it so a test can inspect the layers. Which layers are
#' present is the behaviour that matters here, and it is invisible in a PNG.
.count_plot_build <- function(tidy, group_by_cols) {
  x_col <- if (length(group_by_cols)) group_by_cols[1] else "sample"

  p <- ggplot2::ggplot(tidy, ggplot2::aes(x = .data[[x_col]], y = n_detected))

  if (!length(group_by_cols)) {
    # One bar per sample. NB: the layer is added conditionally rather than given
    # `data = NULL` -- NULL means "inherit the plot data", so that spelling drew
    # the bars in both branches, overplotting one bar per sample underneath the
    # grouped boxplot.
    p <- p + ggplot2::geom_col(fill = "grey70", width = 0.6)
  } else {
    # Several samples per group: show the points, not a bar of a mean.
    p <- p + ggplot2::geom_boxplot(outlier.shape = NA, fill = NA, colour = "grey50")
    if (requireNamespace("ggbeeswarm", quietly = TRUE)) {
      p <- p + ggbeeswarm::geom_quasirandom(width = 0.15, size = 1.6, alpha = 0.85)
    } else {
      p <- p + ggplot2::geom_jitter(width = 0.15, height = 0, size = 1.6, alpha = 0.85)
    }
    if (length(group_by_cols) > 1) {
      p <- p + ggplot2::aes(colour = .data[[group_by_cols[2]]]) +
        ggplot2::labs(colour = group_by_cols[2])
    }
  }

  p <- p +
    ggplot2::facet_wrap(~ feature_type, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = NULL, y = "features detected")

  return(p)
}


.count_plot <- function(tidy, group_by_cols, path) {
  p <- .count_plot_build(tidy, group_by_cols)
  x_col <- if (length(group_by_cols)) group_by_cols[1] else "sample"
  n_x <- length(unique(tidy[[x_col]]))
  ggplot2::ggsave(path, p, width = max(4, min(12, 1 + n_x * 0.5)), height = 4, dpi = 150)
  return(invisible(path))
}


if (!interactive() && sys.nframe() == 0L) count_features_cli()
