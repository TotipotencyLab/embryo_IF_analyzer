#!/usr/bin/env Rscript

# feature_stat_cli.r
#
# Per-feature statistics and their distributions, from the *_features.rds that
# annotate_features_cli.r writes.
#
# This is the threshold-finding step. Size, z-extent, shape and per-channel
# signal are summarised one row per detected object, then plotted so that the
# same cut can be checked across files before it is committed to. Choosing a
# class boundary or a signal filter from a per-ROI distribution would be a
# mistake: adjacent slices of one object are not replicates.
#
#   ./feature_stat_cli.r --input results/ --outdir stats/ \
#       --res_dir segmentation/ --group_by sample --plot
#
# Channel signal appears only when the Fiji measurement tables can be found.
# They are looked up as <sample>_<roi prefix>_res.txt, where the roi prefix is
# read from the `roi` column -- so this still works after --rename, when the
# reporting name and the name on disk differ.
#
# See note/if_quantification.md for what these numbers do and do not support:
# there is no background correction here, and intensities are not yet
# comparable between images.

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

.stat_source_helpers <- function() {
  if (exists(".cli_resolve_arg", mode = "function")) return(invisible(NULL))
  if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
  sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
}

# ------------------------------------------------------------------------------

feature_stat_cli <- function(args = commandArgs(trailingOnly = TRUE)) {

  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)

  .stat_source_helpers()

  p <- arg_parser("Per-feature statistics and their distributions", hide.opts = TRUE)
  p <- add_argument(p, "--input", short = "-i", type = "character", nargs = Inf, default = NULL,
                    help = "*_features.rds files, globs, or directories to scan")
  p <- add_argument(p, "--outdir", short = "-o", type = "character",
                    help = "directory to write results into")
  p <- add_argument(p, "--res_dir", short = "-e", type = "character", nargs = Inf, default = NULL,
                    help = "directory holding the Fiji *_res.txt [default: beside the features]")
  p <- add_argument(p, "--sample_sheet", short = "-S", type = "character",
                    help = "optional table with a 'prefix' column; joins metadata")
  p <- add_argument(p, "--id_column", short = "-I", type = "character", default = "prefix",
                    help = "sample sheet column holding the file prefix")
  p <- add_argument(p, "--group_by", short = "-g", type = "character", nargs = Inf, default = NULL,
                    help = "column to group the plots by [default: sample]")
  p <- add_argument(p, "--feature", short = "-f", type = "character", nargs = Inf, default = NULL,
                    help = "restrict to these feature type(s)")
  p <- add_argument(p, "--stat", short = "-s", type = "character", nargs = Inf, default = NULL,
                    help = "statistics to plot [default: every numeric one present]")
  p <- add_argument(p, "--channel_stat", short = "-c", type = "character", default = "wmean",
                    help = "per-channel aggregation: wmean, mean, median, sd, min, max, sum")
  p <- add_argument(p, "--plot_type", short = "-t", type = "character", nargs = Inf, default = NULL,
                    help = "any of box, violin, quasirandom [default: box quasirandom]")
  p <- add_argument(p, "--colour_by", short = "-C", type = "character",
                    help = "column mapped to point colour")
  p <- add_argument(p, "--output_prefix", short = "-X", type = "character", default = "",
                    help = "prefix for the output files")
  p <- add_argument(p, "--plot_width", short = "-W", type = "double", default = 8,
                    help = "PDF page width, inches")
  p <- add_argument(p, "--plot_height", short = "-H", type = "double", default = 6,
                    help = "PDF page height, inches")
  p <- add_argument(p, "--no_plot", short = "-N", flag = TRUE,
                    help = "write the table only, no PDF")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character",
                    help = "path to scripts/R [default: alongside this script]")

  argv <- parse_args(p, argv = args)
  .cli_require(argv, c("input", "outdir"))

  # stringr: read_fiji_result() calls str_remove() unqualified. scripts/R is
  # sourced, not installed, so nothing attaches its dependencies for us.
  .cli_need(c("dplyr", "tibble", "tidyr", "stringr", "sf"))
  suppressPackageStartupMessages({
    library(dplyr); library(tibble); library(tidyr); library(stringr); library(sf)
  })
  if (!argv$no_plot) {
    .cli_need("ggplot2")
    suppressPackageStartupMessages(library(ggplot2))
    if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
      warning("ggbeeswarm is not installed; points will be jittered instead of ",
              "quasirandom. Same information, less even spacing.", call. = FALSE)
    }
  }
  .source_rlib(argv$rlib_path, .THIS_DIR)

  plot_types <- .cli_resolve_arg(argv$plot_type, "--plot_type")
  if (!length(plot_types)) plot_types <- c("box", "quasirandom")
  group_by <- .cli_resolve_arg(argv$group_by, "--group_by")
  if (!length(group_by)) group_by <- "sample"
  keep_features <- .cli_resolve_arg(argv$feature, "--feature")
  want_stats <- .cli_resolve_arg(argv$stat, "--stat")
  res_dirs <- .cli_resolve_arg(argv$res_dir, "--res_dir")

  files <- .cli_resolve_input_path(argv$input, "_features\\.rds$")
  message("Summarising ", length(files), " feature file(s)")

  outdir <- argv$outdir
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(outdir)) stop("Could not create --outdir: ", outdir, call. = FALSE)

  sheet <- NULL
  if (!is.na(argv$sample_sheet)) {
    sheet <- .cli_read_sample_sheet(path = argv$sample_sheet, id_column = argv$id_column)
  }

  # --- summarise ---------------------------------------------------------------
  per_file <- list()
  rejects <- list()
  n_with_signal <- 0L
  for (path in files) {
    feats <- readRDS(path)
    if (!nrow(feats)) {
      warning("Empty feature table, skipping: ", basename(path), call. = FALSE)
      next
    }
    if (length(keep_features)) {
      feats <- feats[feats$feature_type %in% keep_features, , drop = FALSE]
      if (!nrow(feats)) next
    }

    res <- .read_res_for(feats, path, res_dirs)
    if (!is.null(res)) n_with_signal <- n_with_signal + 1L

    # Everything that is not a known per-ROI column is treated as sample
    # metadata and carried through. is_bridge belongs in this list: it is an
    # ROI-level fact that VARIES within a feature, so leaving it out made
    # summarise_feature_stats() warn about a metadata column varying and then
    # take an arbitrary first value -- producing an is_bridge column on the
    # per-feature table that invites exactly the wrong filter. n_bridge and
    # frac_bridge are the feature-level answer.
    meta_cols <- setdiff(colnames(sf::st_drop_geometry(feats)),
                         c("roi", "z", "area", "is_bridge",
                           "feature_id", "feature_type", "sample",
                           "parent_feature_id", "parent_feature_type",
                           "parent_containment", "parent_match"))
    st <- summarise_feature_stats(feats, res = res,
                                  channel_stat = argv$channel_stat,
                                  meta_cols = meta_cols)
    if (!nrow(st)) next
    per_file[[path]] <- st
    rejects[[path]] <- feature_reject_counts(feats)
  }

  if (!length(per_file)) stop("No features to summarise.", call. = FALSE)
  stats <- dplyr::bind_rows(per_file)

  message("  ", nrow(stats), " feature(s) across ",
          dplyr::n_distinct(stats$sample), " sample(s)")
  if (n_with_signal == 0L) {
    # Not fatal, but the single most likely reason the plots look thin.
    warning("No measurement table was found for any sample, so there is NO ",
            "channel signal in this output. Pass --res_dir pointing at the ",
            "Fiji results.", call. = FALSE)
  } else if (n_with_signal < length(per_file)) {
    warning(length(per_file) - n_with_signal, " of ", length(per_file),
            " sample(s) had no measurement table; their signal columns are NA",
            call. = FALSE)
  }

  if (!is.null(sheet)) {
    stats <- .cli_join_sheet(stats, sheet, argv$id_column)
  }

  missing_group <- setdiff(group_by, colnames(stats))
  if (length(missing_group)) {
    stop("--group_by names column(s) that are not present: ",
         paste(missing_group, collapse = ", "),
         "\n  available: ", paste(colnames(stats), collapse = ", "), call. = FALSE)
  }

  # --- write --------------------------------------------------------------------
  tsv <- file.path(outdir, paste0(argv$output_prefix, "feature_stats.tsv"))
  utils::write.table(stats, tsv, sep = "\t", quote = FALSE, row.names = FALSE)
  message("  -> ", basename(tsv))

  rej <- dplyr::bind_rows(rejects)
  if (nrow(rej)) {
    rej_path <- file.path(outdir, paste0(argv$output_prefix, "feature_rejects.tsv"))
    utils::write.table(rej, rej_path, sep = "\t", quote = FALSE, row.names = FALSE)
    message("  -> ", basename(rej_path))
    .report_rejects(rej)
  }

  if (!argv$no_plot) {
    pdf_path <- file.path(outdir, paste0(argv$output_prefix, "feature_stats.pdf"))
    plots <- list()
    for (g in group_by) {
      pl <- plot_feature_stat_list(
        stats, value_cols = if (length(want_stats)) want_stats else NULL,
        group_col = g, types = plot_types,
        colour_by = if (is.na(argv$colour_by)) NULL else argv$colour_by)
      if (length(group_by) > 1) names(pl) <- paste0(names(pl), " by ", g)
      plots <- c(plots, pl)
    }
    save_plot_list(plots, pdf_path, width = argv$plot_width, height = argv$plot_height)
    message("  -> ", basename(pdf_path), " (", length(plots), " page(s))")
  }

  return(invisible(stats))
}

# --- private helpers ----------------------------------------------------------

#' Find and read the Fiji measurement table matching a feature table
#'
#' Looked up by the ROI PREFIX, not by feature_type: after --rename the
#' reporting name is the new one while the file on disk still carries the name
#' Fiji wrote. The `roi` column keeps that original prefix for exactly this
#' reason.
.read_res_for <- function(feats, features_path, res_dirs) {
  tab <- sf::st_drop_geometry(feats)
  here <- dirname(features_path)
  dirs <- unique(c(res_dirs, here,
                   file.path(here, ".."),
                   file.path(here, "..", "segmentation")))
  out <- list()
  for (smp in unique(tab$sample)) {
    rows <- tab[tab$sample == smp, , drop = FALSE]
    for (ft in unique(rows$feature_type)) {
      prefix <- feature_roi_prefix(rows$roi[rows$feature_type == ft])
      if (is.na(prefix)) next
      cands <- file.path(dirs, paste0(smp, "_", prefix, "_res.txt"))
      hit <- cands[file.exists(cands)]
      if (!length(hit)) next
      one <- tryCatch(read_fiji_result(hit[1]), error = function(e) {
        warning("Could not read ", basename(hit[1]), ": ", conditionMessage(e),
                call. = FALSE)
        NULL
      })
      if (!is.null(one)) out[[paste(smp, ft)]] <- one
    }
  }
  if (!length(out)) return(NULL)
  return(out)
}

#' Join sample metadata onto the per-feature table
.cli_join_sheet <- function(stats, sheet, id_column) {
  .cli_check_reserved(sheet, id_column, extra = colnames(stats))
  meta <- sheet
  names(meta)[names(meta) == id_column] <- "sample"
  unmatched <- setdiff(stats$sample, meta$sample)
  if (length(unmatched)) {
    warning(length(unmatched), " sample(s) are not in the sample sheet: ",
            paste(utils::head(unmatched, 5), collapse = ", "), call. = FALSE)
  }
  return(dplyr::left_join(stats, meta, by = "sample"))
}

#' Say what did not become a feature, so a thin plot can be read correctly
.report_rejects <- function(rej) {
  wide <- rej %>%
    dplyr::group_by(bucket) %>%
    dplyr::summarise(n_roi = sum(n_roi), .groups = "drop")
  parts <- paste0(wide$bucket, "=", wide$n_roi)
  message("  ROI fate: ", paste(parts, collapse = "  "))
  return(invisible(NULL))
}

if (!interactive() && sys.nframe() == 0L) feature_stat_cli()
