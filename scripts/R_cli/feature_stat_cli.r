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
  if (!exists(".cli_resolve_arg", mode = "function")) {
    if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
    sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
  }
  # The reserved class names are needed while BUILDING the parser, and
  # .source_rlib() does not run until after parse_args() -- it takes
  # --rlib_path, which does not exist yet. Sourcing the one file here keeps the
  # help text and the check that enforces it reading from the same constant;
  # spelling the names into the help string instead is how the two drift.
  if (!exists("CLASS_RESERVED")) {
    f <- file.path(.THIS_DIR, "..", "R", "classify_features.r")
    if (file.exists(f)) sys.source(f, envir = globalenv())
  }
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
  p <- add_argument(p, "--z_step", short = "-z", type = "numeric", default = NA,
                    help = paste("distance between slices, in the outline unit (microns).",
                                 "Adds a 'volume' column = area_sum x z_step. Fiji does not",
                                 "record pixel_depth in _config.txt, so it must be given here"))
  p <- add_argument(p, "--class", short = "-k", type = "character", nargs = Inf, default = NULL,
                    help = paste("assign each feature to a class from its own statistics.",
                                 "Space-separated tokens on ONE flag, not a repeated flag:",
                                 "--class 'big:area_med=600:Inf' 'big:circ_med=0:0.7'",
                                 "'small:area_med=0:600'. Tokens sharing a class name are",
                                 "ANDed; the order the names first appear is the priority",
                                 "when a feature matches several.",
                                 "RESERVED, and refused as class names:",
                                 paste(if (exists("CLASS_RESERVED")) CLASS_RESERVED
                                       else c("unclassified", "other"), collapse = " and "),
                                 "-- the first is what a feature matching no class is called,",
                                 "the second is the group the plots fold unmapped classes",
                                 "into. Both are settable in montage_qc_cli.r --color_map"))
  p <- add_argument(p, "--drop_orphan_feature", short = "-D", flag = TRUE,
                    help = paste("drop features matching no --class [default: keep them,",
                                 "class =", paste0("'", if (exists("CLASS_UNCLASSIFIED"))
                                                           CLASS_UNCLASSIFIED else "unclassified", "'"),
                                 "-- a string, not NA, so table() cannot drop them silently]"))
  p <- add_argument(p, "--log_scale", short = "-x", type = "character", nargs = Inf, default = NULL,
                    help = paste("columns to draw on a log10 axis, as names or globs:",
                                 "--log_scale volume 'area_*'. Quote a glob so the shell",
                                 "does not expand it. Nothing is logged unless named"))
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
  # Say why a column is absent, rather than leaving the reader to hunt for it in
  # --show_avail_stats and find nothing. area_med only stands in for size while
  # the object is a sphere, so the volume route matters for irregular ones.
  if (is.na(argv$z_step)) {
    message("No --z_step given, so no 'volume' column. ",
            "area_sum is the shape-free size measure; --z_step turns it into ",
            "volume (Fiji does not record pixel_depth, so it cannot be inferred).")
  }

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
                                  meta_cols = meta_cols,
                                  z_step = if (is.na(argv$z_step)) NULL else argv$z_step)
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

  # Before the --group_by check on purpose, so `--group_by class` can name the
  # column this step creates.
  class_spec <- .cli_class_spec(argv$class)
  if (length(class_spec)) {
    stats <- classify_features(stats, class_spec,
                               drop_orphan = argv$drop_orphan_feature)
    cc <- class_counts(stats)
    message("Classes (priority ", paste(names(class_spec), collapse = " > "), "):")
    for (i in seq_len(nrow(cc))) {
      message("  ", format(cc$class[i], width = max(nchar(cc$class))), "  ", cc$n[i])
    }
    n_orphan <- attr(stats, "n_orphan")
    if (!is.null(n_orphan) && n_orphan > 0) {
      if (argv$drop_orphan_feature) {
        message("  dropped ", n_orphan, " feature(s) matching no class (--drop_orphan_feature)")
      } else {
        # Kept, and said out loud: an unclassified feature is evidence about
        # the class boundaries, not a nuisance row.
        message("  kept ", n_orphan, " unclassified feature(s); ",
                "--drop_orphan_feature removes them")
      }
    }
  } else if (isTRUE(argv$drop_orphan_feature)) {
    warning("--drop_orphan_feature does nothing without --class", call. = FALSE)
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
    # Resolved here, against the columns the stats table actually has, so a
    # pattern naming a channel that this data does not carry is caught.
    log_cols <- .cli_log_cols(argv$log_scale, colnames(stats))

    # Advisory, exactly as --show_avail_stats is in the scatter CLI: span says a
    # log axis would spread the points, not that logging the quantity means
    # anything. Only when nothing was asked for, so it is a hint and not noise.
    if (!length(log_cols)) {
      cand <- log_axis_candidates(stats, cols = feature_stat_value_cols(stats))
      if (length(cand)) {
        message("  Wide enough that --log_scale may help: ",
                paste(sprintf("%s (%sx)", names(cand), format(cand, digits = 3)),
                      collapse = "  "))
      }
    }

    plots <- list()
    for (g in group_by) {
      pl <- plot_feature_stat_list(
        stats, value_cols = if (length(want_stats)) want_stats else NULL,
        group_col = g, types = plot_types,
        colour_by = if (is.na(argv$colour_by)) NULL else argv$colour_by,
        log_cols = log_cols)
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
