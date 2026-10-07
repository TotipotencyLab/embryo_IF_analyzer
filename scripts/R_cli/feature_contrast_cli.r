#!/usr/bin/env Rscript

# feature_contrast_cli.r
#
# Annotated features + per-ROI ring contrast -> keep or drop each feature.
#
# A post-annotation step for the oocyte count (analysis-oo_count-physical_blur,
# not merged). Run_RoiContrast_Batch.groovy measures, for every ROI on its own
# slice, the mean inside and the mean in a ring a few um outside its edge; this
# turns those into one decision per feature:
#
#   signal ratio = inside / ring on --signal_ch, per ROI, then the AREA-WEIGHTED
#                  mean over the feature's ROIs (its widest slices count most --
#                  the same reasoning as feature_stat_cli.r's default)
#   hole ratio   = the same on --hole_ch
#   keep         = signal ratio >= --min_contrast  OR  hole ratio < --max_hole
#
# Why: a global threshold cannot tell an oocyte from the texture of a bright
# tile -- both are brighter than the section, only the oocyte is brighter than
# its own surroundings. The hole rule keeps the dim growing oocyte, whose DDX4
# contrast can be weak but which sits as a nucleus-free space in a ring of
# granulosa nuclei. The defaults (2.5 / 0.6, signal DDX4 = 2, hole DAPI = 1)
# were chosen on 10 hand-annotated sections and are not validated beyond them.
#
# Output, in --outdir:
#   <sample>_features.rds    the input with each DROPPED feature renamed
#                            invalid_contrast_<feature_id>. Everything downstream
#                            (count_features_cli.r, montage_qc_cli.r,
#                            feature_stat_cli.r) already treats invalid_* as not
#                            counted, and shows it rather than hiding it -- so
#                            the filtered table drops in for the original.
#                            run_id is re-derived, so a table computed on the
#                            UNfiltered features refuses to join onto it.
#   feature_contrast.tsv     one row per counted input feature: its ratios and
#                            the decision
#
#   ./feature_contrast_cli.r --features annotation/feature/ --contrast contrast/ \
#       --outdir annotation/feature_contrast

suppressPackageStartupMessages({
  library(argparser)
})

# Directory of THIS file, resolved at source time (see count_features_cli.r for
# why this is a top-level assignment and not a function called later).
.THIS_DIR <- (function() {
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    if (!is.null(f$ofile)) {
      return(dirname(normalizePath(f$ofile, mustWork = FALSE)))
    }
  }
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) {
    return(dirname(normalizePath(sub("^--file=", "", a[1]), mustWork = FALSE)))
  }
  return(NA_character_)
})()

.fc_source_helpers <- function(rlib = NA) {
  if (!exists(".cli_resolve_arg", mode = "function")) {
    if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
    sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
  }
  # Only run_id_from(), from feature_join.r: this CLI reads finished tables and
  # does no geometry, so the rest of scripts/R/ would only add failure modes.
  if (!exists("run_id_from", mode = "function")) {
    dir <- if (is.na(rlib)) file.path(.THIS_DIR, "..", "R") else rlib
    f <- file.path(dir, "feature_join.r")
    if (!file.exists(f)) stop("cannot locate feature_join.r; pass --rlib_path", call. = FALSE)
    sys.source(f, envir = globalenv())
  }
}

#' The columns Run_RoiContrast_Batch.groovy writes (RoiContrast.COLUMNS)
FC_CONTRAST_COLUMNS <- c("name", "roi", "z", "ch", "area_px", "inside_mean",
                         "ring_area_px", "ring_mean")

#' Prefix a dropped feature takes. Starts with invalid_ on purpose: that is what
#' every downstream reader already excludes from a count.
FC_DROP_PREFIX <- "invalid_contrast_"

# ------------------------------------------------------------------------------

feature_contrast_cli <- function(args = commandArgs(trailingOnly = TRUE)) {

  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)

  .fc_source_helpers()

  dflt_args <- list(feature = "nucleus", signal_ch = 2L, hole_ch = 1L,
                    min_contrast = 2.5, max_hole = 0.6)

  p <- arg_parser("Keep or drop annotated features by their contrast against a surrounding ring",
                  hide.opts = TRUE)
  p <- add_argument(p, "--features", short = "-i", type = "character", nargs = Inf, default = NULL,
                    help = "*_features.rds from annotate_features_cli.r: files, globs or directories")
  p <- add_argument(p, "--contrast", short = "-c", type = "character", nargs = Inf, default = NULL,
                    help = "*_contrast.txt from Run_RoiContrast_Batch.groovy: files, globs or directories")
  p <- add_argument(p, "--outdir", short = "-o", type = "character",
                    help = "directory for the filtered *_features.rds and feature_contrast.tsv (not the input's)")
  p <- add_argument(p, "--feature", short = "-f", type = "character", default = dflt_args$feature,
                    help = "feature type to filter; others pass through unchanged [default: nucleus]")
  p <- add_argument(p, "--signal_ch", short = "-s", type = "integer", default = dflt_args$signal_ch,
                    help = "channel whose inside/ring ratio must be high [default: 2, DDX4]")
  p <- add_argument(p, "--hole_ch", short = "-H", type = "integer", default = dflt_args$hole_ch,
                    help = "channel whose inside/ring ratio being LOW also keeps a feature; 0 = no hole rule [default: 1, DAPI]")
  p <- add_argument(p, "--min_contrast", short = "-m", type = "double", default = dflt_args$min_contrast,
                    help = "keep when the signal ratio is at least this [default: 2.5]")
  p <- add_argument(p, "--max_hole", short = "-x", type = "double", default = dflt_args$max_hole,
                    help = "...or when the hole ratio is below this [default: 0.6]")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character",
                    help = "path to scripts/R [default: alongside this script]")

  argv <- parse_args(p, argv = args)
  .cli_require(argv, c("features", "contrast", "outdir"))
  .fc_source_helpers(argv$rlib_path)

  .cli_need(c("dplyr", "sf"))
  suppressPackageStartupMessages({ library(dplyr); library(sf) })

  feat_name <- argv$feature
  sig_ch <- as.integer(argv$signal_ch); hole_ch <- as.integer(argv$hole_ch)
  min_c <- as.numeric(argv$min_contrast); max_h <- as.numeric(argv$max_hole)
  if (is.na(sig_ch) || sig_ch < 1L) stop("--signal_ch must be a channel number (1-based)", call. = FALSE)
  if (is.na(hole_ch) || hole_ch < 0L) stop("--hole_ch must be a channel number, or 0 for no hole rule", call. = FALSE)
  if (hole_ch == sig_ch) stop("--hole_ch and --signal_ch are the same channel", call. = FALSE)
  if (!is.finite(min_c) || min_c <= 0) stop("--min_contrast must be a positive number", call. = FALSE)
  if (hole_ch > 0L && !is.finite(max_h)) stop("--max_hole must be a number", call. = FALSE)
  message("rule: keep ", feat_name, " when ch", sig_ch, " inside/ring >= ", min_c,
          if (hole_ch > 0L) paste0(" OR ch", hole_ch, " inside/ring < ", max_h) else " (no hole rule)")

  feat_files <- .cli_resolve_input_path(argv$features, "_features\\.rds$", "--features")
  con_files  <- .cli_resolve_input_path(argv$contrast, "_contrast\\.txt$", "--contrast")

  outdir <- normalizePath(argv$outdir, mustWork = FALSE)
  in_dirs <- unique(normalizePath(dirname(feat_files)))
  if (outdir %in% in_dirs) {
    stop("--outdir is the folder the features come from; writing there would replace the ",
         "unfiltered tables. Choose another --outdir.", call. = FALSE)
  }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  # --- contrast tables: identity from the `name` column, not the filename ---
  con <- do.call(rbind, lapply(con_files, function(f) {
    d <- utils::read.delim(f, stringsAsFactors = FALSE, colClasses = "character")
    miss <- setdiff(FC_CONTRAST_COLUMNS, colnames(d))
    if (length(miss)) {
      stop(basename(f), " is not a Run_RoiContrast_Batch table (no ", paste(miss, collapse = ", "),
           ")", call. = FALSE)
    }
    d[, FC_CONTRAST_COLUMNS, drop = FALSE]
  }))
  for (k in c("ch", "area_px", "ring_area_px")) con[[k]] <- as.integer(con[[k]])
  for (k in c("inside_mean", "ring_mean")) con[[k]] <- as.numeric(con[[k]])
  # Header-only tables (a series with no ROIs) contribute nothing, correctly.
  con_samples <- unique(con$name)
  message("contrast: ", nrow(con), " rows from ", length(con_files), " file(s), ",
          length(con_samples), " series")

  # One table per ROI: the ratio on each requested channel.
  ratio_on <- function(ch) {
    d <- con[con$ch == ch, c("name", "roi", "area_px", "inside_mean", "ring_mean")]
    d$ratio <- d$inside_mean / pmax(d$ring_mean, 1e-6)
    d
  }
  sig <- ratio_on(sig_ch)
  if (!nrow(sig)) stop("no contrast rows for --signal_ch ", sig_ch, call. = FALSE)
  hol <- if (hole_ch > 0L) ratio_on(hole_ch) else NULL
  if (hole_ch > 0L && !nrow(hol)) stop("no contrast rows for --hole_ch ", hole_ch, call. = FALSE)

  inputs_desc <- vapply(c(feat_files, con_files), function(f) {
    paste(basename(f), file.size(f))
  }, character(1))

  all_rows <- list()
  for (f in feat_files) {
    x <- readRDS(f)
    smp <- unique(x$sample)
    if (length(smp) != 1L) {
      stop(basename(f), " holds ", length(smp), " samples; expected one per file", call. = FALSE)
    }
    is_counted <- x$feature_type == feat_name & !is.na(x$feature_id) &
      !startsWith(x$feature_id, "invalid_") & !startsWith(x$feature_id, "failed_")
    counted <- x[is_counted, c("roi", "feature_id"), drop = FALSE]
    counted <- sf::st_drop_geometry(counted)
    counted <- as.data.frame(counted, stringsAsFactors = FALSE)

    if (nrow(counted)) {
      if (!smp %in% con_samples) {
        stop("no contrast table for sample ", smp, " (", nrow(counted), " counted ", feat_name,
             " ROIs). Was Run_RoiContrast_Batch run on the segmentation these features came from?",
             call. = FALSE)
      }
      s <- merge(counted, sig[sig$name == smp, ], by = "roi", all.x = TRUE)
      if (anyNA(s$ratio)) {
        stop(sum(is.na(s$ratio)), " of ", nrow(s), " counted ", feat_name, " ROIs of ", smp,
             " have no ch", sig_ch, " contrast row (e.g. ", s$roi[is.na(s$ratio)][1], "). ",
             "The contrast table is from a different segmentation.", call. = FALSE)
      }
      agg <- s %>% group_by(feature_id) %>%
        # The weighted mean FIRST: summarise() evaluates in order, so a column
        # redefined earlier (area_px = sum(area_px)) would be the weight here.
        summarise(signal_ratio = sum(ratio * area_px) / sum(area_px),
                  n_roi = n(), area_px = sum(area_px), .groups = "drop")
      agg$hole_ratio <- NA_real_
      if (hole_ch > 0L) {
        h <- merge(counted, hol[hol$name == smp, ], by = "roi", all.x = TRUE)
        if (anyNA(h$ratio)) {
          stop(sum(is.na(h$ratio)), " counted ROIs of ", smp, " have no ch", hole_ch,
               " contrast row; was the contrast batch run with that channel?", call. = FALSE)
        }
        hagg <- h %>% group_by(feature_id) %>%
          summarise(hole_ratio = sum(ratio * area_px) / sum(area_px), .groups = "drop")
        agg$hole_ratio <- hagg$hole_ratio[match(agg$feature_id, hagg$feature_id)]
      }
      agg$keep <- agg$signal_ratio >= min_c | (hole_ch > 0L & !is.na(agg$hole_ratio) & agg$hole_ratio < max_h)
      agg$new_feature_id <- ifelse(agg$keep, agg$feature_id, paste0(FC_DROP_PREFIX, agg$feature_id))
      agg <- data.frame(sample = smp, agg, stringsAsFactors = FALSE)
      all_rows[[length(all_rows) + 1L]] <- agg

      ren <- stats::setNames(agg$new_feature_id, agg$feature_id)
      hit <- is_counted & x$feature_id %in% names(ren)
      x$feature_id[hit] <- unname(ren[x$feature_id[hit]])
      message(sprintf("  %s: %d %s kept, %d dropped", smp, sum(agg$keep), feat_name, sum(!agg$keep)))
    } else {
      message("  ", smp, ": no counted ", feat_name, "; passed through unchanged")
    }

    # A new run: a table computed on the unfiltered features must not join here.
    old_rid <- unique(stats::na.omit(x$run_id))
    if ("run_id" %in% colnames(x)) {
      x$run_id <- run_id_from(c(old_rid, "feature_contrast", feat_name, sig_ch, hole_ch,
                                min_c, max_h, sort(inputs_desc)))
    }
    saveRDS(x, file.path(outdir, basename(f)))
  }

  res <- if (length(all_rows)) do.call(rbind, all_rows) else
    data.frame(sample = character(0), feature_id = character(0), n_roi = integer(0),
               area_px = integer(0), signal_ratio = numeric(0), hole_ratio = numeric(0),
               keep = logical(0), new_feature_id = character(0))
  out_tab <- res
  out_tab$signal_ratio <- round(out_tab$signal_ratio, 4)
  out_tab$hole_ratio <- round(out_tab$hole_ratio, 4)
  utils::write.table(out_tab, file.path(outdir, "feature_contrast.tsv"), sep = "\t",
                     quote = FALSE, row.names = FALSE, na = "")
  writeLines(c("parameter\tvalue",
               paste0("feature\t", feat_name), paste0("signal_ch\t", sig_ch),
               paste0("hole_ch\t", hole_ch), paste0("min_contrast\t", min_c),
               paste0("max_hole\t", max_h),
               paste0("features\t", paste(in_dirs, collapse = " ")),
               paste0("contrast\t", paste(unique(dirname(con_files)), collapse = " "))),
             file.path(outdir, "feature_contrast_params.txt"))
  message(sprintf("%d %s features: %d kept, %d dropped (%s* in the filtered tables) -> %s",
                  nrow(res), feat_name, sum(res$keep), sum(!res$keep), FC_DROP_PREFIX, outdir))
  return(invisible(res))
}

if (!interactive() && sys.nframe() == 0L) feature_contrast_cli()
