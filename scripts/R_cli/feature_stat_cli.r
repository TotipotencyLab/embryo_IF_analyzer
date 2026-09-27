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
                                 "Adds a 'volume' column = area_sum x z_step.",
                                 "[default: pixel_depth from each sample's _config.txt,",
                                 "which Fiji has recorded since 0.2.0]"))
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
  # What happened is reported AFTER the loop, once it is known: whether a
  # z step was found is a fact about the files, not about the arguments.
  z_used <- c()

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
    # An explicit --z_step wins everywhere; otherwise each file answers for
    # itself out of the config Fiji wrote beside it.
    z <- if (!is.na(argv$z_step)) argv$z_step else .z_step_for(feats, path, res_dirs)
    if (!is.na(z)) z_used[[basename(path)]] <- z

    st <- summarise_feature_stats(feats, res = res,
                                  channel_stat = argv$channel_stat,
                                  meta_cols = meta_cols,
                                  z_step = if (is.na(z)) NULL else z)
    if (!nrow(st)) next
    per_file[[path]] <- st
    rejects[[path]] <- feature_reject_counts(feats)
  }

  if (!length(per_file)) stop("No features to summarise.", call. = FALSE)
  stats <- dplyr::bind_rows(per_file)

  message("  ", nrow(stats), " feature(s) across ",
          dplyr::n_distinct(stats$sample), " sample(s)")

  # Where the volume column came from, or why there is not one. area_med only
  # stands in for size while the object is a sphere, so whether volume exists is
  # worth stating rather than leaving the reader to hunt for it in
  # --show_avail_stats and find nothing.
  if (!is.na(argv$z_step)) {
    message("  volume = area_sum x ", argv$z_step, " (--z_step)")
  } else if (length(z_used)) {
    vals <- sort(unique(unlist(z_used)))
    message("  volume = area_sum x pixel_depth from _config.txt (",
            length(z_used), " file(s); ", paste(vals, collapse = ", "), ")")
    if (length(vals) > 1L) {
      message("    NB: the z step differs between files. Each used its own, ",
              "which is right -- but a statistic pooled across them is not in ",
              "one instrument's units.")
    }
  } else {
    message("  no 'volume' column: no --z_step, and no usable pixel_depth in a ",
            "_config.txt beside the inputs. area_sum is the shape-free size ",
            "measure meanwhile. (pixel_depth is blank for single-plane images, ",
            "where there is no z axis to measure.)")
  }
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
#' The z step for one features file, from the Fiji config that produced it
#'
#' `pixel_depth` is written per image, so this is asked per file -- and a
#' features .rds is written per sample, so per file IS per sample in anything
#' the pipeline produced. A hand-built file holding several samples that
#' disagree gets no inference rather than an arbitrary one of them: pixel size
#' varies 4x within a single .lif here, so "they are all about the same" is not
#' a safe assumption to make quietly.
#'
#' @return a positive number, or NA_real_
.z_step_for <- function(feats, features_path, res_dirs) {
  tab <- sf::st_drop_geometry(feats)
  here <- dirname(features_path)
  dirs <- unique(c(res_dirs, here,
                   file.path(here, ".."),
                   file.path(here, "..", "segmentation")))
  vals <- c()
  for (smp in unique(tab$sample)) {
    cfg <- .cli_read_config(.cli_find_config(smp, dirs))
    v <- .cli_config_num(cfg, "pixel_depth")
    # Blank for a single plane, by design on the Fiji side: no z axis, no volume.
    if (!is.na(v) && v > 0) vals[[smp]] <- v
  }
  if (!length(vals)) return(NA_real_)
  if (length(unique(unlist(vals))) > 1L) {
    warning("Samples in ", basename(features_path), " record different ",
            "pixel_depth values (", paste(sort(unique(unlist(vals))), collapse = ", "),
            "); no volume inferred. Pass --z_step to choose one.", call. = FALSE)
    return(NA_real_)
  }
  return(unname(unlist(vals))[1])
}

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
#'
#' A column may already be here. The commonest reason is the benign one: the
#' SAME sheet was passed to annotate_features_cli.r, which binds its metadata
#' onto every feature row, so `genotype` arrives in the .rds and is carried
#' through as metadata -- by the very route this join would take. Guarding the
#' sheet against every column of `stats` therefore refused the ordinary
#' end-to-end run, telling the user to rename a column that was already theirs.
#'
#' Re-joining it is not the answer either: left_join would produce
#' `genotype.x`/`genotype.y` and every later reference to `genotype` -- a
#' --group_by, a --class rule, a plot facet -- would resolve to neither.
#'
#' So a shared column is reconciled instead. Equal per sample means it is the
#' same metadata and the join simply skips it. Unequal means the features were
#' annotated from a DIFFERENT sheet than the one being passed now, or a sheet
#' column is named after a statistic this step computes. Either way, silently
#' choosing one of the two would attach the wrong metadata to real numbers.
.cli_join_sheet <- function(stats, sheet, id_column) {
  .cli_check_reserved(sheet, id_column)
  meta <- sheet
  names(meta)[names(meta) == id_column] <- "sample"

  shared <- setdiff(base::intersect(colnames(meta), colnames(stats)), "sample")
  if (length(shared)) {
    # Metadata is constant within a sample, so one row per sample is the whole
    # comparison.
    have <- stats[!duplicated(stats$sample), c("sample", shared), drop = FALSE]
    cmp <- merge(have, meta[, c("sample", shared), drop = FALSE],
                 by = "sample", suffixes = c(".have", ".sheet"))
    disagree <- character(0)
    for (col in shared) {
      a <- as.character(cmp[[paste0(col, ".have")]])
      b <- as.character(cmp[[paste0(col, ".sheet")]])
      if (!isTRUE(all.equal(a, b))) disagree <- c(disagree, col)
    }
    if (length(disagree)) {
      stop("Sample sheet column(s) disagree with what is already on the features: ",
           paste(disagree, collapse = ", "),
           "\n  The features were annotated from a different sample sheet, or a ",
           "sheet column is named after a statistic this step computes.",
           "\n  Re-run annotate_features_cli.r with this sheet, or rename the ",
           "column (e.g. ", disagree[1], " -> sample_", disagree[1], ").",
           call. = FALSE)
    }
    message("  sample sheet: ", length(shared),
            " column(s) already on the features, not re-joined (",
            paste(utils::head(shared, 6), collapse = ", "),
            if (length(shared) > 6) ", ..." else "", ")")
    meta <- meta[, setdiff(colnames(meta), shared), drop = FALSE]
  }

  unmatched <- setdiff(stats$sample, meta$sample)
  if (length(unmatched)) {
    warning(length(unmatched), " sample(s) are not in the sample sheet: ",
            paste(utils::head(unmatched, 5), collapse = ", "), call. = FALSE)
  }
  if (ncol(meta) <= 1L) {
    return(stats)
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
