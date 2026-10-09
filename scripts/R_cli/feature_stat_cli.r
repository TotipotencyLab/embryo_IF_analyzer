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
#       --res_dir segmentation/ --group_by series_id --plot
#
# Channel signal appears only when the Fiji measurement tables can be found.
# They are looked up as <series_id>_<roi prefix>_res.txt, where the roi prefix is
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
  p <- add_argument(p, "--series_sheet", short = "-S", type = "character",
                    help = "optional series table (series.tsv), keyed by its 'series_id' column; joins metadata")
  p <- add_argument(p, "--id_column", short = "-I", type = "character", default = "series_id",
                    help = "series table column holding the series_id -- the file prefix")
  p <- add_argument(p, "--group_by", short = "-g", type = "character", nargs = Inf, default = NULL,
                    help = "column to group the plots by [default: series_id]")
  p <- add_argument(p, "--feature", short = "-f", type = "character", nargs = Inf, default = NULL,
                    help = "restrict to these feature type(s)")
  p <- add_argument(p, "--stat", short = "-s", type = "character", nargs = Inf, default = NULL,
                    help = "statistics to plot [default: every numeric one present]")
  p <- add_argument(p, "--channel_stat", short = "-c", type = "character", default = "wmean",
                    help = "per-channel aggregation: wmean, mean, median, sd, min, max, sum")
  p <- add_argument(p, "--z_step", short = "-z", type = "numeric", default = NA,
                    help = paste("distance between slices, in the outline unit (microns).",
                                 "Adds a 'volume' column = area_sum x z_step.",
                                 "[default: pixel_depth from each series' _config.txt,",
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
                                 "into. Both are settable in feature_outline_cli.r --color_map"))
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
  p <- add_argument(p, "--track_type", short = "-y", type = "character", default = NA,
                    help = paste("join the tracks Make_FeatureTracks wrote for this feature type:",
                                 "adds track_id, branch_id, branch_merged to every row and",
                                 "writes <prefix>branches.tsv"))
  p <- add_argument(p, "--tracks_dir", short = "-K", type = "character", default = NA,
                    help = "where the tracks tables are [default: beside each features file]")
  p <- add_argument(p, "--force", short = "-U", flag = TRUE,
                    help = "join the tracks even when their run_id disagrees")
  p <- add_argument(p, "--keep_merged_branches", short = "-M", flag = TRUE,
                    help = paste("with --group_by branch_id, plot merged branches too",
                                 "(they describe two objects, so they are left out by default)"))
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
  if (!length(group_by)) group_by <- "series_id"
  keep_features <- .cli_resolve_arg(argv$feature, "--feature")
  want_stats <- .cli_resolve_arg(argv$stat, "--stat")
  res_dirs <- .cli_resolve_arg(argv$res_dir, "--res_dir")

  track_type <- if (is.na(argv$track_type)) NA_character_ else argv$track_type
  if (is.na(track_type)) {
    for (o in c("tracks_dir", "force", "keep_merged_branches")) {
      if (!identical(argv[[o]], NA) && !identical(argv[[o]], FALSE)) {
        warning("--", o, " does nothing without --track_type", call. = FALSE)
      }
    }
  }
  # A track can hold two cells at one t (a mother's daughters), so a statistic
  # pooled over it averages different objects -- the frames-summed-into-a-count
  # mistake again. What a lineage supports is a count, which count_features does.
  if ("track_id" %in% group_by) {
    stop("--group_by track_id would pool a lineage's cells into one distribution. ",
         "Group by branch_id (one object through time), or count features per track ",
         "and t: count_features_cli.r --feature_table <this feature_stats.tsv> ",
         "--feature <type> --feature_class_by track_id", call. = FALSE)
  }

  files <- .cli_resolve_input_path(argv$input, "_features\\.rds$")
  message("Summarising ", length(files), " feature file(s)")

  outdir <- argv$outdir
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(outdir)) stop("Could not create --outdir: ", outdir, call. = FALSE)

  sheet <- NULL
  if (!is.na(argv$series_sheet)) {
    sheet <- .cli_read_series_sheet(path = argv$series_sheet, id_column = argv$id_column)
  }

  # --- summarise ---------------------------------------------------------------
  per_file <- list()
  rejects <- list()
  branches <- list()
  # Say why a column is absent, rather than leaving the reader to hunt for it in
  # --show_avail_stats and find nothing. area_med only stands in for size while
  # the object is a sphere, so the volume route matters for irregular ones.
  # What happened is reported AFTER the loop, once it is known: whether a
  # z step was found is a fact about the files, not about the arguments.
  z_used <- c()

  n_with_signal <- 0L
  for (path in files) {
    feats <- .cli_require_series_id(readRDS(path), path)
    if (!nrow(feats)) {
      warning("Empty feature table, skipping: ", basename(path), call. = FALSE)
      next
    }
    if (length(keep_features)) {
      feats <- feats[feats$feature_type %in% keep_features, , drop = FALSE]
      if (!nrow(feats)) next
    }

    # Joined before summarising, so the three columns ride through as
    # per-feature metadata like any sheet column.
    if (!is.na(track_type)) {
      tdir <- if (is.na(argv$tracks_dir)) dirname(path) else argv$tracks_dir
      feats <- join_tracks(feats, track_type, tdir, force = argv$force)
      branches[[path]] <- attr(feats, "branches")
    }

    res <- .read_res_for(feats, path, res_dirs)
    if (!is.null(res)) n_with_signal <- n_with_signal + 1L

    # Everything that is not a known per-ROI column is treated as series
    # metadata and carried through. is_bridge belongs in this list: it is an
    # ROI-level fact that VARIES within a feature, so leaving it out made
    # summarise_feature_stats() warn about a metadata column varying and then
    # take an arbitrary first value -- producing an is_bridge column on the
    # per-feature table that invites exactly the wrong filter. n_bridge and
    # frac_bridge are the feature-level answer.
    meta_cols <- setdiff(colnames(sf::st_drop_geometry(feats)),
                         c("roi", "z", "area", "is_bridge",
                           "feature_id", "feature_type", "series_id", "t",
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
          dplyr::n_distinct(stats$series_id), " series")

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
    warning("No measurement table was found for any series, so there is NO ",
            "channel signal in this output. Pass --res_dir pointing at the ",
            "Fiji results.", call. = FALSE)
  } else if (n_with_signal < length(per_file)) {
    warning(length(per_file) - n_with_signal, " of ", length(per_file),
            " series had no measurement table; their signal columns are NA",
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

  # join_tracks() gives a series with no valid feature of the type NA ids --
  # an empty position is a result. A type found in NO input is a typo.
  if (!is.na(track_type) && !track_type %in% stats$feature_type) {
    stop("--track_type ", track_type, ": no feature of that type in any input; types present: ",
         paste(sort(unique(stats$feature_type)), collapse = ", "), call. = FALSE)
  }

  # --- write --------------------------------------------------------------------
  tsv <- file.path(outdir, paste0(argv$output_prefix, "feature_stats.tsv"))
  utils::write.table(stats, tsv, sep = "\t", quote = FALSE, row.names = FALSE)
  message("  -> ", basename(tsv))

  if (!is.na(track_type)) {
    br <- dplyr::bind_rows(branches)
    br_path <- file.path(outdir, paste0(argv$output_prefix, "branches.tsv"))
    utils::write.table(br, br_path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
    n_untracked <- sum(stats$feature_type == track_type & is.na(stats$track_id))
    message("  ", length(unique(paste(br$series_id, br$track_id))), " ", track_type,
            " track(s), ", nrow(br), " branch(es), ", sum(br$branch_merged),
            " of them merged; ", n_untracked, " feature(s) linked to nothing")
    message("  -> ", basename(br_path))
  }

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
      data_g <- stats
      if (g == "branch_id") {
        data_g <- .branch_rows(stats, track_type, argv$keep_merged_branches)
      }
      pl <- plot_feature_stat_list(
        data_g, value_cols = if (length(want_stats)) want_stats else NULL,
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

#' The rows a per-branch plot is drawn from, labelled so that a box is one cell
#'
#' A per-branch distribution is a per-cell summary: a merged branch is two
#' objects, and an untracked feature -- or one of another type -- is in no
#' branch at all, so those are left out (merged ones kept on request). Branch
#' ids are numbered per series, so with several series each is prefixed with
#' its series_id: nucleus_track_0001_b001 in two series is two cells, and one
#' box for both would pool them.
#'
#' @return `stats` restricted to the rows drawn, branch_id relabelled when needed
.branch_rows <- function(stats, track_type, keep_merged) {
  other <- !(stats$feature_type %in% track_type)
  untracked <- !other & is.na(stats$branch_id)
  merged <- !other & !is.na(stats$branch_id) & stats$branch_merged %in% TRUE
  drop <- other | untracked | (merged & !keep_merged)
  if (any(drop)) {
    message("  --group_by branch_id: left out ", sum(drop), " feature(s) -- ",
            sum(other), " of another type, ", sum(untracked), " in no branch, ",
            if (keep_merged) 0L else sum(merged), " on merged branches")
  }
  if (keep_merged && any(merged)) {
    message("  --group_by branch_id: ", sum(merged), " feature(s) on merged branches drawn ",
            "(--keep_merged_branches)")
  }
  out <- stats[!drop, , drop = FALSE]
  if (length(unique(out$series_id)) > 1L) {
    out$branch_id <- paste(out$series_id, out$branch_id)
    message("  --group_by branch_id: ", length(unique(out$series_id)), " series, so each branch ",
            "is labelled with its series_id (branch ids restart in every series)")
  }
  return(out)
}

#' The z step for one features file: `.cli_z_step_for()` (cli_helpers.r), with
#' this CLI's own warning when the series in it disagree -- a volume pooled
#' over two instruments' z steps is not in either one's units.
#'
#' @return a positive number, or NA_real_
.z_step_for <- function(feats, features_path, res_dirs) {
  z <- .cli_z_step_for(feats, features_path, res_dirs)
  clash <- attr(z, "conflict")
  if (!is.null(clash)) {
    warning("Series in ", basename(features_path), " record different ",
            "pixel_depth values (", paste(clash, collapse = ", "),
            "); no volume inferred. Pass --z_step to choose one.", call. = FALSE)
  }
  return(as.numeric(z))
}

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
  for (sid in unique(tab$series_id)) {
    rows <- tab[tab$series_id == sid, , drop = FALSE]
    for (ft in unique(rows$feature_type)) {
      prefix <- feature_roi_prefix(rows$roi[rows$feature_type == ft])
      if (is.na(prefix)) next
      cands <- file.path(dirs, paste0(sid, "_", prefix, "_res.txt"))
      hit <- cands[file.exists(cands)]
      if (!length(hit)) next
      one <- tryCatch(read_fiji_result(hit[1]), error = function(e) {
        warning("Could not read ", basename(hit[1]), ": ", conditionMessage(e),
                call. = FALSE)
        NULL
      })
      if (!is.null(one)) out[[paste(sid, ft)]] <- one
    }
  }
  if (!length(out)) return(NULL)
  return(out)
}

#' Join series table metadata onto the per-feature table
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
#' So a shared column is reconciled instead. Equal per series means it is the
#' same metadata and the join simply skips it. Unequal means the features were
#' annotated from a DIFFERENT sheet than the one being passed now, or a sheet
#' column is named after a statistic this step computes. Either way, silently
#' choosing one of the two would attach the wrong metadata to real numbers.
.cli_join_sheet <- function(stats, sheet, id_column) {
  .cli_check_reserved(sheet, id_column)
  meta <- sheet
  names(meta)[names(meta) == id_column] <- "series_id"

  shared <- setdiff(base::intersect(colnames(meta), colnames(stats)), "series_id")
  if (length(shared)) {
    # Metadata is constant within a series, so one row per series is the whole
    # comparison.
    have <- stats[!duplicated(stats$series_id), c("series_id", shared), drop = FALSE]
    cmp <- merge(have, meta[, c("series_id", shared), drop = FALSE],
                 by = "series_id", suffixes = c(".have", ".sheet"))
    disagree <- character(0)
    for (col in shared) {
      a <- as.character(cmp[[paste0(col, ".have")]])
      b <- as.character(cmp[[paste0(col, ".sheet")]])
      if (!isTRUE(all.equal(a, b))) disagree <- c(disagree, col)
    }
    if (length(disagree)) {
      stop("Series table column(s) disagree with what is already on the features: ",
           paste(disagree, collapse = ", "),
           "\n  The features were annotated from a different series table, or a ",
           "sheet column is named after a statistic this step computes.",
           "\n  Re-run annotate_features_cli.r with this sheet, or rename the ",
           "column (e.g. ", disagree[1], " -> sheet_", disagree[1], ").",
           call. = FALSE)
    }
    message("  series table: ", length(shared),
            " column(s) already on the features, not re-joined (",
            paste(utils::head(shared, 6), collapse = ", "),
            if (length(shared) > 6) ", ..." else "", ")")
    meta <- meta[, setdiff(colnames(meta), shared), drop = FALSE]
  }

  unmatched <- setdiff(stats$series_id, meta$series_id)
  if (length(unmatched)) {
    warning(length(unmatched), " series are not in the series table: ",
            paste(utils::head(unmatched, 5), collapse = ", "), call. = FALSE)
  }
  if (ncol(meta) <= 1L) {
    return(stats)
  }
  return(dplyr::left_join(stats, meta, by = "series_id"))
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
