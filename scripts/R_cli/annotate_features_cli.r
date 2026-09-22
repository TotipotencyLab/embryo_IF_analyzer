#!/usr/bin/env Rscript

# annotate_features_cli.r
#
# Fiji ROI outlines -> cellular features.
#
# Takes the *_<feature>_outline.txt tables written by the Groovy pipeline,
# groups the per-slice ROIs of each object across z, and writes one annotated
# sf object per sample plus a tidy per-ROI table. Optionally draws the detected
# features as a QC plot.
#
# The grouping is z-aware: two objects that overlap in the flattened projection
# but sit far apart in z stay separate. Fiji's "merged" overview unions in 2D
# and will therefore show fewer outlines. That is expected -- the overview is a
# picture, the annotation is the count.
#
#   ./annotate_features_cli.r --input data/ --feature nucleus nucleolus \
#       --outdir out --max_z_dist 'default=3' 'nucleolus=1' \
#       --min_z_span 'default=5' 'nucleolus=2' --qc_plot
#
# See .claude/skills/r-cli-convention for the conventions this follows.

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

.annotate_source_helpers <- function() {
  if (exists(".cli_resolve_arg", mode = "function")) return(invisible(NULL))
  if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
  sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
}

# ------------------------------------------------------------------------------

annotate_features_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  
  # Warnings are this stage's feedback channel: an ROI dropped for too few
  # vertices, a feature that grouped to nothing. Deferred, R hides the text past
  # ten of them, so the worst run says the least.
  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)
  
  .annotate_source_helpers()
  
  p <- arg_parser("Annotate Fiji ROI outlines into cellular features", hide.opts = TRUE)
  p <- add_argument(p, "--input", short = "-i", type = "character", nargs = Inf, default = NULL,
                    help = "outline files, globs, or directories to scan")
  p <- add_argument(p, "--outdir", short = "-o", type = "character",
                    help = "directory to write results into")
  p <- add_argument(p, "--feature", short = "-f", type = "character", nargs = Inf, default = NULL,
                    help = "feature name(s) to annotate [default: nucleus]")
  p <- add_argument(p, "--sample_sheet", short = "-S", type = "character",
                    help = "optional table with a 'prefix' column; filters inputs and joins metadata")
  p <- add_argument(p, "--id_column", short = "-I", type = "character", default = "prefix",
                    help = "sample sheet column holding the file prefix")
  p <- add_argument(p, "--output_prefix", short = "-X", type = "character", default = "",
                    help = "prefix for the combined output files")
  # Per-feature grouping parameters, as space-separated key=value tokens.
  p <- add_argument(p, "--max_z_dist", short = "-d", type = "character", nargs = Inf, default = NULL,
                    help = "max z gap linking two ROIs, e.g. 'default=3' 'nucleolus=1'")
  p <- add_argument(p, "--min_z_span", short = "-z", type = "character", nargs = Inf, default = NULL,
                    help = "min distinct slices for a real feature, e.g. 'default=5'")
  p <- add_argument(p, "--min_circularity", short = "-c", type = "character", nargs = Inf, default = NULL,
                    help = "drop ROIs below this circularity; needs the _res.txt file")
  p <- add_argument(p, "--min_intersect_ratio", short = "-r", type = "character", nargs = Inf, default = NULL,
                    help = "min overlap/min-area ratio to link two ROIs [default: 0]")
  p <- add_argument(p, "--within", short = "-w", type = "character", nargs = Inf, default = NULL,
                    help = "containment, as 'child=parent', e.g. 'nucleolus=nucleus'")
  p <- add_argument(p, "--min_containment", short = "-m", type = "character", nargs = Inf, default = NULL,
                    help = "fraction of a child that must lie inside its parent [default: 0.5]")
  p <- add_argument(p, "--require_parent", short = "-E", flag = TRUE,
                    help = "drop inner features that could not be placed in a parent")
  p <- add_argument(p, "--input_pattern", short = "-P", type = "character",
                    help = "regex the input basenames must match [default: the Fiji output contract]")
  p <- add_argument(p, "--qc_plot", short = "-q", flag = TRUE,
                    help = "also write a QC plot of the detected features")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character",
                    help = "path to scripts/R [default: alongside this script]")
  
  argv <- parse_args(p, argv = args)
  .cli_require(argv, c("input", "outdir"))
  
  .cli_need(c("dplyr", "tibble", "stringr", "tidyr", "sp", "sf"))
  suppressPackageStartupMessages({
    library(dplyr); library(tibble); library(stringr); library(tidyr)
    library(sp); library(sf)
  })
  .source_rlib(argv$rlib_path, .THIS_DIR)
  
  features <- .cli_resolve_arg(argv$feature, "--feature")
  if (!length(features)) features <- "nucleus"
  
  # These set of args accept different settings for different features e.g., --max_z_dist 'nucleus=3' 'nucleous=2'
  # The actual setting used at the run-time is resolved by .cli_param_for function
  max_z_dist    <- .cli_key_values(argv$max_z_dist,          "--max_z_dist")
  min_z_span    <- .cli_key_values(argv$min_z_span,          "--min_z_span")
  min_circ      <- .cli_key_values(argv$min_circularity,     "--min_circularity")
  min_int_ratio <- .cli_key_values(argv$min_intersect_ratio, "--min_intersect_ratio")
  min_contain   <- .cli_key_values(argv$min_containment,     "--min_containment")

  # 'child=parent'. The biology is the caller's to declare -- nothing here knows
  # that a nucleolus belongs in a nucleus.
  within <- .cli_key_values(argv$within, "--within", default_key = NA_character_)
  if(length(within) > 0 && any(is.na(names(within)))){
    stop("--within needs 'child=parent' tokens, e.g. 'nucleolus=nucleus'", call. = FALSE)
  }
  
  # Default value that .cli_param_for look up against
  dflt_args = list(
    min_circularity     = NA_real_,
    max_z_dist          = 3,
    min_z_span          = 5,
    min_intersect_ratio = 0,
    min_containment     = 0.5
  )
  
  # --- resolve inputs ---------------------------------------------------------
  # The Fiji output contract is the default, not the only option: --input_pattern
  # takes over for data that arrives named differently. The contract still has to
  # let .cli_parse_contract() recover <sample> and <feature> from the basename.
  pattern <- if(is.na(argv$input_pattern)){
    paste0("_(", paste(features, collapse = "|"), ")_outline\\.txt$")
  }else{
    argv$input_pattern
  }
  files <- .cli_resolve_input_path(argv$input, pattern)
  jobs <- .cli_parse_contract(files, features)
  
  if (!is.na(argv$sample_sheet)) {
    sheet <- .cli_read_sample_sheet(path = argv$sample_sheet, id_column = argv$id_column)
    jobs <- .cli_apply_sample_sheet(contract_df = jobs, sheet = sheet, id_column = argv$id_column)
  } else {
    sheet <- NULL
  }
  
  samples <- sort(unique(jobs$sample))
  message("Annotating ", nrow(jobs), " file(s) across ", length(samples),
          " sample(s); feature(s): ", paste(features, collapse = ", "))
  
  outdir <- argv$outdir
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(outdir)) stop("Could not create --outdir: ", outdir, call. = FALSE)
  
  # NB: attach, not just check. plot_features_topView() lives in scripts/R/ and
  #     calls ggplot()/geom_sf() unqualified.
  if (argv$qc_plot) {
    .cli_need("ggplot2")
    suppressPackageStartupMessages(library(ggplot2))
  }
  
  # --- annotate ---------------------------------------------------------------
  all_rows <- list()
  for (smp in samples) {
    smp_jobs <- jobs[jobs$sample == smp, , drop = FALSE]
    message("  ", smp)
    
    per_feature <- list()
    for (k in seq_len(nrow(smp_jobs))) {
      feat <- smp_jobs$feature[k]
      path <- smp_jobs$path[k]
      
      roi_df <- .read_outline(path)
      if (!nrow(roi_df)) {
        warning("Outline table is empty, skipping: ", path, call. = FALSE)
        next
      }
      
      # Optional circularity pre-filter, which needs the measurement table.
      circ_cut <- if (feat %in% names(min_circ) || "default" %in% names(min_circ)) {
        .cli_param_for(lookup=min_circ, feature=feat, default=dflt_args$min_circularity, what="--min_circularity")
      } else NA_real_
      if (!is.na(circ_cut)) {
        roi_df <- .apply_circularity(roi_df, path, circ_cut)
      }
      
      # The defaults are tuned for nuclei. A nucleolus spans far fewer slices,
      # so running it at min_z_span = 5 quietly returns a fraction of the real
      # count -- which is why the effective values are printed per feature
      # rather than left implicit.
      eff_z_dist <- .cli_param_for(max_z_dist,    feat, dflt_args$max_z_dist,          "--max_z_dist")
      eff_z_span <- .cli_param_for(min_z_span,    feat, dflt_args$min_z_span,          "--min_z_span")
      eff_ratio  <- .cli_param_for(min_int_ratio, feat, dflt_args$min_intersect_ratio, "--min_intersect_ratio")
      
      feature_group <- define_feature_group(
        roi_df,
        pre_roi_filter_colname = "include",
        roi_regex           = paste0("^", feat),
        max_z_dist          = eff_z_dist,
        min_z_span          = eff_z_span,
        min_intersect_ratio = eff_ratio,
        feature_prefix         = paste0(feat, "_"),
        invalid_feature_prefix = paste0("invalid_", feat, "_"),
        fail_ROI_feature_prefix = paste0("failed_", feat, "_")
      )
      feature_group$feature_type <- feat
      feature_group$sample <- smp
      per_feature[[feat]] <- feature_group
      
      n_valid <- length(unique(feature_group$feature_id[grepl(paste0("^", feat, "_"), feature_group$feature_id)]))
      message("    ", feat, ": ", nrow(feature_group), " ROIs -> ", n_valid, " feature(s)",
              "  [max_z_dist=", eff_z_dist, " min_z_span=", eff_z_span,
              " min_intersect_ratio=", eff_ratio, "]")
    } # end for k
    
    if (!length(per_feature)) {
      warning("No usable feature table for sample ", smp, call. = FALSE)
      next
    }
    
    # Collapse all feature into one sample_df
    sample_df <- dplyr::bind_rows(per_feature)
    if (!is.null(sheet)) {
      meta <- sheet[sheet[[argv$id_column]] == smp, , drop = FALSE]
      meta[[argv$id_column]] <- NULL
      if (ncol(meta)) sample_df <- dplyr::bind_cols(sample_df, meta[rep(1, nrow(sample_df)), , drop = FALSE])
    }
    sample_sf <- sf::st_as_sf(sample_df)
    
    # --- containment ----------------------------------------------------------
    # Relating features must never rewrite them: a gap-filled match relaxes the
    # LOOKUP when the parent was not detected on a slice, and does not touch the
    # parent's geometry, area or z-span.
    if(length(within) > 0){
      containment_spec <- if(length(min_contain) > 0){
        min_contain
      }else{
        dflt_args$min_containment
      }
      sample_sf <- assign_feature_parent(
        st_df           = sample_sf,
        within          = within,
        min_containment = containment_spec
      )
      .report_parents(sample_sf, within)
      
      if(argv$require_parent){
        sample_sf <- .drop_orphans(sample_sf, within)
      }
    }
    
    rds_path <- file.path(outdir, paste0(smp, "_features.rds"))
    saveRDS(sample_sf, rds_path)
    message("    -> ", basename(rds_path))
    
    if (argv$qc_plot) {
      png_path <- file.path(outdir, paste0(smp, "_features_qc.png"))
      .qc_plot(sample_sf, smp, png_path)
      message("    -> ", basename(png_path))
    }
    
    all_rows[[smp]] <- sf::st_drop_geometry(sample_sf)
  }
  
  if (!length(all_rows)) stop("Nothing was annotated.", call. = FALSE)
  
  combined <- dplyr::bind_rows(all_rows)
  tsv_path <- file.path(outdir, paste0(argv$output_prefix, "features.tsv"))
  utils::write.table(combined, tsv_path, sep = "\t", quote = FALSE, row.names = FALSE)
  message("Wrote ", nrow(combined), " ROI rows -> ", tsv_path)
  
  return(invisible(combined))
}

# --- private helpers ----------------------------------------------------------

.read_outline <- function(path) {
  df <- utils::read.table(path, header = TRUE, stringsAsFactors = FALSE, sep="\t")
  needed <- c("roi", "z", "x", "y")
  absent <- setdiff(needed, colnames(df))
  if (length(absent)) {
    stop("Outline table ", basename(path), " is missing column(s): ",
         paste(absent, collapse = ", "),
         "\n  (expected the output contract: name, roi, z, x, y)", call. = FALSE)
  }
  # @Chad: You have make sure that all of `need` columns exist anyway, so no need to overcomplicated thing
  return(tibble::as_tibble(df[, needed, drop = FALSE]))
}

#' Report how many inner features found a parent, and how
#'
#' The gap-filled count is the number worth watching: if many children needed
#' their parent's missing slices filled in, the PARENT detection is what needs
#' work, and that should be visible rather than buried in a clean-looking run.
.report_parents <- function(sample_sf, within) {
  tab <- sf::st_drop_geometry(sample_sf)
  for(child_type in names(within)) {
    rows <- tab[tab$feature_type == child_type &
                  !is.na(tab$feature_id) &
                  startsWith(tab$feature_id, paste0(child_type, "_")), ]
    if(nrow(rows) == 0){
      next
    }
    per <- unique(rows[, c("feature_id", "parent_feature_id", "parent_match")])
    n_total  <- nrow(per)
    n_placed <- sum(!is.na(per$parent_feature_id))
    n_gap    <- sum(!is.na(per$parent_match) & per$parent_match == "gap_filled")
    message("    ", child_type, " in ", unname(within[[child_type]]), ": ",
            n_placed, "/", n_total, " placed",
            if(n_gap > 0){ paste0(" (", n_gap, " gap-filled)") }else{ "" },
            if(n_placed < n_total){ paste0(", ", n_total - n_placed, " orphaned") }else{ "" })
  }
  return(invisible(NULL))
}


#' Drop inner features that could not be placed inside a parent
#'
#' Off by default. An orphaned nucleolus is evidence about NUCLEUS detection
#' quality, so discarding it destroys the evidence -- only do it on request.
.drop_orphans <- function(sample_sf, within) {
  drop <- rep(FALSE, nrow(sample_sf))
  for(child_type in names(within)) {
    is_child <- sample_sf$feature_type == child_type &
      !is.na(sample_sf$feature_id) &
      startsWith(sample_sf$feature_id, paste0(child_type, "_"))
    drop <- drop | (is_child & is.na(sample_sf$parent_feature_id))
  }
  if(any(drop)){
    message("    --require_parent dropped ", length(unique(sample_sf$feature_id[drop])),
            " orphaned feature(s) (", sum(drop), " ROIs)")
  }
  return(sample_sf[!drop, ])
}


.apply_circularity <- function(roi_df, outline_path, cutoff) {
  # The circularity lives in the measurement table, which sits beside the
  # outline table under the output contract.
  res_path <- sub("_outline\\.txt$", "_res.txt", outline_path)
  if (!file.exists(res_path)) {
    warning("--min_circularity given but no measurement table beside ",
            basename(outline_path), "; skipping the filter", call. = FALSE)
    return(roi_df)
  }
  res <- read_fiji_result(res_path)
  if (!all(c("roi", "circ") %in% colnames(res))) {
    warning("Measurement table ", basename(res_path),
            " has no usable roi/circ column; skipping the circularity filter",
            call. = FALSE)
    return(roi_df)
  }
  keep <- unique(res$roi[!is.na(res$circ) & res$circ >= cutoff])
  roi_df$include <- roi_df$roi %in% keep
  n_drop <- length(setdiff(unique(roi_df$roi), keep))
  if (n_drop) message("    circularity >= ", cutoff, " drops ", n_drop, " ROI(s)")
  roi_df
}

.qc_plot <- function(sample_sf, sample_name, path) {
  # Per-slice ROIs faintly underneath, the z-aware union on top in colour.
  # plot_features_topView() is the library function; it uses geom_sf (a union
  # can be a MULTIPOLYGON or carry holes) and puts y in image orientation.
  valid <- sample_sf[grepl("^(nucleus|nucleolus|cell|cytoplasm)", sample_sf$feature_id), ]
  if (!nrow(valid)) {
    warning("No valid features to plot for ", sample_name, call. = FALSE)
    return(invisible(NULL))
  }
  unioned <- union_features(valid)
  
  p <- plot_features_topView(sample_sf, unioned, color_by = "feature_type") +
    ggplot2::labs(
      title = sample_name,
      subtitle = paste0(nrow(sample_sf), " ROIs -> ", nrow(unioned), " features"))
  
  ggplot2::ggsave(path, p, width = 6, height = 6, dpi = 150)
  invisible(path)
}

if (!interactive() && sys.nframe() == 0L) annotate_features_cli()
