#!/usr/bin/env Rscript

# feature_footprint_cli.r
#
# Annotated features -> one footprint per feature (the union of its ROIs over
# z), written as a vertex table Fiji can draw.
#
# analysis-oo_count-physical_blur (the oocyte count; not merged). The footprints
# are what Run_Overview_Batch.groovy draws as a named overlay on the overview
# TIFF -- the image hand-placed points are put on -- and confirm_features_cli.r
# tests those points against the same footprints, from the same function
# (scripts/R/feature_footprint.r). So the outline on screen is the outline the
# point is matched against.
#
# Every feature is written, counted and rejected alike; its status is its
# feature_id (`<feature>_N`, `invalid_*`, `failed_<feature>_<reason>`), and the
# overview batch decides which to draw.
#
# Output, in --outdir, one per input table:
#   <sample>_<feature>_footprint.txt   name, feature_id, part, ring, x, y
#                                      (calibrated units, y down, as _outline.txt)
# A sample with no rows of --feature still gets a header-only table, so a
# missing file always means "not run", never "nothing found".
#
#   ./feature_footprint_cli.r --features annotation/feature_contrast/ \
#       --outdir annotation/footprint

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

.ffp_source_helpers <- function(rlib = NA) {
  if (!exists(".cli_resolve_arg", mode = "function")) {
    if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
    sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
  }
  # Only feature_footprint.r: the rest of scripts/R/ would only add failure modes.
  if (!exists("feature_footprints", mode = "function")) {
    dir <- if (is.na(rlib)) file.path(.THIS_DIR, "..", "R") else rlib
    f <- file.path(dir, "feature_footprint.r")
    if (!file.exists(f)) stop("cannot locate feature_footprint.r; pass --rlib_path", call. = FALSE)
    sys.source(f, envir = globalenv())
  }
}

# ------------------------------------------------------------------------------

feature_footprint_cli <- function(args = commandArgs(trailingOnly = TRUE)) {

  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)

  .ffp_source_helpers()

  dflt_args <- list(feature = "nucleus")

  p <- arg_parser("Write one footprint per annotated feature (union over z) for Fiji to draw",
                  hide.opts = TRUE)
  p <- add_argument(p, "--features", short = "-i", type = "character", nargs = Inf, default = NULL,
                    help = "*_features.rds: files, globs or directories")
  p <- add_argument(p, "--outdir", short = "-o", type = "character",
                    help = "directory for the <sample>_<feature>_footprint.txt tables")
  p <- add_argument(p, "--feature", short = "-f", type = "character", default = dflt_args$feature,
                    help = "feature type to write [default: nucleus]")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character",
                    help = "path to scripts/R [default: alongside this script]")

  argv <- parse_args(p, argv = args)
  .cli_require(argv, c("features", "outdir"))
  .ffp_source_helpers(argv$rlib_path)

  .cli_need(c("sf"))
  feat_name <- argv$feature
  if (is.na(feat_name) || !nzchar(feat_name)) stop("--feature must name a feature type", call. = FALSE)

  feat_files <- .cli_resolve_input_path(argv$features, "_features\\.rds$", "--features")
  outdir <- normalizePath(argv$outdir, mustWork = FALSE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  summary_rows <- list()
  for (f in feat_files) {
    x <- readRDS(f)
    smp <- unique(x$sample)
    if (length(smp) != 1L) {
      stop(basename(f), " holds ", length(smp), " samples; expected one per file", call. = FALSE)
    }
    fp <- feature_footprints(x, feature_type = feat_name)
    v <- footprint_vertices(fp, name = smp)
    dest <- file.path(outdir, paste0(smp, "_", feat_name, "_footprint.txt"))
    utils::write.table(v, dest, sep = "\t", quote = FALSE, row.names = FALSE)

    status <- ifelse(startsWith(fp$feature_id, "invalid_"), "invalid",
              ifelse(startsWith(fp$feature_id, "failed_"), "failed", "counted"))
    n <- table(factor(status, levels = c("counted", "invalid", "failed")))
    message(sprintf("  %s: %d counted, %d invalid, %d failed-bucket footprints",
                    smp, n[["counted"]], n[["invalid"]], n[["failed"]]))
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      sample = smp, counted = n[["counted"]], invalid = n[["invalid"]], failed = n[["failed"]],
      stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, summary_rows)
  message(sprintf("%d table(s), %d counted %s footprints -> %s",
                  nrow(res), sum(res$counted), feat_name, outdir))
  return(invisible(res))
}

if (!interactive() && sys.nframe() == 0L) feature_footprint_cli()
