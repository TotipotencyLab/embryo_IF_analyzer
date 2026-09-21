#!/usr/bin/env Rscript

# montage_qc_cli.r
#
# Three-panel QC montage for one sample:
#   (i)   raw z-projection                 (PNG from Run_Overview, roiMode none)
#   (ii)  z-projection + Fiji outlines     (PNG from Run_Overview, roiMode merged)
#   (iii) outlines from R, unioned per feature
#
# Panels (i) and (ii) come from Fiji. Panel (iii) is drawn here and is the one
# that differs: Fiji's "merged" mode unions ROIs in the flattened 2D projection,
# so two objects overlapping in x-y share an outline no matter how far apart in
# z they are. R unions per feature_id, which was assigned z-aware. Seeing them
# side by side is the point of the montage.
#
#   ./montage_qc_cli.r --features out/S_features.rds \
#       --projection S_overview_ch1.png --overlay S_merged_ch1.png \
#       --config data/S_config.txt --output S_montage.png
#
# ALIGNMENT: panel (iii) is drawn over the full image frame only when the image
# dimensions are known. Those come from image_width/image_height in the Fiji
# _config.txt. Without them the panel is cropped to the features' bounding box
# and the three panels are NOT directly comparable -- this CLI says so loudly
# rather than producing a montage that silently misleads.

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
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    if (!is.null(f$ofile)) return(dirname(normalizePath(f$ofile, mustWork = FALSE)))
  }
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) return(dirname(normalizePath(sub("^--file=", "", a[1]), mustWork = FALSE)))
  NA_character_
})()

.montage_source_helpers <- function() {
  if (exists(".cli_multi", mode = "function")) return(invisible(NULL))
  if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
  sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
}

# ------------------------------------------------------------------------------

montage_qc_cli <- function(args = commandArgs(trailingOnly = TRUE)) {

  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)

  .montage_source_helpers()

  p <- arg_parser("Three-panel QC montage of nuclear detection", hide.opts = TRUE)
  p <- add_argument(p, "--features", short = "-F", type = "character",
                    help = "the *_features.rds written by annotate_features_cli.r")
  p <- add_argument(p, "--output", short = "-o", type = "character",
                    help = "montage PNG to write")
  p <- add_argument(p, "--projection", short = "-j", type = "character",
                    help = "PNG of the raw z-projection")
  p <- add_argument(p, "--overlay", short = "-l", type = "character",
                    help = "PNG of the z-projection with Fiji outlines")
  p <- add_argument(p, "--config", short = "-c", type = "character",
                    help = "Fiji _config.txt, for the image extent [default: found beside --features]")
  p <- add_argument(p, "--feature", short = "-f", type = "character", nargs = Inf, default = NULL,
                    help = "restrict panel (iii) to these feature type(s)")
  p <- add_argument(p, "--panel_height", short = "-H", type = "integer", default = 600,
                    help = "height in px each panel is scaled to")
  p <- add_argument(p, "--no_labels", short = "-N", flag = TRUE,
                    help = "omit the panel captions")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character",
                    help = "path to scripts/R [default: alongside this script]")

  argv <- parse_args(p, argv = args)
  .cli_require(argv, c("features", "output"))

  .cli_need(c("dplyr", "stringr", "sf", "ggplot2", "magick"))
  suppressPackageStartupMessages({
    library(dplyr); library(stringr); library(sf); library(ggplot2); library(magick)
  })
  .source_rlib(argv$rlib_path, .THIS_DIR)

  if (!file.exists(argv$features)) {
    stop("No such features file: ", argv$features, call. = FALSE)
  }
  feats <- readRDS(argv$features)
  if (!inherits(feats, "sf")) feats <- sf::st_as_sf(feats)

  keep_features <- .cli_multi(argv$feature, "--feature")
  if (length(keep_features)) {
    feats <- feats[feats$feature_type %in% keep_features, , drop = FALSE]
    if (!nrow(feats)) stop("No rows left after --feature filtering", call. = FALSE)
  }

  sample_name <- sub("_features\\.rds$", "", basename(argv$features))

  # --- image extent -----------------------------------------------------------
  cfg_path <- argv$config
  if (is.na(cfg_path)) cfg_path <- .find_config(argv$features, sample_name)
  extent <- .image_extent(cfg_path)

  # --- panels -----------------------------------------------------------------
  panels <- list()
  if (!is.na(argv$projection)) {
    panels[["raw z-projection"]] <- .read_panel(argv$projection, "--projection")
  }
  if (!is.na(argv$overlay)) {
    panels[["z-projection + Fiji outline"]] <- .read_panel(argv$overlay, "--overlay")
  }

  valid <- feats[grepl("^(nucleus|nucleolus|cell|cytoplasm)", feats$feature_id), ]
  if (!nrow(valid)) stop("No valid features to draw in panel (iii)", call. = FALSE)
  unioned <- union_features(valid)
  message("Panel (iii): ", nrow(feats), " ROIs -> ", nrow(unioned), " unioned feature(s)")

  r_png <- tempfile(fileext = ".png")
  .r_panel(feats, unioned, extent, sample_name, r_png, argv$panel_height)
  panels[[paste0("R union, z-aware (", nrow(unioned), ")")]] <- magick::image_read(r_png)

  if (length(panels) < 2) {
    warning("Only one panel available; a montage of one panel is just the panel.",
            call. = FALSE)
  }

  # --- compose ----------------------------------------------------------------
  h <- argv$panel_height
  imgs <- lapply(names(panels), function(nm) {
    im <- magick::image_scale(panels[[nm]], paste0("x", h))
    if (!argv$no_labels) {
      im <- magick::image_annotate(im, nm, size = max(12, round(h / 30)),
                                   gravity = "northwest", boxcolor = "white",
                                   color = "black", location = "+4+4")
    }
    im
  })
  montage <- magick::image_append(do.call(c, imgs))
  magick::image_write(montage, argv$output)

  info <- magick::image_info(montage)
  message("Wrote ", argv$output, " (", info$width, "x", info$height, ", ",
          length(panels), " panels)")

  invisible(argv$output)
}

# --- private helpers ----------------------------------------------------------

.read_panel <- function(path, what) {
  if (!file.exists(path)) stop("No such file for ", what, ": ", path, call. = FALSE)
  magick::image_read(path)
}

.find_config <- function(features_path, sample_name) {
  # The config is written by Fiji beside the outline tables, not beside the
  # .rds. Look in both places before giving up.
  cands <- c(
    file.path(dirname(features_path), paste0(sample_name, "_config.txt")),
    file.path(dirname(features_path), "..", paste0(sample_name, "_config.txt"))
  )
  hit <- cands[file.exists(cands)]
  if (length(hit)) normalizePath(hit[1]) else NA_character_
}

.image_extent <- function(cfg_path) {
  # Returns list(xmax, ymax) in calibrated units, or NULL when unknown.
  if (is.na(cfg_path) || !file.exists(cfg_path)) {
    warning("No Fiji _config.txt found: panel (iii) will be cropped to the ",
            "features' bounding box and will NOT align with the Fiji panels. ",
            "Pass --config to fix this.", call. = FALSE)
    return(NULL)
  }
  cfg <- utils::read.delim(cfg_path, stringsAsFactors = FALSE)
  if (!all(c("parameter", "value") %in% colnames(cfg))) {
    warning("Unrecognised config format: ", cfg_path, call. = FALSE)
    return(NULL)
  }
  get1 <- function(k) {
    v <- cfg$value[cfg$parameter == k]
    if (!length(v)) return(NA_real_)
    suppressWarnings(as.numeric(v[1]))
  }
  w <- get1("image_width");  h <- get1("image_height")
  pw <- get1("pixel_width"); ph <- get1("pixel_height")

  if (is.na(w) || is.na(h)) {
    warning("This _config.txt has no image_width/image_height ",
            "(written by Run_NucleusSelector only since the montage work). ",
            "Panel (iii) will be cropped to the features' bounding box and ",
            "will NOT align with the Fiji panels.", call. = FALSE)
    return(NULL)
  }
  if (is.na(pw) || is.na(ph)) { pw <- 1; ph <- 1 }
  list(xmax = w * pw, ymax = h * ph)
}

.r_panel <- function(feats, unioned, extent, sample_name, path, panel_height) {
  # y_ref decides what the flip pivots about. With a known frame, flip about the
  # frame so the panel matches the Fiji PNG; otherwise about the data's own
  # bounding box, which is the best available but is a different crop.
  # bare = TRUE: the drawn area is exactly the frame, so this panel lines up
  # with the Fiji PNGs beside it. Axes and a legend would each steal space and
  # shrink the picture relative to the other two panels. The montage caption
  # carries the labelling instead.
  if (!is.null(extent)) {
    p <- plot_features_topView(feats, unioned, color_by = "feature_type",
                               y_ref = extent$ymax,
                               xlim = c(0, extent$xmax), ylim = c(0, extent$ymax),
                               bare = TRUE)
    aspect <- extent$xmax / extent$ymax
  } else {
    p <- plot_features_topView(feats, unioned, color_by = "feature_type", bare = TRUE)
    bb <- sf::st_bbox(feats)
    aspect <- unname((bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"]))
  }

  h_in <- max(3, panel_height / 100)
  ggplot2::ggsave(path, p, width = h_in * max(0.6, aspect), height = h_in, dpi = 150)
  invisible(path)
}

if (!interactive() && sys.nframe() == 0L) montage_qc_cli()
