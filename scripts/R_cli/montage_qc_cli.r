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

.montage_source_helpers <- function() {
  if (exists(".cli_resolve_arg", mode = "function")) return(invisible(NULL))
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
  p <- add_argument(p, "--feature_table", short = "-T", type = "character",
                    help = paste("optional feature_stats.tsv, joined on sample+feature_id,",
                                 "so --feature_class_by can name a column from it (e.g. class)"))
  p <- add_argument(p, "--feature_class_by", short = "-B", type = "character", nargs = Inf, default = NULL,
                    help = paste("column(s) whose values are joined to label each outline",
                                 "[default: feature_type]. Space-separated on ONE flag:",
                                 "--feature_class_by class feature_type. COLUMN NAMES,",
                                 "not values"))
  p <- add_argument(p, "--class_sep", short = "-s", type = "character", default = "-",
                    help = "separator joining --feature_class_by values [default: -]")
  p <- add_argument(p, "--color_map", short = "-m", type = "character", nargs = Inf, default = NULL,
                    help = paste("'<class>=<colour>' for the classes to highlight,",
                                 "space-separated on ONE flag:",
                                 "--color_map 'growing=red' 'small=blue'. Everything else is",
                                 "drawn grey as one 'other' group, named in the caption.",
                                 "'other' and 'unclassified' are settable here too.",
                                 "Automatic colours when omitted"))
  p <- add_argument(p, "--force", short = "-U", flag = TRUE,
                    help = "join --feature_table even when its run_id disagrees")
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

  keep_features <- .cli_resolve_arg(argv$feature, "--feature")
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

  valid <- .cli_valid_rows(feats)

  # "Nothing was valid" is a RESULT, and this panel exists to show results. It
  # used to stop here, which refused to draw the picture in the one case it is
  # most wanted: a field of tissue with no oocyte in it looks exactly like a
  # field where detection silently failed, and only the rejected outlines tell
  # the two apart. Real slides are mostly such fields -- the test data always
  # had an oocyte in it, because it was chosen for having one.
  drew_rejected <- FALSE
  if (!nrow(valid)) {
    if (nrow(feats)) {
      message("Panel (iii): no VALID feature. Drawing the ", nrow(feats),
              " rejected ROI(s) instead -- segmentation found something and ",
              "grouping refused it.")
      valid <- feats
      # union_features groups on feature_id; rows that never got one would
      # otherwise collapse into a single blob.
      if ("feature_id" %in% colnames(valid)) {
        valid$feature_id[is.na(valid$feature_id)] <- "unassigned"
      }
      drew_rejected <- TRUE
    } else {
      # Reachable when every ROI was filtered out before grouping (--roi_area,
      # --min_circularity). NOT reachable from a detection that found nothing:
      # NucleusPipeline.groovy returns before writing any table in that case, so
      # there is no outline file and no features .rds to be here at all.
      message("Panel (iii): no features at all. Drawing the empty frame.")
    }
  }

  # --- how each outline is labelled -------------------------------------------
  class_by <- .cli_resolve_arg(argv$feature_class_by, "--feature_class_by")
  if (is.null(class_by) || !length(class_by)) class_by <- "feature_type"

  if (!is.na(argv$feature_table)) {
    if (!file.exists(argv$feature_table)) {
      stop("--feature_table not found: ", argv$feature_table, call. = FALSE)
    }
    ftab <- utils::read.delim(argv$feature_table, stringsAsFactors = FALSE)
    if (!"sample" %in% colnames(valid)) valid$sample <- sample_name
    geom <- sf::st_geometry(valid)
    tab <- sf::st_drop_geometry(valid)
    tab <- join_feature_table(tab, ftab, force = argv$force)
    valid <- sf::st_set_geometry(tab, geom)
  }

  absent_cb <- setdiff(class_by, colnames(valid))
  if (length(absent_cb)) {
    stop("--feature_class_by column(s) not found: ", paste(absent_cb, collapse = ", "),
         "\n  available: ", paste(colnames(sf::st_drop_geometry(valid)), collapse = ", "),
         if (is.na(argv$feature_table)) "\n  (a column from the stats table needs --feature_table)" else "",
         call. = FALSE)
  }
  valid$feature_class <- compose_feature_class(sf::st_drop_geometry(valid),
                                               class_by, argv$class_sep)
  # A rejected nucleus drawn as "nucleus" would be read as an accepted one --
  # and they are not all rejected for the same reason. define_feature_group()
  # writes WHY into the id itself: `invalid_nucleus_3` was grouped and then
  # failed a rule, `failed_ROI_nucleus_3` never got that far, and a row with no
  # id at all was never grouped. Taking the marker off the id keeps the three
  # apart without this file having to know the list of reasons.
  if (drew_rejected) {
    why <- rep("unassigned", nrow(valid))
    if ("feature_id" %in% colnames(valid)) {
      for (ft in unique(valid$feature_type)) {
        i <- which(valid$feature_type == ft & !is.na(valid$feature_id) &
                     valid$feature_id != "unassigned")
        if (length(i)) {
          why[i] <- sub(paste0("_?", ft, "_[0-9]+$"), "",
                        as.character(valid$feature_id[i]))
        }
      }
    }
    why[!nzchar(why)] <- "grouped"
    valid$feature_class <- paste(why, valid$feature_class)
  }

  cmap <- .cli_key_values(argv$color_map, "--color_map")
  cmap <- if (length(cmap)) unlist(cmap) else NULL
  if (!is.null(cmap)) {
    bad <- cmap[!(cmap %in% grDevices::colors() | grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", cmap))]
    if (length(bad)) {
      stop("--color_map has unusable colour(s): ", paste(bad, collapse = ", "),
           "\n  use an R colour name (see colors()) or #RRGGBB", call. = FALSE)
    }
    # The reserved names are legitimate targets even though "other" never
    # appears in the data -- it is the group the unmapped classes fold into.
    unknown <- setdiff(names(cmap), c(unique(valid$feature_class), CLASS_RESERVED))
    if (length(unknown)) {
      warning("--color_map names class(es) not present: ", paste(unknown, collapse = ", "),
              "\n  present: ", paste(sort(unique(valid$feature_class)), collapse = ", "),
              call. = FALSE)
    }
  }

  # union AFTER the class is assigned, and carry it through the grouping or the
  # summarise drops it.
  unioned <- union_features(valid,
                            group_cols = c("feature_id", "feature_type", "feature_class"))
  message("Panel (iii): ", nrow(feats), " ROIs -> ", nrow(unioned), " unioned feature(s)")

  pal <- class_palette(unioned$feature_class, cmap)
  unioned$feature_class <- pal$values

  r_png <- tempfile(fileext = ".png")
  .r_panel(feats, unioned, extent, sample_name, r_png, argv$panel_height,
           palette = pal$palette)

  # The panel is deliberately legend-free so it lines up with the Fiji PNGs
  # beside it, so the key goes in the caption instead. Naming what is inside
  # "other" is the point: a grey blob nobody can identify is how a QC panel
  # quietly stops being a QC panel.
  cap <- paste0(if (drew_rejected) "R union, z-aware, ALL REJECTED (" else
                "R union, z-aware (", nrow(unioned), ")")
  if (!is.null(pal$palette)) {
    named <- setdiff(names(pal$palette), "other")
    cap <- paste0(cap, " | ",
                  paste(sprintf("%s=%s", named, pal$palette[named]), collapse = " "))
    if (length(pal$other_members)) {
      cap <- paste0(cap, " | other(", length(pal$other_members), "): ",
                    paste(pal$other_members, collapse = ", "))
    }
  }
  panels[[cap]] <- magick::image_read(r_png)

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

  return(invisible(argv$output))
}

# --- private helpers ----------------------------------------------------------

.read_panel <- function(path, what) {
  if (!file.exists(path)) stop("No such file for ", what, ": ", path, call. = FALSE)
  return(magick::image_read(path))
}

.find_config <- function(features_path, sample_name) {
  # The config is written by Fiji beside the outline tables, not beside the
  # .rds. Look in both places before giving up.
  cands <- c(
    file.path(dirname(features_path), paste0(sample_name, "_config.txt")),
    file.path(dirname(features_path), "..", paste0(sample_name, "_config.txt"))
  )
  hit <- cands[file.exists(cands)]
  if (length(hit)) 
    return(normalizePath(hit[1]) )
  else 
    return(NA_character_)
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
  if (is.na(pw) || is.na(ph)) {
    # Falling back to 1 makes the extent a PIXEL count while the outlines are in
    # calibrated units, so the features would be drawn into one corner of a
    # frame many times too large -- misaligned, and silently so, which is the
    # one thing this CLI exists to avoid.
    warning("This _config.txt has image_width/image_height but no ",
            "pixel_width/pixel_height, so the frame cannot be converted to ",
            "calibrated units. Panel (iii) will be cropped to the features' ",
            "bounding box and will NOT align with the Fiji panels.",
            call. = FALSE)
    return(NULL)
  }
  return(list(xmax = w * pw, ymax = h * ph))
}

.r_panel <- function(feats, unioned, extent, sample_name, path, panel_height,
                     palette = NULL) {
  # Nothing to draw is still something to show, but only at a known scale: an
  # empty panel whose extent came from the data would be an empty panel of
  # unknowable size, sitting beside two Fiji PNGs it does not match.
  nothing <- (is.null(feats) || !nrow(feats)) && (is.null(unioned) || !nrow(unioned))
  if (nothing) {
    if (is.null(extent)) {
      stop("Panel (iii) has no feature to draw and no _config.txt to give the ",
           "image frame, so an empty panel would be of unknown size. ",
           "Pass --config.", call. = FALSE)
    }
    p <- ggplot2::ggplot() +
      ggplot2::coord_sf(xlim = c(0, extent$xmax), ylim = c(0, extent$ymax),
                        expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(0, 0, 0, 0),
                     panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = NA))
    h_in <- max(3, panel_height / 100)
    aspect <- extent$xmax / extent$ymax
    ggplot2::ggsave(path, p, width = h_in * max(0.6, aspect), height = h_in, dpi = 150)
    return(invisible(path))
  }

  # y_ref decides what the flip pivots about. With a known frame, flip about the
  # frame so the panel matches the Fiji PNG; otherwise about the data's own
  # bounding box, which is the best available but is a different crop.
  # bare = TRUE: the drawn area is exactly the frame, so this panel lines up
  # with the Fiji PNGs beside it. Axes and a legend would each steal space and
  # shrink the picture relative to the other two panels. The montage caption
  # carries the labelling instead.
  if (!is.null(extent)) {
    p <- plot_features_topView(feats, unioned, color_by = "feature_class",
                               y_ref = extent$ymax,
                               xlim = c(0, extent$xmax), ylim = c(0, extent$ymax),
                               bare = TRUE, palette = palette)
    aspect <- extent$xmax / extent$ymax
  } else {
    p <- plot_features_topView(feats, unioned, color_by = "feature_class",
                               bare = TRUE, palette = palette)
    bb <- sf::st_bbox(feats)
    aspect <- unname((bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"]))
  }

  h_in <- max(3, panel_height / 100)
  ggplot2::ggsave(path, p, width = h_in * max(0.6, aspect), height = h_in, dpi = 150)
  return(invisible(path))
}

if (!interactive() && sys.nframe() == 0L) montage_qc_cli()
