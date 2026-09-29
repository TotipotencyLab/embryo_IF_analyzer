# montage_grid.r
#
# Composing a grid of images that are already rasters.
#
# WHY magick RATHER THAN patchwork/cowplot
#   Those compose grobs. An image has to enter through rasterGrob/draw_image and
#   is then resampled by the graphics device to fit a panel -- a second resample
#   on top of the one Fiji already did when it resized the overview, and the
#   output pixels are no longer the input pixels. Resolution is specified
#   indirectly (inches x dpi), and holding a 100 MP panel as an R raster puts it
#   in R's heap. Here the cell size is arithmetic, so it is computed directly and
#   ImageMagick keeps the pixels outside R.
#
# THE ONE RULE THIS FILE EXISTS TO ENFORCE
#   A cell is always drawn, even when its image is missing. The caller counts
#   objects by eye, so a panel that silently vanished is a wrong answer that
#   looks like a right one: you count four sections, and never learn there were
#   five.

#' A "nice" number at or below x, from the 1/2/5 decade series
#'
#' @param x a positive number
#' @return the largest of 1, 2 or 5 times a power of ten that is <= x
mg_nice_number <- function(x) {
  if (!is.finite(x) || x <= 0) {
    return(NA_real_)
  }
  dec <- 10^floor(log10(x))
  for (m in c(5, 2, 1)) {
    if (m * dec <= x) {
      return(m * dec)
    }
  }
  return(dec / 2)
}

#' Physical extent of a sample, in micrometres
#'
#' The acquisition pixel size times the pixel dimensions. NOT the PNG's own
#' size: Fiji has usually already resized the overview, so the PNG's pixels and
#' the camera's pixels are different things.
#'
#' @param size_px pixel count along the axis
#' @param pixel_um physical size of one acquisition pixel
#' @return micrometres, or NA when either input is missing
mg_extent_um <- function(size_px, pixel_um) {
  px <- suppressWarnings(as.numeric(size_px))
  um <- suppressWarnings(as.numeric(pixel_um))
  out <- px * um
  out[!is.finite(out) | out <= 0] <- NA_real_
  return(out)
}

#' Micrometres per output pixel, so the largest panel fills a cell
#'
#' @param extents_um the longest physical side of each panel
#' @param cell_px the cell side, in output pixels
#' @return micrometres per output pixel
mg_um_per_px <- function(extents_um, cell_px) {
  ok <- extents_um[is.finite(extents_um)]
  if (!length(ok)) {
    stop("No panel has a usable physical size, so nothing can be scaled to it. ",
         "Check pixel_width in the sample sheet, or pass --scale pixel.",
         call. = FALSE)
  }
  return(max(ok) / cell_px)
}

#' One cell of the montage
#'
#' Reads, scales and pads a single image to exactly cell_w x cell_h, with its
#' label burned into the top-left. A missing or unreadable image becomes a
#' labelled placeholder of the same size rather than nothing.
#'
#' @param path image file, or NA
#' @param cell_w,cell_h the cell, in output pixels
#' @param label text for the corner; "" for none
#' @param draw_w,draw_h size to scale the image to, inside the cell. NULL fits it.
#' @param bg pad colour. Never use black: the overview's own background is
#'   black, so a black pad is indistinguishable from correctly-imaged empty
#'   field, which is the misreading this montage exists to prevent.
#' @return a magick image of exactly cell_w x cell_h
mg_cell <- function(path, cell_w, cell_h, label = "", draw_w = NULL, draw_h = NULL,
                    bg = "white", missing_color = "#b00020", label_color = "black") {
  cell_w <- as.integer(round(cell_w))
  cell_h <- as.integer(round(cell_h))
  font <- max(9L, as.integer(round(cell_h / 26)))

  im <- NULL
  if (!is.na(path) && nzchar(path) && file.exists(path)) {
    im <- tryCatch(magick::image_read(path), error = function(e) NULL)
  }

  if (is.null(im)) {
    cell <- magick::image_blank(cell_w, cell_h, color = bg)
    cell <- magick::image_annotate(cell, "no image", gravity = "center",
                                   size = font, color = missing_color)
  } else {
    if (!is.null(draw_w) && !is.null(draw_h)) {
      im <- magick::image_scale(im, paste0(as.integer(round(draw_w)), "x",
                                           as.integer(round(draw_h)), "!"))
    } else {
      im <- magick::image_scale(im, paste0(cell_w, "x", cell_h))
    }
    # Pad to the cell from the top-left, so panels of different physical size
    # share an origin and their sizes can be compared across the grid.
    cell <- magick::image_extent(im, paste0(cell_w, "x", cell_h),
                                 gravity = "northwest", color = bg)
  }

  if (nzchar(label)) {
    cell <- magick::image_annotate(cell, label, gravity = "northwest",
                                   location = "+4+4", size = font,
                                   color = label_color, boxcolor = "#ffffffcc")
  }
  return(cell)
}

#' Lay cells out in a grid
#'
#' Rows are appended left to right, then padded to the full grid width and
#' stacked -- padding each row explicitly rather than letting image_append
#' decide, so the fill colour is the one that was asked for.
#'
#' @param cells list of magick images, all the same size
#' @param ncol columns
#' @param bg pad colour
#' @return a magick image
mg_grid <- function(cells, ncol, bg = "white") {
  if (!length(cells)) {
    stop("A montage of no cells is not a montage.", call. = FALSE)
  }
  ncol <- max(1L, as.integer(ncol))
  info <- magick::image_info(cells[[1]])
  full_w <- info$width * ncol
  rows <- split(seq_along(cells), ceiling(seq_along(cells) / ncol))
  strips <- lapply(rows, function(ix) {
    strip <- magick::image_append(do.call(c, cells[ix]))
    magick::image_extent(strip, paste0(full_w, "x", magick::image_info(strip)$height),
                         gravity = "northwest", color = bg)
  })
  return(magick::image_append(do.call(c, strips), stack = TRUE))
}

#' Put a title band above a montage
#'
#' @param img a magick image
#' @param title text
#' @param bg band colour
#' @return a magick image, taller by the band
mg_title <- function(img, title, bg = "white", color = "black", height = NULL) {
  if (!nzchar(title)) {
    return(img)
  }
  info <- magick::image_info(img)
  band_h <- if (is.null(height)) max(24L, as.integer(round(info$height / 22))) else height
  font <- max(11L, as.integer(round(band_h * 0.6)))
  band <- magick::image_blank(info$width, band_h, color = bg)
  band <- magick::image_annotate(band, title, gravity = "west", location = "+8+0",
                                 size = font, color = color)
  return(magick::image_append(c(band, img), stack = TRUE))
}

#' Burn a scale bar into the bottom-left
#'
#' Not optional when the scale is per group: two montages drawn at different
#' micrometres-per-pixel look directly comparable and are not, and the bar is
#' what makes that visible.
#'
#' @param img a magick image
#' @param um_per_px micrometres per output pixel
#' @param frac target bar length as a fraction of image width
#' @return a magick image
mg_scale_bar <- function(img, um_per_px, frac = 0.18, color = "black",
                         bg = "#ffffffcc") {
  if (!is.finite(um_per_px) || um_per_px <= 0) {
    return(img)
  }
  info <- magick::image_info(img)
  target_um <- mg_nice_number(info$width * frac * um_per_px)
  if (!is.finite(target_um) || target_um <= 0) {
    return(img)
  }
  bar_px <- as.integer(round(target_um / um_per_px))
  if (bar_px < 4 || bar_px > info$width) {
    return(img)
  }
  thick <- max(3L, as.integer(round(info$height / 200)))
  font <- max(10L, as.integer(round(info$height / 45)))
  pad <- max(6L, as.integer(round(info$height / 100)))

  plate_w <- bar_px + 2L * pad
  plate_h <- thick + font + 3L * pad
  plate <- magick::image_blank(plate_w, plate_h, color = bg)
  bar <- magick::image_blank(bar_px, thick, color = color)
  plate <- magick::image_composite(plate, bar,
                                   offset = paste0("+", pad, "+", pad + font + pad))
  plate <- magick::image_annotate(plate, paste0(format(target_um, trim = TRUE), " um"),
                                  gravity = "north", location = paste0("+0+", pad),
                                  size = font, color = color)
  off_x <- pad
  off_y <- info$height - plate_h - pad
  if (off_y < 0) {
    return(img)
  }
  return(magick::image_composite(img, plate,
                                 offset = paste0("+", off_x, "+", off_y)))
}
