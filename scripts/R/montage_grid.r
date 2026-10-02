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
         "Check pixel_width in the series table, or pass --scale pixel.",
         call. = FALSE)
  }
  return(max(ok) / cell_px)
}

#' How far two aspect ratios may differ before they are worth reporting
#'
#' Fiji resizes an overview on the way out, so its PNG is not the acquisition's
#' pixel dimensions -- but the resize follows the aspect, so the SHAPE should
#' still match the sheet. On an 11344 x 9590 series resized to 1000 wide the
#' rounding moves the aspect by 0.07%, so a couple of percent is slack, and a
#' file that is genuinely a different picture misses by tens of percent.
MG_ASPECT_TOL <- 0.02

#' Scale an image to fit INSIDE a box, keeping its aspect
#'
#' ⚠️ Deliberately not magick's "WxH!", which forces the exact dimensions and
#' therefore distorts anything not already that shape. The box here comes from
#' the series table, and an image whose aspect disagrees with the sheet used to
#' be silently stretched to match it -- a distorted follicle still looks like a
#' follicle, so nothing downstream or upstream would have said so. Fitting
#' letterboxes instead, and when the aspects DO agree the two are identical.
#'
#' @param img a magick image, or NULL
#' @param box_w,box_h the box, in output pixels
#' @return a magick image no larger than the box, or NULL
mg_fit <- function(img, box_w, box_h) {
  if (is.null(img)) {
    return(NULL)
  }
  return(magick::image_scale(img, paste0(as.integer(round(box_w)), "x",
                                         as.integer(round(box_h)))))
}

#' Does an image have the shape the sheet says it should?
#'
#' @param img a magick image
#' @param expect_aspect width/height the sheet implies, or NA to skip
#' @return NA when it matches or cannot be checked; otherwise the observed
#'   aspect, so the caller can report by how much
mg_aspect_off <- function(img, expect_aspect, tol = MG_ASPECT_TOL) {
  if (is.null(img) || !is.finite(expect_aspect) || expect_aspect <= 0) {
    return(NA_real_)
  }
  info <- magick::image_info(img)
  if (!is.finite(info$height) || info$height <= 0) {
    return(NA_real_)
  }
  got <- info$width / info$height
  if (abs(got / expect_aspect - 1) <= tol) {
    return(NA_real_)
  }
  return(got)
}

#' Pad a fitted image into a cell, and label it
#'
#' A NULL image becomes a labelled placeholder of the same size, never nothing.
#' The montage exists to be counted from, and a panel that silently vanished is
#' a wrong answer that looks like a right one.
#'
#' @param img a magick image already scaled to its drawn size, or NULL
#' @param cell_w,cell_h the cell, in output pixels
#' @param label text for the corner; "" for none
#' @param bg pad colour. Never black: the overview's own background is black, so
#'   a black pad cannot be told from correctly-imaged empty field.
#' @return a magick image of exactly cell_w x cell_h
mg_pad <- function(img, cell_w, cell_h, label = "", bg = "white",
                   missing_color = "#b00020", label_color = "black") {
  cell_w <- as.integer(round(cell_w))
  cell_h <- as.integer(round(cell_h))
  font <- max(9L, as.integer(round(cell_h / 26)))

  if (is.null(img)) {
    cell <- magick::image_blank(cell_w, cell_h, color = bg)
    cell <- magick::image_annotate(cell, "no image", gravity = "center",
                                   size = font, color = missing_color)
  } else {
    # From the top-left, so panels of different size share an origin and their
    # sizes can be compared across the grid.
    cell <- magick::image_extent(img, paste0(cell_w, "x", cell_h),
                                 gravity = "northwest", color = bg)
  }

  if (nzchar(label)) {
    cell <- magick::image_annotate(cell, label, gravity = "northwest",
                                   location = "+4+4", size = font,
                                   color = label_color, boxcolor = "#ffffffcc")
  }
  return(cell)
}

#' One cell, read from a file: fit then pad
#'
#' The convenience form. A caller that needs the fitted size BEFORE choosing the
#' cell -- which is anything sizing its cells to the images it actually has --
#' calls mg_fit() and mg_pad() separately instead.
#'
#' @param path image file, or NA
#' @inheritParams mg_pad
#' @param draw_w,draw_h size to fit the image into; NULL means the whole cell
#' @return a magick image of exactly cell_w x cell_h
mg_cell <- function(path, cell_w, cell_h, label = "", draw_w = NULL, draw_h = NULL,
                    bg = "white", missing_color = "#b00020", label_color = "black") {
  im <- NULL
  if (!is.na(path) && nzchar(path) && file.exists(path)) {
    im <- tryCatch(magick::image_read(path), error = function(e) NULL)
  }
  im <- if (is.null(draw_w) || is.null(draw_h)) {
    mg_fit(im, cell_w, cell_h)
  } else {
    mg_fit(im, draw_w, draw_h)
  }
  return(mg_pad(im, cell_w, cell_h, label = label, bg = bg,
                missing_color = missing_color, label_color = label_color))
}

#' Lay cells out in a grid
#'
#' Rows are appended left to right, then padded to the width of the widest row
#' and stacked -- padding explicitly rather than letting image_append decide,
#' so the fill colour is the one that was asked for rather than whatever the
#' library defaults to.
#'
#' Cells need NOT all be the same size. The group montage pads every cell to a
#' common square before calling this; montage_qc_cli.r does not, because its
#' three panels are three renderings of ONE image scaled to a common height, and
#' padding them to a common width would put gaps between panels that are meant
#' to be read as a strip. A single row of equal-height cells therefore passes
#' through unchanged, which is what lets that CLI adopt this function without
#' its output moving a pixel.
#'
#' @param cells list of magick images
#' @param ncol columns
#' @param bg pad colour
#' @param full_width pad every row to exactly this width. Given by a caller that
#'   knows the grid it wants -- the group montage passes cell_width * ncol so a
#'   half-empty last row, or a group holding one image, still comes out the same
#'   width as every other montage in the run. NULL means "as wide as the widest
#'   row", which is what a single strip of unequal panels needs. Inferring this
#'   from the first cell was wrong in both directions: it padded a strip that
#'   should not be padded, and it silently assumed every cell was uniform.
#' @return a magick image
mg_grid <- function(cells, ncol, bg = "white", full_width = NULL) {
  if (!length(cells)) {
    stop("A montage of no cells is not a montage.", call. = FALSE)
  }
  ncol <- max(1L, as.integer(ncol))
  rows <- split(seq_along(cells), ceiling(seq_along(cells) / ncol))
  strips <- lapply(rows, function(ix) magick::image_append(do.call(c, cells[ix])))
  full_w <- if (is.null(full_width)) {
    max(vapply(strips, function(s) magick::image_info(s)$width, numeric(1)))
  } else {
    as.numeric(full_width)
  }
  strips <- lapply(strips, function(s) {
    if (magick::image_info(s)$width == full_w) {
      return(s)
    }
    magick::image_extent(s, paste0(full_w, "x", magick::image_info(s)$height),
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

#' Write a montage, at a stated bit depth
#'
#' ⚠️ magick composes in 16 bits internally, and a montage whose pixels all fit
#' in 8 is nonetheless written as 16-bit once a blank is appended to it -- which
#' is what adding a title band does. Measured on the QC montage: 8-bit without a
#' title, 16-bit with one, the file about twice the size, and every flat grey
#' shifted by 1/255 on the way back out. Invisible, and still a changed file for
#' no gain.
#'
#' Both callers build their montages from 8-bit PNGs (Fiji's overviews, and a
#' ggplot panel rendered to PNG), so 8 is not a downgrade here -- it is the
#' depth the inputs already had. Stated rather than left to the library, so the
#' output does not depend on whether a band happened to be added.
#'
#' @param img a magick image
#' @param path file to write
#' @param depth bits per channel
#' @return path, invisibly
mg_write <- function(img, path, depth = 8L) {
  magick::image_write(magick::image_convert(img, depth = depth), path)
  return(invisible(path))
}
