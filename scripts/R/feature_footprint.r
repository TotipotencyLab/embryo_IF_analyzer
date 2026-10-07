# feature_footprint.r
#
# One 2D footprint per feature: the union of its ROIs over z.
#
# analysis-oo_count-physical_blur (the oocyte count; not merged). Two callers,
# and the point of having one function is that they agree:
#
#   feature_footprint_cli.r  writes the footprints for Fiji to draw on the
#                            overview TIFF (Run_Overview_Batch.groovy)
#   confirm_features_cli.r   tests hand-placed points against them
#
# A point placed inside an outline on screen must be inside the same outline
# here. Rebuilding the union in Groovy (ShapeRoi) would give a second geometry
# that differs at the edges, which is exactly where a point gets placed on a dim
# oocyte.
#
# Coordinates stay in the outline table's frame: calibrated units, y DOWN (image
# orientation, as Fiji wrote them). Nothing is flipped here.

#' The vertex table's columns, in order. Run_Overview_Batch reads these
#' (FeatureOverlay.FOOTPRINT_COLUMNS); test-data_formats.R holds the two and
#' note/data_formats.md to one list.
FOOTPRINT_COLUMNS <- c("name", "feature_id", "part", "ring", "x", "y")

#' Union each feature's ROIs over z.
#'
#' @param x  an annotated feature table (annotate_features_cli.r's
#'           `*_features.rds`, or one derived from it): sf, or a tibble with an
#'           sfc column. Rows with NA feature_id (reached no group) are skipped.
#' @param feature_type  keep only these feature types; NULL = all
#' @return sf with `feature_id`, `n_roi`, `z_min`, `z_max` and a MULTIPOLYGON
#'   geometry, one row per feature id, in order of first appearance.
#'   `failed_<feature>_<reason>` is a bucket, not a feature: its union is every
#'   ROI in the bucket, and its separate pieces come out as separate parts.
feature_footprints <- function(x, feature_type = NULL) {
  if (!inherits(x, "sf")) x <- sf::st_as_sf(x)
  if (!is.null(feature_type)) x <- x[x$feature_type %in% feature_type, ]
  x <- x[!is.na(x$feature_id), ]
  # feature_id can carry a names attribute from the grouping; it is not data.
  fid <- unname(as.character(x$feature_id))
  ids <- unique(fid)
  g_all <- sf::st_geometry(x)
  crs <- sf::st_crs(g_all)
  if (!length(ids)) {
    empty <- sf::st_sfc(crs = crs)
    return(sf::st_sf(feature_id = character(0), n_roi = integer(0),
                     z_min = integer(0), z_max = integer(0), geometry = empty))
  }

  geoms <- lapply(ids, function(id) {
    g <- g_all[fid == id]
    # A traced outline is valid as a rule; make_valid only where it is not, so
    # a valid input passes through untouched.
    bad <- !sf::st_is_valid(g)
    if (any(bad)) g[bad] <- sf::st_make_valid(g[bad])
    u <- sf::st_union(g)
    if (inherits(u, "sfc_GEOMETRYCOLLECTION") || inherits(u[[1]], "GEOMETRYCOLLECTION")) {
      u <- sf::st_collection_extract(u, "POLYGON")
      u <- sf::st_union(u)
    }
    return(sf::st_cast(u, "MULTIPOLYGON")[[1]])
  })
  z <- as.integer(x$z)
  out <- sf::st_sf(
    feature_id = ids,
    n_roi = vapply(ids, function(id) sum(fid == id), integer(1), USE.NAMES = FALSE),
    z_min = vapply(ids, function(id) min(z[fid == id]), integer(1), USE.NAMES = FALSE),
    z_max = vapply(ids, function(id) max(z[fid == id]), integer(1), USE.NAMES = FALSE),
    geometry = sf::st_sfc(geoms, crs = crs))
  return(out)
}

#' Footprints as a vertex table, one row per vertex: the shape of
#' `_outline.txt`, which is what Fiji reads.
#'
#' `part` numbers the separate pieces of one feature (from 1); `ring` is 0 for a
#' piece's outer boundary and 1, 2, ... for its holes. A ring is not closed --
#' the first vertex is not repeated -- as ImageJ's polygons are not.
#'
#' @param fp    output of feature_footprints()
#' @param name  the series id, written into every row's `name` (as
#'              `_outline.txt` carries it)
#' @param digits  rounding of x and y; 4 decimals of a um is far below a pixel
footprint_vertices <- function(fp, name, digits = 4) {
  empty <- data.frame(name = character(0), feature_id = character(0), part = integer(0),
                      ring = integer(0), x = numeric(0), y = numeric(0), stringsAsFactors = FALSE)
  if (!nrow(fp)) return(empty)
  rows <- lapply(seq_len(nrow(fp)), function(i) {
    cc <- sf::st_coordinates(sf::st_geometry(fp)[i])
    # MULTIPOLYGON: L1 = ring within its polygon (1 = outer), L2 = polygon.
    key <- paste(cc[, "L2"], cc[, "L1"])
    last <- !duplicated(key, fromLast = TRUE)   # the closing repeat of each ring
    cc <- cc[!last, , drop = FALSE]
    data.frame(name = name, feature_id = fp$feature_id[i],
               part = as.integer(cc[, "L2"]), ring = as.integer(cc[, "L1"]) - 1L,
               x = round(cc[, "X"], digits), y = round(cc[, "Y"], digits),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  return(out[, FOOTPRINT_COLUMNS])
}
