# feature_centroids.r
#
# One point per feature, for linking features across time (the `tracking`
# milestone, note/time_series_plan.md §4), and the fingerprint that says which
# annotation a set of tracks or hand edits was made against.
#
# Depends on feature_join.r for run_id_from().


#' Rows that are real features: `<type>_NNNN`, as `.cli_valid_rows()` reads it
#'
#' `invalid_<type>_NNNN`, `failed_<type>_<reason>` and NA are not features to
#' locate or track (note/data_formats.md §5). Tested against each row's OWN
#' feature_type, so any feature name works, renamed ones included.
.centroid_valid <- function(df){
  !is.na(df$feature_id) & startsWith(df$feature_id, paste0(df$feature_type, "_"))
}


#' Area-weighted centroid of every feature, in calibrated units
#'
#' x and y are the area-weighted mean of the feature's per-slice polygon
#' centroids -- already calibrated, because the outline tables are. z is the
#' area-weighted mean SLICE, converted with `z_step`, slice 1 at z = 0 as ImageJ
#' calibrates it. Weighting by area puts the centroid at the object's bulk
#' rather than halfway between its end slices, and because it is a weighted
#' mean it is not stuck on whole slices: it moves less than one z step when an
#' object's tapering ends come and go.
#'
#' Bridge ROIs are set aside, as feature_stats.r does for every statistic: they
#' hold an object together in the grouping but are not evidence of where it is,
#' and a rejected ROI is often the odd-shaped one.
#'
#' z needs the z step, and when there is none it is left NA rather than written
#' as slice numbers that a reader would take for micrometres:
#'   - a series whose every ROI sits on slice 1 is a single plane: z = 0,
#'     whatever `z_step` is (there is no z axis to measure);
#'   - otherwise z = (weighted slice - 1) * z_step, and NA when z_step is NA.
#'
#' @param st_df  the annotation (`_features.rds`): one row per ROI, with
#'   series_id, t, z, area, is_bridge, feature_id, feature_type, run_id and
#'   a polygon geometry. One series or several.
#' @param z_step the z step in calibrated units, or NA
#' @return a tibble, one row per feature: series_id, t, feature_type,
#'   feature_id, x, y, z, n_roi, area_sum, area_max, run_id, fingerprint
feature_centroids <- function(st_df, z_step = NA_real_){
  need <- c("series_id", "t", "z", "area", "feature_id", "feature_type")
  absent <- setdiff(need, colnames(st_df))
  if(length(absent)){
    stop("feature_centroids() needs column(s): ", paste(absent, collapse = ", "),
         call. = FALSE)
  }
  if(!inherits(st_df, "sf")) st_df <- sf::st_as_sf(st_df)
  tab <- sf::st_drop_geometry(st_df)
  if(!"is_bridge" %in% colnames(tab)) tab$is_bridge <- FALSE
  if(!"run_id" %in% colnames(tab)) tab$run_id <- NA_character_

  # Single plane, decided per series over every ROI it has -- not per feature,
  # where a multi-slice image's one-slice feature would look like a plane.
  single_plane <- tapply(tab$z, tab$series_id, function(z) all(z == 1))

  keep <- .centroid_valid(tab) & !tab$is_bridge
  if(!any(keep)) return(.empty_centroids())
  # The geometry's own centroid, not the table's x/y: a polygon's centroid is
  # its centre of area, which a vertex average is not.
  cxy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(st_df)[keep]))
  pts <- tibble::tibble(series_id = tab$series_id[keep], t = as.integer(tab$t[keep]),
                        feature_type = tab$feature_type[keep],
                        feature_id = tab$feature_id[keep],
                        run_id = tab$run_id[keep],
                        cx = cxy[, "X"], cy = cxy[, "Y"],
                        z = tab$z[keep], area = tab$area[keep])

  out <- pts %>%
    dplyr::group_by(series_id, t, feature_type, feature_id) %>%
    dplyr::summarise(
      x        = sum(area * cx) / sum(area),
      y        = sum(area * cy) / sum(area),
      z_slice  = sum(area * z) / sum(area),
      n_roi    = dplyr::n(),
      area_sum = sum(area),
      area_max = max(area),
      run_id   = dplyr::first(run_id),
      .groups  = "drop")

  # as.vector: tapply() returns a 1-d array, and ifelse() would carry its dim
  # onto z -- a column that prints like a number and compares like an array.
  flat <- as.vector(single_plane[out$series_id])
  out$z <- ifelse(flat, 0, (out$z_slice - 1) * z_step)
  out$z_slice <- NULL

  fp <- feature_fingerprint(st_df)
  out <- dplyr::left_join(out, fp, by = c("series_id", "feature_type"))
  out <- out[order(out$series_id, out$t, out$feature_id), , drop = FALSE]
  return(out[, c("series_id", "t", "feature_type", "feature_id", "x", "y", "z",
                 "n_roi", "area_sum", "area_max", "run_id", "fingerprint")])
}

.empty_centroids <- function(){
  tibble::tibble(series_id = character(0), t = integer(0), feature_type = character(0),
                 feature_id = character(0), x = numeric(0), y = numeric(0),
                 z = numeric(0), n_roi = integer(0), area_sum = numeric(0),
                 area_max = numeric(0), run_id = character(0),
                 fingerprint = character(0))
}


#' What a feature IS, hashed: one fingerprint per (series, feature type)
#'
#' The sorted (feature_id, t, roi) triples of the type's real features, bridge
#' ROIs included -- they took part in the grouping, so a regrouping that
#' changes only them still changes what the features are. Exact (no floating
#' point), needs no _config.txt, and changes exactly when a re-annotation
#' regroups ROIs.
#'
#' Why not run_id: run_id changes with VERSION, every parameter and every input
#' file of the run. That is right for a table regenerated in seconds, and wrong
#' for hand edits, which would be refused after a release bump or after one
#' more series joined the batch though no feature had changed
#' (note/time_series_plan.md §4 `tracking`).
#'
#' @param st_df the annotation; one series or several
#' @return a tibble: series_id, feature_type, fingerprint (10 hex characters)
feature_fingerprint <- function(st_df){
  tab <- if(inherits(st_df, "sf")) sf::st_drop_geometry(st_df) else st_df
  tab <- tab[.centroid_valid(tab), c("series_id", "feature_type", "feature_id", "t", "roi"),
             drop = FALSE]
  if(!nrow(tab)){
    return(tibble::tibble(series_id = character(0), feature_type = character(0),
                          fingerprint = character(0)))
  }
  keys <- unique(tab[, c("series_id", "feature_type")])
  keys$fingerprint <- vapply(seq_len(nrow(keys)), function(i){
    m <- tab[tab$series_id == keys$series_id[i] & tab$feature_type == keys$feature_type[i], ]
    lines <- sort(paste(m$feature_id, as.integer(m$t), m$roi, sep = "\t"))
    run_id_from(c("feature_fingerprint", keys$series_id[i], keys$feature_type[i], lines))
  }, character(1))
  return(tibble::as_tibble(keys))
}
