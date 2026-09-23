# feature_stats.r
#
# Per-FEATURE summaries: one row per detected object, from the per-ROI table
# annotate_features_cli.r writes.
#
# The unit matters. Adjacent z-slices of one object share signal through the
# point-spread function, so ROIs are not replicates -- the feature is. Every
# threshold decision downstream (--class, signal filtering) is a decision about
# features, so it has to be taken against a per-feature distribution, not a
# per-ROI one.
#
# See note/if_quantification.md for why the means here are area-weighted and
# what is still missing before these numbers are comparable between images.


#' Feature name as written in the ROI ids
#'
#' The `roi` column keeps Fiji's original prefix even after --rename, which is
#' what lets this find the matching `_res.txt`: the file on disk is named for
#' what Fiji called the feature, not for what the run reports it as.
#'
#' @param roi character vector of ROI ids
#' @return length-1 character, or NA when no id matches
feature_roi_prefix <- function(roi){
  rx <- "^(.+)_\\d{4}-\\d{4}-\\d{4}$"
  hit <- unique(roi[!is.na(roi) & grepl(rx, roi)])
  if(length(hit) == 0){
    return(NA_character_)
  }
  return(sub(rx, "\\1", hit[1]))
}


#' Aggregate a per-ROI measurement into one number for the feature
#'
#' `wmean` is the default and the only one that accounts for the shape of the
#' object: a feature's end slices are small tapering cross-sections and its
#' middle slices are large, so a plain mean lets the tips vote as loudly as the
#' equator. Weighting by area is equivalent to sum(RawIntDen)/sum(area).
#'
#' @param value  per-ROI values
#' @param weight per-ROI areas
#' @param stat   wmean | mean | median | sd | min | max | sum
aggregate_roi_stat <- function(value, weight, stat = "wmean"){
  known <- c("wmean", "mean", "median", "sd", "min", "max", "sum")
  if(!stat %in% known){
    stop("Unknown statistic '", stat, "'; use one of ", paste(known, collapse = ", "),
         call. = FALSE)
  }
  ok <- !is.na(value)
  if(!any(ok)){
    return(NA_real_)
  }
  value <- value[ok]
  weight <- weight[ok]
  out <- switch(stat,
    wmean  = if(all(is.na(weight)) || sum(weight, na.rm = TRUE) == 0){
               mean(value)
             }else{
               stats::weighted.mean(value, w = weight, na.rm = TRUE)
             },
    mean   = mean(value),
    median = stats::median(value),
    sd     = if(length(value) < 2){ NA_real_ }else{ stats::sd(value) },
    min    = min(value),
    max    = max(value),
    sum    = sum(value))
  return(as.numeric(out))
}


#' One row per detected feature
#'
#' Only rows naming a real feature are summarised -- `invalid_*` and `failed_*`
#' are counted separately and reported, never silently folded in, because a
#' sample whose objects mostly failed a filter must not look like a sample that
#' genuinely has few objects.
#'
#' @param st_df        per-ROI table from annotate_features_cli.r (sf or plain)
#' @param res          optional measurement table from read_fiji_result(), or a
#'                     list of them; joined on `roi`
#' @param channel_stat aggregation for the per-channel signal, see
#'                     aggregate_roi_stat()
#' @param meta_cols    extra columns to carry through, constant within a feature
#' @return tibble, one row per sample + feature_id
summarise_feature_stats <- function(st_df, res = NULL, channel_stat = "wmean",
                                    meta_cols = character(0)){

  tab <- st_df
  if(inherits(tab, "sf")){
    tab <- sf::st_drop_geometry(tab)
  }
  tab <- tibble::as_tibble(tab)

  needed <- c("roi", "z", "area", "feature_id", "feature_type", "sample")
  absent <- setdiff(needed, colnames(tab))
  if(length(absent) > 0){
    stop("Feature table is missing column(s): ", paste(absent, collapse = ", "),
         call. = FALSE)
  }

  # Real features only; see note above.
  is_valid <- !is.na(tab$feature_id) &
    startsWith(tab$feature_id, paste0(tab$feature_type, "_"))
  valid <- tab[is_valid, , drop = FALSE]
  if(nrow(valid) == 0){
    warning("No valid features in this table", call. = FALSE)
    return(tibble::tibble())
  }

  # --- channel measurements -----------------------------------------------------
  if(!is.null(res)){
    if(is.data.frame(res)){
      res <- list(res)
    }
    res_df <- dplyr::bind_rows(res)
    if(nrow(res_df) > 0){
      if(!all(c("roi", "ch", "mean") %in% colnames(res_df))){
        stop("Measurement table needs roi, ch and mean columns", call. = FALSE)
      }
      keep <- c("roi", "ch", "mean", intersect(c("median", "circ"), colnames(res_df)))
      res_df <- unique(res_df[, keep, drop = FALSE])
      # One ROI measured once per channel. Anything else means the join would
      # multiply rows, which would silently reweight every average below.
      dup <- duplicated(res_df[, c("roi", "ch")])
      if(any(dup)){
        warning("Measurement table has ", sum(dup),
                " duplicate roi/channel row(s); keeping the first of each",
                call. = FALSE)
        res_df <- res_df[!dup, , drop = FALSE]
      }
      valid <- dplyr::left_join(valid, res_df, by = "roi",
                                relationship = "many-to-many")
    }
  }

  # --- geometry, one row per feature ---------------------------------------------
  # Distinct on roi first: a channel join above put one row per roi per channel,
  # and area/z must not be counted once per channel.
  # Tables written before --bridge_roi existed have no is_bridge column; treat
  # them as having no bridges rather than failing on a missing column.
  if(!("is_bridge" %in% colnames(valid))){
    valid$is_bridge <- FALSE
  }
  valid$is_bridge <- !is.na(valid$is_bridge) & valid$is_bridge

  geo_src <- unique(valid[, c("sample", "feature_type", "feature_id", "roi", "z", "area", "is_bridge")])

  # Bridge ROIs are counted, then set aside. Every statistic below describes the
  # ROIs that are evidence for the object, which is what define_feature_group()
  # tests min_z_span and feature_area against -- if the reported z_span counted
  # bridges, a threshold read off these plots would not mean the same thing as
  # the same number passed to the filter.
  bridge_n <- geo_src %>%
    dplyr::group_by(sample, feature_type, feature_id) %>%
    dplyr::summarise(n_bridge  = sum(is_bridge),
                     n_roi_all = dplyr::n(),
                     n_z_all   = dplyr::n_distinct(z),
                     # Max SEED ROIs on any one slice. One object contributes
                     # one ROI per slice -- define_feature_group() assumes no
                     # two polygons overlap within a slice -- so anything above
                     # 1 means the feature spans objects sitting SIDE BY SIDE,
                     # not one object followed through z. That is what a merged
                     # mask does once it is allowed to form edges.
                     #
                     # Seeds only, deliberately. A bridge is often a mask lying
                     # OVER the ROIs it connects, so counting bridges would read
                     # 2 on a perfectly good rescue and the signal would stop
                     # meaning "two objects".
                     max_roi_per_z = max(table(z[!is_bridge])),
                     .groups   = "drop")

  geo_src <- geo_src[!geo_src$is_bridge, , drop = FALSE]
  geo <- geo_src %>%
    dplyr::group_by(sample, feature_type, feature_id) %>%
    dplyr::summarise(
      n_roi     = dplyr::n(),
      n_z       = dplyr::n_distinct(z),
      z_min     = min(z),
      z_max     = max(z),
      z_span    = max(z) - min(z) + 1L,
      area_med  = stats::median(area),
      area_mean = mean(area),
      area_max  = max(area),
      area_sum  = sum(area),
      .groups   = "drop")

  # z_span counts the extent, n_z the slices actually detected. They differ
  # exactly when the object has holes in z, which is worth seeing.
  geo$z_gaps <- geo$z_span - geo$n_z

  # n_roi counts ordinary (seed) ROIs only; n_roi_all includes the bridges, so
  # frac_bridge is over the feature's whole ROI set and the two counts are
  # visible side by side rather than the reader having to infer one from the
  # other. A high fraction says the count is resting on ROIs a filter rejected
  # -- evidence about the filter, not a result to trust.
  geo <- dplyr::left_join(geo, bridge_n, by = c("sample", "feature_type", "feature_id"))
  geo$n_bridge  <- ifelse(is.na(geo$n_bridge), 0L, as.integer(geo$n_bridge))
  geo$n_roi_all <- ifelse(is.na(geo$n_roi_all), geo$n_roi, as.integer(geo$n_roi_all))
  geo$n_z_all   <- ifelse(is.na(geo$n_z_all),   geo$n_z,   as.integer(geo$n_z_all))
  geo$frac_bridge <- ifelse(geo$n_roi_all > 0, geo$n_bridge / geo$n_roi_all, NA_real_)
  geo$max_roi_per_z <- ifelse(is.na(geo$max_roi_per_z), 1L, as.integer(geo$max_roi_per_z))

  # --- shape ----------------------------------------------------------------------
  if("circ" %in% colnames(valid)){
    circ <- unique(valid[, c("sample", "feature_id", "roi", "area", "circ")]) %>%
      dplyr::group_by(sample, feature_id) %>%
      dplyr::summarise(circ_med = stats::median(circ, na.rm = TRUE),
                       circ_min = suppressWarnings(min(circ, na.rm = TRUE)),
                       .groups = "drop")
    circ$circ_min[!is.finite(circ$circ_min)] <- NA_real_
    geo <- dplyr::left_join(geo, circ, by = c("sample", "feature_id"))
  }

  # --- per-channel signal ----------------------------------------------------------
  if("ch" %in% colnames(valid)){
    sig <- valid %>%
      dplyr::filter(!is.na(ch)) %>%
      dplyr::group_by(sample, feature_id, ch) %>%
      dplyr::summarise(
        signal = aggregate_roi_stat(mean, area, channel_stat),
        .groups = "drop") %>%
      dplyr::mutate(ch = paste0("ch", ch, "_signal")) %>%
      tidyr::pivot_wider(names_from = ch, values_from = signal)
    geo <- dplyr::left_join(geo, sig, by = c("sample", "feature_id"))
  }

  # --- metadata ---------------------------------------------------------------------
  meta_cols <- intersect(meta_cols, colnames(tab))
  if(length(meta_cols) > 0){
    meta <- unique(valid[, c("sample", "feature_id", meta_cols), drop = FALSE])
    if(nrow(meta) > nrow(geo)){
      warning("Metadata column(s) vary within a feature; taking the first value",
              call. = FALSE)
      meta <- meta[!duplicated(meta[, c("sample", "feature_id")]), , drop = FALSE]
    }
    geo <- dplyr::left_join(geo, meta, by = c("sample", "feature_id"))
  }

  attr(geo, "channel_stat") <- channel_stat
  return(geo)
}


#' Counts of what did NOT become a feature, per sample
#'
#' Reported alongside the stats so that a thin distribution can be read as
#' either "few objects here" or "most of them failed a filter".
#' @param st_df per-ROI table
feature_reject_counts <- function(st_df){
  tab <- st_df
  if(inherits(tab, "sf")){
    tab <- sf::st_drop_geometry(tab)
  }
  tab <- tibble::as_tibble(tab)
  tab$bucket <- dplyr::case_when(
    is.na(tab$feature_id)                                             ~ "unassigned",
    startsWith(tab$feature_id, paste0(tab$feature_type, "_"))         ~ "feature",
    startsWith(tab$feature_id, "invalid_")                            ~ "invalid",
    startsWith(tab$feature_id, "failed_")                             ~ "failed",
    TRUE                                                              ~ "other")
  out <- tab %>%
    dplyr::group_by(sample, feature_type, bucket) %>%
    dplyr::summarise(n_roi = dplyr::n(), .groups = "drop")
  return(out)
}
