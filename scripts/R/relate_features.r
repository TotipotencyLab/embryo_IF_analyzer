## Function group: containment relationships between annotated features
##
## A nucleolus sits inside a nucleus; a nucleus sits inside a cell. Which
## feature contains which is NOT encoded here -- the caller declares it, so the
## same code serves an assay with different compartments.
##
## Two facts about real data shape everything below:
##
##   1. Outlines are imperfect, so an inner feature can poke outside its parent.
##      Containment is therefore a ratio with a threshold, not a boolean.
##   2. Detection skips slices, so a nucleus may be found at z = 5,6,8 while its
##      nucleolus is found at z = 7. Matching per-slice alone would orphan a
##      biologically correct feature. A nucleus is a solid; a missing slice is a
##      detection failure, not a hole.
##
## Nothing here modifies the features themselves. Gap-filling is a matching-time
## relaxation only -- writing an interpolated slice back into the parent would
## silently change its area, its n_roi and its drawn outline.


#' Validate a child=parent containment specification
#'
#' @param within named character vector, names are children, values are parents
#'   (e.g. c(nucleolus = "nucleus", nucleus = "cell"))
#' @param known  feature types actually present, for a useful error message
#' @return the spec, unchanged, or stops
validate_within_spec <- function(within, known = NULL){
  if(length(within) == 0){
    return(within)
  }
  if(is.null(names(within)) || any(!nzchar(names(within)))){
    stop("Containment spec must be named: 'child=parent'")
  }

  # Each child has exactly one parent; a parent may have many children.
  if(anyDuplicated(names(within))){
    dup <- unique(names(within)[duplicated(names(within))])
    stop("A feature cannot have two parents: ", paste(dup, collapse=", "))
  }

  self_ref <- names(within)[names(within) == unname(within)]
  if(length(self_ref) > 0){
    stop("A feature cannot contain itself: ", paste(self_ref, collapse=", "))
  }

  # Walk each chain; a cycle would otherwise loop forever downstream.
  for(child in names(within)){
    seen <- child
    cur <- unname(within[[child]])
    while(!is.na(cur) && cur %in% names(within)){
      if(cur %in% seen){
        stop("Containment spec is circular: ", paste(c(seen, cur), collapse=" -> "))
      }
      seen <- c(seen, cur)
      cur <- unname(within[[cur]])
    }
  }

  if(!is.null(known)){
    unknown <- setdiff(unique(c(names(within), unname(within))), known)
    if(length(unknown) > 0){
      stop("Containment spec names feature(s) that were not annotated: ",
           paste(unknown, collapse=", "),
           "\n  annotated: ", paste(known, collapse=", "))
    }
  }

  return(within)
}


#' Per-feature geometry summary used for containment tests
#'
#' Collapses a per-ROI table into one entry per feature_id holding the slices it
#' occupies, the geometry on each slice, and the 2D union footprint.
#'
#' @param st_df per-ROI sf with feature_id, z and geometry
#' @return named list, one element per feature_id
.feature_z_index <- function(st_df){
  if(!inherits(st_df, "sf")){
    st_df <- sf::st_as_sf(st_df)
  }
  out <- list()
  for(fid in unique(st_df$feature_id)){
    rows <- st_df[st_df$feature_id == fid, ]
    zs <- sort(unique(rows$z))

    # NB: a feature is assumed to have one ROI per slice, but nothing enforces
    #     it upstream, so union whatever is on each slice rather than taking
    #     the first and silently ignoring the rest.
    by_z <- lapply(zs, function(zz){
      g <- sf::st_geometry(rows[rows$z == zz, ])
      if(length(g) == 1){ return(g[[1]]) }
      return(sf::st_union(g)[[1]])
    })
    names(by_z) <- as.character(zs)

    out[[fid]] <- list(
      feature_id = fid,
      z          = zs,
      z_min      = min(zs),
      z_max      = max(zs),
      by_z       = by_z,
      footprint  = sf::st_union(sf::st_geometry(rows))[[1]],
      # NB: measured on the SAME per-slice geometries the intersection uses,
      #     not by summing the individual ROI areas. Where one feature holds
      #     two overlapping ROIs on a slice, the shared area is counted once in
      #     by_z and twice in a naive row sum -- which made the denominator
      #     bigger than the numerator could ever be, so a child lying wholly
      #     inside its parent scored 0.7 and could be rejected outright.
      area       = sum(vapply(by_z, function(g){
        as.numeric(sf::st_area(sf::st_sfc(g)))
      }, numeric(1)))
    )
  }
  return(out)
}


#' Containment of one child feature in one parent feature
#'
#' Sums the intersected area slice by slice and divides by the child's total
#' area, so the result is "what fraction of the child lies inside the parent".
#'
#' Matching is per-slice in priority order:
#'   direct      the parent has an ROI on that slice -- the most specific test,
#'               and the one that tells two parents apart when they overlap in
#'               x-y but sit far apart in z.
#'   gap_filled  the slice lies inside the parent's z-range but the parent was
#'               not detected there; tested against the parent's 2D footprint.
#'   (neither)   contributes nothing.
#'
#' @return list(containment, match) where match is "direct", "gap_filled" or "none"
.containment_one <- function(child, parent){
  if(child$area <= 0){
    return(list(containment = 0, match = "none"))
  }

  inter_area <- 0
  used <- character(0)
  for(zz in child$z){
    key <- as.character(zz)
    parent_geom <- NULL
    how <- NA_character_

    if(key %in% names(parent$by_z)){
      parent_geom <- parent$by_z[[key]]
      how <- "direct"
    }else if(zz >= parent$z_min && zz <= parent$z_max){
      # Inside the parent's extent but undetected on this slice.
      parent_geom <- parent$footprint
      how <- "gap_filled"
    }

    if(is.null(parent_geom)){
      next
    }

    a <- .safe_intersection_area(child$by_z[[key]], parent_geom)
    if(a > 0){
      inter_area <- inter_area + a
      used <- c(used, how)
    }
  }

  if(length(used) == 0){
    return(list(containment = 0, match = "none"))
  }

  # "direct" only when no slice needed the parent's z-range filled in; the
  # caller needs to know how much of the answer rests on that relaxation.
  match_kind <- if(all(used == "direct")){ "direct" }else{ "gap_filled" }
  return(list(containment = inter_area / child$area, match = match_kind))
}


.safe_intersection_area <- function(g1, g2){
  # st_intersection on a self-intersecting ring can error rather than return
  # an empty geometry; a bad outline should cost that pair, not the whole run.
  a <- tryCatch({
    inter <- sf::st_intersection(sf::st_sfc(g1), sf::st_sfc(g2))
    if(length(inter) == 0){ 0 }else{ sum(as.numeric(sf::st_area(inter))) }
  }, error = function(e){
    warning("Intersection failed, treating as no overlap: ",
            conditionMessage(e), call. = FALSE)
    return(0)
  })
  return(a)
}


#' Assign each inner feature to the outer feature that contains it
#'
#' @param st_df           per-ROI sf from define_feature_group(), with
#'                        feature_id, feature_type, z and geometry
#' @param within          named character vector, 'child=parent'
#' @param min_containment fraction of the child that must lie inside the parent;
#'                        1 is strict containment, 0.5 the default. Accepts a
#'                        named vector for per-child thresholds.
#' @param tie_margin      warn when the runner-up parent is within this much of
#'                        the winner and also clears the threshold
#' @param verbose         report per-child assignments
#' @return st_df with parent_feature_id, parent_feature_type, parent_containment
#'   and parent_match added (one value per ROI, constant within a feature)
assign_feature_parent <- function(st_df, within, min_containment = 0.5,
                                  tie_margin = 0.1, verbose = FALSE){
  if(!inherits(st_df, "sf")){
    st_df <- sf::st_as_sf(st_df)
  }
  if(!all(c("feature_id", "feature_type", "z") %in% colnames(st_df))){
    stop("Input needs feature_id, feature_type and z columns")
  }

  # Columns exist even when nothing is related, so downstream code never has to
  # test for their presence. NB: length-0 vectors, not NA -- assigning a
  # length-1 value to a zero-row frame is an error ("replacement has 1 row,
  # data has 0"), and an empty table is a legitimate input.
  n <- nrow(st_df)
  st_df$parent_feature_id   <- rep(NA_character_, n)
  st_df$parent_feature_type <- rep(NA_character_, n)
  st_df$parent_containment  <- rep(NA_real_, n)
  st_df$parent_match        <- rep(NA_character_, n)
  if(n == 0){
    return(st_df)
  }

  # Validated ONCE, against the feature types present in the whole table. A
  # per-sample subset may legitimately lack a feature type -- that is a warning
  # about the data, not a broken spec -- so the recursion below must not
  # re-run this check against the subset.
  within <- validate_within_spec(within, known = unique(st_df$feature_type))
  if(length(within) == 0){
    return(st_df)
  }

  # Several samples in one table would compare a nucleolus against a nucleus
  # from a different image, which is meaningless. Handle them separately.
  if("sample" %in% colnames(st_df) && length(unique(st_df$sample)) > 1){
    parts <- lapply(unique(st_df$sample), function(s){
      .assign_parent_one_sample(st_df[st_df$sample == s, ], within = within,
                                min_containment = min_containment,
                                tie_margin = tie_margin, verbose = verbose)
    })
    return(do.call(rbind, parts))
  }

  return(.assign_parent_one_sample(st_df, within = within,
                                   min_containment = min_containment,
                                   tie_margin = tie_margin, verbose = verbose))
}


#' Parent assignment within a single sample
#'
#' Assumes the spec has already been validated and the columns already added.
#' @keywords internal
.assign_parent_one_sample <- function(st_df, within, min_containment,
                                      tie_margin, verbose){
  for(child_type in names(within)){
    parent_type <- unname(within[[child_type]])
    threshold <- .containment_threshold(min_containment, child_type)

    child_rows  <- .valid_feature_rows(st_df, child_type)
    parent_rows <- .valid_feature_rows(st_df, parent_type)

    if(nrow(child_rows) == 0){
      warning("No valid ", child_type, " features to place inside ", parent_type,
              call. = FALSE)
      next
    }
    if(nrow(parent_rows) == 0){
      warning("No valid ", parent_type, " features: every ", child_type,
              " is left unassigned", call. = FALSE)
      next
    }

    children <- .feature_z_index(child_rows)
    parents  <- .feature_z_index(parent_rows)

    for(cid in names(children)){
      child <- children[[cid]]

      # Scored once per parent and kept, rather than recomputing the winner:
      # .containment_one() walks every slice of the child.
      results <- lapply(parents, function(p){ .containment_one(child, p) })
      scores <- vapply(results, function(r){ r$containment }, numeric(1))

      if(all(scores <= 0)){
        if(verbose){
          message("    ", cid, ": no overlapping ", parent_type)
        }
        next
      }

      ord <- order(scores, decreasing = TRUE)
      best_id <- names(parents)[ord[1]]
      best <- results[[ord[1]]]

      if(best$containment < threshold){
        if(verbose){
          message("    ", cid, ": best ", parent_type, " ", best_id, " at ",
                  round(best$containment, 3), " < ", threshold, "; unassigned")
        }
        next
      }

      # A near-tie that both clear the threshold is a genuine ambiguity. Taking
      # the argmax silently would turn 0.51 vs 0.49 into a fact.
      if(length(ord) > 1){
        second_id <- names(parents)[ord[2]]
        second <- scores[[ord[2]]]
        if(second >= threshold && (best$containment - second) < tie_margin){
          warning(cid, " lies in two ", parent_type, " features: ",
                  best_id, " (", round(best$containment, 3), ") and ",
                  second_id, " (", round(second, 3), "); took ", best_id,
                  call. = FALSE)
        }
      }

      idx <- st_df$feature_id == cid
      st_df$parent_feature_id[idx]   <- best_id
      st_df$parent_feature_type[idx] <- parent_type
      st_df$parent_containment[idx]  <- best$containment
      st_df$parent_match[idx]        <- best$match

      if(verbose){
        message("    ", cid, " -> ", best_id, " (",
                round(best$containment, 3), ", ", best$match, ")")
      }
    }
  }

  return(st_df)
}


.containment_threshold <- function(min_containment, child_type){
  if(length(min_containment) == 1 && is.null(names(min_containment))){
    return(as.numeric(min_containment))
  }
  if(child_type %in% names(min_containment)){
    return(as.numeric(min_containment[[child_type]]))
  }
  if("default" %in% names(min_containment)){
    return(as.numeric(min_containment[["default"]]))
  }
  return(0.5)
}


.valid_feature_rows <- function(st_df, feature_type){
  # Valid features are named <type>_N. invalid_* and failed_* groups are not
  # candidates in either direction -- an ROI that failed the z-span filter is
  # not a nucleus, and should not adopt a nucleolus.
  keep <- st_df$feature_type == feature_type &
    !is.na(st_df$feature_id) &
    startsWith(st_df$feature_id, paste0(feature_type, "_"))
  return(st_df[keep, ])
}


#' Summarise parent assignment, one row per parent feature
#'
#' @param st_df output of assign_feature_parent()
#' @param child_type which child to count
#' @return tibble with one row per parent feature and the number of children
summarise_parent_children <- function(st_df, child_type){
  rows <- st_df[!is.na(st_df$parent_feature_id) &
                  st_df$feature_type == child_type, ]
  if(nrow(rows) == 0){
    return(tibble::tibble(parent_feature_id = character(0),
                          n_child = integer(0),
                          n_gap_filled = integer(0)))
  }
  per_child <- unique(data.frame(
    feature_id        = rows$feature_id,
    parent_feature_id = rows$parent_feature_id,
    parent_match      = rows$parent_match,
    stringsAsFactors  = FALSE
  ))
  out <- per_child %>%
    dplyr::group_by(parent_feature_id) %>%
    dplyr::summarise(n_child = dplyr::n(),
                     n_gap_filled = sum(parent_match == "gap_filled"),
                     .groups = "drop")
  return(tibble::as_tibble(out))
}
