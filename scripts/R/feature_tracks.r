# feature_tracks.r
#
# Tracks, joined onto features at use: Make_FeatureTracks.groovy writes the
# links (<series_id>_<feature_type>_tracks.tsv), and this file is the one place
# they become ids (note/time_series_plan.md §4 `tracking`).
#
# Nothing here is stored. track_id and branch_id are numbered from the links
# every time they are joined, deterministically, so the feature table keeps
# features and the tracks table stays the only store of tracks -- a second copy
# of every track_id is a copy a later hand edit would make disagree, with
# nothing to say which is right.
#
#   track_id   a lineage: everything connected by links, mother and daughters
#              together. <feature_type>_track_NNNN.
#   branch_id  one object through time: a stretch of a track with no division
#              or merge in it, at most one feature per t. <track_id>_bNN.
#
# A feature linked to nothing, in either direction, has neither: NA, not a
# track of one.
#
# Depends on feature_join.r for check_run_id().

TRACK_COLUMNS <- c("series_id", "t", "feature_id", "prev_feature_id", "run_id")


#' Where a series' tracks are: beside annotate's output, named by series and type
tracks_path <- function(dir, series_id, feature_type){
  file.path(dir, paste0(series_id, "_", feature_type, "_tracks.tsv"))
}


#' Read one tracks table, refusing one that cannot be checked
#'
#' Every column is read as text: a blank prev_feature_id is a track's start and
#' must stay "", not become NA by a reader's guess. A table without run_id is
#' refused outright -- there are no tracks tables older than run_id to stay
#' compatible with, and without it a table from before a re-annotation would
#' join cleanly onto different nuclei.
read_tracks <- function(path){
  if(!file.exists(path)){
    stop("No tracks table: ", path, call. = FALSE)
  }
  tr <- utils::read.delim(path, colClasses = "character", na.strings = character(0),
                          check.names = FALSE)
  name <- basename(path)
  if(!"run_id" %in% colnames(tr)){
    stop(name, " has no run_id, so nothing can say which annotation it was made ",
         "from. Re-run Make_FeatureTracks.groovy on annotate's current output.",
         call. = FALSE)
  }
  absent <- setdiff(TRACK_COLUMNS, colnames(tr))
  if(length(absent)){
    stop(name, " lacks column(s): ", paste(absent, collapse = ", "),
         " -- is it a tracks table from Make_FeatureTracks.groovy?", call. = FALSE)
  }
  if(nrow(tr) && any(!nzchar(tr$run_id))){
    stop(name, " has ", sum(!nzchar(tr$run_id)), " row(s) with a blank run_id. ",
         "Re-run Make_FeatureTracks.groovy.", call. = FALSE)
  }
  bad_t <- !grepl("^[0-9]+$", tr$t)
  if(any(bad_t)){
    stop(name, ": t is not a whole number on ", sum(bad_t), " row(s), e.g. '",
         tr$t[bad_t][1], "'", call. = FALSE)
  }
  tr$t <- as.integer(tr$t)
  return(tr[, TRACK_COLUMNS, drop = FALSE])
}


#' Hold a tracks table to the features it claims to describe
#'
#' join_feature_table() only WARNS when features go unmatched; a tracks table
#' missing a row, or holding one twice, would shrink or skew every track
#' without a word. So, before anything is numbered: every row names features
#' that exist, every feature has a row, no link appears twice, each row's t is
#' its feature's, and every link goes forward in time. Each refusal names what
#' it found and says to regenerate the table.
#'
#' @param tr    one series' tracks (read_tracks())
#' @param feat  that series' tracked features: one row each, feature_id and t
#' @param name  the file's name, for messages
check_track_links <- function(tr, feat, name){
  fix <- " Re-run Make_FeatureTracks.groovy on annotate's current output."
  eg <- function(x) paste0(paste(utils::head(unique(x), 3), collapse = ", "),
                           if(length(unique(x)) > 3) ", ..." else "")
  unknown <- setdiff(tr$feature_id, feat$feature_id)
  if(length(unknown)){
    stop(name, " names ", length(unknown), " feature(s) not in the features table (",
         eg(unknown), ").", fix, call. = FALSE)
  }
  prev <- tr$prev_feature_id[nzchar(tr$prev_feature_id)]
  unknown_prev <- setdiff(prev, feat$feature_id)
  if(length(unknown_prev)){
    stop(name, " links from ", length(unknown_prev), " feature(s) not in the features ",
         "table (", eg(unknown_prev), ").", fix, call. = FALSE)
  }
  missing <- setdiff(feat$feature_id, tr$feature_id)
  if(length(missing)){
    stop(name, " has no row for ", length(missing), " of ", nrow(feat),
         " feature(s) (", eg(missing), "): every feature has at least one, a blank ",
         "prev_feature_id where nothing leads to it.", fix, call. = FALSE)
  }
  key <- paste(tr$feature_id, tr$prev_feature_id, sep = "\r")
  if(anyDuplicated(key)){
    d <- tr$feature_id[duplicated(key)]
    stop(name, " repeats ", sum(duplicated(key)), " row(s) (", eg(d), ").", fix,
         call. = FALSE)
  }
  starts <- tr$feature_id[!nzchar(tr$prev_feature_id)]
  both <- intersect(starts, tr$feature_id[nzchar(tr$prev_feature_id)])
  if(length(both)){
    stop(name, " has both a start row and a link into ", length(both),
         " feature(s) (", eg(both), ").", fix, call. = FALSE)
  }
  t_of <- stats::setNames(feat$t, feat$feature_id)
  wrong_t <- tr$feature_id[tr$t != t_of[tr$feature_id]]
  if(length(wrong_t)){
    stop(name, " gives ", length(unique(wrong_t)), " feature(s) a t that is not ",
         "their own (", eg(wrong_t), ").", fix, call. = FALSE)
  }
  lk <- tr[nzchar(tr$prev_feature_id), , drop = FALSE]
  back <- lk$feature_id[t_of[lk$prev_feature_id] >= t_of[lk$feature_id]]
  if(length(back)){
    stop(name, " links ", length(back), " feature(s) from one at the same or a later ",
         "t (", eg(back), ").", fix, call. = FALSE)
  }
  return(invisible(TRUE))
}


#' Number one series' tracks and branches from its links
#'
#' Deterministic: identical links give identical ids. Features are ordered by
#' (t, feature_id); a track is numbered by its first feature in that order, and
#' a branch, within its track, by its first feature likewise.
#'
#' A branch starts at a track's first feature, at each daughter of a division
#' (a feature whose predecessor has two successors), and at a merged feature
#' (one with two predecessors); a feature continues its predecessor's branch
#' when it has exactly one predecessor and that predecessor has exactly one
#' successor. A branch formed by a merge is flagged: whatever is measured on it
#' describes two objects.
#'
#' @param feat         the series' tracked features: feature_id, t
#' @param links        its links: feature_id, prev_feature_id (non-blank only)
#' @param feature_type for the id prefix
#' @return list(features = feature_id, track_id, branch_id, branch_merged;
#'              branches = one row per branch: track_id, branch_id,
#'              parent_branch_id, branch_merged, first_t, last_t, n_features)
number_tracks <- function(feat, links, feature_type){
  o <- order(feat$t, feat$feature_id)
  ids <- feat$feature_id[o]
  tt <- feat$t[o]
  n <- length(ids)
  from <- match(links$prev_feature_id, ids)
  to <- match(links$feature_id, ids)
  preds <- split(from, factor(to, levels = seq_len(n)))
  n_pred <- lengths(preds)
  n_succ <- tabulate(from, nbins = n)
  linked <- n_pred > 0L | n_succ > 0L

  # Connected components by union-find, always keeping the SMALLER index as the
  # root: a component's root is then its first feature in (t, feature_id)
  # order, which is exactly what tracks are numbered by.
  root <- seq_len(n)
  find <- function(i){
    while(root[i] != i) i <- root[i]
    i
  }
  for(k in seq_along(from)){
    a <- find(from[k]); b <- find(to[k])
    if(a != b){
      root[max(a, b)] <- min(a, b)
    }
  }
  comp <- vapply(seq_len(n), find, integer(1))
  comp[!linked] <- NA_integer_
  track_no <- match(comp, sort(unique(stats::na.omit(comp))))
  track_id <- ifelse(is.na(track_no), NA_character_,
                     sprintf("%s_track_%04d", feature_type, track_no))

  # Branches, in (t, feature_id) order so a predecessor is always settled first.
  start <- rep(NA_integer_, n)
  for(i in seq_len(n)){
    if(!linked[i]) next
    p <- preds[[i]]
    start[i] <- if(length(p) == 1L && n_succ[p] == 1L) start[p] else i
  }
  branch_id <- rep(NA_character_, n)
  for(tr in unique(stats::na.omit(track_id))){
    in_tr <- which(track_id == tr)
    starts <- sort(unique(start[in_tr]))
    branch_id[in_tr] <- sprintf("%s_b%02d", tr, match(start[in_tr], starts))
  }
  merged <- ifelse(linked, n_pred[start] >= 2L, NA)

  features <- data.frame(feature_id = ids, track_id = track_id, branch_id = branch_id,
                         branch_merged = merged, stringsAsFactors = FALSE)

  b_start <- sort(unique(stats::na.omit(start)))
  branches <- data.frame(
    track_id = track_id[b_start],
    branch_id = branch_id[b_start],
    # The branch(es) its first feature was linked from: one for a daughter,
    # two for a merged branch, none for a track's first branch.
    parent_branch_id = vapply(b_start, function(s){
      paste(sort(unique(branch_id[preds[[s]]])), collapse = ";")
    }, character(1)),
    branch_merged = n_pred[b_start] >= 2L,
    first_t = tt[b_start],
    last_t = vapply(b_start, function(s) max(tt[which(start == s)]), integer(1)),
    n_features = vapply(b_start, function(s) sum(start == s, na.rm = TRUE), integer(1)),
    stringsAsFactors = FALSE)
  branches <- branches[order(branches$track_id, branches$branch_id), , drop = FALSE]
  rownames(branches) <- NULL
  return(list(features = features, branches = branches))
}


#' Join tracks onto a feature table: track_id, branch_id, branch_merged
#'
#' For every series holding features of `feature_type`, its tracks table is
#' read from `tracks_dir`, checked against the run that made the features
#' (check_run_id(), the guard every per-feature join uses) and against the
#' features themselves (check_track_links()), numbered, and joined on
#' series_id + feature_id. Rows of other types, and invalid or failed rows, get
#' NA; so does a feature linked to nothing.
#'
#' A series with no tracks table stops the join: a time course quietly coming
#' out untracked is a join matching nothing.
#'
#' @param st_df        the annotation (per ROI) or any table with series_id,
#'                     feature_id, feature_type, t and run_id
#' @param feature_type the type that was tracked (after --rename, the new name)
#' @param tracks_dir   where the tracks tables are; one directory for all
#' @param force        join even when the tracks' run_id disagrees (warns)
#' @return `st_df` with the three columns added; attr "branches" holds one row
#'         per branch, with series_id and feature_type
join_tracks <- function(st_df, feature_type, tracks_dir, force = FALSE){
  need <- c("series_id", "feature_id", "feature_type", "t")
  absent <- setdiff(need, colnames(st_df))
  if(length(absent)){
    stop("join_tracks() needs column(s): ", paste(absent, collapse = ", "), call. = FALSE)
  }
  tab <- if(inherits(st_df, "sf")) sf::st_drop_geometry(st_df) else as.data.frame(st_df)
  real <- !is.na(tab$feature_id) & !is.na(tab$feature_type) &
    tab$feature_type == feature_type & startsWith(tab$feature_id, paste0(feature_type, "_"))
  if(!any(real)){
    stop("No '", feature_type, "' features to join tracks onto; types present: ",
         paste(sort(unique(tab$feature_type)), collapse = ", "), call. = FALSE)
  }
  sids <- sort(unique(tab$series_id[real]))
  paths <- tracks_path(tracks_dir, sids, feature_type)
  gone <- !file.exists(paths)
  if(any(gone)){
    stop("No tracks table for ", sum(gone), " of ", length(sids), " series in ",
         tracks_dir, ": ", paste(basename(paths[gone]), collapse = ", "),
         ". Run Make_FeatureTracks.groovy (featureType='", feature_type,
         "') on annotate's output, or point at where it wrote them.", call. = FALSE)
  }

  per_feature <- list(); per_branch <- list()
  for(k in seq_along(sids)){
    sid <- sids[k]
    tr <- read_tracks(paths[k])
    name <- basename(paths[k])
    if(nrow(tr) && any(tr$series_id != sid)){
      stop(name, " holds series ", paste(setdiff(unique(tr$series_id), sid), collapse = ", "),
           ", not ", sid, call. = FALSE)
    }
    rows <- tab[real & tab$series_id == sid, , drop = FALSE]
    if(!nrow(tr)){
      # Written for a series that then had no feature of this type; it has
      # some now, so this table was made from another annotation.
      stop(name, " has no rows, but ", sid, " has ", length(unique(rows$feature_id)), " ",
           feature_type, " feature(s): it was made from another annotation. Re-run ",
           "Make_FeatureTracks.groovy on annotate's current output.", call. = FALSE)
    }
    rid_x <- unique(stats::na.omit(rows$run_id))
    # Every tracks table carries a run_id (read_tracks() refused it otherwise);
    # features without one cannot be the features it was made from.
    check_run_id(if(length(rid_x)) rid_x else "(none)", unique(tr$run_id), what = name,
                 force = force,
                 remedy = "Re-run Make_FeatureTracks.groovy on annotate's current output")
    feat <- unique(rows[, c("feature_id", "t")])
    feat$t <- as.integer(feat$t)
    if(anyDuplicated(feat$feature_id)){
      stop(sid, ": a feature_id sits at two t values in the features table",
           call. = FALSE)
    }
    check_track_links(tr, feat, name)
    num <- number_tracks(feat, tr[nzchar(tr$prev_feature_id), c("feature_id", "prev_feature_id")],
                         feature_type)
    per_feature[[sid]] <- cbind(series_id = sid, num$features, stringsAsFactors = FALSE)
    if(nrow(num$branches)){
      per_branch[[sid]] <- cbind(series_id = sid, feature_type = feature_type, num$branches,
                                 stringsAsFactors = FALSE)
    }
  }
  ids <- do.call(rbind, per_feature)
  new <- c("track_id", "branch_id", "branch_merged")
  clash <- intersect(new, colnames(st_df))
  if(length(clash)){
    stop("The table already has ", paste(clash, collapse = ", "),
         "; tracks are joined at use, never stored", call. = FALSE)
  }
  key <- paste(tab$series_id, tab$feature_id, sep = "\r")
  m <- match(key, paste(ids$series_id, ids$feature_id, sep = "\r"))
  m[!real] <- NA_integer_
  out <- st_df
  for(cl in new) out[[cl]] <- ids[[cl]][m]
  branches <- if(length(per_branch)) do.call(rbind, per_branch) else
    data.frame(series_id = character(0), feature_type = character(0), track_id = character(0),
               branch_id = character(0), parent_branch_id = character(0),
               branch_merged = logical(0), first_t = integer(0), last_t = integer(0),
               n_features = integer(0))
  rownames(branches) <- NULL
  attr(out, "branches") <- branches
  return(out)
}
