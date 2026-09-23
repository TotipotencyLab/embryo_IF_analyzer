# feature_join.r
#
# Provenance for the R side, and the join that depends on it.
#
# feature_id is sequential WITHIN AN IMAGE and carries no meaning across runs:
# nucleus_1 from two annotate runs are unrelated objects. So joining one run's
# per-feature table onto another run's annotation matches on sample+feature_id
# at essentially 100% and attaches every class to the wrong object. The match
# rate -- the obvious guard -- reads perfect in exactly the case that is broken.
#
# run_id is what makes that detectable. It is the same idea as Fiji's
# _config.txt recording the parameters that produced a results directory; the R
# side simply had no equivalent.


#' A short, stable id for one run's parameters
#'
#' Hashed with tools::md5sum rather than digest::digest so this stays
#' dependency-free: scripts/R/ is copied around and run on other machines.
#'
#' @param parts Character vector of anything that defines the run: resolved
#'   input paths, the effective parameters, the repo version. Order-sensitive
#'   by design -- the caller decides what counts, and sorts where order should
#'   not matter.
#' @return 10 hex characters.
run_id_from <- function(parts){
  txt <- paste(as.character(parts), collapse = "\n")
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f), add = TRUE)
  writeLines(txt, f)
  return(substr(unname(tools::md5sum(f)), 1, 10))
}


#' Describe the inputs of a run, for hashing
#'
#' Name and size, not content. Hashing the outline tables would be stricter but
#' costs a full read of every input on a large batch; name+size catches the
#' realistic mistake (pointing at a different results directory) and does NOT
#' catch a file edited in place to the same length. Stated rather than hidden.
#'
#' @param paths Resolved input file paths.
#' @return Character vector, sorted, one "basename:size" per file.
run_input_fingerprint <- function(paths){
  if(!length(paths)){
    return(character(0))
  }
  sz <- file.size(paths)
  sz[is.na(sz)] <- -1
  return(sort(paste0(basename(paths), ":", sz)))
}


#' Join a per-feature table onto a feature-keyed table, refusing a mismatch
#'
#' @param x        the spine -- the annotation, one row per ROI, or any table
#'                 carrying `sample` and `feature_id`
#' @param tbl      the table to bring columns from, one row per feature
#' @param cols     columns of `tbl` to add; default every column that is not a
#'                 key and not already in `x`
#' @param by       join keys
#' @param force    join even when the two run_ids disagree
#' @param expect   logical over rows of `x`: which rows are EXPECTED to find a
#'   match. Default all. A per-feature table holds only valid features, so
#'   without this the invalid and failed rows count as misses and the run warns
#'   every time -- and a warning that always fires is one nobody reads.
#' @param what     name of `tbl` for messages
#' @return `x` with the requested columns added
join_feature_table <- function(x, tbl, cols = NULL, by = c("sample", "feature_id"),
                               force = FALSE, expect = NULL,
                               what = "--feature_table"){
  absent_x <- setdiff(by, colnames(x))
  absent_t <- setdiff(by, colnames(tbl))
  if(length(absent_x) || length(absent_t)){
    stop("Cannot join on ", paste(by, collapse = " + "), ": missing ",
         if(length(absent_x)) paste0("from the features table (",
                                     paste(absent_x, collapse = ", "), ")") else "",
         if(length(absent_x) && length(absent_t)) " and " else "",
         if(length(absent_t)) paste0("from ", what, " (",
                                     paste(absent_t, collapse = ", "), ")") else "",
         call. = FALSE)
  }

  # --- provenance ---------------------------------------------------------------
  rid_x <- unique(stats::na.omit(x$run_id))
  rid_t <- unique(stats::na.omit(tbl$run_id))
  if(length(rid_x) && length(rid_t)){
    if(!identical(sort(as.character(rid_x)), sort(as.character(rid_t)))){
      msg <- paste0(
        what, " was produced by a different annotate run.\n",
        "  features:   ", paste(rid_x, collapse = ", "), "\n",
        "  ", what, ": ", paste(rid_t, collapse = ", "), "\n",
        "  feature_id is sequential per image, so these tables describe ",
        "different objects under the same names.\n",
        "  Re-run the statistics against these features, or pass --force if ",
        "you are certain.")
      if(!force){
        stop(msg, call. = FALSE)
      }
      warning(msg, call. = FALSE)
    }
  }else{
    # Not an error: tables written before run_id existed are still usable, but
    # the one guard that would catch a cross-run join is absent and saying so
    # is the point.
    warning("No run_id on ", if(!length(rid_x)) "the features table" else what,
            ", so the join cannot be checked against the run that produced it. ",
            "Re-run the earlier step to get one.", call. = FALSE)
  }

  # --- the join -----------------------------------------------------------------
  # A duplicated key would multiply rows and silently reweight every count
  # taken afterwards.
  dup <- duplicated(tbl[, by, drop = FALSE])
  if(any(dup)){
    stop(what, " has ", sum(dup), " duplicate ", paste(by, collapse = "+"),
         " row(s); joining would multiply rows and reweight the counts",
         call. = FALSE)
  }

  if(is.null(cols)){
    cols <- setdiff(colnames(tbl), c(by, setdiff(colnames(x), by)))
  }
  absent_cols <- setdiff(cols, colnames(tbl))
  if(length(absent_cols)){
    stop(what, " has no column(s): ", paste(absent_cols, collapse = ", "),
         "\n  available: ", paste(colnames(tbl), collapse = ", "), call. = FALSE)
  }

  keep <- unique(c(by, cols))
  out <- dplyr::left_join(x, tbl[, keep, drop = FALSE], by = by)

  # Report the match rate over FEATURES, not rows: the spine is per-ROI, so a
  # row count would be dominated by how many slices each object happens to have.
  want <- if(is.null(expect)) rep(TRUE, nrow(x)) else as.logical(expect)
  feat <- unique(x[want, by, drop = FALSE])
  matched <- nrow(dplyr::semi_join(feat, tbl[, by, drop = FALSE], by = by))
  attr(out, "n_features") <- nrow(feat)
  attr(out, "n_matched") <- matched
  if(matched < nrow(feat)){
    warning(what, " matched ", matched, " of ", nrow(feat),
            " feature(s) that should have matched; the rest get NA. That usually ",
            "means the two tables came from different runs or different --feature ",
            "settings.", call. = FALSE)
  }
  return(out)
}


#' Join several columns into one class label
#'
#' Shared by count_features_cli.r and montage_qc_cli.r so --feature_class_by
#' means the same thing in both.
#'
#' The components are kept as their own columns beside the composite, so
#' nothing downstream has to split the label back apart -- which is where the
#' separator would bite.
#'
#' @param tab  table holding the columns
#' @param cols column names, in label order
#' @param sep  separator
#' @return character vector
compose_feature_class <- function(tab, cols, sep = "-"){
  vals <- lapply(cols, function(cl) {
    v <- as.character(tab[[cl]])
    # An absent class is a fact about the feature, not a blank: the literal
    # keeps it countable and stops paste() producing "oocyte-NA". Same string
    # classify_features() writes, so the two tables agree.
    v[is.na(v) | !nzchar(v)] <- CLASS_UNCLASSIFIED
    v
  })
  hit <- cols[vapply(vals, function(v) any(grepl(sep, v, fixed = TRUE)), logical(1))]
  if (length(hit)) {
    warning("--feature_class_by column(s) ", paste(hit, collapse = ", "),
            " already contain the separator '", sep,
            "', so the composite label is ambiguous. Use --class_sep to pick another.",
            call. = FALSE)
  }
  return(do.call(paste, c(vals, sep = sep)))
}
