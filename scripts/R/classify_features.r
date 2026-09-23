# classify_features.r
#
# Put each detected feature into a named class from its own statistics.
#
# The biology is declared, never hardcoded -- the same rule as relate_features.r.
# Nothing here knows what an oocyte is; it knows that a class is a set of ranges
# a feature's statistics must all fall inside.


#' Assign each feature to the first class it matches
#'
#' Priority is the order of `spec`, which is the order the classes were named on
#' the command line. A feature matching several classes takes the first, and the
#' number of such features is reported rather than left implicit -- the choice
#' is defensible, but only if you can see that it was made.
#'
#' A feature matching none is an ORPHAN: `class` is `NA` and it is kept unless
#' `drop_orphan` says otherwise. Keeping it is the default for the same reason
#' relate_features.r keeps a parentless nucleolus -- an object that fits no
#' class is evidence about the classes, and dropping it destroys the evidence.
#'
#' @param stats       per-feature table (feature_stats.tsv)
#' @param spec        ordered named list; each element is a named list of
#'                    `c(lo, hi)` ranges keyed by column
#' @param drop_orphan remove features matching no class
#' @return `stats` with a `class` column, carrying attributes `n_multi`,
#'         `n_orphan` and `class_order`
classify_features <- function(stats, spec, drop_orphan = FALSE){
  if(!length(spec)){
    return(stats)
  }
  if(!nrow(stats)){
    stats$class <- character(0)
    return(stats)
  }

  wanted <- unique(unlist(lapply(spec, names)))
  absent <- setdiff(wanted, colnames(stats))
  if(length(absent)){
    stop("--class names column(s) not in the stats table: ",
         paste(absent, collapse = ", "),
         "\n  available: ", paste(colnames(stats), collapse = ", "),
         call. = FALSE)
  }
  not_num <- wanted[!vapply(stats[wanted], is.numeric, logical(1))]
  if(length(not_num)){
    stop("--class needs numeric column(s); these are not: ",
         paste(not_num, collapse = ", "), call. = FALSE)
  }

  # One logical column per class. NA never matches: a feature whose ch1_signal
  # is NA because no measurement table was found has not been shown to be
  # outside the range, but it has not been shown to be inside it either, and
  # quietly treating unknown as a match would invent members.
  hits <- vapply(spec, function(conds){
    ok <- rep(TRUE, nrow(stats))
    for(col in names(conds)){
      rng <- conds[[col]]
      v <- stats[[col]]
      ok <- ok & !is.na(v) & v >= rng[1] & v <= rng[2]
    }
    return(ok)
  }, logical(nrow(stats)))
  # vapply drops to a vector when there is a single feature; keep it a matrix.
  if(is.null(dim(hits))){
    hits <- matrix(hits, nrow = nrow(stats), dimnames = list(NULL, names(spec)))
  }

  n_hit <- rowSums(hits)
  first <- apply(hits, 1, function(r) if(any(r)) which(r)[1] else NA_integer_)
  stats$class <- names(spec)[first]

  n_multi <- sum(n_hit > 1)
  if(n_multi > 0){
    warning(n_multi, " feature(s) matched more than one class and took the ",
            "first. The current class priority is: ",
            paste(names(spec), collapse = " > "),
            "\n  Reorder the --class flags to change it.", call. = FALSE)
  }

  n_orphan <- sum(is.na(stats$class))
  if(drop_orphan && n_orphan > 0){
    stats <- stats[!is.na(stats$class), , drop = FALSE]
  }

  attr(stats, "n_multi") <- n_multi
  attr(stats, "n_orphan") <- n_orphan
  attr(stats, "class_order") <- names(spec)
  return(stats)
}


#' Count features per class, orphans included
#'
#' @param stats classified table
#' @param order class names in priority order
#' @return data.frame of class and n, in priority order, with `(unclassified)`
#'         last when any orphan survived
class_counts <- function(stats, order = attr(stats, "class_order")){
  if(!("class" %in% colnames(stats))){
    return(data.frame(class = character(0), n = integer(0)))
  }
  n <- vapply(order, function(k) sum(stats$class == k, na.rm = TRUE), integer(1))
  out <- data.frame(class = order, n = as.integer(n), stringsAsFactors = FALSE)
  n_na <- sum(is.na(stats$class))
  if(n_na > 0){
    out <- rbind(out, data.frame(class = "(unclassified)", n = n_na,
                                 stringsAsFactors = FALSE))
  }
  return(out)
}
