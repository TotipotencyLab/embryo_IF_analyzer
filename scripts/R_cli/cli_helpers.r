# cli_helpers.r
#
# Shared helpers for the CLIs in this directory. Sourced by each one, not
# duplicated into them: the repo's standing rule is that a new entry point is a
# new configuration of the shared library, not a fork of it.
#
# Everything here is prefixed with "." so that sourcing it into globalenv()
# alongside scripts/R/ cannot shadow a library function.

# --- multi-value arguments ----------------------------------------------------
#' Normalise a multi-value argument
#'
#' argparser has three different shapes for "nothing given", and they are
#' genuinely different objects. Measured on argparser 0.7.3:
#'   flag absent  -> NA            (length-1 LOGICAL)
#'   --flag ''    -> character(0)
#'   --flag a b   -> c("a", "b")
#'
#' @param v    the raw value from parse_args()
#' @param what flag name, for the error message only
#' @return character vector, empty when nothing was given
.cli_resolve_arg <- function(v, what = "argument") {
  if (is.null(v)) return(character(0))
  if (length(v) == 0L) return(character(0))
  if (all(is.na(v))) return(character(0))

  v <- as.character(v[!is.na(v)])
  v <- trimws(v)
  v <- v[nzchar(v)]

  # A value starting with "--" means the preceding flag was rendered empty and
  # swallowed the next one. Silently accepting it turns a malformed command into
  # a plausible-looking run.
  bad <- v[startsWith(v, "--")]
  if (length(bad)) {
    stop("Value(s) for ", what, " look like flags, not values: ",
         paste(bad, collapse = ", "),
         "\n  (a preceding argument was probably rendered empty)", call. = FALSE)
  }
  return(v)
}

#' Stop unless every required argument was supplied
#'
#' argparser has no required=. all(), not any(): a multi-value argument holds
#' 2+ elements and is.na() on those returns a vector, which `if` refuses in
#' R >= 4.2.
#'
#' @param argv     parsed argument list
#' @param required character vector of long names, without the leading --
.cli_require <- function(argv, required) {
  missing <- required[vapply(required, function(a) all(is.na(argv[[a]])), logical(1))]
  if (length(missing)) {
    stop("Missing required argument(s): ", paste0("--", missing, collapse = ", "),
         call. = FALSE)
  }
  return(invisible(NULL))
}

#' Parse 'key=value' tokens into a named character vector
#'
#'   c("default=3", "nucleolus=1")  ->  c(default = "3", nucleolus = "1")
#'   c("3")                         ->  c(default = "3")
#'
#' NB: "=" and not ",", because argparser splits nargs=Inf values on commas
#'     even when the shell delivered them as one element.
#'
#' @param tokens      raw argument value
#' @param what        flag name, for error messages only
#' @param default_key key a bare token is filed under
#' @return named character vector
.cli_key_values <- function(tokens, what = "argument", default_key = "default") {
  tokens <- .cli_resolve_arg(tokens, what)
  if (!length(tokens)) return(character(0))

  has_eq <- grepl("=", tokens, fixed = TRUE)
  keys <- ifelse(has_eq, sub("=.*$", "", tokens), default_key)
  vals <- ifelse(has_eq, sub("^[^=]*=", "", tokens), tokens)

  keys <- trimws(keys); vals <- trimws(vals)
  if (any(!nzchar(keys))) {
    stop("Empty key in ", what, ": ", paste(tokens[!nzchar(keys)], collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(keys)) {
    stop("Duplicate key(s) in ", what, ": ",
         paste(unique(keys[duplicated(keys)]), collapse = ", "), call. = FALSE)
  }
  return(stats::setNames(vals, keys))
}

#' Parse 'key=value' tokens, KEEPING repeats, into a named list
#'
#' Unlike .cli_key_values(), a repeated key collects rather than erroring:
#'
#'   c("area_med=100", "area_med=400", "circ_med=0.6")
#'     -> list(area_med = c("100", "400"), circ_med = "0.6")
#'
#' Needed by --threshold, where two guide lines on one variable is the normal
#' case (a band: small below 400, growing above).
#'
#' @param tokens raw argument value
#' @param what   flag name, for error messages only
.cli_key_values_multi <- function(tokens, what = "argument") {
  tokens <- .cli_resolve_arg(tokens, what)
  if (!length(tokens)) return(list())

  no_eq <- !grepl("=", tokens, fixed = TRUE)
  if (any(no_eq)) {
    stop(what, " needs 'key=value' tokens, got: ",
         paste(tokens[no_eq], collapse = ", "), call. = FALSE)
  }
  keys <- trimws(sub("=.*$", "", tokens))
  vals <- trimws(sub("^[^=]*=", "", tokens))
  if (any(!nzchar(keys))) {
    stop("Empty key in ", what, ": ",
         paste(tokens[!nzchar(keys)], collapse = ", "), call. = FALSE)
  }
  # split() orders by factor level; keep the order the keys were first given in,
  # so the run log reads the way the command line did.
  out <- split(vals, factor(keys, levels = unique(keys)))
  return(as.list(out))
}

#' Parse --plot specs: 'x:y' or 'id=x:y'
#'
#' The id is cosmetic -- it names the page -- because thresholds are keyed to
#' the COLUMN, not to the plot, so nothing has to be matched up by position.
#' Unkeyed specs get p1..pN by position.
#'
#' @param tokens raw argument value
#' @param what   flag name, for error messages only
#' @return data.frame(id, x, y)
.cli_parse_plot_specs <- function(tokens, what = "--plot") {
  tokens <- .cli_resolve_arg(tokens, what)
  if (!length(tokens)) {
    stop("No plot requested: give ", what, " 'x:y', e.g. 'area_med:ch1_signal'",
         call. = FALSE)
  }

  ids <- character(length(tokens))
  pairs <- character(length(tokens))
  for (i in seq_along(tokens)) {
    tk <- tokens[i]
    if (grepl("=", tk, fixed = TRUE)) {
      ids[i] <- trimws(sub("=.*$", "", tk))
      pairs[i] <- trimws(sub("^[^=]*=", "", tk))
      if (!nzchar(ids[i])) {
        stop("Empty plot id in ", what, ": ", tk, call. = FALSE)
      }
    } else {
      ids[i] <- ""                  # filled in below
      pairs[i] <- trimws(tk)
    }
  }

  parts <- strsplit(pairs, ":", fixed = TRUE)
  bad <- vapply(parts, function(p) length(p) != 2L || any(!nzchar(trimws(p))), logical(1))
  if (any(bad)) {
    stop(what, " needs 'x:y' (exactly one colon, neither side empty), got: ",
         paste(tokens[bad], collapse = ", "), call. = FALSE)
  }
  x <- trimws(vapply(parts, `[`, character(1), 1))
  y <- trimws(vapply(parts, `[`, character(1), 2))

  same <- x == y
  if (any(same)) {
    stop(what, " has the same column on both axes, which plots a diagonal and ",
         "nothing else: ", paste(tokens[same], collapse = ", "), call. = FALSE)
  }

  # Auto-ids by position. Done AFTER the explicit ones so a collision between
  # 'p2=a:b' and an auto-assigned p2 is caught rather than silently shadowing.
  auto <- !nzchar(ids)
  ids[auto] <- paste0("p", which(auto))
  if (anyDuplicated(ids)) {
    dup <- unique(ids[duplicated(ids)])
    stop("Duplicate plot id(s) in ", what, ": ", paste(dup, collapse = ", "),
         "\n  (ids not given are auto-assigned p1..pN by position, so an ",
         "explicit 'p2=' can collide with one)", call. = FALSE)
  }

  return(data.frame(id = ids, x = x, y = y, stringsAsFactors = FALSE))
}

#' Read a one-value-per-line list file
#'
#' For arguments that get long enough to live in a file rather than on the
#' command line -- a list of samples to facet, say. Blank lines and anything
#' after a # are dropped, so the file can be annotated.
#'
#' @param path file to read
#' @param what flag name, for error messages only
.cli_read_value_list <- function(path, what = "argument") {
  if (!file.exists(path)) {
    stop("No such file for ", what, ": ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  lines <- sub("#.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) {
    stop("File for ", what, " holds no values: ", path, call. = FALSE)
  }
  return(unique(lines))
}

#' Extract setting per feature
#' Look a per-feature parameter up: the feature's own value, else "default",
#' else the built-in default. Returns a length-1 numeric.
#'
#' `feature` may name SEVERAL keys, tried in order. That is what makes
#' --rename safe: after 'nucleus=oocyte' the reporting name is "oocyte" but the
#' operator may well have written --max_z_dist 'nucleus=2' against the name Fiji
#' produced. Passing c("oocyte", "nucleus") accepts either. Without it the key
#' matched nothing and the flag silently fell back to the built-in default --
#' the run looked fine and used the wrong number.
#'
#' @param lookup  key-value setting
#' @param feature key(s) to try, in order, before "default"
#' @param default the default value when feature is not part of the lookup
#' @param what    typically a flag name, for reporting when value is in the wrong format only
.cli_param_for <- function(lookup, feature, default, what = "parameter") {
  if (!length(lookup)) return(default)
  hit <- feature[feature %in% names(lookup)]
  v <- if (length(hit)) lookup[[hit[1]]]
       else if ("default" %in% names(lookup)) lookup[["default"]]
       else return(default)
  n <- suppressWarnings(as.numeric(v))
  if (is.na(n)) {
    stop("Value for ", what, " (", feature[1], ") is not a number: ", v, call. = FALSE)
  }
  return(n)
}

#' Warn about per-feature keys that match no feature in this run
#'
#' A key nobody claims is almost always a typo or a stale name, and its flag
#' then does nothing at all. Silence there is the repo's characteristic failure:
#' the run looks clean and used a value the operator did not ask for.
#'
#' @param lookups named list of key-value/range lookups, named by flag
#' @param known   every feature name a key may legitimately use
.cli_check_param_keys <- function(lookups, known) {
  for (what in names(lookups)) {
    keys <- names(lookups[[what]])
    if (!length(keys)) next
    orphan <- setdiff(keys, c(known, "default"))
    if (length(orphan)) {
      warning(what, " has key(s) matching no feature in this run: ",
              paste(orphan, collapse = ", "),
              "\n  features present: ", paste(sort(unique(known)), collapse = ", "),
              "\n  that setting is being IGNORED.", call. = FALSE)
    }
  }
  return(invisible(NULL))
}

# --- input resolution ---------------------------------------------------------

#' Turn --input elements into an existing, de-duplicated, sorted file list
#'
#' Each element is one of:
#'   a directory  -> scanned with `pattern`
#'   a glob       -> Sys.glob() (covers the quoted case; an unquoted glob has
#'                   already been expanded by the shell into many elements)
#'   a plain path -> must exist
#'
#' Resolving to nothing is an error. The characteristic failure in this repo is
#' the silent no-op, and an empty glob looks exactly like a clean run.
#'
#' @param input   raw --input value
#' @param pattern regex the basenames must match, e.g. "_nucleus_outline\\.txt$"
#' @param what    flag name, for error messages only
#' @return character vector of absolute paths
.cli_resolve_input_path <- function(input, pattern, what = "--input") {
  input <- .cli_resolve_arg(input, what)
  if (!length(input)) stop("No value given for ", what, call. = FALSE)

  out <- character(0)
  for (el in input) {
    if (dir.exists(el)) {
      hits <- list.files(el, pattern = pattern, full.names = TRUE)
      if (!length(hits)) {
        warning("No file matching the output contract in directory: ", el, call. = FALSE)
      }
      out <- c(out, hits)
    } else if (grepl("[*?[]", el)) {
      hits <- Sys.glob(el)
      hits <- hits[grepl(pattern, basename(hits))]
      if (!length(hits)) warning("Glob matched nothing: ", el, call. = FALSE)
      out <- c(out, hits)
    } else {
      # NB: a path containing a comma has already been split in half by
      #     argparser, so this is where that shows up -- as a file that is not
      #     there, named by a fragment of the real path.
      if (!file.exists(el)) {
        stop("No such file: ", el,
             if (grepl(",", el, fixed = TRUE))
               "\n  (a comma in a path is split by argparser; rename the file)"
             else "", call. = FALSE)
      }
      out <- c(out, el)
    }
  }

  out <- sort(unique(normalizePath(out, mustWork = FALSE)))
  if (!length(out)) {
    stop("No input files resolved from ", what, ". ",
         "Nothing would have been produced.", call. = FALSE)
  }
  return(out)
}

#' Split <prefix><image id>_<feature>_outline.txt into sample and feature
#'
#' Drops nothing silently: files that do not name a requested feature are
#' warned about, and none matching at all is an error.
#'
#' @param paths    resolved input paths
#' @param features feature names to recognise
#' @return data.frame(path, sample, feature)
.cli_parse_contract <- function(paths, features) {
  # NB: (.*?) and not (.*). A GREEDY prefix lets the longest possible sample
  #     name win, so with features "oocyte" and "growing_oocyte" the file
  #     S1_growing_oocyte_outline.txt parsed as sample "S1_growing", feature
  #     "oocyte" -- silently, and reordering the alternation does not help
  #     because the quantifier drives the match, not the branch order. Lazy
  #     takes the shortest prefix that still lets the rest match, which is the
  #     intended reading. Prefer .cli_scan_inputs(), which reads the identity
  #     from the file's own content and cannot be fooled at all.
  rx <- paste0("^(.*?)_(", paste(features, collapse = "|"), ")_outline\\.txt$")
  base <- basename(paths)
  ok <- grepl(rx, base)
  if (any(!ok)) {
    warning("Ignoring ", sum(!ok), " file(s) not matching ",
            "<sample>_<feature>_outline.txt for feature(s) ",
            paste(features, collapse = "/"), ": ",
            paste(utils::head(base[!ok], 5), collapse = ", "),
            if (sum(!ok) > 5) ", ..." else "", call. = FALSE)
  }
  if (!any(ok)) {
    stop("None of the ", length(paths), " input file(s) name a requested feature (",
         paste(features, collapse = ", "), ")", call. = FALSE)
  }
  return(
    data.frame(
      path    = paths[ok],
      sample  = sub(rx, "\\1", base[ok]),
      feature = sub(rx, "\\2", base[ok]),
      stringsAsFactors = FALSE
    )
  )
}

# --- identity from file content -----------------------------------------------

# The ROI id's tail is fixed-shape and anchored: <feature>_SSSS-NNNN-YYYY. A
# GREEDY prefix is correct here, and is what makes multi-word feature names such
# as "growing_oocyte" work -- the opposite of the filename case above, where
# there is no anchor and greedy takes too much.
.ROI_ID_RX <- "^(.+)_\\d{4}-\\d{4}-\\d{4}$"

#' Feature name(s) carried by a vector of ROI ids
#'
#' @param roi character vector of ROI ids
#' @return the distinct feature prefixes found, ids that do not match dropped
.cli_feature_from_roi <- function(roi) {
  roi <- unique(roi[!is.na(roi)])
  ok <- grepl(.ROI_ID_RX, roi)
  if (!any(ok)) return(character(0))
  return(sort(unique(sub(.ROI_ID_RX, "\\1", roi[ok]))))
}

#' Read a Fiji outline table's identity out of the table itself
#'
#' Both identities are already in the file: the `name` column holds the full
#' sample id (the operator's --output_prefix plus the image id) and the `roi`
#' column's prefix holds the feature name. Reading them from the content rather
#' than from the filename means a renamed or oddly-named file cannot corrupt
#' them, and multi-word feature names need no special handling.
#'
#' Only the head of the file is read, because this runs over every candidate
#' path before any work starts. The full read later re-checks consistency
#' across all rows -- see .cli_check_identity().
#'
#' @param path  outline table
#' @param nrows rows to read for the probe
#' @return list(sample, feature, ok, why); ok = FALSE when the content could not
#'         be read, so the caller can fall back to the filename
.cli_identify_outline <- function(path, nrows = 2000L) {
  bad <- function(why) list(sample = NA_character_, feature = NA_character_,
                            ok = FALSE, why = why)
  df <- try(utils::read.table(path, header = TRUE, sep = "\t", nrows = nrows,
                              stringsAsFactors = FALSE), silent = TRUE)
  if (inherits(df, "try-error") || !nrow(df)) return(bad("unreadable or empty"))
  if (!"roi" %in% colnames(df)) return(bad("no roi column"))

  feats <- .cli_feature_from_roi(df$roi)
  if (!length(feats)) return(bad("no ROI id of the form <feature>_SSSS-NNNN-YYYY"))
  if (length(feats) > 1L) {
    # Never pick one silently: a file holding two feature types would otherwise
    # have half its rows filed under the wrong name.
    stop("Outline table ", basename(path), " mixes feature types: ",
         paste(feats, collapse = ", "),
         "\n  (one file is expected to hold exactly one feature)", call. = FALSE)
  }

  smp <- NA_character_
  if ("name" %in% colnames(df)) {
    nm <- unique(trimws(as.character(df$name)))
    nm <- nm[!is.na(nm) & nzchar(nm)]
    if (length(nm) == 1L) {
      smp <- nm
    } else if (length(nm) > 1L) {
      stop("Outline table ", basename(path), " mixes sample names: ",
           paste(utils::head(nm, 4), collapse = ", "),
           call. = FALSE)
    }
  }
  if (is.na(smp)) return(bad("no usable name column"))
  return(list(sample = smp, feature = feats, ok = TRUE, why = NA_character_))
}

#' Re-check a fully-read outline table against the identity the probe found
#'
#' The probe reads only the head of the file. This is the cheap assertion that
#' the tail agrees, run once the rows are in memory anyway.
.cli_check_identity <- function(roi, path, expect_feature) {
  feats <- .cli_feature_from_roi(roi)
  if (length(feats) > 1L || (length(feats) == 1L && !identical(feats, expect_feature))) {
    stop("Outline table ", basename(path), " holds feature(s) ",
         paste(feats, collapse = ", "), " beyond the first ", 2000L,
         " rows, but was identified as '", expect_feature, "'", call. = FALSE)
  }
  return(invisible(TRUE))
}

#' Resolve input paths into jobs, preferring the file's content over its name
#'
#' Returns one row per usable file with:
#'   path        the file
#'   sample      from the `name` column, else the filename
#'   roi_prefix  the feature name AS WRITTEN IN THE ROI IDS -- this is what the
#'               grouping step matches on, and it does not change when the
#'               feature is renamed for reporting
#'   feature     the reporting name (same as roi_prefix until --rename)
#'   from        "content" or "filename", so a run can say which it trusted
#'
#' @param paths    candidate files
#' @param features feature names to keep; empty keeps everything found
.cli_scan_inputs <- function(paths, features = character(0)) {
  rows <- lapply(paths, function(p) {
    id <- .cli_identify_outline(p)
    if (id$ok) {
      return(data.frame(path = p, sample = id$sample, roi_prefix = id$feature,
                        from = "content", stringsAsFactors = FALSE))
    }
    # Fall back to the filename, but only when the caller named the features --
    # without them there is nothing to anchor the split on.
    if (!length(features)) return(NULL)
    one <- tryCatch(.cli_parse_contract(p, features), error = function(e) NULL,
                    warning = function(w) NULL)
    if (is.null(one) || !nrow(one)) return(NULL)
    data.frame(path = p, sample = one$sample, roi_prefix = one$feature,
               from = "filename", stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    stop("None of the ", length(paths), " input file(s) could be identified.",
         "\n  Expected a Fiji outline table with a 'name' column and ROI ids of",
         "\n  the form <feature>_SSSS-NNNN-YYYY.", call. = FALSE)
  }
  jobs <- do.call(rbind, rows)

  if (length(features)) {
    keep <- jobs$roi_prefix %in% features
    if (any(!keep)) {
      message("  ignoring ", sum(!keep), " file(s) holding feature(s) not requested: ",
              paste(sort(unique(jobs$roi_prefix[!keep])), collapse = ", "))
    }
    if (!any(keep)) {
      stop("No input file holds a requested feature (",
           paste(features, collapse = ", "), ").",
           "\n  Found instead: ", paste(sort(unique(jobs$roi_prefix)), collapse = ", "),
           call. = FALSE)
    }
    jobs <- jobs[keep, , drop = FALSE]
  }
  jobs$feature <- jobs$roi_prefix          # --rename overwrites this, not roi_prefix
  rownames(jobs) <- NULL
  return(jobs)
}

#' Apply --rename 'old=new' to the reporting name only
#'
#' roi_prefix is deliberately untouched: the ROI ids inside the file still carry
#' the original prefix, and the grouping step matches on those. Renaming the
#' matcher as well would silently match nothing.
.cli_apply_rename <- function(jobs, rename) {
  if (!length(rename)) return(jobs)
  unknown <- setdiff(names(rename), jobs$roi_prefix)
  if (length(unknown)) {
    warning("--rename names feature(s) not present in the input: ",
            paste(unknown, collapse = ", "), call. = FALSE)
  }
  hit <- jobs$roi_prefix %in% names(rename)
  jobs$feature[hit] <- unname(rename[jobs$roi_prefix[hit]])
  if (any(hit)) {
    for (k in intersect(names(rename), jobs$roi_prefix)) {
      message("  rename: ", k, " -> ", rename[[k]])
    }
  }
  return(jobs)
}

#' Rows holding a real, detected feature
#'
#' The vocabulary (note/data_formats.md §5) is: `<type>_N` is real, while
#' `invalid_<type>_N`, `failed_<type>_<reason>` and NA are not. Testing that
#' against the row's OWN feature_type works for any feature name; the earlier
#' hardcoded list of nucleus/nucleolus/cell/cytoplasm silently dropped anything
#' else from the QC plots, and would drop every renamed feature.
#'
#' @param df a data.frame/sf with feature_id and feature_type columns
.cli_valid_rows <- function(df) {
  if (!all(c("feature_id", "feature_type") %in% colnames(df))) {
    stop("Expected feature_id and feature_type columns", call. = FALSE)
  }
  keep <- !is.na(df$feature_id) &
    startsWith(df$feature_id, paste0(df$feature_type, "_"))
  return(df[keep, , drop = FALSE])
}

# --- ranges -------------------------------------------------------------------

#' Parse 'key=lo:hi' tokens into a named list of length-2 numeric ranges
#'
#'   c("nucleus=80:Inf", "nucleolus=3:150")
#'     -> list(nucleus = c(80, Inf), nucleolus = c(3, 150))
#'
#' NB: ":" and not "-", because a range like 80-Inf is ambiguous against a
#'     negative bound, and not ",", which argparser reserves (see
#'     note/data_formats.md). An open end may be written as "" or "Inf":
#'     "80:" and "80:Inf" both mean 80 upwards.
#'
#' @param tokens raw argument value
#' @param what   flag name, for error messages only
.cli_key_ranges <- function(tokens, what = "argument", default_key = "default") {
  kv <- .cli_key_values(tokens, what, default_key = default_key)
  if (!length(kv)) return(list())

  parse_one <- function(v, key) {
    if (!grepl(":", v, fixed = TRUE)) {
      stop("Value for ", what, " (", key, ") must be 'lo:hi', got: ", v,
           "\n  e.g. '", key, "=80:Inf'", call. = FALSE)
    }
    parts <- strsplit(v, ":", fixed = TRUE)[[1]]
    if (length(parts) == 1L) parts <- c(parts, "")      # "80:" -> 80 upwards
    if (length(parts) != 2L) {
      stop("Value for ", what, " (", key, ") must have exactly one ':', got: ", v,
           call. = FALSE)
    }
    parts <- trimws(parts)
    lo <- if (!nzchar(parts[1])) 0        else suppressWarnings(as.numeric(parts[1]))
    hi <- if (!nzchar(parts[2])) Inf      else suppressWarnings(as.numeric(parts[2]))
    if (is.na(lo) || is.na(hi)) {
      stop("Value for ", what, " (", key, ") is not numeric: ", v, call. = FALSE)
    }
    if (lo > hi) {
      stop("Value for ", what, " (", key, ") has lo > hi: ", v, call. = FALSE)
    }
    return(c(lo, hi))
  }
  out <- lapply(seq_along(kv), function(i) parse_one(kv[[i]], names(kv)[i]))
  names(out) <- names(kv)
  return(out)
}

#' Look a per-feature RANGE up, with the same default/fallback rule as
#' .cli_param_for(), including its several-candidate-keys behaviour. Returns
#' NULL when nothing applies, so a caller can tell "not given" from
#' "given as 0:Inf".
.cli_range_for <- function(lookup, feature, what = "parameter") {
  if (!length(lookup)) return(NULL)
  hit <- feature[feature %in% names(lookup)]
  if (length(hit)) return(lookup[[hit[1]]])
  if ("default" %in% names(lookup)) return(lookup[["default"]])
  return(NULL)
}

# --- sample sheet -------------------------------------------------------------

.cli_read_table <- function(path, what = "table") {
  if (!file.exists(path)) stop("No such ", what, ": ", path, call. = FALSE)
  ext <- tolower(tools::file_ext(path))
  df <- if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading ", path, " needs the readxl package", call. = FALSE)
    }
    as.data.frame(readxl::read_excel(path))
  } else if (ext == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!nrow(df)) stop(what, " is empty: ", path, call. = FALSE)
  return(df)
}

.cli_read_sample_sheet <- function(path, id_column = "prefix") {
  # Read sample sheet specific to this repo
  sheet <- .cli_read_table(path, "sample sheet")
  if (!id_column %in% colnames(sheet)) {
    stop("Sample sheet has no '", id_column, "' column. Found: ",
         paste(colnames(sheet), collapse = ", "), call. = FALSE)
  }
  .cli_check_reserved(sheet, id_column)
  sheet[[id_column]] <- trimws(as.character(sheet[[id_column]]))
  if (anyDuplicated(sheet[[id_column]])) {
    stop("Duplicate '", id_column, "' in sample sheet: ",
         paste(unique(sheet[[id_column]][duplicated(sheet[[id_column]])]),
               collapse = ", "), call. = FALSE)
  }
  return(sheet)
}

# Columns the CLIs write themselves. A sample sheet column of the same name
# would be silently renamed by bind_cols() to `area...7`, producing a file that
# violates the documented schema -- and the next stage then cannot find the
# column it needs.
.CLI_RESERVED_COLUMNS <- c("roi", "z", "area", "geometry", "sample",
                           "feature_id", "feature_type",
                           "parent_feature_id", "parent_feature_type",
                           "parent_containment", "parent_match",
                           "n_detected", "n_invalid", "n_failed", "n_roi")

#' Stop if the sample sheet would collide with a column the CLI writes
#'
#' @param sheet     the sample sheet
#' @param id_column the column holding the file prefix (never metadata)
#' @param extra     further column names this particular CLI writes, beyond the
#'                  shared reserved set -- feature_stat_cli.r computes its own
#'                  (area_med, ch1_signal, ...), and a sheet column of the same
#'                  name would otherwise be silently renamed to `area_med.x` by
#'                  the join instead of being rejected.
.cli_check_reserved <- function(sheet, id_column = "prefix", extra = character(0)) {
  meta <- setdiff(colnames(sheet), id_column)
  clash <- base::intersect(meta, unique(c(.CLI_RESERVED_COLUMNS, extra)))
  if (length(clash)) {
    stop("Sample sheet column(s) collide with columns the output already uses: ",
         paste(clash, collapse = ", "),
         "\n  rename them in the sheet (e.g. ", clash[1], " -> sample_", clash[1], ")",
         call. = FALSE)
  }
  return(invisible(NULL))
}

.cli_apply_sample_sheet <- function(contract_df, sheet, id_column = "prefix") {
  # The sheet filters the resolved files AND supplies metadata. It never
  # supplies paths -- that is --input's job.
  wanted <- sheet[[id_column]]
  keep <- contract_df$sample %in% wanted
  
  # The "sample" column is hardcoded here as the contract_df is a pipeline-internal sheet.
  unmatched_sheet <- setdiff(wanted, contract_df$sample)
  if (length(unmatched_sheet)) {
    warning(length(unmatched_sheet), " sample sheet row(s) matched no input file: ",
            paste(utils::head(unmatched_sheet, 5), collapse = ", "),
            if (length(unmatched_sheet) > 5) {", ..."} else {""}, call. = FALSE)
  }
  dropped <- unique(contract_df$sample[!keep])
  if (length(dropped)) {
    message("  sample sheet excluded ", length(dropped), " sample(s) found on disk: ",
            paste(utils::head(dropped, 5), collapse = ", "),
            if (length(dropped) > 5) {", ..."} else {""})
  }
  if (!any(keep)) {
    stop("The sample sheet and --input have no sample in common. ",
         "On disk: ", paste(utils::head(unique(contract_df$sample), 3), collapse = ", "),
         "; in sheet: ", paste(utils::head(wanted, 3), collapse = ", "), call. = FALSE)
  }
  return(contract_df[keep, , drop = FALSE])
}

# --- sourcing scripts/R/ ------------------------------------------------------

.cli_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (!length(file_arg)) return(NA_character_)
  return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)))
}

# Functions the CLIs call out of scripts/R/. Used as the "is it loaded?" test,
# so keep it in step with what the CLIs actually use.
.RLIB_REQUIRED <- c("read_fiji_result", "polygonize_roi_df", "define_feature_group",
                    "find_ROI_z_intersect", "assign_feature_parent",
                    "union_features", "plot_features_topView")

#' Source scripts/R/ into the global environment
#'
#' scripts/R/ is source()d, not installed, so pkg::fn does not work.
#'
#' @param dir        explicit path, from --rlib_path
#' @param script_dir directory of the calling CLI, resolved at SOURCE time
.source_rlib <- function(dir = NA, script_dir = NA) {
  # NB: test ALL the required functions, not one of them. A test file that
  #     pre-sources a couple of scripts/R files leaves a partial global
  #     environment, and a single-function guard reads that as "already
  #     loaded" -- so the CLI runs with the rest of the library missing and
  #     fails on whichever function it reaches first. Re-sourcing is cheap and
  #     idempotent; guessing is not.
  if (all(vapply(.RLIB_REQUIRED, exists, logical(1), mode = "function"))) {
    return(invisible(NULL))
  }
  if (length(dir) != 1L || is.na(dir)) {
    # script_dir is resolved by the caller at SOURCE time (see .THIS_DIR in each
    # CLI). Falling back to commandArgs() here is only right when Rscript ran
    # the CLI directly -- under a test it names the test runner instead.
    here <- if (length(script_dir) == 1L && !is.na(script_dir)) script_dir else .cli_script_dir()
    if (is.na(here)) {
      stop("Cannot locate scripts/R automatically; pass --rlib_path", call. = FALSE)
    }
    dir <- file.path(here, "..", "R")
  }
  if (!dir.exists(dir)) stop("scripts/R directory not found: ", dir, call. = FALSE)
  # NB: alphabetical. A top-level constant reading another file's constant would
  #     capture it before it exists -- keep such things functions.
  for (f in sort(list.files(dir, pattern = "[.][Rr]$", full.names = TRUE))) {
    sys.source(f, envir = globalenv())
  }
  return(invisible(NULL))
}

.cli_need <- function(pkgs) {
  # Fail with a clear message rather than a segfault or an obscure error deep in
  # a helper. sf in particular aborts the whole process on a bad install.
  absent <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) {
    stop("Missing required package(s): ", paste(absent, collapse = ", "),
         "\n  install.packages(c(", paste0('"', absent, '"', collapse = ", "), "))",
         call. = FALSE)
  }
  return(invisible(NULL))
}


#' Resolve --log_scale into the column names to draw on a log axis
#'
#' Shared by feature_scatter_cli.r and feature_stat_cli.r so the flag means the
#' same thing in both: names or globs, matched against the columns actually
#' present, nothing logged unless named.
#'
#' A pattern matching no column is a **warning**. Silently producing linear
#' panels looks exactly like a run where the flag worked, which is the failure
#' mode this repo keeps meeting.
#'
#' @param arg       raw argument value
#' @param available column names present in the table
#' @param what      flag name, for messages
#' @return character vector of patterns (possibly empty)
.cli_log_cols <- function(arg, available, what = "--log_scale") {
  pats <- .cli_resolve_arg(arg, what)
  if (is.null(pats) || !length(pats)) {
    return(character(0))
  }
  unused <- pats[!vapply(pats, function(pat) {
    any(vapply(available, log_axis_matcher(pat), logical(1)))
  }, logical(1))]
  if (length(unused)) {
    warning(what, " matched no column: ", paste(unused, collapse = ", "),
            "\n  available: ", paste(available, collapse = ", "), call. = FALSE)
  }
  hit <- Filter(log_axis_matcher(pats), available)
  if (length(hit)) {
    message("Log10 axis for: ", paste(hit, collapse = " "))
  }
  return(pats)
}


#' Parse --class tokens into an ordered spec
#'
#' Each token is `class:column=lo:hi`. Several tokens naming the same class are
#' ANDed, and the order the class names first appear is the priority order.
#'
#' The name is everything before the FIRST colon, which is unambiguous because a
#' column name cannot contain one while a range always does. A token with no
#' name -- `area_med=400:Inf` -- would otherwise parse as a class called
#' "area_med=400", so it is refused with the form spelled out.
#'
#' @param tokens raw argument value
#' @param what   flag name, for messages
#' @return ordered named list; each element a named list of `c(lo, hi)`
.cli_class_spec <- function(tokens, what = "--class") {
  v <- .cli_resolve_arg(tokens, what)
  if (is.null(v) || !length(v)) return(list())

  spec <- list()
  for (tok in v) {
    tok <- trimws(tok)
    i <- regexpr(":", tok, fixed = TRUE)
    if (i < 1L) {
      stop(what, " must be 'class:column=lo:hi', got: ", tok,
           "\n  e.g. 'growing:area_med=400:Inf'", call. = FALSE)
    }
    name <- trimws(substr(tok, 1L, i - 1L))
    rest <- trimws(substr(tok, i + 1L, nchar(tok)))
    if (!nzchar(name) || grepl("=", name, fixed = TRUE)) {
      stop(what, " is missing the class name before the first ':', got: ", tok,
           "\n  e.g. 'growing:area_med=400:Inf'", call. = FALSE)
    }
    if (!grepl("=", rest, fixed = TRUE)) {
      stop(what, " needs 'column=lo:hi' after the class name, got: ", tok,
           call. = FALSE)
    }
    rng <- .cli_key_ranges(rest, paste0(what, " (", name, ")"))
    for (col in names(rng)) {
      if (!is.null(spec[[name]]) && !is.null(spec[[name]][[col]])) {
        stop(what, " names ", col, " twice for class '", name,
             "': the second would silently replace the first", call. = FALSE)
      }
      if (is.null(spec[[name]])) spec[[name]] <- list()
      spec[[name]][[col]] <- rng[[col]]
    }
  }
  return(spec)
}
