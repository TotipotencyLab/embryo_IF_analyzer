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

#' Extract setting per feature
#' Look a per-feature parameter up: the feature's own value, else "default",
#' else the built-in default. Returns a length-1 numeric.
#' @param lookup  key-value setting
#' @param feature value within the parameter lookup
#' @param default the default value when feature is not part of the lookup
#' @param what    typically a flag name, for reporting when value is in the wrong format only
.cli_param_for <- function(lookup, feature, default, what = "parameter") {
  if (!length(lookup)) return(default)
  v <- if (feature %in% names(lookup)) lookup[[feature]]
       else if ("default" %in% names(lookup)) lookup[["default"]]
       else return(default)
  n <- suppressWarnings(as.numeric(v))
  if (is.na(n)) {
    stop("Value for ", what, " (", feature, ") is not a number: ", v, call. = FALSE)
  }
  return(n)
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
  rx <- paste0("^(.*)_(", paste(features, collapse = "|"), ")_outline\\.txt$")
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
  sheet[[id_column]] <- trimws(as.character(sheet[[id_column]]))
  if (anyDuplicated(sheet[[id_column]])) {
    stop("Duplicate '", id_column, "' in sample sheet: ",
         paste(unique(sheet[[id_column]][duplicated(sheet[[id_column]])]),
               collapse = ", "), call. = FALSE)
  }
  return(sheet)
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

.source_rlib <- function(dir = NA, script_dir = NA) {
  # scripts/R/ is source()d, not installed, so pkg::fn does not work.
  # Tests pre-source it; do not source twice.
  if (exists("polygonize_roi_df", mode = "function")) return(invisible(NULL))
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
