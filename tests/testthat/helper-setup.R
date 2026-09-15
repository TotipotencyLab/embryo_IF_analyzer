# Shared setup for the R test suite.
#
# The repo is a collection of scripts, not a package, so the functions under
# test are source()d rather than loaded from a namespace.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(stringr)
})

repo_root <- function(){
  # testthat runs with the working directory set to tests/testthat/
  p <- normalizePath(file.path(getwd(), "..", ".."), mustWork = FALSE)
  if(file.exists(file.path(p, "scripts", "R"))) return(p)
  normalizePath(getwd(), mustWork = FALSE)
}

source_r_scripts <- function(files){
  for(f in files) source(file.path(repo_root(), "scripts", "R", f))
}

fixture_dir <- function(){
  file.path(repo_root(), "fixture", "if_data", "data")
}

fixture_file <- function(feature, kind){
  file.path(fixture_dir(), sprintf("GRV_Position010_%s_%s.txt", feature, kind))
}

skip_if_no_fixture <- function(path){
  if(!file.exists(path)){
    testthat::skip(paste0("fixture not present: ", basename(path)))
  }
}

# `sf` is loaded in a separate process on purpose ------------------------------
# A broken units/udunits install makes library(sf) abort the whole R process with
# a segfault rather than raise a catchable error, which would take the test run
# down with it. Probe it once in a child process and cache the answer, so the
# non-spatial tests still run on a machine where sf is unusable.
.sf_usable <- local({
  cached <- NULL
  function(){
    if(!is.null(cached)) return(cached)
    ok <- FALSE
    if(requireNamespace("callr", quietly = TRUE) || TRUE){
      rscript <- file.path(R.home("bin"), "Rscript")
      status <- suppressWarnings(system2(rscript,
                                         c("-e", shQuote("suppressPackageStartupMessages(library(sf))")),
                                         stdout = FALSE, stderr = FALSE))
      ok <- identical(status, 0L)
    }
    cached <<- ok
    ok
  }
})

skip_if_no_sf <- function(){
  if(!.sf_usable()){
    testthat::skip("sf cannot be loaded in this R installation (units/udunits segfault)")
  }
  suppressPackageStartupMessages({library(sp); library(sf)})
}

# Build a Fiji-style measurement label, so tests can vary one part at a time.
make_label <- function(roi = "nucleus_0001-0001-0433",
                       image = "20241216_dkD.lif-Position010-1.tif",
                       ch = 1, n_ch = 4, z = 1, n_z = 50,
                       tail = "Lightning 001/Mark_and_Find 001/Position010"){
  sprintf("%s:%s:c:%d/%d z:%d/%d - %s", image, roi, ch, n_ch, z, n_z, tail)
}

# Minimal measurement table in the shape Fiji's "Results" export has.
write_res_table <- function(path, labels, area = 100){
  df <- data.frame(
    X = seq_along(labels), Label = labels, Area = area, Mean = 50, StdDev = 10,
    Min = 0, Max = 255, X.1 = 1, Y = 1, Circ. = 0.9, IntDen = 1, Median = 1,
    RawIntDen = 1, Ch = 1, Slice = 1, AR = 1, Round = 1, Solidity = 1,
    check.names = FALSE, stringsAsFactors = FALSE)
  colnames(df)[1] <- " "
  colnames(df)[colnames(df) == "X.1"] <- "X"
  write.table(df, path, sep = "\t", quote = FALSE, row.names = FALSE)
  path
}
