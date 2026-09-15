#!/usr/bin/env Rscript
# Run the R test suite.
#
#   Rscript tests/run_tests.R
#
# The spatial tests need a working sf. If library(sf) cannot be loaded they are
# skipped rather than failed, and the reason is printed -- see the note on the
# R 4.4 units binary in CLAUDE.local.md. Run under an R where sf works to
# exercise them.

suppressPackageStartupMessages(library(testthat))

cat("R:      ", R.version.string, "\n")
cat("R home: ", R.home(), "\n")

# Locate tests/testthat whether this is run from the repo root or from tests/.
script_path <- sub("^--file=", "",
                   grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
base_dir <- if(length(script_path) == 1) dirname(normalizePath(script_path)) else getwd()

test_root <- file.path(base_dir, "testthat")
if(!dir.exists(test_root)) test_root <- file.path(getwd(), "tests", "testthat")
if(!dir.exists(test_root)) stop("cannot locate tests/testthat from ", getwd())

res <- test_dir(test_root, reporter = "summary", stop_on_failure = FALSE)
df <- as.data.frame(res)
cat(sprintf("\nfailed: %d | skipped: %d | passed: %d\n",
            sum(df$failed), sum(df$skipped), sum(df$passed)))
if(sum(df$failed) > 0 || sum(df$error) > 0) quit(status = 1)
