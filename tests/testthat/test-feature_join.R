# run_id and the join it protects.
#
# The failure this exists to stop is a join that SUCCEEDS: feature_id is
# sequential per image, so two annotate runs over the same images share sample
# names and share nucleus_1, nucleus_2 ... Joining one run's per-feature table
# onto the other matches at ~100% and attaches every value to the wrong object.
# A match-rate check reads perfect in exactly that case, which is why the id
# has to exist at all.

.ann <- function(run_id = "aaaaaaaaaa", n = 3) {
  data.frame(
    sample     = "S1",
    feature_id = rep(paste0("nucleus_", seq_len(n)), each = 2),
    roi        = paste0("nucleus_", sprintf("%04d", seq_len(2 * n))),
    z          = rep(1:2, n),
    run_id     = run_id,
    stringsAsFactors = FALSE)
}

.stats <- function(run_id = "aaaaaaaaaa", n = 3, class = "big") {
  data.frame(
    sample     = "S1",
    feature_id = paste0("nucleus_", seq_len(n)),
    class      = class,
    area_med   = seq_len(n) * 100,
    run_id     = run_id,
    stringsAsFactors = FALSE)
}

test_that("the same parameters give the same run_id, different ones do not", {
  source_r_scripts("feature_join.r")
  a <- run_id_from(c("cli", "max_z_dist=3", "x.txt:100"))
  expect_identical(a, run_id_from(c("cli", "max_z_dist=3", "x.txt:100")))
  expect_false(identical(a, run_id_from(c("cli", "max_z_dist=2", "x.txt:100"))))
  # A different input file must change it even at identical settings.
  expect_false(identical(a, run_id_from(c("cli", "max_z_dist=3", "y.txt:100"))))
  expect_identical(nchar(a), 10L)
})

test_that("the input fingerprint is order-independent and notices a size change", {
  source_r_scripts("feature_join.r")
  d <- withr::local_tempdir()
  f1 <- file.path(d, "a_nucleus_outline.txt"); writeLines("aaa", f1)
  f2 <- file.path(d, "b_nucleus_outline.txt"); writeLines("bbbbbb", f2)

  expect_identical(run_input_fingerprint(c(f1, f2)),
                   run_input_fingerprint(c(f2, f1)))
  before <- run_input_fingerprint(f1)
  writeLines("aaaaaaaaaaaa", f1)
  expect_false(identical(before, run_input_fingerprint(f1)))
})

test_that("a join across two runs is refused, naming both ids", {
  source_r_scripts("feature_join.r")
  expect_error(join_feature_table(.ann("aaaaaaaaaa"), .stats("bbbbbbbbbb")),
               "different annotate run")
  expect_error(join_feature_table(.ann("aaaaaaaaaa"), .stats("bbbbbbbbbb")),
               "aaaaaaaaaa")
  expect_error(join_feature_table(.ann("aaaaaaaaaa"), .stats("bbbbbbbbbb")),
               "bbbbbbbbbb")
})

test_that("the refusal is what --force overrides, and it still warns", {
  source_r_scripts("feature_join.r")
  expect_warning(out <- join_feature_table(.ann("aaaaaaaaaa"), .stats("bbbbbbbbbb"),
                                           force = TRUE),
                 "different annotate run")
  out <- suppressWarnings(join_feature_table(.ann("aaaaaaaaaa"), .stats("bbbbbbbbbb"),
                                             force = TRUE))
  expect_true("class" %in% colnames(out))
})

test_that("matching ids join silently", {
  # Without this the test above could pass on a join that always refuses.
  source_r_scripts("feature_join.r")
  expect_no_warning(out <- join_feature_table(.ann(), .stats()))
  expect_identical(nrow(out), 6L)
  expect_true(all(out$class == "big"))
  expect_identical(attr(out, "n_matched"), 3L)
})

test_that("a table with no run_id joins, but says the check could not be made", {
  source_r_scripts("feature_join.r")
  old <- .stats(); old$run_id <- NULL
  expect_warning(join_feature_table(.ann(), old), "cannot be checked")
})

test_that("a duplicated key is refused rather than multiplying rows", {
  # It would silently reweight every count taken afterwards.
  source_r_scripts("feature_join.r")
  dup <- rbind(.stats(), .stats()[1, ])
  expect_error(join_feature_table(.ann(), dup), "duplicate")
})

test_that("a partial join warns, and `expect` says which rows should have matched", {
  source_r_scripts("feature_join.r")
  a <- .ann(n = 3)
  s <- .stats(n = 2)                       # nucleus_3 is absent

  expect_warning(join_feature_table(a, s), "2 of 3 feature")

  # Marking nucleus_3 as not-expected silences it. That is the invalid/failed
  # case: a per-feature table never holds those, so without this the run would
  # warn every single time and the warning would stop meaning anything.
  ok <- a$feature_id != "nucleus_3"
  expect_no_warning(join_feature_table(a, s, expect = ok))
})

test_that("join_feature_table names the missing key rather than failing inside dplyr", {
  source_r_scripts("feature_join.r")
  a <- .ann(); a$feature_id <- NULL
  expect_error(join_feature_table(a, .stats()), "feature_id")
})

test_that("annotate writes a run_id that survives into the stats table", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  ann <- suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  expect_true("run_id" %in% colnames(ann))
  rid <- unique(ann$run_id)
  expect_identical(length(rid), 1L)
  expect_identical(nchar(rid), 10L)

  out <- withr::local_tempdir()
  st <- suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", out, "--res_dir", fixture_dir(), "--no_plot")))
  # Carried through, so the join downstream has something to check against.
  expect_identical(unique(st$run_id), rid)
})
