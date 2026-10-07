# feature_contrast_cli.r (analysis-oo_count-physical_blur).
#
# The features are a REAL annotate output (the fixture: 6 counted nuclei, 2
# invalid); the contrast table is written by hand so that every rule has a case
# that only the right implementation passes:
#   bright   -> kept on its signal ratio
#   hole     -> low signal, but a DAPI hole: kept
#   dim      -> low signal, no hole: dropped (renamed invalid_contrast_*)
#   weighted -> its small ROIs are bright and its big ones are not; the
#               UNweighted mean of ratios passes, the area-weighted one fails
#   the two invalid_ nuclei are never evaluated and keep their names

source_cli("cli_helpers.r")

.fc_fixture <- function(env = parent.frame()) {
  source_cli("annotate_features_cli.r")
  out <- withr::local_tempdir(.local_envir = env)
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", out,
    "--max_z_dist", "default=3", "--min_z_span", "default=5")))
  out
}

# One contrast table for the fixture's ROIs: signal and hole ratios chosen per
# feature, areas taken from the features' own ROIs.
.fc_contrast <- function(feat_dir, dir) {
  f <- list.files(feat_dir, "_features[.]rds$", full.names = TRUE)
  x <- sf::st_drop_geometry(readRDS(f[1]))
  smp <- unique(x$sample)
  counted <- sort(unique(x$feature_id[!startsWith(x$feature_id, "invalid_") &
                                       !startsWith(x$feature_id, "failed_")]))
  role <- setNames(rep("bright", length(counted)), counted)
  role[counted[1]] <- "hole"; role[counted[2]] <- "dim"; role[counted[3]] <- "weighted"
  rows <- list()
  for (i in seq_len(nrow(x))) {
    r <- x[i, ]
    rl <- if (r$feature_id %in% counted) role[[r$feature_id]] else "bright"
    area <- if (rl == "weighted") {
      # largest ROIs of this feature get the low ratio
      a <- x$area[x$feature_id == r$feature_id]
      if (r$area >= stats::median(a)) 1000L else 10L
    } else 100L
    sig <- switch(rl, bright = 5, hole = 1.2, dim = 1.2,
                  weighted = if (area == 1000L) 1.0 else 9.0)
    hol <- switch(rl, hole = 0.3, 1.0)
    rows[[length(rows) + 1L]] <- data.frame(
      name = smp, roi = r$roi, z = r$z, ch = c(1L, 2L), area_px = area,
      inside_mean = c(hol * 40, sig * 40), ring_area_px = 500L, ring_mean = 40)
  }
  d <- do.call(rbind, rows)
  path <- file.path(dir, paste0(smp, "_nucleus_contrast.txt"))
  utils::write.table(d, path, sep = "\t", quote = FALSE, row.names = FALSE)
  list(path = path, role = role, sample = smp, feat_file = f[1], x = x)
}

test_that("feature_contrast_cli keeps, rescues and drops by the declared rule", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "dplyr"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("feature_contrast_cli.r")

  feat_dir <- .fc_fixture()
  cdir <- withr::local_tempdir()
  fx <- .fc_contrast(feat_dir, cdir)
  out <- withr::local_tempdir()
  res <- suppressMessages(feature_contrast_cli(c(
    "--features", feat_dir, "--contrast", cdir, "--outdir", out)))

  role <- fx$role
  got <- setNames(res$keep, res$feature_id)
  expect_identical(nrow(res), length(role))                      # every counted feature, once
  expect_true(all(got[names(role)[role == "bright"]]))
  expect_true(got[[names(role)[role == "hole"]]])                 # rescued by the hole rule
  expect_false(got[[names(role)[role == "dim"]]])
  expect_false(got[[names(role)[role == "weighted"]]])            # area weighting decides it

  # The weighted case really is decided by the weighting: the plain mean of its
  # ROI ratios is above the cut-off.
  cd <- utils::read.delim(fx$path)
  w_rois <- fx$x$roi[fx$x$feature_id == names(role)[role == "weighted"]]
  plain <- mean(cd$inside_mean[cd$ch == 2 & cd$roi %in% w_rois] / 40)
  expect_gt(plain, 2.5)
  expect_lt(res$signal_ratio[res$feature_id == names(role)[role == "weighted"]], 2.5)

  # The filtered table: dropped features renamed, everything else untouched.
  y <- readRDS(file.path(out, basename(fx$feat_file)))
  expect_identical(nrow(y), nrow(fx$x))
  dropped <- names(role)[role %in% c("dim", "weighted")]
  expect_setequal(unique(y$feature_id[startsWith(y$feature_id, "invalid_contrast_")]),
                  paste0("invalid_contrast_", dropped))
  inv_before <- unique(fx$x$feature_id[startsWith(fx$x$feature_id, "invalid_")])
  expect_true(length(inv_before) > 0)
  expect_true(all(inv_before %in% y$feature_id))                 # never evaluated, never renamed
  kept <- names(role)[!role %in% c("dim", "weighted")]
  expect_true(all(kept %in% y$feature_id))
  expect_false(identical(unique(y$run_id), unique(fx$x$run_id))) # a new run
  expect_true(file.exists(file.path(out, "feature_contrast.tsv")))
})

test_that("feature_contrast_cli refuses to filter against the wrong contrast table", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "dplyr"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("feature_contrast_cli.r")

  feat_dir <- .fc_fixture()
  cdir <- withr::local_tempdir()
  fx <- .fc_contrast(feat_dir, cdir)
  # Drop one COUNTED ROI's rows: a table from another segmentation looks like
  # this. (An invalid feature's ROI would not do -- those are never looked up,
  # rightly, and the first version of this test picked one and saw no error.)
  d <- utils::read.delim(fx$path)
  gone <- fx$x$roi[fx$x$feature_id == names(fx$role)[fx$role == "bright"][1]][1]
  utils::write.table(d[d$roi != gone, ], fx$path, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(suppressMessages(feature_contrast_cli(c(
    "--features", feat_dir, "--contrast", cdir, "--outdir", withr::local_tempdir()))),
    "different segmentation")
})

test_that("feature_contrast_cli will not write over the unfiltered features", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "dplyr"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("feature_contrast_cli.r")

  feat_dir <- .fc_fixture()
  cdir <- withr::local_tempdir()
  .fc_contrast(feat_dir, cdir)
  expect_error(suppressMessages(feature_contrast_cli(c(
    "--features", feat_dir, "--contrast", cdir, "--outdir", feat_dir))),
    "would replace the unfiltered tables")
})

test_that("feature_contrast_cli runs as a script, with its own narrow sourcing", {
  # The suite pre-sources everything, so only a subprocess shows a missing
  # library file -- as count_features_cli.r's test explains.
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "dplyr"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if_not(file.exists(rscript), "Rscript not found")

  feat_dir <- .fc_fixture()
  cdir <- withr::local_tempdir()
  .fc_contrast(feat_dir, cdir)
  out <- withr::local_tempdir()
  txt <- suppressWarnings(system2(rscript, c(cli_path("feature_contrast_cli.r"),
    "--features", feat_dir, "--contrast", cdir, "--outdir", out), stdout = TRUE, stderr = TRUE))
  expect_true(file.exists(file.path(out, "feature_contrast.tsv")))
  expect_true(any(grepl("kept,", txt)))
})
