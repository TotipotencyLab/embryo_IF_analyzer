# count_features_cli.r and montage_qc_cli.r.
#
# The counting test asserts that invalid groups are REPORTED rather than
# dropped: a sample whose nuclei mostly failed the z-span filter must not look
# like a sample that genuinely has few nuclei.

source_cli("cli_helpers.r")

annotated_fixture <- function(env = parent.frame()){
  # Produce a real annotate output to count/montage from, once per test.
  source_cli("annotate_features_cli.r")
  out <- withr::local_tempdir(.local_envir = env)
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(),
    "--feature", "nucleus", "nucleolus",
    "--outdir", out,
    "--max_z_dist", "default=3", "nucleolus=1",
    "--min_z_span", "default=5", "nucleolus=2")))
  out
}

# --- counting -----------------------------------------------------------------

test_that("count_features_cli counts detected and invalid features separately", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("count_features_cli.r")

  feat_dir <- annotated_fixture()
  out <- withr::local_tempdir()
  res <- suppressMessages(count_features_cli(c("--input", feat_dir, "--outdir", out)))

  expect_true(file.exists(file.path(out, "feature_counts.tsv")))
  expect_identical(nrow(res), 2L)                    # one row per feature type

  nuc <- res[res$feature_type == "nucleus", ]
  expect_identical(nuc$n_detected, 6L)
  expect_identical(nuc$n_roi, 70L)
  # The two nuclei that failed min_z_span are reported, not silently discarded.
  expect_identical(nuc$n_invalid, 2L)

  nol <- res[res$feature_type == "nucleolus", ]
  expect_identical(nol$n_detected, 7L)
  expect_identical(nol$n_roi, 27L)
})

test_that("count_features_cli joins metadata and groups by it", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("count_features_cli.r")

  feat_dir <- annotated_fixture()
  d <- withr::local_tempdir()
  sheet <- file.path(d, "samples.tsv")
  write.table(data.frame(prefix = "GRV_Position010", genotype = "wt"),
              sheet, sep = "\t", quote = FALSE, row.names = FALSE)

  out <- withr::local_tempdir()
  res <- suppressMessages(count_features_cli(c(
    "--input", feat_dir, "--outdir", out,
    "--sample_sheet", sheet, "--group_by", "genotype")))

  expect_true("genotype" %in% colnames(res))
  expect_true(file.exists(file.path(out, "feature_counts_summary.tsv")))
})

test_that("count_features_cli refuses a --group_by column that is not there", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("count_features_cli.r")

  feat_dir <- annotated_fixture()
  expect_error(suppressMessages(count_features_cli(c(
    "--input", feat_dir, "--outdir", withr::local_tempdir(),
    "--group_by", "genotype"))),
    "not found")
})

test_that("count_features_cli rejects a file it did not write", {
  skip_if_no_pkg("argparser")
  source_cli("count_features_cli.r")
  d <- withr::local_tempdir()
  saveRDS(data.frame(a = 1), file.path(d, "X_features.rds"))
  expect_error(suppressMessages(count_features_cli(c(
    "--input", d, "--outdir", withr::local_tempdir()))),
    "missing column")
})

# --- montage ------------------------------------------------------------------

test_that("montage_qc_cli composes one panel per input plus the R panel", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("montage_qc_cli.r")

  feat_dir <- annotated_fixture()
  rds <- file.path(feat_dir, "GRV_Position010_features.rds")

  d <- withr::local_tempdir()
  for (f in c("proj.png", "overlay.png")) {
    magick::image_write(magick::image_blank(300, 300, color = "gray30"), file.path(d, f))
  }

  out <- file.path(d, "montage.png")
  suppressWarnings(suppressMessages(montage_qc_cli(c(
    "--features", rds, "--feature", "nucleus",
    "--projection", file.path(d, "proj.png"),
    "--overlay", file.path(d, "overlay.png"),
    "--output", out, "--panel_height", "200"))))

  expect_true(file.exists(out))
  info <- magick::image_info(magick::image_read(out))
  expect_identical(info$height, 200L)
  expect_gt(info$width, 400L)             # three panels side by side
})

test_that("montage_qc_cli warns loudly when the image extent is unknown", {
  # Without image_width/image_height the R panel is cropped to the features'
  # bounding box and does NOT align with the Fiji panels. Producing that
  # silently would defeat the purpose of putting them side by side.
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("montage_qc_cli.r")

  feat_dir <- annotated_fixture()   # no _config.txt is copied here
  rds <- file.path(feat_dir, "GRV_Position010_features.rds")
  out <- file.path(withr::local_tempdir(), "m.png")

  expect_warning(suppressMessages(montage_qc_cli(c(
    "--features", rds, "--feature", "nucleus", "--output", out))),
    "will NOT align")
})

test_that("montage_qc_cli reads the image extent from a Fiji config", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("montage_qc_cli.r")

  feat_dir <- annotated_fixture()
  rds <- file.path(feat_dir, "GRV_Position010_features.rds")

  # Two stand-in panels, so the "only one panel" warning cannot be what fires.
  d <- withr::local_tempdir()
  for (f in c("proj.png", "overlay.png")) {
    magick::image_write(magick::image_blank(300, 300, color = "gray30"), file.path(d, f))
  }

  cfg <- file.path(feat_dir, "GRV_Position010_config.txt")
  write.table(data.frame(
    parameter = c("image_width", "image_height", "pixel_width", "pixel_height"),
    value = c("1024", "1024", "0.14147068967810042", "0.14147068967810042")),
    cfg, sep = "\t", quote = FALSE, row.names = FALSE)

  out <- file.path(withr::local_tempdir(), "m.png")
  # No "will NOT align" warning this time -- the extent is known.
  expect_no_warning(suppressMessages(montage_qc_cli(c(
    "--features", rds, "--feature", "nucleus", "--output", out,
    "--projection", file.path(d, "proj.png"),
    "--overlay", file.path(d, "overlay.png")))))
  expect_true(file.exists(out))
})

test_that(".image_extent converts pixels to calibrated units", {
  d <- withr::local_tempdir()
  cfg <- file.path(d, "c.txt")
  write.table(data.frame(parameter = c("image_width", "image_height",
                                       "pixel_width", "pixel_height"),
                         value = c("1024", "512", "0.5", "0.25")),
              cfg, sep = "\t", quote = FALSE, row.names = FALSE)
  source_cli("montage_qc_cli.r")
  got <- .image_extent(cfg)
  expect_equal(got$xmax, 512)
  expect_equal(got$ymax, 128)
})
