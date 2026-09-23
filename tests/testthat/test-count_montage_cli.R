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

test_that("--feature_class_by composes a label, keeping the components", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")
  source_cli("count_features_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))

  st <- withr::local_tempdir()
  suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", st, "--res_dir", fixture_dir(), "--no_plot",
    "--class", "big:area_med=120:Inf", "small:area_med=0:120")))

  out <- withr::local_tempdir()
  got <- suppressMessages(count_features_cli(c(
    "--input", feat, "--outdir", out,
    "--feature_table", file.path(st, "feature_stats.tsv"),
    "--feature_class_by", "class", "feature_type")))

  # Composite AND components, so nothing downstream has to split the label.
  expect_true(all(c("feature_class", "class", "feature_type") %in% colnames(got)))
  expect_true(all(got$feature_class == paste(got$class, got$feature_type, sep = "-") |
                    grepl("^unclassified-", got$feature_class)))
  # The fate accounting survives, which is the reason the annotation stays the
  # spine rather than counting from the stats table.
  expect_true(all(c("n_detected", "n_invalid", "n_failed", "n_roi") %in% colnames(got)))
  expect_gt(sum(got$n_invalid), 0)
})

test_that("counting without --feature_table is unchanged", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("count_features_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  out <- withr::local_tempdir()
  got <- suppressMessages(count_features_cli(c("--input", feat, "--outdir", out)))

  expect_identical(nrow(got), 1L)
  expect_identical(got$feature_class, "nucleus")   # default is feature_type
  expect_identical(got$n_detected, 6L)
})

test_that("--feature_class_by naming a stats column without --feature_table says so", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("count_features_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  out <- withr::local_tempdir()
  expect_error(
    suppressMessages(count_features_cli(c("--input", feat, "--outdir", out,
                                          "--feature_class_by", "class"))),
    "needs --feature_table")
})

test_that("a separator already inside a class value is a warning, not a silent label", {
  source_r_scripts("feature_join.r")
  d <- data.frame(class = c("growing-oocyte", "small"), feature_type = "nucleus",
                  stringsAsFactors = FALSE)
  expect_warning(compose_feature_class(d, c("class", "feature_type"), "-"),
                 "already contain the separator")
  # Another separator makes it unambiguous again, and silent.
  expect_no_warning(lab <- compose_feature_class(d, c("class", "feature_type"), "|"))
  expect_identical(lab, c("growing-oocyte|nucleus", "small|nucleus"))
})

test_that("an absent class becomes (unclassified), never 'NA'", {
  source_r_scripts("feature_join.r")
  d <- data.frame(class = c("big", NA, ""), feature_type = "nucleus",
                  stringsAsFactors = FALSE)
  expect_identical(compose_feature_class(d, "class", "-"),
                   c("big", "unclassified", "unclassified"))
})

test_that("class_palette never leaves a level for ggplot to grey out silently", {
  # Measured on ggplot2 4.0.3: a level absent from scale_*_manual(values=) is
  # drawn in na.value grey AND dropped from the legend, so a real class
  # disappears from the figure's account of itself. The fix is to recode, not
  # to rely on na.value. See .claude/skills/r-ggplot.
  source_r_scripts("plot_outline_topView.r")
  v <- c("growing", "small", "debris", NA)

  pal <- class_palette(v, c(growing = "red"))
  expect_identical(levels(pal$values), c("other", "growing"))
  # Every level has a colour. Nothing falls through.
  expect_setequal(names(pal$palette), levels(pal$values))
  expect_false(any(is.na(pal$palette)))
  expect_identical(unname(pal$palette[["growing"]]), "red")
  # And what got folded in is recoverable, for the caption.
  expect_setequal(pal$other_members, c("small", "debris", "unclassified"))
})

test_that("other is the FIRST level, so it draws underneath", {
  # Draw order is level order; greys added last would cover what is being
  # inspected, and nothing about the code would look wrong.
  source_r_scripts("plot_outline_topView.r")
  pal <- class_palette(c("a", "b"), c(a = "red"))
  expect_identical(levels(pal$values)[1], "other")
})

test_that("no --color_map leaves the classes alone", {
  source_r_scripts("plot_outline_topView.r")
  pal <- class_palette(c("growing", "small", NA))
  expect_null(pal$palette)
  expect_setequal(levels(pal$values), c("growing", "small", "unclassified"))
  expect_length(pal$other_members, 0)
})

test_that("a colour the device cannot use is refused before ggplot sees it", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("montage_qc_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  out <- file.path(withr::local_tempdir(), "m.png")

  expect_error(
    suppressMessages(montage_qc_cli(c(
      "--features", file.path(feat, "GRV_Position010_features.rds"),
      "--output", out, "--color_map", "nucleus=chartreuseX"))),
    "unusable colour")
})

test_that("the montage labels outlines by class and names the rest in the caption", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")
  source_cli("montage_qc_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  st <- withr::local_tempdir()
  suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", st, "--res_dir", fixture_dir(), "--no_plot",
    "--class", "big:area_med=140:Inf", "small:area_med=0:140")))

  out <- file.path(withr::local_tempdir(), "m.png")
  expect_no_error(suppressMessages(suppressWarnings(montage_qc_cli(c(
    "--features", file.path(feat, "GRV_Position010_features.rds"),
    "--output", out,
    "--feature_table", file.path(st, "feature_stats.tsv"),
    "--feature_class_by", "class",
    "--color_map", "big=red")))))
  expect_true(file.exists(out))
})

test_that("--color_map can set the greys, which are not in the data", {
  # "other" is a group class_palette() invents, so matching it against the
  # data's values would drop the colour that was asked for.
  source_r_scripts(c("classify_features.r", "plot_outline_topView.r"))
  v <- c("growing", "small", "debris", "unclassified")

  pal <- class_palette(v, c(growing = "red", other = "black", unclassified = "grey90"))
  expect_identical(unname(pal$palette[["other"]]), "black")
  expect_identical(unname(pal$palette[["unclassified"]]), "grey90")
  # Named explicitly, unclassified keeps its own level instead of folding in.
  expect_true("unclassified" %in% levels(pal$values))
  expect_false("unclassified" %in% pal$other_members)
  # Background first, inspected classes last.
  expect_identical(levels(pal$values), c("other", "unclassified", "growing"))
})

test_that("unclassified folds into other unless it is named", {
  source_r_scripts(c("classify_features.r", "plot_outline_topView.r"))
  pal <- class_palette(c("growing", "unclassified"), c(growing = "red"))
  expect_false("unclassified" %in% levels(pal$values))
  expect_true("unclassified" %in% pal$other_members)
})

test_that("the default other is dark enough to separate from the ROI outlines", {
  # The per-ROI features underneath are drawn grey80; an 'other' at grey75 was
  # indistinguishable from them.
  source_r_scripts(c("classify_features.r", "plot_outline_topView.r"))
  pal <- class_palette(c("a", "b"), c(a = "red"))
  expect_identical(unname(pal$palette[["other"]]), "grey30")
})
