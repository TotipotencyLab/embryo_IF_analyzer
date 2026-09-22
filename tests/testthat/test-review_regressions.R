# Regressions from the code review of the R CLI work.
#
# Every test here failed before its fix. They are kept together because they
# share one shape: the code produced a plausible answer rather than an error,
# and the fixture did not happen to contain the triggering case.

source_cli("cli_helpers.r")

sq <- function(x0, y0, s = 10){
  sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0),
                            c(y0, y0, y0 + s, y0 + s, y0))))
}

# --- 1. containment denominator ------------------------------------------------

test_that("containment is measured on unioned slices, not summed ROIs", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # One nucleolus represented by two OVERLAPPING ROIs on the same slice, wholly
  # inside the nucleus. Summing the two ROI areas double-counts the overlap, so
  # the denominator exceeded anything the numerator could reach and a fully
  # contained child scored 0.7 -- close enough to the 0.5 default to look fine,
  # and low enough to be rejected at a stricter threshold.
  x <- sf::st_sf(
    feature_id   = c("nucleus_1", "nucleolus_1", "nucleolus_1"),
    feature_type = c("nucleus", "nucleolus", "nucleolus"),
    z            = c(5, 5, 5),
    sample       = "S1",
    geometry     = sf::st_sfc(sq(0, 0, 100), sq(40, 40, 10), sq(44, 40, 10)))

  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))
  d <- sf::st_drop_geometry(rel)
  expect_equal(unique(d$parent_containment[d$feature_id == "nucleolus_1"]), 1,
               tolerance = 1e-9)

  # ...and it is still rejected at a threshold it genuinely cannot meet.
  half <- sf::st_sf(
    feature_id   = c("nucleus_1", "nucleolus_1"),
    feature_type = c("nucleus", "nucleolus"),
    z            = c(5, 5),
    sample       = "S1",
    geometry     = sf::st_sfc(sq(0, 0, 100), sq(95, 40, 10)))
  d2 <- sf::st_drop_geometry(
    assign_feature_parent(half, within = c(nucleolus = "nucleus"), min_containment = 0.6))
  expect_true(is.na(unique(d2$parent_feature_id[d2$feature_id == "nucleolus_1"])))
})

test_that("assign_feature_parent survives a zero-row table", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")
  empty <- sf::st_sf(feature_id = character(0), feature_type = character(0),
                     z = integer(0), geometry = sf::st_sfc())
  out <- suppressWarnings(assign_feature_parent(empty, within = c(nucleolus = "nucleus")))
  expect_identical(nrow(out), 0L)
  expect_true(all(c("parent_feature_id", "parent_containment") %in% colnames(out)))
})

# --- 2. the count plot ---------------------------------------------------------

test_that("the count plot draws bars only when not grouping", {
  skip_if_no_pkg(c("argparser", "ggplot2"))
  suppressPackageStartupMessages(library(ggplot2))
  source_cli("count_features_cli.r")

  tidy <- data.frame(sample = c("A", "B"), feature_type = "nucleus",
                     n_detected = c(3L, 5L), genotype = c("wt", "ko"),
                     stringsAsFactors = FALSE)
  geoms_of <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))

  ungrouped <- geoms_of(.count_plot_build(tidy, character(0)))
  grouped   <- geoms_of(.count_plot_build(tidy, "genotype"))

  expect_true("GeomCol" %in% ungrouped)
  # The bug: `data = NULL` means "inherit the plot data", not "skip this
  # layer", so GeomCol appeared here too -- one bar per sample, overplotted
  # under the boxplot.
  expect_false("GeomCol" %in% grouped)
  expect_true("GeomBoxplot" %in% grouped)
  expect_false("GeomBoxplot" %in% ungrouped)
})

# --- 3. sample sheet column collisions ----------------------------------------

test_that("a sample sheet column that collides with an output column is refused", {
  d <- withr::local_tempdir()
  p <- file.path(d, "s.tsv")

  # bind_cols() silently renamed these to area...7 / feature_type...8, producing
  # a file that violates the documented schema and that count_features_cli then
  # cannot read.
  write.table(data.frame(prefix = "GRV_Position010", area = "BIG"),
              p, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(.cli_read_sample_sheet(p), "collide")

  write.table(data.frame(prefix = "X", feature_type = "a", sample = "b"),
              p, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(.cli_read_sample_sheet(p), "feature_type")
})

test_that("the id column itself is not treated as a collision", {
  d <- withr::local_tempdir()
  p <- file.path(d, "s.tsv")
  # --id_column sample is legitimate: it is the key, not metadata.
  write.table(data.frame(sample = c("A", "B"), genotype = c("wt", "ko")),
              p, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_identical(nrow(.cli_read_sample_sheet(p, id_column = "sample")), 2L)
})

test_that("the shipped template has no reserved column names", {
  tmpl <- file.path(repo_root(), "config", "sample_sheet_template.tsv")
  expect_no_error(.cli_read_sample_sheet(tmpl))
})

# --- 4. the montage extent -----------------------------------------------------

test_that("a config without pixel size does not yield a pixel-space extent", {
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  source_cli("montage_qc_cli.r")

  d <- withr::local_tempdir()
  cfg <- file.path(d, "c.txt")
  write.table(data.frame(parameter = c("image_width", "image_height"),
                         value = c("1024", "1024")),
              cfg, sep = "\t", quote = FALSE, row.names = FALSE)

  # Falling back to pixel_width = 1 returned a 1024-unit frame for geometry
  # measured in microns (~145 units), drawing the features into one corner --
  # misaligned, and silently.
  expect_warning(.image_extent(cfg), "pixel_width")
  expect_null(suppressWarnings(.image_extent(cfg)))

  # With the pixel size present it still works.
  write.table(data.frame(parameter = c("image_width", "image_height",
                                       "pixel_width", "pixel_height"),
                         value = c("1024", "1024", "0.5", "0.5")),
              cfg, sep = "\t", quote = FALSE, row.names = FALSE)
  got <- .image_extent(cfg)
  expect_equal(got$xmax, 512)
})

# --- 5. library loading --------------------------------------------------------

test_that("a partially loaded scripts/R is detected, not mistaken for loaded", {
  # .source_rlib() used to test for one function. A caller that had sourced only
  # part of scripts/R left a partial global environment that the guard read as
  # "loaded", so the CLI ran without the rest of the library.
  expect_true(exists(".RLIB_REQUIRED"))
  expect_true(all(c("polygonize_roi_df", "assign_feature_parent") %in% .RLIB_REQUIRED))
  expect_gt(length(.RLIB_REQUIRED), 3)
})

test_that("all three CLIs resolve their own directory the same way", {
  # They diverged once; a CLI that cannot find scripts/R fails only at run time.
  for (f in c("annotate_features_cli.r", "count_features_cli.r", "montage_qc_cli.r")) {
    src <- readLines(cli_path(f))
    expect_true(any(grepl("\\.THIS_DIR <- \\(function\\(\\)", src)), info = f)
    expect_true(any(grepl("rstudioapi", src)), info = f)
    expect_true(any(grepl("ofile", src)), info = f)
  }
})
