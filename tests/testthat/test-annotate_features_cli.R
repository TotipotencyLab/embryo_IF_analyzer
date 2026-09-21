# End-to-end behaviour of annotate_features_cli.r and the geometry it depends on.
#
# The point of these is the output CONTRACT, not the plumbing: the number of
# features found on the fixture, the fact that the union is z-aware, and the
# fact that a run which resolves no input stops rather than reporting success.

source_cli("cli_helpers.r")

# --- union_features and the sf-vs-tibble trap ---------------------------------

test_that("polygonize_roi_df returns a tibble, NOT an sf object", {
  # This is a live trap: the geometry column is a real sfc either way, and
  # class(df$geometry) is identical, so the two look the same. dplyr's
  # summarise() then drops the geometry instead of unioning it.
  skip_if_no_sf()
  source_r_scripts(c("FnGroup_roi_2_polygons.r"))
  outline <- fixture_file("nucleus", "outline")
  skip_if_no_fixture(outline)

  roi_df <- as_tibble(read.table(outline, header = TRUE, stringsAsFactors = FALSE))
  pg <- polygonize_roi_df(dplyr::select(roi_df, -name))

  expect_false(inherits(pg, "sf"))
  expect_true(inherits(pg$geometry, "sfc"))
})

test_that("union_features collapses per-slice ROIs into one polygon per feature", {
  skip_if_no_sf()
  skip_if_no_pkg("ggplot2")
  suppressPackageStartupMessages(library(ggplot2))
  source_r_scripts(c("FnGroup_roi_2_polygons.r", "find_ROI_z_intersect.r",
                     "define_feature_group.r", "plot_outline_topView.r"))
  outline <- fixture_file("nucleus", "outline")
  skip_if_no_fixture(outline)

  roi_df <- as_tibble(read.table(outline, header = TRUE, stringsAsFactors = FALSE)) %>%
    dplyr::select(-name)
  fg <- define_feature_group(roi_df, roi_regex = "^nucleus",
                             feature_prefix = "nucleus_",
                             invalid_feature_prefix = "invalid_",
                             max_z_dist = 3, min_z_span = 5)
  fg$feature_type <- "nucleus"

  nuc <- dplyr::filter(fg, stringr::str_detect(feature_id, "^nucleus_"))
  expect_identical(nrow(nuc), 65L)        # ROIs assigned to real nuclei

  u <- union_features(nuc)
  expect_identical(nrow(u), 6L)           # ...collapsing to 6 nuclei
  expect_true(all(sf::st_is_valid(u)))
  expect_true(all(as.character(sf::st_geometry_type(u)) == "POLYGON"))

  # union_features must accept the plain tibble too, not only a registered sf.
  expect_identical(nrow(union_features(nuc)), nrow(union_features(sf::st_as_sf(nuc))))
})

test_that("the R union is z-aware where Fiji's projection union is not", {
  # Fiji's Overview "merged" mode unions in the flattened 2D projection and
  # draws 5 outlines. R keeps 6 because one overlapping pair is 30 slices apart.
  # If this ever stops being true, the discrepancy documented in CLAUDE.md has
  # changed and the docs are stale.
  skip_if_no_sf()
  skip_if_no_pkg("ggplot2")
  suppressPackageStartupMessages(library(ggplot2))
  source_r_scripts(c("FnGroup_roi_2_polygons.r", "find_ROI_z_intersect.r",
                     "define_feature_group.r", "plot_outline_topView.r"))
  outline <- fixture_file("nucleus", "outline")
  skip_if_no_fixture(outline)

  roi_df <- as_tibble(read.table(outline, header = TRUE, stringsAsFactors = FALSE)) %>%
    dplyr::select(-name)
  fg <- define_feature_group(roi_df, roi_regex = "^nucleus",
                             feature_prefix = "nucleus_",
                             invalid_feature_prefix = "invalid_",
                             max_z_dist = 3, min_z_span = 5)
  fg$feature_type <- "nucleus"
  u <- union_features(dplyr::filter(fg, stringr::str_detect(feature_id, "^nucleus_")))

  ov <- sf::st_intersects(u, u, sparse = FALSE)
  diag(ov) <- FALSE
  expect_identical(sum(ov) / 2, 1)        # exactly one overlapping pair in x-y

  w <- which(ov, arr.ind = TRUE)
  pair <- sort(u$feature_id[w[1, ]])
  z <- sf::st_drop_geometry(u)
  gap <- abs(z$z_min[z$feature_id == pair[2]] - z$z_max[z$feature_id == pair[1]])
  expect_gt(gap, 5)                       # ...and it is far apart in z
})

test_that("do_union = FALSE collects instead of merging", {
  # Easy to reach for by accident. It yields the right number of rows and the
  # wrong geometry, which still plots.
  skip_if_no_sf()
  suppressPackageStartupMessages(library(sf))
  a <- sf::st_polygon(list(cbind(c(0, 2, 2, 0, 0), c(0, 0, 2, 2, 0))))
  b <- sf::st_polygon(list(cbind(c(1, 3, 3, 1, 1), c(0, 0, 2, 2, 0))))
  x <- sf::st_sf(g = c("f", "f"), geometry = sf::st_sfc(a, b))

  merged <- x %>% dplyr::group_by(g) %>% dplyr::summarise(.groups = "drop")
  collected <- x %>% dplyr::group_by(g) %>% dplyr::summarise(.groups = "drop", do_union = FALSE)

  expect_identical(as.character(sf::st_geometry_type(merged)), "POLYGON")
  expect_identical(as.character(sf::st_geometry_type(collected)), "MULTIPOLYGON")
  expect_lt(as.numeric(sf::st_area(merged)), as.numeric(sf::st_area(collected)))
})

# --- the y-axis flip ----------------------------------------------------------

test_that("flip_y_image actually flips, because coord_sf ignores scale_y_reverse", {
  skip_if_no_sf()
  skip_if_no_pkg("ggplot2")
  suppressPackageStartupMessages({library(sf); library(ggplot2)})
  source_r_scripts("plot_outline_topView.r")

  # A marker that is NOT symmetric in y: most of its mass sits at low y.
  poly <- sf::st_polygon(list(cbind(c(0, 10, 10, 6, 6, 0, 0), c(0, 0, 2, 2, 8, 8, 0))))
  x <- sf::st_sf(id = "L", geometry = sf::st_sfc(poly))

  y_ref <- 8
  flipped <- flip_y_image(x, y_ref)
  # suppressWarnings: st_centroid warns that attributes are assumed constant
  # over geometries, which is true here and irrelevant to the coordinates.
  cy_before <- suppressWarnings(sf::st_coordinates(sf::st_centroid(x))[, "Y"])
  cy_after  <- suppressWarnings(sf::st_coordinates(sf::st_centroid(flipped))[, "Y"])
  expect_equal(cy_after, y_ref - cy_before, tolerance = 1e-9)

  # And the demonstration that the obvious spelling does nothing: the rendered
  # y range is identical with and without scale_y_reverse().
  rng <- function(p) ggplot_build(p)$layout$panel_params[[1]]$y_range
  expect_identical(rng(ggplot(x) + geom_sf() + coord_sf()),
                   rng(ggplot(x) + geom_sf() + scale_y_reverse() + coord_sf()))
})

test_that("flip_y_labels undoes the flip for display", {
  source_r_scripts("plot_outline_topView.r")
  lab <- flip_y_labels(100)
  expect_identical(lab(c(0, 25, 100)), c("100", "75", "0"))
})

# --- the CLI end to end -------------------------------------------------------

test_that("annotate_features_cli finds 6 nuclei and 7 nucleoli on the fixture", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("annotate_features_cli.r")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))

  out <- withr::local_tempdir()
  res <- suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(),
    "--feature", "nucleus", "nucleolus",
    "--outdir", out,
    "--max_z_dist", "default=3", "nucleolus=1",
    "--min_z_span", "default=5", "nucleolus=2")))

  expect_true(file.exists(file.path(out, "GRV_Position010_features.rds")))
  expect_true(file.exists(file.path(out, "features.tsv")))

  x <- readRDS(file.path(out, "GRV_Position010_features.rds"))
  expect_s3_class(x, "sf")
  expect_identical(nrow(x), 97L)          # 70 nucleus + 27 nucleolus ROIs

  n_nuc <- length(unique(x$feature_id[grepl("^nucleus_", x$feature_id)]))
  n_nol <- length(unique(x$feature_id[grepl("^nucleolus_", x$feature_id)]))
  expect_identical(n_nuc, 6L)
  expect_identical(n_nol, 7L)

  # The tidy table must not lose or invent rows.
  expect_identical(nrow(res), 97L)
  expect_setequal(colnames(res),
                  c("roi", "z", "area", "feature_id", "feature_type", "sample"))
})

test_that("annotate_features_cli writes a QC plot only when asked", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("annotate_features_cli.r")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))

  # An option that should change nothing when off must be shown to change
  # nothing when off, AND to do something when on -- identical output can mean
  # "correctly did nothing" or "silently never ran".
  off <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_file("nucleus", "outline"), "--outdir", off)))
  expect_length(list.files(off, pattern = "_qc\\.png$"), 0)

  on <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_file("nucleus", "outline"), "--outdir", on, "--qc_plot")))
  png <- list.files(on, pattern = "_qc\\.png$", full.names = TRUE)
  expect_length(png, 1)
  expect_gt(file.size(png), 5000)         # a real image, not an empty canvas
})

test_that("annotate_features_cli requires its required arguments", {
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")
  expect_error(annotate_features_cli(c("--outdir", tempdir())), "--input")
  expect_error(annotate_features_cli(c("--input", "x")), "--outdir")
})

test_that("annotate_features_cli honours a sample sheet without needing one", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("annotate_features_cli.r")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))

  d <- withr::local_tempdir()
  sheet <- file.path(d, "samples.tsv")
  write.table(data.frame(prefix = "GRV_Position010", genotype = "wt"),
              sheet, sep = "\t", quote = FALSE, row.names = FALSE)

  out <- withr::local_tempdir()
  res <- suppressMessages(annotate_features_cli(c(
    "--input", fixture_file("nucleus", "outline"),
    "--outdir", out, "--sample_sheet", sheet)))

  expect_true("genotype" %in% colnames(res))
  expect_true(all(res$genotype == "wt"))
})

test_that("annotate_features_cli stops when the sheet excludes everything", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))

  d <- withr::local_tempdir()
  sheet <- file.path(d, "samples.tsv")
  write.table(data.frame(prefix = "SOME_OTHER_SAMPLE"), sheet,
              sep = "\t", quote = FALSE, row.names = FALSE)

  expect_error(suppressWarnings(suppressMessages(annotate_features_cli(c(
    "--input", fixture_file("nucleus", "outline"),
    "--outdir", withr::local_tempdir(), "--sample_sheet", sheet)))),
    "no sample in common")
})
