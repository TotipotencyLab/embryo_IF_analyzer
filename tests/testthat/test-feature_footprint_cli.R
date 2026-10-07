# feature_footprint.r + feature_footprint_cli.r (analysis-oo_count-physical_blur).
#
# Synthetic features where the right union is known by hand -- an overlap over
# z, a ring with a hole, a feature in two pieces, a single ROI that must come
# back vertex for vertex -- and then the fixture's real annotate output, where
# the footprint file is rebuilt into polygons and compared with a union taken
# straight from the ROIs. Tests/groovy/Test_FeatureOverlay.groovy holds the other
# half: those um vertices landing back on the ROI's pixels in Fiji.

source_cli("cli_helpers.r")

.sq <- function(x0, y0, x1, y1) {
  sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
}

.ffp_synthetic <- function() {
  rows <- list(
    # nucleus_1: two squares on two slices, overlapping -> one L-shaped piece
    list("r1", 1L, "nucleus_1", "nucleus", .sq(0, 0, 10, 10)),
    list("r2", 2L, "nucleus_1", "nucleus", .sq(5, 5, 15, 15)),
    # nucleus_2: four bars on four slices around a hole -> one piece, one hole
    list("r3", 1L, "nucleus_2", "nucleus", .sq(20, 0, 40, 5)),
    list("r4", 2L, "nucleus_2", "nucleus", .sq(20, 15, 40, 20)),
    list("r5", 3L, "nucleus_2", "nucleus", .sq(20, 5, 25, 15)),
    list("r6", 4L, "nucleus_2", "nucleus", .sq(35, 5, 40, 15)),
    # nucleus_3: two squares far apart -> two pieces
    list("r7", 1L, "nucleus_3", "nucleus", .sq(50, 0, 55, 5)),
    list("r8", 3L, "nucleus_3", "nucleus", .sq(60, 0, 65, 5)),
    # invalid: a single ROI, which must come back as itself
    list("r9", 5L, "invalid_nucleus_4", "nucleus", .sq(70, 30, 74, 38)),
    # failed bucket: two unrelated single-slice ROIs
    list("r10", 1L, "failed_nucleus_overlap", "nucleus", .sq(80, 0, 82, 2)),
    list("r11", 6L, "failed_nucleus_overlap", "nucleus", .sq(90, 0, 92, 2)),
    # reached no group, and another feature type: neither is written
    list("r12", 1L, NA_character_, "nucleus", .sq(100, 0, 102, 2)),
    list("r13", 1L, "nucleolus_1", "nucleolus", .sq(1, 1, 2, 2)))
  d <- data.frame(roi = vapply(rows, `[[`, "", 1), z = vapply(rows, `[[`, 0L, 2),
                  feature_id = vapply(rows, `[[`, "", 3), feature_type = vapply(rows, `[[`, "", 4),
                  sample = "S1", stringsAsFactors = FALSE)
  # As define_feature_group leaves it: a NAMED character vector.
  d$feature_id <- stats::setNames(d$feature_id, seq_len(nrow(d)))
  d$geometry <- sf::st_sfc(lapply(rows, `[[`, 5))
  return(sf::st_as_sf(d))
}

# Area of the symmetric difference. sf's binary operations DROP empty results,
# so two identical geometries give a length-0 sfc rather than area 0.
.ffp_symdiff <- function(a, b) {
  d <- sf::st_sym_difference(sf::st_sfc(a), b)
  return(if (length(d)) sum(as.numeric(sf::st_area(d))) else 0)
}

# Rebuild sf polygons from a vertex table: close each ring, holes into their part.
.ffp_rebuild <- function(v) {
  out <- lapply(split(v, v$feature_id), function(f) {
    parts <- lapply(split(f, f$part), function(p) {
      rings <- lapply(split(p, p$ring), function(r) {
        m <- cbind(r$x, r$y); rbind(m, m[1, ])
      })
      sf::st_polygon(rings[order(as.integer(names(rings)))])
    })
    sf::st_multipolygon(parts)
  })
  return(out[sort(names(out))])   # a named list of sfg
}

test_that("feature_footprints unions over z, keeps holes and pieces, skips what is not a feature", {
  skip_if_no_sf()
  x <- .ffp_synthetic()
  fp <- feature_footprints(x, feature_type = "nucleus")
  expect_identical(fp$feature_id, c("nucleus_1", "nucleus_2", "nucleus_3", "invalid_nucleus_4",
                                    "failed_nucleus_overlap"))
  expect_null(names(fp$feature_id))                              # the names attribute is not carried
  area <- stats::setNames(as.numeric(sf::st_area(fp)), fp$feature_id)
  expect_equal(unname(area), c(175, 300, 50, 32, 8))            # by hand
  expect_identical(fp$n_roi, c(2L, 4L, 2L, 1L, 2L))
  expect_identical(fp$z_min, c(1L, 1L, 1L, 5L, 1L))
  expect_identical(fp$z_max, c(2L, 4L, 3L, 5L, 6L))
  expect_true(all(sf::st_geometry_type(fp) == "MULTIPOLYGON"))
  expect_identical(nrow(feature_footprints(x, feature_type = "oocyte")), 0L)
})

test_that("footprint_vertices writes parts, rings and the ROI's own coordinates", {
  skip_if_no_sf()
  x <- .ffp_synthetic()
  fp <- feature_footprints(x, feature_type = "nucleus")
  v <- footprint_vertices(fp, name = "S1")
  expect_identical(colnames(v), FOOTPRINT_COLUMNS)
  expect_true(all(v$name == "S1"))

  rings <- unique(v[, c("feature_id", "part", "ring")])
  key <- function(id) {
    k <- rings[rings$feature_id == id, c("part", "ring")]; rownames(k) <- NULL; k
  }
  expect_equal(nrow(key("nucleus_1")), 1L)
  expect_equal(key("nucleus_2"), data.frame(part = c(1L, 1L), ring = c(0L, 1L)))
  expect_equal(key("nucleus_3"), data.frame(part = c(1L, 2L), ring = c(0L, 0L)))

  # A single ROI comes back as itself: its four corners, unflipped, unclosed.
  one <- v[v$feature_id == "invalid_nucleus_4", ]
  expect_identical(nrow(one), 4L)
  expect_setequal(paste(one$x, one$y), c("70 30", "74 30", "74 38", "70 38"))

  # No ring repeats its first vertex at the end (ImageJ's polygons do not).
  closes <- vapply(split(v, paste(v$feature_id, v$part, v$ring)), function(r) {
    r$x[1] == r$x[nrow(r)] && r$y[1] == r$y[nrow(r)]
  }, logical(1))
  expect_false(any(closes))

  # The table rebuilds into the same geometry: nothing lost between sf and the file.
  rb <- .ffp_rebuild(v)
  orig <- stats::setNames(as.list(sf::st_geometry(fp)), fp$feature_id)[names(rb)]
  diff <- vapply(seq_along(rb), function(i) {
    .ffp_symdiff(rb[[i]], sf::st_sfc(orig[[i]]))
  }, numeric(1))
  expect_equal(diff, rep(0, length(rb)))
})

test_that("feature_footprint_cli writes one table per sample that rebuilds to the ROIs' union", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "dplyr"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli(c("annotate_features_cli.r", "feature_footprint_cli.r"))

  feat_dir <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat_dir,
    "--max_z_dist", "default=3", "--min_z_span", "default=5")))
  out <- withr::local_tempdir()
  res <- suppressMessages(feature_footprint_cli(c("--features", feat_dir, "--outdir", out)))

  rds <- list.files(feat_dir, "_features[.]rds$", full.names = TRUE)
  x <- readRDS(rds[1])
  smp <- unique(x$sample)
  f <- file.path(out, paste0(smp, "_nucleus_footprint.txt"))
  expect_true(file.exists(f))
  v <- utils::read.delim(f, stringsAsFactors = FALSE)
  expect_identical(colnames(v), FOOTPRINT_COLUMNS)
  expect_true(all(v$name == smp))

  ids <- unique(unname(x$feature_id[!is.na(x$feature_id)]))
  expect_setequal(unique(v$feature_id), ids)
  counted <- ids[!startsWith(ids, "invalid_") & !startsWith(ids, "failed_")]
  expect_identical(res$counted, length(counted))
  expect_true(length(counted) > 0)

  # Each counted feature, rebuilt from the FILE, against a union taken straight
  # from its ROIs: the rounding to 4 decimals must not move an outline.
  rb <- .ffp_rebuild(v[v$feature_id %in% counted, ])
  xs <- sf::st_as_sf(x)
  worst <- max(vapply(names(rb), function(id) {
    u <- sf::st_union(sf::st_geometry(xs)[unname(xs$feature_id) == id & !is.na(xs$feature_id)])
    .ffp_symdiff(rb[[id]], u) / as.numeric(sf::st_area(u))
  }, numeric(1)))
  expect_lt(worst, 1e-6)

  # A feature type the table does not hold: a header-only file, not no file.
  suppressMessages(feature_footprint_cli(c("--features", feat_dir, "--outdir", out, "--feature", "oocyte")))
  h <- readLines(file.path(out, paste0(smp, "_oocyte_footprint.txt")))
  expect_identical(h, paste(FOOTPRINT_COLUMNS, collapse = "\t"))
})

test_that("feature_footprint_cli runs as a script, with its own narrow sourcing", {
  # The suite pre-sources everything, so only a subprocess shows a missing
  # library file -- as count_features_cli.r's test explains.
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "dplyr"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  rscript <- file.path(R.home("bin"), "Rscript")
  skip_if_not(file.exists(rscript), "Rscript not found")
  source_cli("annotate_features_cli.r")

  feat_dir <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat_dir,
    "--max_z_dist", "default=3", "--min_z_span", "default=5")))
  out <- withr::local_tempdir()
  txt <- suppressWarnings(system2(rscript, c(cli_path("feature_footprint_cli.r"),
    "--features", feat_dir, "--outdir", out), stdout = TRUE, stderr = TRUE))
  expect_length(list.files(out, "_nucleus_footprint[.]txt$"), 1L)
  expect_true(any(grepl("counted nucleus footprints", txt)))
})
