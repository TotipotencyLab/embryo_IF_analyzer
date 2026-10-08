# Feature centroids, the membership fingerprint, and the QC colour/label options
# (tracking milestone, PR 1: note/time_series_plan.md §4).
#
# The centroid cases are built so that an UNWEIGHTED mean gives a different
# answer from the area-weighted one, and the z cases so that slice numbers,
# a missing step and a single plane are each told apart.

source_cli("cli_helpers.r")

# A square ROI as outline-table rows: four vertices, (x0, y0) to (x0+s, y0+s).
.sq <- function(name, roi, t, z, x0, y0, s){
  data.frame(name = name, roi = roi, t = t, z = z,
             x = c(x0, x0 + s, x0 + s, x0), y = c(y0, y0, y0 + s, y0 + s))
}
.write_outline <- function(d, sid, feature, rows){
  write.table(rows, file.path(d, paste0(sid, "_", feature, "_outline.txt")),
              sep = "\t", quote = FALSE, row.names = FALSE)
}
.write_config <- function(d, sid, depth){
  write.table(data.frame(parameter = c("series_id", "pixel_depth"), value = c(sid, depth)),
              file.path(d, paste0(sid, "_config.txt")), sep = "\t", quote = FALSE,
              row.names = FALSE)
}
.read_centroids <- function(out, sid){
  read.delim(file.path(out, paste0(sid, "_feature_centroids.tsv")), stringsAsFactors = FALSE)
}

# One nucleus: a 10x10 square on slice 1 and a 20x20 one on slice 2 that
# contains it. Area-weighted: x = y = (100*15 + 400*20)/500 = 19, mean slice
# 1.8. Unweighted the centroid would be 17.5 and the slice 1.5.
.one_nucleus <- function(d, sid = "W"){
  .write_outline(d, sid, "nucleus",
                 rbind(.sq(sid, "nucleus_0001-0001-0015", 1, 1, 10, 10, 10),
                       .sq(sid, "nucleus_0001-0002-0020", 1, 2, 10, 10, 20)))
}
.annotate <- function(d, out, ...){
  annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                          "--min_z_span", "default=2", ...))
}

test_that("a centroid is the area-weighted mean of its slices, z in calibrated units", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  .one_nucleus(d)
  .write_config(d, "W", 2.5)
  res <- suppressMessages(.annotate(d, out))

  cen <- .read_centroids(out, "W")
  expect_identical(nrow(cen), 1L)
  expect_equal(c(cen$x, cen$y), c(19, 19))
  # (1.8 - 1) * 2.5: slice 1 at z = 0, as ImageJ calibrates it.
  expect_equal(cen$z, 2.0)
  expect_identical(cen$n_roi, 2L)
  expect_equal(c(cen$area_sum, cen$area_max), c(500, 400))
  expect_identical(cen$run_id, unique(res$run_id))
  expect_match(cen$fingerprint, "^[0-9a-f]{10}$")
})

test_that("z is blank without a z step, and annotate still finishes", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  .one_nucleus(d)                                   # no _config.txt at all
  msgs <- testthat::capture_messages(.annotate(d, out))
  cen <- .read_centroids(out, "W")
  expect_true(is.na(cen$z))
  expect_equal(cen$x, 19)                           # x and y need no config
  expect_true(any(grepl("z BLANK: no pixel_depth", msgs, fixed = TRUE)))
  expect_true(file.exists(file.path(out, "W_features.rds")))
})

test_that("the z step comes from beside the INPUTS, and a clash gives none", {
  skip_if_no_sf()
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir(); elsewhere <- withr::local_tempdir()
  .write_config(d1, "A", 5); .write_config(d2, "B", 0.5)
  feats <- data.frame(series_id = "A")
  # The features file is somewhere else entirely, as annotate's outdir is.
  expect_identical(.cli_z_step_for(feats, file.path(elsewhere, "A_features.rds"), d1), 5)
  expect_true(is.na(.cli_z_step_for(feats, file.path(elsewhere, "A_features.rds"), character(0))))
  both <- .cli_z_step_for(data.frame(series_id = c("A", "B")),
                          file.path(elsewhere, "x.rds"), c(d1, d2))
  expect_true(is.na(both))
  expect_identical(attr(both, "conflict"), c(0.5, 5))
})

test_that("a time course gets a centroid per feature per frame", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  # The object moves 30 units in x between frames.
  .write_outline(d, "TL", "nucleus",
                 rbind(.sq("TL", "nucleus_0001-0001-0001-0015", 1, 1, 10, 10, 10),
                       .sq("TL", "nucleus_0001-0002-0001-0015", 1, 2, 10, 10, 10),
                       .sq("TL", "nucleus_0002-0001-0001-0015", 2, 1, 40, 10, 10),
                       .sq("TL", "nucleus_0002-0002-0001-0015", 2, 2, 40, 10, 10)))
  suppressMessages(.annotate(d, out))
  cen <- .read_centroids(out, "TL")
  expect_identical(cen$t, c(1L, 2L))
  expect_identical(cen$feature_id, c("nucleus_0001", "nucleus_0002"))
  expect_equal(cen$x, c(15, 45))
})

# --- the library directly ----------------------------------------------------------

# A hand-built annotation: one real feature of two ROIs, a bridge ROI far away
# inside it, and rows that are not features at all.
.hand_annotation <- function(){
  sq <- function(x0, y0, s) sf::st_polygon(list(rbind(c(x0, y0), c(x0 + s, y0),
                                                      c(x0 + s, y0 + s), c(x0, y0 + s),
                                                      c(x0, y0))))
  geom <- sf::st_sfc(sq(0, 0, 10), sq(0, 0, 10), sq(200, 200, 10),
                     sq(50, 50, 10), sq(60, 60, 10), sq(70, 70, 10), sq(80, 80, 4))
  sf::st_sf(series_id = "H", t = 1L, z = c(1, 2, 3, 1, 1, 1, 1),
            roi = paste0("r", 1:7),
            area = c(100, 100, 100, 100, 100, 100, 16),
            is_bridge = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
            feature_id = c("nucleus_0001", "nucleus_0001", "nucleus_0001",
                           "invalid_nucleus_0001", "failed_nucleus_area", NA,
                           "nucleolus_0001"),
            feature_type = c(rep("nucleus", 6), "nucleolus"),
            run_id = "abc", geometry = geom)
}

test_that("bridge, invalid, failed and NA rows place no centroid", {
  skip_if_no_sf()
  source_r_scripts(c("feature_join.r", "feature_centroids.r"))
  cen <- feature_centroids(.hand_annotation(), z_step = 1)
  expect_identical(cen$feature_id, c("nucleolus_0001", "nucleus_0001"))
  nuc <- cen[cen$feature_id == "nucleus_0001", ]
  # The bridge sits at (205, 205): counted, the centroid would move to ~70.
  expect_equal(c(nuc$x, nuc$y), c(5, 5))
  expect_identical(nuc$n_roi, 2L)
})

test_that("a single-plane series has z = 0 whatever the step; otherwise z needs one", {
  # Library level: annotate cannot reach this today -- a lone ROI per object
  # overlaps nothing in z, so define_feature_group() makes no feature from a
  # single plane (failed_<type>_overlap). The rule is here for when it can.
  skip_if_no_sf()
  source_r_scripts(c("feature_join.r", "feature_centroids.r"))
  flat <- .hand_annotation()
  flat$z <- 1
  expect_identical(unique(feature_centroids(flat, z_step = NA_real_)$z), 0)
  expect_identical(unique(feature_centroids(flat, z_step = 5)$z), 0)
  # One series flat, one not: decided per series, not per feature.
  deep <- .hand_annotation(); deep$series_id <- "D"
  both <- feature_centroids(rbind(flat, deep), z_step = NA_real_)
  expect_identical(unique(both$z[both$series_id == "H"]), 0)
  expect_true(all(is.na(both$z[both$series_id == "D"])))
})

test_that("the fingerprint is what the features are, per type", {
  skip_if_no_sf()
  source_r_scripts(c("feature_join.r", "feature_centroids.r"))
  a <- .hand_annotation()
  fp <- function(x) feature_fingerprint(x)$fingerprint[feature_fingerprint(x)$feature_type == "nucleus"]
  base <- fp(a)

  b <- a; b$run_id <- "a-later-VERSION"                 # not part of what it is
  expect_identical(fp(b), base)
  c2 <- a; c2$feature_id[4] <- "invalid_nucleus_0009"   # not a real feature
  expect_identical(fp(c2), base)
  d <- a; d$roi[7] <- "r99"                             # another TYPE regrouped
  expect_identical(fp(d), base)
  e <- a; e$roi[2] <- "r42"                             # a nucleus ROI changed
  expect_false(identical(fp(e), base))
  f <- a; f$feature_id[2] <- "nucleus_0002"             # regrouped
  expect_false(identical(fp(f), base))
})

# --- colour and labels by feature ---------------------------------------------------

test_that("each focus feature gets a colour, other types one grey, grey never a feature's", {
  source_r_scripts("plot_outline_topView.r")
  ids <- c(sprintf("nucleus_%04d", 1:12), "nucleolus_0001")
  types <- c(rep("nucleus", 12), "nucleolus")
  k <- qc_id_colours(ids, types, "nucleus")
  expect_identical(as.character(k$values[13]), "other")
  expect_identical(unname(k$palette["other"]), QC_OTHER_GREY)
  expect_setequal(names(k$palette), c("other", ids[1:12]))  # complete: no NA colour
  # Tableau 10 without its grey has 9: ids 1 and 10 share, 1 and 2 do not.
  expect_identical(unname(k$palette["nucleus_0001"]), unname(k$palette["nucleus_0010"]))
  expect_false(identical(unname(k$palette["nucleus_0001"]), unname(k$palette["nucleus_0002"])))
  expect_false("#BAB0AC" %in% k$palette)                    # Tableau's grey, dropped
  # A ramp is strided: consecutive ids are not neighbouring shades.
  v <- qc_id_colours(sprintf("nucleus_%04d", 1:6), rep("nucleus", 6), "nucleus", "Viridis")
  ramp <- qc_palette_colours("Viridis", 6)$colours
  pos <- match(v$palette[sprintf("nucleus_%04d", 1:6)], ramp)
  expect_true(all(abs(diff(pos)) > 1))
  expect_error(qc_palette_colours("Grays"), "fewer than two colours")
  expect_error(qc_palette_colours("nope"), "Unknown palette")
})

test_that("labels: all, some by number or id, focus type only", {
  source_r_scripts("plot_outline_topView.r")
  ids <- c("nucleus_0001", "nucleus_0007", "nucleolus_0007")
  types <- c("nucleus", "nucleus", "nucleolus")
  expect_identical(qc_label_select(ids, types, "nucleus", "all"), c("0001", "0007", NA))
  for (spec in c("7", "0007", "nucleus_0007")) {
    expect_identical(qc_label_select(ids, types, "nucleus", spec), c(NA, "0007", NA), info = spec)
  }
  expect_identical(qc_label_select(ids, types, "nucleus", NULL), rep(NA_character_, 3))
  expect_warning(qc_label_select(ids, types, "nucleus", "12"), "numbered 0012")
  expect_error(qc_label_select(ids, types, "nucleus", "seven"), "takes 'all'")
})

test_that("annotate's QC: colour by id draws something else, bad options stop early", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  source_cli("annotate_features_cli.r")
  d <- withr::local_tempdir()
  .write_outline(d, "Q", "nucleus",
                 rbind(.sq("Q", "nucleus_0001-0001-0015", 1, 1, 10, 10, 10),
                       .sq("Q", "nucleus_0001-0002-0015", 1, 2, 10, 10, 10),
                       .sq("Q", "nucleus_0002-0001-0045", 1, 1, 40, 40, 10),
                       .sq("Q", "nucleus_0002-0002-0045", 1, 2, 40, 40, 10)))
  plain <- withr::local_tempdir(); by_id <- withr::local_tempdir()
  suppressMessages(.annotate(d, plain, "--qc_plot"))
  suppressMessages(.annotate(d, by_id, "--qc_plot", "--qc_color_by", "feature_id",
                             "--qc_label", "all"))
  px <- function(dir) as.integer(magick::image_data(
    magick::image_read(file.path(dir, "Q_features_qc.png")), channels = "rgb"))
  a <- px(plain); b <- px(by_id)
  # Tableau 10's first two colours are the two nuclei's: present by id, absent before.
  has <- function(p, hex) { v <- grDevices::col2rgb(hex)
    any(p[, , 1] == v[1] & p[, , 2] == v[2] & p[, , 3] == v[3]) }
  expect_true(has(b, "#4E79A7") && has(b, "#F28E2B"))
  expect_false(has(a, "#4E79A7") || has(a, "#F28E2B"))

  never <- file.path(withr::local_tempdir(), "never")
  expect_error(suppressMessages(.annotate(d, never, "--qc_plot", "--qc_color_by", "colour")),
               "takes feature_type or feature_id")
  expect_error(suppressMessages(.annotate(d, never, "--qc_plot", "--qc_palette", "nope")),
               "Unknown palette")
  expect_false(dir.exists(never))                   # refused before anything ran
  expect_warning(suppressMessages(.annotate(d, withr::local_tempdir(), "--qc_label", "all")),
                 "without --qc_plot")
})

test_that("a time course keeps each feature's colour: ids coloured over the whole series", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  source_cli("annotate_features_cli.r")
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  .write_outline(d, "TC", "nucleus",
                 rbind(.sq("TC", "nucleus_0001-0001-0001-0015", 1, 1, 10, 10, 10),
                       .sq("TC", "nucleus_0001-0002-0001-0015", 1, 2, 10, 10, 10),
                       .sq("TC", "nucleus_0002-0001-0001-0015", 2, 1, 12, 10, 10),
                       .sq("TC", "nucleus_0002-0002-0001-0015", 2, 2, 12, 10, 10)))
  suppressMessages(.annotate(d, out, "--qc_plot", "--qc_color_by", "feature_id"))
  pages <- magick::image_read(file.path(out, "TC_features_qc.tif"))
  has <- function(i, hex) { p <- as.integer(magick::image_data(pages[i], channels = "rgb"))
    v <- grDevices::col2rgb(hex); any(p[, , 1] == v[1] & p[, , 2] == v[2] & p[, , 3] == v[3]) }
  # Frame 2's only feature is nucleus_0002: the series' SECOND colour. Coloured
  # per page it would take the first, and every page would look the same.
  expect_true(has(1, "#4E79A7")); expect_false(has(1, "#F28E2B"))
  expect_true(has(2, "#F28E2B")); expect_false(has(2, "#4E79A7"))
})

test_that("montage_qc: colour by id needs one type and no class colouring", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli(c("annotate_features_cli.r", "montage_qc_cli.r"))
  out <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "nucleolus", "--outdir", out,
    "--min_z_span", "default=5", "nucleolus=2")))
  rds <- file.path(out, "GRV_Position010_features.rds")
  cfg <- file.path(fixture_dir(), "GRV_Position010_config.txt")
  run <- function(...) suppressWarnings(suppressMessages(montage_qc_cli(c(
    "--features", rds, "--config", cfg, ...))))

  expect_error(run("--output", file.path(out, "a.png"), "--color_by", "feature_id"),
               "acts on one feature type")
  expect_error(run("--output", file.path(out, "b.png"), "--feature", "nucleus",
                   "--color_by", "feature_id", "--color_map", "nucleus=red"),
               "would be ignored")
  png <- file.path(out, "c.png")
  run("--output", png, "--feature", "nucleus", "nucleolus", "--color_by", "feature_id",
      "--label", "all")
  p <- as.integer(magick::image_data(magick::image_read(png), channels = "rgb"))
  v <- grDevices::col2rgb("#4E79A7")
  expect_true(any(p[, , 1] == v[1] & p[, , 2] == v[2] & p[, , 3] == v[3]))
})
