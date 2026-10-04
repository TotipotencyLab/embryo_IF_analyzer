# A time course through the R side: grouped, related and counted one frame at a
# time, with feature ids numbered across the whole series.
#
# The failure this guards against is silent. Grouping ROIs by z overlap alone
# joins one object's frames into one feature -- the fixture in three frames
# comes out as its own six nuclei, each spanning all three -- and the count
# looks entirely plausible. Every test here that shows the partition working
# has its control beside it, showing what the same input does without it.

source_cli("cli_helpers.r")

group_lib <- function(){
  source_r_scripts(c("FnGroup_roi_2_polygons.r", "find_ROI_z_intersect.r",
                     "define_feature_group.r"))
}

# The fixture's nucleus outline as Fiji writes a multi-frame one: every frame
# the same ROIs, the ids carrying TTTT-. One frame keeps the 3-field ids.
fixture_frames <- function(frames){
  o <- read.table(fixture_file("nucleus", "outline"), header = TRUE, sep = "\t",
                  stringsAsFactors = FALSE)[, c("roi", "z", "x", "y")]
  if(length(frames) == 1){ o$t <- frames; return(o) }
  do.call(rbind, lapply(frames, function(t){
    o$roi <- sub("^nucleus_", sprintf("nucleus_%04d-", t), o$roi)
    o$t <- t
    o
  }))
}

group_nuclei <- function(roi_df, ...){
  define_feature_group(roi_df, roi_regex = "^nucleus", max_z_dist = 3, min_z_span = 5,
                       feature_prefix = "nucleus_", invalid_feature_prefix = "invalid_nucleus_",
                       fail_ROI_feature_prefix = "failed_nucleus_", ...)
}

ids_of <- function(df, rx){
  v <- unique(df$feature_id[!is.na(df$feature_id) & grepl(rx, df$feature_id)])
  sort(v)
}

# --- the id -------------------------------------------------------------------

test_that("feature ids are <feature>_NNNN, four digits", {
  skip_if_no_sf()
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  group_lib()
  fg <- group_nuclei(fixture_frames(1)[, c("roi", "z", "x", "y")])
  expect_identical(ids_of(fg, "^nucleus_"), sprintf("nucleus_%04d", 1:6))
  expect_identical(ids_of(fg, "^invalid_"), sprintf("invalid_nucleus_%04d", 1:2))
  expect_identical(.feature_ids("nucleus_", c(1, 12, 10000)),
                   c("nucleus_0001", "nucleus_0012", "nucleus_10000"))
})

# --- the partition ------------------------------------------------------------

test_that("one partition value is the unpartitioned result plus its column", {
  skip_if_no_sf()
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  group_lib()
  o <- fixture_frames(1)
  with_t <- group_nuclei(o, partition = "t")
  without <- group_nuclei(o[, c("roi", "z", "x", "y")])

  expect_identical(colnames(with_t)[1:3], c("roi", "t", "z"))
  expect_equal(unique(with_t$t), 1)
  expect_identical(dplyr::select(with_t, -t), without)
})

test_that("each frame is grouped alone, and numbered on from the last", {
  skip_if_no_sf()
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  group_lib()
  o <- fixture_frames(1:3)
  fg <- group_nuclei(o, partition = "t")

  for(t in 1:3){
    f <- fg[fg$t == t, ]
    expect_identical(ids_of(f, "^nucleus_"), sprintf("nucleus_%04d", (t - 1) * 6 + 1:6),
                     info = paste("t =", t))
    expect_identical(ids_of(f, "^invalid_"), sprintf("invalid_nucleus_%04d", (t - 1) * 2 + 1:2),
                     info = paste("t =", t))
  }
  # No feature reaches into another frame.
  spans <- tapply(fg$t, fg$feature_id, function(x) length(unique(x)))
  expect_identical(max(spans[!grepl("^failed_", names(spans))]), 1L)

  # Frame 3 is frame 1 again: the same ROIs make the same features, 12 apart.
  one <- fg[fg$t == 1 & grepl("^nucleus_", fg$feature_id), ]
  three <- fg[fg$t == 3 & grepl("^nucleus_", fg$feature_id), ]
  key <- function(d) paste(sub("^nucleus_\\d{4}-", "nucleus_", d$roi),
                           as.integer(sub(".*_", "", d$feature_id)) %% 12)
  expect_setequal(key(three), key(one))

  # The control: the same table without the partition is six nuclei, each
  # three frames deep. This is what a run that ignored t would report.
  fr <- setNames(o$t, o$roi)
  merged <- suppressWarnings(group_nuclei(o[, c("roi", "z", "x", "y")]))
  expect_length(ids_of(merged, "^nucleus_"), 6)
  m <- merged[grepl("^nucleus_", merged$feature_id), ]
  expect_identical(unname(max(tapply(fr[m$roi], m$feature_id, function(x) length(unique(x))))), 3L)
})

test_that("frames are numbered in t order, not in the order the rows came", {
  skip_if_no_sf()
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  group_lib()
  o <- fixture_frames(1:2)
  fg <- group_nuclei(o[order(-o$t), ], partition = "t")
  expect_identical(ids_of(fg[fg$t == 1, ], "^nucleus_"), sprintf("nucleus_%04d", 1:6))
  expect_identical(ids_of(fg[fg$t == 2, ], "^nucleus_"), sprintf("nucleus_%04d", 7:12))
})

test_that("an empty frame names itself and uses no numbers", {
  skip_if_no_sf()
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  group_lib()
  o <- fixture_frames(1:3)
  # Frame 2 holds only ROIs of another feature, so roi_regex leaves it nothing.
  o$roi[o$t == 2] <- sub("^nucleus_", "other_", o$roi[o$t == 2])
  expect_warning(fg <- group_nuclei(o, partition = "t"),
                 "^t = 2: All ROIs were filtered out \\(ROI name filtering\\)")
  expect_identical(ids_of(fg[fg$t == 3, ], "^nucleus_"), sprintf("nucleus_%04d", 7:12))
  expect_true(all(fg$feature_id[fg$t == 2] == "failed_nucleus_name"))

  # One frame: the warning reads as it always did, with no "t = 1:".
  one <- o[o$t == 2, ]
  one$t <- 1L
  expect_warning(group_nuclei(one, partition = "t"), "^All ROIs were filtered out")
})

test_that("a partition column that is absent or holds NA is refused", {
  skip_if_no_sf()
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  group_lib()
  o <- fixture_frames(1)
  expect_error(group_nuclei(o, partition = "frame"), "not in the ROI table: frame")
  o$t[1] <- NA
  expect_error(group_nuclei(o, partition = "t"), "hold NA")
})

# --- containment --------------------------------------------------------------

sq <- function(x0, y0, s = 10){
  sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0),
                            c(y0, y0, y0 + s, y0 + s, y0))))
}
feat_t <- function(feature_id, feature_type, t, zs, geom){
  sf::st_sf(feature_id = feature_id, feature_type = feature_type, t = t, z = zs,
            series_id = "S1", geometry = sf::st_sfc(lapply(zs, function(z) geom)))
}

test_that("a child is placed only in a parent of its own frame", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")
  # The same nucleus, in the same place, at two time points.
  x <- rbind(feat_t("nucleus_0001",   "nucleus",   1, 5:6, sq(0, 0, 100)),
             feat_t("nucleus_0002",   "nucleus",   2, 5:6, sq(0, 0, 100)),
             feat_t("nucleolus_0001", "nucleolus", 1, 5:6, sq(40, 40, 10)),
             feat_t("nucleolus_0002", "nucleolus", 2, 5:6, sq(40, 40, 10)))
  rel <- expect_silent(assign_feature_parent(x, c(nucleolus = "nucleus")))
  d <- sf::st_drop_geometry(rel)
  got <- tapply(d$parent_feature_id, d$feature_id, unique)
  expect_identical(got[["nucleolus_0001"]], "nucleus_0001")
  expect_identical(got[["nucleolus_0002"]], "nucleus_0002")
  # Row order is kept for a single frame and series ...
  expect_identical(rel$feature_id, x$feature_id)

  # The control: without t both nuclei contain both nucleoli equally, and the
  # second nucleolus goes to the first frame's nucleus.
  x$t <- NULL
  expect_warning(rel0 <- assign_feature_parent(x, c(nucleolus = "nucleus")), "lies in two")
  d0 <- sf::st_drop_geometry(rel0)
  expect_identical(unique(d0$parent_feature_id[d0$feature_id == "nucleolus_0002"]), "nucleus_0001")
})

test_that("a frame with nothing to relate says which frame", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")
  x <- rbind(feat_t("nucleus_0001",   "nucleus",   1, 5:6, sq(0, 0, 100)),
             feat_t("nucleus_0002",   "nucleus",   2, 5:6, sq(0, 0, 100)),
             feat_t("nucleolus_0001", "nucleolus", 1, 5:6, sq(40, 40, 10)))
  expect_warning(assign_feature_parent(x, c(nucleolus = "nucleus")),
                 "^t = 2: No valid nucleolus")
})

# --- the CLIs -----------------------------------------------------------------

# Two frames, annotated: frame 1 holds two nuclei, frame 2 one.
annotated_time_course <- function(env = parent.frame()){
  source_cli("annotate_features_cli.r")
  d <- withr::local_tempdir(.local_envir = env)
  sq_rows <- function(roi, t, z, x0) data.frame(name = "tc", roi = roi, t = t, z = z,
                                                x = x0 + c(0, 10, 10, 0), y = c(10, 10, 20, 20))
  write.table(rbind(sq_rows("nucleus_0001-0001-0001-0015", 1, 1, 0),
                    sq_rows("nucleus_0001-0002-0001-0015", 1, 2, 0),
                    sq_rows("nucleus_0001-0001-0002-0015", 1, 1, 50),
                    sq_rows("nucleus_0001-0002-0002-0015", 1, 2, 50),
                    sq_rows("nucleus_0002-0001-0001-0015", 2, 1, 0),
                    sq_rows("nucleus_0002-0002-0001-0015", 2, 2, 0)),
              file.path(d, "tc_nucleus_outline.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  out <- withr::local_tempdir(.local_envir = env)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus",
                                           "--outdir", out, "--min_z_span", "default=2")))
  out
}

test_that("count is per frame, and its plot is drawn over t", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli("count_features_cli.r")
  feat <- annotated_time_course()
  out <- withr::local_tempdir()
  msgs <- testthat::capture_messages(
    counts <- count_features_cli(c("--input", feat, "--outdir", out)))
  expect_identical(colnames(counts)[1:3], c("series_id", "t", "feature_class"))
  expect_equal(counts$t, c(1, 2))
  expect_identical(counts$n_detected, c(2L, 1L))
  expect_identical(counts$n_roi, c(4L, 2L))
  expect_true(any(grepl("tc: 2 frames; summed over them nucleus=3", msgs, fixed = TRUE)))

  # geom_col stacks rows sharing an x, so a bar per series would show 3.
  geom <- function(p) class(p$layers[[1]]$geom)[1]
  expect_identical(geom(.count_plot_build(counts, character(0))), "GeomLine")
  expect_identical(geom(.count_plot_build(counts[counts$t == 1, ], character(0))), "GeomCol")
})

test_that("the QC montage refuses a time course rather than overlaying its frames", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  source_cli("montage_qc_cli.r")
  feat <- annotated_time_course()
  expect_error(suppressMessages(montage_qc_cli(c(
    "--features", file.path(feat, "tc_features.rds"),
    "--output", file.path(withr::local_tempdir(), "m.png")))),
    "holds 2 frames")
})

test_that("a series table may not carry a column called t", {
  expect_error(.cli_check_reserved(data.frame(series_id = "A", t = 1)), "collide.*\\bt\\b")
})
