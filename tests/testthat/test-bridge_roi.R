# Bridging: a pre-grouping ROI filter must not be able to cut one object in two.
#
# These build their ROIs rather than reading the fixture, because the failure
# needs a filter that removes an object's INTERIOR slices. The fixture has no
# such object, and a test that cannot fail is worth nothing here.

# A closed square of half-width `half`, centred on `cx`, on slice `z`.
.sq <- function(roi, z, half, cx = 0){
  data.frame(roi = roi, z = z,
             x   = c(cx - half, cx + half, cx + half, cx - half, cx - half),
             y   = c(-half, -half, half, half, -half),
             stringsAsFactors = FALSE)
}

# One object over z 1..9 whose middle three slices are tiny: a lower-bound area
# filter removes exactly the interior, which is the shape of the real failure
# (a circularity cut removing a large oocyte's widest cross-sections).
.pinched_object <- function(cx = 0, tag = "nucleus", big = 10, small = 2){
  halves <- c(big, big, big, small, small, small, big, big, big)
  do.call(rbind, lapply(seq_along(halves), function(i){
    .sq(sprintf("%s_%04d-0001-0433", tag, i), i, halves[i], cx)
  }))
}

.n_valid <- function(fg, prefix = "nucleus"){
  length(unique(fg$feature_id[grepl(paste0("^", prefix, "_\\d+$"), fg$feature_id)]))
}

.group_args <- list(max_z_dist = 1, min_z_span = 5, feature_prefix = "nucleus_",
                    roi_area_range = c(50, Inf))

.run_group <- function(roi_df, ...){
  do.call(define_feature_group, c(list(roi_df), utils::modifyList(.group_args, list(...))))
}

.setup <- function(){
  skip_if_no_sf()
  source_r_scripts(c("FnGroup_roi_2_polygons.r", "find_ROI_z_intersect.r",
                     "define_feature_group.r"))
}

test_that("an area filter splits one object in two, and bridging rejoins it", {
  .setup()
  o <- .pinched_object()

  # Without bridging the filter opens a 3-slice gap that max_z_dist=1 cannot
  # span, so the object becomes two 3-slice groups -- both below min_z_span=5.
  off <- .run_group(o)
  expect_identical(.n_valid(off), 0L)
  expect_identical(sum(grepl("^invalid_", off$feature_id)), 6L)

  # Bridging keeps the small slices in the graph, so the halves stay connected.
  on <- .run_group(o, bridge_roi = "area")
  expect_identical(.n_valid(on), 1L)

  # ... and the object is whole: all nine slices carry the one feature_id.
  fid <- unique(on$feature_id[grepl("^nucleus_\\d+$", on$feature_id)])
  expect_identical(sum(on$feature_id == fid, na.rm = TRUE), 9L)
})

test_that("the bridging ROIs are exactly the ones the filter rejected", {
  .setup()
  on <- .run_group(.pinched_object(), bridge_roi = "area")

  expect_true("is_bridge" %in% colnames(on))
  expect_identical(sum(on$is_bridge), 3L)
  # Slices 4, 5 and 6 are the tiny ones.
  expect_setequal(on$z[on$is_bridge], c(4L, 5L, 6L))
})

test_that("is_bridge is present, and all FALSE, when nothing bridges", {
  # Otherwise every downstream reader has to test for the column first.
  .setup()
  off <- .run_group(.pinched_object())
  expect_true("is_bridge" %in% colnames(off))
  expect_false(any(off$is_bridge))
})

test_that("a bridge does not count toward min_z_span", {
  .setup()
  o <- .pinched_object()

  # Six ordinary slices, nine in total. At min_z_span=6 the object survives; at
  # 7 it must not -- which it only fails if the three bridges are uncounted.
  expect_identical(.n_valid(.run_group(o, bridge_roi = "area", min_z_span = 6)), 1L)
  expect_identical(.n_valid(.run_group(o, bridge_roi = "area", min_z_span = 7)), 0L)
})

test_that("bridges alone cannot assemble themselves into a feature", {
  .setup()
  # Every slice of this object is below the area cut, so every ROI is a bridge.
  # It must never be counted, however well its rejects connect to each other.
  only_small <- do.call(rbind, lapply(1:9, function(i){
    .sq(sprintf("nucleus_%04d-0001-0433", i), i, half = 2)
  }))
  expect_warning(on <- .run_group(only_small, bridge_roi = "area"),
                 "bridges alone cannot form a feature")
  on <- suppressWarnings(.run_group(only_small, bridge_roi = "area"))

  expect_identical(.n_valid(on), 0L)
  expect_true(all(on$is_bridge))
})

test_that("a bridged object and an all-bridge object are told apart in one run", {
  .setup()
  # Two objects far enough apart in x that they never intersect.
  both <- rbind(.pinched_object(cx = 0),
                do.call(rbind, lapply(1:9, function(i){
                  .sq(sprintf("nucleus_%04d-0002-0433", i), i, half = 2, cx = 1000)
                })))
  on <- .run_group(both, bridge_roi = "area")

  # Exactly one real feature: the pinched object. The all-bridge one is not it.
  expect_identical(.n_valid(on), 1L)
  fid <- unique(on$feature_id[grepl("^nucleus_\\d+$", on$feature_id)])
  expect_true(all(on$x[on$feature_id %in% fid] < 500, na.rm = TRUE))
})

test_that("bridges do not move the mean area that --feature_area tests", {
  .setup()
  o <- .pinched_object()
  # Ordinary slices are 20x20 = 400; the bridges are 4x4 = 16. Counting the
  # bridges would drag the mean to ~272 and the feature would fail this range.
  on <- .run_group(o, bridge_roi = "area", feature_area_range = c(300, Inf))
  expect_identical(.n_valid(on), 1L)
})

test_that("the name filter cannot bridge, and is refused by name", {
  .setup()
  # It rejects on feature identity, not quality: bridging it would let an ROI
  # of one feature type glue two objects of another together.
  expect_error(.run_group(.pinched_object(), bridge_roi = "name"),
               "rejects on feature identity")
  expect_error(.run_group(.pinched_object(), bridge_roi = "circularity"),
               "must be a subset")
})

test_that("--bridge_roi resolves CLI names to library reasons", {
  source_cli(c("cli_helpers.r", "annotate_features_cli.r"))

  expect_null(.cli_bridge_spec(NA))
  expect_identical(.cli_bridge_spec("circularity"), "include")
  expect_identical(.cli_bridge_spec("roi_area"), "area")
  expect_setequal(.cli_bridge_spec("all"), c("include", "area"))
  expect_setequal(.cli_bridge_spec(c("circularity", "roi_area")), c("include", "area"))
  expect_error(.cli_bridge_spec("name"), "takes circularity, roi_area or 'all'")
})

test_that("feature_stats reports n_bridge and frac_bridge", {
  .setup()
  source_r_scripts("feature_stats.r")

  on <- .run_group(.pinched_object(), bridge_roi = "area")
  on$feature_type <- "nucleus"
  on$sample <- "S1"

  st <- summarise_feature_stats(sf::st_as_sf(on))
  expect_identical(nrow(st), 1L)
  expect_identical(st$n_bridge, 3L)
  expect_equal(st$frac_bridge, 3 / 9)

  # n_roi counts the ordinary ROIs only, and z_span likewise ignores bridges --
  # so a threshold read off these numbers means the same thing as the same
  # number handed to min_z_span.
  expect_identical(st$n_roi, 6L)
  expect_identical(st$n_z, 6L)
})

test_that("a table written before bridging existed still summarises", {
  .setup()
  source_r_scripts("feature_stats.r")

  on <- .run_group(.pinched_object(), bridge_roi = "area")
  on$feature_type <- "nucleus"
  on$sample <- "S1"
  on$is_bridge <- NULL              # as an older _features.rds would be

  st <- summarise_feature_stats(sf::st_as_sf(on))
  expect_identical(st$n_bridge, 0L)
  expect_equal(st$frac_bridge, 0)
})
