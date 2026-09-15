source_r_scripts(c("FnGroup_roi_2_polygons.r", "find_ROI_z_intersect.r",
                   "define_feature_group.r", "find_overlap_roi_features.r"))

read_outline <- function(feature){
  p <- fixture_file(feature, "outline")
  skip_if_no_fixture(p)
  as_tibble(read.table(p, header = TRUE, sep = "\t", stringsAsFactors = FALSE)) %>%
    dplyr::select(-name)
}

# A closed unit square, in the shape roi_extract_xy_coord() expects.
square <- function(id, z = 1, ox = 0, oy = 0, w = 2){
  tibble(roi = id, z = z,
         x = ox + c(0, 0, w, w),
         y = oy + c(0, w, w, 0))
}

# Turning outlines into polygons ----------------------------------------------

test_that("every fixture ROI becomes exactly one valid polygon", {
  skip_if_no_sf()
  roi_df <- read_outline("nucleus")
  
  pg <- polygonize_roi_df(roi_df)
  
  expect_equal(nrow(pg), length(unique(roi_df$roi)))
  expect_true(all(st_is_valid(pg$geometry)))
  expect_false(any(duplicated(pg$roi)))
  expect_true(all(as.numeric(st_area(pg$geometry)) > 0))
})

test_that("a missing ROI yields no coordinates rather than a row of NAs", {
  skip_if_no_sf()
  roi_df <- square("a_0001-0001-0001")
  
  m <- roi_extract_xy_coord(roi_df, roi_id = "does_not_exist")
  
  # `.[1, ]` on a zero-row tibble returns a row of NAs, which used to sail past
  # the empty-ROI check and fail much later inside Polygon().
  expect_equal(nrow(m), 0L)
  expect_equal(colnames(m), c("x", "y"))
})

test_that("a degenerate ROI is dropped, and does not take another ROI's polygon", {
  skip_if_no_sf()
  roi_df <- bind_rows(
    square("good_0001-0001-0001"),
    tibble(roi = "bad_0001-0001-0002", z = 1, x = c(0, 1), y = c(0, 1)))
  
  expect_warning(pg <- polygonize_roi_df(roi_df), "too few coordinates")
  
  # The regression this guards: geometry used to be paired to ROI ids by
  # position, so dropping one ROI let tibble() recycle the survivor's polygon
  # onto it -- a wrong answer that raised nothing.
  expect_equal(nrow(pg), 1L)
  expect_equal(pg$roi, "good_0001-0001-0001")
  expect_equal(as.numeric(st_area(pg$geometry)), 4)
})

test_that("roi_2_polygons labels its geometry with the ROI id", {
  skip_if_no_sf()
  roi_df <- bind_rows(square("a_0001-0001-0001"), square("b_0001-0001-0002", ox = 10))
  
  pg <- roi_2_polygons(roi_df, roi_id_vec = c("a_0001-0001-0001", "b_0001-0001-0002"))
  expect_equal(pg$roi, c("a_0001-0001-0001", "b_0001-0001-0002"))
})

test_that("pg_2_coord_df returns input that already carries x/y", {
  skip_if_no_sf()
  df <- square("a_0001-0001-0001")
  # This branch referenced `feature_df`, which is not a parameter, and so raised
  # "object 'feature_df' not found" for every such input.
  expect_equal(pg_2_coord_df(df), df)
})

# Merging ROIs across z -------------------------------------------------------

test_that("z-intersect pairs respect max_z_dist and exclude self-pairs", {
  skip_if_no_sf()
  pg <- polygonize_roi_df(read_outline("nucleus"))
  
  ovl <- find_ROI_z_intersect(pg, max_z_dist = 1, min_intersect_ratio = 0)
  
  expect_gt(nrow(ovl), 0)
  expect_true(all(ovl$z_dist <= 1))
  expect_true(all(ovl$z_dist > 0))
  expect_true(all(ovl$roi_1 != ovl$roi_2))
  expect_true(all(ovl$int_ratio >= 0 & ovl$int_ratio <= 1 + 1e-9))
})

test_that("no overlap across z returns zero rows instead of erroring", {
  skip_if_no_sf()
  # Two squares far apart in x, one per slice: nothing intersects anything but
  # itself, and self-pairs are removed by the z_dist == 0 filter. The zero-row
  # table then reaches the intersect-area loop, where 1:nrow() used to count
  # c(1, 0) and index a row that is not there. This is the bug that was marked
  # in the source.
  roi_df <- bind_rows(square("a_0001-0001-0001", z = 1),
                      square("b_0002-0001-0002", z = 2, ox = 100))
  pg <- polygonize_roi_df(roi_df)
  
  expect_warning(ovl <- find_ROI_z_intersect(pg, max_z_dist = 1, min_intersect_ratio = 0),
                 "No ROI feature overlap")
  
  expect_equal(nrow(ovl), 0L)
  # Callers index these columns unconditionally, so an empty result still has to
  # carry the full set.
  expect_true(all(c("roi_1", "roi_2", "z_dist", "int_area", "int_ratio") %in% colnames(ovl)))
})

# Grouping ROIs into features -------------------------------------------------

test_that("fixture nuclei group into features covering every ROI once", {
  skip_if_no_sf()
  roi_df <- read_outline("nucleus")
  
  fg <- define_feature_group(roi_df, roi_regex = "^nucleus",
                             feature_prefix = "nucleus_",
                             invalid_feature_prefix = "invalid_",
                             max_z_dist = 3, min_z_span = 5)
  
  # Every input ROI is accounted for exactly once, valid or not.
  expect_equal(sort(fg$roi), sort(unique(roi_df$roi)))
  expect_false(any(duplicated(fg$roi)))
  expect_true(all(!is.na(fg$feature_id)))
  
  n_nuclei <- length(unique(subset(fg$feature_id, str_detect(fg$feature_id, "^nucleus_"))))
  expect_equal(n_nuclei, 6)
})

test_that("ROIs that never overlap do not crash feature grouping", {
  skip_if_no_sf()
  roi_df <- bind_rows(square("a_0001-0001-0001", z = 1),
                      square("b_0002-0001-0002", z = 2, ox = 100))
  
  # Empty edge set: the assignment loop must be skipped, not run over c(1, 0).
  fg <- suppressWarnings(define_feature_group(roi_df, roi_regex = "^[ab]_",
                                              feature_prefix = "f_", min_z_span = 1))
  
  expect_equal(nrow(fg), 2L)
  # With nothing overlapping, both ROIs land in the "overlap" fail bucket.
  expect_true(all(str_detect(fg$feature_id, "^failed_ROI_")))
})

# Feature-to-feature overlap --------------------------------------------------

test_that("nucleoli are matched to the nuclei that contain them", {
  skip_if_no_sf()
  nuc_roi <- read_outline("nucleus")
  nucl_roi <- read_outline("nucleolus")
  
  nuc_fg <- define_feature_group(nuc_roi, roi_regex = "^nucleus",
                                 feature_prefix = "nucleus_",
                                 invalid_feature_prefix = "invalid_nucleus_",
                                 max_z_dist = 3, min_z_span = 5) %>%
    dplyr::filter(str_detect(feature_id, "^nucleus_"))
  nucl_fg <- suppressWarnings(
    define_feature_group(nucl_roi, roi_regex = "^nucleolus",
                         feature_prefix = "nucleolus_",
                         invalid_feature_prefix = "invalid_nucleolus_",
                         max_z_dist = 1, min_z_span = 2)) %>%
    dplyr::filter(str_detect(feature_id, "^nucleolus_"))
  
  skip_if(nrow(nucl_fg) == 0, "no nucleolus feature survived grouping in this fixture")
  
  ovl <- find_overlap_roi_features(nuc_fg, nucl_fg,
                                   feature_1_regex = "^nucleus_",
                                   feature_2_regex = "^nucleolus_",
                                   min_intersect_ratio = 0.5,
                                   min_ratio_roi_overlap = 1)
  
  expect_true(all(c("feature_id_1", "feature_id_2") %in% colnames(ovl)))
  # A nucleolus sits inside exactly one nucleus.
  expect_false(any(duplicated(ovl$feature_id_2)))
})
