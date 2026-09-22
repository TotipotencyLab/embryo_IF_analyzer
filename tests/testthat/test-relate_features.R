# Containment relationships between features.
#
# The fixture only exercises the easy path -- every nucleolus there sits wholly
# inside its nucleus, on slices the nucleus was also detected on, so every
# assignment comes back containment = 1, match = "direct". Everything
# interesting (partial overlap, a missing parent slice, a tie, an orphan) is
# synthesised here, because a test that could not have failed proves nothing.

# --- builders -----------------------------------------------------------------

sq <- function(x0, y0, s = 10){
  sf::st_polygon(list(cbind(c(x0, x0 + s, x0 + s, x0, x0),
                            c(y0, y0, y0 + s, y0 + s, y0))))
}

feat <- function(feature_id, feature_type, zs, geom_fn, sample = "S1"){
  sf::st_sf(feature_id   = feature_id,
            feature_type = feature_type,
            z            = zs,
            sample       = sample,
            geometry     = sf::st_sfc(lapply(zs, geom_fn)))
}

stack_of <- function(...){
  parts <- list(...)
  out <- do.call(rbind, parts)
  return(out)
}

parent_of <- function(rel, child_id){
  d <- sf::st_drop_geometry(rel)
  unique(d$parent_feature_id[d$feature_id == child_id])
}
detail_of <- function(rel, child_id, col){
  d <- sf::st_drop_geometry(rel)
  unique(d[[col]][d$feature_id == child_id])
}

# --- spec validation ----------------------------------------------------------

test_that("validate_within_spec accepts a sane forest", {
  source_r_scripts("relate_features.r")
  spec <- c(nucleolus = "nucleus", nucleus = "cell")
  expect_identical(validate_within_spec(spec), spec)
  expect_identical(validate_within_spec(character(0)), character(0))
})

test_that("validate_within_spec rejects nonsense", {
  source_r_scripts("relate_features.r")
  expect_error(validate_within_spec(c(a = "a")), "cannot contain itself")
  expect_error(validate_within_spec(c(a = "b", a = "c")), "two parents")
  expect_error(validate_within_spec(c(a = "b", b = "a")), "circular")
  expect_error(validate_within_spec(c(a = "b", b = "c", c = "a")), "circular")
  expect_error(validate_within_spec(setNames("b", "")), "must be named")
  expect_error(validate_within_spec(c(nucleolus = "cell"), known = c("nucleolus", "nucleus")),
               "not annotated")
})

# --- the straightforward case -------------------------------------------------

test_that("a child wholly inside its parent scores 1 and matches directly", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:8, function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 6:7, function(z) sq(40, 40, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))

  expect_identical(parent_of(rel, "nucleolus_1"), "nucleus_1")
  expect_equal(detail_of(rel, "nucleolus_1", "parent_containment"), 1)
  expect_identical(detail_of(rel, "nucleolus_1", "parent_match"), "direct")
  expect_identical(detail_of(rel, "nucleolus_1", "parent_feature_type"), "nucleus")

  # The parent itself is not given a parent, and is not modified.
  expect_true(is.na(parent_of(rel, "nucleus_1")))
  expect_identical(nrow(rel), nrow(x))
})

# --- imperfect outlines -------------------------------------------------------

test_that("containment is the fraction of the CHILD inside the parent", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # Parent spans x 0-100. Child spans x 95-105, so exactly half of it is inside.
  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:6, function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 5:6, function(z) sq(95, 40, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"), min_containment = 0.4)
  expect_equal(detail_of(rel, "nucleolus_1", "parent_containment"), 0.5, tolerance = 1e-9)
  expect_identical(parent_of(rel, "nucleolus_1"), "nucleus_1")
})

test_that("min_containment is enforced, and is per-child when named", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:6, function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 5:6, function(z) sq(95, 40, 10))   # 50% inside
  )
  strict <- assign_feature_parent(x, within = c(nucleolus = "nucleus"), min_containment = 0.6)
  expect_true(is.na(parent_of(strict, "nucleolus_1")))

  loose <- assign_feature_parent(x, within = c(nucleolus = "nucleus"), min_containment = 0.4)
  expect_identical(parent_of(loose, "nucleolus_1"), "nucleus_1")

  named <- assign_feature_parent(x, within = c(nucleolus = "nucleus"),
                                 min_containment = c(default = "0.9", nucleolus = "0.4"))
  expect_identical(parent_of(named, "nucleolus_1"), "nucleus_1")
})

test_that("a child entirely outside any parent is orphaned, not forced in", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:6, function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 5:6, function(z) sq(500, 500, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))
  expect_true(is.na(parent_of(rel, "nucleolus_1")))
  expect_true(is.na(detail_of(rel, "nucleolus_1", "parent_match")))
})

# --- the missing parent slice -------------------------------------------------

test_that("a child on a slice the parent missed is still placed, and marked", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # The case from the design discussion: nucleus at z = 5,6,8; nucleolus at z = 7.
  x <- stack_of(
    feat("nucleus_1",   "nucleus",   c(5, 6, 8), function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 7,          function(z) sq(40, 40, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))

  expect_identical(parent_of(rel, "nucleolus_1"), "nucleus_1")
  expect_equal(detail_of(rel, "nucleolus_1", "parent_containment"), 1)
  # Marked, so a run leaning heavily on this is visible rather than silent.
  expect_identical(detail_of(rel, "nucleolus_1", "parent_match"), "gap_filled")
})

test_that("gap filling does not reach outside the parent's z-range", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # parent_z_pad is deliberately NOT implemented: z = 9 is one slice past the
  # parent's last, and must not match. If a pad is ever added, this test is
  # where the change becomes visible.
  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:8, function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 9,   function(z) sq(40, 40, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))
  expect_true(is.na(parent_of(rel, "nucleolus_1")))
})

test_that("gap filling does not rewrite the parent", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  x <- stack_of(
    feat("nucleus_1",   "nucleus",   c(5, 6, 8), function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 7,          function(z) sq(40, 40, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))

  before <- x[x$feature_id == "nucleus_1", ]
  after  <- rel[rel$feature_id == "nucleus_1", ]
  expect_identical(nrow(after), nrow(before))          # no slice invented
  expect_identical(sort(after$z), sort(before$z))
  expect_equal(as.numeric(sf::st_area(after)), as.numeric(sf::st_area(before)))
})

# --- ambiguity ----------------------------------------------------------------

test_that("a child split between two parents warns and takes the larger share", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # Two parents meeting at x = 100. Child spans x 94-104: 60% left, 40% right.
  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:6, function(z) sq(0, 0, 100)),
    feat("nucleus_2",   "nucleus",   5:6, function(z) sq(100, 0, 100)),
    feat("nucleolus_1", "nucleolus", 5:6, function(z) sq(94, 40, 10))
  )
  expect_warning(
    assign_feature_parent(x, within = c(nucleolus = "nucleus"),
                          min_containment = 0.3, tie_margin = 0.5),
    "lies in two nucleus features")

  rel <- suppressWarnings(
    assign_feature_parent(x, within = c(nucleolus = "nucleus"),
                          min_containment = 0.3, tie_margin = 0.5))
  expect_identical(parent_of(rel, "nucleolus_1"), "nucleus_1")
  expect_equal(detail_of(rel, "nucleolus_1", "parent_containment"), 0.6, tolerance = 1e-9)
})

test_that("a clear winner does not warn", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")
  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:6, function(z) sq(0, 0, 100)),
    feat("nucleus_2",   "nucleus",   5:6, function(z) sq(200, 0, 100)),
    feat("nucleolus_1", "nucleolus", 5:6, function(z) sq(40, 40, 10))
  )
  expect_no_warning(assign_feature_parent(x, within = c(nucleolus = "nucleus")))
})

test_that("z separates parents that overlap in x-y", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # The fixture's real situation: two nuclei sharing x-y, far apart in z.
  # Flattened to 2D the child would be ambiguous; per-slice it is not.
  x <- stack_of(
    feat("nucleus_1",   "nucleus",   1:5,   function(z) sq(0, 0, 100)),
    feat("nucleus_2",   "nucleus",   30:35, function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 32:33, function(z) sq(40, 40, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))
  expect_identical(parent_of(rel, "nucleolus_1"), "nucleus_2")
})

# --- what is not a candidate --------------------------------------------------

test_that("invalid_ and failed_ groups are not parents", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # An ROI that failed the z-span filter is not a nucleus and must not adopt.
  x <- stack_of(
    feat("invalid_nucleus_1", "nucleus",   5:6, function(z) sq(0, 0, 100)),
    feat("nucleolus_1",       "nucleolus", 5:6, function(z) sq(40, 40, 10))
  )
  expect_warning(assign_feature_parent(x, within = c(nucleolus = "nucleus")),
                 "No valid nucleus features")
  rel <- suppressWarnings(assign_feature_parent(x, within = c(nucleolus = "nucleus")))
  expect_true(is.na(parent_of(rel, "nucleolus_1")))
})

test_that("samples are kept apart", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  # Same coordinates in two images. A nucleolus must not adopt a nucleus from a
  # different image just because the pixels line up.
  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:6, function(z) sq(0, 0, 100),  sample = "A"),
    feat("nucleolus_1", "nucleolus", 5:6, function(z) sq(40, 40, 10), sample = "B")
  )
  rel <- suppressWarnings(assign_feature_parent(x, within = c(nucleolus = "nucleus")))
  d <- sf::st_drop_geometry(rel)
  expect_true(all(is.na(d$parent_feature_id[d$feature_type == "nucleolus"])))
})

# --- summary ------------------------------------------------------------------

test_that("summarise_parent_children counts children per parent", {
  skip_if_no_sf()
  source_r_scripts("relate_features.r")

  x <- stack_of(
    feat("nucleus_1",   "nucleus",   5:8, function(z) sq(0, 0, 100)),
    feat("nucleolus_1", "nucleolus", 5:6, function(z) sq(10, 10, 10)),
    feat("nucleolus_2", "nucleolus", 5:6, function(z) sq(40, 40, 10))
  )
  rel <- assign_feature_parent(x, within = c(nucleolus = "nucleus"))
  s <- summarise_parent_children(rel, "nucleolus")

  expect_identical(nrow(s), 1L)
  expect_identical(s$n_child, 2L)
  expect_identical(s$n_gap_filled, 0L)
})

# --- the fixture --------------------------------------------------------------

test_that("every nucleolus in the fixture lands in a nucleus", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleolus", "outline"))
  source_cli("annotate_features_cli.r")

  out <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(),
    "--feature", "nucleus", "nucleolus",
    "--outdir", out,
    "--max_z_dist", "default=3", "nucleolus=1",
    "--min_z_span", "default=5", "nucleolus=2",
    "--within", "nucleolus=nucleus")))

  x <- readRDS(file.path(out, "GRV_Position010_features.rds"))
  d <- sf::st_drop_geometry(x)
  per <- unique(d[grepl("^nucleolus_", d$feature_id),
                  c("feature_id", "parent_feature_id", "parent_containment")])

  expect_identical(nrow(per), 7L)
  expect_true(all(!is.na(per$parent_feature_id)))       # no orphans
  expect_true(all(per$parent_containment > 0.99))       # all wholly inside

  # One nucleus carries three nucleoli; nucleus_1 (the z 1-5 object) carries none.
  counts <- table(per$parent_feature_id)
  expect_identical(max(as.integer(counts)), 3L)
  expect_false("nucleus_1" %in% names(counts))
})

test_that("--require_parent drops orphans only when asked", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleolus", "outline"))
  source_cli("annotate_features_cli.r")

  # Force orphans by demanding containment the data cannot reach.
  args <- c("--input", fixture_dir(), "--feature", "nucleus", "nucleolus",
            "--max_z_dist", "default=3", "nucleolus=1",
            "--min_z_span", "default=5", "nucleolus=2",
            "--within", "nucleolus=nucleus", "--min_containment", "default=1.1")

  keep <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(args, "--outdir", keep)))
  k <- sf::st_drop_geometry(readRDS(file.path(keep, "GRV_Position010_features.rds")))
  expect_true(any(grepl("^nucleolus_", k$feature_id)))            # kept, unassigned
  expect_true(all(is.na(k$parent_feature_id[grepl("^nucleolus_", k$feature_id)])))

  drop <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(args, "--outdir", drop, "--require_parent")))
  dd <- sf::st_drop_geometry(readRDS(file.path(drop, "GRV_Position010_features.rds")))
  expect_false(any(grepl("^nucleolus_", dd$feature_id)))          # gone
  expect_true(any(grepl("^nucleus_", dd$feature_id)))             # parents untouched
})
