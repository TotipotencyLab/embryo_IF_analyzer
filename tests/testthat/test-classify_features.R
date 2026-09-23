# --class: put each feature in a named class from its own statistics.
#
# The two things worth pinning are that priority is the FLAG ORDER and that a
# feature fitting no class survives by default. Both are choices, and a choice
# nothing asserts is a choice that quietly changes.

.fake <- function() {
  data.frame(
    sample       = rep(c("S1", "S2"), each = 3),
    feature_type = "nucleus",
    feature_id   = paste0("nucleus_", 1:6),
    area_med     = c(100, 500, 1200, 150, 800, 2000),
    circ_med     = c(0.9, 0.8, 0.6, 0.85, 0.5, 0.55),
    ch1_signal   = c(2, 5, 20, 3, NA, 18),
    stringsAsFactors = FALSE)
}

.spec <- function(...) {
  source_cli("cli_helpers.r")
  .cli_class_spec(c(...))
}

test_that("a class is a set of ranges its members must all fall inside", {
  source_r_scripts("classify_features.r")
  out <- classify_features(.fake(), .spec("big:area_med=1000:Inf",
                                          "small:area_med=0:200"))
  expect_identical(out$class, c("small", "unclassified", "big",
                                "small", "unclassified", "big"))
})

test_that("several tokens for one class are ANDed", {
  source_r_scripts("classify_features.r")
  # Large AND round. nucleus_3 is large but circ 0.6, nucleus_6 large but 0.55.
  out <- classify_features(.fake(), .spec("biground:area_med=1000:Inf",
                                          "biground:circ_med=0.7:1"))
  # A real string, not NA: table(stats$class) drops NA without saying so, and
  # the obvious way to count a classified table then comes out short.
  expect_true(all(out$class == "unclassified"))
  expect_false(anyNA(out$class))

  loosened <- classify_features(.fake(), .spec("biground:area_med=1000:Inf",
                                               "biground:circ_med=0.5:1"))
  expect_identical(sum(loosened$class == "biground", na.rm = TRUE), 2L)
})

test_that("priority is the flag order, and the overlap is reported not hidden", {
  source_r_scripts("classify_features.r")
  big_first <- .spec("big:area_med=400:Inf", "round:circ_med=0.5:1")
  expect_warning(classify_features(.fake(), big_first),
                 "matched more than one class")
  expect_warning(classify_features(.fake(), big_first),
                 "priority is: big > round")

  a <- suppressWarnings(classify_features(.fake(), big_first))
  b <- suppressWarnings(classify_features(
    .fake(), .spec("round:circ_med=0.5:1", "big:area_med=400:Inf")))

  # Same data, same classes, different flag order -> different answer. That is
  # the whole contract, so it must be visible in a test.
  expect_false(identical(a$class, b$class))
  expect_identical(a$class[3], "big")
  expect_identical(b$class[3], "round")
})

test_that("no overlap means no warning", {
  # Otherwise the warning above could be firing on everything and still pass.
  source_r_scripts("classify_features.r")
  expect_no_warning(
    classify_features(.fake(), .spec("big:area_med=1000:Inf",
                                     "small:area_med=0:200")))
})

test_that("an orphan is kept by default and dropped only when asked", {
  source_r_scripts("classify_features.r")
  spec <- .spec("big:area_med=1000:Inf")

  kept <- classify_features(.fake(), spec)
  expect_identical(nrow(kept), 6L)
  expect_identical(sum(kept$class == "unclassified"), 4L)
  expect_identical(attr(kept, "n_orphan"), 4L)

  dropped <- classify_features(.fake(), spec, drop_orphan = TRUE)
  expect_identical(nrow(dropped), 2L)
  expect_false(any(dropped$class == "unclassified"))
  # The count survives the drop, so the run can still say what it removed.
  expect_identical(attr(dropped, "n_orphan"), 4L)
})

test_that("NA never matches a range", {
  # A feature whose ch1_signal is NA because no measurement table was found has
  # not been shown to be inside the range. Treating unknown as a match would
  # invent members of a class.
  source_r_scripts("classify_features.r")
  out <- classify_features(.fake(), .spec("bright:ch1_signal=0:Inf"))
  expect_identical(out$class[5], "unclassified")   # the NA-signal row
  expect_identical(sum(out$class == "bright", na.rm = TRUE), 5L)
})

test_that("classify_features refuses a column it cannot use", {
  source_r_scripts("classify_features.r")
  expect_error(classify_features(.fake(), .spec("x:nosuch=1:2")),
               "not in the stats table")
  expect_error(classify_features(.fake(), .spec("x:feature_id=1:2")),
               "needs numeric column")
})

test_that("no spec leaves the table exactly as it was", {
  source_r_scripts("classify_features.r")
  d <- .fake()
  expect_identical(classify_features(d, list()), d)
  expect_false("class" %in% colnames(classify_features(d, list())))
})

test_that("--class tokens are parsed into an ordered spec", {
  source_cli("cli_helpers.r")
  spec <- .cli_class_spec(c("growing:area_med=600:Inf", "growing:circ_med=0:0.7",
                            "small:area_med=0:600"))
  expect_identical(names(spec), c("growing", "small"))
  expect_identical(names(spec$growing), c("area_med", "circ_med"))
  expect_equal(spec$growing$area_med, c(600, Inf))
  expect_equal(spec$small$area_med, c(0, 600))
})

test_that("a malformed --class token is refused with the form spelled out", {
  source_cli("cli_helpers.r")
  # No class name: the first ':' is the range's, so this would otherwise become
  # a class called "area_med=400".
  expect_error(.cli_class_spec("area_med=400:Inf"), "missing the class name")
  expect_error(.cli_class_spec("growing:area_med"), "column=lo:hi")
  expect_error(.cli_class_spec("growing:area_med=hi:lo"), "not numeric")
  expect_error(.cli_class_spec("growing:area_med=900:100"), "lo > hi")
  # A repeated column would silently replace the first condition.
  expect_error(.cli_class_spec(c("g:area_med=1:2", "g:area_med=3:4")),
               "twice for class")
})

test_that("class_counts lists classes in priority order, orphans last", {
  source_r_scripts("classify_features.r")
  out <- classify_features(.fake(), .spec("big:area_med=1000:Inf",
                                          "small:area_med=0:200"))
  cc <- class_counts(out)
  expect_identical(cc$class, c("big", "small", "unclassified"))
  expect_identical(cc$n, c(2L, 2L, 2L))
})

test_that("the CLI writes a class column and can group plots by it", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))

  out <- withr::local_tempdir()
  st <- suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", out, "--res_dir", fixture_dir(),
    "--class", "big:area_med=100:Inf", "small:area_med=0:100",
    "--group_by", "class", "--plot_type", "box")))

  expect_true("class" %in% colnames(st))
  expect_true(all(st$class %in% c("big", "small", NA)))
  # --group_by resolves against the column this step creates, so it has to be
  # classified before that check runs.
  expect_true(file.exists(file.path(out, "feature_stats.pdf")))

  tsv <- utils::read.delim(file.path(out, "feature_stats.tsv"))
  expect_true("class" %in% colnames(tsv))
})

test_that("--drop_orphan_feature without --class says so rather than passing", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))
  out <- withr::local_tempdir()
  expect_warning(
    suppressMessages(feature_stat_cli(c(
      "--input", feat, "--outdir", out, "--res_dir", fixture_dir(),
      "--no_plot", "--drop_orphan_feature"))),
    "does nothing without --class")
})

test_that("a reserved name cannot be used as a class", {
  source_r_scripts("classify_features.r")
  # Without this, --class 'other:...' collides with the group the plots fold
  # unmapped classes into, and one label would mean two different things.
  expect_error(classify_features(.fake(), .spec("other:area_med=0:Inf")),
               "reserved name")
  expect_error(classify_features(.fake(), .spec("unclassified:area_med=0:Inf")),
               "reserved name")
  # The message has to say where to go instead.
  expect_error(classify_features(.fake(), .spec("other:area_med=0:Inf")),
               "--color_map")
})
