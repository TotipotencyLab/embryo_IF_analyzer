source_r_scripts("read_fiji_result.r")

# The output contract ---------------------------------------------------------
# read_fiji_result() joins measurements to outlines through the roi id embedded
# in the Label column. Every number in the table can be correct while that join
# quietly yields nothing, so these tests assert the join, not just the parse.

test_that("the roi id is recovered for every measurement row", {
  p <- fixture_file("nucleus", "res")
  skip_if_no_fixture(p)
  
  res <- read_fiji_result(p)
  
  expect_true("roi" %in% colnames(res))
  # The failure this guards against: a label-format change leaves `roi` all NA
  # and every downstream left_join() silently returns zero matched rows.
  expect_false(any(is.na(res$roi)))
  expect_true(all(str_detect(res$roi, "\\d{4}-\\d{4}-\\d{4}$")))
})

test_that("reading a table neither adds nor drops rows", {
  p <- fixture_file("nucleus", "res")
  skip_if_no_fixture(p)
  
  raw <- read.table(p, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  res <- read_fiji_result(p)
  
  # A many-to-many join against the label lookup would inflate this silently.
  expect_equal(nrow(res), nrow(raw))
})

test_that("nucleus and nucleolus tables hold only their own feature", {
  pn <- fixture_file("nucleus", "res")
  pl <- fixture_file("nucleolus", "res")
  skip_if_no_fixture(pn); skip_if_no_fixture(pl)
  
  nuc <- read_fiji_result(pn)
  nucl <- read_fiji_result(pl)
  
  # Since the Groovy port each feature writes its own files. Code that used to
  # read the nucleolus table and get nuclei for free must now read both.
  expect_true(all(str_detect(nuc$roi, "^nucleus_")))
  expect_true(all(str_detect(nucl$roi, "^nucleolus_")))
  expect_length(intersect(unique(nuc$roi), unique(nucl$roi)), 0)
})

test_that("measurement columns match the forced Set Measurements list", {
  p <- fixture_file("nucleus", "res")
  skip_if_no_fixture(p)
  
  res <- read_fiji_result(p)
  # Fiji's Set Measurements is a persistent user preference; the Groovy scripts
  # force it so these columns do not depend on the operator's machine.
  expect_true(all(c("area", "mean", "stddev", "min", "max", "x", "y", "circ",
                    "intden", "median", "rawintden", "ch", "slice",
                    "ar", "round", "solidity") %in% colnames(res)))
})

test_that("each ROI is measured once per channel", {
  p <- fixture_file("nucleus", "res")
  skip_if_no_fixture(p)
  
  res <- read_fiji_result(p)
  per_roi <- res %>% count(roi, name = "n")
  expect_equal(length(unique(per_roi$n)), 1L)
  expect_equal(nrow(res), length(unique(res$roi)) * unique(per_roi$n))
})

# Field types -----------------------------------------------------------------

test_that("z comes back numeric", {
  p <- fixture_file("nucleus", "res")
  skip_if_no_fixture(p)
  
  res <- read_fiji_result(p)
  # As character, abs(z1 - z2) in find_ROI_z_intersect() fails outright and
  # sort() puts "10" before "2".
  expect_type(res$z, "double")
  expect_true(all(res$z >= 1))
  expect_equal(res$z, as.numeric(res$slice))
})

test_that("the image filename is recovered from the label", {
  p <- tempfile(fileext = ".txt")
  # Fiji names a series pulled out of a .lif "<file>.lif-Position010-1.tif",
  # so the extension sits in the middle of the token, not at the end.
  write_res_table(p, c(make_label(roi = "nucleus_0001-0001-0433"),
                       make_label(roi = "nucleus_0001-0002-0508")))
  
  res <- expect_no_warning(read_fiji_result(p))
  expect_true("filename" %in% colnames(res))
  expect_equal(unique(res$filename), "20241216_dkD.lif-Position010-1.tif")
})

test_that("a plain .lif label still resolves", {
  p <- tempfile(fileext = ".txt")
  write_res_table(p, c(make_label(image = "20241216_dkD.lif", roi = "nucleus_0001-0001-0433"),
                       make_label(image = "20241216_dkD.lif", roi = "nucleus_0001-0002-0508")))
  
  res <- expect_no_warning(read_fiji_result(p))
  expect_equal(unique(res$filename), "20241216_dkD.lif")
})

# Failing loudly --------------------------------------------------------------

test_that("an unmatched field warns instead of vanishing", {
  p <- tempfile(fileext = ".txt")
  # No Position token anywhere in the label.
  write_res_table(p, c(make_label(roi = "nucleus_0001-0001-0433", tail = "Series 001"),
                       make_label(roi = "nucleus_0001-0002-0508", tail = "Series 001")))
  
  # Assigning to a zero-length index is a silent no-op in R, so this case used
  # to drop the column with nothing reported anywhere.
  expect_warning(read_fiji_result(p), "No column matched the expected pattern for 'pos'")
})

test_that("an empty measurement table warns and returns no rows", {
  p <- tempfile(fileext = ".txt")
  write_res_table(p, make_label())
  writeLines(readLines(p)[1], p)   # header only
  
  expect_warning(res <- read_fiji_result(p), "empty")
  expect_equal(nrow(res), 0L)
})

test_that("labels of differing token depth warn rather than half-fill a column", {
  p <- tempfile(fileext = ".txt")
  # This matching is positional: the label is split on / : and whitespace, and a
  # pattern has to hold for a whole column. When two images in one table produce
  # labels of different token depth, "Position010" lands in different columns
  # and no single column matches.
  write_res_table(p, c(make_label(roi = "nucleus_0001-0001-0433", tail = "Position010"),
                       make_label(roi = "nucleus_0001-0002-0508",
                                  tail = "Lightning 001/Mark_and_Find 001/Position010")))
  
  # The honest outcome is a warning and a missing column. Matching such a column
  # anyway would give "" for some rows -- silently wrong, and worse than absent.
  expect_warning(res <- read_fiji_result(p),
                 "No column matched the expected pattern for 'pos'")
  
  # The roi id is positionally stable, so the join still holds.
  expect_false(any(is.na(res$roi)))
})
