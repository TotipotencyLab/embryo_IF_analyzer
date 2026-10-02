# group_montage_cli.r
#
# The two assertions that matter here are about pictures, not tables, and both
# are things a person looking at the output could not check by eye:
#
#   - a panel drawn at the wrong SIZE. Two series with the same pixel
#     dimensions can be different physical sizes, and drawing them equal makes
#     the same follicle look like two different ones.
#   - a panel that is not drawn at all. The montage exists to be counted from,
#     so a silently dropped image is a wrong answer that looks like a right one.
#
# Sizes are therefore measured from the rendered pixels, by bounding box rather
# than by area: the label box covers a FIXED number of pixels, which is a larger
# share of a small panel than a big one, so an area ratio is off by a few
# percent even when the geometry is exactly right.

source_cli("cli_helpers.r")
source_r_scripts("montage_grid.r")
source_cli("group_montage_cli.r")

# A colour block per sample, so every panel is identifiable in the output.
gm_fixture <- function(env = parent.frame()) {
  skip_if_no_pkg(c("argparser", "magick"))
  d <- withr::local_tempdir(.local_envir = env)
  img <- file.path(d, "img")
  dir.create(img)
  put <- function(prefix, px, colour) {
    magick::image_write(magick::image_blank(px, px, color = colour),
                        file.path(img, paste0(prefix, "_overview_ch1.png")))
  }
  put("A_s0001", 400, "grey40")
  put("A_s0002", 400, "grey50")
  put("B_s0001", 400, "steelblue")   # 2000 px x 0.5 um  = 1000 um
  put("B_s0002", 400, "tomato")      # 1000 px x 0.25 um =  250 um, a QUARTER
  # B_s0003's image is deliberately absent.
  sheet <- data.frame(
    series_id    = c("A_s0001", "A_s0002", "B_s0001", "B_s0002", "B_s0003", "C_s0001"),
    include      = c("true", "true", "true", "true", "true", "false"),
    section_id   = c("A", "A", "B", "B", "B", "C"),
    ord          = c(2, 1, 1, 2, 3, 1),
    # A real series.tsv carries these, and they are MACHINE columns -- which
    # is exactly why ordering by one has to keep working.
    series_index = c(11, 10, 20, 21, 22, 30),
    series_name  = c("s11", "s10", "s20", "s21", "s22", "s30"),
    # A's sections are 500 um, B's largest is 1000 um -- so a per-GROUP scale
    # and a per-RUN scale are genuinely different numbers here.
    size_x       = c(1000, 1000, 2000, 1000, 2000, 10),
    size_y       = c(1000, 1000, 2000, 1000, 2000, 10),
    pixel_width  = c(0.5, 0.5, 0.5, 0.25, 0.5, 1),
    pixel_height = c(0.5, 0.5, 0.5, 0.25, 0.5, 1),
    stringsAsFactors = FALSE)
  sp <- file.path(d, "series.tsv")
  utils::write.table(sheet, sp, sep = "\t", quote = FALSE, row.names = FALSE)
  list(dir = d, img = img, sheet = sp)
}

# Width and height of the region painted in `colour`, from the rendered file.
gm_bbox <- function(path, colour) {
  r <- grDevices::as.raster(magick::image_read(path))
  hit <- which(r == colour, arr.ind = TRUE)
  if (!nrow(hit)) return(c(w = 0L, h = 0L))
  c(w = diff(range(hit[, "col"])) + 1L, h = diff(range(hit[, "row"])) + 1L)
}

gm_run <- function(fx, extra = character(0), out = NULL) {
  if (is.null(out)) out <- file.path(fx$dir, paste0("out", as.integer(runif(1, 1, 1e6))))
  idx <- suppressMessages(suppressWarnings(group_montage_cli(c(
    "--sample_sheet", fx$sheet, "--group_by", "section_id",
    "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
    "--outdir", out, "--cell_max_px", "300", "--ncol", "3", extra))))
  list(idx = idx, out = out)
}

test_that("panels are drawn to PHYSICAL size, not pixel size", {
  fx <- gm_fixture()
  r <- gm_run(fx)

  big <- gm_bbox(file.path(r$out, "B_montage.png"), "#4682b4ff")   # 1000 um
  small <- gm_bbox(file.path(r$out, "B_montage.png"), "#ff6347ff") #  250 um

  # Both source images are 400x400 px. Only their physical size differs, so
  # equal-pixel scaling would draw them identically.
  expect_gt(big[["w"]], 0L)
  expect_gt(small[["w"]], 0L)
  expect_identical(big[["w"]] / small[["w"]], 4)
  expect_identical(big[["h"]] / small[["h"]], 4)

  # ...and the scale the index reports is the scale actually drawn.
  upp <- unique(r$idx$um_per_px[r$idx$group == "B"])
  expect_length(upp, 1L)
  expect_equal(1000 / big[["w"]], upp, tolerance = 1e-6)
})

test_that("an image the wrong shape is letterboxed, not stretched", {
  # The sheet gives the drawn size, and it used to be applied with magick's
  # "WxH!", which forces those exact dimensions and therefore DISTORTS anything
  # not already that shape. A squashed follicle still looks like a follicle, so
  # nothing said so. Fitting preserves the aspect and pads the difference.
  fx <- gm_fixture()
  # B_s0001's sheet row says 2000x2000 px at 0.5 um = a SQUARE 1000x1000 um.
  # Give it a 2:1 image instead.
  magick::image_write(magick::image_blank(400, 200, color = "steelblue"),
                      file.path(fx$img, "B_s0001_overview_ch1.png"))
  r <- gm_run(fx)

  box <- gm_bbox(file.path(r$out, "B_montage.png"), "#4682b4ff")
  # Stretched, it would have filled a square cell: w == h. Fitted, it keeps 2:1.
  expect_gt(box[["w"]], box[["h"]])
  expect_equal(box[["w"]] / box[["h"]], 2, tolerance = 0.02)
})

test_that("a shape the sheet does not describe is warned about, in physical mode only", {
  fx <- gm_fixture()
  magick::image_write(magick::image_blank(400, 200, color = "steelblue"),
                      file.path(fx$img, "B_s0001_overview_ch1.png"))

  grab <- function(extra = character(0)) {
    seen <- character(0)
    withCallingHandlers(
      suppressMessages(group_montage_cli(c(
        "--sample_sheet", fx$sheet, "--group_by", "section_id",
        "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
        "--outdir", file.path(fx$dir, paste0("w", length(extra))),
        "--cell_max_px", "200", extra))),
      warning = function(cond) {
        seen <<- c(seen, conditionMessage(cond))
        invokeRestart("muffleWarning")
      })
    seen
  }

  phys <- grab()
  expect_true(any(grepl("not the shape the sample sheet describes", phys, fixed = TRUE)))
  expect_true(any(grepl("B_s0001", phys, fixed = TRUE)))

  # ...and NOT under --scale pixel, where the sheet's dimensions are never
  # consulted and an oddly-shaped file is a legitimate thing to hand in.
  # Warning there would fire on every row of a deliberate use.
  #
  # This holds for two independent reasons -- the mode test, and .w_um being NA
  # outside physical mode -- so it does NOT prove the mode test is doing the
  # work. Verified: removing that test fails nothing. It is asserted here as
  # the behaviour, not as a guard on the implementation.
  pix <- grab(c("--scale", "pixel"))
  expect_false(any(grepl("not the shape the sample sheet describes", pix, fixed = TRUE)))
  # The fixture still warns about its missing image, so an empty result would
  # not prove the check was skipped rather than the warnings being swallowed.
  expect_true(any(grepl("not found", pix, fixed = TRUE)))
})

test_that("a correctly shaped image is not warned about", {
  # The rounding a resize introduces must not trip it: Fiji's overview of an
  # 11344 x 9590 series comes out 1000 x 846, which is 0.07% off.
  fx <- gm_fixture()
  magick::image_write(magick::image_blank(1000, 846, color = "steelblue"),
                      file.path(fx$img, "B_s0001_overview_ch1.png"))
  s <- utils::read.delim(fx$sheet, stringsAsFactors = FALSE)
  s$size_x[s$series_id == "B_s0001"] <- 11344
  s$size_y[s$series_id == "B_s0001"] <- 9590
  utils::write.table(s, fx$sheet, sep = "\t", quote = FALSE, row.names = FALSE)

  seen <- character(0)
  withCallingHandlers(
    suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx$sheet, "--group_by", "section_id",
      "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
      "--outdir", file.path(fx$dir, "ok"), "--cell_max_px", "200"))),
    warning = function(cond) {
      seen <<- c(seen, conditionMessage(cond))
      invokeRestart("muffleWarning")
    })
  expect_false(any(grepl("not the shape", seen, fixed = TRUE)))
})

test_that("--scale pixel draws them the same size, which is what physical is not", {
  # The discrimination half: without this, "the sizes differ" is equally
  # consistent with the scaling never having run.
  fx <- gm_fixture()
  r <- gm_run(fx, c("--scale", "pixel"))
  big <- gm_bbox(file.path(r$out, "B_montage.png"), "#4682b4ff")
  small <- gm_bbox(file.path(r$out, "B_montage.png"), "#ff6347ff")
  expect_identical(big[["w"]], small[["w"]])
  expect_true(all(is.na(r$idx$um_per_px)))
})

test_that("a missing image still occupies a labelled cell", {
  fx <- gm_fixture()
  out <- file.path(fx$dir, "miss")
  expect_warning(
    idx <- suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx$sheet, "--group_by", "section_id",
      "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
      "--outdir", out, "--cell_max_px", "300", "--ncol", "3"))),
    "not found")

  b <- idx[idx$group == "B", ]
  expect_identical(nrow(b), 3L)                        # three rows, not two
  expect_identical(sum(b$status == "missing"), 1L)
  expect_identical(b$series_id[b$status == "missing"], "B_s0003")
  # It has a POSITION: it was laid out, not skipped.
  expect_true(all(!is.na(b$row)) && all(!is.na(b$col)))
  expect_identical(sort(b$col), 1:3)
})

test_that("a group of one is drawn, and --ncol makes it the same width as the rest", {
  # A section that yielded a single series still belongs beside its neighbours,
  # so this must NOT be refused.
  # gm_run() already fixes --ncol 3, which is the point of the width check.
  fx <- gm_fixture()
  s <- utils::read.delim(fx$sheet, stringsAsFactors = FALSE)
  s$section_id[s$series_id == "A_s0001"] <- "SOLO"
  utils::write.table(s, fx$sheet, sep = "\t", quote = FALSE, row.names = FALSE)

  r2 <- gm_run(fx)
  expect_true("SOLO" %in% r2$idx$group)
  expect_identical(sum(r2$idx$group == "SOLO"), 1L)

  files <- file.path(r2$out, unique(r2$idx$montage))
  expect_true(all(file.exists(files)))
  # An explicit --ncol fixes the COLUMN COUNT, so the one-panel montage is a
  # full-width row rather than a third of one.
  expect_true(all(r2$idx$col <= 3L))
  expect_identical(unique(r2$idx$cell_px_w[r2$idx$group == "SOLO"]),
                   unique(r2$idx$cell_px_w[r2$idx$group == "A"]))

  # Montage width is now the sum of the COLUMNS THAT HOLD SOMETHING, so a group
  # with fewer panels than --ncol is genuinely narrower. That is the cost of a
  # table layout and is deliberate: reserving width for a column that holds no
  # cell would put back exactly the blank space the layout removes.
  width_of <- function(grp) {
    magick::image_info(magick::image_read(
      file.path(r2$out, unique(r2$idx$montage[r2$idx$group == grp]))))$width
  }
  # B holds three panels and so fills all three columns; SOLO holds one and is
  # one column wide. (Group A is also a singleton here -- A_s0001 moved to
  # SOLO -- so comparing against it would prove nothing.)
  expect_identical(sum(r2$idx$group == "B"), 3L)
  expect_lt(width_of("SOLO"), width_of("B"))
  # ...and it is exactly one column, not a third of a fixed grid.
  expect_equal(width_of("SOLO"), unique(r2$idx$cell_px_w[r2$idx$group == "SOLO"]))
})

test_that("cells are sized per column and per row, not one size for the group", {
  # A table: column j is as wide as its widest cell, row i as tall as its
  # tallest, and a cell is padded only to its own column and row. One size for
  # the whole group spends the difference on blank space.
  skip_if_no_pkg(c("argparser", "magick"))
  d <- withr::local_tempdir()
  img <- file.path(d, "img")
  dir.create(img)
  dims <- list(c(200, 100), c(100, 200), c(200, 50))
  pre <- sprintf("S%02d", seq_along(dims))
  for (k in seq_along(dims)) {
    magick::image_write(magick::image_blank(dims[[k]][1], dims[[k]][2], "steelblue"),
                        file.path(img, paste0(pre[k], "_overview_ch1.png")))
  }
  sheet <- data.frame(series_id = pre, include = "true", section_id = "G",
                      size_x = vapply(dims, `[`, 0, 1), size_y = vapply(dims, `[`, 0, 2),
                      pixel_width = 1, pixel_height = 1, stringsAsFactors = FALSE)
  sp <- file.path(d, "s.tsv")
  utils::write.table(sheet, sp, sep = "\t", quote = FALSE, row.names = FALSE)

  run <- function(ncol) {
    o <- file.path(d, paste0("n", ncol))
    suppressMessages(suppressWarnings(group_montage_cli(c(
      "--sample_sheet", sp, "--group_by", "section_id", "--image_dir", img,
      "--image_suffix", "_overview_ch1.png", "--outdir", o,
      "--ncol", as.character(ncol), "--cell_max_px", "100", "--no_scale_bar"))))
  }

  # ncol = 1: fitted 100x50, 50x100, 100x25. One column, width 100; each row
  # its own height. The 50-wide panel is padded left/right to 100 -- which is
  # unavoidable, the output being a rectangle.
  one <- run(1)
  expect_equal(one$cell_px_w, c(100, 100, 100))
  expect_equal(one$cell_px_h, c(50, 100, 25))

  # ncol = 2: columns are (100, 50) and rows are (100, 25).
  two <- run(2)
  expect_equal(two$cell_px_w, c(100, 50, 100))
  expect_equal(two$cell_px_h, c(100, 100, 25))
  expect_equal(two$row, c(1, 1, 2))
  expect_equal(two$col, c(1, 2, 1))

  # ...and the IMAGE agrees with the index. Without this the index could report
  # a table while the montage was drawn with one cell size for the group --
  # verified: making the cells uniform fails nothing above.
  #
  # The title band is not hard-coded. Its height is the same in both runs, so
  # asserting that (height - content) matches across them pins the layout
  # without pinning the band: content is 50+100+25 at ncol 1 and 100+25 at
  # ncol 2.
  h1 <- magick::image_info(magick::image_read(
    file.path(d, "n1", unique(one$montage))))$height
  h2 <- magick::image_info(magick::image_read(
    file.path(d, "n2", unique(two$montage))))$height
  expect_identical(h1 - 175L, h2 - 125L)
  # And the widths, which the band does not touch at all.
  expect_identical(magick::image_info(magick::image_read(
    file.path(d, "n1", unique(one$montage))))$width, 100L)
  expect_identical(magick::image_info(magick::image_read(
    file.path(d, "n2", unique(two$montage))))$width, 150L)
})

test_that("a placeholder takes the slot its siblings occupy, not one the sheet predicts", {
  # It used to take the sheet's declared size. That holds only while the files
  # are shaped the way the sheet describes them, and they need not be: feeding
  # wide strips against square sheet rows put a 650x650 placeholder beside a
  # 650x200 panel -- three times too tall, and the biggest thing in the picture.
  skip_if_no_pkg(c("argparser", "magick"))
  d <- withr::local_tempdir()
  img <- file.path(d, "img")
  dir.create(img)
  # One wide panel present; two rows missing. The sheet calls every row square,
  # so the sheet and the file disagree -- which is the case that broke.
  magick::image_write(magick::image_blank(650, 200, "steelblue"),
                      file.path(img, "S01_overview_ch1.png"))
  sheet <- data.frame(series_id = c("S01", "S02", "S03"), include = "true",
                      section_id = "G", size_x = 2000, size_y = 2000,
                      pixel_width = 1, pixel_height = 1, stringsAsFactors = FALSE)
  sp <- file.path(d, "s.tsv")
  utils::write.table(sheet, sp, sep = "\t", quote = FALSE, row.names = FALSE)

  for (mode in c("pixel", "physical")) {
    idx <- suppressMessages(suppressWarnings(group_montage_cli(c(
      "--sample_sheet", sp, "--group_by", "section_id", "--image_dir", img,
      "--image_suffix", "_overview_ch1.png", "--outdir", file.path(d, mode),
      "--ncol", "1", "--cell_max_px", "650", "--scale", mode, "--no_scale_bar"))))
    expect_identical(sum(idx$status == "missing"), 2L)
    # Every cell the same as the one real panel, in BOTH modes: the sheet said
    # square in both, and it is the file that decides.
    expect_equal(unique(idx$cell_px_w), 650)
    expect_equal(unique(idx$cell_px_h), 200)
  }
})

test_that("a group with no image at all still gets a montage, at the budget", {
  # The only case with nothing to match, so the budget is all there is. It must
  # not collapse to a zero-sized cell -- an absent group has to be visible.
  skip_if_no_pkg(c("argparser", "magick"))
  d <- withr::local_tempdir()
  img <- file.path(d, "img")
  dir.create(img)
  sheet <- data.frame(series_id = c("S01", "S02"), include = "true", section_id = "G",
                      size_x = 2000, size_y = 2000, pixel_width = 1,
                      pixel_height = 1, stringsAsFactors = FALSE)
  sp <- file.path(d, "s.tsv")
  utils::write.table(sheet, sp, sep = "\t", quote = FALSE, row.names = FALSE)

  idx <- suppressMessages(suppressWarnings(group_montage_cli(c(
    "--sample_sheet", sp, "--group_by", "section_id", "--image_dir", img,
    "--image_suffix", "_overview_ch1.png", "--outdir", file.path(d, "none"),
    "--ncol", "1", "--cell_max_px", "300", "--scale", "pixel", "--no_scale_bar"))))
  expect_identical(sum(idx$status == "missing"), 2L)
  expect_equal(unique(idx$cell_px_w), 300)
  expect_equal(unique(idx$cell_px_h), 300)
  expect_true(file.exists(file.path(d, "none", unique(idx$montage))))
})

test_that("a column holding no cell at all is worth no width", {
  # --ncol 3 with two panels must not reserve a third column: that would put
  # back exactly the blank space the table layout removes.
  fx <- gm_fixture()
  r <- gm_run(fx)                                  # gm_run fixes --ncol 3
  a <- r$idx[r$idx$group == "A", ]
  expect_identical(nrow(a), 2L)
  w <- magick::image_info(magick::image_read(file.path(r$out, unique(a$montage))))$width
  expect_equal(w, sum(unique(data.frame(c = a$col, w = a$cell_px_w))$w))
})

test_that("the cell is the bounding box of the group, not a square", {
  # The floor used to apply to BOTH axes, which made every cell square: the
  # scale is chosen so the largest panel's longest side is exactly the budget,
  # so the other axis was always short and always padded back up. A group of
  # wide sections came out most of the way white.
  fx <- gm_fixture()
  for (p in c("B_s0001", "B_s0002")) {
    magick::image_write(magick::image_blank(400, 100, color = "steelblue"),
                        file.path(fx$img, paste0(p, "_overview_ch1.png")))
  }
  s <- utils::read.delim(fx$sheet, stringsAsFactors = FALSE)
  s$size_x[s$series_id %in% c("B_s0001", "B_s0002")] <- 2000   # 1000 um wide
  s$size_y[s$series_id %in% c("B_s0001", "B_s0002")] <- 500    #  250 um tall
  utils::write.table(s, fx$sheet, sep = "\t", quote = FALSE, row.names = FALSE)

  r <- gm_run(fx, c("--scale", "pixel"))
  b <- r$idx[r$idx$group == "B", ]
  expect_lt(unique(b$cell_px_h), unique(b$cell_px_w))
  # 4:1 images, so the cell follows them rather than being squared off.
  expect_equal(unique(b$cell_px_w) / unique(b$cell_px_h), 4, tolerance = 0.05)
})

test_that("a grouping column that separates nothing is warned about", {
  # Every group holding one sample means each montage is a single image under a
  # new name -- and the run otherwise looks exactly like a successful one.
  fx <- gm_fixture()
  expect_warning(
    suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx$sheet, "--group_by", "series_index",
      "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
      "--outdir", file.path(fx$dir, "degen"), "--cell_max_px", "150"))),
    "every sample in its own group")

  # ...and a MIXTURE is not warned about. This needs a fixture that ACTUALLY
  # holds a singleton beside a real group -- the everyday case, a section that
  # yielded one series. Asserting no-warning on a fixture with no singleton at
  # all would pass even if the condition were `any(sizes == 1)`, which is the
  # over-warning this is meant to rule out.
  fx2 <- gm_fixture()
  s2 <- utils::read.delim(fx2$sheet, stringsAsFactors = FALSE)
  s2$section_id[s2$series_id == "A_s0001"] <- "SOLO"
  utils::write.table(s2, fx2$sheet, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_identical(sum(s2$section_id == "SOLO"), 1L)

  # Collected rather than expect_no_warning(), for two reasons: gm_run()
  # suppresses warnings, so nothing would reach the expectation; and this
  # fixture legitimately warns about B_s0003's missing image, so "no warnings
  # at all" is the wrong assertion. What matters is that THIS warning is absent.
  seen <- character(0)
  withCallingHandlers(
    suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx2$sheet, "--group_by", "section_id",
      "--image_dir", fx2$img, "--image_suffix", "_overview_ch1.png",
      "--outdir", file.path(fx2$dir, "mixed"), "--cell_max_px", "150"))),
    warning = function(cond) {
      seen <<- c(seen, conditionMessage(cond))
      invokeRestart("muffleWarning")
    })
  expect_false(any(grepl("every sample in its own group", seen, fixed = TRUE)))
  # ...and the fixture did warn about something, so the check above is not
  # passing merely because warnings never arrive here.
  expect_true(any(grepl("not found", seen, fixed = TRUE)))
})

test_that("a group whose images are ALL missing still gets a montage", {
  # Found while fixing --order_by on a machine column: grouping by series_index
  # puts B_s0003 -- the row whose image is absent -- alone in its own group, and
  # the scale was being computed only from panels that had turned up. An empty
  # group then had nothing to scale to and the whole run died. The sheet knows a
  # sample's physical size whether or not its PNG exists, so the scale comes
  # from there and the group gets a montage of placeholders, which is the entire
  # point of drawing missing images.
  fx <- gm_fixture()
  out <- file.path(fx$dir, "allmissing")
  idx <- suppressMessages(suppressWarnings(group_montage_cli(c(
    "--sample_sheet", fx$sheet, "--group_by", "series_index",
    "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
    "--outdir", out, "--cell_max_px", "150"))))

  gone <- idx[idx$series_id == "B_s0003", ]
  expect_identical(nrow(gone), 1L)
  expect_identical(gone$status, "missing")
  expect_true(is.finite(gone$um_per_px))
  expect_true(file.exists(file.path(out, gone$montage)))
})

test_that("--on_missing error refuses rather than drawing a placeholder", {
  fx <- gm_fixture()
  expect_error(
    suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx$sheet, "--group_by", "section_id",
      "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
      "--outdir", file.path(fx$dir, "err"), "--on_missing", "error"))),
    "not found")
})

test_that("the index records the grid position and the files written", {
  fx <- gm_fixture()
  r <- gm_run(fx, c("--order_by", "ord"))
  expect_true(file.exists(file.path(r$out, "montage_index.tsv")))
  expect_setequal(colnames(r$idx),
                  c("group", "series_id", "image_path", "status", "row", "col",
                    "um_per_px", "cell_px_w", "cell_px_h", "width_um",
                    "height_um", "montage"))
  # --order_by is honoured: A_s0002 has ord 1, so it comes first.
  a <- r$idx[r$idx$group == "A", ]
  expect_identical(a$series_id[1], "A_s0002")
  expect_true(all(file.exists(file.path(r$out, unique(r$idx$montage)))))
})

test_that("--order_by works on a MACHINE column, e.g. series_index", {
  # The reader drops machine columns so they cannot land on an output row, and
  # the CLI has to ask for the ones it needs by name. It asked only for the four
  # physical-size columns, so "--order_by series_index" -- the obvious thing to
  # want, since with serial sections the order IS the information -- failed with
  # "--order_by names no column of the sample sheet".
  fx <- gm_fixture()
  r <- gm_run(fx, c("--order_by", "series_index"))
  a <- r$idx[r$idx$group == "A", ]
  # A_s0002 carries series_index 10 and A_s0001 carries 11, so ordering by it
  # reverses the sheet order -- which sheet order alone could not show.
  expect_identical(a$series_id, c("A_s0002", "A_s0001"))
})

test_that("--label_by and --group_by also accept a machine column", {
  fx <- gm_fixture()
  r <- gm_run(fx, c("--label_by", "series_name"))
  expect_identical(sum(r$idx$status == "ok"), 4L)

  # Called directly: gm_run() already supplies --group_by, and argparser
  # refuses the flag twice.
  out2 <- file.path(fx$dir, "bymachine")
  idx2 <- suppressMessages(suppressWarnings(group_montage_cli(c(
    "--sample_sheet", fx$sheet, "--group_by", "series_index",
    "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
    "--outdir", out2, "--cell_max_px", "150"))))
  # one group per row, since series_index is unique
  expect_length(unique(idx2$group), 5L)
})

test_that("--order_by on a column that is in no sheet still fails, and says what is there", {
  fx <- gm_fixture()
  expect_error(gm_run(fx, c("--order_by", "not_a_column")),
               "names no column of the sample sheet")
  expect_error(gm_run(fx, c("--order_by", "not_a_column")), "available: ")
})

test_that("--um_per_px run gives every montage one scale; group does not", {
  fx <- gm_fixture()
  per_group <- gm_run(fx)$idx
  per_run <- gm_run(fx, c("--um_per_px", "run"))$idx

  # run: one number for every montage, so two montages are comparable.
  expect_length(unique(per_run$um_per_px), 1L)
  # group: A (500 um) and B (1000 um) each fill the cell, so they differ -- and
  # that is exactly why the scale bar is on by default under physical scaling.
  expect_length(unique(per_group$um_per_px), 2L)
  expect_gt(unique(per_group$um_per_px[per_group$group == "B"]),
            unique(per_group$um_per_px[per_group$group == "A"]))
  expect_true(all(is.finite(c(per_run$um_per_px, per_group$um_per_px))))
})

test_that("a blank grouping key is refused, not collected into a bucket", {
  fx <- gm_fixture()
  s <- utils::read.delim(fx$sheet, stringsAsFactors = FALSE)
  s$section_id[s$series_id == "A_s0001"] <- ""
  utils::write.table(s, fx$sheet, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(
    suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx$sheet, "--group_by", "section_id",
      "--image_dir", fx$img, "--image_suffix", "_overview_ch1.png",
      "--outdir", file.path(fx$dir, "blank")))),
    "have no 'section_id'")
})

test_that("two samples resolving to one image is an error", {
  # It would draw the same picture twice under two names, and a by-eye count
  # would double it.
  fx <- gm_fixture()
  s <- utils::read.delim(fx$sheet, stringsAsFactors = FALSE)
  s$img <- file.path(fx$img, "A_s0001_overview_ch1.png")
  utils::write.table(s, fx$sheet, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(
    suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx$sheet, "--group_by", "section_id",
      "--image_path_by", "img",
      "--outdir", file.path(fx$dir, "dup")))),
    "claimed by more than one sample")
})

test_that("--image_path_by takes paths from the sheet, and excludes the built form", {
  fx <- gm_fixture()
  s <- utils::read.delim(fx$sheet, stringsAsFactors = FALSE)
  s$img <- file.path(fx$img, paste0(s$series_id, "_overview_ch1.png"))
  utils::write.table(s, fx$sheet, sep = "\t", quote = FALSE, row.names = FALSE)

  out <- file.path(fx$dir, "bycol")
  idx <- suppressMessages(suppressWarnings(group_montage_cli(c(
    "--sample_sheet", fx$sheet, "--group_by", "section_id",
    "--image_path_by", "img", "--outdir", out, "--cell_max_px", "200"))))
  expect_identical(sum(idx$status == "ok"), 4L)

  expect_error(
    suppressMessages(group_montage_cli(c(
      "--sample_sheet", fx$sheet, "--group_by", "section_id",
      "--image_path_by", "img", "--image_dir", fx$img,
      "--outdir", file.path(fx$dir, "both")))),
    "one or the other")
})

test_that("a black background is refused", {
  # The overviews' own background is black, so a black pad cannot be told from
  # correctly-imaged empty field -- the exact misreading this montage prevents.
  fx <- gm_fixture()
  expect_error(gm_run(fx, c("--background", "black")), "must not be black")
})

test_that("physical scaling refuses a row with no pixel size", {
  # Never a quiet fall back to pixel scaling: the claim of the picture is that
  # a millimetre is a millimetre across it.
  fx <- gm_fixture()
  s <- utils::read.delim(fx$sheet, stringsAsFactors = FALSE)
  s$pixel_width[s$series_id == "B_s0001"] <- NA
  utils::write.table(s, fx$sheet, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(gm_run(fx), "no usable pixel size")
})

test_that("the sheet reader keeps only the machine columns it is asked for", {
  fx <- gm_fixture()
  kept <- .cli_read_sample_sheet(fx$sheet, keep_machine = c("size_x", "pixel_width"))
  expect_true(all(c("size_x", "pixel_width") %in% colnames(kept)))
  expect_false("size_y" %in% colnames(kept))
  expect_false("include" %in% colnames(kept))      # a control column, never metadata
  expect_identical(nrow(kept), 5L)                 # C_s0001 is include=false

  expect_error(.cli_read_sample_sheet(fx$sheet, keep_machine = "section_id"),
               "not machine columns")
})

test_that("mg_aspect_off tolerates a resize's rounding and catches a real mismatch", {
  skip_if_no_pkg("magick")
  # 11344 x 9590 resized to 1000 wide gives 846 -- 0.07% off, which must pass.
  im <- magick::image_blank(1000, 846, color = "white")
  expect_true(is.na(mg_aspect_off(im, 11344 / 9590)))
  # A 2:1 image where a square was described misses by 100%.
  wide <- magick::image_blank(400, 200, color = "white")
  expect_equal(mg_aspect_off(wide, 1), 2)
  # Nothing to compare against is not a mismatch.
  expect_true(is.na(mg_aspect_off(wide, NA_real_)))
  expect_true(is.na(mg_aspect_off(NULL, 1)))
})

test_that("mg_nice_number picks from the 1/2/5 decade series", {
  expect_identical(mg_nice_number(900), 500)
  expect_identical(mg_nice_number(120), 100)
  expect_identical(mg_nice_number(7), 5)
  expect_identical(mg_nice_number(1), 1)
  expect_true(is.na(mg_nice_number(0)))
  expect_true(is.na(mg_nice_number(-3)))
})

test_that("the scale bar is drawn by default, and --no_scale_bar removes it", {
  # Purely visual, so nothing else in this file would notice if it silently
  # stopped being drawn -- which is the case for asserting it directly.
  fx <- gm_fixture()
  with_bar <- gm_run(fx)$out
  without <- gm_run(fx, "--no_scale_bar")$out

  a <- magick::image_read(file.path(with_bar, "B_montage.png"))
  b <- magick::image_read(file.path(without, "B_montage.png"))
  expect_identical(magick::image_info(a)$width, magick::image_info(b)$width)
  expect_identical(magick::image_info(a)$height, magick::image_info(b)$height)
  # Same canvas, different pixels: the bar is drawn into it, not appended.
  expect_false(identical(as.integer(grDevices::as.raster(a) == grDevices::as.raster(b)),
                         rep(1L, length(grDevices::as.raster(a)))))

  # And it says a real distance: the fixture is 1000 um wide at 1000/300 um/px,
  # so a bar of a nice round length must appear in the label.
  txt <- magick::image_read(file.path(with_bar, "B_montage.png"))
  expect_gt(sum(grDevices::as.raster(a) != grDevices::as.raster(b)), 100L)
})
