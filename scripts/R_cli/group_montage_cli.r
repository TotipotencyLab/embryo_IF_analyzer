#!/usr/bin/env Rscript

# group_montage_cli.r
#
# One montage per GROUP of samples: every image belonging to a group, tiled,
# titled and drawn to a common physical scale.
#
# The case it exists for is counting by eye. An ovary is cut into serial
# sections that are imaged as separate series, so one specimen's sections are
# scattered across a sample sheet; looking at them together is how you judge
# whether a count is plausible, and how you count at all when automatic
# filtering cannot be trusted.
#
#   ./group_montage_cli.r --image_dir overviews --image_suffix _overview_ch1.png \
#       --sample_sheet sheets/series.tsv --group_by section_id --outdir montages
#
# TWO THINGS HERE ARE NOT PREFERENCES.
#
# 1. A missing image still occupies a labelled cell. Counting from a grid that
#    silently dropped a panel gives a wrong answer that looks like a right one:
#    you count four sections and never learn there were five.
#
# 2. Panels are scaled by PHYSICAL size, not pixel size. Two series with
#    identical pixel dimensions can be different physical sizes -- this dataset
#    holds 0.2227 and 0.4456 um pixels -- so equal-pixel scaling draws the same
#    follicle at two sizes in one picture, which is exactly the misjudgement a
#    by-eye count makes. The scale bar is on by default for the same reason:
#    with a per-group scale, two montages are not comparable with each other,
#    and the bar is what makes that visible rather than silent.

suppressWarnings({
  .warn_option <- NULL
})

# Directory of THIS file, resolved at source time. See montage_qc_cli.r for why
# this is a top-level assignment rather than a function called later.
.THIS_DIR <- (function() {
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    if (!is.null(f$ofile)) {
      return(dirname(normalizePath(f$ofile, mustWork = FALSE)))
    }
  }
  a <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(a)) {
    return(dirname(normalizePath(sub("^--file=", "", a[1]), mustWork = FALSE)))
  }
  a <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) character(0))
  if (length(a)) {
    return(dirname(a[1]))
  }
  return(NA_character_)
})()

.gm_source_helpers <- function(rlib = NA) {
  if (!exists(".cli_resolve_arg", mode = "function")) {
    if (is.na(.THIS_DIR)) stop("cannot locate cli_helpers.r", call. = FALSE)
    sys.source(file.path(.THIS_DIR, "cli_helpers.r"), envir = globalenv())
  }
  # Only montage_grid.r, not the whole of scripts/R/: this CLI composes rasters
  # and does no geometry, so the sf-dependent library would add failure modes it
  # cannot hit. Each file is named with a symbol it must supply, so an omission
  # shows up where the list is edited rather than at the point of use -- which
  # is the failure count_features_cli had.
  dir <- if (is.na(rlib)) file.path(.THIS_DIR, "..", "R") else rlib
  needed <- c(montage_grid.r = "mg_cell")
  for (fname in names(needed)) {
    if (exists(needed[[fname]])) next
    f <- file.path(dir, fname)
    if (!file.exists(f)) {
      stop("cannot locate ", fname, "; pass --rlib_path", call. = FALSE)
    }
    sys.source(f, envir = globalenv())
  }
}

# ------------------------------------------------------------------------------

group_montage_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  .gm_source_helpers()
  .cli_need(c("argparser", "magick"))
  suppressPackageStartupMessages(library(argparser))

  # Deferred warnings print "There were N warnings" and hide the text past ten,
  # so the run with the most wrong with it says the least.
  .warn_option <- options(warn = 1)
  on.exit(options(.warn_option), add = TRUE)

  p <- arg_parser("Tile every image of a sample group into one montage", hide.opts = TRUE)

  p <- add_argument(p, "--sample_sheet", short = "-s", type = "character",
                    help = "series.tsv; its series_id column names each image")
  p <- add_argument(p, "--group_by", short = "-g", type = "character",
                    help = "sheet column whose value names the group, e.g. section_id")
  p <- add_argument(p, "--outdir", short = "-o", type = "character",
                    help = "directory for the montages and the index")

  p <- add_argument(p, "--image_dir", short = "-i", type = "character", default = NA,
                    help = "directory holding the images")
  p <- add_argument(p, "--image_suffix", short = "-u", type = "character", default = NA,
                    help = "appended to the series_id, extension included, e.g. _overview_ch1.png")
  p <- add_argument(p, "--image_prefix", short = "-X", type = "character", default = "",
                    help = "prepended to the series_id, if the images carry one")
  p <- add_argument(p, "--image_path_by", short = "-A", type = "character", default = NA,
                    help = paste("sheet column holding each image's path, INSTEAD of",
                                 "building it from --image_dir/--image_suffix"))

  p <- add_argument(p, "--order_by", short = "-O", type = "character", default = NA,
                    help = "sheet column ordering the panels within a group [default: sheet order]")
  p <- add_argument(p, "--label_by", short = "-L", type = "character", nargs = Inf,
                    default = NULL,
                    help = "sheet column(s) for the per-panel label [default: the series_id]")
  p <- add_argument(p, "--ncol", short = "-n", type = "integer", default = 0L,
                    help = "grid columns [default: about the square root of the group size]")
  p <- add_argument(p, "--cell_max_px", short = "-C", type = "integer", default = 500L,
                    help = paste("the cell's LONGEST side, in output pixels [default: 500].",
                                 "Under --scale pixel each image's longest side becomes this;",
                                 "under --scale physical only the largest panel's does, and",
                                 "the rest stay proportionally smaller"))
  p <- add_argument(p, "--scale", short = "-S", type = "character", default = "physical",
                    help = "physical | pixel [default: physical]")
  p <- add_argument(p, "--um_per_px", short = "-U", type = "character", default = "group",
                    help = paste("group | run | a number. 'group' fits each group's largest",
                                 "panel to the cell; 'run' uses one scale for every montage,",
                                 "so they can be compared with each other [default: group]"))
  p <- add_argument(p, "--no_scale_bar", short = "-B", flag = TRUE,
                    help = "do not draw the scale bar (it is drawn by default under --scale physical)")
  p <- add_argument(p, "--background", short = "-b", type = "character", default = "white",
                    help = "pad colour; must not be black [default: white]")

  p <- add_argument(p, "--title", short = "-T", type = "character", default = NA,
                    help = "montage title [default: the group's name]")
  p <- add_argument(p, "--output_prefix", short = "-P", type = "character", default = "",
                    help = "prefix for the written files")
  p <- add_argument(p, "--on_missing", short = "-M", type = "character", default = "placeholder",
                    help = "placeholder | error -- what to do about an image that is not there")
  p <- add_argument(p, "--rlib_path", short = "-R", type = "character", default = NA,
                    help = "scripts/R directory, if it is not beside this script")

  argv <- parse_args(p, argv = args)
  .gm_source_helpers(argv$rlib_path)
  .cli_require(argv, c("sample_sheet", "group_by", "outdir"))

  scale_mode <- match.arg(tolower(.gm_one(.cli_resolve_arg(argv$scale, "--scale"), "physical")),
                          c("physical", "pixel"))
  on_missing <- match.arg(tolower(.gm_one(.cli_resolve_arg(argv$on_missing, "--on_missing"), "placeholder")),
                          c("placeholder", "error"))
  bg <- .gm_one(.cli_resolve_arg(argv$background, "--background"), "white")
  if (tolower(bg) %in% c("black", "#000000", "#000")) {
    stop("--background must not be black: the overviews' own background is black, ",
         "so a black pad cannot be told from a correctly-imaged empty field.",
         call. = FALSE)
  }

  path_by <- .gm_one(.cli_resolve_arg(argv$image_path_by, "--image_path_by"))
  img_dir <- .gm_one(.cli_resolve_arg(argv$image_dir, "--image_dir"))
  suffix  <- .gm_one(.cli_resolve_arg(argv$image_suffix, "--image_suffix"))
  if (!is.na(path_by) && (!is.na(img_dir) || !is.na(suffix))) {
    stop("--image_path_by names the column holding each path, so --image_dir and ",
         "--image_suffix have nothing to do. Pass one or the other.", call. = FALSE)
  }
  if (is.na(path_by) && (is.na(img_dir) || is.na(suffix))) {
    stop("Give either --image_path_by, or both --image_dir and --image_suffix.",
         call. = FALSE)
  }

  # Every column this run names has to be resolved BEFORE the sheet is read,
  # because the reader drops machine columns and cannot know which of them this
  # step needs. Two kinds have to be kept:
  #
  #   - the four physical-size columns, when scaling physically;
  #   - whatever the USER named. `--order_by series_index` is the obvious and
  #     correct thing to ask for -- with serial sections the order IS the
  #     information -- and series_index is a machine column, so asking only for
  #     the physical four made a sensible command fail with "--order_by names no
  #     column of the sample sheet". --group_by, --label_by and --image_path_by
  #     can each name one just as easily.
  group_col <- .gm_one(.cli_resolve_arg(argv$group_by, "--group_by"))
  order_col <- .gm_one(.cli_resolve_arg(argv$order_by, "--order_by"))
  label_cols <- .cli_resolve_arg(argv$label_by, "--label_by")
  if (!length(label_cols)) label_cols <- "series_id"

  physical_cols <- if (scale_mode == "physical") {
    c("size_x", "size_y", "pixel_width", "pixel_height")
  } else {
    character(0)
  }
  named_cols <- c(group_col, order_col, label_cols, path_by)
  named_cols <- named_cols[!is.na(named_cols) & nzchar(named_cols)]
  # Intersected rather than passed straight through: keep_machine refuses a name
  # that is not a machine column, and naming an ordinary one is the common case.
  # This asks only for those that would otherwise be dropped.
  want <- base::intersect(unique(c(physical_cols, named_cols)),
                          .cli_sheet_machine_columns())

  sheet <- .cli_read_sample_sheet(.gm_one(.cli_resolve_arg(argv$sample_sheet, "--sample_sheet")),
                                  keep_machine = want)

  if (!group_col %in% colnames(sheet)) {
    stop("--group_by names no column of the sample sheet: ", group_col,
         "\n  available: ", paste(colnames(sheet), collapse = ", "), call. = FALSE)
  }
  groups <- trimws(as.character(sheet[[group_col]]))
  # A blank grouping key on a counting workflow is a data-entry slip far more
  # often than an intention, and silently collecting those rows into an
  # "(ungrouped)" montage would hide it. include=false is the way to say a row
  # genuinely has no group.
  blank <- is.na(groups) | !nzchar(groups)
  if (any(blank)) {
    stop(sum(blank), " included row(s) have no '", group_col, "': ",
         paste(utils::head(sheet[["series_id"]][blank], 6), collapse = ", "),
         if (sum(blank) > 6) ", ..." else "",
         "\n  Fill the column, or set include=false on those rows.", call. = FALSE)
  }
  sheet[[".group"]] <- groups

  # --- resolve one path per row -------------------------------------------------
  if (!is.na(path_by)) {
    if (!path_by %in% colnames(sheet)) {
      stop("--image_path_by names no column of the sample sheet: ", path_by,
           "\n  available: ", paste(colnames(sheet), collapse = ", "), call. = FALSE)
    }
    paths <- trimws(as.character(sheet[[path_by]]))
    paths[is.na(paths) | !nzchar(paths)] <- NA_character_
  } else {
    # Built exactly, never globbed. "S1" + "_overview_ch1.png" cannot match
    # S10's file, so the prefix-collision hazard does not arise rather than
    # being checked for -- and the sheet's four-digit series padding means two
    # prefixes never collide in the first place.
    paths <- file.path(img_dir,
                       paste0(.gm_one(.cli_resolve_arg(argv$image_prefix, "--image_prefix"), ""),
                              sheet[["series_id"]], suffix))
  }
  sheet[[".path"]] <- paths

  # Two rows resolving to one file would draw the same picture twice under two
  # names, and a by-eye count would double it.
  present <- paths[!is.na(paths)]
  if (anyDuplicated(present)) {
    dup <- unique(present[duplicated(present)])
    stop(length(dup), " image path(s) are claimed by more than one sample: ",
         paste(utils::head(dup, 4), collapse = ", "), call. = FALSE)
  }

  found <- !is.na(sheet[[".path"]]) & file.exists(sheet[[".path"]])
  if (any(!found)) {
    msg <- paste0(sum(!found), " of ", nrow(sheet), " image(s) not found, e.g. ",
                  paste(utils::head(sheet[[".path"]][!found], 3), collapse = ", "))
    if (on_missing == "error") {
      stop(msg, "\n  Pass --on_missing placeholder to draw them as empty cells.",
           call. = FALSE)
    }
    warning(msg, "; drawn as labelled placeholders.", call. = FALSE)
  }
  sheet[[".found"]] <- found
  sizes <- table(sheet[[".group"]])
  message("Resolved ", sum(found), " of ", nrow(sheet), " image(s) in ",
          length(sizes), " group(s); ", sum(sizes == 1L), " of them hold one sample")

  # A group of one is fine and is drawn like any other -- a section that yielded
  # a single series still belongs beside its neighbours, and --ncol gives it the
  # same width as them. EVERY group holding one is a different thing: it means
  # the grouping column separates nothing, so each "montage" is one image under
  # a new name, and the run looks exactly like a successful one. Almost always a
  # column was named that is unique per row, --group_by series_index being the
  # easy mistake. A warning rather than a refusal: uniform output for a later
  # step is a legitimate reason to want it.
  if (length(sizes) > 1L && all(sizes == 1L)) {
    warning("--group_by ", group_col, " puts every sample in its own group, so ",
            "each montage is a single image. Nothing is being placed beside ",
            "anything. Did you mean a column that repeats across samples?",
            call. = FALSE)
  }

  # --- physical extents ---------------------------------------------------------
  if (scale_mode == "physical") {
    sheet[[".w_um"]] <- mg_extent_um(sheet[["size_x"]], sheet[["pixel_width"]])
    sheet[[".h_um"]] <- mg_extent_um(sheet[["size_y"]], sheet[["pixel_height"]])
    bad <- sheet[[".found"]] & (!is.finite(sheet[[".w_um"]]) | !is.finite(sheet[[".h_um"]]))
    if (any(bad)) {
      # Loud, never a quiet fall back to pixel scaling: the whole claim of this
      # montage is that a millimetre is a millimetre across it.
      stop(sum(bad), " row(s) have no usable pixel size, so they cannot be placed ",
           "on a physical scale: ",
           paste(utils::head(sheet[["series_id"]][bad], 6), collapse = ", "),
           "\n  Fill pixel_width/pixel_height, or pass --scale pixel.", call. = FALSE)
    }
  } else {
    sheet[[".w_um"]] <- NA_real_
    sheet[[".h_um"]] <- NA_real_
  }

  # --- the scale ----------------------------------------------------------------
  cell_px <- max(50L, as.integer(argv$cell_max_px))
  spec <- tolower(.gm_one(.cli_resolve_arg(argv$um_per_px, "--um_per_px"), "group"))
  run_upp <- NA_real_
  if (scale_mode == "physical") {
    longest <- pmax(sheet[[".w_um"]], sheet[[".h_um"]])
    if (spec == "run") {
      # Every row, not only the ones whose image turned up. The sheet knows a
      # sample's physical size whether or not its PNG exists, and a group whose
      # images are ALL missing must still get a montage of placeholders rather
      # than an error -- the whole point is that a missing panel is visible.
      run_upp <- mg_um_per_px(longest, cell_px)
    } else if (spec != "group") {
      run_upp <- suppressWarnings(as.numeric(spec))
      if (!is.finite(run_upp) || run_upp <= 0) {
        stop("--um_per_px must be 'group', 'run', or a positive number, not '",
             spec, "'", call. = FALSE)
      }
    }
  }

  outdir <- .gm_one(.cli_resolve_arg(argv$outdir, "--outdir"))
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  out_prefix <- .gm_one(.cli_resolve_arg(argv$output_prefix, "--output_prefix"), "")

  if (!is.na(order_col) && !order_col %in% colnames(sheet)) {
    stop("--order_by names no column of the sample sheet: ", order_col,
         "\n  available: ", paste(colnames(sheet), collapse = ", "), call. = FALSE)
  }
  absent_l <- setdiff(label_cols, colnames(sheet))
  if (length(absent_l)) {
    stop("--label_by names no column of the sample sheet: ",
         paste(absent_l, collapse = ", "), call. = FALSE)
  }

  index <- list()
  written <- character(0)
  # Collected across every group and reported once. One line per row would be
  # thousands on a tile scan, and a warning nobody finishes reading is a warning
  # that does not work.
  odd <- list()
  for (g in unique(sheet[[".group"]])) {
    rows <- sheet[sheet[[".group"]] == g, , drop = FALSE]
    if (!is.na(order_col)) {
      rows <- rows[order(rows[[order_col]]), , drop = FALSE]
    }
    n <- nrow(rows)
    ncol <- if (argv$ncol > 0L) as.integer(argv$ncol) else max(1L, ceiling(sqrt(n)))

    upp <- NA_real_
    draw_w <- rep(NA_real_, n)
    draw_h <- rep(NA_real_, n)
    if (scale_mode == "physical") {
      longest <- pmax(rows[[".w_um"]], rows[[".h_um"]])
      upp <- if (is.finite(run_upp)) run_upp else mg_um_per_px(longest, cell_px)
      draw_w <- rows[[".w_um"]] / upp
      draw_h <- rows[[".h_um"]] / upp
    }

    labels <- apply(rows[, label_cols, drop = FALSE], 1L,
                    function(r) paste(as.character(r), collapse = " | "))

    # Read once, scale immediately, keep only the scaled copy. The full-size
    # image is never held beyond the line that shrinks it: an overview of a tile
    # merge can be a hundred megapixels, and a group of them held at once is the
    # same mistake the Fiji side had to be dug out of.
    fitted <- lapply(seq_len(n), function(i) {
      if (!rows[[".found"]][i]) {
        return(NULL)
      }
      im <- tryCatch(magick::image_read(rows[[".path"]][i]), error = function(e) NULL)
      if (is.null(im)) {
        return(NULL)
      }
      # Checked HERE, where the image is in hand, rather than by reading every
      # file a second time. Physical scaling only: it is the one mode that uses
      # the sheet's dimensions, so it is the one mode where a mismatch has a
      # consequence. Under --scale pixel the sheet's size is never consulted and
      # an oddly-shaped file is a legitimate thing to hand in, so warning there
      # would fire on every row of a deliberate use and teach the warning to be
      # ignored.
      #
      # NB the mode test is belt-and-braces and cannot currently fail: .w_um is
      # NA outside physical mode, so mg_aspect_off() has nothing to compare and
      # returns NA anyway. Removing it would leave the behaviour resting on that
      # one non-obvious fact, and computing the extents in both modes -- a
      # plausible thing to want, so the index could report them -- would then
      # silently switch the warning on. Stated rather than inferred.
      if (scale_mode == "physical") {
        got <- mg_aspect_off(im, rows[[".w_um"]][i] / rows[[".h_um"]][i])
        if (is.finite(got)) {
          odd[[length(odd) + 1L]] <<- list(series_id = rows[["series_id"]][i], got = got,
                                           want = rows[[".w_um"]][i] / rows[[".h_um"]][i])
        }
      }
      if (is.finite(draw_w[i]) && is.finite(draw_h[i])) {
        mg_fit(im, draw_w[i], draw_h[i])
      } else {
        # Pixel mode: no physical size to honour, so each image's LONGEST side
        # becomes the budget. Fitting inside a square box does exactly that and
        # keeps the aspect; the cell below is then measured, not assumed.
        mg_fit(im, cell_px, cell_px)
      }
    })

    # A TABLE, not a uniform grid: each column is as wide as its widest cell and
    # each row as tall as its tallest, and a cell is padded only to its own
    # column and row.
    #
    # One cell size for the whole group is what a uniform grid forces, and it
    # spends the difference on blank space: a group holding one tall panel and
    # two short ones gave all three the tall panel's height, so two thirds of
    # the montage was white below the content. Sizing per column and per row
    # removes exactly that and nothing else.
    #
    # Horizontal blank does NOT all go away, and cannot: the output is a
    # rectangle, so a row narrower than the widest row is padded out to it. What
    # goes is the vertical waste, which is the larger share whenever panels
    # differ in height.
    #
    # Padding is top-left, so cells share an origin and a column can be scanned
    # down. Centring would look tidier and would cost that.
    #
    # NB padding never carried size information -- the DRAWN pixels do, at a
    # scale that is constant across the montage and stated by the scale bar. So
    # letting cells differ in size does not make panels less comparable.
    row_of <- ((seq_len(n) - 1L) %/% ncol) + 1L
    col_of <- ((seq_len(n) - 1L) %% ncol) + 1L

    # The slot a cell asks for.
    have_w <- vapply(fitted, function(im) {
      if (is.null(im)) NA_real_ else as.numeric(magick::image_info(im)$width)
    }, numeric(1))
    have_h <- vapply(fitted, function(im) {
      if (is.null(im)) NA_real_ else as.numeric(magick::image_info(im)$height)
    }, numeric(1))

    # A MISSING image still asks for a slot, and it asks for the one its
    # SIBLINGS actually occupy -- not one predicted from the sheet.
    #
    # It used to take the sheet's declared size, on the reasoning that the sheet
    # knows how big the absent section would have been. That holds only while
    # the files are shaped the way the sheet describes them. They need not be:
    # gathering a section's per-series QC montages into one sheet feeds wide
    # strips against square sheet rows, and the placeholder came out 650x650
    # beside a 650x200 panel -- three times too tall, and the biggest thing in
    # the picture.
    #
    # A placeholder's job is to keep the layout readable and to be visibly a
    # gap. It is not to encode the size of what is absent; the words "no image"
    # do that. So it matches what is there.
    fb_w <- if (any(is.finite(have_w))) max(have_w, na.rm = TRUE) else as.numeric(cell_px)
    fb_h <- if (any(is.finite(have_h))) max(have_h, na.rm = TRUE) else as.numeric(cell_px)
    slot_w <- ifelse(is.finite(have_w), have_w, fb_w)
    slot_h <- ifelse(is.finite(have_h), have_h, fb_h)

    # A column that holds NO CELL AT ALL -- ncol 3 with two panels in the group
    # -- is worth no width. Reserving space for it would put back exactly the
    # blank this layout removes, to hold a column that does not exist.
    #
    # Every cell that DOES exist has a slot by now, missing ones included, so
    # there is no third case: a group with no image anywhere still gets the
    # budget, through fb_w/fb_h above.
    span <- function(v, key, k) {
      out <- vapply(seq_len(k), function(j) {
        here <- v[key == j]
        if (!length(here)) return(0)
        max(here)
      }, numeric(1))
      return(ceiling(out))
    }
    col_w <- span(slot_w, col_of, ncol)
    row_h <- span(slot_h, row_of, max(row_of))

    cells <- lapply(seq_len(n), function(i) {
      mg_pad(fitted[[i]], cell_w = col_w[col_of[i]], cell_h = row_h[row_of[i]],
             label = labels[i], bg = bg)
    })

    # full_width stated, not inferred: a group of one, or a half-empty last
    # row, still comes out the width of a full grid, so every montage in the
    # run lines up when they are read side by side.
    # Every row is already sum(col_w) wide by construction, except a last row
    # holding fewer than ncol cells -- which full_width pads out, as before.
    img <- mg_grid(cells, ncol = ncol, bg = bg, full_width = sum(col_w))
    if (scale_mode == "physical" && !argv$no_scale_bar) {
      img <- mg_scale_bar(img, upp)
    }
    ttl <- .gm_one(.cli_resolve_arg(argv$title, "--title"))
    if (is.na(ttl)) {
      ttl <- paste0(g, "  (", sum(rows[[".found"]]), "/", n, " images",
                    if (is.finite(upp)) paste0(", ", signif(upp, 3), " um/px") else "", ")")
    }
    img <- mg_title(img, ttl, bg = bg)

    f <- file.path(outdir, paste0(out_prefix, .gm_safe(g), "_montage.png"))
    mg_write(img, f)
    written <- c(written, f)
    info <- magick::image_info(img)
    message("  ", g, ": ", n, " panel(s), ", ncol, " col -> ", basename(f),
            " (", info$width, "x", info$height, ")")

    index[[length(index) + 1L]] <- data.frame(
      group = g, series_id = rows[["series_id"]], image_path = rows[[".path"]],
      status = ifelse(rows[[".found"]], "ok", "missing"),
      row = ((seq_len(n) - 1L) %/% ncol) + 1L,
      col = ((seq_len(n) - 1L) %% ncol) + 1L,
      # Per CELL now, not per group: with a table layout a montage no longer has
      # one cell size, and a reader matching a picture back to a row needs the
      # slot that row was actually given.
      um_per_px = upp, cell_px_w = col_w[col_of], cell_px_h = row_h[row_of],
      width_um = rows[[".w_um"]], height_um = rows[[".h_um"]],
      montage = basename(f), stringsAsFactors = FALSE)
  }

  if (length(odd)) {
    ex <- utils::head(odd, 3)
    warning(length(odd), " image(s) are not the shape the sample sheet describes, ",
            "so they are fitted into it with blank space rather than stretched to ",
            "match: ",
            paste(vapply(ex, function(o) sprintf("%s (%.2f vs %.2f)",
                                                 o$series_id, o$got, o$want), character(1)),
                  collapse = ", "),
            if (length(odd) > 3) ", ..." else "",
            ". Either the sheet is stale, --image_suffix picked a differently ",
            "shaped file, or these images are not the samples the sheet names.",
            call. = FALSE)
  }

  idx <- do.call(rbind, index)
  idx_path <- file.path(outdir, paste0(out_prefix, "montage_index.tsv"))
  utils::write.table(idx, idx_path, sep = "\t", quote = FALSE,
                     row.names = FALSE, na = "")
  message("Wrote ", length(written), " montage(s) and ", basename(idx_path))
  return(invisible(idx))
}

#' One value, or a default
#'
#' .cli_resolve_arg() returns character(0) for "not given", not NA -- so is.na()
#' on it yields logical(0), and `&&` on that is an error rather than FALSE. This
#' is the accessor that keeps every downstream test a plain scalar comparison.
.gm_one <- function(v, default = NA_character_) {
  if (length(v) == 0L) return(default)
  return(as.character(v)[1])
}

#' Make a group name safe to put in a filename
.gm_safe <- function(x) {
  out <- gsub("[^A-Za-z0-9._-]+", "_", as.character(x))
  out[!nzchar(out)] <- "group"
  return(out)
}

if (!interactive() && sys.nframe() == 0L) group_montage_cli()
