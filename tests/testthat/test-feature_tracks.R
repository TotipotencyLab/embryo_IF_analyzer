# Tracks joined onto features (tracking milestone, PR 3: note/time_series_plan.md
# §4). The numbering cases are the plan's own examples, so a wrong id is a
# visible disagreement with the design rather than a matter of taste; the
# refusal cases are each a tracks table that would otherwise join cleanly onto
# the wrong nuclei, or shrink a track without a word.

source_cli("cli_helpers.r")
source_r_scripts(c("feature_join.r", "feature_centroids.r", "feature_tracks.r"))

.f <- function(n) sprintf("nucleus_%04d", n)

# The plan's example: A divides at t3 into A1 and A2; A2 is missed at t4 and
# found at t5 (gap-closed); B never divides. Feature numbers as annotate would
# give them: per frame, in order.
#   t1: 1=A 2=B   t2: 3=A 4=B   t3: 5=A1 6=A2 7=B   t4: 8=A1   t5: 9=A2
.aab <- function(){
  list(feat = data.frame(feature_id = .f(1:9), t = c(1L, 1L, 2L, 2L, 3L, 3L, 3L, 4L, 5L)),
       links = data.frame(feature_id = .f(c(3, 4, 5, 6, 7, 8, 9)),
                          prev_feature_id = .f(c(1, 2, 3, 3, 4, 5, 6))))
}

test_that("the plan's A/A1/A2/B example gives exactly its tracks and branches", {
  x <- .aab()
  r <- number_tracks(x$feat, x$links, "nucleus")
  ids <- stats::setNames(r$features$branch_id, r$features$feature_id)
  # A (t1-2) _b001, A1 (t3-4) _b002, A2 (t3, t5) _b003, all one lineage; B its own.
  expect_identical(unname(ids[.f(c(1, 3))]), rep("nucleus_track_0001_b001", 2))
  expect_identical(unname(ids[.f(c(5, 8))]), rep("nucleus_track_0001_b002", 2))
  expect_identical(unname(ids[.f(c(6, 9))]), rep("nucleus_track_0001_b003", 2))
  expect_identical(unname(ids[.f(c(2, 4, 7))]), rep("nucleus_track_0002_b001", 3))
  expect_identical(sort(unique(r$features$track_id)), c("nucleus_track_0001", "nucleus_track_0002"))
  b <- r$branches
  expect_identical(b$parent_branch_id, c("", "nucleus_track_0001_b001", "nucleus_track_0001_b001", ""))
  expect_identical(b$first_t, c(1L, 3L, 3L, 1L))
  expect_identical(b$last_t, c(2L, 4L, 5L, 3L))            # A2's branch spans its gap
  expect_identical(b$n_features, c(2L, 2L, 2L, 3L))        # ...and holds two features, not three
  expect_false(any(b$branch_merged))
})

test_that("ids do not depend on the order the links or features arrive in", {
  x <- .aab()
  a <- number_tracks(x$feat, x$links, "nucleus")
  set.seed(4)
  b <- number_tracks(x$feat[sample(nrow(x$feat)), ], x$links[sample(nrow(x$links)), ], "nucleus")
  ord <- function(d) d[order(d$feature_id), ]
  expect_identical(ord(a$features), ord(b$features))
  expect_identical(a$branches, b$branches)
})

test_that("tracks are numbered by first t, then first feature_id", {
  # Track X starts at t2; track Y at t1 with a higher feature number. Y first.
  feat <- data.frame(feature_id = .f(c(1, 5, 2, 6)), t = c(2L, 1L, 3L, 2L))
  links <- data.frame(feature_id = .f(c(2, 6)), prev_feature_id = .f(c(1, 5)))
  r <- number_tracks(feat, links, "nucleus")
  tid <- stats::setNames(r$features$track_id, r$features$feature_id)
  expect_identical(unname(tid[.f(5)]), "nucleus_track_0001")
  expect_identical(unname(tid[.f(1)]), "nucleus_track_0002")
})

test_that("a merge: one track, the merged feature starts a branch flagged as two objects", {
  # P and Q approach, are one feature U for t3-t4, then two again (pronuclei).
  #   t1: 1=P 2=Q   t2: 3=P 4=Q   t3: 5=U   t4: 6=U   t5: 7=P 8=Q
  feat <- data.frame(feature_id = .f(1:8), t = c(1L, 1L, 2L, 2L, 3L, 4L, 5L, 5L))
  links <- data.frame(feature_id = .f(c(3, 4, 5, 5, 6, 7, 8)),
                      prev_feature_id = .f(c(1, 2, 3, 4, 5, 6, 6)))
  r <- number_tracks(feat, links, "nucleus")
  expect_identical(unique(r$features$track_id), "nucleus_track_0001")
  b <- r$branches
  m <- b[b$branch_merged, ]
  expect_identical(nrow(m), 1L)
  expect_identical(c(m$first_t, m$last_t), c(3L, 4L))
  expect_identical(m$parent_branch_id, "nucleus_track_0001_b001;nucleus_track_0001_b002")
  expect_identical(sum(b$parent_branch_id == m$branch_id), 2L)   # it divides again after
  fm <- r$features$branch_merged[r$features$feature_id %in% .f(5:6)]
  expect_identical(fm, c(TRUE, TRUE))
})

test_that("a feature linked to nothing has no track and no branch", {
  feat <- data.frame(feature_id = .f(1:3), t = c(1L, 2L, 2L))
  r <- number_tracks(feat, data.frame(feature_id = .f(2), prev_feature_id = .f(1)), "nucleus")
  lone <- r$features[r$features$feature_id == .f(3), ]
  expect_true(is.na(lone$track_id)); expect_true(is.na(lone$branch_id))
  expect_true(is.na(lone$branch_merged))
  expect_identical(nrow(r$branches), 1L)
})

# --- join_tracks: the table on disk, checked against the features ----------------

# A per-ROI feature table: two ROIs per feature, plus an invalid and a
# nucleolus row that must come out with NA.
.feats <- function(run_id = "run0000aaaa", sid = "S"){
  x <- .aab()$feat
  rows <- x[rep(seq_len(nrow(x)), each = 2), ]
  extra <- data.frame(feature_id = c("invalid_nucleus_0001", "nucleolus_0001"), t = c(1L, 1L))
  rows <- rbind(rows, extra)
  data.frame(series_id = sid, roi = paste0("r", seq_len(nrow(rows))),
             feature_id = rows$feature_id, t = rows$t,
             feature_type = ifelse(grepl("nucleolus", rows$feature_id), "nucleolus", "nucleus"),
             run_id = run_id, stringsAsFactors = FALSE)
}
# The tracks table exactly as FeatureTracks.groovy writes it: a start row per
# feature nothing leads to, one row per link, sorted by t, feature_id.
.write_tracks <- function(dir, run_id = "run0000aaaa", sid = "S", edit = identity){
  x <- .aab()
  rows <- rbind(data.frame(feature_id = .f(c(1, 2)), prev_feature_id = ""), x$links)
  rows$t <- x$feat$t[match(rows$feature_id, x$feat$feature_id)]
  rows <- rows[order(rows$t, rows$feature_id), ]
  tr <- data.frame(series_id = sid, t = rows$t, feature_id = rows$feature_id,
                   prev_feature_id = rows$prev_feature_id, run_id = run_id)
  tr <- edit(tr)
  path <- tracks_path(dir, sid, "nucleus")
  utils::write.table(tr, path, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  path
}

test_that("join_tracks adds the three columns to every row of the type, NA elsewhere", {
  d <- withr::local_tempdir(); .write_tracks(d)
  out <- join_tracks(.feats(), "nucleus", d)
  expect_identical(nrow(out), nrow(.feats()))
  a <- out[out$feature_id == .f(6), ]
  expect_identical(unique(a$branch_id), "nucleus_track_0001_b003")
  expect_identical(nrow(a), 2L)                                   # both ROIs
  other <- out[!startsWith(out$feature_id, "nucleus_"), ]
  expect_true(all(is.na(other$track_id) & is.na(other$branch_id)))
  expect_identical(nrow(attr(out, "branches")), 4L)
  expect_identical(unique(attr(out, "branches")$series_id), "S")
})

test_that("a tracks table from another annotate run is refused; --force joins with a warning", {
  d <- withr::local_tempdir(); .write_tracks(d, run_id = "run0000bbbb")
  expect_error(join_tracks(.feats(), "nucleus", d), "different annotate run")
  expect_error(join_tracks(.feats(), "nucleus", d), "Re-run Make_FeatureTracks")
  expect_warning(out <- join_tracks(.feats(), "nucleus", d, force = TRUE), "different annotate run")
  expect_false(all(is.na(out$track_id)))
  # Features with no run_id cannot be the ones the tracks were made from.
  expect_error(join_tracks(.feats(run_id = NA), "nucleus", d), "\\(none\\)")
})

test_that("a tracks table without run_id, or with a blank one, is refused", {
  d <- withr::local_tempdir()
  .write_tracks(d, edit = function(tr) tr[, setdiff(colnames(tr), "run_id")])
  expect_error(join_tracks(.feats(), "nucleus", d), "has no run_id")
  .write_tracks(d, edit = function(tr) { tr$run_id[3] <- ""; tr })
  expect_error(join_tracks(.feats(), "nucleus", d), "1 row\\(s\\) with a blank run_id")
})

test_that("a dropped, duplicated or inconsistent row stops the join rather than shrinking it", {
  d <- withr::local_tempdir()
  cases <- list(
    "no row for 1 of 9"                 = function(tr) tr[tr$feature_id != .f(9), ],
    "repeats 1 row"                     = function(tr) rbind(tr, tr[5, ]),
    "not in the features table"         = function(tr) rbind(tr, transform(tr[1, ], feature_id = .f(99))),
    "links from 1 feature\\(s\\) not in" = function(tr) { tr$prev_feature_id[tr$feature_id == .f(9)] <- .f(98); tr },
    "a t that is not"                   = function(tr) { tr$t[tr$feature_id == .f(8)] <- 7L; tr },
    "same or a later"                   = function(tr) { tr$prev_feature_id[tr$feature_id == .f(5)] <- .f(8); tr },
    "both a start row and a link"       = function(tr) rbind(tr, transform(tr[tr$feature_id == .f(3), ], prev_feature_id = "")))
  for(msg in names(cases)){
    .write_tracks(d, edit = cases[[msg]])
    expect_error(join_tracks(.feats(), "nucleus", d), msg, info = msg)
  }
  .write_tracks(d)
  expect_no_error(join_tracks(.feats(), "nucleus", d))     # the unedited table joins
})

test_that("a series with no tracks table, or a table naming another series, is refused", {
  d <- withr::local_tempdir()
  expect_error(join_tracks(.feats(), "nucleus", d), "No tracks table for 1 of 1 series")
  .write_tracks(d)
  two <- rbind(.feats(), .feats(sid = "T"))
  expect_error(join_tracks(two, "nucleus", d), "S?T_nucleus_tracks.tsv")
  p <- .write_tracks(d, sid = "T", edit = function(tr) { tr$series_id <- "S"; tr })
  expect_error(join_tracks(.feats(sid = "T"), "nucleus", d), "holds series S, not T")
  # A type with no valid feature is a result, not a missing input (review,
  # PR 6): NA ids, an empty branches table. A typo is the CLI's to catch.
  none <- expect_message(join_tracks(.feats(), "oocyte", d), "No valid 'oocyte' feature in S")
  expect_true(all(is.na(none$track_id)) && all(is.na(none$branch_id)) && all(is.na(none$branch_merged)))
  expect_identical(nrow(attr(none, "branches")), 0L)
  withcols <- transform(.feats(), track_id = "x")
  .write_tracks(d)
  expect_error(join_tracks(withcols, "nucleus", d), "never stored")
  # Code review, PR 3: a table written when the series had no feature of the
  # type said "different annotate run" with a blank run_id; it now says what
  # it is.
  .write_tracks(d, edit = function(tr) tr[0, ])
  expect_error(join_tracks(.feats(), "nucleus", d), "has no rows, but S has 9 nucleus feature")
})

test_that("two joins of the same tables give identical ids", {
  d <- withr::local_tempdir(); .write_tracks(d)
  expect_identical(join_tracks(.feats(), "nucleus", d), join_tracks(.feats(), "nucleus", d))
})

# --- the CLIs, end to end on a synthetic time course ------------------------------

# Outlines for a time course whose answer is known: the A/A1/A2/B example, with
# a third object C at t2 only (linked to nothing). Two slices per feature, so
# each passes --min_z_span 2. x positions keep every object apart in space.
.sq <- function(name, roi, t, z, x0, y0, s){
  data.frame(name = name, roi = roi, t = t, z = z,
             x = c(x0, x0 + s, x0 + s, x0), y = c(y0, y0, y0 + s, y0 + s))
}
.time_course <- function(d, sid = "TC", frames = NULL){
  objs <- data.frame(name = c("A", "B", "C", "A", "B", "A1", "A2", "B", "A1", "A2"),
                     t = c(1, 1, 2, 2, 2, 3, 3, 3, 4, 5),
                     x = c(10, 60, 110, 10, 60, 5, 15, 60, 5, 15),
                     y = c(10, 10, 10, 10, 10, 30, 0, 10, 30, 0))
  rows <- list(); k <- 0
  for(i in seq_len(nrow(objs))) for(z in 1:2){
    k <- k + 1
    rows[[length(rows) + 1]] <- .sq(sid, sprintf("nucleus_%04d-%04d-%04d-%04d", objs$t[i], z, k, objs$y[i]),
                                    objs$t[i], z, objs$x[i], objs$y[i], 8)
  }
  utils::write.table(do.call(rbind, rows), file.path(d, paste0(sid, "_nucleus_outline.txt")),
                     sep = "\t", quote = FALSE, row.names = FALSE)
  cfg <- data.frame(parameter = c("series_id", "pixel_depth", "image_width",
                                  "image_height", "pixel_width", "pixel_height"),
                    value = c(sid, "1.0", "140", "50", "1", "1"))
  if(!is.null(frames)) cfg <- rbind(cfg, data.frame(parameter = "frames_analysed", value = frames))
  utils::write.table(cfg,
                     file.path(d, paste0(sid, "_config.txt")), sep = "\t", quote = FALSE,
                     row.names = FALSE)
  objs
}
# The links Make_FeatureTracks would write for that course, by object name,
# using the ids annotate actually assigned (read from its centroid table).
.tracks_for <- function(out, sid = "TC"){
  cen <- utils::read.delim(file.path(out, paste0(sid, "_feature_centroids.tsv")), stringsAsFactors = FALSE, comment.char = "#")
  cen <- cen[cen$feature_type == "nucleus", ]
  id_at <- function(t, x) cen$feature_id[cen$t == t & abs(cen$x - (x + 4)) < 1e-6]
  links <- rbind(c(id_at(2, 10), id_at(1, 10)), c(id_at(2, 60), id_at(1, 60)),
                 c(id_at(3, 5), id_at(2, 10)), c(id_at(3, 15), id_at(2, 10)),
                 c(id_at(3, 60), id_at(2, 60)), c(id_at(4, 5), id_at(3, 5)),
                 c(id_at(5, 15), id_at(3, 15)))
  linked <- c(links[, 1], links[, 2])
  starts <- setdiff(cen$feature_id, links[, 1])
  tr <- rbind(data.frame(feature_id = starts, prev_feature_id = ""),
              data.frame(feature_id = links[, 1], prev_feature_id = links[, 2]))
  tr$t <- cen$t[match(tr$feature_id, cen$feature_id)]
  tr <- tr[order(tr$t, tr$feature_id), ]
  utils::write.table(data.frame(series_id = sid, t = tr$t, feature_id = tr$feature_id,
                                prev_feature_id = tr$prev_feature_id, run_id = cen$run_id[1]),
                     tracks_path(out, sid, "nucleus"), sep = "\t", quote = FALSE, row.names = FALSE)
  list(cen = cen, id_at = id_at)
}

test_that("feature_stat --track_type: ids on every row, a branch table, the summary rules", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli(c("annotate_features_cli.r", "feature_stat_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir(); st <- withr::local_tempdir()
  .time_course(d)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                                           "--min_z_span", "nucleus=2")))
  k <- .tracks_for(out)
  run <- function(...) suppressWarnings(suppressMessages(feature_stat_cli(c(
    "--input", out, "--outdir", st, "--no_plot", ...))))

  s <- run("--track_type", "nucleus")
  bid <- function(t, x) s$branch_id[s$feature_id == k$id_at(t, x)]
  expect_identical(bid(1, 10), "nucleus_track_0001_b001")          # A
  expect_identical(bid(5, 15), "nucleus_track_0001_b003")          # A2, after its gap
  expect_identical(bid(3, 60), "nucleus_track_0002_b001")          # B
  expect_true(is.na(bid(2, 110)))                                 # C, linked to nothing
  expect_true(all(c("track_id", "branch_id", "branch_merged") %in% colnames(
    utils::read.delim(file.path(st, "feature_stats.tsv"), nrows = 1))))
  br <- utils::read.delim(file.path(st, "branches.tsv"), stringsAsFactors = FALSE)
  expect_identical(nrow(br), 4L)
  expect_identical(colnames(br), c("series_id", "feature_type", "track_id", "track_source",
                                   "branch_id", "parent_branch_id", "branch_merged", "first_t",
                                   "last_t", "n_features"))
  expect_identical(unique(br$track_source), "auto")               # no edits here

  # Without --track_type nothing about tracks appears (the option off changes nothing).
  s0 <- run()
  expect_false(any(c("track_id", "branch_id", "branch_merged") %in% colnames(s0)))
  expect_identical(s0[, colnames(s0)], s[, colnames(s0)])

  expect_error(run("--track_type", "nucleus", "--group_by", "track_id"),
               "would pool a lineage")

  # What that refusal points to: counting features per track per t. Track 1
  # holds one cell at t1-2 and two at t3 (A1 and A2) -- counted, not averaged.
  run("--track_type", "nucleus")
  source_cli("count_features_cli.r")
  cn <- file.path(withr::local_tempdir(), "counts")
  ct <- suppressWarnings(suppressMessages(count_features_cli(c(
    "--input", out, "--outdir", cn, "--feature_table", file.path(st, "feature_stats.tsv"),
    "--feature_class_by", "track_id"))))
  n_at <- function(cl, t) ct$n_detected[ct$feature_class == cl & ct$t == t]
  expect_identical(as.integer(c(n_at("nucleus_track_0001", 1), n_at("nucleus_track_0001", 3),
                                n_at("nucleus_track_0002", 3))), c(1L, 2L, 1L))
  expect_identical(as.integer(n_at("unclassified", 2)), 1L)        # C, in no track
})

test_that("feature_stat --group_by branch_id leaves out merged branches and untracked features", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli(c("annotate_features_cli.r", "feature_stat_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir(); st <- withr::local_tempdir()
  .time_course(d)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                                           "--min_z_span", "nucleus=2")))
  .tracks_for(out)
  expect_message(suppressWarnings(feature_stat_cli(c(
    "--input", out, "--outdir", st, "--track_type", "nucleus", "--group_by", "branch_id"))),
    "left out 1 feature\\(s\\) -- 0 of another type, 1 in no branch, 0 on merged branches")
  expect_true(file.exists(file.path(st, "feature_stats.pdf")))
})

# Which palette colours each page of a montage holds. Outlines are thin and
# anti-aliased, so whether any pixel lands on the EXACT colour depends on where
# the line falls on the pixel grid (it did for one object here and not another).
# A pixel is counted for the reference colour it is nearest, and only when it
# is within 30 of it -- blends with the white ground fall out -- and a colour
# is present on a page when more than 50 pixels are. Captions and title are
# turned off by the caller: anti-aliased text makes greys of its own.
.page_colours <- function(tif){
  ref <- c(white = "#FFFFFF", black = "#000000", grey80 = "#CCCCCC", grey60 = "#999999",
           grDevices::palette.colors(palette = "Tableau 10")[-8])   # [-8]: its grey, dropped by qc_palette_colours()
  R <- grDevices::col2rgb(ref)
  pages <- magick::image_read(tif)
  out <- t(vapply(seq_along(pages), function(i){
    m <- matrix(as.integer(magick::image_data(pages[i], channels = "rgb")), ncol = 3)
    d <- vapply(seq_len(ncol(R)), function(j)
      (m[, 1] - R[1, j])^2 + (m[, 2] - R[2, j])^2 + (m[, 3] - R[3, j])^2, numeric(nrow(m)))
    nn <- max.col(-d, ties.method = "first")
    tabulate(nn[d[cbind(seq_along(nn), nn)] < 30^2], nbins = ncol(R))
  }, integer(ncol(R))))
  colnames(out) <- toupper(substr(c(names(ref)[1:4], ref[-(1:4)]), 1, 7))
  colnames(out)[1:4] <- names(ref)[1:4]
  out > 50
}

test_that("montage --color_by branch_id: one colour per branch over every page, untracked grey", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  source_cli(c("annotate_features_cli.r", "feature_outline_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  .time_course(d)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                                           "--min_z_span", "nucleus=2")))
  .tracks_for(out)
  rds <- file.path(out, "TC_features.rds")
  cfg <- file.path(d, "TC_config.txt")
  run <- function(...) suppressWarnings(suppressMessages(feature_outline_cli(c(
    "--features", rds, "--config", cfg, "--no_title", "--no_labels", ...))))
  pages_by <- function(mode){
    tif <- file.path(out, paste0(mode, ".tif"))
    run("--output", tif, "--color_by", mode)
    .page_colours(tif)
  }
  # Tableau 10 in id order. By branch: track_0001 _b001 (A), _b002 (A1), _b003
  # (A2), then track_0002 _b001 (B).
  A <- "#4E79A7"; A1 <- "#F28E2B"; A2 <- "#E15759"; B <- "#76B7B2"
  br <- pages_by("branch_id")
  expect_identical(nrow(br), 5L)
  expect_identical(unname(br[, A]),  c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(unname(br[, A1]), c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(unname(br[, A2]), c(FALSE, FALSE, TRUE, FALSE, TRUE))   # across its gap
  expect_identical(unname(br[, B]),  c(TRUE, TRUE, TRUE, FALSE, FALSE))
  # C, at t2 only and linked to nothing, is the only grey.
  expect_identical(unname(br[, "grey60"]), c(FALSE, TRUE, FALSE, FALSE, FALSE))

  # By lineage, A, A1 and A2 are one colour on every page; B the second.
  tr <- pages_by("track_id")
  expect_identical(unname(tr[, A]),  rep(TRUE, 5))
  expect_identical(unname(tr[, A1]), c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(unname(tr[, A2]), rep(FALSE, 5))
  expect_identical(unname(tr[, "grey60"]), c(FALSE, TRUE, FALSE, FALSE, FALSE))

  # The control: by feature id the same outlines are never grey, so the grey
  # above is the tracks' doing and not something the plot always draws.
  expect_identical(unname(pages_by("feature_id")[, "grey60"]), rep(FALSE, 5))

  expect_error(run("--output", file.path(out, "x.tif"), "--color_by", "branch_id",
                   "--tracks_dir", withr::local_tempdir()), "No tracks table")

  # Code review, PR 3: a feature_stats.tsv made with --track_type carries its
  # own track columns. Given as --feature_table it was joined first, and the
  # tracks join then refused ("already has track_id ... never stored"). The
  # tracks now go first and the table's copies are not brought in; the pages
  # are those drawn without it.
  source_cli("feature_stat_cli.r")
  st <- withr::local_tempdir()
  suppressWarnings(suppressMessages(feature_stat_cli(c("--input", out, "--outdir", st, "--no_plot",
                                                       "--track_type", "nucleus"))))
  ft <- file.path(out, "ft.tif")
  expect_no_error(run("--output", ft, "--color_by", "branch_id",
                      "--feature_table", file.path(st, "feature_stats.tsv")))
  expect_identical(.page_colours(ft), br)
})


# --- hand edits (PR 4) -------------------------------------------------------------

# An edits table as Make_FeatureTracks seeds it (comments, the header, the
# commented-out template) with `rows` -- list(c(action, from, to), ...) --
# filled in below it, each carrying `fp`.
.fp <- function(feats = .feats()) feature_fingerprint(feats)$fingerprint[1]
.write_edits <- function(dir, rows, fp = .fp(), sid = "S", eol = "\n"){
  lines <- c("# Hand corrections to the tracks beside this file, applied by join_tracks() in R",
             "# Keep the fingerprint as written.",
             paste(EDIT_COLUMNS, collapse = "\t"),
             paste("#", paste(c("link", "nucleus_0001", "nucleus_0002", fp, "why"), collapse = "\t")))
  for(r in rows) lines <- c(lines, paste(c(r, fp, "test"), collapse = "\t"))
  path <- track_edits_path(dir, sid, "nucleus")
  writeBin(charToRaw(paste0(paste(lines, collapse = eol), eol)), path)
  path
}
.ids <- function(out){
  u <- unique(out[startsWith(out$feature_id, "nucleus_"), c("feature_id", "track_id", "branch_id")])
  u[order(u$feature_id), ]
}

test_that("no edits, an absent edits file, and the seed alone all change nothing", {
  d <- withr::local_tempdir(); .write_tracks(d)
  base <- join_tracks(.feats(), "nucleus", d)
  .write_edits(d, list(), fp = "a_stale_fp")       # the seed: only a commented template
  expect_identical(join_tracks(.feats(), "nucleus", d), base)
  expect_identical(unique(attr(base, "branches")$track_source), "auto")
})

test_that("cut: a link removed; a feature left linked to nothing has no track", {
  d <- withr::local_tempdir(); .write_tracks(d)
  # 9 (A2 at t5) had one link, from 6: cut, it is linked to nothing -- NA, not
  # a track of one -- and A's lineage, which the cut touched, is 'edited'.
  .write_edits(d, list(c("cut", .f(6), .f(9))))
  expect_message(out <- join_tracks(.feats(), "nucleus", d), "1 hand edit\\(s\\) applied")
  ids <- .ids(out)
  expect_true(is.na(ids$track_id[ids$feature_id == .f(9)]))
  expect_identical(ids$branch_id[ids$feature_id == .f(6)], "nucleus_track_0001_b003")
  src <- unique(attr(out, "branches")[, c("track_id", "track_source")])
  expect_identical(src$track_source, c("edited", "auto"))
  # Cutting A from both daughters splits the lineage in three -- A1 and A2 are
  # connected only through A -- numbered after B (their first features are at
  # t3), and every piece the cuts touched is 'edited'.
  .write_edits(d, list(c("cut", .f(3), .f(5)), c("cut", .f(3), .f(6))))
  out <- suppressMessages(join_tracks(.feats(), "nucleus", d))
  ids <- .ids(out)
  expect_identical(ids$track_id[ids$feature_id %in% .f(c(5, 8))], rep("nucleus_track_0003", 2))
  expect_identical(ids$track_id[ids$feature_id %in% .f(c(6, 9))], rep("nucleus_track_0004", 2))
  src <- unique(attr(out, "branches")[, c("track_id", "track_source")])
  expect_identical(src$track_source[match(sprintf("nucleus_track_%04d", 1:4), src$track_id)],
                   c("edited", "auto", "edited", "edited"))
})

test_that("edits apply in file order: cut, then link the freed feature elsewhere", {
  d <- withr::local_tempdir(); .write_tracks(d)
  # A2's later self (9) is really B's: cut it from A2, link it after B at t3.
  .write_edits(d, list(c("cut", .f(6), .f(9)), c("link", .f(7), .f(9))))
  ids <- .ids(suppressMessages(join_tracks(.feats(), "nucleus", d)))
  expect_identical(ids$branch_id[ids$feature_id == .f(9)], ids$branch_id[ids$feature_id == .f(7)])
  # The same two rows the other way round: link first makes 9 a merge, then
  # the cut leaves it B's alone -- the same links, so the same ids.
  .write_edits(d, list(c("link", .f(7), .f(9)), c("cut", .f(6), .f(9))))
  expect_identical(.ids(suppressMessages(join_tracks(.feats(), "nucleus", d))), ids)
})

test_that("link: a merge by hand -- two predecessors, one lineage, the branch flagged", {
  d <- withr::local_tempdir(); .write_tracks(d)
  .write_edits(d, list(c("link", .f(7), .f(9))))
  out <- suppressMessages(join_tracks(.feats(), "nucleus", d))
  expect_identical(length(unique(stats::na.omit(out$track_id))), 1L)
  expect_true(unique(out$branch_merged[out$feature_id == .f(9)]))
})

test_that("join undoes a cut exactly: the same ids, the track marked edited", {
  d <- withr::local_tempdir(); .write_tracks(d)
  base <- join_tracks(.feats(), "nucleus", d)
  .write_edits(d, list(c("cut", .f(6), .f(9)), c("join", .f(6), .f(9))))
  out <- suppressMessages(join_tracks(.feats(), "nucleus", d))
  expect_identical(.ids(out), .ids(base))
  expect_identical(attr(out, "branches")$track_source, c("edited", "edited", "edited", "auto"))
})

test_that("join across a daughter's break is accepted although her sister shares its t's", {
  # A divides at t3 into A1 (3) and A2 (4). A2 is lost at t4 and comes back as
  # D (7, 9) at t5-6 -- a track of its own -- while A1 (5, 6, 8) runs on
  # through t4-6. A whole-track check would see D overlap A's track in t5-6.
  feats <- data.frame(series_id = "S", roi = paste0("r", 1:9), feature_id = .f(1:9),
                      t = c(1L, 2L, 3L, 3L, 4L, 5L, 5L, 6L, 6L), feature_type = "nucleus",
                      run_id = "r1")
  d <- withr::local_tempdir()
  tr <- data.frame(series_id = "S", t = feats$t, feature_id = feats$feature_id,
                   prev_feature_id = c("", .f(1), .f(2), .f(2), .f(3), .f(5), "", .f(6), .f(7)),
                   run_id = "r1")
  utils::write.table(tr, tracks_path(d, "S", "nucleus"), sep = "\t", quote = FALSE, row.names = FALSE)
  fp <- .fp(feats)
  .write_edits(d, list(c("join", .f(4), .f(7))), fp = fp)
  out <- suppressMessages(join_tracks(feats, "nucleus", d))
  ids <- .ids(out)
  expect_identical(length(unique(ids$track_id)), 1L)
  expect_identical(ids$branch_id[ids$feature_id == .f(9)], ids$branch_id[ids$feature_id == .f(4)])
  # join checks branch ENDS: 3 leads on to 5, and 7 is now led to by 4.
  .write_edits(d, list(c("join", .f(3), .f(7))), fp = fp)
  expect_error(join_tracks(feats, "nucleus", d), "does not end a branch: it leads to nucleus_0005")
  .write_edits(d, list(c("join", .f(4), .f(7)), c("join", .f(4), .f(9))), fp = fp)
  expect_error(join_tracks(feats, "nucleus", d), "line 6 \\(join nucleus_0004 nucleus_0009\\)")
  .write_edits(d, list(c("join", .f(4), .f(9))), fp = fp)                 # 9 follows 7
  expect_error(join_tracks(feats, "nucleus", d), "nucleus_0009 does not start a branch")
})

test_that("each malformed edit is refused, naming its line", {
  d <- withr::local_tempdir(); .write_tracks(d)
  cases <- list(
    "a link must go forward in time; t\\(nucleus_0008\\) = 4" = list(c("link", .f(8), .f(3))),
    "forward in time; t\\(nucleus_0005\\) = 3, t\\(nucleus_0006\\) = 3" = list(c("link", .f(5), .f(6))),
    "nucleus_0001 -> nucleus_0003 is already linked"           = list(c("link", .f(1), .f(3))),
    "there is no link nucleus_0001 -> nucleus_0004 to cut"     = list(c("cut", .f(1), .f(4))),
    "the action must be one of link, cut, join"                = list(c("merge", .f(1), .f(3))),
    "nucleus_0099 is not a tracked feature"                    = list(c("link", .f(1), .f(99))),
    "invalid_nucleus_0001 is not a tracked feature"            = list(c("link", "invalid_nucleus_0001", .f(3))),
    "\\(blank\\) is not a tracked feature"                     = list(c("cut", .f(1), "")))
  for(msg in names(cases)){
    .write_edits(d, cases[[msg]])
    expect_error(join_tracks(.feats(), "nucleus", d), msg, info = msg)
  }
  .write_edits(d, list(c("cut", .f(6), .f(9)), c("cut", .f(6), .f(9))))
  expect_error(join_tracks(.feats(), "nucleus", d), "line 6 \\(cut")      # the second, after the first
})

test_that("edits made on another annotation are refused; on the same one after a VERSION bump, kept", {
  d <- withr::local_tempdir(); .write_tracks(d)
  .write_edits(d, list(c("cut", .f(6), .f(9))), fp = "0123456789")
  expect_error(join_tracks(.feats(), "nucleus", d), "1 of 1 edit\\(s\\) were made on another annotation")
  # A re-annotation that regrouped ROIs: the same ids now hold other ROIs.
  regrouped <- .feats(); regrouped$roi[1:2] <- regrouped$roi[3:4]
  .write_edits(d, list(c("cut", .f(6), .f(9))))
  expect_error(join_tracks(regrouped, "nucleus", d), "made on another annotation")
  # A VERSION bump changes run_id, not what the features are: tracks are
  # regenerated (their run_id must follow), and the edits still apply.
  .write_tracks(d, run_id = "run0000cccc")
  expect_message(join_tracks(.feats(run_id = "run0000cccc"), "nucleus", d), "1 hand edit")
  # An edit row with no fingerprint at all.
  e <- .write_edits(d, list(c("cut", .f(6), .f(9))))
  writeLines(sub("\t[0-9a-f]{10}\ttest$", "\t\ttest", readLines(e)), e)
  expect_error(join_tracks(.feats(run_id = "run0000cccc"), "nucleus", d),
               "line 5 \\(cut nucleus_0006 nucleus_0009\\): no fingerprint")
})

test_that("an edits file that cannot be read is refused, not guessed at; CRLF is fine", {
  d <- withr::local_tempdir(); .write_tracks(d)
  e <- .write_edits(d, list(c("cut", .f(6), .f(9))))
  txt <- readLines(e)
  writeLines(sub("\tnote$", "\tnote\tnote", txt), e)
  expect_error(join_tracks(.feats(), "nucleus", d), "duplicate column\\(s\\): note")
  writeLines(txt[!startsWith(txt, "action")], e)
  expect_error(join_tracks(.feats(), "nucleus", d), "line 4 is not the header")
  .write_edits(d, list(c("cut", .f(6), .f(9))), eol = "\r\n")
  expect_message(join_tracks(.feats(), "nucleus", d), "1 hand edit")
  # Code review, PR 4, each reproduced on Make_FeatureTracks' own seed first.
  # A row typed with spaces was refused for "no fingerprint" -- plainly there
  # on the line; it now says what is wrong.
  e <- .write_edits(d, list(c("cut", .f(6), .f(9))))
  txt <- readLines(e)
  writeLines(c(txt[1:4], gsub("\t", " ", txt[5])), e)
  expect_error(join_tracks(.feats(), "nucleus", d), "line 5 has 1 column\\(s\\) where 4 are needed.*separated by tabs")
  # A spreadsheet's UTF-8 byte-order mark hid the first comment's `#`, and the
  # whole file was refused as having no header. It is dropped now.
  e <- .write_edits(d, list(c("cut", .f(6), .f(9))))
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), readBin(e, "raw", file.size(e))), e)
  expect_message(join_tracks(.feats(), "nucleus", d), "1 hand edit")
})

test_that("end to end: the centroid table's fingerprint is the one join_tracks checks edits against", {
  # The fingerprint a person copies comes from annotate's centroid table, via
  # the seed Make_FeatureTracks writes; join_tracks() recomputes it from the
  # features. If the two were computed differently every edit would be refused.
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli(c("annotate_features_cli.r", "feature_stat_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir(); st <- withr::local_tempdir()
  .time_course(d)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                                           "--min_z_span", "nucleus=2")))
  k <- .tracks_for(out, "TC")
  a2 <- k$id_at(3, 15); a2_later <- k$id_at(5, 15); b3 <- k$id_at(3, 60)
  .write_edits(out, list(c("cut", a2, a2_later), c("link", b3, a2_later)),
               fp = k$cen$fingerprint[1], sid = "TC")
  s <- suppressWarnings(suppressMessages(feature_stat_cli(c(
    "--input", out, "--outdir", st, "--no_plot", "--track_type", "nucleus"))))
  expect_identical(s$branch_id[s$feature_id == a2_later], s$branch_id[s$feature_id == b3])
  br <- utils::read.delim(file.path(st, "branches.tsv"), stringsAsFactors = FALSE)
  expect_identical(sort(unique(br$track_source)), c("edited"))   # both lineages touched
})

# --- review fixes (PR 6) ----------------------------------------------------------

test_that("annotate stamps the Fiji run's time axis above the centroid table's header", {
  skip_if_no_sf()
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  # t = 6 analysed and nothing found there: only the stamp can say so.
  .time_course(d, frames = "1 2 3 4 5 6")
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                                           "--min_z_span", "nucleus=2")))
  f <- file.path(out, "TC_feature_centroids.tsv")
  expect_identical(readLines(f, n = 3)[1:2], c("# frames_analysed: 1 2 3 4 5 6", "# pixel_depth: 1.0"))
  expect_identical(sort(unique(utils::read.delim(f, comment.char = "#")$t)), 1:5)
  # No frames_analysed in the config: that line is left out, not invented.
  d2 <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  .time_course(d2)
  suppressMessages(annotate_features_cli(c("--input", d2, "--feature", "nucleus", "--outdir", out2,
                                           "--min_z_span", "nucleus=2")))
  expect_identical(readLines(file.path(out2, "TC_feature_centroids.tsv"), n = 1), "# pixel_depth: 1.0")
})

# A second series where every nucleus fails --min_z_span (one slice each), and
# one nucleolus that passes: the montage draws the valid features, and the
# reviewer's stop came from there being some.
.empty_course <- function(d, sid = "TD"){
  rows <- lapply(1:3, function(t) .sq(sid, sprintf("nucleus_%04d-0001-%04d-0010", t, t), t, 1, 10, 10, 8))
  utils::write.table(do.call(rbind, rows), file.path(d, paste0(sid, "_nucleus_outline.txt")),
                     sep = "\t", quote = FALSE, row.names = FALSE)
  nl <- rbind(.sq(sid, "nucleolus_0001-0001-0001-0012", 1, 1, 12, 12, 3),
              .sq(sid, "nucleolus_0001-0002-0002-0012", 1, 2, 12, 12, 3))
  utils::write.table(nl, file.path(d, paste0(sid, "_nucleolus_outline.txt")),
                     sep = "\t", quote = FALSE, row.names = FALSE)
}

test_that("a series with no valid feature of the type does not stop the run; a typo does", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli(c("annotate_features_cli.r", "feature_stat_cli.r", "feature_outline_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir(); st <- withr::local_tempdir()
  .time_course(d); .empty_course(d)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "nucleolus",
                                           "--outdir", out, "--min_z_span", "nucleus=2", "nucleolus=2")))
  k <- .tracks_for(out)
  # What Make_FeatureTracks writes for a series without the type: the header.
  utils::write.table(data.frame(series_id = character(0), t = integer(0), feature_id = character(0),
                                prev_feature_id = character(0), run_id = character(0)),
                     tracks_path(out, "TD", "nucleus"), sep = "\t", quote = FALSE, row.names = FALSE)
  expect_message(s <- suppressWarnings(feature_stat_cli(c(
    "--input", out, "--outdir", st, "--no_plot", "--track_type", "nucleus"))),
    "No valid 'nucleus' feature in TD")
  expect_identical(s$branch_id[s$feature_id == k$id_at(1, 10) & s$series_id == "TC"], "nucleus_track_0001_b001")
  expect_true(all(is.na(s$track_id[s$series_id == "TD"])) && any(s$series_id == "TD"))
  expect_error(suppressWarnings(suppressMessages(feature_stat_cli(c(
    "--input", out, "--outdir", st, "--no_plot", "--track_type", "nucleous")))),
    "--track_type nucleous: no feature of that type in any input")
  # The montage of the empty series draws, untracked, rather than stopping.
  tif <- file.path(withr::local_tempdir(), "TD.png")
  expect_message(suppressWarnings(feature_outline_cli(c(
    "--features", file.path(out, "TD_features.rds"), "--output", tif, "--t", "1",
    "--feature", "nucleus", "nucleolus", "--color_by", "branch_id"))), "No valid 'nucleus' feature in TD")
  expect_true(file.exists(tif))
})

test_that("per-branch plot rows: one box per cell, never two series' branches pooled", {
  source_cli("feature_stat_cli.r")
  st <- data.frame(series_id = c("S", "S", "T", "T", "S", "S"),
                   feature_type = c("nucleus", "nucleus", "nucleus", "nucleus", "nucleus", "nucleolus"),
                   branch_id = c("nucleus_track_0001_b001", "nucleus_track_0001_b001",
                                 "nucleus_track_0001_b001", "nucleus_track_0001_b002", NA, NA),
                   branch_merged = c(FALSE, FALSE, FALSE, TRUE, NA, NA), stringsAsFactors = FALSE)
  expect_message(r <- .branch_rows(st, "nucleus", keep_merged = FALSE),
                 "left out 3 feature\\(s\\) -- 1 of another type, 1 in no branch, 1 on merged branches")
  # The same id in S and T is two cells: two labels, not one.
  expect_identical(sort(unique(r$branch_id)), c("S nucleus_track_0001_b001", "T nucleus_track_0001_b001"))
  r2 <- suppressMessages(.branch_rows(st, "nucleus", keep_merged = TRUE))
  expect_identical(nrow(r2), 4L)
  # One series: ids left as they are.
  r1 <- suppressMessages(.branch_rows(st[st$series_id == "S", ], "nucleus", keep_merged = FALSE))
  expect_identical(unique(r1$branch_id), "nucleus_track_0001_b001")
})

test_that("the per-lineage count needs --feature: other types land in unclassified otherwise", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  source_cli(c("annotate_features_cli.r", "feature_stat_cli.r", "count_features_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir(); st <- withr::local_tempdir()
  .time_course(d)
  # Two nucleoli at t = 2, inside A and B.
  nl <- rbind(.sq("TC", "nucleolus_0002-0001-0001-0012", 2, 1, 12, 12, 3),
              .sq("TC", "nucleolus_0002-0002-0002-0012", 2, 2, 12, 12, 3),
              .sq("TC", "nucleolus_0002-0001-0003-0012", 2, 1, 62, 12, 3),
              .sq("TC", "nucleolus_0002-0002-0004-0012", 2, 2, 62, 12, 3))
  utils::write.table(nl, file.path(d, "TC_nucleolus_outline.txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "nucleolus",
                                           "--outdir", out, "--min_z_span", "nucleus=2", "nucleolus=2")))
  .tracks_for(out)
  suppressWarnings(suppressMessages(feature_stat_cli(c(
    "--input", out, "--outdir", st, "--no_plot", "--track_type", "nucleus"))))
  count <- function(...) suppressWarnings(suppressMessages(count_features_cli(c(
    "--input", out, "--outdir", file.path(withr::local_tempdir(), "c"),
    "--feature_table", file.path(st, "feature_stats.tsv"), "--feature_class_by", "track_id", ...))))
  n_unc <- function(ct) as.integer(ct$n_detected[ct$feature_class == "unclassified" & ct$t == 2])
  expect_identical(n_unc(count("--feature", "nucleus")), 1L)       # C alone, as documented
  expect_identical(n_unc(count()), 3L)                             # C and the two nucleoli
})

test_that("a blank fingerprint before a trailing tab is called that, not a spacing problem", {
  d <- withr::local_tempdir(); .write_tracks(d)
  e <- .write_edits(d, list(c("cut", .f(6), .f(9))))
  writeLines(sub("\t[0-9a-f]{10}\ttest$", "\t", readLines(e)), e)
  expect_error(join_tracks(.feats(), "nucleus", d), "line 5 \\(cut nucleus_0006 nucleus_0009\\): no fingerprint")
})

# --- labels: --label_by says what, --label says which (PR 6) -------------------------

test_that("--label parses key=values, OR, argparser's comma split, and refuses a guess", {
  source_cli("cli_helpers.r")
  L <- function(...) .cli_label_spec(c(...), "--label")
  # `feature_id=7,3,1 track_id=2` as argparser delivers it.
  expect_identical(L("feature_id=7", "3", "1", "track_id=2"),
                   list(feature_id = c("7", "3", "1"), track_id = "2"))
  expect_identical(L("feature_id=7 3"), list(feature_id = c("7", "3")))
  expect_identical(L("feature_id=7", "feature_id=3"), list(feature_id = c("7", "3")))
  expect_identical(L("all"), list(all = TRUE))
  expect_identical(L("3b2", "nucleus_0005"), list(branch_id = "3b2", feature_id = "nucleus_0005"))
  expect_null(L())
  expect_error(L("7"), "say what it names: feature_id=7 or track_id=7")
  expect_error(L("all", "feature_id=1"), "not both")
  expect_error(L("branch_id=3"), "a branch needs its track")
  expect_error(L("track_id=3b2"), "is a branch id, under track_id=; write branch_id=3b2")
  expect_error(L("cell=3"), "'cell=' is not a kind")
  expect_error(L("feature_id="), "names no value")
})

test_that("labels: stacked kinds, a selection by any kind, never a feature read as a track", {
  source_r_scripts("plot_outline_topView.r")
  source_cli("cli_helpers.r")
  L <- function(...) .cli_label_spec(c(...), "--label")
  # Feature 5 is in track 1; track 5 holds feature 9. Before --label_by,
  # `--label nucleus_0005` under branch colouring kept only the number and
  # labelled TRACK 5's branch -- nucleus_0009, another object.
  tab <- data.frame(feature_id = c("nucleus_0005", "nucleus_0007", "nucleus_0009", "nucleus_0011", "nucleolus_0001"),
                    feature_type = c(rep("nucleus", 4), "nucleolus"),
                    track_id = c("nucleus_track_0001", "nucleus_track_0001", "nucleus_track_0005", NA, NA),
                    branch_id = c("nucleus_track_0001_b001", "nucleus_track_0001_b002",
                                  "nucleus_track_0005_b001", NA, NA))
  expect_identical(qc_labels(tab, "nucleus", L("nucleus_0005"), "branch_id"),
                   c("0001b001", NA, NA, NA, NA))
  # Stacked, in --label_by's order; an untracked feature keeps what it has.
  expect_identical(qc_labels(tab, "nucleus", L("all"), c("feature_id", "branch_id")),
                   c("0005\n0001b001", "0007\n0001b002", "0009\n0005b001", "0011", NA))
  # OR across kinds; zeros optional in the short form.
  expect_identical(qc_labels(tab, "nucleus", L("track_id=5", "branch_id=1b2"), "feature_id"),
                   c(NA, "0007", "0009", NA, NA))
  expect_identical(qc_labels(tab, "nucleus", L("branch_id=0001b001"), "feature_id"),
                   c("0005", NA, NA, NA, NA))
  expect_error(qc_labels(tab, "nucleus", L("nucleolus_0001"), "feature_id"),
               "labels act on the focus type, nucleus. Name nucleolus first in --feature")
  expect_warning(qc_labels(tab, "nucleus", L("track_id=9"), "feature_id"),
                 "No track nucleus_track_0009 to label")
  expect_error(qc_labels(tab[, 1:2], "nucleus", L("all"), "branch_id"), "the tracks are not joined")
})

test_that("feature_outline --label_by: tracks read for labels alone, and bad options stop early", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2", "magick"))
  source_cli(c("annotate_features_cli.r", "feature_outline_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  .time_course(d)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                                           "--min_z_span", "nucleus=2")))
  .tracks_for(out)
  run <- function(...) suppressWarnings(feature_outline_cli(c(
    "--features", file.path(out, "TC_features.rds"), "--config", file.path(d, "TC_config.txt"),
    "--output", file.path(withr::local_tempdir(), "m.tif"), ...)))
  # Class colouring, branch labels: the tracks are read because a label needs them.
  expect_message(run("--label_by", "feature_id", "branch_id", "--label", "track_id=1"),
                 "2 nucleus track\\(s\\), 4 branch\\(es\\)")
  expect_error(run("--label", "7"), "say what it names")
  expect_error(run("--label_by", "class"), "--label_by takes feature_id, track_id and/or branch_id")
  expect_error(run("--color_by", "branch_id", "--label", "track_id=3b2"), "write branch_id=3b2")
  expect_warning(feature_outline_cli(c(
    "--features", file.path(out, "TC_features.rds"), "--config", file.path(d, "TC_config.txt"),
    "--output", file.path(withr::local_tempdir(), "m.tif"), "--tracks_dir", out, "--label", "all")),
    "--tracks_dir is used only when")
})
