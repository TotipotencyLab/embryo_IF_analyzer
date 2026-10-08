# Tracks joined onto features (tracking milestone, PR 3: note/time_series_plan.md
# §4). The numbering cases are the plan's own examples, so a wrong id is a
# visible disagreement with the design rather than a matter of taste; the
# refusal cases are each a tracks table that would otherwise join cleanly onto
# the wrong nuclei, or shrink a track without a word.

source_cli("cli_helpers.r")
source_r_scripts(c("feature_join.r", "feature_tracks.r"))

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
  # A (t1-2) _b01, A1 (t3-4) _b02, A2 (t3, t5) _b03, all one lineage; B its own.
  expect_identical(unname(ids[.f(c(1, 3))]), rep("nucleus_track_0001_b01", 2))
  expect_identical(unname(ids[.f(c(5, 8))]), rep("nucleus_track_0001_b02", 2))
  expect_identical(unname(ids[.f(c(6, 9))]), rep("nucleus_track_0001_b03", 2))
  expect_identical(unname(ids[.f(c(2, 4, 7))]), rep("nucleus_track_0002_b01", 3))
  expect_identical(sort(unique(r$features$track_id)), c("nucleus_track_0001", "nucleus_track_0002"))
  b <- r$branches
  expect_identical(b$parent_branch_id, c("", "nucleus_track_0001_b01", "nucleus_track_0001_b01", ""))
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
  expect_identical(m$parent_branch_id, "nucleus_track_0001_b01;nucleus_track_0001_b02")
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
  expect_identical(unique(a$branch_id), "nucleus_track_0001_b03")
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
  expect_error(join_tracks(.feats(), "oocyte", d), "No 'oocyte' features")
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
.time_course <- function(d, sid = "TC"){
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
  utils::write.table(data.frame(parameter = c("series_id", "pixel_depth", "image_width",
                                              "image_height", "pixel_width", "pixel_height"),
                                value = c(sid, "1.0", "140", "50", "1", "1")),
                     file.path(d, paste0(sid, "_config.txt")), sep = "\t", quote = FALSE,
                     row.names = FALSE)
  objs
}
# The links Make_FeatureTracks would write for that course, by object name,
# using the ids annotate actually assigned (read from its centroid table).
.tracks_for <- function(out, sid = "TC"){
  cen <- utils::read.delim(file.path(out, paste0(sid, "_feature_centroids.tsv")), stringsAsFactors = FALSE)
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
  expect_identical(bid(1, 10), "nucleus_track_0001_b01")          # A
  expect_identical(bid(5, 15), "nucleus_track_0001_b03")          # A2, after its gap
  expect_identical(bid(3, 60), "nucleus_track_0002_b01")          # B
  expect_true(is.na(bid(2, 110)))                                 # C, linked to nothing
  expect_true(all(c("track_id", "branch_id", "branch_merged") %in% colnames(
    utils::read.delim(file.path(st, "feature_stats.tsv"), nrows = 1))))
  br <- utils::read.delim(file.path(st, "branches.tsv"), stringsAsFactors = FALSE)
  expect_identical(nrow(br), 4L)
  expect_identical(colnames(br), c("series_id", "feature_type", "track_id", "branch_id",
                                   "parent_branch_id", "branch_merged", "first_t", "last_t",
                                   "n_features"))

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
    "left out 1 feature\\(s\\) -- 1 in no branch, 0 on merged branches")
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
  source_cli(c("annotate_features_cli.r", "montage_qc_cli.r"))
  d <- withr::local_tempdir(); out <- withr::local_tempdir()
  .time_course(d)
  suppressMessages(annotate_features_cli(c("--input", d, "--feature", "nucleus", "--outdir", out,
                                           "--min_z_span", "nucleus=2")))
  .tracks_for(out)
  rds <- file.path(out, "TC_features.rds")
  cfg <- file.path(d, "TC_config.txt")
  run <- function(...) suppressWarnings(suppressMessages(montage_qc_cli(c(
    "--features", rds, "--config", cfg, "--no_title", "--no_labels", ...))))
  pages_by <- function(mode){
    tif <- file.path(out, paste0(mode, ".tif"))
    run("--output", tif, "--color_by", mode)
    .page_colours(tif)
  }
  # Tableau 10 in id order. By branch: track_0001 _b01 (A), _b02 (A1), _b03
  # (A2), then track_0002 _b01 (B).
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
