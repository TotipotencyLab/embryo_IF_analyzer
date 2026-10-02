# The documented formats, asserted against the code that reads and writes them.
#
# A format description in prose drifts silently; the point of these is that it
# cannot. Each one pins something note/data_formats.md claims.

source_cli("cli_helpers.r")

# --- the shipped template -----------------------------------------------------

test_that("the series table template loads through the real reader", {
  tmpl <- file.path(repo_root(), "config", "series_template.tsv")
  expect_true(file.exists(tmpl))

  sheet <- .cli_read_series_sheet(tmpl)
  expect_true("series_id" %in% colnames(sheet))
  expect_gt(nrow(sheet), 0)
  expect_false(anyDuplicated(sheet$series_id) > 0)
})

test_that("the template's first series is the fixture, so it can be run as-is", {
  tmpl <- file.path(repo_root(), "config", "series_template.tsv")
  skip_if_no_fixture(fixture_file("nucleus", "outline"))

  sheet <- .cli_read_series_sheet(tmpl)
  fixture_id <- sub("_nucleus_outline\\.txt$", "",
                    basename(fixture_file("nucleus", "outline")))
  expect_true(fixture_id %in% sheet$series_id)
})

test_that("only series_id is required; other columns are free metadata", {
  d <- withr::local_tempdir()
  p <- file.path(d, "minimal.tsv")
  write.table(data.frame(series_id = c("A", "B")), p,
              sep = "\t", quote = FALSE, row.names = FALSE)
  expect_identical(nrow(.cli_read_series_sheet(p)), 2L)
})

test_that("the series table reader accepts tsv, txt and csv", {
  d <- withr::local_tempdir()
  df <- data.frame(series_id = "A", genotype = "wt")
  for (ext in c("tsv", "txt")) {
    p <- file.path(d, paste0("s.", ext))
    write.table(df, p, sep = "\t", quote = FALSE, row.names = FALSE)
    expect_identical(.cli_read_series_sheet(p)$genotype, "wt", info = ext)
  }
  p <- file.path(d, "s.csv")
  write.csv(df, p, row.names = FALSE)
  expect_identical(.cli_read_series_sheet(p)$genotype, "wt")
})

# --- the Fiji output contract -------------------------------------------------

test_that("the fixture matches the documented outline columns", {
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  hdr <- names(read.table(fixture_file("nucleus", "outline"),
                          header = TRUE, nrows = 1, stringsAsFactors = FALSE))
  expect_identical(hdr, c("name", "roi", "z", "x", "y"))
})

test_that("ROI ids match the documented <feature>_SSSS-NNNN-YYYY form", {
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  o <- read.table(fixture_file("nucleus", "outline"), header = TRUE,
                  stringsAsFactors = FALSE)
  expect_true(all(grepl("^nucleus_\\d{4}-\\d{4}-\\d{4}$", unique(o$roi))))
})

test_that("read_fiji_result returns the documented columns", {
  skip_if_no_fixture(fixture_file("nucleus", "res"))
  source_r_scripts("read_fiji_result.r")
  res <- read_fiji_result(fixture_file("nucleus", "res"))
  expect_identical(
    colnames(res),
    c("label", "area", "mean", "stddev", "min", "max", "x", "y", "circ",
      "intden", "median", "rawintden", "ch", "slice", "ar", "round",
      "solidity", "filename", "roi", "pos", "z"))
})

test_that("the config file is a two-column parameter/value table", {
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  cfg <- file.path(fixture_dir(), "GRV_Position010_config.txt")
  skip_if_no_fixture(cfg)
  d <- read.delim(cfg, stringsAsFactors = FALSE)
  expect_identical(colnames(d), c("parameter", "value"))
  # Fields other code looks up by name.
  for (k in c("pixel_width", "pixel_height", "pixel_unit", "script")) {
    expect_true(k %in% d$parameter, info = k)
  }
  # The fixture predates pixel_depth and is deliberately NOT regenerated: an
  # older config must stay readable, and this is the case that proves it.
  expect_false("pixel_depth" %in% d$parameter)
})

test_that("the sheet column schema is readable and its owners are known", {
  # schema/sheet_columns.tsv exists so this list does NOT live twice, once in
  # Groovy and once in R. This is the R half reading it; Test_SeriesSheet is
  # the Groovy half.
  f <- file.path(repo_root(), "schema", "sheet_columns.tsv")
  skip_if_not(file.exists(f), "schema/sheet_columns.tsv not found")
  d <- read.delim(f, stringsAsFactors = FALSE)
  expect_identical(colnames(d),
                   c("sheet", "column", "owner", "type", "required", "description"))
  expect_true(all(d$sheet %in% c("files", "series", "sources")))
  expect_true(all(d$owner %in% c("machine", "seeded", "user")))
  expect_true(all(d$type %in% c("string", "integer", "double", "boolean")))
  expect_true(all(d$required %in% c("yes", "no")))

  # series_id is the join key the R side requires, and it must be seeded rather
  # than machine: it is yours to edit, and regeneration must not overwrite it.
  sid <- d[d$sheet == "series" & d$column == "series_id", ]
  expect_identical(nrow(sid), 1L)
  expect_identical(sid$owner, "seeded")
  expect_identical(sid$required, "yes")
  expect_identical(d$owner[d$sheet == "series" & d$column == "series_index"], "machine")
  # The pre-v0.7.0 names are gone, not merely joined by the new ones.
  expect_false(any(d$sheet == "samples"))
  expect_false(any(d$column == "prefix"))

  # `sources` is the Luxendo SOURCES table -- one row per .lux.h5, keyed to the
  # series it feeds. It is never handed to a CLI.
  man <- d$column[d$sheet == "sources"]
  expect_true(all(c("source_path", "alias", "series_index", "channel", "t") %in% man))
  # The join to series.tsv is (alias, series_index) -- two MACHINE columns on
  # the series table -- and NOT series_id, which is editable: a join on an id
  # the operator may change would orphan every source the moment they did.
  expect_false("series_id" %in% man)
  for (col in c("alias", "series_index")) {
    expect_identical(d$required[d$sheet == "sources" & d$column == col], "yes")
    expect_identical(d$owner[d$sheet == "series" & d$column == col], "machine")
  }
  # NO include here: it is a property of the series, so it lives on the series
  # table. One value per series makes "the channels of this output disagree
  # about include" unrepresentable rather than merely handled.
  expect_false("include" %in% man)
  # NO stored output path: it was only ever series_id + ".tif", so it is derived.
  # A stored copy of a derived value is a second thing to keep in step.
  expect_false("target_output_path" %in% man)
  expect_false("output_path" %in% man)
  # channel_name belongs here and NOT on the series table, because it is per
  # channel and the series table has one row per series.
  expect_true("channel_name" %in% man)
  # pixel_depth must be optional here for the same reason it is blank in
  # _config.txt: a single plane has no z step to record.
  expect_identical(d$required[d$sheet == "sources" & d$column == "pixel_depth"], "no")
  # The four series columns a Luxendo row reads its own way must stay REQUIRED
  # -- defined to be true for every format, not relaxed for one.
  for (col in c("path", "series_index", "series_name", "alias")) {
    expect_identical(d$required[d$sheet == "series" & d$column == col], "yes")
  }

  # Every declared column has to be described where people look for it.
  doc <- paste(readLines(file.path(repo_root(), "note", "data_formats.md"),
                         warn = FALSE), collapse = "\n")
  words <- unlist(regmatches(doc, gregexpr("[A-Za-z_]+", doc)))
  for (sh in c("series", "sources")) {
    undocumented <- setdiff(d$column[d$sheet == sh], words)
    expect_identical(undocumented, character(0))
  }
})

test_that("pixel_depth is written by the Groovy side and documented here", {
  # Cross-language, so it is a source check rather than a call: the writer is
  # Groovy and the documentation is Markdown, and the failure being guarded
  # against is one of them changing without the other.
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  skip_if_not(file.exists(np), "NucleusPipeline.groovy not found")
  src <- paste(readLines(np, warn = FALSE), collapse = "\n")
  expect_match(src, "pixel_depth", fixed = TRUE)
  # Blank for a single plane, not ImageJ's default 1.0 -- a z step that does
  # not exist must not arrive looking usable.
  expect_match(src, "getNSlices() > 1", fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  # The ROW in the _config.txt field table, not merely a mention of the name:
  # the first version of this assertion passed while the row was deleted,
  # because another paragraph elsewhere happened to say "pixel_depth".
  row <- grep("^\\|\\s*`pixel_depth`\\s*\\|", doc, value = TRUE)
  expect_length(row, 1L)
  expect_match(row, "single plane", fixed = TRUE)
})

test_that("open_method is written, readable back, and documented", {
  # Two ways of opening an image now exist and are asserted to produce identical
  # output; which one ran must still be recorded, or two runs that differed are
  # indistinguishable afterwards. Same reasoning as recording VERSION.
  br <- file.path(repo_root(), "scripts", "groovy", "BatchRunner.groovy")
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  rc <- file.path(repo_root(), "scripts", "groovy", "RunConfig.groovy")
  skip_if_not(all(file.exists(br, np, rc)), "Groovy library not found")

  br_src <- paste(readLines(br, warn = FALSE), collapse = "\n")
  expect_match(br_src, "OPEN_MODES", fixed = TRUE)
  # The summary column, so a long run says per row which reader opened it.
  expect_match(br_src, '"open_method"', fixed = TRUE)

  # Written into _config.txt ...
  expect_match(paste(readLines(np, warn = FALSE), collapse = "\n"),
               "open_method", fixed = TRUE)
  # ... and therefore readable back: an unknown key in a config is a hard error,
  # so a provenance field that is not registered makes a run's own _config.txt
  # unusable as the config of the next run.
  expect_match(paste(readLines(rc, warn = FALSE), collapse = "\n"),
               '"open_method"', fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  # The ROWS in the two field tables, not a passing mention: _config.txt and
  # batch_summary.tsv each document it.
  rows <- grep("^\\|\\s*`open_method`\\s*\\|", doc, value = TRUE)
  expect_length(rows, 2L)
  expect_true(any(grepl("importer", rows, fixed = TRUE)))
  expect_true(any(grepl("reader", rows, fixed = TRUE)))
})

test_that("the Fiji circularity filter records what it deleted, and is documented", {
  # This filter runs inside Analyze Particles, so a rejected ROI never reaches
  # _outline.txt -- there is no row for --bridge_roi to promote and no record
  # beyond the count. That makes the count part of the format, not a log line.
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  rc <- file.path(repo_root(), "scripts", "groovy", "RunConfig.groovy")
  skip_if_not(all(file.exists(np, rc)), "Groovy library not found")

  np_src <- paste(readLines(np, warn = FALSE), collapse = "\n")
  expect_match(np_src, "nucleus_circularity", fixed = TRUE)
  expect_match(np_src, "nucleus_circ_rejected", fixed = TRUE)
  # Registered as provenance, or a run's own _config.txt stops being readable
  # back in as the config of the next run.
  expect_match(paste(readLines(rc, warn = FALSE), collapse = "\n"),
               '"nucleus_circ_rejected"', fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  # The ROW in the _config.txt field table, not a passing mention elsewhere.
  row <- grep("^\\|\\s*`nucleus_circ_rejected`\\s*\\|", doc, value = TRUE)
  expect_length(row, 1L)
  # Blank, not 0: "0" would claim a filter ran and removed nothing.
  expect_match(row, "Blank when the filter was off", fixed = TRUE)
})

test_that("the overview settings are parameters and survive the config round trip", {
  # They were constants. A config from a GUI run that is fed to the batch has to
  # carry them, or the batch silently falls back to the defaults and produces
  # different pictures from the ones that were tuned.
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  tmpl <- file.path(repo_root(), "config", "nucleus_config_template.txt")
  skip_if_not(all(file.exists(np, tmpl)), "Groovy library not found")

  keys <- c("overview_method", "overview_width", "overview_height",
            "overview_contrast", "overview_saturated")
  np_src <- paste(readLines(np, warn = FALSE), collapse = "\n")
  for (k in keys) {
    # Declared as a parameter, AND written into the config -- two occurrences.
    expect_gte(lengths(regmatches(np_src, gregexpr(k, np_src, fixed = TRUE))), 2L)
  }
  # The shipped template is what a user copies; a parameter missing from it is
  # a parameter nobody knows exists.
  cfg <- read.delim(tmpl, comment.char = "#", stringsAsFactors = FALSE)
  expect_true(all(keys %in% cfg$parameter))
  expect_true("nucleus_circularity" %in% cfg$parameter)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  expect_true(any(grepl("`overview_width`", doc, fixed = TRUE)))
  # The trap that cost a test: auto contrast normalises two different
  # projections into identical bytes.
  expect_true(any(grepl("stretches each picture to full range", doc, fixed = TRUE)))
})

test_that("the threshold a run used is recorded, and is provenance not a parameter", {
  # The number the auto method chose was thrown away until now, so a run could
  # not say what it had thresholded at -- no way to tell a sensible threshold
  # from a disastrous one afterwards, and no way to read a value off in order to
  # pin it. Recorded as the RANGE it selected, so pinning is copy-paste.
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  rd <- file.path(repo_root(), "scripts", "groovy", "RoiDetect.groovy")
  rc <- file.path(repo_root(), "scripts", "groovy", "RunConfig.groovy")
  br <- file.path(repo_root(), "scripts", "groovy", "BatchRunner.groovy")
  skip_if_not(all(file.exists(np, rd, rc, br)), "Groovy library not found")

  np_src <- paste(readLines(np, warn = FALSE), collapse = "\n")
  expect_match(np_src, "nucleus_threshold_used", fixed = TRUE)
  expect_match(np_src, "nucleus_mask_pct", fixed = TRUE)

  # Provenance, so a run's own config still reads back in -- and so that feeding
  # a config forward RE-DERIVES the threshold instead of freezing one image's.
  rc_src <- paste(readLines(rc, warn = FALSE), collapse = "\n")
  expect_match(rc_src, '"nucleus_threshold_used"', fixed = TRUE)
  expect_match(rc_src, '"nucleus_mask_pct"', fixed = TRUE)

  # The batch columns, so finding the rows where it went wrong does not mean
  # opening a thousand _config.txt files.
  br_src <- paste(readLines(br, warn = FALSE), collapse = "\n")
  expect_match(br_src, '"threshold", "mask_pct"', fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  # A ROW in each of the two field tables, not a passing mention.
  expect_length(grep("^\\|\\s*`nucleus_threshold_used`\\s*\\|", doc), 1L)
  expect_length(grep("^\\|\\s*`nucleus_mask_pct`\\s*\\|", doc), 1L)
  expect_length(grep("^\\|\\s*`threshold`\\s*\\|", doc), 1L)
  expect_length(grep("^\\|\\s*`mask_pct`\\s*\\|", doc), 1L)
  # The rule that makes it safe to feed a config forward.
  expect_true(any(grepl("pinning a threshold is a", doc, fixed = TRUE)))

  # A uniform frame has no threshold and says so, rather than the pre-v0.3.0
  # behaviour of silently handing back an unthresholded image as the mask.
  expect_match(paste(readLines(rd, warn = FALSE), collapse = "\n"),
               "NO_THRESHOLD", fixed = TRUE)
  expect_true(any(grepl("A uniform frame has no threshold", doc, fixed = TRUE)))
  expect_true(any(grepl("v0.3.0 and earlier", doc, fixed = TRUE)))
})

test_that("Manual thresholding and the per-slice option are parameters, and documented", {
  # nucleus_threshold_range is only read when the method is Manual, the same
  # shape as nucleolus_rel_fraction. A parameter missing from the written config
  # silently becomes the default on the next run, which is the whole hazard the
  # round-trip format exists to prevent -- so it must be in NucleusPipeline
  # twice: declared, and written.
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  rd <- file.path(repo_root(), "scripts", "groovy", "RoiDetect.groovy")
  nd <- file.path(repo_root(), "scripts", "groovy", "NucleolusDetect.groovy")
  tmpl <- file.path(repo_root(), "config", "nucleus_config_template.txt")
  skip_if_not(all(file.exists(np, rd, nd, tmpl)), "Groovy library not found")

  np_src <- paste(readLines(np, warn = FALSE), collapse = "\n")
  for (k in c("nucleus_threshold_range", "nucleus_stack_histogram")) {
    expect_gte(lengths(regmatches(np_src, gregexpr(k, np_src, fixed = TRUE))), 2L)
  }
  cfg <- read.delim(tmpl, comment.char = "#", stringsAsFactors = FALSE)
  expect_true(all(c("nucleus_threshold_range", "nucleus_stack_histogram") %in% cfg$parameter))
  # Off is not a sensible default for the risky one.
  expect_equal(cfg$value[cfg$parameter == "nucleus_stack_histogram"], "true")

  # Both features now take their algorithms from the same plugin. The nucleolus
  # used ImageJ's own enum, which has no Huang2 -- the nucleus default -- so the
  # same word meant something in one field and threw in the other.
  nd_src <- paste(readLines(nd, warn = FALSE), collapse = "\n")
  expect_match(nd_src, "fiji.threshold.Auto_Threshold", fixed = TRUE)
  expect_false(grepl("AutoThresholder.Method.valueOf", nd_src, fixed = TRUE))

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  expect_length(grep("^\\|\\s*`nucleus_threshold_range`\\s*\\|", doc), 1L)
  expect_length(grep("^\\|\\s*`nucleus_stack_histogram`\\s*\\|", doc), 1L)
  # The two hazards, stated where the parameters are described.
  expect_true(any(grepl("raw pixel value, and does not travel", doc, fixed = TRUE)))
  expect_true(any(grepl("empty slice's noise become", doc, fixed = TRUE)))
  # And the per-slice reporting shape, which is not a pasteable range.
  expect_true(any(grepl("per-slice", doc, fixed = TRUE)))

  # CLAUDE.md's persist=false decision said the tuning dialog remembers
  # everything; three fields are now the exception (the series id since
  # v0.7.0), so that claim had to move.
  # The METHOD is not one of them -- it persists, and is safe to because the
  # range does not, so a stale "Manual" is refused rather than silently reusing
  # a pixel value from another image.
  cl <- readLines(file.path(repo_root(), "CLAUDE.md"), warn = FALSE)
  expect_true(any(grepl("Three fields there are the exception", cl, fixed = TRUE)))
  rns <- readLines(file.path(repo_root(), "scripts", "groovy",
                             "Run_NucleusSelector.groovy"), warn = FALSE)
  np_line <- grep("nucRange$", rns, value = TRUE)
  expect_length(np_line, 1L)
  expect_match(np_line, "persist=false", fixed = TRUE)
  sh_line <- grep("nucStackHist$", rns, value = TRUE)
  expect_match(sh_line, "persist=false", fixed = TRUE)
  # ...and the method line deliberately does NOT carry it.
  m_line <- grep("nucMethod$", rns, value = TRUE)
  expect_length(m_line, 1L)
  expect_false(grepl("persist=false", m_line, fixed = TRUE))
})

test_that("every run-config parameter is written back, and the rule is recorded", {
  # An absent key in _config.txt is legal -- readParams rejects unknown keys,
  # not missing ones -- so a parameter the writer forgets silently becomes its
  # DEFAULT on the next run. The round trip fails while looking like it worked.
  # Three couplings around this file already had a set-difference assertion and
  # never drifted; this one had none and drifted three times.
  np <- file.path(repo_root(), "scripts", "groovy", "NucleusPipeline.groovy")
  tnp <- file.path(repo_root(), "tests", "groovy", "Test_NucleusPipeline.groovy")
  skip_if_not(all(file.exists(np, tnp)), "Groovy library not found")

  np_src <- paste(readLines(np, warn = FALSE), collapse = "\n")
  # The five that were missing, now written.
  for (k in c("save_roi_zips", "save_outlines", "save_measurements",
              "save_config", "save_overview")) {
    expect_gte(lengths(regmatches(np_src, gregexpr(k, np_src, fixed = TRUE))), 2L)
  }
  # The guard itself, in the test that observes a config it actually produced
  # rather than scraping source for key names.
  tnp_src <- paste(readLines(tnp, warn = FALSE), collapse = "\n")
  expect_match(tnp_src, "every parameter is written back", fixed = TRUE)
  # With no exclusions since v0.7.0 (output_prefix is retired): the guard is a
  # bare set difference, and the series id is checked as provenance beside it.
  expect_match(tnp_src, "(NP.PARAM_TYPES.keySet() - writtenKeys)", fixed = TRUE)
  expect_match(tnp_src, "the series id is recorded", fixed = TRUE)

  cl <- readLines(file.path(repo_root(), "CLAUDE.md"), warn = FALSE)
  expect_true(any(grepl("Every `PARAM_TYPES` key must be written back", cl, fixed = TRUE)))
  # The four couplings, as a table naming where each is asserted.
  expect_true(any(grepl("what `saveRunConfig()` actually writes", cl, fixed = TRUE)))
  # And why the series table needs none of it: one run-time schema file, read by
  # both languages. Someone "fixing" the sheet to look like the config would be
  # going backwards.
  expect_true(any(grepl("read at \\*run time\\* by `SheetSchema.groovy`", cl)))

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  # No exclusion since v0.7.0 retired output_prefix; the doc must say so rather
  # than go on describing one.
  expect_true(any(grepl("There is **no exception** to that since v0.7.0", doc, fixed = TRUE)))
  expect_false(any(grepl("single deliberate exception", doc, fixed = TRUE)))
  # The writer/reader inventory: the R CLIs read this file by hard-coded field
  # name and nothing checks them against the writer.
  expect_true(any(grepl("no run-time schema file", doc, fixed = TRUE)))
})

test_that("the batch overview switch overrides the config instead of replacing it", {
  # save_overview is a real parameter and a config now carries it, so a Boolean
  # on the batch dialog would be two sources of truth with the config's value
  # permanently unreachable -- a Boolean has no third state for "leave it".
  b <- file.path(repo_root(), "scripts", "groovy", "Run_NucleusSelector_Batch.groovy")
  skip_if_not(file.exists(b), "batch runner not found")
  src <- paste(readLines(b, warn = FALSE), collapse = "\n")
  expect_match(src, "(from config)", fixed = TRUE)
  expect_false(grepl("Boolean (persist=false, label=\"Save overview", src, fixed = TRUE))
  # The vocabulary and the refusal live in BatchRunner, not in the front end:
  # a `#@` script is only ever compiled by the suite, never run, so a check
  # written there could not be tested at all.
  br <- file.path(repo_root(), "scripts", "groovy", "BatchRunner.groovy")
  br_src <- paste(readLines(br, warn = FALSE), collapse = "\n")
  expect_match(br_src, "overviewOverride", fixed = TRUE)
  expect_match(br_src, "OVERVIEW_CHOICES", fixed = TRUE)
  # NOT asserted here: the caller in sandbox/, which passed the old boolean.
  # sandbox/ is gitignored, so a check on it would pass vacuously on every
  # checkout but this one.
  # The dead field is gone, and since v0.7.0 so is output_prefix itself: the
  # sheet's series_id is the whole name.
  expect_false(grepl("outPrefix", src, fixed = TRUE))
})

test_that("the series_id carries the series index, and the doc says so", {
  # Series names repeat -- a tile scan is many series under one name -- so the
  # index is what makes the id unique WITHIN a file, as the alias does across
  # files. Forced always, never only where names happen to collide: an id that
  # depends on which other series share its name is an id that changes when a
  # file is re-acquired.
  ss <- file.path(repo_root(), "scripts", "groovy", "SeriesSheet.groovy")
  skip_if_not(file.exists(ss), "SeriesSheet.groovy not found")
  src <- paste(readLines(ss, warn = FALSE), collapse = "\n")
  # Fixed width, not derived from the series count.
  expect_match(src, 'INDEX_FORMAT = "s%04d"', fixed = TRUE)

  schema <- read.delim(file.path(repo_root(), "schema", "sheet_columns.tsv"),
                       stringsAsFactors = FALSE)
  id_row <- schema[schema$sheet == "series" & schema$column == "series_id", ]
  expect_equal(nrow(id_row), 1L)
  expect_match(id_row$description, "s<NNNN>", fixed = TRUE)

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  expect_true(any(grepl("sanitise(<alias>_s<NNNN>_<series_name>)", doc, fixed = TRUE)))
  # The old two-part form must be gone, not merely joined by the new one.
  expect_false(any(grepl("`sanitise(<alias>_<series_name>)`", doc, fixed = TRUE)))
})

test_that("a duplicate series_id is refused downstream, allowed at sheet time", {
  # The asymmetry is deliberate: the sheet step is a draft for a person to read,
  # so it writes the file and THEN fails -- a duplicate you cannot open the
  # table to see is one you cannot fix. The analysis step has no such excuse.
  ms <- file.path(repo_root(), "scripts", "groovy", "Make_SeriesSheet.groovy")
  br <- file.path(repo_root(), "scripts", "groovy", "BatchRunner.groovy")
  skip_if_not(all(file.exists(ms, br)), "Groovy front ends not found")

  ms_src <- readLines(ms, warn = FALSE)
  # Anchored on the BUILD-mode write: scan mode writes with the same call prefix.
  write_at <- grep("TSV.write(rows, outSheet, sheet.columnOrder(rows))", ms_src, fixed = TRUE)
  throw_at <- grep("duplicated series_id", ms_src, fixed = TRUE)
  expect_length(write_at, 1L)
  expect_length(throw_at, 1L)
  # The ordering IS the feature.
  expect_lt(write_at, throw_at)
  expect_match(paste(ms_src, collapse = "\n"), "allowDuplicateId", fixed = TRUE)

  expect_match(paste(readLines(br, warn = FALSE), collapse = "\n"),
               "share a series_id", fixed = TRUE)

  # R has refused one all along; this pins that it still does, since the
  # documented contract now leans on it.
  expect_match(paste(readLines(file.path(repo_root(), "scripts", "R_cli",
                                         "cli_helpers.r"), warn = FALSE),
                     collapse = "\n"),
               "Duplicate '", fixed = TRUE)
})

test_that("both ends read `include` the same way, and the doc says so", {
  # One sheet, two readers. A word that means "run this" to Fiji and something
  # else to R would show up only as a sample count, months later.
  br <- file.path(repo_root(), "scripts", "groovy", "BatchRunner.groovy")
  skip_if_not(file.exists(br), "BatchRunner.groovy not found")
  groovy <- paste(readLines(br, warn = FALSE), collapse = "\n")

  # The accepted words, asserted against the Groovy source rather than restated.
  for (word in c("true", "yes", "false", "no")) {
    expect_match(groovy, paste0('"', word, '"'), fixed = TRUE)
  }
  expect_true(all(.cli_is_included(c("true", "yes", "1"))))
  expect_false(any(.cli_is_included(c("false", "no", "0"))))
  expect_error(.cli_is_included("maybe"), "true/false")

  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  expect_true(any(grepl("BatchRunner.isIncluded", doc, fixed = TRUE)))
})

test_that("the machine columns R drops are the schema's, not a second list", {
  schema <- read.delim(file.path(repo_root(), "schema", "sheet_columns.tsv"),
                       stringsAsFactors = FALSE)
  want <- schema$column[schema$sheet == "series" & schema$owner == "machine"]
  expect_gt(length(want), 0L)
  # Read at run time, so adding a machine column to the schema needs no R edit.
  expect_setequal(.cli_sheet_machine_columns(), want)

  helpers <- paste(readLines(file.path(repo_root(), "scripts", "R_cli",
                                       "cli_helpers.r"), warn = FALSE),
                   collapse = "\n")
  expect_match(helpers, "sheet_columns.tsv", fixed = TRUE)
  # ...and no hardcoded copy of the list alongside it.
  expect_false(grepl('"size_x", "size_y"', helpers, fixed = TRUE))
})

test_that("pixel_depth is documented as the z_step default, not as unread", {
  doc <- readLines(file.path(repo_root(), "note", "data_formats.md"), warn = FALSE)
  row <- grep("^\\|\\s*`pixel_depth`\\s*\\|", doc, value = TRUE)
  expect_length(row, 1L)
  expect_match(row, "defaults `--z_step` to it", fixed = TRUE)
  # The claim this replaced must be gone, not merely joined by the new one.
  expect_false(any(grepl("nothing on the R side reads it yet", doc, fixed = TRUE)))

  cli <- paste(readLines(file.path(repo_root(), "scripts", "R_cli",
                                   "feature_stat_cli.r"), warn = FALSE),
               collapse = "\n")
  expect_match(cli, "pixel_depth", fixed = TRUE)
})

# --- the R CLI outputs --------------------------------------------------------

test_that("annotate writes the documented columns", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")

  out <- withr::local_tempdir()
  tsv <- suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "nucleolus",
    "--outdir", out,
    "--max_z_dist", "default=3", "nucleolus=1",
    "--min_z_span", "default=5", "nucleolus=2",
    "--within", "nucleolus=nucleus")))

  documented <- c("roi", "z", "area", "is_bridge", "feature_id", "feature_type",
                  "series_id", "run_id", "parent_feature_id", "parent_feature_type",
                  "parent_containment", "parent_match")
  expect_identical(colnames(tsv), documented)

  rds <- readRDS(file.path(out, "GRV_Position010_features.rds"))
  expect_setequal(colnames(rds), c(documented, "geometry"))
  expect_s3_class(rds, "sf")

  # parent_match takes only the documented values.
  expect_true(all(is.na(tsv$parent_match) |
                    tsv$parent_match %in% c("direct", "gap_filled")))
})

test_that("feature_id uses the documented vocabulary", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")

  out <- withr::local_tempdir()
  tsv <- suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "nucleolus",
    "--outdir", out,
    "--max_z_dist", "default=3", "nucleolus=1",
    "--min_z_span", "default=5", "nucleolus=2")))

  ids <- unique(tsv$feature_id[!is.na(tsv$feature_id)])
  ok <- grepl("^(nucleus|nucleolus)_\\d+$", ids) |
    grepl("^invalid_(nucleus|nucleolus)_\\d+$", ids) |
    grepl("^failed_(nucleus|nucleolus)_(excluded|name|area|overlap)$", ids)
  expect_true(all(ok), info = paste(ids[!ok], collapse = ", "))
})

test_that("count writes the documented columns", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("count_features_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "--outdir", feat)))

  out <- withr::local_tempdir()
  counts <- suppressMessages(count_features_cli(c("--input", feat, "--outdir", out)))
  expect_identical(colnames(counts),
                   c("series_id", "feature_class", "feature_type", "n_detected",
                     "n_invalid", "n_failed", "n_roi"))
  # With the default --feature_class_by the composite IS the feature type, so
  # the extra column is a rename of nothing rather than a change of meaning.
  expect_identical(counts$feature_class, counts$feature_type)

  d <- withr::local_tempdir()
  sheet <- file.path(d, "s.tsv")
  write.table(data.frame(series_id = "GRV_Position010", genotype = "wt"),
              sheet, sep = "\t", quote = FALSE, row.names = FALSE)
  out2 <- withr::local_tempdir()
  suppressMessages(count_features_cli(c(
    "--input", feat, "--outdir", out2,
    "--series_sheet", sheet, "--group_by", "genotype")))
  s <- read.delim(file.path(out2, "feature_counts_summary.tsv"),
                  stringsAsFactors = FALSE)
  expect_identical(colnames(s),
                   c("genotype", "feature_type", "n_series", "mean_detected",
                     "sd_detected", "total_detected"))
})

test_that("feature_stat writes the documented columns", {
  skip_if_no_sf()
  skip_if_no_pkg(c("argparser", "ggplot2"))
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  source_cli("annotate_features_cli.r")
  source_cli("feature_stat_cli.r")

  feat <- withr::local_tempdir()
  suppressMessages(annotate_features_cli(c(
    "--input", fixture_dir(), "--feature", "nucleus", "nucleolus",
    "--outdir", feat, "--min_z_span", "default=5", "nucleolus=2",
    "--max_z_dist", "default=3", "nucleolus=1")))

  out <- withr::local_tempdir()
  st <- suppressMessages(feature_stat_cli(c(
    "--input", feat, "--outdir", out, "--res_dir", fixture_dir(), "--no_plot")))

  documented <- c("series_id", "feature_type", "feature_id", "n_roi", "n_z",
                  "z_min", "z_max", "z_span", "area_med", "area_mean",
                  "area_max", "area_sum", "z_gaps", "circ_med", "circ_min")
  expect_true(all(documented %in% colnames(st)),
              info = paste(setdiff(documented, colnames(st)), collapse = ", "))

  # One signal column per channel Fiji measured, named ch<N>_signal.
  sig <- grep("^ch\\d+_signal$", colnames(st), value = TRUE)
  expect_gt(length(sig), 0)

  # z_gaps is defined as z_span - n_z; assert the arithmetic, not just the name.
  expect_equal(st$z_gaps, st$z_span - st$n_z)
  # And z_span as max - min + 1.
  expect_equal(st$z_span, st$z_max - st$z_min + 1L)

  rej <- read.delim(file.path(out, "feature_rejects.tsv"), stringsAsFactors = FALSE)
  expect_identical(colnames(rej), c("series_id", "feature_type", "bucket", "n_roi"))
  expect_true(all(rej$bucket %in% c("feature", "invalid", "failed", "unassigned")))
})

test_that("the documented default aggregation really is area-weighted", {
  source_r_scripts("feature_stats.r")
  # §3 claims --channel_stat defaults to wmean. A tapering object is where that
  # differs from a plain mean, so the claim is testable rather than decorative.
  v <- c(0, 100, 0); w <- c(1, 98, 1)
  expect_equal(aggregate_roi_stat(v, w, "wmean"), 98)
  expect_false(isTRUE(all.equal(aggregate_roi_stat(v, w, "wmean"),
                                aggregate_roi_stat(v, w, "mean"))))
})

# --- whitespace in the image id ------------------------------------------------

test_that(".read_outline survives a name column containing spaces", {
  skip_if_no_pkg("argparser")
  source_cli("annotate_features_cli.r")

  # The `name` column holds the image id, and a Leica series called
  # "Image005 Denoised" put a space in every row. read.table() defaults to
  # splitting on ANY whitespace, so the row became six fields against a
  # five-column header: R silently took the first as row names and every
  # column shifted. Explicit sep = "\t" is what stops that.
  d <- withr::local_tempdir()
  p <- file.path(d, "S1_nucleus_outline.txt")
  writeLines(c(
    "name\troi\tz\tx\ty",
    "Image005 Denoised\tnucleus_0001-0001-0433\t1\t1.5\t2.5",
    "Image005 Denoised\tnucleus_0001-0001-0433\t1\t3.5\t4.5",
    "Image005 Denoised\tnucleus_0001-0001-0433\t1\t5.5\t0.5"), p)

  got <- .read_outline(p)
  expect_identical(colnames(got), c("roi", "z", "x", "y"))
  expect_identical(nrow(got), 3L)
  expect_identical(unique(got$roi), "nucleus_0001-0001-0433")
  expect_equal(got$x, c(1.5, 3.5, 5.5))
  expect_equal(got$z, c(1L, 1L, 1L))
})

test_that("the documented identity columns are the ones the code reads", {
  # note/data_formats.md §2 claims the series id comes from `name` and the
  # feature from the `roi` prefix. Assert that against the fixture and the real
  # reader, so the claim cannot rot.
  skip_if_no_fixture(fixture_file("nucleus", "outline"))
  o <- read.table(fixture_file("nucleus", "outline"), header = TRUE,
                  sep = "\t", stringsAsFactors = FALSE)
  expect_identical(unique(o$name), "GRV_Position010")
  expect_identical(.cli_feature_from_roi(o$roi), "nucleus")

  id <- .cli_identify_outline(fixture_file("nucleus", "outline"))
  expect_true(id$ok)
  expect_identical(id$series_id, "GRV_Position010")
  # And `name` IS the file stem. The R side finds <series_id>_config.txt and
  # <series_id>_<feature>_res.txt from the name it read here, so a prefix in
  # the file names but not in `name` would lose them -- which is why v0.7.0
  # removed output_prefix rather than moving it out of `name` alone.
  expect_identical(unique(o$name),
                   sub("_nucleus_outline\\.txt$", "", basename(fixture_file("nucleus", "outline"))))
  expect_identical(id$feature, "nucleus")
})

test_that("the documented range syntax is what the parser accepts", {
  # §4 documents key=lo:hi with either end omittable.
  expect_equal(.cli_key_ranges("nucleus=80:Inf", "--roi_area")$nucleus, c(80, Inf))
  expect_equal(.cli_key_ranges("nucleus=80:",    "--roi_area")$nucleus, c(80, Inf))
  expect_equal(.cli_key_ranges("nucleus=:100",   "--roi_area")$nucleus, c(0, 100))
  # §4 also says a comma is reserved, so it must not work as a separator here.
  expect_error(.cli_key_ranges("nucleus=80,100", "--roi_area"), "lo:hi")
})

test_that("Groovy no longer emits an image id containing whitespace", {
  # sanitize() collapses whitespace, so the case above should not arise from
  # our own writer any more. Belt and braces: the reader is explicit anyway.
  gv <- file.path(repo_root(), "scripts", "groovy", "RoiExport.groovy")
  skip_if(!file.exists(gv), "RoiExport.groovy not present")
  src <- paste(readLines(gv), collapse = "\n")
  expect_match(src, "replaceAll\\(/\\\\s\\+/, \"_\"\\)",
               info = "sanitize() must collapse whitespace to underscore")
})
