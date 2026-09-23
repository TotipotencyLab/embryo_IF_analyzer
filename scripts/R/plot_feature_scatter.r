# plot_feature_scatter.r
#
# Two statistics against each other, one point per detected feature.
#
# The distribution plots in plot_feature_stats.r answer "where do the features
# sit on this axis"; these answer "do the axes agree". That is the question a
# class boundary actually turns on -- a cut on area alone may be hopeless while
# the same cut with circularity beside it separates cleanly.
#
# Guide lines are keyed to the COLUMN, not to the panel, so a threshold decided
# once is drawn everywhere that column appears, on whichever axis it landed on.
# See note/data_formats.md for the flag contract.


#' Should this column be drawn on a log axis by default?
#'
#' Areas span orders of magnitude here (nucleoli ~5 um^2, growing oocytes
#' ~1000), so a linear axis buries the small end. Counts, z extents and
#' circularity do not, and 0 is a legitimate value for several of them -- log
#' would silently drop those points.
#'
#' @param col column name
scatter_should_log <- function(col){
  return(grepl("^area_", col))
}


#' Resolve a three-state log setting
#' @param setting "auto", "on"/"yes"/"true", or "off"/"no"/"false"
#' @param col     column the axis shows
resolve_log <- function(setting, col){
  s <- tolower(trimws(as.character(setting)))
  if(s %in% c("on", "yes", "true", "t")){
    return(TRUE)
  }
  if(s %in% c("off", "no", "false", "f")){
    return(FALSE)
  }
  if(s != "auto"){
    stop("Log setting must be auto, on or off; got '", setting, "'", call. = FALSE)
  }
  return(scatter_should_log(col))
}


#' One scatter panel
#'
#' @param stats      per-feature table (feature_stats.tsv)
#' @param x,y        column names
#' @param id         page label
#' @param color_by   column mapped to point colour, or NULL
#' @param facet_by   column to facet on, or NULL for one pooled panel
#' @param thresholds named list column -> numeric vector of guide-line positions
#' @param log_x,log_y "auto", "on" or "off"
#' @param smooth     add a linear fit
#' @param corr       report Spearman rho in the subtitle
#' @param legend_max drop the colour legend past this many levels
#' @return ggplot, or NULL when there is nothing to draw
plot_feature_scatter <- function(stats, x, y, id = NULL, color_by = NULL,
                                 facet_by = NULL, thresholds = list(),
                                 log_x = "auto", log_y = "auto",
                                 smooth = FALSE, corr = FALSE,
                                 legend_max = 12){

  for(col in c(x, y)){
    if(!col %in% colnames(stats)){
      stop("No such column to plot: ", col, call. = FALSE)
    }
    if(!is.numeric(stats[[col]])){
      stop("Column '", col, "' is not numeric, so it cannot be a scatter axis.",
           "\n  For a categorical split use feature_stat_cli.r, which draws ",
           "distributions by group.", call. = FALSE)
    }
  }

  d <- stats[!is.na(stats[[x]]) & !is.na(stats[[y]]), , drop = FALSE]
  if(nrow(d) == 0){
    return(NULL)
  }
  d$.x <- d[[x]]
  d$.y <- d[[y]]

  use_log_x <- resolve_log(log_x, x)
  use_log_y <- resolve_log(log_y, y)
  # A log axis cannot show zero or negative values, and ggplot drops them
  # silently -- which looks like missing data rather than a scale choice.
  if(use_log_x && any(d$.x <= 0)){
    warning("Not logging x (", x, "): ", sum(d$.x <= 0),
            " value(s) are <= 0 and would be dropped", call. = FALSE)
    use_log_x <- FALSE
  }
  if(use_log_y && any(d$.y <= 0)){
    warning("Not logging y (", y, "): ", sum(d$.y <= 0),
            " value(s) are <= 0 and would be dropped", call. = FALSE)
    use_log_y <- FALSE
  }

  aes_pt <- if(!is.null(color_by) && color_by %in% colnames(d)){
    ggplot2::aes(x = .x, y = .y, colour = .data[[color_by]])
  }else{
    ggplot2::aes(x = .x, y = .y)
  }

  p <- ggplot2::ggplot(d, aes_pt)

  # Guide lines UNDER the points: a threshold is context, not data.
  tl <- .scatter_threshold_lines(thresholds, x, y, use_log_x, use_log_y)
  if(length(tl$v) > 0){
    p <- p + ggplot2::geom_vline(xintercept = tl$v, linetype = "dashed",
                                 colour = "firebrick", linewidth = 0.4)
  }
  if(length(tl$h) > 0){
    p <- p + ggplot2::geom_hline(yintercept = tl$h, linetype = "dashed",
                                 colour = "firebrick", linewidth = 0.4)
  }

  p <- p + ggplot2::geom_point(size = 1.6, alpha = 0.85)

  if(smooth){
    # lm, not loess: per-sample n here is single digits, and a loess through
    # three points is noise with a confidence ribbon drawn around it.
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                                  colour = "steelblue", linewidth = 0.5,
                                  alpha = 0.15)
  }

  if(use_log_x){
    p <- p + ggplot2::scale_x_log10()
  }
  if(use_log_y){
    p <- p + ggplot2::scale_y_log10()
  }

  if(!is.null(facet_by) && facet_by %in% colnames(d)){
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~ ", facet_by)))
  }

  sub <- paste0(nrow(d), " feature(s)")
  if(corr){
    sub <- paste0(sub, "   ", .scatter_corr_label(d$.x, d$.y))
  }
  if(length(tl$v) > 0 || length(tl$h) > 0){
    bits <- c(if(length(tl$v) > 0){ paste0("x=", paste(tl$v, collapse = ",")) },
              if(length(tl$h) > 0){ paste0("y=", paste(tl$h, collapse = ",")) })
    sub <- paste0(sub, "   threshold ", paste(bits, collapse = " "))
  }

  # The colour legend earns its space only sometimes.
  #
  #   - On a faceted panel coloured by the facet column, the strip above each
  #     panel already says the name; the legend just repeats it.
  #   - Past a dozen or so levels the key is unreadable AND it eats the figure:
  #     ggplot shrinks the panel to fit the legend, so fifty samples leave a
  #     sliver of actual chart. The colours still carry grouping structure, so
  #     the mapping stays and only the key goes.
  #
  # Announced in the subtitle rather than silently, or a reader hunts for a
  # legend that was never going to be there.
  hide_legend <- FALSE
  if(!is.null(color_by) && color_by %in% colnames(d)){
    n_lev <- length(unique(d[[color_by]]))
    redundant <- !is.null(facet_by) && identical(color_by, facet_by)
    too_many <- n_lev > legend_max
    hide_legend <- redundant || too_many
    if(too_many && !redundant){
      sub <- paste0(sub, "   legend omitted (", n_lev, " ", color_by,
                    " levels > ", legend_max, ")")
    }
  }

  title <- paste0(y, " vs ", x)
  if(!is.null(id) && nzchar(id)){
    title <- paste0("[", id, "] ", title)
  }

  p <- p +
    ggplot2::labs(x = x, y = y, title = title, subtitle = sub) +
    ggplot2::theme_bw()

  # NB: after theme_bw(), not before. theme_bw() is a COMPLETE theme, so adding
  #     it replaces everything set by an earlier theme() call -- suppressing the
  #     legend first and styling second silently puts the legend back.
  if(hide_legend){
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}


# Which guide lines land on which axis for this panel.
#
# A threshold belongs to a variable, so it is drawn wherever that variable is,
# on whichever axis it happens to be. Values that a log scale cannot show are
# dropped here with a warning rather than vanishing inside ggplot.
.scatter_threshold_lines <- function(thresholds, x, y, log_x, log_y){
  pick <- function(col, is_log, axis){
    if(!col %in% names(thresholds)){
      return(numeric(0))
    }
    v <- suppressWarnings(as.numeric(thresholds[[col]]))
    if(any(is.na(v))){
      stop("Threshold for '", col, "' is not numeric: ",
           paste(thresholds[[col]][is.na(v)], collapse = ", "), call. = FALSE)
    }
    if(is_log && any(v <= 0)){
      warning("Threshold(s) <= 0 on the logged ", axis, " axis (", col,
              ") cannot be drawn: ", paste(v[v <= 0], collapse = ", "),
              call. = FALSE)
      v <- v[v > 0]
    }
    return(v)
  }
  return(list(v = pick(x, log_x, "x"), h = pick(y, log_y, "y")))
}


# Spearman, because these are small samples and often not linear. Reported only
# on request: with an n in single digits a coefficient invites more confidence
# than the data supports, and it tends to migrate into figure legends.
.scatter_corr_label <- function(x, y){
  ok <- is.finite(x) & is.finite(y)
  if(sum(ok) < 3){
    return("rho: n too small")
  }
  r <- suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
  if(is.na(r)){
    return("rho: undefined")
  }
  return(paste0("Spearman rho = ", format(round(r, 3), nsmall = 3),
                " (n=", sum(ok), ")"))
}


#' Every requested panel, ready for save_plot_list()
#'
#' The pooled page ALWAYS uses every row. Only the faceted page is subset by
#' `facet_keep` -- the point of the pair is to see the whole population once and
#' then a readable subset of it broken out, not to answer both questions from
#' the same reduced set.
#'
#' @param stats      per-feature table
#' @param specs      data.frame(id, x, y) from .cli_parse_plot_specs()
#' @param facet      "none", "both", or a column name
#' @param facet_keep values of the facet column to break out; NULL = all
#' @param facet_max  skip the faceted page past this many levels
#' @param ...        passed to plot_feature_scatter()
#' @return named list of ggplot
plot_feature_scatter_list <- function(stats, specs, facet = "both",
                                      facet_keep = NULL, facet_max = 16, ...){
  facet_col <- if(facet %in% c("none", "both")){ "sample" }else{ facet }
  want_pooled <- facet %in% c("none", "both")
  want_facet  <- facet != "none"

  faceted <- stats
  if(want_facet){
    if(!facet_col %in% colnames(stats)){
      stop("--facet names a column that is not present: ", facet_col,
           "\n  available: ", paste(colnames(stats), collapse = ", "), call. = FALSE)
    }
    if(length(facet_keep) > 0){
      have <- unique(stats[[facet_col]])
      absent <- setdiff(facet_keep, have)
      if(length(absent) > 0){
        warning("--facet_keep names ", length(absent), " value(s) not in ",
                facet_col, ": ", paste(utils::head(absent, 5), collapse = ", "),
                call. = FALSE)
      }
      faceted <- stats[stats[[facet_col]] %in% facet_keep, , drop = FALSE]
      if(nrow(faceted) == 0){
        stop("--facet_keep left no rows to facet.", call. = FALSE)
      }
    }
    n_lev <- length(unique(faceted[[facet_col]]))
    if(n_lev > facet_max){
      # A page of fifty panels is not a figure. Refusing it loudly beats
      # producing something nobody can read and calling it output.
      warning("Faceting by ", facet_col, " would give ", n_lev,
              " panels on one page, which is past --facet_max (", facet_max,
              "). Skipping the faceted page; the pooled one is unaffected.",
              "\n  Use --facet_keep to choose which to break out, or raise ",
              "--facet_max.", call. = FALSE)
      want_facet <- FALSE
    }
  }

  out <- list()
  for(i in seq_len(nrow(specs))){
    sp <- specs[i, ]
    if(want_pooled){
      p <- plot_feature_scatter(stats, sp$x, sp$y, id = sp$id, facet_by = NULL, ...)
      if(!is.null(p)){
        out[[paste0(sp$id, " ", sp$y, " vs ", sp$x)]] <- p
      }
    }
    if(want_facet){
      p <- plot_feature_scatter(faceted, sp$x, sp$y, id = sp$id,
                                facet_by = facet_col, ...)
      if(!is.null(p)){
        out[[paste0(sp$id, " ", sp$y, " vs ", sp$x, " by ", facet_col)]] <- p
      }
    }
  }
  return(out)
}


#' What can be put on an axis, and whether it actually holds anything
#'
#' Printed by --show_avail_stats. The non-NA count is the point: a channel that
#' was never measured shows as 0/N, which explains an empty panel before it is
#' drawn rather than after.
#'
#' @param stats per-feature table
#' @return data.frame(column, type, non_na, range)
describe_feature_stats <- function(stats){
  rows <- lapply(colnames(stats), function(col){
    v <- stats[[col]]
    n_ok <- sum(!is.na(v))
    if(is.numeric(v)){
      rng <- if(n_ok > 0){
        paste0(format(min(v, na.rm = TRUE), digits = 4), " - ",
               format(max(v, na.rm = TRUE), digits = 4))
      }else{
        "(all NA)"
      }
      type <- "numeric"
    }else{
      rng <- paste0(length(unique(v[!is.na(v)])), " distinct")
      type <- "chr"
    }
    data.frame(column = col, type = type,
               non_na = paste0(n_ok, "/", length(v)),
               range = rng, stringsAsFactors = FALSE)
  })
  return(do.call(rbind, rows))
}
