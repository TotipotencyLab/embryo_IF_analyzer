# plot_feature_stats.r
#
# Distribution plots of per-feature statistics, one panel per statistic, grouped
# by whatever the caller nominates (input file, genotype, ...).
#
# The point is threshold-finding: you cannot choose a --class boundary or a
# signal cut-off without first seeing where the features actually sit, and
# whether the same cut works across files. So the default is to show every
# individual feature as a point, with a summary behind it -- never a summary
# alone, which hides the n and the shape.


#' Write a list of plots to one multi-page PDF
#'
#' @param plot_list list of ggplot (or anything print() renders)
#' @param file      output path
#' @param width,height inches, per page
#' @param ... passed to pdf()
#' @return file, invisibly
save_plot_list <- function(plot_list, file, width = 8, height = 6, ...){
  if(length(plot_list) == 0){
    warning("No plots to write to ", file, call. = FALSE)
    return(invisible(NULL))
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  grDevices::pdf(file = file, onefile = TRUE, width = width, height = height, ...)
  # on.exit, not a plain dev.off() at the end: a plot that fails to render would
  # otherwise leave the device open, and every later plot in the session would
  # be written silently into this half-finished file.
  on.exit(grDevices::dev.off(), add = TRUE)

  failed <- character(0)
  for(i in seq_along(plot_list)){
    nm <- names(plot_list)[i]
    if(is.null(nm) || !nzchar(nm)){
      nm <- paste0("plot ", i)
    }
    ok <- tryCatch({
      print(plot_list[[i]])
      TRUE
    }, error = function(e){
      # Keep going: one bad panel must not cost the other twenty.
      warning("Could not render '", nm, "': ", conditionMessage(e), call. = FALSE)
      FALSE
    })
    if(!ok){
      failed <- c(failed, nm)
    }
  }
  if(length(failed) > 0){
    message("  ", length(failed), " of ", length(plot_list),
            " page(s) failed to render: ", paste(failed, collapse = ", "))
  }
  return(invisible(file))
}


#' One distribution panel for one statistic
#'
#' @param stats     per-feature table from summarise_feature_stats()
#' @param value_col column to plot on y
#' @param group_col column to put on x
#' @param types     any of "box", "violin", "quasirandom", drawn in that order
#' @param colour_by optional column mapped to point colour
#' @param log_y     log10 the y axis (areas span orders of magnitude)
plot_feature_stat <- function(stats, value_col, group_col = "sample",
                              types = c("box", "quasirandom"),
                              colour_by = NULL, log_y = FALSE){

  known <- c("box", "violin", "quasirandom")
  bad <- setdiff(types, known)
  if(length(bad) > 0){
    stop("Unknown plot type(s): ", paste(bad, collapse = ", "),
         "; use one or more of ", paste(known, collapse = ", "), call. = FALSE)
  }
  if(!value_col %in% colnames(stats)){
    stop("No such column to plot: ", value_col, call. = FALSE)
  }
  if(!group_col %in% colnames(stats)){
    stop("No such grouping column: ", group_col, call. = FALSE)
  }

  d <- stats[!is.na(stats[[value_col]]), , drop = FALSE]
  if(nrow(d) == 0){
    return(NULL)
  }
  d$.x <- factor(d[[group_col]])
  d$.y <- d[[value_col]]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y))

  # Summary layers first so the individual features sit on top of them.
  if("violin" %in% types){
    p <- p + ggplot2::geom_violin(fill = "grey92", colour = "grey60",
                                  scale = "width", width = 0.85)
  }
  if("box" %in% types){
    p <- p + ggplot2::geom_boxplot(fill = NA, colour = "grey35", width = 0.45,
                                   outlier.shape = NA)
  }
  if("quasirandom" %in% types){
    pt <- if(!is.null(colour_by) && colour_by %in% colnames(d)){
      ggplot2::aes(colour = .data[[colour_by]])
    }else{
      ggplot2::aes()
    }
    if(requireNamespace("ggbeeswarm", quietly = TRUE)){
      p <- p + ggbeeswarm::geom_quasirandom(mapping = pt, width = 0.25,
                                            size = 1.4, alpha = 0.8)
    }else{
      # Jitter is the honest fallback -- same information, less even spacing.
      p <- p + ggplot2::geom_jitter(mapping = pt, width = 0.2, height = 0,
                                    size = 1.4, alpha = 0.8)
    }
  }

  # n per group, so a group of three is never mistaken for a distribution.
  # Positioned at each group's OWN maximum, not the panel's: with groups an
  # order of magnitude apart (nucleoli against nuclei) a shared height leaves
  # the label floating in empty space, nowhere near the data it counts.
  n_lab <- stats::aggregate(list(.y = d$.y), by = list(.x = d$.x), FUN = max,
                            na.rm = TRUE)
  n_lab$n <- as.integer(table(d$.x)[as.character(n_lab$.x)])
  p <- p + ggplot2::geom_text(data = n_lab,
                              ggplot2::aes(x = .x, y = .y, label = paste0("n=", n)),
                              vjust = -0.8, size = 3, colour = "grey30",
                              inherit.aes = FALSE)

  # Headroom for those labels. On a log axis a multiplicative pad is the one
  # that stays constant on screen; mult= does that in both cases.
  # A log axis cannot show zero, and ggplot drops those points silently, which
  # looks like missing data rather than a scale choice. Say so, as the scatter
  # panels do.
  used_log <- log_y
  if(log_y && !all(d$.y > 0, na.rm = TRUE)){
    warning("Not logging ", value_col, ": ", sum(d$.y <= 0, na.rm = TRUE),
            " value(s) are <= 0 and would be dropped", call. = FALSE)
    used_log <- FALSE
  }
  if(used_log){
    p <- p + ggplot2::scale_y_log10(expand = ggplot2::expansion(mult = c(0.05, 0.12)))
  }else{
    p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.12)))
  }

  p <- p +
    ggplot2::labs(x = group_col,
                  y = if(used_log){ paste0(value_col, " (log10)") }else{ value_col },
                  title = value_col,
                  subtitle = paste0(nrow(d), " feature(s) across ",
                                    nlevels(d$.x), " group(s)")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  return(p)
}


#' The statistics a distribution PDF draws, in the order it draws them
#'
#' Its own function so the CLI can advise on exactly the columns that will be
#' plotted. Suggesting a log axis for a column that never gets a panel -- z_min
#' is skipped here and spans 42x on real data -- is advice the reader cannot act
#' on.
#'
#' @param stats per-feature table
#' @return character vector of column names
feature_stat_value_cols <- function(stats){
  # Size, then extent, then shape, then signal. Ids and coordinates are not
  # statistics about the object.
  skip <- c("sample", "feature_id", "feature_type", "z_min", "z_max")
  cols <- names(stats)[vapply(stats, is.numeric, logical(1))]
  cols <- setdiff(cols, skip)
  preferred <- c("area_med", "area_mean", "area_max", "area_sum", "volume",
                 "n_roi", "n_z", "z_span", "z_gaps",
                 "circ_med", "circ_min")
  return(c(intersect(preferred, cols), setdiff(cols, preferred)))
}


#' One panel per statistic, ready for save_plot_list()
#'
#' @param stats     per-feature table
#' @param value_cols columns to plot; default: every numeric statistic present
#' @param group_col,types,colour_by passed to plot_feature_stat()
#' @param log_cols columns to draw on a log10 y axis, as names or globs; see
#'                 log_axis_matcher(). Nothing is logged unless named.
#' @return named list of ggplot
plot_feature_stat_list <- function(stats, value_cols = NULL, group_col = "sample",
                                   types = c("box", "quasirandom"),
                                   colour_by = NULL, log_cols = character(0)){
  if(is.null(value_cols)){
    value_cols <- feature_stat_value_cols(stats)
  }
  # Declared, not guessed -- the same contract as the scatter CLI. The list was
  # hardcoded to the area_ family, which silently left volume linear although
  # volume IS area_sum x z_step, so the same quantity was drawn two ways
  # depending on which panel you looked at.
  wants_log <- if(is.function(log_cols)){ log_cols }else{ log_axis_matcher(log_cols) }
  out <- lapply(value_cols, function(v){
    plot_feature_stat(stats, value_col = v, group_col = group_col, types = types,
                      colour_by = colour_by, log_y = wants_log(v))
  })
  names(out) <- value_cols
  out <- out[!vapply(out, is.null, logical(1))]
  return(out)
}
