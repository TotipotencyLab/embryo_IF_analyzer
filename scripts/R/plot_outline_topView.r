

colorName_2_hex <- function(color_name_vec, alpha=NULL){
  color_rgb_mat <- col2rgb(color_name_vec)
  color_hex <- apply(color_rgb_mat, MARGIN=2, function(v){
    o <- paste0(str_pad(as.hexmode(v), width=2, side="left", pad="0"), 
                collapse="")
    return(o)
  }) %>% 
    paste0("#", .) %>% toupper
  
  # Calculate alpha (transparentcy)
  if(!is.null(alpha)){
    alpha_hex <- as.hexmode(ceiling(alpha*255))
  }else{
    alpha_hex <- ""
  }
  
  out_hex <- toupper(paste0(color_hex, alpha_hex))
  return(out_hex)
}

flip_y_image <- function(st_df, y_ref=NULL){
  # Put an sf object into image orientation (y increasing DOWNWARD).
  #
  # NB: neither scale_y_reverse() nor coord_sf(ylim=rev(...)) does this --
  #     coord_sf silently ignores both, and the plot comes out mirrored while
  #     looking entirely reasonable. Verified on ggplot2 4.0.3. The only
  #     reliable way is to flip the geometry itself, which is what this does;
  #     pair it with flip_y_labels() so the axis still reads true coordinates.
  #
  # y_ref = ymin+ymax maps the bounding box onto itself. Pass an explicit
  # y_ref (e.g. the image height) when several plots must share an extent.
  if(is.null(y_ref)){
    bb <- sf::st_bbox(st_df)
    y_ref <- unname(bb["ymin"] + bb["ymax"])
  }
  sf::st_geometry(st_df) <- (sf::st_geometry(st_df) * matrix(c(1, 0, 0, -1), 2, 2)) + c(0, y_ref)
  attr(st_df, "y_ref") <- y_ref
  return(st_df)
}

flip_y_labels <- function(y_ref){
  # Axis labeller undoing flip_y_image() for display, so the tick labels show
  # the original image coordinates.
  function(breaks){ format(y_ref - breaks, trim=TRUE) }
}

flip_y_breaks <- function(y_ref){
  # Break positions that land on round numbers in the ORIGINAL coordinates.
  # Without this the breaks are pretty in flipped space and the labels come out
  # as 52.178, 62.178, ... -- correct, but unreadable.
  function(limits){
    orig <- sort(y_ref - range(limits, na.rm=TRUE))
    y_ref - pretty(orig)
  }
}

plot_features_topView <- function(st_df, union_df=NULL, color_by="feature_type",
                                  y_ref=NULL, line_width=0.7, background_width=0.2,
                                  xlim=NULL, ylim=NULL, bare=FALSE, palette=NULL){
  # Top view of annotated features, drawn with geom_sf.
  #
  # Use this, not plot_outline_topView(), for anything that has been unioned:
  # plot_outline_topView() flattens geometry to x/y columns for geom_polygon,
  # which cannot represent a hole or a MULTIPOLYGON and draws both wrongly
  # without raising anything.
  #
  #   st_df    per-ROI features (drawn faintly underneath); may be NULL
  #   union_df one row per feature (drawn on top); may be NULL
  if(is.null(st_df) && is.null(union_df)){
    stop("Nothing to plot: both st_df and union_df are NULL")
  }

  if(is.null(y_ref)){
    bb <- sf::st_bbox(if(is.null(st_df)) union_df else st_df)
    y_ref <- unname(bb["ymin"] + bb["ymax"])
  }

  p <- ggplot()
  if(!is.null(st_df)){
    p <- p + geom_sf(data=flip_y_image(st_df, y_ref), fill=NA,
                     colour="grey80", linewidth=background_width)
  }
  if(!is.null(union_df)){
    p <- p + geom_sf(data=flip_y_image(union_df, y_ref),
                     mapping=aes(colour=.data[[color_by]]), fill=NA, linewidth=line_width)
  }

  # A complete map, never a partial one -- see class_palette().
  if(!is.null(palette)){
    p <- p + scale_colour_manual(values=palette, drop=FALSE)
  }

  # NB: one coord_sf only. Adding a second later replaces this one and ggplot
  #     says so on stderr ("Coordinate system already present"), which is easy to
  #     scroll past -- so take the limits here rather than bolting on another.
  p <- p + coord_sf(xlim=xlim, ylim=ylim, expand=is.null(xlim) && is.null(ylim))

  if(bare){
    # Picture mode: the drawn area IS the image frame, so the panel can sit
    # beside a Fiji PNG of the same extent and line up. Axes and legend would
    # each steal space and break that.
    p <- p +
      theme_void() +
      theme(legend.position="none",
            plot.margin=margin(0, 0, 0, 0),
            panel.background=element_rect(fill="white", colour=NA))
  }else{
    p <- p +
      scale_y_continuous(breaks=flip_y_breaks(y_ref), labels=flip_y_labels(y_ref)) +
      theme_minimal() +
      labs(x="x (um)", y="y (um)", colour=NULL)
  }

  return(p)
}

union_features <- function(st_df, group_cols=c("feature_id", "feature_type")){
  # Collapse the per-slice ROIs of each feature into one outline.
  #
  # NB: st_as_sf() first. polygonize_roi_df() and define_feature_group() return
  #     a plain tibble carrying an sfc column, NOT an sf object, and
  #     summarise() on the unregistered tibble drops the geometry silently
  #     instead of unioning it -- no error, no geometry.
  if(!inherits(st_df, "sf")) st_df <- sf::st_as_sf(st_df)
  group_cols <- base::intersect(group_cols, colnames(st_df))
  if(length(group_cols) == 0){stop("None of the grouping columns are present")}

  out <- st_df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(n_roi = dplyr::n(),
              z_min = min(z, na.rm=TRUE),
              z_max = max(z, na.rm=TRUE),
              .groups = "drop")
  return(out)
}

plot_outline_topView <- function(feature_df, color_by=NULL, color_map=NULL, line_alpha=0.5, line_width=1){
  # Retrieve x,y coordinate ----------------------------------------------------------------------
  feature_coord_df <- pg_2_coord_df(feature_df)
  
  # Define asthetic mappin -----------------------------------------------------------------------
  aes_mapping <- aes(x=x, y=y)
  ## Add mapping asthetic based on user input:
  add_modify_aes <- function(mapping, ...) {
    ggplot2:::rename_aes(modifyList(mapping, ...))  
  }
  
  use_default_color=TRUE
  if(!is.null(color_by)){
    if(color_by %in% colnames(feature_df)){
      aes_mapping <- add_modify_aes(aes_mapping, aes(color=.data[[color_by]]))
      use_default_color=FALSE
    }else{
      warning("couldn't find color mapping column")
    }
  }
  
  ## Make a plot ----------------------------------------------------------------------------------
  p <- feature_coord_df %>% 
    ggplot(mapping=aes_mapping)
  
  if(use_default_color){
    p <- p + geom_polygon(aes(group=roi), fill=NA, linewidth=line_width, color=colorName_2_hex("gray75", alpha=line_alpha))
  }else{
    p <- p + geom_polygon(aes(group=roi), fill=NA, linewidth=line_width)
  }
  
  p <- p +
    theme_minimal() +
    coord_fixed(ratio=1) +
    scale_y_reverse() +
    theme(panel.background=element_rect(color="gray25"),
          legend.position="bottom")
  
  
  if(!is.null(color_map)){
    # Geom_polygon is not affected by the typical alpha setting, we need in include that into the color asthetic mapping
    color_hex_idx <- str_detect(color_map, "^#")
    
    color_hex_vec <- color_map[color_hex_idx]
    color_name_vec <- color_map[!color_hex_idx]
    
    if(length(color_hex_vec)>0){
      # Make sure there is only the RGB part of the hex code
      color_hex_vec <- str_sub(color_hex_vec, 1, 7)
      
      alpha_hex <- as.hexmode(ceiling(line_alpha*255))
      color_hex_vec <- toupper(paste0(color_hex_vec, alpha_hex))
      
      # Adjust the main color vec
      color_map[color_hex_idx] <- color_hex_vec
    }
    
    if(length(color_name_vec)>0){
      color_name_vec <- colorName_2_hex(color_name_vec, alpha=line_alpha)
      color_map[!color_hex_idx] <- color_name_vec
    }
    
    # Adjust the scale color
    p <- p + scale_color_manual(values=color_map)
  }
  
  return(p)
}

#' Build a complete colour map over the classes actually present
#'
#' ⚠️ Never hand ggplot a partial `values=`. Measured on ggplot2 4.0.3: a level
#' absent from `values` is drawn in `na.value` grey -- indistinguishable from a
#' genuine NA -- AND is dropped from the legend entirely. The figure then
#' silently denies that the class exists. See .claude/skills/r-ggplot.
#'
#' So the classes that were not named are RECODED into one `other` level rather
#' than left to fall through, and `other` is put FIRST so it is drawn
#' underneath the classes being inspected.
#'
#' @param values  character vector of class labels present in the data
#' @param map     named character vector, class -> colour; NULL for automatic
#' @param other   label for everything unmapped
#' @param grey    colour for `other`
#' @return list(values = recoded factor, palette = complete named vector,
#'         other_members = the labels folded into `other`)
class_palette <- function(values, map = NULL, other = "other", grey = "grey75"){
  values <- as.character(values)
  values[is.na(values)] <- "(unclassified)"
  present <- sort(unique(values))

  if(is.null(map) || !length(map)){
    # Automatic: every class keeps its own level and ggplot picks the colours.
    return(list(values = factor(values, levels = present),
                palette = NULL, other_members = character(0)))
  }

  named <- names(map)[names(map) %in% present]
  members <- setdiff(present, named)
  lev <- c(if(length(members)) other, named)   # other FIRST: drawn underneath
  recoded <- ifelse(values %in% named, values, other)

  pal <- stats::setNames(rep(grey, length(lev)), lev)
  pal[named] <- unname(map[named])

  return(list(values = factor(recoded, levels = lev),
              palette = pal,
              other_members = members))
}
