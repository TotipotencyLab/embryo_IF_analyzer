

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
                                  xlim=NULL, ylim=NULL, bare=FALSE, palette=NULL,
                                  label_by=NULL, label_size=2.6, legend=TRUE){
  # Top view of annotated features, drawn with geom_sf.
  #
  # Use this, not plot_outline_topView(), for anything that has been unioned:
  # plot_outline_topView() flattens geometry to x/y columns for geom_polygon,
  # which cannot represent a hole or a MULTIPOLYGON and draws both wrongly
  # without raising anything.
  #
  #   st_df    per-ROI features (drawn faintly underneath); may be NULL
  #   union_df one row per feature (drawn on top); may be NULL
  #   label_by a union_df column of text drawn on each feature, in its own
  #            outline colour; NA rows get none (qc_label_select())
  #   legend   FALSE drops the colour legend -- for colour-by-feature_id, where
  #            a hundred keys would be a legend bigger than the plot and the
  #            labels name the outlines instead
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
    if(!is.null(label_by)){
      lab <- union_df[!is.na(union_df[[label_by]]), , drop=FALSE]
      if(nrow(lab)){
        # At a point ON the outline's surface (geom_sf_text's default), not the
        # centroid: a crescent's centroid lies outside it, and a label beside
        # an object names its neighbour.
        # On a translucent white box: a light outline colour (Tableau's yellow,
        # a viridis end) is unreadable as bare text over the grey per-ROI
        # outlines, and the box keeps the label in the outline's colour.
        p <- p + geom_sf_label(data=flip_y_image(lab, y_ref),
                               mapping=aes(label=.data[[label_by]], colour=.data[[color_by]]),
                               size=label_size, fontface="bold", show.legend=FALSE,
                               fill=grDevices::adjustcolor("white", alpha.f=0.75),
                               border.colour=NA, label.padding=grid::unit(0.1, "lines"))
      }
    }
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
    if(!legend) p <- p + theme(legend.position="none")
  }

  return(p)
}

# --- colour and label by feature ------------------------------------------------

# A qualitative palette with its grey taken out: in colour-by-id mode grey means
# "a feature of another type", so a palette entry that is grey would make one
# feature look like it belonged to the background.
QC_DEFAULT_PALETTE <- "Tableau 10"
QC_OTHER_GREY <- "grey60"

#' The colours of a named palette, minus anything achromatic or near-white
#'
#' Two families, both in base R, so no package is needed for either:
#'   - grDevices::palette.pals(): qualitative sets (Okabe-Ito, Tableau 10,
#'     Polychrome 36, and RColorBrewer's Set 1-3, Dark 2, Paired, ...). A fixed
#'     list of distinct colours, CYCLED over the features.
#'   - grDevices::hcl.pals(): sequential and diverging ramps (Viridis, Plasma,
#'     RColorBrewer's Blues, YlOrRd, ...). n colours along the ramp.
#' A name in both (Dark 2, Set 2, ...) means the qualitative one. Matched
#' ignoring case, spaces and punctuation.
#'
#' @return list(colours, kind = "qualitative" | "ramp", name)
qc_palette_colours <- function(name = QC_DEFAULT_PALETTE, n = 1L){
  norm <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
  qual <- grDevices::palette.pals()
  ramp <- grDevices::hcl.pals()
  if(norm(name) %in% norm(qual)){
    nm <- qual[match(norm(name), norm(qual))]
    cols <- grDevices::palette.colors(palette = nm)
    kind <- "qualitative"
  }else if(norm(name) %in% norm(ramp)){
    nm <- ramp[match(norm(name), norm(ramp))]
    # A few more than needed, so dropping a near-white end still leaves n.
    cols <- grDevices::hcl.colors(max(2L, as.integer(n)) + 2L, palette = nm)
    kind <- "ramp"
  }else{
    stop("Unknown palette '", name, "'. Qualitative (cycled): ",
         paste(qual, collapse = ", "), ". Ramps: see grDevices::hcl.pals(), e.g. ",
         "Viridis, Plasma, Blues, YlOrRd.", call. = FALSE)
  }
  rgb <- grDevices::col2rgb(cols)
  chroma <- apply(rgb, 2, max) - apply(rgb, 2, min)
  usable <- chroma >= 40 & apply(rgb, 2, min) < 225
  cols <- unname(cols[usable])
  if(length(cols) < 2L){
    stop("Palette '", nm, "' has fewer than two colours that are neither grey nor ",
         "near-white; grey is reserved for features of other types.", call. = FALSE)
  }
  return(list(colours = substr(toupper(cols), 1, 7), kind = kind, name = nm))
}

#' One colour per feature of the focus type; every other type one grey
#'
#' Colours are assigned in feature_id order, so a feature keeps its colour on
#' every page of a time course as long as the set of ids is the same -- callers
#' pass every id in the series, not one frame's. A qualitative palette is cycled
#' (id i takes colour i mod k): consecutive ids differ, and ids k apart share a
#' colour, which the label settles. A ramp is strided -- ids i and i+1 sit far
#' apart along it -- so that neighbouring ids, which are often neighbouring
#' objects, do not get two nearly identical shades.
#'
#' The ids need not be feature ids: a track_id or branch_id per outline
#' colours by lineage or by branch the same way. An NA id (a feature no track
#' reaches) is drawn grey with the other types.
#'
#' @param feature_id,feature_type vectors, one per outline
#' @param focus_type the type coloured by id
#' @param palette    a palette name, see qc_palette_colours()
#' @return list(values = factor key per outline (its feature_id, or "other"),
#'         palette = complete named colour vector over the keys)
qc_id_colours <- function(feature_id, feature_type, focus_type,
                          palette = QC_DEFAULT_PALETTE, other_colour = QC_OTHER_GREY){
  is_focus <- feature_type == focus_type & !is.na(feature_id)
  ids <- sort(unique(feature_id[is_focus]))
  pal <- qc_palette_colours(palette, n = length(ids))
  k <- length(pal$colours)
  n <- length(ids)
  idx <- if(!n){
    integer(0)
  }else if(pal$kind == "qualitative"){
    (seq_len(n) - 1L) %% k + 1L
  }else{
    # Stride through the ramp by the integer nearest k/phi that is coprime with
    # k, which visits every colour once before repeating.
    step <- max(1L, round(k / 1.618))
    while(step > 1L && .gcd(step, k) != 1L) step <- step - 1L
    ((seq_len(n) - 1L) * step) %% k + 1L
  }
  colour_of <- stats::setNames(pal$colours[idx], ids)
  others <- any(!is_focus)
  key <- ifelse(is_focus, feature_id, "other")
  lev <- c(if(others) "other", ids)
  values <- c(if(others) c(other = other_colour), colour_of)
  return(list(values = factor(key, levels = lev), palette = values))
}

.gcd <- function(a, b){ while(b) { t <- b; b <- a %% b; a <- t }; a }

#' The short label of an id: `nucleus_0007` -> `0007`, a track
#' `nucleus_track_0003` -> `0003`, a branch `nucleus_track_0003_b002` -> `0003b002`
qc_short_label <- function(feature_id){
  out <- sub("^.*_track_([0-9]+)_b([0-9]+)$", "\\1b\\2", feature_id)
  plain <- !is.na(feature_id) & out == feature_id
  out[plain] <- sub("^.*_", "", feature_id[plain])
  out
}

#' Which outlines get a label
#'
#' `spec` is "all", or feature ids written as the CLI user would: `7`, `0007`
#' or `nucleus_0007` all mean feature 7 of the focus type. Only the focus type
#' is labelled -- labelling every nucleolus inside every nucleus would cover
#' the picture it is meant to explain. Given track or branch ids, the numbers
#' are TRACK numbers: `3` labels every branch of track 3.
#'
#' @return a character vector, one per outline: the short label, or NA
qc_label_select <- function(feature_id, feature_type, focus_type, spec){
  lab <- ifelse(feature_type == focus_type, qc_short_label(feature_id), NA_character_)
  if(is.null(spec) || !length(spec)) return(rep(NA_character_, length(feature_id)))
  if(identical(tolower(spec), "all")) return(lab)
  # The number a spec is matched against: a branch label's track part.
  key <- sub("b[0-9]+$", "", lab)
  want <- vapply(spec, function(s){
    tail <- sub("^.*_", "", s)
    if(!grepl("^[0-9]+$", tail)){
      stop("--qc_label takes 'all' or feature numbers/ids (7, 0007, nucleus_0007); got '",
           s, "'", call. = FALSE)
    }
    sprintf("%04d", as.integer(tail))
  }, character(1))
  absent <- setdiff(want, key)
  if(length(absent)){
    warning("No ", focus_type, " ", if(any(grepl("_track_", feature_id))) "track" else "feature",
            " numbered ", paste(absent, collapse = ", "), " to label", call. = FALSE)
  }
  return(ifelse(key %in% want, lab, NA_character_))
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
class_palette <- function(values, map = NULL, other = CLASS_OTHER,
                          grey = "grey30"){
  values <- as.character(values)
  # Tables written before `class` held a real string still carry NA here.
  values[is.na(values)] <- CLASS_UNCLASSIFIED
  present <- sort(unique(values))

  if(is.null(map) || !length(map)){
    # Automatic: every class keeps its own level and ggplot picks the colours.
    return(list(values = factor(values, levels = present),
                palette = NULL, other_members = character(0)))
  }

  # "other" is a group this function invents, so it is never in `present` --
  # matching it against the data would drop the colour asked for. Taken from
  # the map directly instead, and likewise "unclassified", which a caller may
  # want to distinguish from the rest of the grey rather than fold in with it.
  other_col <- if(other %in% names(map)) unname(map[[other]]) else grey
  keep_unc <- CLASS_UNCLASSIFIED %in% names(map) && CLASS_UNCLASSIFIED %in% present

  named <- names(map)[names(map) %in% present]
  members <- setdiff(present, named)
  # other FIRST so it is drawn underneath what is being inspected; then
  # unclassified, which is background too; then the classes of interest.
  ordered <- c(if(keep_unc) CLASS_UNCLASSIFIED, setdiff(named, CLASS_UNCLASSIFIED))
  lev <- c(if(length(members)) other, ordered)
  recoded <- ifelse(values %in% named, values, other)

  pal <- stats::setNames(rep(other_col, length(lev)), lev)
  for(k in ordered){
    pal[[k]] <- unname(map[[k]])
  }

  return(list(values = factor(recoded, levels = lev),
              palette = pal,
              other_members = members))
}
