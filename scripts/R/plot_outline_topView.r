

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