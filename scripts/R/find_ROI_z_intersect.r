find_ROI_z_intersect <- function(roi_pg_df, max_z_dist=1, min_intersect_ratio=0.0, verbose=FALSE){
  
  roi_pg_df$area <- st_area(roi_pg_df$geometry) # Assign area
  # Making a reference vector with ROI ID as a key for later filtering step
  roi_z_map <- dplyr::pull(roi_pg_df, z, name=roi)
  roi_area_map <- dplyr::pull(roi_pg_df, area, name=roi)
  
  
  if(verbose){
    n_roi_start <- dplyr::filter(roi_pg_df, !is.na(roi)) %>% 
      dplyr::pull(roi) %>% 
      length()
    cat("Number of ROI at the start:", n_roi_start, "\n")
  }
  
  # Find intersecting ROI across all z-stack ------------------------------------------------------
  roi_ovl_mat <- st_intersects(roi_pg_df$geometry, roi_pg_df$geometry, sparse=F)
  colnames(roi_ovl_mat) <- rownames(roi_ovl_mat) <- roi_pg_df$roi
  
  # Turning matrix into a data frame of ROI overlap (tidy format)
  ovl_pair_df <- as.data.frame(roi_ovl_mat) %>% 
    rownames_to_column(var="roi_1") %>% 
    tidyr::gather(key="roi_2", value="overlap", -roi_1) %>% 
    as_tibble()
  
  # Reduce the size of the data frame
  ovl_pair_df <- ovl_pair_df %>% 
    dplyr::filter(overlap) %>% 
    rowwise() %>%
    # ROI_1-ROI_2 and ROI_2-ROI_1 is the redundant information -> Filter out
    mutate(unq_ovl_id = paste0(sort(c(roi_1, roi_2)), collapse="::")) %>% 
    ungroup() %>% 
    dplyr::filter(!duplicated(unq_ovl_id))
  
  if(verbose){
    cur_n_roi <- unique(c(ovl_pair_df$roi_1, ovl_pair_df$roi_2)) %>% 
      subset(., !is.na(.)) %>% length()
    cat("Current ROI:", cur_n_roi, "\n")
  }
  
  # Checkpoint 0
  if(nrow(ovl_pair_df)==0){
    warning("No ROI feature overlap across z-stack detected")
  }
  
  # FILTER 1) by max z-stack range ----------------------------------------------------------------
  do_z_dist_filter <- !is.null(max_z_dist) & (max_z_dist>0) & (max_z_dist<Inf)
  if(do_z_dist_filter){
    # Assigning z-stack number for each ROI
    ovl_pair_df <- ovl_pair_df %>% 
      mutate(z1 = roi_z_map[roi_1], 
             z2 = roi_z_map[roi_2],
             z_dist = abs(z1-z2))
    
    # Find invalid overlap from:
    #   a) self overlap 
    #   b) overlap with ROI thar are to far away (on z-stack dimension)
    ovl_pair_df <- dplyr::filter(ovl_pair_df, !((z_dist > max_z_dist) | (z_dist == 0)))
    # # DEPRECIATED: Keep. this chunk for historical reason
    # invalid_z_dist_flag <- (ovl_pair_df$z_dist > max_z_dist) | (ovl_pair_df$z_dist == 0)
    # if(sum(invalid_z_dist_flag)>0){
    #   ovl_pair_df$overlap[invalid_z_dist_flag] <- FALSE
    # }
    # ovl_pair_df <- dplyr::filter(ovl_pair_df, overlap)
    
    if(verbose){
      cur_n_roi <- unique(c(ovl_pair_df$roi_1, ovl_pair_df$roi_2)) %>% 
        subset(., !is.na(.)) %>% length()
      cat("Current ROI (after z-stack dist filter):", cur_n_roi, "\n")
    }
    
    # Checkpoint 1
    if(nrow(ovl_pair_df)==0){
      warning("No ROI feature overlap across z-stack detected (after z-stack distance filter)")
    }
    
  } # End of do_z_dist_filter
  
  # FILTER 2) by ratio of overlap -----------------------------------------------------------------
  do_int_area_filter <- !is.null(min_intersect_ratio) & (min_intersect_ratio >= 0) & (min_intersect_ratio <= 1)
  if(do_int_area_filter){
    # Calculating ratio of overlap (overlap area/min area)
    ovl_pair_df <- ovl_pair_df %>% 
      mutate(area_1 = roi_area_map[roi_1],
             area_2 = roi_area_map[roi_2],
             int_area = NA) # int: intersect; NA as a placeholder for now
    
    # TODO: This can take quite some time; consider vectorizing it
    for(i in 1:nrow(ovl_pair_df)){
      pg1 <- roi_pg_df$geometry[roi_pg_df$roi == ovl_pair_df$roi_1[i]]
      pg2 <- roi_pg_df$geometry[roi_pg_df$roi == ovl_pair_df$roi_2[i]]
      # plot(c(pg1, pg2)) # For visual inspection
      pg_int <- st_intersection(pg1, pg2)
      if(length(pg_int) == 0){
        ovl_area <- 0
      }else{
        ovl_area <- st_area(pg_int)
      }
      ovl_pair_df$int_area[i] <- ovl_area
    }
    
    # Calculate intersect ratio
    ovl_pair_df <- ovl_pair_df %>% 
      mutate(min_area = pmin(area_1, area_2),
             int_ratio = int_area/min_area)
    # filter only the overlap with ROI in nearby z-stack
    ovl_pair_df <- dplyr::filter(ovl_pair_df, int_ratio >= min_intersect_ratio)
    # # DEPRECIATED: Keep this chunk for historical reason
    # invalid_int_ratio_flag <- (ovl_pair_df$int_ratio < min_intersect_ratio) 
    # if(sum(invalid_int_ratio_flag)>0){
    #   ovl_pair_df$overlap[invalid_int_ratio_flag] <- FALSE
    # }
    # ovl_pair_df <- dplyr::filter(ovl_pair_df, overlap)
    
    if(verbose){
      cur_n_roi <- unique(c(ovl_pair_df$roi_1, ovl_pair_df$roi_2)) %>% 
        subset(., !is.na(.)) %>% length()
      cat("Current ROI (after area intersect filter):", cur_n_roi, "\n")
    }
    
    # Checkpoint 2
    if(nrow(ovl_pair_df)==0){
      warning("No ROI feature overlap across z-stack detected (after area of overlap ratio filter)")
    }
  } # end of do_int_area_filter
  
  return(ovl_pair_df)
}
