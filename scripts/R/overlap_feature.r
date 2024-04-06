find_overlap_roi_features <- function(feature_df_1, feature_df_2, 
                                      feature_1_regex="feature_", feature_2_regex="feature_",
                                      min_intersect_ratio=0.5, 
                                      # min_share_z_ratio = 1,
                                      min_ratio_roi_overlap=1){
  
  # min_share_z_ratio:
  #   share_z_ratio = length(share_z with overlap) / min(length(z_1), length(z_2))
  #   if 1, feature with smaller number of z-stack spaning must be fully overlap with the larger z-stack spanning feature.
  
  # min_ratio_roi_overlap: 
  #   ratio_roi_overlap = length(overlap_roi) / min(length(ROI_1), length(ROI_2))
  #   if 1, all ROI in the feature that has smaller number of ROI has to be part of the overlap
  
  
  # Detect feature 
  F1_df <- dplyr::filter(feature_df_1, str_detect(feature_id, feature_1_regex))
  F2_df <- dplyr::filter(feature_df_2, str_detect(feature_id, feature_2_regex))
  
  # F1_feature <- unique(F1_df$feature_id)
  # F2_feature <- unique(F2_df$feature_id)
  # roi_feature_map_1 <- dplyr::pull(F1_df, feature_id, name=roi)
  # roi_feature_map_2 <- dplyr::pull(F2_df, feature_id, name=roi)
  
  # # Area
  F1_df$area <- st_area(F1_df$geometry)
  F2_df$area <- st_area(F2_df$geometry)
  # roi_area_map_1 <- dplyr::pull(F1_df, area, name=roi)
  # roi_area_map_2 <- dplyr::pull(F2_df, area, name=roi)
  
  # # Comparing features
  # z_range_list_1 <- split(F1_df$z, F1_df$feature_id)
  # z_range_list_2 <- split(F2_df$z, F2_df$feature_id)
  # roi_z_map_1 <- dplyr::pull(F1_df, z, name=roi)
  # roi_z_map_2 <- dplyr::pull(F2_df, z, name=roi)
  # 
  # roi_list_1 <- split(F1_df$roi, F1_df$feature_id)
  # roi_list_2 <- split(F2_df$roi, F2_df$feature_id)
  
  # Find overlap of all ROIs ----------------------------------------------------------------------
  roi_ovl_mat <- st_intersects(F1_df$geometry, F2_df$geometry, sparse=F)
  rownames(roi_ovl_mat) <- F1_df$roi
  colnames(roi_ovl_mat) <- F2_df$roi
  
  roi_ovl_pair_df <- as.data.frame(roi_ovl_mat) %>% 
    rownames_to_column(var="roi_1") %>% 
    tidyr::gather(key="roi_2", value="overlap", -roi_1) %>% 
    as_tibble()
  
  roi_ovl_pair_df <- roi_ovl_pair_df %>% 
    left_join(., dplyr::select(F1_df, roi_1=roi, z_1=z, area_1=area, f_id_1=feature_id), by="roi_1") %>% 
    left_join(., dplyr::select(F2_df, roi_2=roi, z_2=z, area_2=area, f_id_2=feature_id), by="roi_2") %>% 
    # Rearrange (for debugging)
    dplyr::select(starts_with("roi"), overlap, starts_with("f_id"), starts_with("z"), starts_with("area"), everything()) %>% 
    mutate(min_area = pmin(area_1, area_2),
           int_area = 0,
           ovl_idx = paste0("idx", seq_along(roi_1)))
  
  # Only consider roi on the same z-stack
  roi_ovl_pair_df <- roi_ovl_pair_df %>% 
    mutate(overlap = (overlap & (z_1 == z_2)))
  
  # Calculate overlap area ------------------------------------------------------------------------
  # Only do this for those with known to be overlap
  roi_ovl_pair_df_fil <- dplyr::filter(roi_ovl_pair_df, overlap)
  for(i in 1:nrow(roi_ovl_pair_df_fil)){
    pg1 <- F1_df$geometry[F1_df$roi == roi_ovl_pair_df_fil$roi_1[i]]
    pg2 <- F2_df$geometry[F2_df$roi == roi_ovl_pair_df_fil$roi_2[i]]
    # plot(c(pg1, pg2)) # For visual inspection
    ovl_area <- st_area(st_intersection(pg1, pg2))
    
    roi_ovl_pair_df_fil$int_area[i] <- ovl_area
  }
  
  # Merge the data back
  roi_ovl_pair_df <- left_join(dplyr::select(roi_ovl_pair_df, -int_area), 
            dplyr::select(roi_ovl_pair_df_fil, ovl_idx, int_area), by="ovl_idx") %>% 
    replace_na(replace=list(int_area=0))
  
  # Calculate intersect ratio
  roi_ovl_pair_df <- roi_ovl_pair_df %>% 
    mutate(int_ratio = as.double(int_area/min_area),
           overlap = (overlap & (int_ratio >= min_intersect_ratio)))
  
  # # For checking
  # roi_ovl_pair_df %>% 
  #   dplyr::filter(int_ratio != 0) %>% 
  #   dplyr::pull(int_ratio) %>% 
  #   range()
  
  # Detecting overlap features --------------------------------------------------------------------
  ## BOOKMARK ----
  # Summarizing overlap by feature
  feature_pair_stats <- roi_ovl_pair_df %>% 
    group_by(f_id_1, f_id_2) %>% 
    reframe(
      # # Z-filtering
      # n_z_1 = length(unique(z_1)),
      # n_z_2 = length(unique(z_2)),
      # min_z = pmin(n_z_1, n_z_2),
      # n_z_1_ovl = length(unique(z_1[overlap])),
      # n_z_2_ovl = length(unique(z_2[overlap])),
      # ratio_z_ovl = pmin(n_z_1_ovl, n_z_2_ovl) / min_z,
      
      ## ROI ovl ratio filtering
      n_roi_1 = length(unique(roi_1)),
      n_roi_2 = length(unique(roi_2)),
      min_roi = pmin(n_roi_1, n_roi_2),
      n_roi_1_ovl = length(unique(roi_1[overlap])),
      n_roi_2_ovl = length(unique(roi_2[overlap])),
      ratio_roi_ovl = pmin(n_roi_1_ovl, n_roi_2_ovl) / min_roi
    )
  
  # Preparing output ------------------------------------------------------------------------------
  feature_ovl_df <- feature_pair_stats %>% 
    dplyr::filter(ratio_roi_ovl >= min_ratio_roi_overlap) %>% 
    dplyr::select(feature_id_1 = f_id_1,
                  feature_id_2 = f_id_2)
  
  
  return(feature_ovl_df)
}




## Test section ===================================================================================
if(F){
  # Load required libraries ----
  # Generic
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  
  # Spatial analysis
  library(sp)
  library(sf)
  
  # plot function
  library(ggplot2)
  library(RColorBrewer)
  library(plotly)
  
  # Getting data ----
  test_data_dir <- "/Volumes/pool-toti-imaging/Alina/LeicaStellaris/27032024_Nr5a2_timecourse_1hRT/SR/raw_measurements"
  pos_id <- "Position001"
  
  # nuc_outline_path <- list.files(test_data_dir, pattern=paste0(pos_id, ".+nucleus_outline\\.txt$"))
  # nuc_res_path <- list.files(test_data_dir, pattern=paste0(pos_id, ".+nucleus_res\\.txt$"))
  
  nuc_outline_path <- list.files(test_data_dir, pattern=paste0(pos_id, ".+nucleolus_outline\\.txt$"))
  nuc_res_path <- list.files(test_data_dir, pattern=paste0(pos_id, ".+nucleolus_res\\.txt$"))
  
  nuc_outline_path <- paste0(test_data_dir, "/", nuc_outline_path)
  nuc_res_path <- paste0(test_data_dir, "/", nuc_res_path)
  
  nuc_outline_df <- as_tibble(read.table(nuc_outline_path, header=TRUE, stringsAsFactors=FALSE))
  nuc_res_df <- read_fiji_result(nuc_res_path)
  
  # Assign key input ----
  roi_df <- nuc_outline_df %>% 
    dplyr::select(-name)
  unq_roi_id <- unique(roi_df$roi)
  
  valid_roi <- nuc_res_df %>% 
    # dplyr::filter(!((area>=200)&(circ<0.5))) %>% 
    # dplyr::filter(circ>0.7) %>%
    # dplyr::filter(circ>0.6) %>%
    dplyr::filter(circ>0.5) %>% # Test value for zygote
    dplyr::pull(roi) %>% unique()
  
  invalid_roi <- unique(subset(roi_df$roi, !(roi_df$roi %in% valid_roi)))
  length(valid_roi)
  length(invalid_roi)
  
  # roi_df <- dplyr::filter(roi_df, roi %in% valid_roi)
  roi_df <- roi_df %>% 
    mutate(include = roi %in% valid_roi)
  
  roi_pg_df <- polygonize_roi_df(roi_df)
  roi_pg_df$area <- st_area(roi_pg_df$geometry) # Assign area
  roi_pg_df$area %>% hist(100)
  
  ## Get feature ----
  feature_df_nuc <- define_feature_group(roi_df, pre_roi_filter_colname="include", roi_regex="^nucleus", 
                                         feature_prefix="nucleus_", invalid_feature_prefix="invalid_", max_z_dist=3, min_z_span=5)
  
  feature_df_nucl <- define_feature_group(roi_df, pre_roi_filter_colname="include", roi_regex="^nucleolus", 
                                          feature_prefix="nucleolus_", invalid_feature_prefix="invalid_", max_z_dist=1, min_z_span=2)
  
  # visualize the features ------------------------------------------------------------------------
  tmpFn_plot_top_feature <- function(feature_df, 
                                     feature_name_regex="^feature_", invalid_feature_name_regex="^invalid_feature_", failed_ROI_regex="^fail"){
    feature_names <- feature_df %>% 
      dplyr::filter(str_detect(feature_id, feature_name_regex)) %>% 
      dplyr::pull(feature_id) %>% unique() %>% sort()
    invalid_feature_names <- feature_df %>% 
      dplyr::filter(str_detect(feature_id, invalid_feature_name_regex)) %>% 
      dplyr::pull(feature_id) %>% unique() %>% sort()
    fail_feature_names <- feature_df %>% 
      dplyr::filter(str_detect(tolower(feature_id), failed_ROI_regex)) %>% 
      dplyr::pull(feature_id) %>% unique() %>% sort()
    
    n_feature <- length(feature_names)
    n_color <- length(unique(feature_df$feature_id))
    
    use_pallette <- "Paired"
    pallette_info <- RColorBrewer::brewer.pal.info %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "name") %>% 
      as_tibble() %>% 
      dplyr::filter(name == use_pallette)
    
    # Recycling the color if number of feature is too large  
    feature_col_map <- c() # place holder
    remain_feature_names <- feature_names
    while(length(remain_feature_names) > 0){
      cur_n_color <- min(length(remain_feature_names), pallette_info$maxcolors)
      cur_name <- remain_feature_names[1:cur_n_color]
      tmp_vec <- suppressWarnings(RColorBrewer::brewer.pal(n=cur_n_color, pallette_info$name[1]))
      if(length(tmp_vec) > cur_n_color){
        # brewer.pal will always return a minimum number of color
        tmp_vec <- tmp_vec[1:cur_n_color]
      }
      names(tmp_vec) <- cur_name
      
      # update parameter
      feature_col_map <- c(feature_col_map, tmp_vec)
      remain_feature_names <- remain_feature_names[-c(1:cur_n_color)]
    }
    
    col_map <- c(
      feature_col_map,
      setNames(rep("gray40", length(invalid_feature_names)), sort(invalid_feature_names)),
      setNames(rep("gray80", length(fail_feature_names)), sort(fail_feature_names))
    )
    
    p <- feature_df %>% 
      arrange(z) %>% 
      ggplot(aes(geometry=geometry, color = feature_id)) +
      geom_sf(fill=NA, alpha=0.5) +
      # coord_sf() +
      scale_color_manual(values=col_map) +
      scale_y_reverse() +
      theme_minimal()
    return(p)
  }
  
  # Visualization
  feature_df_1 <- feature_df_nuc %>% 
    dplyr::filter(str_detect(feature_id, "^nucleus"))
  tmpFn_plot_top_feature(feature_df_1, feature_name_regex="^nucleus")
  
  feature_df_2 <- feature_df_nucl %>% 
    dplyr::filter(str_detect(feature_id, "^nucleolus"))
  tmpFn_plot_top_feature(feature_df_2, feature_name_regex="^nucleolus")
  
  feature_1_regex="nucleus_"
  feature_2_regex="nucleolus_"
  
  
  pull_feature_df <- rbind(feature_df_1, feature_df_2)
  z_range <- sort(unique(pull_feature_df$z))
  
  i=24
  i=i+1
  p <- pull_feature_df %>% 
    dplyr::filter(z==z_range[i]) %>%
    tmpFn_plot_top_feature(feature_name_regex="^nuc") +
    labs(title = paste0("z:", z_range[i], ", i=", i))
  plot(p)
  
  
  # Test function ----
  find_overlap_roi_features(feature_df_1=feature_df_1, feature_df_2=feature_df_2,
                            feature_1_regex="nucleus_", feature_2_regex="nucleolus_", 
                            min_intersect_ratio=1, 
                            min_ratio_roi_overlap=1)
  
  find_overlap_roi_features(feature_df_1=feature_df_1, feature_df_2=feature_df_2,
                            feature_1_regex="nucleus_", feature_2_regex="nucleolus_", 
                            min_intersect_ratio=0.99999, 
                            min_ratio_roi_overlap=1)
  
  p <- pull_feature_df %>% 
    tmpFn_plot_top_feature(feature_name_regex="^nuc") +
    labs(title = paste0("z:", z_range[i], ", i=", i))
  ggplotly(p)
}

