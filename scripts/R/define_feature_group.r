define_feature_group <- function(roi_df, 
                                 # ROI filtering
                                 pre_roi_filter_colname = "include", roi_area_range = c(0, Inf), roi_regex = NULL,
                                 # ROI overlapping filter
                                 min_intersect_ratio=0.0,
                                 # Feature filtering
                                 max_z_dist=1, min_z_span=5, min_avg_area=NULL, 
                                 # Output control
                                 feature_prefix = "feature_", invalid_feature_prefix = "invalid_feature_", fail_ROI_feature_prefix = "failed_ROI_",
                                 verbose = FALSE){
  
  # Identify a group of ROIs across z-stacks that potentially represent part of the cellular feature/compartment.
  # Dependencies:
  #   from FnGroup_roi_2_polygons.r:
  #     roi_extract_xy_coord
  #     roi_2_polygons
  #     polygonize_roi_df
  
  # Inputs:
  #   Data input:
  #     roi_df:
  #   Filtering variables:
  #     max_z_dist:
  #     min_z_span:
  #     min_avg_area:
  #     min_intersect_ratio:
  
  # ASSUMPTION: 
  #   Each ROI only locate on one z-stack
  #   No overlap between polygons within the same z-stack (TODO: consider addressing this in the future)
  
  
  # Input processing ------------------------------------------------------------------------------
  reject_input <- FALSE
  if(all(c("x", "y", "z") %in% colnames(roi_df))){
    # Collapse x,y,z coordinate in each ROI into polygon objects (sf package)
    roi_pg_df <- polygonize_roi_df(roi_df)
    
  }else if(("geometry" %in% colnames(roi_df))){
    if(("sfc_POLYGON" %in% class(roi_df$geometry))){
      # i.e., the input already has polygon
      roi_pg_df <- roi_df
    }else{
      reject_input <- TRUE
    }
  }else{
    reject_input <- TRUE
  }
  
  if(reject_input){stop("Incorrect type of input")}
  
  
  roi_pg_df$area <- st_area(roi_pg_df$geometry) # Assign area
  
  # Making a reference vector with ROI ID as a key for later on when preparing the output
  roi_z_map <- dplyr::pull(roi_pg_df, z, name=roi)
  roi_area_map <- dplyr::pull(roi_pg_df, area, name=roi)
  
  # Filtering out ROI -----------------------------------------------------------------------------
  
  ## By pre-determined excluded ROI ----
  roi_fail_df <- tibble()
  if(pre_roi_filter_colname %in% colnames(roi_df)){
    # i.e. user pre-filter the ROI before using with this function
    use_roi_flag <- roi_df[[pre_roi_filter_colname]]
    if(sum(!use_roi_flag)>0){
      valid_roi <- roi_df$roi[use_roi_flag]
      # The unused ROI will be excluded from the analysis but will merge back in the output
      roi_fail_df <-  dplyr::filter(roi_pg_df, !(roi %in% valid_roi)) %>% 
        mutate(feature_id = paste0(fail_ROI_feature_prefix, "excluded")) %>% 
        rbind(roi_fail_df, .)
      
      roi_pg_df <- dplyr::filter(roi_pg_df, (roi %in% valid_roi))
      
      if(nrow(roi_pg_df) == 0){
        stop("All ROIs were filtered out (include flag)")
      }
    }
  }
  
  ## By out by ROI name ----
  if(!is.null(roi_regex)){
    # Expect roi_regex to have a length of 1
    valid_roi <- roi_pg_df %>% 
      dplyr::filter(!is.na(roi)) %>% 
      dplyr::filter(str_detect(roi, roi_regex)) %>% 
      dplyr::pull(roi) %>% unique()
    
    roi_fail_df <- dplyr::filter(roi_pg_df, !(roi %in% valid_roi)) %>% 
      mutate(feature_id = paste0(fail_ROI_feature_prefix, "name")) %>% 
      rbind(roi_fail_df, .)
    
    roi_pg_df <- dplyr::filter(roi_pg_df, (roi %in% valid_roi))
    
    if(nrow(roi_pg_df) == 0){
      stop("All ROIs were filtered out (ROI name filtering)")
    }
  }
  
  # By by area 
  if(!is.null(roi_area_range)){
    # Expect roi_area_range to have a length of 2
    valid_roi <- roi_pg_df %>% 
      dplyr::filter(area >= min(roi_area_range, na.rm=TRUE),
                    area <= max(roi_area_range, na.rm=TRUE),
                    !is.na(roi)) %>% 
      dplyr::pull(roi) %>% unique()
    
    roi_fail_df <- dplyr::filter(roi_pg_df, !(roi %in% valid_roi)) %>% 
      mutate(feature_id = paste0(fail_ROI_feature_prefix, "area")) %>% 
      rbind(roi_fail_df, .)
    
    roi_pg_df <- dplyr::filter(roi_pg_df, (roi %in% valid_roi))
    
    if(nrow(roi_pg_df) == 0){
      stop("All ROIs were filtered out (ROI Area)")
    }
  }
  
  
  # for internal tracking of ROI at each stage of the script
  roi_set <- list(
    start = unique(dplyr::pull(dplyr::filter(roi_df, !is.na(roi)), roi)),
    input = unique(dplyr::pull(dplyr::filter(roi_pg_df, !is.na(roi)), roi))
  )
  
  # Find overlap ROI ------------------------------------------------------------------------------
  ovl_pair_df <- find_ROI_z_intersect(roi_pg_df, max_z_dist=max_z_dist, min_intersect_ratio=min_intersect_ratio, verbose=verbose)
  ovl_pair_df <- dplyr::filter(ovl_pair_df, !is.na(roi_1), !is.na(roi_2)) # Just in case
  roi_set$overlap <- unique(c(ovl_pair_df$roi_1, ovl_pair_df$roi_2))
  
  # Which ROI got removed out from this step
  roi_overlap_fail <- roi_set$input[!(roi_set$input %in% roi_set$overlap)]
  if(length(roi_overlap_fail) > 0){
    roi_fail_df <- dplyr::filter(roi_pg_df, (roi %in% roi_overlap_fail)) %>% 
      mutate(feature_id = paste0(fail_ROI_feature_prefix, "overlap")) %>% 
      rbind(roi_fail_df, .)
    
    roi_pg_df <- dplyr::filter(roi_pg_df, !(roi %in% roi_overlap_fail))
  }
  
  # Grouping features (consider making a function) ------------------------------------------------
  
  # Borrow the terminology from the graph network field
  #   node: each ROI
  #   edge: overlap between ROI
  
  # This is similar to roi_pg_df, but without the geometry field
  # Doing it this way will include ROIs that are filtering out when finding z-overlap
  # roi_pg_df %>% 
  #   dplyr::select(roi_id = roi, z, area) %>% 
  #   mutate(feature_group = NA)
  
  roi_node_df <- data.frame(roi_id = unique(c(ovl_pair_df$roi_1, ovl_pair_df$roi_2))) %>%
    as_tibble() %>% 
    mutate(z = roi_z_map[roi_id], 
           feature_group=NA, # place holder
           area = roi_area_map[roi_id])
  
  roi_edge_df <- ovl_pair_df %>%
    dplyr::select(roi_1, roi_2)
  roi_edge_mat <- as.matrix(roi_edge_df)
  
  # Assigning feature groups
  feature_count <- 0
  for(i in 1:nrow(roi_edge_mat)){
    cur_idx <- which(roi_node_df$roi_id %in% roi_edge_mat[i, ])
    # Check if any of these two already has group number assign to it
    cur_nuc_num <- unique(roi_node_df$feature_group[cur_idx]) %>% 
      subset(., !is.na(.))
    
    # Assign nucleus ID
    if(length(cur_nuc_num)==0){
      # i.e., new nucleus found!
      feature_count <- feature_count +1
      roi_node_df$feature_group[cur_idx] <- feature_count
      
    }else if(length(cur_nuc_num)==1){
      # Adding new ROI to the existing group
      roi_node_df$feature_group[cur_idx] <- cur_nuc_num
      
    }else if(length(cur_nuc_num)==2){
      # Joining the two assigned nucleus together
      cur_idx <- which(roi_node_df$feature_group %in% cur_nuc_num)
      roi_node_df$feature_group[cur_idx] <- min(cur_nuc_num)
    }
  }
  
  # Filtering feature -----------------------------------------------------------------------------
  # By number of z-span
  # Calculate number of z-span per nucleus
  z_span_df <- roi_node_df %>% 
    dplyr::filter(!is.na(feature_group)) %>% 
    dplyr::select(feature_group, z) %>% 
    unique() %>% 
    {table(.$feature_group)} %>% 
    as.data.frame() %>% as_tibble() %>% 
    `colnames<-`(c("feature_group", "n_z_span")) %>% 
    mutate(feature_group = as.double(feature_group))
  
  valid_feature_group <- z_span_df %>% 
    dplyr::filter(n_z_span >= min_z_span) %>% 
    pull(feature_group)
  
  # Filter by average area
  if(!is.null(min_avg_area)){
    nuc_stats_df <- roi_node_df %>% 
      group_by(feature_group) %>% 
      reframe(mean_area = mean(area))
    
    valid_feature_group_byArea <- nuc_stats_df %>% 
      dplyr::filter(mean_area >= min_avg_area) %>% 
      pull(feature_group) 
    
    # Filtering the list
    valid_feature_group <-  base::intersect(valid_feature_group, valid_feature_group_byArea)
  }
  
  ## Assign feature_id --------------------------------------------------------------------------------
  # valid nuc
  valid_feature_id_map <- paste0(feature_prefix, seq_along(valid_feature_group))
  names(valid_feature_id_map) <- sort(valid_feature_group)
  # invalid nuc
  invalid_feature_group <- unique(roi_node_df$feature_group) %>% subset(., !(. %in% valid_feature_group))
  invalid_feature_id_map <- paste0(invalid_feature_prefix, seq_along(invalid_feature_group))
  names(invalid_feature_id_map) <- invalid_feature_group
  
  ## Assign feature_id
  feature_id_map <- c(valid_feature_id_map, invalid_feature_id_map)
  roi_node_df <- mutate(roi_node_df, feature_id = feature_id_map[as.character(feature_group)])
  
  # # Visualizing graph
  # g <- tbl_graph(nodes=roi_node_df, edges=roi_edge_df, directed=FALSE)
  # g %>%
  #   activate(nodes) %>%
  #   mutate(feature_id = factor(feature_id)) %>%
  #   ggraph(layout = 'kk') +
  #   geom_edge_link() +
  #   geom_node_point(aes(color=feature_id), size = 1)
  
  # Preparing output ------------------------------------------------------------------------------
  out_df <- left_join(roi_pg_df, dplyr::select(roi_node_df, roi=roi_id, feature_id), by="roi")
  if(nrow(roi_fail_df) > 0){
    # Merged back with the held out ROIs
    out_df <- add_row(out_df, roi_fail_df)
  }
  return(out_df)
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
  
  # Testing function ------------------------------------------------------------------------------
  feature_df_1 <- define_feature_group(roi_df, pre_roi_filter_colname="include", 
                                       feature_prefix="nuc_", invalid_feature_prefix="invalid_")
  dplyr::filter(feature_df_1, is.na(feature_id)) # Checking roi_pg_df that might be failed to filtering out
  any(duplicated(feature_df_1$roi))
  table(feature_df_1$feature_id)
  
  # Test ROI area range
  feature_df_2 <- define_feature_group(roi_df, pre_roi_filter_colname="include", roi_area_range=c(100, 300), 
                                       feature_prefix="nuc_", invalid_feature_prefix="invalid_")
  dplyr::filter(feature_df_2, is.na(feature_id)) # Checking roi_pg_df that might be failed to filtering out
  any(duplicated(feature_df_2$roi))
  table(feature_df_2$feature_id)
  
  # Test Name filtering
  feature_df_3 <- define_feature_group(roi_df, pre_roi_filter_colname="include", roi_regex="^nucleus", 
                                       feature_prefix="nuc_", invalid_feature_prefix="invalid_")
  dplyr::filter(feature_df_3, is.na(feature_id)) # Checking roi_pg_df that might be failed to filtering out
  any(duplicated(feature_df_3$roi))
  table(feature_df_3$feature_id)
  
  
  feature_df_nuc <- define_feature_group(roi_df, pre_roi_filter_colname="include", roi_regex="^nucleus", 
                                          feature_prefix="nucleus_", invalid_feature_prefix="invalid_", max_z_dist=3, min_z_span=5)
  dplyr::filter(feature_df_nuc, is.na(feature_id)) # Checking roi_pg_df that might be failed to filtering out
  any(duplicated(feature_df_nuc$roi))
  table(feature_df_nuc$feature_id)
  
  feature_df_nucl <- define_feature_group(roi_df, pre_roi_filter_colname="include", roi_regex="^nucleolus", 
                                          feature_prefix="nucleolus_", invalid_feature_prefix="invalid_", max_z_dist=1, min_z_span=2)
  dplyr::filter(feature_df_nucl, is.na(feature_id)) # Checking roi_pg_df that might be failed to filtering out
  any(duplicated(feature_df_nucl$roi))
  table(feature_df_nucl$feature_id)
  
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
  
  
  tmpFn_plot_top_feature(feature_df_1, feature_name_regex="^nuc_", invalid_feature_name_regex="^invalid_")
  tmpFn_plot_top_feature(feature_df_2, feature_name_regex="^nuc_", invalid_feature_name_regex="^invalid_")
  tmpFn_plot_top_feature(feature_df_3, feature_name_regex="^nuc_", invalid_feature_name_regex="^invalid_")
  
  feature_df_nuc_fil <- feature_df_nuc %>% 
    dplyr::filter(str_detect(feature_id, "^nucleus"))
  tmpFn_plot_top_feature(feature_df_nuc_fil, feature_name_regex="^nucleus")
  
  feature_df_nucl_fil <- feature_df_nucl %>% 
    dplyr::filter(str_detect(feature_id, "^nucleolus"))
  tmpFn_plot_top_feature(feature_df_nucl_fil, feature_name_regex="^nucleolus")
  
  rbind(feature_df_nuc_fil, feature_df_nucl_fil) %>% 
    tmpFn_plot_top_feature(feature_name_regex="^nuc")
  
}

