## Function group: Turning Fiji ROI (Region of Interest) into polygons object (sp or sf package)

# The general input -- `roi_df` -- is a data frame object recording the coordinate of each point within individual ROI.
#   This code design to use the coordinate output from the Fiji script: nuclear_selector.ijm
#   This should containing the following columns (in tidy format):
#     ROI ID(s)
#     coordinate -- i.e., x, y, and z columns

roi_extract_xy_coord <- function(roi_df, roi_id){
  # Extract XY coordinate from a single ROI ID.
  # NB: This function is likely used as a dependencies of other functions
  df <- roi_df %>% 
    dplyr::filter(roi == roi_id) %>% 
    {dplyr::add_row(., .[1, ])} # required for closing the polygons in R
  
  m <- as.matrix(df[ , c("x", "y")])
  return(m)
}

## DEPRECIATED: Keep for historical reason
# roi_2_sp_polygons <- function(df, roi_id){
#   xy_coord_m <- roi_extract_xy_coord(df, roi_id)
#   sp_pg <- Polygon(xy_coord_m)
#   return(sp_pg)
# }

roi_2_polygons <- function(roi_df, roi_id_vec){
  # Extract and create multiple polygon object (by sf package) from the given list of ROI IDs
  # Dependencies:
  #   roi_extract_xy_coord
  
  # NB: This function is likely used as a dependencies of other functions
  
  # Extract XY coordinate for all roi_id
  xy_coord_list <- lapply(X=roi_id_vec, FUN=function(x){roi_extract_xy_coord(roi_df, roi_id=x)})
  names(xy_coord_list) <- roi_id_vec
  
  # QC
  n_points_vec <- sapply(xy_coord_list, nrow)
  empty_roi <- n_points_vec == 0
  if(any(empty_roi)){
    warning("Some ROIs didn't return XY coordinate\n")
    xy_coord_list <- xy_coord_list[!empty_roi]
    n_points_vec <- n_points_vec[!empty_roi]
  }
  
  if(length(xy_coord_list) == 0){
    stop("No XY coordinate extracted from the given ROI vector")
  }
  
  # Converting to polygons object (sp package)
  # PG = Polygon
  sp_PG <- lapply(roi_id_vec, FUN=function(x){
    Polygons(list(Polygon(xy_coord_list[[x]])), ID=x)
  })
  names(sp_PG) <- roi_id_vec
  sp_spg <- SpatialPolygons(sp_PG)
  
  # Converting to polygons object (sf package)
  sf_pg <- suppressWarnings(st_as_sf(sp_spg))
  
  return(sf_pg)
  
  # Initialy tryiing to do it this way, but didn't seems to compatible with our goal
  # pg <- st_polygon(xy_coord_list)
}


polygonize_roi_df <- function(roi_df){
  # old function name: df_2_polygons
  # Transforming plain ROI coordinate table -- 
  # Collapsing the x,y,z positioning information of ROI table into polygon geometry object (sf package)
  # ASSUMPTION: each ROI can only locate on a single z-stack
  
  # NB: This function is likely used as a dependencies of other functions
  
  # simplify the ROI information
  unq_roi_id <- unique(roi_df$roi)
  roi_info_df <- unique(dplyr::select(roi_df, roi, z))
  
  ## DEPRECIATED: only keep for historical reason
  # # Extract polygon for each ROI
  # geom_df <- data.frame()
  # for(i in seq_along(unq_roi_id)){
  #   cur_roi <- unq_roi_id[i]
  #   geom_df <- data.frame(roi = cur_roi,
  #                         polygons = roi_2_polygons(roi_df, roi_id_vec = unq_roi_id[i])) %>% 
  #     rbind(geom_df, .)
  # }
  # geom_df <- as_tibble(geom_df)
  
  # Vectorized version:
  geom_df <- tibble(roi = unq_roi_id, geometry = roi_2_polygons(roi_df, roi_id_vec=unq_roi_id)$geometry)
  
  roi_info_df <- left_join(roi_info_df, geom_df, by="roi")
  return(roi_info_df)
}



# Test section ================================================================
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
  
  # Getting data ----
  test_data_dir <- "/Volumes/pool-toti-imaging/Alina/LeicaStellaris/27032024_Nr5a2_timecourse_1hRT/SR/raw_measurements"
  pos_id <- "Position001"
  
  nuc_outline_path <- list.files(test_data_dir, pattern=paste0(pos_id, ".+nucleus_outline\\.txt$"))
  nuc_res_path <- list.files(test_data_dir, pattern=paste0(pos_id, ".+nucleus_res\\.txt$"))
  
  nuc_outline_path <- paste0(test_data_dir, "/", nuc_outline_path)
  nuc_res_path <- paste0(test_data_dir, "/", nuc_res_path)
  
  nuc_outline_df <- as_tibble(read.table(nuc_outline_path, header=TRUE, stringsAsFactors=FALSE))
  # nuc_res_df <- read_fiji_result(nuc_res_path)
  
  
  # Assign key input ----
  roi_df <- nuc_outline_df %>% 
    dplyr::select(-name)
  unq_roi_id <- unique(roi_df$roi)
  
  # Test the functions ----
  # 1)
  xy_mat <- roi_extract_xy_coord(roi_df, roi_id=unq_roi_id[1])
  plot(xy_mat)
  
  # 2)
  roi_pg1 <- roi_2_polygons(roi_df, roi_id_vec=unq_roi_id[1])
  roi_pg2 <- roi_2_polygons(roi_df, roi_id_vec=unq_roi_id[2])
  roi_pg3 <- roi_2_polygons(roi_df, roi_id_vec=unq_roi_id[1:3])
  
  # 3)
  roi_pg_df <- polygonize_roi_df(roi_df)
}

