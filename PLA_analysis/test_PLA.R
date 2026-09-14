
# Set up ======================================================================
suppressPackageStartupMessages({
  # Generic
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(writexl)
  
  # Spatial analysis
  library(sp)
  library(sf)
  
  # plot function
  library(ggplot2)
  library(ggprism) # Doesn't seems to works
  library(RColorBrewer)
  library(cowplot)
  library(ggpubr)
  library(ggbeeswarm)
  library(plotly)
})



wd <- "/Users/chad/Lab/dev/embryo_IF_analyzer"
script_dir <- paste0(wd, "/scripts/R")

source(paste0(script_dir, "/brewer_pal_2.r"))
source(paste0(script_dir, "/read_fiji_result.r"))
source(paste0(script_dir, "/FnGroup_roi_2_polygons.r"))
source(paste0(script_dir, "/find_ROI_z_intersect.r"))
source(paste0(script_dir, "/define_feature_group.r"))
source(paste0(script_dir, "/find_overlap_roi_features.r"))

# Input setup ==============================================================
NG_cond_vec <- c("No Primary", "anti-NR5A2", "anti-GATA6", "anti-NR5A2 +\nanti-GATA6")
NK_cond_vec <- c("No Primary", "anti-NR5A2", "anti-KLF5", "anti-NR5A2 +\nanti-KLF5")
cond_color_map <- c(
  `No Primary` = "gray50",
  `anti-NR5A2` = "goldenrod2",
  `anti-GATA6` = "darkturquoise",
  `anti-NR5A2 +\nanti-GATA6` = "coral2",
  `anti-KLF5` = "aquamarine3",
  `anti-NR5A2 +\nanti-KLF5` = "coral2",
  character(0)
)
# For black and white plot
cond_color_map_BW <- setNames(rep("gray50", length(cond_color_map)), nm=names(cond_color_map))


# Choose dataset ==============================================================
DATASET_NAME="klf"
REP_NAME="rep2"


if(grepl(tolower(DATASET_NAME), pattern="^gata") && grepl(tolower(REP_NAME), pattern="^rep1")){
  ## N-G Rep1 ----------------------------------------------------------------- 
  #   path: /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251003_Nr5a2_Gata6_PLA_rep1_DAPI_TexasRed.lif
  # description: 
  #   replicate: rep1
  # Date: 2025-10-03
  # Channels:
  #   ch1: DAPI
  # ch2: PLA (NR5A2 + GATA6, TexasRed)
  # condition_position:
  #   no_primary: position 1-6
  #   NR5A2_only: position 7-13
  #   GATA6_only: position 14-20
  #   NR5A2_GATA6_PLA: position 21-35
  
  ## NG Rep1
  data_dir <- "/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Gata6_rep1"
  data_basename = "^PLA_Nr5a2_Gata6\\-"
  data_group_name = "Nr5a2_Gata6_rep1"
  cond_vec <- NG_cond_vec
  pos_2_cond_map <- c(
    rep(cond_vec[1], 6-1+1), 
    rep(cond_vec[2], 13-7+1),
    rep(cond_vec[3], 20-14+1),
    rep(cond_vec[4], 35-21+1),
    character(0)
  )
  excluded_position = c(
    14, # 16-cell
    integer(0)
  )
  excluded_nuc <- list()
  
  
  
}else if(grepl(tolower(DATASET_NAME), pattern="^gata") && grepl(tolower(REP_NAME), pattern="^rep2")){
  ## N-G Rep2 -----------------------------------------------------------------
  #   path: /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251005_PLA_Nr5a2_Gata6_rep2_DAPU_Texasred.lif
  # description: 
  #   replicate: rep2
  # Date: 2025-10-05
  # Channels:
  #   ch1: DAPI
  # ch2: PLA (Nr5a2 + GATA6, TexasRed)
  # condition_position:
  #   no_primary: position 1-6
  # NR5A2_only: position 7-12
  # GATA6_only: position 13-20
  # NR5A2_GATA6_PLA: position 21-35
  ## NG Rep2
  # data_dir <- "/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Gata6_rep2/measurement_01"
  data_dir <- "/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Gata6_rep2"
  data_basename = "^PLA_Nr5a2_Gata6_rep2\\-"
  data_group_name = "Nr5a2_Gata6_rep2"
  cond_vec <- NG_cond_vec
  pos_2_cond_map <- c(
    rep(cond_vec[1], 6-1+1), 
    rep(cond_vec[2], 12-7+1),
    rep(cond_vec[3], 20-13+1),
    rep(cond_vec[4], 35-21+1),
    character(0)
  )
  excluded_position = c()
  excluded_nuc <- list()
  # Representative position:
  # P11 P10 (7?)
  # P14
  # P22 24 (7-8), 28
  
  
  
}else if(grepl(tolower(DATASET_NAME), pattern="^klf") && grepl(tolower(REP_NAME), pattern="^rep1")){
  ## N-K Rep1 -----------------------------------------------------------------
  #   path: /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251003_Nr5a2_Klf5_PLA_rep1_DAPI_TexasRed.lif
  # description: 
  #   replicate: rep1
  # Date: 2025-10-03
  # Channels:
  #   ch1: DAPI
  # ch2: PLA (NR5A2 + KLF5, TexasRed)
  # condition_position:
  #   no_primary: position 1-6
  # NR5A2_only: position 7-14
  # KLF5_only: position 15-21
  # NR5A2_KLF5_PLA: position 22-32
  
  # # NK Rep1
  data_dir <- "/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Klf5_rep1"
  data_basename = "^PLA_Nr5a2_Klf5_rep1\\-"
  data_group_name = "Nr5a2_Klf5_rep1"
  cond_vec <- NK_cond_vec
  pos_2_cond_map <- c(
    rep(cond_vec[1], 6-1+1), 
    rep(cond_vec[2], 14-7+1),
    rep(cond_vec[3], 21-15+1),
    rep(cond_vec[4], 32-22+1),
    character(0)
  )
  # excluding entire position
  excluded_position = c(
    17, # 4C?
    20, # > 8C
    30  # > 8C
  )
  # excluding some nucleus of certain position
  excluded_nuc <- list(
    position_4 = c(4),
    position_5 = c(6, 9)
  )
  
  
  
}else if(grepl(tolower(DATASET_NAME), pattern="^klf") && grepl(tolower(REP_NAME), pattern="^rep2")){
  ## N-K Rep2 -----------------------------------------------------------------
  #   path: /Volumes/pool-toti-imaging/Wataru/Stellaris/2025/20251005_PLA_Nr5a2_KLF5_rep2_DAPI_TexaRed.lif
  # description: 
  #   replicate: rep2
  # Date: 2025-10-05
  # Channels:
  #   ch1: DAPI
  # ch2: PLA (NR5A2 + KLF5, TexasRed)
  # condition_position:
  #   no_primary: position 1-7
  # NR5A2_only: position 8-14
  # KLF5_only: position 15-21
  # NR5A2_KLF5_PLA: position 22-31
  
  # NK Rep2
  data_dir <- "/Users/chad/Lab/dev/embryo_IF_analyzer/data/PLA_Nr5a2_Klf5_rep2/pool_res"
  data_basename = "^PLA_Nr5a2_Klf5_rep2\\-"
  data_group_name = "Nr5a2_Klf5_rep2"
  cond_vec <- NK_cond_vec
  pos_2_cond_map <- c(
    rep(cond_vec[1], 7-1+1), 
    rep(cond_vec[2], 14-8+1),
    rep(cond_vec[3], 21-15+1),
    rep(cond_vec[4], 31-22+1),
    character(0)
  )
  # excluding entire position
  excluded_position = c(
    24, # funky nucleus
    25, # duplicated position with 26
    integer(0)
  )
  # excluding some nucleus of certain position
  excluded_nuc <- list()
  
  # P26, 28
  
  
  
}else{
  stop("Unknown dataset name or replicate name", "\nDATASET_NAME: ", DATASET_NAME, "\nREP_NAME: ", REP_NAME)
}


## Declare conditon -----------------------------------------------------------
cond_compare_list <- list(
  G1 = list(
    c(cond_vec[4], cond_vec[3]),
    c(cond_vec[4], cond_vec[2]),
    c(cond_vec[4], cond_vec[1])
  ),
  G2 = list(
    c(cond_vec[2], cond_vec[3]),
    c(cond_vec[1], cond_vec[2]),
    c(cond_vec[1], cond_vec[3])
  )
)


## Listing inputs -------------------------------------------------------------
outdir <- paste0(data_dir, "/output")
plot_qc_dir <- paste0(outdir, "/plot_QC")
dir.create(plot_qc_dir, recursive=TRUE, showWarnings=FALSE)




# ........................................ ----
# Select position =============================================================
# ........................................ ----
if(F){
  rm(list=ls()[!ls() %in% c("outdir", "data_group_name")])
  load(paste0(outdir, "/", data_group_name, "-session.RData"))
  
  ggplotly(plot_list$pla_density + labs(y="density"))
  ggplotly(plot_list$pla_density_embryo + labs(y="density"))
  
  # NG-rep1
  # ctrl: any
  # N: 7, or 11
  # G: any (maybe 15 or 17?)
  # NG: 33, 28, 21
  
  # NG-rep2
  # ctrl: any (maybe 3)
  # N: 9, 11
  # G: any (maybe 16?)
  # NG: 22, 27
  
  # NK-rep1
  # ctrl: any (maybe 1, 5)
  # N: any (maybe 9, 12)
  # K: 15, 21, 19
  # NK: 22, 29, 31
  
  # NK-rep2
  # ctrl: any (maybe 5, 2)
  # N: 8, 14, 11
  # K: 18, 20, 21
  # NK: 28 (2,7), 30 (6), 31 (1,4)
  
}


# list inputs -----------------------------------------------------------------
# data_basename = "^test_PLA\\-"
data_df <- tibble()
all_data_path <- list.files(data_dir, pattern=data_basename, full.names=TRUE, recursive=TRUE)
all_position <- sort(unique(str_extract(basename(all_data_path), pattern="[Pp]osition\\d+")))

data_df <- tibble(position = all_position, nuc_outline=NA, nuc_result=NA, PLA_outline=NA, PLA_result=NA)

# Find each type of data
for(i in seq_along(data_df$position)){
  cur_pos <- data_df$position[i]
  cur_f <- all_data_path %>% 
    {subset(., str_detect(basename(.), pattern=cur_pos))}
  
  # Nucleus-related outputs
  cur_nuc_outline <- cur_f %>% 
    {subset(., str_detect(basename(.), pattern="_nucleus_outline\\.txt$"))}
  data_df$nuc_outline[i] <- cur_nuc_outline[1]
  
  cur_nuc_result <- cur_f %>% 
    {subset(., str_detect(basename(.), pattern="_nucleus_res\\.txt$"))}
  data_df$nuc_result[i] <- cur_nuc_result[1]
  
  # PLA-related outputs
  cur_PLA_outline <- cur_f %>% 
    {subset(., str_detect(basename(.), pattern="_PLA_outline\\.txt$"))}
  data_df$PLA_outline[i] <- cur_PLA_outline[1]
  
  cur_PLA_result <- cur_f %>% 
    {subset(., str_detect(basename(.), pattern="_PLA_res\\.txt$"))}
  data_df$PLA_result[i] <- cur_PLA_result[1]
}


head(data_df)
dim(data_df)

# Util functions ============================================================
annotate_particle <- function(outline_df, fiji_res, particle_name="", roi_regex=NULL, 
                              min_circ=0, max_circ=1, min_area=0, max_area=Inf,
                              min_avg_area=0, max_z_dist=3, min_z_span=2){
  # hist(outline$circ, 100)
  valid_roi <- fiji_res %>% 
    # dplyr::filter(!((area>=200)&(circ<0.5))) %>% 
    dplyr::filter(area >= min_area,
                  area <= max_area,
                  circ >= min_circ, 
                  circ <= max_circ) %>%
    dplyr::pull(roi) %>% unique()
  
  invalid_roi <- unique(subset(outline_df$roi, !(outline_df$roi %in% valid_roi)))
  outline_df <- outline_df %>% 
    mutate(include = roi %in% valid_roi)
  message("The total of ", sum(outline_df$include), "/", nrow(outline_df), " (", signif(sum(outline_df$include)/nrow(outline_df)*100, 2), " %) particle remains")
  
  # pg = polygon
  outline_pg_df <- polygonize_roi_df(dplyr::filter(outline_df, include))
  outline_pg_df$area <- sf::st_area(outline_pg_df$geometry)
  outline_pg_df <- dplyr::mutate(outline_pg_df, roi_type = stringr::str_extract(roi, "^[:alpha:]+"))
  
  # Annotate nucleus
  f_prefix <- paste0(particle_name, "_")
  f_prefix <- sub(f_prefix, pattern="^_", replacement="") # in case of empty input
  # return(outline_pg_df)
  particle_df <- tibble()
  try({
    particle_df <- define_feature_group(outline_pg_df, 
                                        pre_roi_filter_colname="include", 
                                        roi_regex=roi_regex, 
                                        feature_prefix=f_prefix, 
                                        invalid_feature_prefix="invalid_", 
                                        min_avg_area=min_avg_area,
                                        max_z_dist=max_z_dist, 
                                        min_z_span=min_z_span)
  })
  
  
  return(particle_df)
}


find_group_centroid <- function(p_df, group_by, add_coord=FALS){
  # This may only work with small particle, like PLA
  # Not optimized for big and complicated shape
  
  # Find centroid of individual shape
  p_df$centroid <- sf::st_centroid(p_df$geometry)
  p_ct_df <- p_df %>% 
    as_tibble() %>% 
    dplyr::group_by(!!sym(group_by)) %>% 
    dplyr::reframe(
      # Centroid (XY coordinate)
      group_centroid = sf::st_centroid(sf::st_union(centroid)),
      # Middle Z stack -- not the cleanest approach, but should work for PLA
      min_z = min(z), max_z = max(z),
      z_span = max_z-min_z+1,
      # Finding middle stack
      middle_z = min_z + (max_z-min_z)/2,
    ) %>% 
    dplyr::select(-min_z, -max_z) %>% 
    dplyr::rename(z=middle_z, centroid=group_centroid)
  
  # Adding XY coordinate
  if(add_coord){
    coord_mat <- sp::coordinates(sf::as_Spatial(p_ct_df$centroid))
    p_ct_df$x <- coord_mat[ , 1, drop=TRUE]
    p_ct_df$y <- coord_mat[ , 2, drop=TRUE]
  }
  return(p_ct_df)
}


# point_df <- PLA_ct_df
# polygon_df <- nuc_df
find_point_in_polygon <- function(point_df, polygon_df, point_colname="geometry", polygon_colname="geometry", z_colname="z"){
  # Extract only relevant columns
  p_df <- point_df %>% 
    as_tibble() %>% 
    dplyr::select(point = !!sym(point_colname), z=!!sym(z_colname)) %>% 
    dplyr::mutate(id = seq_along(point)) %>% 
    dplyr::select(id, point, z)
  
  pg_df <- polygon_df %>% 
    as_tibble() %>% 
    dplyr::select(polygon = !!sym(polygon_colname), z=!!sym(z_colname)) %>% 
    dplyr::mutate(id = seq_along(polygon)) %>% 
    dplyr::select(id, polygon, z)
  
  # Round up Z stack to make sure they are comparable
  p_df$z <- round(p_df$z)
  pg_df$z <- round(pg_df$z)
  
  # Placeholder variable
  ovl_df <- tibble(queryHits=integer(0), subjectHits=integer(0))
  
  # Run through z stacks
  z_p <- unique(sort(p_df$z))
  z_pg <- unique(sort(pg_df$z))
  z_vec <- intersect(z_p, z_pg)
  for(i in seq_along(z_vec)){
    p <- dplyr::filter(p_df, z==z_vec[i])
    pg <- dplyr::filter(pg_df, z==z_vec[i])
    # plot(c(p$point, pg$polygon)) # For debugging
    # ASSUMPTION: Expect one point to ovelap to just one polygon only
    ovl <- sf::st_intersects(p$point, pg$polygon, sparse=TRUE)
    ovl_found <- any(sapply(ovl, function(x){length(x)>0}))
    if(!ovl_found) next
    # Taking note of the overlap
    for(o in seq_along(ovl)){
      if(length(ovl[[o]]) == 0) next
      ovl_df <- rbind(ovl_df, tibble(queryHits=p$id[o], subjectHits=pg$id[ovl[[o]]]))
    }
  }
  
  # Check output and return
  ovl_df <- dplyr::arrange(ovl_df, queryHits, subjectHits)
  if(any(duplicated(ovl_df))){
    # ASSUMPTION: Expect one point to ovelap to just one polygon only
    warning("Unexpected behavior detected -- Duplicated hits found, maybe the function is not working properly?")
  }else if(any(duplicated(ovl_df$queryHits))){
    warning("Unexpected behavior detected -- One point overlap with multiple polygon?")
  }
  
  return(ovl_df)
}


# region_df <- nuc_df
# spot_df <- PLA_df
# spot_df$centroid <- sf::st_centroid(spot_df$geometry)
# group_region_by = "feature_id"
count_PLA_spots <- function(region_df, spot_df, group_region_by, region_colname="geometry", spot_colname="centroid", slice_colname="z"){
  # Counting total number of spots inside each feature (group_region_by)
  region_df$region_id <- 1:nrow(region_df)
  ovl <- find_point_in_polygon(point_df=spot_df, polygon_df=region_df, point_colname=spot_colname, polygon_colname=region_colname, z_colname=slice_colname) %>% 
    dplyr::rename(point_id = queryHits, region_id=subjectHits)
  
  # spot_count_df <- 
  ovl[[group_region_by]] <- region_df[[group_region_by]][ovl$region_id]
  spot_df <- ovl %>% 
    dplyr::group_by(region_id, !!sym(group_region_by)) %>% 
    dplyr::reframe(n=n()) %>% 
    dplyr::group_by(!!sym(group_region_by)) %>% 
    dplyr::reframe(n_region = n(), 
                   n_total_spots=sum(n), 
                   n_spot_per_slice=mean(n))
  
  return(spot_df)
}

# ............ ----
# /////////////////////////////////////////////////////////////////////////////
# Analysis section ====
# /////////////////////////////////////////////////////////////////////////////
# ............ ----
force_rerun=FALSE
spot_count_df_path <- paste0(outdir, "/", data_group_name, "-spot_count_df.rds")
PLA_count_df_path <- paste0(outdir, "/", data_group_name, "-PLA_count_df.rds")

if(file.exists(spot_count_df_path) && file.exists(PLA_count_df_path) && !force_rerun){
  cat("\nLoading spot_count_df object ... ")
  spot_count_df <- readRDS(spot_count_df_path)
  PLA_count_df <- readRDS(PLA_count_df_path)
  cat("Done\n")
  
}else{
  PLA_count_df <- tibble() # For count of annotated PLA
  spot_count_df <- tibble() # For avg spot counts in each slice of nucleus, without prior annotation of PLA spots
  for(df_i in 1:nrow(data_df)){
    message("Analyzing Image: ", data_df$position[df_i], " ... \n")
    
    ## Reading Inputs -----------------------------------------------------------
    nuc_outline_df <- read.table(data_df$nuc_outline[df_i], sep="\t", header=TRUE, stringsAsFactors=FALSE)
    nuc_result_df <- read_fiji_result(data_df$nuc_result[df_i])
    
    PLA_outline_df <- data.frame()
    PLA_result_df <- data.frame()
    if(!is.na(data_df$PLA_outline[df_i])){
      PLA_outline_df <- read.table(data_df$PLA_outline[df_i], sep="\t", header=TRUE, stringsAsFactors=FALSE)
    }
    
    if(!is.na(data_df$PLA_result[df_i])){
      PLA_result_df <- read_fiji_result(data_df$PLA_result[df_i])
    }
    
    
    
    ## Annotate nucleus ----------------------------------------------------------
    cat("1")
    nuc_df <- annotate_particle(nuc_outline_df, nuc_result_df, particle_name="nucleus", roi_regex=NULL, 
                                min_circ=0.8, min_area=100, max_area=500,
                                min_avg_area=100, max_z_dist=3, min_z_span=2)
    
    # Centroid of each nucleus (for visualization only)
    nuc_ct_df <- find_group_centroid(nuc_df, group_by="feature_id", add_coord=TRUE) %>% 
      dplyr::filter(!str_detect(feature_id, "(invalid)|(failed)"))
    
    # Placeholder for output
    # empty_nuc_df <- nuc_ct_df %>% 
    #   dplyr::select(nucleus_id = feature_id) %>% 
    #   dplyr::mutate(group = data_group_name,
    #                 position = data_df$position[df_i],
    #                 n_pla = 0)
    empty_nuc_df <- nuc_df %>% 
      dplyr::rename(nucleus_id=feature_id) %>% 
      group_by(nucleus_id) %>% 
      dplyr::reframe(z_span = n(), 
                     total_area = sum(area),
                     mean_area = mean(area)) %>% 
      dplyr::mutate(group = data_group_name,
                    position = data_df$position[df_i],
                    n_pla = 0)
    
    # # take a Quick look
    # if(interactive()){
    #   nuc_df %>% 
    #     dplyr::filter(!grepl(feature_id, pattern="(invalid)|(failed)")) %>%
    #     mutate(z_pc = (z/max(z, na.rm=TRUE))*100) %>%
    #     ggplot(aes(geometry=geometry, color = feature_id)) +
    #     geom_sf(fill=NA) +
    #     geom_sf(data=nuc_ct_df, aes(geometry=centroid), color="gray75") +
    #     ggrepel::geom_text_repel(data=nuc_ct_df, aes(x=x, y=y, label=feature_id, geometry=centroid), color="gray75") +
    #     # scale_color_distiller(type="seq", palette=1) +
    #     # labs(title = "Colored by ROI type (before defining feature)") +
    #     theme_void() +
    #     theme(legend.position="none", panel.background=element_rect(fill="gray10"))
    # }
    
    ## Annotate PLA -------------------------------------------------------------
    PLA_df <- data.frame()
    if(nrow(PLA_outline_df) > 0){
      try({
        PLA_df <- annotate_particle(outline_df=PLA_outline_df, fiji_res=PLA_result_df, particle_name="PLA", roi_regex=NULL, 
                                    min_circ=0, min_area=0, max_area=Inf,
                                    min_avg_area=0, max_z_dist=1, min_z_span=2)
        PLA_df$centroid <- sf::st_centroid(PLA_df$geometry)
      })
    }
    cat(" 2")
    # PLA_pg_df <- polygonize_roi_df(PLA_outline_df)
    # PLA_pg_df$area <- sf::st_area(PLA_pg_df$geometry)
    # PLA_pg_df <- dplyr::mutate(PLA_pg_df, roi_type = stringr::str_extract(roi, "^[:alpha:]+"))
    # PLA_pg_df %>%
    #   mutate(z_pc = (z/max(z, na.rm=TRUE))*100) %>%
    #   ggplot(aes(geometry=geometry, color = z_pc)) +
    #   geom_sf(fill=NA) +
    #   theme_minimal() +
    #   scale_color_distiller(type="seq", palette=1) +
    #   labs(title = "Colored by ROI type (before defining feature)") +
    #   theme_void() +
    #   theme(legend.position="none", panel.background=element_rect(fill="gray10"))
    
    
    PLA_ct_df <- data.frame()
    if(nrow(PLA_df)>0){
      PLA_ct_df <- find_group_centroid(PLA_df, group_by="feature_id", add_coord=TRUE) %>% 
        dplyr::filter(!str_detect(feature_id, "failed"))
    }
    cat(" 3")
    
    # PLA_df %>% 
    #   dplyr::filter(!grepl(feature_id, pattern="failed_")) %>%
    #   mutate(z_pc = (z/max(z, na.rm=TRUE))*100) %>%
    #   ggplot(aes(geometry=geometry, color = feature_id)) +
    #   geom_sf(fill=NA) +
    #   geom_sf(data=PLA_ct_df, aes(geometry=centroid), color="gray75") +
    #   ggrepel::geom_text_repel(data=PLA_ct_df, aes(x=x, y=y, label=str_extract(feature_id, "\\d+"), geometry=centroid), color="gray75") +
    #   theme_minimal() +
    #   # scale_color_distiller(type="seq", palette=1) +
    #   # labs(title = "Colored by ROI type (before defining feature)") +
    #   theme_void() +
    #   theme(legend.position="none", panel.background=element_rect(fill="gray10"))
    
    # Count PLA spots that is inside nucleus ======================================
    
    p2pg_ovl <- NULL
    if(nrow(PLA_ct_df) > 0){
      p2pg_ovl <- find_point_in_polygon(point_df=PLA_ct_df, polygon_df=nuc_df, point_colname="centroid", polygon_colname="geometry", z_colname="z")
      PLA_ct_df$assoc_nucleus_id <- NA
      PLA_ct_df$assoc_nucleus_id[p2pg_ovl$queryHits] <- nuc_df$feature_id[p2pg_ovl$subjectHits]
    }
    cat(" 4")
    
    # Check our work
    nuc_coord_df <- pg_2_coord_df(nuc_df)
    
    # Nucleus
    p <- ggplot() +
      # geom_sf(data=dplyr::filter(nuc_df, !grepl(feature_id, pattern="^nucleus_")), aes(geometry=geometry), fill=NA, color="gray75", alpha=0.3) +
      # geom_sf(data=dplyr::filter(nuc_df, grepl(feature_id, pattern="^nucleus_")), aes(geometry=geometry, color=feature_id), fill=NA, alpha=0.3) +
      geom_polygon(data=dplyr::filter(nuc_coord_df, !grepl(feature_id, pattern="^nucleus_")), 
                   aes(x=x, y=y, group=roi), fill=NA, color="gray75", alpha=0.3) +
      geom_polygon(data=dplyr::filter(nuc_coord_df, grepl(feature_id, pattern="^nucleus_")), 
                   aes(x=x, y=y, group=roi, color=feature_id), fill=NA, alpha=0.3)
    
    # PLA
    if(nrow(PLA_ct_df) > 0){
      PLA_coord_df <- pg_2_coord_df(PLA_df) %>% 
        as_tibble() %>% 
        left_join(., dplyr::select(PLA_ct_df, feature_id, assoc_nucleus_id), by="feature_id") %>% 
        dplyr::mutate(valid = stringr::str_detect(feature_id, pattern="^PLA_"),
                      with_nuc = !is.na(assoc_nucleus_id))
      p <- p + 
        geom_polygon(data=dplyr::filter(PLA_coord_df, !valid),
                     aes(x=x, y=y, group=roi), fill=NA, color="gray50", alpha=0.1) +
        geom_polygon(data=dplyr::filter(PLA_coord_df, valid, !with_nuc),
                     aes(x=x, y=y, group=roi), fill=NA, color="gray50", alpha=0.1) +
        geom_polygon(data=dplyr::filter(PLA_coord_df, valid, with_nuc),
                     aes(x=x, y=y, group=roi, color=assoc_nucleus_id), fill=NA, alpha=0.3) #+
      # geom_point(data=dplyr::filter(PLA_ct_df, !is.na(assoc_nucleus_id)), aes(x=x, y=y, color = assoc_nucleus_id), alpha=0.8, size=2) +
      # geom_point(data=dplyr::filter(PLA_ct_df, is.na(assoc_nucleus_id)), aes(x=x, y=y), alpha=0.8, size=2, shape=21, color="gray25")
    }
    
    p <- p +
      geom_text(data=nuc_ct_df, aes(x=x, y=y, label=sub(feature_id, pattern="nucleus_", replacement="N")), size=8, color="black") +
      theme_void() +
      coord_fixed() +
      scale_y_reverse() +
      labs(title=paste0(data_group_name, " | ", data_df$position[df_i])) +
      theme(legend.position="none", 
            title=element_text(size=20),
            panel.background=element_rect(fill="white", color="gray25", linewidth=2))
    ggsave(plot=p, filename=paste0(plot_qc_dir, "/", data_group_name, "_", data_df$position[df_i], "_QC.png"), width=5, height=5, dpi=300, units="in")
    
    cat(" 5")
    
    # Adding main data
    if(nrow(PLA_df) == 0){
      PLA_count_df <- bind_rows(PLA_count_df, empty_nuc_df)
      spot_count_df <- bind_rows(PLA_count_df, empty_nuc_df)
      next
    }
    cur_PLA_count_df <- PLA_ct_df %>% 
      dplyr::rename(nucleus_id = assoc_nucleus_id) %>% 
      tidyr::replace_na(replace=list(nucleus_id = "non_nucleus")) %>% 
      dplyr::group_by(nucleus_id) %>% 
      dplyr::reframe(n_pla = n()) %>% 
      dplyr::full_join(x=., y = dplyr::select(empty_nuc_df, -n_pla), by = "nucleus_id") %>% 
      tidyr::replace_na(list(n_pla=0, 
                             group=unique(empty_nuc_df$group), 
                             position=unique(empty_nuc_df$position))) %>% 
      dplyr::mutate(is_nucleus = grepl(nucleus_id, pattern="^nucleus_\\d+$"))
    
    PLA_count_df <- bind_rows(PLA_count_df, cur_PLA_count_df)
    cat(" 6")
    
    # other count type
    cur_spot_count_df <- count_PLA_spots(region_df=nuc_df, spot_df=PLA_df, group_region_by="feature_id", region_colname="geometry", spot_colname="centroid", slice_colname="z") %>% 
      dplyr::rename(nucleus_id = feature_id) %>% 
      dplyr::mutate(n_pla = n_total_spots) %>% # compatibility with other dataframe
      dplyr::full_join(x=., y = dplyr::select(empty_nuc_df, -n_pla), by = "nucleus_id") %>% 
      tidyr::replace_na(list(n_pla=0, 
                             group=unique(empty_nuc_df$group), 
                             position=unique(empty_nuc_df$position))) %>% 
      dplyr::mutate(is_nucleus = grepl(nucleus_id, pattern="^nucleus_\\d+$")) # Don't think we'll use this
    spot_count_df <- bind_rows(spot_count_df, cur_spot_count_df)
    cat(" 7")
    cat(" X\n")
  }
  
  saveRDS(PLA_count_df, file=PLA_count_df_path)
  saveRDS(spot_count_df, file=spot_count_df_path)
}


## Filter and pick which value will be used for plotting

spot_count_df <- spot_count_df %>% 
  tidyr::replace_na(list(n_total_spots=0, n_spot_per_slice=0)) %>% 
  dplyr::filter(grepl(nucleus_id, pattern="^nucleus_"),
                z_span > 5) %>% 
  # pick what we actually want to use for plotting
  dplyr::mutate(n_pla = n_spot_per_slice)
  # dplyr::mutate(n_pla = n_total_spots)

hist(spot_count_df$z_span)


## Pick which one we gonna use for plotting
count_df <- PLA_count_df
count_df <- spot_count_df


# Calculate spot density (divided by area)
count_df <- count_df %>% 
  dplyr::mutate(
    pla_density = (n_total_spots/total_area)*1E2 # Number of spots detected per 100 um^2
  )
# range(count_df$pla_density)



# /////////////////////////////////////////////////////////////////////////////
# Annotate positions ====
# /////////////////////////////////////////////////////////////////////////////

# pos_2_cond_map
count_df <- count_df %>% 
  dplyr::mutate(pos = as.numeric(str_extract(position, "\\d+")),
                antibody = pos_2_cond_map[pos], # defined at the beginning of the script
                condition = factor(antibody, levels=cond_vec)) %>% 
  dplyr::filter(!(pos %in% excluded_position))

if(length(excluded_nuc) > 0){
  for(i in seq_along(excluded_nuc)){
    excluded_pos_num <- as.integer(str_extract(names(excluded_nuc)[i], "\\d+"))
    count_df <- count_df %>% 
      dplyr::mutate(nuc_num = as.numeric(str_extract(nucleus_id, "\\d+"))) %>% 
      dplyr::filter(!((pos == excluded_pos_num) & (nuc_num %in% excluded_nuc[[i]])))
  }
}

# colnames(count_df)
avg_count_df <- count_df %>% 
  dplyr::filter(nucleus_id != "non_nucleus") %>% 
  dplyr::group_by(position, condition) %>% 
  reframe(num_nucleus=n(), 
          mean_z_span = mean(z_span),
          mean_nuclear_area = sum(mean_area),
          total_num_spots = sum(n_total_spots),
          mean_num_spots = mean(n_total_spots),
          mean_spots_per_slice = mean(n_spot_per_slice),
          mean_spots_density = mean(pla_density))

# PLA_count_df %>% 
#   dplyr::select(position, antibody) %>% 
#   unique()

# Spots per nucleus, per position

## didn't work
# cond_map <- c(
#   `No Primary` = expression("No Primary"), 
#   `anti-NR5A2` = expression(paste0(alpha, "-NR5A2")), 
#   `anti-GATA6` = expression(paste0(alpha, "-GATA6")), 
#   `anti-NR5A2 +\nanti-GATA6` = expression(paste0(alpha, "-NR5A2 + \n", alpha, "-GATA6"))
# )

# Making plots ================================================================
PLA_theme <- theme_classic() +
  theme(title=element_text(color="black", size=15, family="Helvetica"),
        axis.line=element_line(color="black", linewidth=1),
        axis.ticks.length.x = unit(2, "mm"),
        axis.ticks.length.y = unit(4, "mm"),
        axis.ticks=element_line(color="black", linewidth=1),
        axis.text=element_text(color="black", size=15, family="Helvetica"),
        axis.text.x=element_text(color="black", size=15, family="Helvetica"),
        axis.title=element_text(color="black", size=15, family="Helvetica"),
        legend.text=element_text(color="black", size=15, family="Helvetica"),
        legend.title=element_text(color="black", size=15, family="Helvetica"))



## Average spots per nucleus --------------------------------------------------
plot_PLA <- function(df, y, ylab=NULL, colmap=NULL, y_break_step=1,
                     plot_per_embryo=FALSE, embryo_summary_function="mean", add_n=FALSE, stats_compare_list){
  if(is.null(ylab)) ylab <- y
  df$y <- df[[y]]
  # df$x <- df$condition # Assume this is in the input
  df <- df %>% 
    dplyr::filter(nucleus_id != "non_nucleus") %>% 
    dplyr::mutate(label=paste0(position, "-", nucleus_id),
                  condition = as.character(condition),
                  x = condition)
  
  if(plot_per_embryo){
    embryo_summary_function = eval(parse(text=embryo_summary_function))
    df <- df %>% 
      dplyr::filter(nucleus_id != "non_nucleus") %>% 
      dplyr::group_by(position, condition, x) %>% 
      reframe(n=n(), 
              y = embryo_summary_function(y)) %>% 
      dplyr::mutate(label=position)
  }
  
  if(add_n){
    # Adding information of number of samples per condition
    cond_rename_map <- df %>%
      dplyr::mutate(condition = as.character(condition)) %>% 
      dplyr::group_by(condition) %>% 
      dplyr::reframe(n=n(), 
                     condition_label = paste0(condition, "\n(n=", n, ")")) %>% 
      dplyr::pull(condition_label, name=condition)
    
    df <- dplyr::mutate(df, x = cond_rename_map[x])
    
    # Rename condition in stats compare list
    for(i in seq_along(stats_compare_list)){
      stats_compare_list[[i]] <- cond_rename_map[stats_compare_list[[i]]]
    }
  }
  
  # Factorizing condition and X-axis
  p_df <- df %>% 
    dplyr::mutate(
      x_f = factor(x, levels=unique(x)),
      condition_f = factor(condition, levels=unique(condition))
    )
  
  y_break <- seq(0, max(p_df$y)*2, by=y_break_step)
  # return(p_df)
  p <- p_df %>% 
    ggplot(aes(x=x_f, y=y, label=label)) +
    ggbeeswarm::geom_quasirandom(aes(color=condition_f), size=4, alpha=0.8) +
    geom_boxplot(color="gray25", outlier.color=NA, fill=NA, linewidth=1, alpha=0.7) +
    # Global variable used -- could cause the problem in the future!
    ggpubr::stat_compare_means(comparisons=stats_compare_list, method="wilcox.test", 
                               tip.length=0, step.increase=0.08, hide.ns=TRUE) +
    # labs(title=data_group_name, subtitle=paste0("point = ", ifelse(plot_per_embryo, "embryo", "nucleus")),
    #      x = "Antibody", y=ylab) +
    scale_y_continuous(breaks=y_break, expand=expansion(mult=c(0, 0.07), add=c(0, 1)), limits=c(0, NA)) +
    PLA_theme
  
  if(!is.null(colmap)){
    p <- p + scale_color_manual(values=colmap[as.character(unique(df$condition))])
  }
  return(p)
}


## Assembling plots -----------------------------------------------------------
add_n=TRUE # strats test may not works here
stats_compare_list=c(cond_compare_list$G1, cond_compare_list$G2)
plot_list <- list() # Placeholder list

### PLA / position ----
plot_list$PLA_per_pos <- count_df %>% 
  dplyr::filter(nucleus_id != "non_nucleus") %>% 
  ggplot(aes(x=factor(pos, unique(pos)), y=n_pla, color=condition)) +
  geom_boxplot(color="gray30", outlier.alpha=0) +
  ggbeeswarm::geom_quasirandom() +
  scale_color_manual(values=cond_color_map) +
  labs(title="Number of PLA per imaging positions",
       x = "Position", y="#PLA spots", 
       color="Antibody") +
  PLA_theme
print(plot_list$PLA_per_pos)


### z-span ----
plot_list$z_span <- plot_PLA(
  df=count_df, y="z_span", add_n=add_n,
  colmap=cond_color_map, y_break_step=2,
  stats_compare_list=stats_compare_list
) +
  labs(title="Z-span per nucleus", 
       subtitle=paste0(data_group_name, " (point = nucleus)"),
       x = "Antibody", 
       y="#z-slices per nucleus",
       color="Antibody"
  )
plot(plot_list$z_span)


### total area ----
plot_list$tot_area <- plot_PLA(
  df=count_df, y="total_area", add_n=add_n,
  colmap=cond_color_map, y_break_step=500,
  stats_compare_list=stats_compare_list
) +
  labs(title="Total area per nucleus", 
       subtitle=paste0(data_group_name, " (point = nucleus)"),
       x = "Antibody", 
       y = expression("Total area per nucleus ("~mu~"M"^2~")"),
       color="Antibody"
  )
plot(plot_list$tot_area)



### Mean area ----
plot_list$mean_area <- plot_PLA(
  df=count_df, y="mean_area", add_n=add_n,
  colmap=cond_color_map, y_break_step=50,
  stats_compare_list=stats_compare_list
) +
  labs(title="Mean area per nucleus", 
       subtitle=paste0(data_group_name, " (point = nucleus)"),
       x = "Antibody", 
       y = expression("Mean area per nucleus ("~mu~"M"^2~")"),
       color="Antibody"
  )
plot(plot_list$mean_area)


### total PLA ----
plot_list$tot_pla <- plot_PLA(
  df=count_df, y="n_total_spots", 
  colmap=cond_color_map, y_break_step=20,
  stats_compare_list=stats_compare_list
) +
  labs(title="Total PLA spots", 
       subtitle=paste0(data_group_name, " (point = nucleus)"),
       x = "Antibody", 
       y = "Total #PLA spots per nucleus",
       color="Antibody"
  )
plot(plot_list$tot_pla)
# ggsave(plot=plot_list$tot_pla, filename=paste0(outdir, "/", data_group_name, "_plot_nucleus_Tot_spots.pdf"), 
#        width=7, height=7, dpi=300, units="in")



### PLA per slice ----
plot_list$avg_pla <- plot_PLA(
  df=count_df, y="n_spot_per_slice", 
  colmap=cond_color_map, y_break_step=2,
  stats_compare_list=stats_compare_list
) +
  labs(title="#PLA spots per z-slice", 
       subtitle=paste0(data_group_name, " (point = nucleus)"),
       x = "Antibody", 
       y = "Mean #PLA spots per nucleus slice",
       color="Antibody"
  )
plot(plot_list$avg_pla)
# ggsave(plot=plot_list$avg_pla, filename=paste0(outdir, "/", data_group_name, "_plot_nucleus_Avg_spots.pdf"), 
#        width=7, height=7, dpi=300, units="in")


### PLA Density ----
plot_list$pla_density <- plot_PLA(df=count_df, y="pla_density", add_n=add_n,
         colmap=cond_color_map, y_break_step=1, 
         ylab="Nuclear PLA spot density\n(spots/100 um^2)",
         stats_compare_list=stats_compare_list) +
  labs(title="PLA Density", 
       subtitle=paste0(data_group_name, " (point = nucleus)"),
       x = "Antibody", 
       y = expression("PLA Density  (spots /"~mu~"M"^2~")"),
       color="Antibody"
  )
plot(plot_list$pla_density)

# in black and white
plot_list$BW_pla_density <- plot_PLA(df=count_df, y="pla_density", add_n=add_n,
                                  colmap=cond_color_map_BW, y_break_step=1, 
                                  ylab="Nuclear PLA spot density\n(spots/100 um^2)",
                                  stats_compare_list=stats_compare_list) +
  labs(title="PLA Density", 
       subtitle=paste0(data_group_name, " (point = nucleus)"),
       x = "Antibody", 
       y = expression("PLA Density  (spots /"~mu~"M"^2~")"),
       color="Antibody"
  ) +
  theme(legend.position="none")
plot(plot_list$BW_pla_density)


## Average spots per embryos --------------------------------------------------
### Total PLA ----
plot_list$tot_pla_embryo <- plot_PLA(
  df=count_df, y="n_total_spots", 
  # ylab="Total #PLA spots per embryo",
  colmap=cond_color_map, y_break_step=25, 
  plot_per_embryo=TRUE, 
  embryo_summary_function="sum",
  stats_compare_list=stats_compare_list
) +
  labs(title="Total PLA spots", 
       subtitle=paste0(data_group_name, " (point = embryo)"),
       x = "Antibody", 
       y = "Total #PLA spots per embryo",
       color="Antibody"
  )
print(plot_list$tot_pla_embryo)
# ggsave(plot=plot_list$tot_pla_embryo, filename=paste0(outdir, "/", data_group_name, "_plot_embryo_Tot_spots.pdf"), 
#        width=7, height=7, dpi=300, units="in")


### PLA per slice ----
plot_list$avg_pla_embryo <- plot_PLA(
  df=count_df, y="n_spot_per_slice", 
  colmap=cond_color_map, y_break_step=1,
  plot_per_embryo=TRUE,
  embryo_summary_function="mean",
  stats_compare_list=stats_compare_list
) +
  labs(title="#PLA spots per z-slice", 
       subtitle=paste0(data_group_name, " (point = embryo)"),
       x = "Antibody", 
       y = "Mean #PLA spots per nucleus slice",
       color="Antibody"
  )
plot(plot_list$avg_pla_embryo)
# ggsave(plot=plot_list$avg_pla_embryo, filename=paste0(outdir, "/", data_group_name, "_plot_embryo_Avg_spots.pdf"), 
#        width=7, height=7, dpi=300, units="in")



### PLA Density ----
plot_list$pla_density_embryo <- plot_PLA(
  df=count_df, y="pla_density", add_n=add_n,
  colmap=cond_color_map, y_break_step=1, 
  ylab="Nuclear PLA spot density\n(spots/100 um^2)",
  plot_per_embryo=TRUE,
  embryo_summary_function="mean",
  stats_compare_list=stats_compare_list
) +
  labs(title="PLA Density", 
       subtitle=paste0(data_group_name, " (point = embryo)"),
       x = "Antibody", 
       y = expression("PLA Density  (spots /"~mu~"M"^2~")"),
       color="Antibody"
  )
plot(plot_list$pla_density)


## Save plots =================================================================
### nucleus ----
{
  pdf(paste0(outdir, "/", data_group_name, "_plot_nucleus.pdf"), width=10, height=10)
  for(i in seq_along(plot_list)){
    if(!grepl(names(plot_list)[i], pattern="_embryo$")){
      print(plot_list[[i]])
    }
  }
  dev.off()
}

p_nuc <- cowplot::plot_grid(
  plotlist=lapply(list(
    plot_list$z_span, plot_list$tot_area, plot_list$mean_area,
    plot_list$tot_pla, plot_list$avg_pla, plot_list$pla_density
  ), FUN=function(p){p <- p + theme(legend.position="none")}), 
  nrow=2
)
ggsave(plot=p_nuc, filename=paste0(outdir, "/", data_group_name, "_plot_nucleus_composit.pdf"),
       width=20, height=18, dpi=300, units="in")

### embryo ----
{
  pdf(paste0(outdir, "/", data_group_name, "_plot_embryo.pdf"), width=10, height=10)
  for(i in seq_along(plot_list)){
    if(grepl(names(plot_list)[i], pattern="_embryo$")){
      print(plot_list[[i]])
    }
  }
  dev.off()
}

p_embryo <- cowplot::plot_grid(
  plotlist=lapply(list(
    plot_list$tot_pla_embryo, plot_list$avg_pla_embryo, plot_list$pla_density_embryo
  ), FUN=function(p){p <- p + theme(legend.position="none")}), 
  nrow=1
)
ggsave(plot=p_embryo, filename=paste0(outdir, "/", data_group_name, "_plot_embryo_composit.pdf"),
       width=20, height=9, dpi=300, units="in")


saveRDS(plot_list, file=paste0(outdir, "/", data_group_name, "-plot_list.rds"))


# Save output =======
saveRDS(count_df, file=paste0(outdir, "/", data_group_name, "-count_df.rds"))

xlsx_list <- list(
  PLA_count_nucleus = dplyr::select(count_df, group, position, pos, nucleus_id, antibody, condition, 
                                    z_span, total_area, n_total_spots, pla_density),
  PLA_count_embryo = avg_count_df
)
write_xlsx(x=xlsx_list, path=paste0(outdir, "/", data_group_name, "_spot_count.xlsx"))


# Save session
save.image(file=paste0(outdir, "/", data_group_name, "-session.RData"))


