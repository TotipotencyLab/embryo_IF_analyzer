#' Would this bridge weld two objects that sit side by side?
#'
#' One object contributes one ROI per z-slice -- `define_feature_group()`
#' assumes no two polygons overlap within a slice -- so a group holding two
#' ORDINARY ROIs on the same slice is not one object followed through z, it is
#' two objects fused. That is the difference between the two things a bridge
#' can do: rescuing a pinched object joins components that are DISJOINT in z,
#' while a merged mask joins components that COEXIST on the same slices.
#'
#' Measured on real oocyte data: a circularity cut rejects the mask covering
#' two touching oocytes precisely BECAUSE it is a figure-of-eight, so bridging
#' those rejects hands back what the filter was removing.
#'
#' Counted as a DELTA, not as a presence. The seed pass can legitimately leave
#' a component already stacked (a run with no circularity filter does this on
#' real data), and testing for "any duplicate" would then refuse every later
#' bridge touching it for a collision it did not cause.
#'
#' @param node_df Node table with `z`, `is_bridge` and `feature_group`.
#' @param new_idx Row indices of the edge's two endpoints.
#' @param groups Group numbers those endpoints already belong to.
#' @return `TRUE` if the merge would create a new same-slice collision.
.stacks_seeds <- function(node_df, new_idx, groups){
  seed_z <- function(idx){
    idx <- idx[!node_df$is_bridge[idx]]
    return(node_df$z[idx])
  }
  idx_all <- unique(c(new_idx, which(node_df$feature_group %in% groups)))
  after <- sum(duplicated(seed_z(idx_all)))

  before <- 0
  for(g in groups){
    before <- before + sum(duplicated(seed_z(which(node_df$feature_group == g))))
  }
  return(after > before)
}

#' Warning text for "no ordinary ROI survived a pre-grouping filter"
#'
#' Keeps the original wording as a prefix so existing callers matching on it
#' still match, and names the bridges when there are any -- otherwise the
#' message says everything was filtered out while the output still holds rows.
#'
#' @param stage Filter name, as it appeared in the original message.
#' @param df ROI table carrying `is_bridge`.
#' @return A single string.
.no_real_roi_msg <- function(stage, df){
  nb <- if("is_bridge" %in% colnames(df)){sum(df$is_bridge)}else{0}
  if(nb > 0){
    return(paste0("All ROIs were filtered out (", stage, "); ", nb,
                  " bridge ROI(s) remain, and bridges alone cannot form a feature"))
  }
  return(paste0("All ROIs were filtered out (", stage, ")"))
}

#' Label surviving bridge ROIs as failures
#'
#' Used on the early-return paths, where every non-bridge ROI has been filtered
#' out. A bridge cannot stand on its own, so with nothing left to bridge it is
#' reported as a failed ROI rather than silently dropped from the output.
#'
#' @param df ROI table carrying an `is_bridge` column.
#' @param fail_prefix `fail_ROI_feature_prefix` from the caller.
#' @return The bridge rows, with `feature_id` set; zero rows if there are none.
.bridges_as_failed <- function(df, fail_prefix){
  if(!nrow(df) || !("is_bridge" %in% colnames(df))){return(df[0, , drop=FALSE])}
  b <- dplyr::filter(df, is_bridge)
  if(!nrow(b)){return(b)}
  return(dplyr::mutate(b, feature_id = paste0(fail_prefix, "bridge")))
}

define_feature_group <- function(roi_df,
                                 # ROI filtering
                                 pre_roi_filter_colname = "include", roi_area_range = c(0, Inf), roi_regex = NULL,
                                 # Which pre-grouping rejects bridge instead of dropping
                                 bridge_roi = NULL,
                                 # ROI overlapping filter
                                 min_intersect_ratio=0.0,
                                 # Feature filtering
                                 max_z_dist=1, min_z_span=5, min_avg_area=NULL, feature_area_range=NULL,
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

  # Bridging --------------------------------------------------------------------------------------
  # A pre-grouping ROI filter drops ROIs before the overlap graph is built, so
  # removing an object's interior slices opens a z-gap that max_z_dist cannot
  # span and one object is counted as two. Naming a filter here keeps its
  # rejects in the graph as EDGE-FORMERS ONLY: a bridge ROI can hold an object
  # together across the gap, but it cannot seed a feature, does not count
  # toward min_z_span, and does not contribute to the feature's mean area.
  # It can connect real ROIs; it can never invent an object out of rejects.
  #
  # "name" is deliberately NOT bridgeable. The other two reject on quality, and
  # a low-quality ROI of the right feature is still that feature. roi_regex
  # rejects on IDENTITY -- an ROI of a DIFFERENT feature type -- and letting one
  # form edges would glue two unrelated objects into one.
  bridgeable <- c("include", "area")
  if(!is.null(bridge_roi)){
    bridge_roi <- unique(as.character(bridge_roi))
    bad <- bridge_roi[!(bridge_roi %in% bridgeable)]
    if(length(bad)){
      stop("bridge_roi must be a subset of c(", paste0('"', bridgeable, '"', collapse=", "),
           "); got: ", paste(bad, collapse=", "),
           if("name" %in% bad) " -- 'name' cannot bridge: it rejects on feature identity, not quality" else "",
           call. = FALSE)
    }
  }
  bridges <- function(reason){ !is.null(bridge_roi) && (reason %in% bridge_roi) }

  roi_pg_df$area <- st_area(roi_pg_df$geometry) # Assign area

  # Initialised after area so the output column order matches
  # note/data_formats.md. Always present, whether or not anything bridges: a
  # column that appears only when a flag is set makes every downstream reader
  # test for its existence before it can use it.
  roi_pg_df$is_bridge <- FALSE
  
  # Making a reference vector with ROI ID as a key for later on when preparing the output
  roi_z_map <- dplyr::pull(roi_pg_df, z, name=roi)
  roi_area_map <- dplyr::pull(roi_pg_df, area, name=roi)
  # NB: no is_bridge map here. The pre-filters below are what SET is_bridge, so
  #     a map built at this point would be all FALSE. It is built after them.
  
  # Filtering out ROI -----------------------------------------------------------------------------
  
  ## By pre-determined excluded ROI ----
  roi_fail_df <- tibble()
  if(pre_roi_filter_colname %in% colnames(roi_df)){
    # i.e. user pre-filter the ROI before using with this function
    use_roi_flag <- roi_df[[pre_roi_filter_colname]]
    if(sum(!use_roi_flag)>0){
      valid_roi <- roi_df$roi[use_roi_flag]
      if(bridges("include")){
        # Kept in the graph, but demoted: edges only.
        roi_pg_df$is_bridge <- roi_pg_df$is_bridge | !(roi_pg_df$roi %in% valid_roi)
      }else{
        # The unused ROI will be excluded from the analysis but will merge back in the output
        roi_fail_df <-  dplyr::filter(roi_pg_df, !(roi %in% valid_roi)) %>%
          mutate(feature_id = paste0(fail_ROI_feature_prefix, "excluded")) %>%
          rbind(roi_fail_df, .)

        roi_pg_df <- dplyr::filter(roi_pg_df, (roi %in% valid_roi))
      }

      if(sum(!roi_pg_df$is_bridge) == 0){
        warning(.no_real_roi_msg("include flag", roi_pg_df))
        return(rbind(roi_fail_df, .bridges_as_failed(roi_pg_df, fail_ROI_feature_prefix)))
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

    if(sum(!roi_pg_df$is_bridge) == 0){
      warning(.no_real_roi_msg("ROI name filtering", roi_pg_df))
      return(rbind(roi_fail_df, .bridges_as_failed(roi_pg_df, fail_ROI_feature_prefix)))
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
    
    if(bridges("area")){
      roi_pg_df$is_bridge <- roi_pg_df$is_bridge | !(roi_pg_df$roi %in% valid_roi)
    }else{
      roi_fail_df <- dplyr::filter(roi_pg_df, !(roi %in% valid_roi)) %>%
        mutate(feature_id = paste0(fail_ROI_feature_prefix, "area")) %>%
        rbind(roi_fail_df, .)

      roi_pg_df <- dplyr::filter(roi_pg_df, (roi %in% valid_roi))
    }

    if(sum(!roi_pg_df$is_bridge) == 0){
      warning(.no_real_roi_msg("ROI Area", roi_pg_df))
      return(rbind(roi_fail_df, .bridges_as_failed(roi_pg_df, fail_ROI_feature_prefix)))
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
  
  # Built here, not with the z/area maps above: the pre-filters are what set
  # is_bridge, so a map made before them would be uniformly FALSE.
  roi_bridge_map <- dplyr::pull(roi_pg_df, is_bridge, name=roi)

  roi_node_df <- data.frame(roi_id = unique(c(ovl_pair_df$roi_1, ovl_pair_df$roi_2))) %>%
    as_tibble() %>%
    mutate(z = roi_z_map[roi_id],
           feature_group=NA, # place holder
           area = roi_area_map[roi_id],
           is_bridge = unname(roi_bridge_map[roi_id]))
  
  roi_edge_df <- ovl_pair_df %>%
    dplyr::select(roi_1, roi_2)
  roi_edge_mat <- as.matrix(roi_edge_df)
  
  # Assigning feature groups
  # NB: seq_len(), not 1:nrow(). find_ROI_z_intersect() legitimately returns a
  #     zero-row table when nothing overlaps across z (it only warns), and
  #     1:nrow() would then iterate over c(1, 0) and index rows that do not
  #     exist. With seq_len() the loop is simply skipped and every ROI falls
  #     through to the "overlap" fail bucket, which is the correct answer.
  # Edges are processed in two passes: ordinary ROIs first, then the bridges.
  #
  # Order matters because only the second pass is guarded. Running the seed
  # edges first means the ordinary components are fully formed before any
  # bridge is asked to join them, so the guard below is deciding about real
  # components rather than about whatever half-built fragments the edge order
  # happened to produce. It also keeps a run with no bridges byte-identical:
  # every edge is a seed edge and the loop is exactly what it was.
  edge_is_bridge <- rep(FALSE, nrow(roi_edge_mat))
  if(nrow(roi_edge_mat) > 0 && any(roi_node_df$is_bridge)){
    br <- roi_node_df$roi_id[roi_node_df$is_bridge]
    edge_is_bridge <- (roi_edge_mat[, 1] %in% br) | (roi_edge_mat[, 2] %in% br)
  }

  feature_count <- 0
  # `guard` is FALSE for the seed pass and TRUE for the bridge pass.
  for(guard in c(FALSE, TRUE)){
    rows <- which(edge_is_bridge == guard)
    # Sorted, so a refusal does not depend on the order find_ROI_z_intersect()
    # happened to return its pairs in.
    if(length(rows) > 1){
      rows <- rows[order(roi_edge_mat[rows, 1], roi_edge_mat[rows, 2])]
    }
    for(i in rows){
      cur_idx <- which(roi_node_df$roi_id %in% roi_edge_mat[i, ])
      # Check if any of these two already has group number assign to it
      cur_nuc_num <- unique(roi_node_df$feature_group[cur_idx]) %>% 
        subset(., !is.na(.))

      if(guard && .stacks_seeds(roi_node_df, cur_idx, cur_nuc_num)){
        # Refused: see .stacks_seeds(). The bridge keeps whatever group it has
        # already; nothing is dropped, the merge simply does not happen.
        next
      }

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
  }
  
  # Filtering feature -----------------------------------------------------------------------------
  # By number of z-span
  # Calculate number of z-span per nucleus
  # NB: counted with count(), not table(). as.data.frame(table(x)) on an empty
  #     column comes back with a single column, so the `colnames<-` below
  #     renamed nothing and the next filter failed with "object 'n_z_span' not
  #     found". That is reachable whenever no ROI overlaps any other, which
  #     find_ROI_z_intersect() reports by warning rather than by stopping.
  #     Counting after unique() gives distinct z per group either way.
  # NB: bridges are excluded here, which is most of what "bridge" means. A
  #     bridge ROI holds an object together across a gap but is not evidence
  #     that the object is there, so it must not inflate the z-span that
  #     min_z_span tests. A group made ONLY of bridges therefore drops out of
  #     this table entirely and can never be selected as valid below -- rejects
  #     cannot assemble themselves into a feature.
  z_span_df <- roi_node_df %>%
    dplyr::filter(!is.na(feature_group), !is_bridge) %>%
    dplyr::select(feature_group, z) %>%
    unique() %>% 
    dplyr::count(feature_group, name="n_z_span") %>% 
    mutate(feature_group = as.double(feature_group))
  
  valid_feature_group <- z_span_df %>% 
    dplyr::filter(n_z_span >= min_z_span) %>% 
    pull(feature_group)
  
  # Filter by average area
  #
  # Two ways in, and they mean the same thing: min_avg_area is the original
  # lower-bound-only form, feature_area_range is c(lo, hi). Both act on the
  # feature's MEAN ROI area, i.e. AFTER grouping -- unlike roi_area_range above,
  # which drops individual ROIs before the graph is built and can therefore
  # split one object into two by opening a z-gap. Prefer this one.
  area_range <- NULL
  if(!is.null(feature_area_range)){
    if(length(feature_area_range) != 2 || anyNA(feature_area_range)){
      stop("feature_area_range must be a length-2 numeric c(lo, hi)")
    }
    area_range <- c(min(feature_area_range), max(feature_area_range))
  }
  if(!is.null(min_avg_area)){
    # Both given: take the tighter lower bound rather than letting one win silently.
    area_range <- if(is.null(area_range)) c(min_avg_area, Inf)
                  else c(max(area_range[1], min_avg_area), area_range[2])
  }
  if(!is.null(area_range)){
    # Bridges excluded for the same reason as in the z-span above: a rejected
    # ROI is not a measurement of the object, so it must not move the mean the
    # area filter tests.
    nuc_stats_df <- roi_node_df %>%
      dplyr::filter(!is_bridge) %>%
      group_by(feature_group) %>%
      reframe(mean_area = mean(area))

    valid_feature_group_byArea <- nuc_stats_df %>%
      dplyr::filter(mean_area >= area_range[1], mean_area <= area_range[2]) %>%
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
  out_df <- left_join(roi_pg_df, dplyr::select(roi_node_df, roi=roi_id, feature_id), by="roi", suffix=c("", "_x"))
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

