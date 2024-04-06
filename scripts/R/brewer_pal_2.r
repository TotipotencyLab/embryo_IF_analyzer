brewer_pal_2 <- function(n, pal_name="Paired", max_repeat=10){
  # Allow recycling the color return by brewer.pal() function
  pallette_info <- RColorBrewer::brewer.pal.info %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "name") %>% 
    as_tibble() %>% 
    dplyr::filter(name == pal_name)
  
  # Recycling the color if number of feature is too large  
  col_vec <- c() # place holder for an output
  n_remain <- n
  n_repeat <- 1
  while((n > 0) & (n_repeat <= max_repeat)){
    n_repeat = n_repeat +1
    cur_n_color <- min(n, pallette_info$maxcolors)
    tmp_vec <- suppressWarnings(RColorBrewer::brewer.pal(n=cur_n_color, pallette_info$name[1]))
    if(length(tmp_vec) > cur_n_color){
      # brewer.pal will always return a minimum number of color
      tmp_vec <- tmp_vec[1:cur_n_color]
    }
    col_vec <- c(col_vec, tmp_vec)
    n_remain <- n - cur_n_color
  }
  
  return(col_vec)
}
