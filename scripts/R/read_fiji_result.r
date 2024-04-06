read_fiji_result <- function(res_path){
  # Read FIJI Measurement results
  res_df <- as_tibble(read.table(res_path, sep="\t", header=TRUE, stringsAsFactors=FALSE))
  # ignore the first column if it's just a row number
  first_col_check <- (unlist(res_df[ , 1]) == 1:nrow(res_df)) %>% all()
  if(first_col_check){
    res_df <- res_df[ , -1]
  }
  # Repair column names:
  res_df <- res_df %>% 
    dplyr::rename_all(.funs=function(x){tolower(str_remove(x, "\\.+$"))})
  
  # Extract information from the Label column -----------------------------------------------------
  unq_label <- unique(res_df$label)
  
  res_label_info <- str_split(unq_label, pattern="(\\/)|(\\s+)|(\\:)", simplify=TRUE) %>% 
    as.data.frame() %>% as_tibble()
  
  # Round 1: Assign column name based on the pattern of data
  # Making a temporary function --> NB: consider separating into a stand alone function
  colname_by_regex <- function(df, regex_list){
    # Assign column name based on the pattern of data
    m <- matrix(F, ncol=ncol(df), nrow=length(regex_list))
    colnames(m) <- colnames(df)
    rownames(m) <- names(regex_list)
    
    for(r in seq_along(regex_list)){
      cur_regex <- regex_list[[r]]
      
      # Checking individual column
      for(i in 1:ncol(df)){
        cur_data <- dplyr::pull(df, i)
        cur_data <- subset(cur_data, !is.na(cur_data)) # just in case
        if(length(cur_data) == 0){
          # i.e., all value are NA 
          m[r, i] <- FALSE
        }else{
          m[r, i] <- all(str_detect(cur_data, pattern=cur_regex))
        }
      }
      
      # Assign column name if match found
      match_col <- which(m[r, ])
      if(length(match_col)==1){
        # Ideal case
        colnames(df)[match_col] <- names(regex_list)[r]
        
      }else{
        # i.e., multiple matches found, this could lead to problem in the future.
        # TODO: Alternative solution: merging columns with exactly the same information together.
        # Give the suffix
        col_suffix <- paste0("_", (0:length(match_col)))
        col_suffix[1] <- ""
        colnames(df)[match_col] <- paste0(names(regex_list)[r], col_suffix)
      }
    }
    
    # Checking for potential problem ----
    conflict_col <- (colSums(m) > 1)
    if(any(conflict_col)){
      warning("Conflict column found")
    }
    
    conflict_row <- rowSums(m) > 1
    if(any(conflict_row)){
      warning("Conflict row found")
    }
    
    # Returning output
    return(df)
  } # End of function
  
  # The regex is for content within the table
  regex_list <- list(
    filename = "\\.lif$",
    roi = "\\d{4}\\-\\d{4}\\-\\d{4}$",
    pos = "^[Pp]osition\\d+$"
  )
  
  res_label_info <- colname_by_regex(res_label_info, regex_list)
  # Filtering out unused columns (R default column name pattern V[number])
  use_colnames <- colnames(res_label_info) %>% 
    subset(., !str_detect(., "^V\\d+$"))
  res_label_info <- res_label_info[ , use_colnames]
  
  # Round 2: Extract information directly from the Label
  regex_extract_list <- list(
    z = "(?<=z\\:)\\d+"#,
    # ch = "(?<=c\\:)\\d+"
  )
  
  for(i in seq_along(regex_extract_list)){
    res_label_info[[names(regex_extract_list)[i]]] <- str_extract(unq_label, regex_extract_list[[i]])
  }
  
  res_label_info$label <- unq_label
  
  # Join back to the main table and return as output
  res_df2 <- left_join(res_df, res_label_info, by="label")
  
  return(res_df2)
}

