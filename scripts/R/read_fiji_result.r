read_fiji_result <- function(res_path){
  # Read FIJI Measurement results
  res_df <- as_tibble(read.table(res_path, sep="\t", header=TRUE, stringsAsFactors=FALSE))
  
  if(nrow(res_df) == 0){
    warning("Measurement table is empty: ", res_path)
    return(res_df)
  }
  
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
      cur_name <- names(regex_list)[r]
      
      # Checking individual column
      for(i in 1:ncol(df)){
        cur_data <- dplyr::pull(df, i)
        cur_data <- subset(cur_data, !is.na(cur_data)) # just in case
        # NB: the "" cells that str_split(simplify=TRUE) uses as right-padding
        #     are deliberately NOT dropped here. Dropping them lets a pattern
        #     match a column that is only partly populated, which yields a
        #     column of half "" values -- silently wrong data, which is worse
        #     than no column at all. Labels that split into different numbers of
        #     tokens instead fall through to the "no column matched" warning
        #     below, which is the honest answer: this matching is positional and
        #     cannot align labels of differing depth.
        if(length(cur_data) == 0){
          # i.e., all value are NA
          m[r, i] <- FALSE
        }else{
          m[r, i] <- all(str_detect(cur_data, pattern=cur_regex))
        }
      }
      
      # Assign column name if match found
      match_col <- which(m[r, ])
      if(length(match_col) == 0){
        # NB: assigning to a zero-length index is a silent no-op in R, so this
        #     case used to leave the column simply absent. Everything downstream
        #     then carried on without it -- which is how a label-format change
        #     can drop a field with nothing reported anywhere. Warn loudly.
        warning("No column matched the expected pattern for '", cur_name, 
                "'; that field will be missing from the output")
        
      }else if(length(match_col) == 1){
        # Ideal case
        colnames(df)[match_col] <- cur_name
        
      }else{
        # i.e., multiple matches found, this could lead to problem in the future.
        # TODO: Alternative solution: merging columns with exactly the same information together.
        # Give the suffix
        # NB: one suffix per matched column. The previous 0:length(match_col)
        #     produced one element too many and tripped the recycling warning
        #     "number of items to replace is not a multiple of replacement length".
        col_suffix <- paste0("_", seq_along(match_col))
        col_suffix[1] <- ""
        colnames(df)[match_col] <- paste0(cur_name, col_suffix)
      }
    }
    
    # Checking for potential problem ----
    # NB: these two messages used to be swapped.
    conflict_col <- (colSums(m) > 1)
    if(any(conflict_col)){
      warning("One label column matched more than one pattern: ", 
              paste(colnames(m)[conflict_col], collapse=", "))
    }
    
    conflict_row <- (rowSums(m) > 1)
    if(any(conflict_row)){
      warning("One pattern matched more than one label column: ", 
              paste(rownames(m)[conflict_row], collapse=", "))
    }
    
    # Returning output
    return(df)
  } # End of function
  
  # The regex is for content within the table
  # NB: `filename` must not be anchored to ".lif$". Fiji labels the image with the
  #     window title, which for a series pulled out of a .lif reads
  #     "<file>.lif-Position010-1.tif" -- the extension is in the middle. Anchoring
  #     to the end matched nothing and (see above) failed silently.
  regex_list <- list(
    filename = "\\.(lif|lifext|tif|tiff|czi|nd2)\\b",
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
  
  # Fields that must come back as numbers rather than text
  numeric_field <- c("z")
  
  for(i in seq_along(regex_extract_list)){
    cur_name <- names(regex_extract_list)[i]
    cur_value <- str_extract(unq_label, regex_extract_list[[i]])
    # NB: z is a slice index and has to be numeric. As character, abs(z1-z2) in
    #     find_ROI_z_intersect() fails outright ("non-numeric argument to binary
    #     operator") and sort() puts "10" before "2".
    if(cur_name %in% numeric_field){
      cur_value <- as.numeric(cur_value)
    }
    res_label_info[[cur_name]] <- cur_value
  }
  
  res_label_info$label <- unq_label
  
  # Join back to the main table and return as output
  res_df2 <- left_join(res_df, res_label_info, by="label")
  
  return(res_df2)
}
