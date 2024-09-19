#' Compute Modified Z-Scores for numeric columns and add them as new columns or return them separately.
#'
#' Modified Z-scores are calculated using the Median Absolute Deviation (MAD), aiming to handle 
#' non-parametric distributions within data. 
#'
#' @author Thomas Warburton
#'
#' @param df A data frame with columns to be processed.
#' 
#' @param non_numeric Either a vector of column names or indices that are considered non-numeric, 
#' or the string "NONE" to indicate no columns should be excluded.
#' 
#' @param return_type Character value indicating how to return the modified Z-scores. Options are 
#' "add_columns" or "separate_df". Default is "add_columns".
#' 
#' @param zero_remove Logical indicating whether to replace zeros with NA before Z-Score calculation. 
#' Default is TRUE.
#' 
#' @return A data frame with modified Z-scores added to the original data frame or a separate data 
#' frame with only the modified Z-scores.
#' 
#' @export
#' 
calculate_mod_z <- function(df, non_numeric = "NONE", return_type = "add_columns", zero_remove = TRUE) {
  
  # Helper function to calculate the median absolute deviation (MAD)
  calculate_mad <- function(x) {
    x <- na.omit(x)
    if (length(x) == 0) {
      warning("Input vector contains only NA values or is empty. MAD will be NA.")
      return(NA)
    }
    median_abs_deviation <- median(abs(x - median(x, na.rm = TRUE)), na.rm = TRUE)
    return(median_abs_deviation)
  }
  
  # Validate 'non_numeric' parameter
  if (!is.character(non_numeric) && !is.numeric(non_numeric) && non_numeric != "NONE") {
    stop("The 'non_numeric' parameter must be a vector of column names, indices, or the string 'NONE'.")
  }
  
  if (is.character(non_numeric) && non_numeric != "NONE") {
    non_numeric_cols <- non_numeric
  } else if (is.numeric(non_numeric)) {
    if (any(non_numeric <= 0) || any(non_numeric > ncol(df))) {
      stop("Column indices in 'non_numeric' must be within the range of the data frame columns.")
    }
    non_numeric_cols <- names(df)[non_numeric]
  } else {
    non_numeric_cols <- character()
  }
  
  # Identify columns to process
  all_cols <- names(df)
  numeric_cols <- setdiff(all_cols, non_numeric_cols)
  
  # Convert columns not in 'non_numeric' to numeric
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
    as.numeric(as.character(x))
  })
  
  # Initialize list to store modified Z-scores
  mod_z_list <- list()
  
  # Calculate modified Z-scores for numeric columns
  for (col_name in numeric_cols) {
    column_data <- df[[col_name]]
    
    if (zero_remove) {
      column_data[column_data == 0] <- NA
    }
    
    median_value <- median(column_data, na.rm = TRUE)
    mad_value <- calculate_mad(column_data)
    
    if (is.na(mad_value) || mad_value == 0 || all(is.na(column_data))) {
      warning(paste("MAD is zero or column contains all NAs for column", col_name, "- modified Z-scores will be NA."))
      mod_z <- rep(NA, nrow(df))
    } else {
      mod_z <- 0.6745 * (column_data - median_value) / mad_value
    }
    
    # Add the modified Z-scores to the list with the original column name prefixed
    mod_z_list[[paste0("modified_z_", col_name)]] <- mod_z
  }
  
  # Combine modified Z-scores with the original data frame
  if (return_type == "separate_df") {
    # Create a separate data frame with only the modified Z-scores
    mod_z_df <- as.data.frame(mod_z_list, check.names = FALSE)
    non_numeric_df <- df[ , non_numeric_cols, drop = FALSE]
    result_df <- cbind(non_numeric_df, mod_z_df)
    
    return(result_df)
  } else if (return_type == "add_columns") {
    # Add modified Z-scores as new columns to the original data frame
    df_with_mod_z <- cbind(df, as.data.frame(mod_z_list, check.names = FALSE))
    
    return(df_with_mod_z)
  } else {
    stop("Invalid 'return_type' parameter. Choose either 'add_columns' or 'separate_df'.")
  }
}
