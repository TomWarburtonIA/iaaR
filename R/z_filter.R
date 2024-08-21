#' Replace values with NA based on Modified Z-Scores
#'
#' This function replaces values in a data frame with NA based on thresholds
#' of modified Z-scores. It is important to ensure that filtering is done
#' ethically and only when justified.
#'
#' @param df A data frame with numeric columns.
#' @param z_upper Numeric value for the upper threshold of modified Z-scores. Default is 3.5.
#' @param z_lower Numeric value for the lower threshold of modified Z-scores. Default is -3.5.
#' @param return_tidy Logical indicating whether to remove modified Z-score columns from the returned data frame. Default is TRUE.
#' @return A data frame with outliers replaced by NA, optionally excluding modified Z-score columns.
#' @export
z_filter <- function(df, z_upper = 3.5, z_lower = -3.5, return_tidy = TRUE) {
  
  # Function to prompt the user for confirmation
  prompt_user <- function(message, default = "N") {
    cat(message, " (Y/N): ")
    response <- readline()
    if (response == "") response <- default
    return(toupper(response))
  }
  
  # Ask the user for confirmation before proceeding
  confirm <- prompt_user("You're about to filter data based on modified Z-scores. This can affect the integrity of your analysis. Proceed? (Y/N)")
  if (confirm != "Y") {
    cat("Operation cancelled. The data have not been filtered.\n")
    return(df)  # Return the original data frame without modifications
  }
  
  # Identify modified Z-score columns
  mod_z_cols <- grep("^modified_z_", names(df), value = TRUE)
  print(paste("Modified Z-score columns found:", paste(mod_z_cols, collapse = ", ")))
  
  # Process each numeric column that has a corresponding Modified Z-Score column
  for (mod_z_col in mod_z_cols) {
    # Extract the original column name by removing the prefix
    original_col_name <- sub("^modified_z_", "", mod_z_col)
    
    if (original_col_name %in% names(df)) {
      cat("Processing column:", original_col_name, "\n")
      
      # Replace outliers with NA based on Z-score thresholds
      df[[original_col_name]] <- ifelse(
        !is.na(df[[mod_z_col]]) & is.finite(df[[mod_z_col]]) &
          (df[[mod_z_col]] > z_upper | df[[mod_z_col]] < z_lower),
        NA,
        df[[original_col_name]]
      )
    } else {
      warning(paste("Original column not found for modified Z-score column", mod_z_col))
    }
  }
  
  # Optionally remove Modified Z-Score columns
  if (return_tidy) {
    df <- df[ , !names(df) %in% mod_z_cols]
  }
  
  return(df)
}
