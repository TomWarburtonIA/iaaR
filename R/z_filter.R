#' Replace values with NA based on Modified Z-Scores. For use after running \strong{calculate_mod_z}, also from the iaaR package
#'
#' Be careful doing this - make sure you are being ethical with your data, and
#' only filter with this function if you have genuinely large outliers that are
#' skewing your data. Remember you can typically squash data together nicely in
#' a way that allows you to do more robust stats if you transform by logarithm.
#' Don't filter just because you want 'good' p-values as this isn't ethical
#' and can have disastrous consequences.
#'
#' If you have separate data frames for the original values and the corresponding
#' modified Z-scores, make sure they are using the same column heads, as if not the
#' code will not work.
#'
#' @param df A data frame with either the original values and modified Z-scores within, or the original values without modified Z-scores.
#' @param df_z A data frame with modified Z-scores corresponding to the values in 'df'. Must have the same column names as 'df'. Default is NULL (use a data frame with both values and modified Z-scores as 'df' for the easiest option).
#' @param z_upper Numeric value for upper threshold. Default is 3.5.
#' @param z_lower Numeric value for lower threshold. Default is -3.5.
#' @param return_tidy Logical indicating whether to remove modified Z-score columns from the returned data frame. Default is TRUE.
#' @return A data frame with values replaced based on thresholds, optionally excluding modified Z-score columns.
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
  confirm <- prompt_user("You're sure you want to filter? You can transform datasets more robustly by logarithm transformation.
Willful removal of outliers based on a computed value isn't always the right thing to do. Be sure.")
  if (confirm != "Y") {
    cat("Operation cancelled. The data have not been filtered.\n")
    return(df)  # Return the original data frame without modifications
  }

  # Convert column names to a consistent format for processing
  original_colnames <- names(df)
  df_colnames <- make.names(original_colnames, unique = TRUE)
  colnames(df) <- df_colnames

  # Initialize a list to keep track of which columns are modified Z-score columns
  mod_z_cols <- grep("^modified_z_", df_colnames, value = TRUE)

  # Process each numeric column that is not a Modified Z-Score column
  for (col_name in original_colnames) {
    if (is.numeric(df[[col_name]]) && !grepl("^modified_z_", col_name)) {
      mod_z_col <- paste0("modified_z_", make.names(col_name, unique = TRUE))

      if (mod_z_col %in% df_colnames) {
        df[[col_name]] <- ifelse(df[[mod_z_col]] > z_upper |
                                   df[[mod_z_col]] < z_lower,
                                 NA,
                                 df[[col_name]])
      } else {
        warning(paste("Modified Z-score column", mod_z_col, "not found for", col_name))
      }
    }
  }

  # Restore original column names
  colnames(df) <- original_colnames

  # Optionally remove Modified Z-Score columns
  if (return_tidy) {
    df <- df[ , !colnames(df) %in% mod_z_cols]
  }

  return(df)
}
