#' Replace values with NA based on Modified Z-Scores
#'
#' Be careful doing this - make sure you are being ethical with your data, and
#' only filter with this function if you have genuinely large outliers that are
#' skewing your data. Remember you can typically squash data together nicely in
#' a way that allows you to do more robust stats if you transform by logarithm.
#' Don't filter just because you want 'good' p-values as this isn't ethical
#' and can have disastrous consequences.
#'
#' @param df A data frame with numeric columns.
#' @param z_upper Numeric value for upper threshold. Default is 3.5.
#' @param z_lower Numeric value for lower threshold. Default is -3.5.
#' @param replace_zeros Logical indicating whether to replace zeros with NA. Default is TRUE.
#' @return A data frame with values replaced based on thresholds.
#' @export
z_filter <- function(df, z_upper = 3.5, z_lower = -3.5, replace_zeros = TRUE) {

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

  # Proceed with filtering if user confirms
  df %>%
    mutate(across(
      where(is.numeric) & !starts_with("modified_z_"),  # Select original columns
      ~ ifelse(get(paste0("modified_z_", cur_column())) > z_upper |
                 get(paste0("modified_z_", cur_column())) < z_lower |
                 (replace_zeros & . == 0),  # Replace 0 with NA if replace_zeros is TRUE
               NA,
               .)  # Keep the original value if conditions are not met
    ))
}
