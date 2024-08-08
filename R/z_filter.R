#' Replace values with NA based on Modified Z-Scores
#'
#' @param df A data frame with numeric columns.
#' @param z_upper Numeric value for upper threshold. Default is 3.5.
#' @param z_lower Numeric value for lower threshold. Default is -3.5.
#' @param replace_zeros Logical indicating whether to replace zeros with NA. Default is TRUE.
#' @return A data frame with values replaced based on thresholds.
#' @export
z_filter <- function(df, z_threshold_upper = 3.5, z_threshold_lower = -3.5, replace_zeros = TRUE) {
  df %>%
    mutate(across(
      where(is.numeric) & !starts_with("modified_z_"),  # Select original columns
      ~ ifelse(get(paste0("modified_z_", cur_column())) > z_threshold_upper |
                 get(paste0("modified_z_", cur_column())) < z_threshold_lower |
                 (replace_zeros & . == 0),  # Replace 0 with NA if replace_zeros is TRUE
               NA,
               .)  # Keep the original value if conditions are not met
    ))
}