#' Compute Modified Z-Scores, and add columns to the end of existing dataframe
#'
#' @param df A data frame with numeric columns.
#' @return A data frame with modified Z-scores added as new columns.
#' @export
compute_mod_z <- function(df) {
  df %>%
    mutate(across(where(is.numeric),
                  ~ (0.6745 * (. - median(., na.rm = TRUE)) / modified_z(.)),
                  .names = "modified_z_{.col}")) %>%
    ungroup()
}