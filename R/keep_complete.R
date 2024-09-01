#' Remove all columns with a single NA or 0 value
#' 
#' A function to tidy a data frame to keep only columns with a complete series
#' of observations. Will remove any columns which contain at least one NA or 
#' 0 value. Treats NaN and Inf as NA.
#' 
#' 
#' @author Thomas Warburton
#' 
#' @param df The data frame to have columns removed from
#' 
#' @return  The data frame originating from \code{df}, with the specified columns
#'          returned
#'          
#' @export
keep_complete <- function(df) {
  # Replace NaN and Inf values with NA
  df[] <- lapply(df, function(x) {
    x[is.nan(x) | is.infinite(x)] <- NA
    return(x)
  })
  
  # Replace 0 values with NA
  df[df == 0] <- NA
  
  # Remove columns with any NA values
  df <- df[, colSums(is.na(df)) == 0]
  
  return(df)
}