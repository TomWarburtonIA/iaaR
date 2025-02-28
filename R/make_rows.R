#' Elongate a dataframe by adding NA rows at the end
#' 
#' This function takes a data frame and adds in a set number of NA rows. Mainly 
#' for creating data frames of equal length.
#' 
#' @author Thomas Warburton
#' 
#' @param df The data frame to be made longer.
#' 
#' @param number_of_total_rows A numeric with the number of rows to extend `df` 
#' up to.
#' 
#' @return `df` with additional NA rows added to be made equal to 
#' `number_of_total_rows`.
#' 
#' @export

make_rows <- function(df, number_of_total_rows) {
  nrow_df <- nrow(df)
  n = number_of_rows - nrow_df
  if (n < 0) {
    warning("Numer of rows provided is less than nrow(df), enter a number of rows larger than nrow(df). Returning original df.")
    return(df)
  }
  if(n==0){
    warning("There is no difference in row number, returning original data frame.")
    return(df)
  } else {
    df2 <- data.frame(matrix(ncol = ncol(df), nrow = n))
    colnames(df2) <- colnames(df)
    df_new <- rbind(df, df2)
    return(df_new)
  }
}
