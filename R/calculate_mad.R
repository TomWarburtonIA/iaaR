#' Compute Median Absolute Deviation (MAD)
#'
#' @author Thomas Warburton
#' 
#' @param x A numeric vector.
#' 
#' @return A numeric value representing the MAD.
#' 
#' @export
compute_mad <- function(x) {
  mad(x, constant = 1, na.rm = TRUE)
}
