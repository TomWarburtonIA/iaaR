#' Calculate the internal volume in a tube of set length
#' 
#' This function is intended to be used to calculate the internal volumes of 
#' tubing, for example to aid in the building of loop injectors with specific
#' volume requirements.
#' 
#' @author Thomas Warburton
#' 
#' @param id A numeric giving the internal diameter (ID) of the tube being used.
#' 
#' @param length A numeric giving the length of tubing being used.
#' 
#' @return A numeric of the calculated tube volume.
#' 
#' @export

tube_vol <- function(id, length) {
  csa <- pi*(id/2)^2
  vol = length * csa
  
  return(vol)
}

