#' Calculate the length of tube for a required volume
#' 
#' This function is intended to be used to calculate the length of tube/pipe 
#' required for a set internal volume. Generally for the creation of a measured
#' target gas, or to create a gas spiking volume.
#' 
#' @author Thomas Warburton
#' 
#' @param id A numeric giving the internal diameter (ID) of the tube being used.
#' 
#' @param vol A numeric giving the required internal volume.
#' 
#' @return A numeric of the calculated tube length.
#' 
#' @export

tube_length <- function(id, vol, units) {
  csa <- pi*(id/2)^2
  length <- vol/csa
  
  return(length)
}

