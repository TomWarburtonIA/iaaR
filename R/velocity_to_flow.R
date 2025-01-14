#' Convert average fluid velocity (m / s) to volumetric flow rate (L / min)
#' 
#' A function to convert the flow rate `V` (m / s) of a fluid in a pipe of 
#' diameter `D` (m) into a velocity `Q` (L / min). Make sure you use the 
#' correct units!
#' 
#' @author Thomas Warburton
#' 
#' @param velocity A numeric giving the velocity of the fluid in units of 
#' metres per second (m / s)
#' 
#' @param pipe_diameter A numeric giving the diamteter of the pipe in units 
#' of metres (m)
#' 
#' @return A numeric of the calculated flow rate of the fluid in units of 
#' litres per minute (L / min).
#' 
#' @export
#' 
#' @examples
#' library(iaaR)
#' 
#' result <- v2f(15, 0.037)
#' print(result)

v2f <- function(velocity, pipe_diameter) {
  velocity_conv = (velocity * 1000) * 60
  csa = ((pipe_diameter/2)^2) * pi
  flow_rate = velocity_conv * csa
  
  return(flow_rate)
}