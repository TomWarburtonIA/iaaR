#' Convert volumetric flow rate (L / min) to velocity (m / s)
#' 
#' A function to convert the flow rate `Q` (L / min) of a fluid in a pipe of 
#' diameter `D` (m) into a velocity `V` (m / s). Make sure you use the correct
#' units!
#' 
#' @author Thomas Warburton
#' 
#' @param flow_rate A numeric giving the flow rate of the fluid in units of 
#' litres per minute (L / min)
#' 
#' @param pipe_diameter A numeric giving the diamteter of the pipe in units 
#' of metres (m)
#' 
#' @return A numeric of the calculated velocity of the fluid.
#' 
#' @export
#' 
#' @examples
#' library(iaaR)
#' 
#' result <- f2v(90, 0.037)
#' print(result)

f2v <- function(flow_rate, pipe_diameter) {
  flow_rate_conv = (flow_rate/1000) / 60
  csa = ((pipe_diameter/2)^2) * pi
  velocity = flow_rate_conv / csa
  
  return(velocity)
}