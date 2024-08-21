#' Calculate the volume required to dilute a calibration mix to a required final
#' fraction.
#'
#' This should be used in the context of creating a target gas at a specified 
#' mixing fraction (ppb/ppm). The function will return a volume (or vector of
#' volumes if a vector of fractions was given) required in the cylinder volume
#' specified diluted to the specified final pressure. Use common units between
#' the two fraction parameters. Please use 'bar' as the pressure units.
#'
#' @author Thomas Warburton
#' 
#' @param cylinder_volume The volume of the cylinder the dilutions will be done in.
#'                        Must be a numeric value.
#' 
#' @param cal_fraction The fraction of the calibration mix being used for the 
#'                     dilution. Must be a numeric value.
#' 
#' @param fill_pressure The pressure of the final target gas mix, in units of bar 
#'                      (gauge). Must be a numeric value.
#'                      
#' @param target_fraction The required fraction of the final target gas. Can be a 
#'                        single numeric value or a vector of numeric values.
#'
#' @param ambient_pressure The ambient pressure of the cylinder. Default value 
#'                         of 1 bar.
#' 
#' @return A data frame with the target_fraction, fill_pressure, cal_fraction, 
#'         and the calculated dilution_volume.
#' 
#' @export
#' 
#' @examples
#' library(iaaR)
#' 
#' fractions=c(0.1, 0.2, 0.3, 0.4, 0.5)
#' 
#' df <- dilution_volumes(cylinder_volume=45, 
#'                        fill_pressure=100,
#'                        cal_fraction=4,
#'                        target_fraction=fractions)
#' 
#' 
dilution_volumes <- function(cylinder_volume,
                             cal_fraction,
                             fill_pressure,
                             target_fraction,
                             ambient_pressure=1) {
  
  # Check that all parameters are numeric
  if (!is.numeric(cylinder_volume) || length(cylinder_volume) != 1) {
    stop("cylinder_volume must be a single numeric value.")
  }
  
  if (!is.numeric(cal_fraction) || length(cal_fraction) != 1) {
    stop("cal_fraction must be a single numeric value.")
  }
  
  if (!is.numeric(fill_pressure) || length(fill_pressure) != 1) {
    stop("fill_pressure must be a single numeric value.")
  }
  
  if (!is.numeric(target_fraction)) {
    stop("target_fraction must be a numeric value or vector.")
  }
  
  # Calculate the dilution volume
  fill_volume = cylinder_volume * (fill_pressure + ambient_pressure)
  
  dilution_volume = fill_volume * (target_fraction / cal_fraction) 
  
  # Create the result data frame
  result <- data.frame(
    target_fraction = target_fraction,
    fill_pressure = fill_pressure,
    cal_fraction = cal_fraction,
    dilution_volume = dilution_volume
  )
  
  return(result)
}
