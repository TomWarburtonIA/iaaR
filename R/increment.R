#' Calculate the concentration increment from a steady emission rate.
#' 
#' This function calculates the concentration increment from a steady emission
#' rate. This is calculated according to a steady-state assumption, using a 
#' one-box model. This is only for an upper-bound estimation, as this model does
#' not account for chemical loss through deposition/surface sinking or through 
#' oxidation reactions. Chemical loss in this model is only through ventilation.
#' The intended use of this function is to calculate an in-room VOC increment 
#' through a point emission.
#' 
#' @author Thomas Warburton
#' 
#' @param emission_rate The emission rate of the VOC in grams per hour (g/hr)
#' 
#' @param volume A numeric of the room volume in metres cubed. Can be a vector 
#' of lower and upper volumes, in which case a volume step of 0.1 will be used
#' to produce the sequence.
#' 
#' @param volume_step A numeric used to indicate the steps between volume 
#' values when creating the volume sequence. Only to be used if using a vector
#' input to `volume`.
#' 
#' @param aer A numeric of the air exchange rate (AER) per hour. Can be a vector
#' of lower and upper AER, in which case an AER step of 0.01 will be used to 
#' produce the sequence.
#' 
#' @param aer_step A numeric used to indicate the steps between AER 
#' values when creating the AER sequence. Only to be used if using a vector
#' input to `aer`.
#' 
#' @return A data frame with the calculated VOC concentration increment, as well
#' as the volume and AER inputs. VOC increment is in micrograms per cubic metre.
#' 
#' @examples
#' library(iaaR)
#' 
#' df <- increment(emission_rate = 0.005, 
#' volume = 25, 
#' aer = 0.75)
#' 
#' print(df)
#' 
#' @examples
#' library(iaaR)
#' 
#' df_expanded <- increment(emission_rate = 0.005, 
#' volume = c(5,25), 
#' aer = c(0.1,0.75))
#' 
#' print(df_expanded)
#' 

increment <- function(emission_rate, volume, aer, volume_step = NULL, aer_step = NULL) {
  
  # Handle volume sequence generation
  if (length(volume) > 1) {  # Check if volume is a vector
    if (is.null(volume_step)) {
      volume_step <- 0.1  # Default step if not provided
    }
    volume_seq <- seq(volume[1], volume[2], by = volume_step)
  } else {
    volume_seq <- volume  # Single volume, no sequence
  }
  
  # Handle AER sequence generation
  if (length(aer) > 1) {  # Check if AER is a vector
    if (is.null(aer_step)) {
      aer_step <- 0.01  # Default step if not provided
    }
    aer_seq <- seq(aer[1], aer[2], by = aer_step)
  } else {
    aer_seq <- aer  # Single AER, no sequence
  }
  
  # Create a data frame with the combinations of volume and AER
  result_df <- expand.grid(volume = volume_seq, aer = aer_seq)
  
  # Calculate concentration increment using the formula provided
  result_df$concentration_increment <- (emission_rate / (result_df$volume * result_df$aer)) * 1e6
  
  return(result_df)
}

  
  
  
  
  