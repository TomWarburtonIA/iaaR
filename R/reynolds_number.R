#' Calculates Reynold's number for flow in a circular tube
#'
#' This function calculates the Reynold's number for a fluid flowing through a 
#' circular tube. The function prompts for all necessary parameters 
#' interactively in the console.
#' 
#' @author Thomas Warburton
#'
#' @param None. The function prompts for user input interactively in the 
#' console.
#'
#' @return A numeric of the calculated Reynold's number.
#' 
#' @export
#' @examples
#' result <- reynolds_number()
#' print(result)
#' 
reynolds_number <- function() {
  
  # Gather common inputs
  density <- as.numeric(readline(prompt = "Enter gas density (kg/m³): "))
  velocity <- as.numeric(readline(prompt = "Enter flow velocity (m/s): "))
  diameter <- as.numeric(readline(prompt = "Enter pipe diameter (m): "))
  viscosity <- as.numeric(readline(prompt = "Enter dynamic viscosity (Pa·s): "))
  
  # Calculate Reynolds number
  reynolds_number <- (density * velocity * diameter) / viscosity
  
  return(reynolds_number)
}
