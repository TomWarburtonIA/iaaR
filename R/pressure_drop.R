#' Calculates pressure drop in circular tubing
#'
#' This function calculates the pressure drop in a circular tube for either 
#' laminar or turbulent flow. There is currently no logic for if a calculated
#' Reynolds number indicates the flow is in a laminar-turbulent transition
#' state (taken as having a Reynolds number between 2300 and 4000), but the 
#' function does display warnings for Reynolds numbers that indicate this 
#' transition. Laminar flow is calculated using the Hagen-Poiseuille equation,
#' while turbulent flow is calculated using the Darcy-Weisbach equation. The 
#' function prompts for all necessary parameters interactively in the console.
#' 
#' @author Thomas Warburton
#'
#' @param None. The function prompts for user input interactively in the 
#' console.
#'
#' @return A list containing:
#'   - `flow_type`: The calculated flow type ('laminar' or 'turbulent').
#'   - `reynolds_number`: The calculated Reynolds number.
#'   - `pressure_drop`: The calculated pressure drop in Pascals.
#' 
#' @export
#' @examples
#' result <- pressure_drop()
#' print(result)
#' 
pressure_drop <- function() {
  # Prompt the user for the type of flow (initial flow type selection)
  flow_type <- readline(prompt = "Is the gas flow 'laminar' or 'turbulent'? ")
  
  # Validate input for flow_type
  flow_type <- tolower(trimws(flow_type))  # Normalize input
  if (!flow_type %in% c("laminar", "turbulent")) {
    stop("Invalid input. Please specify 'laminar' or 'turbulent'.")
  }
  
  # Gather common inputs
  velocity <- as.numeric(readline(prompt = "Enter flow velocity (m/s): "))
  diameter <- as.numeric(readline(prompt = "Enter pipe diameter (m): "))
  density <- as.numeric(readline(prompt = "Enter gas density (kg/m³): "))
  viscosity <- as.numeric(readline(prompt = "Enter dynamic viscosity (Pa·s): "))
  
  # Calculate Reynolds number
  reynolds_number <- (density * velocity * diameter) / viscosity
  
  # Handle transition flow range
  if (reynolds_number > 2300 && reynolds_number < 4000) {
    calc_type <- readline(prompt = "Warning: Reynolds number suggests the flow is in laminar-turbulent transition. Calculate for 'laminar' or 'turbulent' flow? ")
    
    # Validate input for calc_type
    calc_type <- tolower(trimws(calc_type))  # Normalize input
    if (!calc_type %in% c("laminar", "turbulent")) {
      stop("Invalid input. Please specify 'laminar' or 'turbulent'.")
    }
  } else {
    calc_type <- flow_type  # If not in transition, use the originally selected flow type
  }
  
  # Forcing parameter: specify which equation to use
  forcing_param <- readline(prompt = "Force calculation with 'DW' (Darcy-Weisbach) or 'HP' (Hagen-Poiseuille) equation?\nLeave empty for automatic selection:")
  forcing_param <- tolower(trimws(forcing_param))
  
  # Flow-specific calculations
  if (forcing_param == "hp" || calc_type == "laminar") {
    # Hagen-Poiseuille equation for laminar flow
    if (reynolds_number >= 2300) {
      message("Warning: Reynolds number suggests the flow may not be laminar.")
    }
    # Hagen-Poiseuille equation for laminar flow
    length <- as.numeric(readline(prompt = "Enter pipe length (m): "))
    pressure_drop <- (8 * viscosity * velocity * length) / (pi * (diameter/2)^4)
    
  } else if (forcing_param == "dw" || calc_type == "turbulent") {
    # Darcy-Weisbach equation for turbulent flow
    if (reynolds_number < 4000) {
      message("Warning: Reynolds number suggests the flow may not be turbulent.")
    }
    # Approximate friction factor for turbulent flow, then calculate by Darcy-Weisbach equation
    friction_factor <- 0.3164 / reynolds_number^(1/4)  # Blasius correlation
    length <- as.numeric(readline(prompt = "Enter pipe length (m): "))
    pressure_drop <- (friction_factor * length * density * velocity^2) / (2 * diameter)
  }
  
  # Return results as a list
  return(list(flow_type = calc_type, reynolds_number = reynolds_number, pressure_drop = pressure_drop))
}
