#' Calculates pressure drop in circular tubing
#'
#' This function calculates the pressure drop in a circular tube for either 
#' laminar or turbulent flow. There is currently no logic for if a calculated
#' Reynolds number indicates the flow is in a laminar-turbulent transition
#' state (taken as having a Reynolds number between 2300 and 4000), but the 
#' function does display warnings for Reynolds numbers that indicate this 
#' transition. Laminar flow is calculated using the Hagen-Poiseuille equation,
#' while turbulent flow is calculated using the Darcy-Weisbach equation. Now 
#' with added vertical functionality, calculating vertical pressure gains or 
#' losses through calculation of wall shear stressed for both laminar and
#' turbulent flow. The function prompts for all necessary parameters 
#' interactively in the console.
#' 
#' @author Thomas Warburton
#'
#' @param None. The function prompts for user input interactively in the 
#' console.
#'
#' @return A list containing the following:
#'   - `flow_type`: The calculated flow type ('laminar' or 'turbulent').
#'   - `flow_orientation`: Either 'horizontal' or 'vertical'.
#'   - `flow_direction`: Whether vertical flow was 'upwards' or 'downwards'. 
#'   Only applicable for 'vertical' flow.
#'   - `reynolds_number`: The calculated Reynolds number.
#'   - `pressure_drop`: The calculated pressure drop in Pascals.
#' 
#' @export
#' @examples
#' result <- pressure_drop()
#' print(result$pressure_drop)
#' 
pressure_drop <- function() {
  flow_type <- readline(prompt = "Is the gas flow 'laminar' or 'turbulent'? ")
  flow_type <- tolower(trimws(flow_type))  # Normalize input
  if (!flow_type %in% c("laminar", "turbulent")) {
    stop("Invalid input. Please specify 'laminar' or 'turbulent'.")
  }
  
  # Gather common inputs
  density <- as.numeric(readline(prompt = "Enter gas density (kg/m³): "))
  velocity <- as.numeric(readline(prompt = "Enter flow velocity (m/s): "))
  diameter <- as.numeric(readline(prompt = "Enter pipe diameter (m): "))
  viscosity <- as.numeric(readline(prompt = "Enter dynamic viscosity (Pa·s): "))
  
  # Calculate Reynolds number
  reynolds_number <- (density * velocity * diameter) / viscosity
  
  message(paste0("Calculated reynolds number: ", reynolds_number))
  
  # Prompt the user for the type of flow (initial flow orientation)
  flow_orientation <- readline(prompt = "Is the flow 'horizontal' or 'vertical'? ")
  flow_orientation <- tolower(trimws(flow_orientation))  # Normalize input
  if (!flow_orientation %in% c("horizontal", "vertical")) {
    stop("Invalid input. Please specify 'horizontal' or 'vertical'.")
  }
  
  
  # Vertical flow calculation
  if (flow_orientation == "vertical") {
    height <- as.numeric(readline(prompt = "Enter vertical pipe height (m): "))
    if(flow_type == "laminar"){
      friction_factor = 64 / reynolds_number
    } else {
      if(flow_type == "turbulent") {
        friction_factor = 0.3164 / reynolds_number^(1 / 4) # Blasius correlation
      }
    }
    
    wall_shear_stress = (1 / 8) * friction_factor * density * velocity^2
    
    # Prompt the user for the vertical flow direction
    flow_direction <- readline(prompt = "Is the flow 'upwards' or 'downwards'? ")
    flow_direction <- tolower(trimws(flow_direction))  # Normalize input
    if (!flow_direction %in% c("upwards", "downwards")) {
      stop("Invalid input. Please specify 'upwards' or 'downwards'.")
    }
    
    if (flow_direction == "upwards") {
      pressure_drop <- (((4 * height) / diameter) * wall_shear_stress) - (density * 9.81 * height)
    } else if (flow_direction == "downwards") {
      pressure_drop <- (((4 * height) / diameter) * wall_shear_stress) + (density * 9.81 * height)
    }
    
    
    return(list(
      flow_type = flow_type,
      flow_orientation = flow_orientation,
      flow_direction = flow_direction,
      pressure_drop = pressure_drop
    ))
  }
  
  # For horizontal flow, proceed with DW/HP calculations
  # Handle transition flow range
  if (reynolds_number > 2300 && reynolds_number < 4000) {
    calc_type <- readline(prompt = "Warning: Reynolds number suggests the flow is in laminar-turbulent transition. Calculate for 'laminar' or 'turbulent' flow? ")
    calc_type <- tolower(trimws(calc_type))  # Normalize input
    if (!calc_type %in% c("laminar", "turbulent")) {
      stop("Invalid input. Please specify 'laminar' or 'turbulent'.")
    }
  } else {
    calc_type <- flow_type  # If not in transition, use the originally selected flow type
  }
  
  # Forcing parameter: specify which equation to use
  forcing_param <- readline(prompt = "Force calculation with 'DW' (Darcy-Weisbach) or 'HP' (Hagen-Poiseuille) equation?\nLeave empty for automatic selection: ")
  forcing_param <- tolower(trimws(forcing_param))
  
  # Common input for length
  length <- as.numeric(readline(prompt = "Enter horizontal pipe length (m): "))
  
  # Flow-specific calculations
  if (forcing_param == "hp" || calc_type == "laminar") {
    # Hagen-Poiseuille equation for laminar flow
    if (reynolds_number >= 2300) {
      message("Warning: Reynolds number suggests the flow may not be laminar.")
    }
    pressure_drop <- (8 * viscosity * length * ((((diameter/2)^2)*pi)*(velocity)) / (pi * ((diameter / 2)^4)))
  } else if (forcing_param == "dw" || calc_type == "turbulent") {
    # Darcy-Weisbach equation for turbulent flow
    if (reynolds_number < 4000) {
      message("Warning: Reynolds number suggests the flow may not be turbulent.")
    }
    # Approximate friction factor for turbulent flow
    friction_factor <- 0.3164 / reynolds_number^(1 / 4)  # Blasius correlation
    pressure_drop <- (friction_factor * length * density * velocity^2) / (2 * diameter)
  }
  
  # Return results for horizontal flow
  return(list(
    flow_orientation = flow_orientation,
    flow_type = calc_type,
    reynolds_number = reynolds_number,
    pressure_drop = pressure_drop
  ))
}
