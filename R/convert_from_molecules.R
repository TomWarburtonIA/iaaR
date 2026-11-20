#' Function to convert from molecules per centimetre cubed
#' 
#' This function will convert from molecules per centimetre cubed to either ppb
#' or micrograms per metre cubed. The function will attempt to gather inputs
#' from the user interactively if the parameter `conv_type` is empty.
#' 
#' @author Thomas Warburton
#' 
#' @param mol_cm3 Numeric number of molecules per centimetre cubed.
#' 
#' @param molar_mass Numeric molar mass of the species (in g/mol).
#' 
#' @param conv_type Only "ppb" or "ugm3" arguments allowed.
#' 
#' @param temp_k Numeric temperature in Kelvin. Assumed 298.15 K if left empty.
#' 
#' @param pressure_Pa Numeric pressure in Pascals. Assumed 101325 Pa if left 
#'                    empty.
#'                    
#' @export
#' 
#' @examples
#' library(iaaR)
#' 
#' convert_from_molecules(mol_cm3 = 12913923195, conv_type = "ppb")
#' 
#' @examples
#' library(iaaR)
#' 
#' convert_from_molecules(cm_cmn3 = 12913923195, temp_K = 273.15, conv_type="ugm3")

convert_from_molecules <- function(mol_cm3,
                                   molar_mass  = NULL,   
                                   conv_type   = NULL,   
                                   temp_K      = 298.15, 
                                   pressure_Pa = 101325)
  
{
  avogadro <- 6.02214076e23     # molecules per mol
  R        <- 8.314462618       # J / (mol K)
  
  if (is.null(conv_type)) {
    if (!interactive()) {
      stop("No conversion type given. Use conv_type = \"ppb\" or \"ugm3\".")
    }
    choice <- readline("No conversion type given. Convert to ppb (1), or micrograms per metre cubed (2)?: ")
    conv_type <- switch(trimws(choice),
                        "1" = "ppb",
                        "2" = "ugm3",
                        stop("Conversion type provided forbidden. Only input 1 for ppb, or 2 for micrograms per metre cubed."))
  }
  
  conv_type <- tolower(conv_type)
  
  if (conv_type == "ppb") {
    molar_vol_cm3 <- R * temp_K / pressure_Pa * 1e6
    result <- (mol_cm3 / avogadro) * molar_vol_cm3 * 1e9
    return(result)
    
  } else if (conv_type == "ugm3") {
    if (is.null(molar_mass)) {
      stop("molar_mass (g/mol) is required for conversion to ugm3.")
    }
    result <- (mol_cm3 / avogadro) * molar_mass * 1e12
    return(result)
    
  } else {
    stop("Conversion type provided forbidden. Only character inputs `ppb` or `ugm3` allowed.")
  }
}