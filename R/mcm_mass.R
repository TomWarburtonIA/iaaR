#' Extract molecular mass from Master Chemical Mechanism (MCM)
#' 
#' The MCM is a near-explicit mechanism for the degradation of atmospheric 
#' pollutants to the final oxidation products of carbon dioxide and water.
#' This function will extract the molecular mass for the named species from 
#' the MCM website.
#' 
#' @author Thomas Warburton
#' 
#' @param species_name A character variable for the name of the primary 
#' pollutant to extract molecular mass for. MUST follow the same naming
#' mechanism as used in the MCM.
#' 
#' @return A numeric of the scraped molecular mass for the named species.
#' 
#' @export

mcm_mass <- function(species_name) {
  
  # Load valid species names from the package data
  valid_species_names <- get("valid_species_names", envir = asNamespace("iaaR"))
  
  # Helper function to check if the species name is valid
  is_valid_species <- function(name) {
    return(name %in% valid_species_names)
  }
  
  # Check if the species_name is valid
  if (!is_valid_species(species_name)) {
    stop("Invalid species name. Please choose from the names within /data/valid_species_names.rda")
  }
  
  # Read the HTML content directly from the species page
  html_content <- readLines(paste0("https://mcm.york.ac.uk/MCM/species/", species_name))
  
  mass_line <- grep("Mass:", html_content, value = TRUE)
  
  mass_value <- as.numeric(gsub("[^0-9.]", "", mass_line))
  
  return(mass_value)
}
