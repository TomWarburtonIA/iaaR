#' Extract reaction rates from Master Chemical Mechanism (MCM)
#' 
#' The MCM is a near-explicit mechanism for the degradation of atmospheric 
#' pollutants to the final oxidation products of carbon dioxide and water.
#' This function will extract the primary reaction rates for the reaction of
#' the gas-phase reactant with an oxidant. This function does not include
#' precursor reaction rates (rates for the formation of the species in 
#' question). Essentially, this function is a data scraper.
#' 
#' @author Thomas Warburton
#' 
#' @param species_name A character variable for the name of the primary 
#' pollutant to extract reaction rates for. MUST follow the same naming
#' mechanism as used in the MCM.
#' 
#' @return A data frame with the names of each oxidant, the rate of reaction
#' between the primary species with the oxidant, and the oxidation product.
#' 
#' @export

MCM_rates <- function(species_name) {
  
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
  
  # Helper function to convert mhchem string to R equation
  convert_mhchem_to_R <- function(mhchem_string) {
    # Remove the leading arrow (->)
    cleaned_string <- sub("^->", "", mhchem_string)
    cleaned_string <- gsub("\\\\times", "*", cleaned_string)
    cleaned_string <- gsub("\\\\exp", "exp", cleaned_string)
    cleaned_string <- gsub("\\\\frac", "", cleaned_string)
    cleaned_string <- gsub("\\T", "/T", cleaned_string)
    cleaned_string <- gsub("\\{", "", cleaned_string)
    cleaned_string <- gsub("\\}", "", cleaned_string)
    cleaned_string <- gsub("\\[", "", cleaned_string)
    cleaned_string <- gsub("\\]", "", cleaned_string)
    
    # Return the final cleaned string
    return(trimws(cleaned_string))
  }
  
  # Read the HTML content directly from the URL
  html_content <- readLines(paste0("https://mcm.york.ac.uk/MCM/species/", species_name))
  
  # Initialize vectors to store results
  reactants_list <- c()
  rates_list <- c()
  products_list <- c()
  
  # Flags to capture multi-line content
  in_reaction <- FALSE
  rate <- ""
  reactants <- ""
  products <- ""
  
  # Loop through lines to find relevant information
  for (i in seq_along(html_content)) {
    
    # Stop processing if precursor reactions div is found
    if (grepl("<div id=\"precursorRxnsCollapse\" hidden>", html_content[i])) {
      break
    }
    
    # Check if a new reaction block begins (indicated by "rxn-rate")
    if (grepl("rxn-rate", html_content[i])) {
      in_reaction <- TRUE
      rate <- html_content[i]  # Start capturing the rate line
    }
    
    # If we're inside a reaction block, capture reactants, products, and rate
    if (in_reaction) {
      # Extract reactants (4 lines before the current line)
      if (i - 4 > 0) {
        reactants <- html_content[i - 4]
      } else {
        reactants <- NA
      }
      
      # Extract products (4 lines after the current line)
      if (i + 4 <= length(html_content)) {
        products <- html_content[i + 4]
      } else {
        products <- NA
      }
      
      # Extract LaTeX portion of the rate string (\\ce{...})
      match <- regmatches(rate, regexpr("<a>(.*?)</a>", rate, perl = TRUE))
      
      # Clean the result to get only the content inside the <a> tags
      if (length(match) > 0) {
        cleaned_content <- gsub("<a>|</a>", "", match)
        
        # Extract only the part after \ce
        rate <- sub(".*\\\\ce\\{(.*)\\}.*", "\\1", cleaned_content)
        
        # Convert the mhchem string to R equation
        rate <- convert_mhchem_to_R(rate)
      } else {
        rate <- NA  # No match found
      }
      
      # Append the cleaned data to the lists
      reactants_list <- c(reactants_list, reactants)
      rates_list <- c(rates_list, rate)
      products_list <- c(products_list, products)
      
      # Reset the flag to stop capturing after one reaction
      in_reaction <- FALSE
    }
  }
  
  # Find the maximum length
  max_length <- max(length(reactants_list), length(rates_list), length(products_list))
  
  # Fill shorter lists with NA
  reactants_list <- c(reactants_list, rep(NA, max_length - length(reactants_list)))
  rates_list <- c(rates_list, rep(NA, max_length - length(rates_list)))
  products_list <- c(products_list, rep(NA, max_length - length(products_list)))
  
  # Combine the extracted data into a data frame
  reactions <- data.frame(
    Oxidant = reactants_list,
    Rate = rates_list,
    Product = products_list,
    stringsAsFactors = FALSE
  )
  
  # Return the extracted data
  return(reactions)
}

