#' Extract reaction rates from Master Chemical Mechanism (MCM)
#' 
#' The MCM is a near-explicit mechanism for the degradation of atmospheric 
#' pollutants to the final oxidation products of carbon dioxide and water.
#' This function will extract the primary reaction rates for the reaction of
#' the gas-phase reactant with an oxidant. This function does not include
#' precursor reaction rates (rates for the formation of the species in 
#' question). Essentially, this function is a data scraper. However, this is
#' only intended for primary VOCs, as with secondary VOCs with several precursor
#' reactions, the rates often turn very complex - in this case, you are probably 
#' better off downloading a FACSIMILE (.fac) file directly from the website 
#' (mcm.york.ac.uk). This function will not attempt to resolve any rates which
#' make reference to complex rates or photolysis parameters - in this case, you
#' will be better served referring directly to the MCM.
#' 
#' @author Thomas Warburton
#' 
#' @param species_name A character variable for the name of the primary 
#' pollutant to extract reaction rates for. MUST follow the same naming
#' mechanism as used in the MCM.
#' 
#' @return A data frame with the names of each oxidant, the rate of reaction
#' between the primary species with the oxidant stored as a string, and the 
#' oxidation product.
#' 
#' @export

mcm_rates <- function(species_name) {
  
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
  
  # Generic rates vector
  generic_rates <- get("generic_rates", envir = asNamespace("iaaR"))
  
  # Fetch the HTML content from the generic rates page
  generic_html <- readLines("https://mcm.york.ac.uk/MCM/rates/generic", warn = FALSE)
  
  # Read the HTML content directly from the species page
  html_content <- readLines(paste0("https://mcm.york.ac.uk/MCM/species/", species_name))
  
  # Initialize vectors to store results
  reactants_list <- c()
  rates_list <- c()
  products_list <- c()
  constants_list <- c()  # To store constants for generic rates
  
  # Loop through lines to find relevant information
  for (i in seq_along(html_content)) {
    
    # Stop processing if precursor reactions div is found
    if (grepl("<div id=\"precursorRxnsCollapse\" hidden>", html_content[i])) {
      break
    }
    
    # Check if a new reaction block begins (indicated by "rxn-rate")
    if (grepl("rxn-rate", html_content[i])) {
      rate_line <- html_content[i]  # Start capturing the rate line
      
      # Extract reactants (4 lines before the current line)
      reactants <- if (i - 4 > 0) html_content[i - 4] else NA
      
      # Extract products (4 lines after the current line)
      products <- if (i + 4 <= length(html_content)) html_content[i + 4] else NA
      
      # Initialize the rate variable
      rate <- NA
      constant <- NA  # Initialize constant variable
      
      # Check if the rate contains a generic rate variable
      matched_generic_rates <- na.omit(sapply(generic_rates, function(rate_name) {
        if (grepl(paste0("\\b", rate_name, "\\b"), rate_line)) {  # Exact match with word boundaries
          return(rate_name)
        } else {
          return(NA)
        }
      }))
      
      if (length(matched_generic_rates) > 0) {
        # If a generic rate is found, keep only the name
        rate <- matched_generic_rates[1]  # Only the first matched generic rate
        
        # Extract the constant from the rate line using exact match
        const_match <- regmatches(rate_line, regexpr(paste0("\\{", rate, "\\}\\*(\\d*\\.?\\d+)"), rate_line))
        if (length(const_match) > 0) {
          constant <- sub(paste0(".*\\{", rate, "\\}\\*(\\d*\\.?\\d+).*"), "\\1", const_match)
        }
        
        # Extract the actual rate from the line following the generic rate line
        line_number <- which(grepl(rate, generic_html))
        if (length(line_number) > 0) {
          if (line_number[1] + 1 <= length(generic_html)) {
            actual_rate_line <- generic_html[line_number[1] + 1]
            actual_rate_match <- regmatches(actual_rate_line, regexpr(">([^<]+)<", actual_rate_line))
            if (length(actual_rate_match) > 0) {
              actual_rate <- gsub("[<>]", "", actual_rate_match)  # Clean and store the actual rate
              rate <- actual_rate  # Replace the generic rate with the actual rate in the rates list
            }
          }
        }
      } else {
        # Check for non-generic rates and extract them
        match <- regmatches(rate_line, regexpr("<a>(.*?)</a>", rate_line, perl = TRUE))
        rate <- if (length(match) > 0) gsub("<a>|</a>", "", match) else "complex rate, refer to MCM"
      }
      
      # Append the cleaned data to the lists
      reactants_list <- c(reactants_list, reactants)
      
      # Clean the rate string using the helper function (at end of function)
      cleaned_rate <- convert_mhchem_to_R(rate)
      
      # Append the generic rate constant if it exists
      if (!is.na(constant)) {
        cleaned_rate <- paste0(cleaned_rate, "*", constant)  # Append constant to the cleaned rate
      }
      
      rates_list <- c(rates_list, cleaned_rate)  # Store cleaned rate
      products_list <- c(products_list, products)
      constants_list <- c(constants_list, constant)  # Append the constant
    }
  }
  
  # Find the maximum length
  max_length <- max(length(reactants_list), length(rates_list), length(products_list), length(constants_list))
  
  # Fill shorter lists with NA
  reactants_list <- c(reactants_list, rep(NA, max_length - length(reactants_list)))
  rates_list <- c(rates_list, rep(NA, max_length - length(rates_list)))
  products_list <- c(products_list, rep(NA, max_length - length(products_list)))
  
  # Combine the extracted data into a data frame
  reactions <- data.frame(
    Reactant = reactants_list,
    Rate = rates_list,
    Product = products_list,
    stringsAsFactors = FALSE
  )
  
  # Return the cleaned data
  return(reactions)
}

# Helper function to clean up the rate string
convert_mhchem_to_R <- function(mhchem_string) {
  # Start with the original string
  cleaned_string <- mhchem_string
  
  # Remove the leading '\(' and '\ce{'
  cleaned_string <- gsub("^\\\\\\(", "", cleaned_string)  # Remove leading '\('
  cleaned_string <- gsub("^\\\\ce\\{", "", cleaned_string) # Remove leading '\ce{'
  
  # Remove the trailing '\)'
  cleaned_string <- gsub("\\\\)$", "", cleaned_string)  # Remove trailing '\)'
  
  # Remove any other unwanted LaTeX symbols
  cleaned_string <- gsub("\\->", "", cleaned_string)        # Remove the arrow
  cleaned_string <- gsub("\\\\times", "*", cleaned_string)  # Replace \times with *
  cleaned_string <- gsub("\\\\exp", "exp", cleaned_string)  # Keep exp as is
  cleaned_string <- gsub("\\\\frac", "", cleaned_string)    # Remove \frac if needed
  cleaned_string <- gsub("T", "/T", cleaned_string)          # Convert T to /T
  cleaned_string <- gsub("\\{", "", cleaned_string)          # Remove {
  cleaned_string <- gsub("\\}", "", cleaned_string)          # Remove }
  
  # Clean any additional unwanted characters
  cleaned_string <- gsub("\\[", "", cleaned_string)          # Remove [
  cleaned_string <- gsub("\\]", "", cleaned_string)          # Remove ]
  
  # Return the cleaned string, trimming whitespace if necessary
  return(trimws(cleaned_string))
}
