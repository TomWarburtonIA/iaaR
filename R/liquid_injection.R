#' Calculate the volume of liquid required to generate a target gas mixture
#'
#' This function calculates the volume of a liquid chemical required to produce
#' a target gas mixture at a specified mixing ratio within a vessel. It assumes
#' that the liquid fully evaporates into the gas phase and does not partition
#' or adsorb within the vessel. 
#'
#' Provide the function with the chemical name (or CAS number), the desired
#' mixing ratio of the target gas (in parts per billion), and the final gas
#' volume within the vessel (in metres cubed). The final gas volume can be 
#' calculated, for example, by multiplying the vessel volume by the final 
#' pressure (in bar) and then adding the physical vessel volume (ie. a 200 bar,
#' 10 litre vessel will have a final volume of 2010 L, or 2.01 m^3).
#'
#' The function will attempt to automatically retrieve the chemical's density
#' and molecular weight from PubChem. However, either or both of these values
#' may be supplied directly via the `density` and `mw` arguments if preferred.
#'
#' @author Thomas Warburton
#'
#' @param name The name or CAS number of the chemical used to generate the
#' target gas.
#'
#' @param mixing_ratio The mixing ratio of the target gas, expressed in parts
#' per billion (ppb).
#'
#' @param volume The total volume of gas being blended, in cubic metres (m^3).
#'
#' @param mw An optional numeric value specifying the molecular weight of the
#' chemical (in g/mol). If omitted, the function attempts to retrieve it
#' automatically.
#'
#' @param density An optional numeric value specifying the density of the
#' liquid chemical (in g/cm^3). If omitted, the function attempts to retrieve
#' it automatically.
#'
#' @return A data frame containing the chemical name, mixing ratio, gas volume,
#' molecular weight, density, and the calculated liquid injection volume in
#' microlitres.
#'
#' @export
#'
#' @examples
#' library(iaaR)
#'
#' df <- liquid_injection("ethanol", 4, 2.01)
#' view(df)
#'
#' df_CAS_density_mw_provided <- liquid_injection(
#'   "64-17-5", 4, 2.01,
#'   density = 0.79,
#'   mw = 46.07
#' )
#' view(df_CAS_density_mw_provided)

#' 
#' 
liquid_injection <- function(name, mixing_ratio, volume, mw=NULL, density=NULL) {
  
  ## initialise variables
  chemical_mw <- mw
  chemical_density <- density
  
  ## if either property is missing, scrape it
  if (is.null(chemical_mw) || is.null(chemical_density)) {
    cid <- get_pubchem_cid(name)
    if (is.na(cid)) stop("Could not resolve PubChem CID for: ", name)
    
    url <- paste0(
      "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/",
      cid, "/JSON"
    )
    lines <- readLines(url, warn = FALSE)
    
    if (is.null(chemical_mw))
      chemical_mw <- extract_pubchem_value(lines, "Molecular Weight")
    
    if (is.null(chemical_density))
      chemical_density <- extract_pubchem_value(lines, "Density")
  }
  
  ## safety checks
  if (is.na(chemical_mw))
    stop("Could not determine molecular weight (PubChem entry incomplete).")
  
  if (is.na(chemical_density))
    stop("Could not determine density (PubChem entry incomplete).")
  
  ## calculation
  conc <- (chemical_mw * mixing_ratio) / 24.45        # micro g/m^3
  mass_ug <- conc * volume                            # total micro g
  mass_g  <- mass_ug / 1e6                            # convert to grams
  volume_mL <- mass_g / chemical_density              # mL liquid required
  volume_uL <- volume_mL * 1000                       # microlitres
  
  ## return result
  data.frame(
    name = name,
    ppb = mixing_ratio,
    volume = volume,
    molecular_weight = chemical_mw,
    density = chemical_density,
    liquid_injection_volume_microlitres = volume_uL
  )
}


##helper functions

get_pubchem_cid <- function(identifier) {
  id <- gsub(" ", "%20", identifier)  # URL encode basic characters
  url <- paste0(
    "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
    id,
    "/cids/TXT"
  )
  
  out <- try(readLines(url, warn = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) return(NA)
  
  cid <- suppressWarnings(as.numeric(out[1]))
  if (is.na(cid)) return(NA)
  cid
}

extract_pubchem_value <- function(lines, heading) {
  # find the section by TOCHeading
  idx <- grep(paste0('"TOCHeading"\\s*:\\s*"', heading, '"'),
              lines, perl = TRUE)
  if (length(idx) == 0) return(NA_real_)
  
  # search forward for the first "String": " ... "
  for (i in seq.int(idx, length(lines))) {
    
    m <- regexpr('"String"\\s*:\\s*"([^"]+)"', lines[i], perl = TRUE)
    if (m > 0) {
      
      # extract the text inside "String" : " ... "
      full_string <- sub('"String"\\s*:\\s*"([^"]+)"',
                         "\\1",
                         regmatches(lines[i], m))
      
      # extract FIRST numeric substring
      num_match <- regexpr("[0-9]+\\.?[0-9]*", full_string, perl = TRUE)
      
      if (num_match > 0) {
        num <- as.numeric(regmatches(full_string, num_match))
        return(num)
      }
    }
  }
  
  NA_real_
}
