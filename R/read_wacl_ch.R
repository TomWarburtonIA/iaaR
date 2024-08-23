#' Read and Parse Agilent ChemStation .ch Files
#'
#' This function reads Agilent \code{.ch} files and extracts chromatogram data
#' such as intensity and retention times. This is a heavily edited version of
#' code originally from chromConverter. See note for more information. This 
#' function does not scale intensities extracted from \code{.ch} file. For use
#' primarily for users of the LABGCFIDQMS instrument at Wolfson Atmospheric
#' Chemistry Laboratories (WACL)
#'
#' @param path Path to the \code{.ch} file.
#' 
#' @param format_out Format of the output data: either "matrix" or "data.frame". Default is "data.frame".
#' 
#' @param data_format Format of the data: "wide" for a single column of intensity values,
#' or "long" for separate columns for retention time and intensity. Default is "long".
#' 
#' @return A dataframe or matrix of retention time and peak intensity in the specified format.
#' 
#' @note This function is derived from a script originally written by Ethan Bass.
#' Please visit github.com/ethanbass/chromConverter to see the original script, 
#' plus many more functions for reading chromatography files into R. I (TW) only
#' adapted the code, and take no credit whatsoever for it. Please see the reference 
#' below for Ethan's original code, called by \code{read_chemstation_ch}, plus many more 
#' functions for reading chromatograms into R in. This version of \code{read_chemstation_ch}
#' exists solely to read .ch files into a format with retention time and intensity values.
#' 
#' @references Bass, E. (2023). chromConverter: Chromatographic File Converter. http://doi.org/10.5281/zenodo.6792521.
#' 
#' @author Thomas Warburton and Ethan Bass
#' 
#' #' @export
read_wacl_ch <- function(path, format_out = "data.frame", data_format = "long") {
  # Open the file for binary reading
  f <- file(path, "rb")
  on.exit(close(f))
  
  # Set the offsets for version 179
  offsets <- list(
    scaling_factor = 4732,
    intercept = 4724,
    data_start = 4096
  )
  
  # Sample Info
  seek(f, 264, "start")
  offset <- (readBin(f, "integer", n = 1, endian = "big", size = 4) - 1) * 512
  
  # Decode the data
  data <- decode_double_array_8byte(f, offset)
  
  # Extract times
  seek(f, 282, "start")
  xmin <- readBin(f, "numeric", n = 1, endian = "big", size = 4) / 60000
  xmax <- readBin(f, "numeric", n = 1, endian = "big", size = 4) / 60000
  times <- seq(xmin, xmax, length.out = length(data))
  
  # Format data as per user request
  if (data_format == "wide") {
    data <- data
  } else {
    data <- data.frame(RT = times, Intensity = data)
  }
  
  # Convert to matrix if requested
  if (format_out == "matrix") {
    data <- as.matrix(data)
  }
  
  return(data)
}

# Decoding function for version 179 .ch files
decode_double_array_8byte <- function(file, offset) {
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  offset <- 6144
  # Read data
  seek(file, offset, "start")
  signal <- readBin(file, what = "double", size = 8, endian = "little",
                    n = (fsize - offset))
  return(signal)
}

