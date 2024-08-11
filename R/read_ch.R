#' Read and Parse Agilent ChemStation .ch Files
#'
#' This function reads Agilent \code{.ch} files and extracts chromatogram data
#' such as intensity and retention times. It supports various file formats from
#' different versions of Agilent ChemStation and OpenLab software.
#'
#' @param path Path to the \code{.ch} file.
#' 
#' @param format_out Format of the output data: either "matrix" or "data.frame". Default is "data.frame".
#' 
#' @param data_format Format of the data: "wide" for a single column of intensity values,
#' or "long" for separate columns for retention time and intensity. Default is "long".
#' 
#' @return A dataframe of rentention time and peak intensity in the specified format.
#' 
#' @export
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
read_ch <- function(path, 
                    format_out = c("data.frame", "matrix", "df", "data", "mat", "m"),
                    data_format = c("long", "wide")) {
  
  # Map abbreviations to full names
  format_out_map <- c("data.frame" = "data.frame", "df" = "data.frame", "data" = "data.frame",
                      "matrix" = "matrix", "mat" = "matrix", "m" = "matrix")
  data_format_map <- c("long" = "long", "wide" = "wide")
  
  # Match and map arguments
  format_out <- match.arg(format_out)
  format_out <- format_out_map[format_out]
  
  data_format <- match.arg(data_format)
  data_format <- data_format_map[data_format]
  
  f <- file(path, "rb")
  on.exit(close(f))
  
  # HEADER
  seek(f, 1, "start")
  version <- readBin(f, "character", n = 1)
  version <- match.arg(version, choices = c("8", "81", "30", "130", "179", "181"))
  offsets <- get_agilent_offsets(version)
  
  if (version == "179") {
    seek(f, 348)
    filetype <- paste(readBin(f, "character", n = 2), collapse = "")
    if (filetype == "OL") {
      bytes <- "8b"
    } else if (filetype == "GC") {
      seek(f, offsets$software)
      n <- get_nchar(f)
      soft <- cc_collapse(readBin(f, "character", n = n))
      chemstation_version <- strsplit(soft, " ")[[1]][1]
      bytes <- ifelse(chemstation_version == "Mustang", "8b", "4b")
    }
    version <- paste(version, bytes, sep = "_")
  }
  
  decoder <- switch(version,
                    "8" = decode_delta,
                    "81" = decode_double_delta,
                    "30" = decode_delta,
                    "130" = decode_delta,
                    "181" = decode_double_delta,
                    "179_4b" = decode_double_array_4byte,
                    "179_8b" = decode_double_array_8byte)
  
  # Sample Info
  seek(f, 264, "start")
  offset <- (readBin(f, "integer", n = 1, endian = "big", size = 4) - 1) * 512
  data <- decoder(f, offset)
  
  seek(f, where = 282, origin = "start")
  
  if (version %in% c("8", "30", "130")) {
    xmin <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE, endian = "big")) / 60000
    xmax <- as.double(readBin(f, "integer", n = 1, size = 4, signed = TRUE, endian = "big")) / 60000
  } else {
    xmin <- readBin(f, "numeric", n = 1, endian = "big", size = 4) / 60000
    xmax <- readBin(f, "numeric", n = 1, endian = "big", size = 4) / 60000
  }
  
  times <- seq(xmin, xmax, length.out = length(data))
  
  seek(f, offsets$intercept, "start")
  intercept <- readBin(f, "double", n = 1, endian = "big", size = 8)
  if (is.na(intercept)) intercept <- 0
  
  seek(f, offsets$scaling_factor, "start")
  scaling_factor <- readBin(f, "double", n = 1, endian = "big", size = 8)
  
  data <- data * scaling_factor + intercept
  
  if (data_format == "wide") {
    data <- data.frame(Intensity = data, row.names = times)
  } else if (data_format == "long") {
    data <- data.frame(RT = times, Intensity = data)
  }
  
  if (format_out == "matrix") {
    data <- as.matrix(data)
  }
  
  return(data)
}

# Helper functions as per the original code
cc_collapse <- function(x) {
  paste(x, collapse = "")
}

get_nchar <- function(f) {
  as.numeric(readBin(f, what = "raw", n = 1))
}

decode_double_delta <- function(file, offset) {
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  
  seek(file, offset, "start")
  signal <- numeric(fsize / 2)
  count <- 1
  buffer <- numeric(3)
  
  while (seek(file, NA, "current") < fsize) {
    buffer[3] <- readBin(file, "integer", n = 1, endian = "big", size = 2)
    
    if (buffer[3] != 32767) {
      buffer[2] <- buffer[2] + buffer[3]
      buffer[1] <- buffer[1] + buffer[2]
    } else {
      buffer[1] <- readBin(file, "integer", n = 1, endian = "big", size = 2) * 4294967296
      buffer[1] <- readBin(file, "integer", n = 1, endian = "big", size = 4) + buffer[1]
      buffer[2] <- 0
    }
    
    signal[count] <- buffer[1]
    count <- count + 1
  }
  
  signal <- signal[1:(count - 1)]
  return(signal)
}

decode_double_array_4byte <- function(file, offset) {
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  offset <- 6144
  
  seek(file, offset, "start")
  signal <- readBin(file, what = "double", size = 4, endian = "little", n = (fsize - offset))
  signal <- signal[seq(2, length(signal), 2)]
  return(signal)
}

decode_double_array_8byte <- function(file, offset) {
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  offset <- 6144
  
  seek(file, offset, "start")
  signal <- readBin(file, what = "double", size = 8, endian = "little", n = (fsize - offset))
  return(signal)
}

decode_delta <- function(file, offset) {
  seek(file, 0, 'end')
  fsize <- seek(file, NA, "current")
  
  seek(file, offset, "start")
  start <- seek(file, NA, "current")
  
  signal <- rep(NA, round((fsize - start) / 2))
  buffer <- rep(0, 4)
  index <- 1
  
  while (TRUE) {
    head <- readBin(file, "integer", n = 1, size = 1, endian = "big")
    if (head != 0x10) {
      break
    }
    buffer[2] <- buffer[4]
    
    segment_length <- readBin(file, "integer", n = 1, size = 1, endian = "big")
    for (i in seq_len(segment_length)) {
      buffer[3] <- readBin(file, "integer", n = 1, size = 2, endian = "big")
      if (buffer[3] != -32768L) {
        buffer[2] <- buffer[2] + buffer[3]
      } else {
        buffer[2] <- readBin(file, "integer", n = 1, size = 4, endian = "big")
      }
      
      signal[index] <- buffer[2]
      index <- index + 1
    }
    buffer[4] <- buffer[2]
  }
  signal <- signal[!is.na(signal)]
  return(signal)
}

get_agilent_offsets <- function(version) {
  offsets <- switch(version,
                    "131_LC" = list(version = 326, file_type = 347, sample_name = 858,
                                    operator = 1880, date = 2391, detector = 2492,
                                    method = 2574, software = 3089, units = 3093,
                                    sample_id = 4055, num_times = 278, rt_first = 282,
                                    rt_last = 286, scaling_factor = 3085, data_start = 4096),
                    "131_OL" = list(version = 326, file_type = 347, sample_name = 858,
                                    operator = 1880, date = 2391, method = 2574,
                                    units = 3093, sample_id = 4055, num_times = 278,
                                    rt_first = 282, rt_last = 286, scaling_factor = 3085,
                                    data_start = 4096),
                    "31" = list(version = 0, file_type = 4, sample_name = 24,
                                operator = 148, date = 178, detector = 208, instrument = 218,
                                method = 228, num_times = 278, scaling_factor = 318,
                                units = 326, data_start = 512),
                    "179" = list(version = 326, file_type = 347, sample_name = 858,
                                 operator = 1880, date = 2391, instrument = 2492,
                                 method = 2574, software = 3089, unit = 4172,
                                 signal = 4213, num_times = 278, rt_first = 282,
                                 rt_last = 286, scaling_factor = 4732, intercept = 4724,
                                 data_start = 4096),
                    "179_4b" = list(version = 326, file_type = 347, sample_name = 858,
                                    operator = 1880, date = 2391, instrument = 2492,
                                    method = 2574, software = 3089, unit = 4172,
                                    signal = 4213, num_times = 278, rt_first = 282,
                                    rt_last = 286, scaling_factor = 4732, intercept = 4724,
                                    data_start = 4096),
                    "179_8b" = list(version = 326, file_type = 347, sample_name = 858,
                                    operator = 1880, date = 2391, instrument = 2492,
                                    method = 2574, software = 3089, unit = 4172,
                                    signal = 4213, num_times = 278, rt_first = 282,
                                    rt_last = 286, scaling_factor = 4732, intercept = 4724,
                                    data_start = 4096),
                    "130" = list(version = 326, file_type = 347, sample_name = 858,
                                 operator = 1880, date = 2391, inlet = 2492,
                                 instrument = 2533, method = 2574, software = 3089,
                                 software_version = 3601, software_revision = 3802,
                                 sample_id = 4054, units = 4172, signal = 4213,
                                 intercept = 4110, scaling_factor = 4732),
                    "30" = list(version = 0, file_type = 4, sample_name = 24,
                                operator = 148, date = 178, detector = 208,
                                instrument = 218, method = 228, software = 322,
                                software_version = 355, software_revision = 405,
                                units = 580, signal = 596, intercept = 636,
                                scaling_factor = 644, data_start = 1024),
                    "8" = list(version = 0, file_type = 4, sample_name = 24,
                               description = 86, operator = 148, date = 178,
                               detector = 208, instrument = 218, method = 228,
                               unit = 580, num_times = 0x116, rt_first = 0x11A,
                               rt_last = 0x11E, scaling_factor = 644, intercept = 636,
                               data_start = 4096),
                    "81" = list(version = 0, file_type = 4, sample_name = 24,
                                description = 86, operator = 148, date = 178,
                                detector = 208, instrument = 218, method = 228,
                                unit = 580, num_times = 0x116, rt_first = 0x11A,
                                rt_last = 0x11E, scaling_factor = 644, intercept = 636,
                                data_start = 4096)
  )
  return(offsets)
}
