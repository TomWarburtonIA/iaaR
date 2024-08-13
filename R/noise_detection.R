#' Detect Noise Regions and Manage Chromatographic Data
#'
#' For the identification of areas of noise in chromatographic data based on 
#' retention time and intensity. It can either keep the noise regions (and 
#' replace non-noise regions with `NA`) or remove the noise regions (and keep 
#' non-noise regions). As with any type of noise detection, there are obvious
#' possibilities non-noise data will be retained if you wish to isolate noise-
#' data only, and vice versa. As such you will need to adjust the parameters
#' accordingly, on the understanding you will likely need to compromise in 
#' order to isolate either noise or non-noise. If you wish to run calculations
#' on the noise data, you will need to apply rolling windows on your data.
#' Running `detect_noise` twice could be needed for data with large peaks. 
#'
#' @author Thomas Warburton
#' 
#' @param df A data frame containing chromatographic data with retention time 
#'   and intensity columns. The function will attempt to identify the columns 
#'   automatically based on typical names if not specified by the user.
#' @param rt_col_name Optional. A string specifying the column name for retention time.
#'   If not provided, the function will attempt to detect it automatically.
#' @param intensity_col_name Optional. A string specifying the column name for intensity.
#'   If not provided, the function will attempt to detect it automatically.
#' @param smoothing_window An integer specifying the window size for smoothing 
#'   the intensity data. Default is 10. Reducing this size increases sensitivity.
#' @param noise_threshold A numeric value specifying the threshold below which 
#'   the smoothed intensity is considered noise. Default is 5. Lowering this value
#'   makes the function more sensitive.
#' @param min_noise_duration Minimum number of consecutive points to consider 
#'   as a noise region. Default is 5. Reducing this value helps in detecting shorter
#'   noise bursts.
#' @param keep_noise Logical. If `TRUE`, the function replaces non-noise values 
#'   with `NA`. If `FALSE`, the function removes noise regions from the data frame 
#'   entirely and keeps only non-noise regions. Default is `TRUE`.
#' @return A data frame with the same structure as the input `df`, but with 
#'   either non-noise values or noise values replaced with `NA` based 
#'   on the `keep_noise` parameter.
#' @examples
#' df <- data.frame(rt = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                  signal = c(2, 3, 2, 2, 10, 50, 2, 2, 2, 2))
#' detect_noise(df, smoothing_window = 3, noise_threshold = 1, min_noise_duration = 2, keep_noise = TRUE)
#' detect_noise(df, keep_noise = FALSE)
#' 
#' @export
detect_noise <- function(df, 
                         rt_col_name = NULL, 
                         intensity_col_name = NULL,
                         smoothing_window = 10, 
                         noise_threshold = 5, 
                         min_noise_duration = 5,
                         keep_noise = TRUE) {
  
  # Possible column names for retention time and intensity
  rt_columns <- c("retention_time", "rt", "RT", "R_T", "r-t", "R-T")
  intensity_columns <- c("intensity", "Intensity", "signal", "Signal")
  
  # Helper function to detect column names
  detect_column <- function(col_names, possible_names) {
    matched_cols <- names(df)[sapply(names(df), function(col) tolower(col) %in% tolower(possible_names))]
    if (length(matched_cols) != 1) {
      stop("Could not find a unique column for ", paste(possible_names, collapse = ", "), " in the data frame.")
    }
    return(matched_cols)
  }
  
  # Determine the retention time column
  if (is.null(rt_col_name)) {
    rt_col_name <- detect_column(names(df), rt_columns)
  }
  
  # Determine the intensity column
  if (is.null(intensity_col_name)) {
    intensity_col_name <- detect_column(names(df), intensity_columns)
  }
  
  # Extract the relevant data
  retention_time <- df[[rt_col_name]]
  intensity <- df[[intensity_col_name]]
  
  # Ensure extracted vectors are numeric
  if (!is.numeric(retention_time) || !is.numeric(intensity)) {
    stop("Both retention_time and intensity should be numeric columns.")
  }
  
  # Smooth the intensity data using a moving average
  smoothed_intensity <- zoo::rollmean(intensity, smoothing_window, fill = NA)
  
  # Detect baseline and adjust intensity
  baseline <- zoo::rollmean(smoothed_intensity, smoothing_window, fill = NA)
  adjusted_intensity <- smoothed_intensity - baseline
  
  # Identify noise regions based on the adjusted intensity
  noise_regions <- which(adjusted_intensity <= noise_threshold)
  
  # Group consecutive noise points
  noise_groups <- split(noise_regions, cumsum(c(1, diff(noise_regions) != 1)))
  
  # Filter groups by minimum noise duration
  noise_groups <- noise_groups[sapply(noise_groups, length) >= min_noise_duration]
  
  # Create a logical vector indicating noise
  noise_logical <- rep(FALSE, length(intensity))
  for (group in noise_groups) {
    noise_logical[group] <- TRUE
  }
  
  if (keep_noise) {
    # Replace non-noise values with NA in the intensity column
    df[[intensity_col_name]] <- ifelse(noise_logical, intensity, NA)
  } else {
    # Keep only non-noise rows in the data frame
    df[[intensity_col_name]] <- ifelse(!noise_logical, intensity, NA)
  }
  
  return(df)
}

