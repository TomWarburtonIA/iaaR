#' Detect Noise Regions and Subtle Peaks in Chromatographic Data
#'
#' This function identifies areas of noise and subtle peaks in chromatographic data 
#' based on retention time and intensity. It can automatically detect relevant columns 
#' based on common naming conventions or the user can specify their own column names 
#' for retention time and intensity. Non-noise regions are replaced with `NA`.
#'
#' @param df A data frame containing chromatographic data with retention time 
#'   and intensity columns. The function will attempt to identify the columns 
#'   automatically based on typical names if not specified by the user.
#' @param rt_col_name Optional. A string specifying the column name for retention time.
#'   If not provided, the function will attempt to detect it automatically.
#' @param intensity_col_name Optional. A string specifying the column name for intensity.
#'   If not provided, the function will attempt to detect it automatically.
#' @param smoothing_window An integer specifying the window size for smoothing 
#'   the intensity data. Default is 5. Reducing this size increases sensitivity.
#' @param noise_threshold A numeric value specifying the threshold below which 
#'   the smoothed intensity is considered noise. Default is 2. Lowering this value
#'   makes the function more sensitive.
#' @param min_noise_duration Minimum number of consecutive points to consider 
#'   as a noise region. Default is 3. Reducing this value helps in detecting shorter
#'   noise bursts.
#' @return A data frame with the same structure as the input `df`, but with 
#'   non-noise values in the intensity column replaced with `NA`.
#' @examples
#' df <- data.frame(rt = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'                  signal = c(2, 3, 2, 2, 10, 50, 2, 2, 2, 2))
#' detect_noise(df, smoothing_window = 3, noise_threshold = 1, min_noise_duration = 2)
#' @export
detect_noise <- function(df, 
                         rt_col_name = NULL, 
                         intensity_col_name = NULL,
                         smoothing_window = 5, 
                         noise_threshold = 2, 
                         min_noise_duration = 3) {
  
  # Possible column names for retention time and intensity
  rt_columns <- c("retention_time", "rt", "RT", "R_T", "r-t", "R-T")
  intensity_columns <- c("intensity", "Intensity", "signal", "Signal")
  
  # Determine the retention time column
  if (is.null(rt_col_name)) {
    rt_col <- names(df)[sapply(names(df), function(col) tolower(col) %in% tolower(rt_columns))]
    if (length(rt_col) != 1) {
      stop("Could not find a unique retention time column in the data frame.")
    }
    rt_col_name <- rt_col
  }
  
  # Determine the intensity column
  if (is.null(intensity_col_name)) {
    intensity_col <- names(df)[sapply(names(df), function(col) tolower(col) %in% tolower(intensity_columns))]
    if (length(intensity_col) != 1) {
      stop("Could not find a unique intensity column in the data frame.")
    }
    intensity_col_name <- intensity_col
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
  
  # Replace non-noise values with NA in the intensity column
  df[[intensity_col_name]] <- ifelse(noise_logical, intensity, NA)
  
  return(df)
}
