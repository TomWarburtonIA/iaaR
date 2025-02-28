#' Background correction for chromatograms.
#' 
#' This function takes background data from a sample run and subtracts it from
#' a sample chromatogram. It is ONLY intended for use following the extraction 
#' of chromatographic data using `read_wacl_ch`, as it relies on the data frame
#' structure from the `read_wacl_ch` function. 
#' @author Thomas Warburton
#' 
#' @param chrom Data frame with the chromatographic data.
#' 
#' @param back Data frame with the background data.
#' 
#' @return Data frame of the background-subtracted sample chromatogram.
#' 
#' @export

background_subtract <- function(chrom, back) {
  nrow_chrom <- nrow(chrom)
  nrow_back <- nrow(back)
  
  # Determine the smaller dataframe
  smallest <- if (nrow_chrom < nrow_back) chrom else back
  
  # Calculate row difference
  n <- abs(nrow_chrom - nrow_back)
  
  # Function to process subtraction and clean data
  process_subtraction <- function(df1, df2) {
    chrom_subtracted <- data.frame(df1, df2$Intensity)
    chrom_subtracted$diff <- chrom_subtracted$Intensity - chrom_subtracted$df2.Intensity
    
    chrom_subtracted <- chrom_subtracted[, !(names(chrom_subtracted) %in% c("Intensity", "df2.Intensity"))]
    names(chrom_subtracted)[names(chrom_subtracted) == "diff"] <- "Intensity"
    
    chrom_subtracted$Intensity <- pmax(chrom_subtracted$Intensity, 0)
    return(chrom_subtracted)
  }
  
  # Case when n is 0 (no row difference)
  if (n == 0) {
    return(process_subtraction(chrom, back))
  }
  
  # Create NA rows to equalize row count
  na_rows <- data.frame(RT = rep(NA, n), Intensity = rep(NA, n))
  df_equal_rows <- rbind(smallest, na_rows)
  
  # Compare first 20 rows and decide how to process
  if (identical(df_equal_rows[1:20, ], chrom[1:20, ])) {
    return(process_subtraction(df_equal_rows, back))
  } else {
    return(process_subtraction(chrom, df_equal_rows))
  }
}
