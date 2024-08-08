#' Calculate the limits of detection (LOD) or quantification/quantitation (LOQ). This will only work with a data frame with VOC names as col heads, and each sample as a separate row. Limits calculated through dividing the linearity residuals of a calibration curve against the slope of the regression. Values within the data frame should be GC peak response (eg. peak area)
#'
#' @param df A data frame with numeric columns
#' @param limit_method "detection", "quant" or "both". Calculates either LOD, LOQ or does both. Must be specified.
#' @param concs Creates a value of the concentrations used in the calibration curve. Must be specified if 'concs_in_df' is FALSE.
#' @param concs_in_df Logical to return a TRUE or FALSE for whether the concentrations are included in df. FALSE by default.
#' @param zero_remove Logical to change any 0 into an NA in df. TRUE by default.
#' @param concs_col_position State the position of the concentration column in df. Must be either "first" or "last" if 'concs_in_df' is TRUE. "none" if 'concs_in_df" is FALSE. "none" by default.
#' @return A data frame with the LOD/LOQ calculated for each column in df
#' @export
instrument_limits <- function(df, limit_method, concs = NULL, concs_in_df = FALSE, concs_units = NULL, zero_remove = TRUE, conc_col_position = "none") {

  # Validate inputs
  if (!limit_method %in% c("detection", "quant", "both")) {
    stop("Invalid value for 'limit_method'. Must be 'detection', 'quant', or 'both'.")
  }
  if (is.null(concs) && !concs_in_df) {
    stop("If 'concs_in_df' is FALSE, 'concs' must be specified.")
  }

  # Remove zeros
  if (zero_remove) {
    df[df == 0] <- NA
  }

  # Handle concentration column
  if (concs_in_df) {
    if (conc_col_position == "first") {
      concs <- df[, 1]
      df <- df[, -1]
    } else if (conc_col_position == "last") {
      concs <- df[, ncol(df)]
      df <- df[, -ncol(df)]
    } else {
      stop("Invalid value for 'concs_col_position'. Must be 'first' or 'last'.")
    }
  }

  # If concentrations are not in the data frame, ensure they are provided
  if (!concs_in_df) {
    if (is.null(concs)) {
      stop("Concentrations must be provided if 'concs_in_df' is FALSE.")
    }
  }

  # Initialize results list
  results <- list()

  # Loop over each column (VOC)
  for (i in seq_along(df)) {
    values <- df[, i]

    # Fit the linear model
    model <- lm(values ~ concs)

    # Extract model coefficients and statistics
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    sigma <- summary(model)$sigma
    adj_r_squared <- summary(model)$adj.r.squared

    # Calculate LOD and LOQ if needed
    LOD <- if (limit_method %in% c("detection", "both")) (3.3 * sigma) / slope else NA
    LOQ <- if (limit_method %in% c("quant", "both")) (10 * sigma) / slope else NA

    # Store results in a data frame
    results[[i]] <- data.frame(
      variable = names(df)[i],
      y_intercept = intercept,
      slope = slope,
      sigma = sigma,
      adj.r.squared = adj_r_squared,
      LOD = LOD,
      LOQ = LOQ
    )
  }

  # Combine all results into a single data frame
  final <- do.call(rbind, results)

  # Set row names to NULL to ensure they are numeric
  rownames(final) <- NULL

  # If only one limit method is selected, remove the NA column
  if (limit_method == "detection") {
    final <- final[, !names(final) %in% "LOQ"]
  } else if (limit_method == "quant") {
    final <- final[, !names(final) %in% "LOD"]
  }

  # Return the final data frame
  return(final)
}
