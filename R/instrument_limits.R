#' Calculate the limits of detection (LOD) or quantification/quantitation (LOQ).
#'
#' This function calculates the LOD or LOQ for each VOC (volatile organic compound)
#' using the peak area as input data. Limits are calculated by dividing the linearity
#' residuals of a calibration curve against the slope of the regression. Regression
#' Pearson's R. Doesn't allow for Spearman's Rho / Kendall's Tau (these  methods
#' are more for either non-parametric regressions or those with outliers, and if your
#' calibration curve contains outliers, it probably needs to be run again).
#'
#' @author Thomas Warburton
#' 
#' @param df A data frame with numeric columns representing peak areas.
#' 
#' @param limit_method Character string specifying the type of limit to calculate:
#'        "detection", "quant" or "both".
#'        
#' @param concs Optional vector of concentrations used in the calibration curve.
#'        Must be provided if 'concs_in_df' is FALSE.
#'        
#' @param concs_in_df Logical indicating whether concentrations are included in df.
#' 
#' @param concs_col_position Position of the concentration column in df if 'concs_in_df'
#'        is TRUE. Must be either "first", "last", or "none".
#'        
#' @param df_concs Optional data frame with concentrations corresponding to each species
#'        in df. Must be of equal size to df.
#'        
#' @param zero_remove Logical indicating whether to replace zeros with NA in df.
#' 
#' @return A data frame with the LOD/LOQ calculated for each column in df.
#' 
#' @export
#' 
instrument_limits <- function(df,
                              limit_method,
                              concs = NULL,
                              concs_in_df = FALSE,
                              zero_remove = TRUE,
                              conc_col_position = "none",
                              df_concs = NULL)
  {

  # Validate inputs
  if (!is.null(conc_col_position)) {
    concs_in_df=TRUE
  }
  if (!limit_method %in% c("detection", "quant", "both")) {
    stop("Invalid value for 'limit_method'. Must be 'detection', 'quant', or 'both'.")
  }
  if (is.null(concs) && !concs_in_df && is.null(df_concs)) {
    stop("No concentrations provided. Please provide 'concs', 'concs_in_df', or 'df_concs'.")
  }
  if (is.null(concs) && !concs_in_df && is.null(df_concs)) {
    stop("If 'concs_in_df' is FALSE, 'concs' must be specified.")
  }
  

  # Remove zeros if specified
  if (zero_remove) {
    df[df == 0] <- NA
  }

  # Handle concentration column if included in df
  if (concs_in_df) {
    if (is.character(conc_col_position)) {
      if (conc_col_position == "first") {
        concs <- df[, 1]
        df <- df[, -1]
      } else if (conc_col_position == "last") {
        concs <- df[, ncol(df)]
        df <- df[, -ncol(df)]
      } else if (conc_col_position == "none") {
        concs <- NULL  # Or any appropriate action for 'none'
      } else {
        stop("Invalid value for 'conc_col_position'. Must be 'first', 'last', 'none', or a vector.")
      }
    } else if (is.numeric(conc_col_position) && all(conc_col_position %in% seq_len(ncol(df)))) {
      # Handle vector assignment
      concs <- df[, conc_col_position]
      df <- df[, -conc_col_position]
    } else {
      stop("Invalid value for 'conc_col_position'. Must be 'first', 'last', 'none', or a valid vector of column indices.")
    }
  }

  # If df_concs is provided, ensure column names match and filter as needed
  if (!is.null(df_concs)) {

    # Check if df_concs has only one column
    if (ncol(df_concs) == 1) {
      message("df_concs has only one column, assuming this is a column of concentrations.")
      proceed <- readline(prompt = "Do you want to apply these concentrations to all species in df? (Y/N): ")

      if (toupper(proceed) == "Y") {
        # Replicate the single column across all columns of df
        df_concs <- as.data.frame(matrix(rep(df_concs[, 1], ncol(df)), nrow = nrow(df), ncol = ncol(df)))
        colnames(df_concs) <- colnames(df)
      } else {
        stop("Operation aborted by user.")
      }
    }

    # Ensure column names match and filter as needed
    common_cols <- intersect(colnames(df), colnames(df_concs))
    if (!setequal(colnames(df), colnames(df_concs))) {
      message("Uncommon columns have been found and will be discarded.")
      proceed <- readline(prompt = "Do you want to continue with the common columns only? (Y/N): ")
      if (toupper(proceed) != "Y") {
        stop("Cannot proceed with unmatched columns, operation will not execute.")
      }
      df <- df[, common_cols, drop = FALSE]
      df_concs <- df_concs[, common_cols, drop = FALSE]
    } else {
      message("All columns match in df and df_concs. Proceeding with the original data frames.")
    }
  }

  # Initialize results list
  results <- list()

  # Loop over each column (VOC)
  for (i in seq_along(df)) {
    values <- df[, i]

    # Use the corresponding column for concentrations if df_concs is provided
    if (!is.null(df_concs)) {
      concs <- df_concs[, i]
    }

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
