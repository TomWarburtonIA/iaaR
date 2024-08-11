#' Calculate Confidence Intervals (Regular or Bootstrapped) and return summary data frame.
#'
#' This function calculates confidence intervals for a given numeric data set using
#' either regular-based (parametric) or bootstrapped (non-parametric) methods. Can be
#' used inside `dplyr` pipelines using `reframe`.
#'
#' Note that bootstrapped confidence intervals may differ slightly when compared to other
#' bootstrapped confidence intervals derived from other functions, such as `boot` from the
#' boot library. Set seeds between functions likely differ, so ensure you use the same package
#' throughout the work to ensure reproducibility within your work. For transparency, in this
#' function, bootstrapped confidence intervals are calculated using the 'percentile' method,
#' using `set.seed(123)`. More information on percentile bootstrapping confidence intervals
#' can be found in DOI: 10.1177/2515245920911881.
#'
#' @param data A numeric vector containing the data for which the confidence interval is calculated.
#' 
#' @param conf_level A numeric value specifying the confidence level for the interval (default is 0.95).
#' 
#' @param method A character string specifying the method to use for calculating the confidence interval.
#'        Choose between "regular" (default) and "bootstrapped".
#'        
#' @param bootstrap_samples An integer specifying the number of bootstrap replicates to use when
#'        method = "bootstrapped" (default is 1000).
#'        
#' @return A data frame containing the number of samples, standard deviation, standard error, mean,
#'         and the lower and upper bounds of the confidence interval.
#'         
#' @examples
#' library(iaaR)
#'
#' data <- c(5.1, 4.9, 5.0, 5.1, 5.2)
#' confidence_interval(data, method = "regular")
#' confidence_interval(data, method = "bootstrapped", bootstrap_samples = 1000)
#' 
#' @examples
#' library(iaaR)
#' library(dplyr)
#'
#' df_long <- data.frame(
#' variable = rep(c("A", "B"), each = 10),
#' value = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
#' )
#' df_ci <- df_long %>%
#'   group_by(variable) %>%
#'   reframe(confidence_intervals(value, method = "regular", conf_level = 0.95))
#'   
#' @export
#' 
confidence_interval <- function(data, conf_level = 0.95, method = "regular", bootstrap_samples = NULL) {
  # Ensure data is a numeric vector
  if (!is.numeric(data))
    stop("Data must be a numeric vector.")

  if (method %in% c("bootstrapped", "b", "boot") && is.null(bootstrap_samples))
    stop("A number of 'bootstrap_samples' must be given.")

  alpha <- 1 - conf_level
  n <- length(data)
  sd_value <- sd(data)
  sem <- sd_value / sqrt(n)
  mean_value <- mean(data)

  if (method == "regular") {
    critical_value <- qt(1 - alpha / 2, df = n - 1)
    margin_of_error <- critical_value * sem
    lower_bound <- mean_value - margin_of_error
    upper_bound <- mean_value + margin_of_error

  } else if (method %in% c("bootstrapped", "b", "boot")) {
    # Bootstrap procedure
    set.seed(123)  # For reproducibility
    bootstrap_means <- numeric(bootstrap_samples)
    for (i in 1:bootstrap_samples) {
      sample_indices <- sample(seq_len(n), size = n, replace = TRUE)
      bootstrap_sample <- data[sample_indices]
      bootstrap_means[i] <- mean(bootstrap_sample)
    }

    # Calculate confidence intervals using percentile method
    lower_bound <- quantile(bootstrap_means, probs = alpha / 2)
    upper_bound <- quantile(bootstrap_means, probs = 1 - alpha / 2)
  }

  result <- data.frame(
    n = n,
    mean = mean_value,
    sd = sd_value,
    se = sem,
    lower_ci = lower_bound,
    upper_ci = upper_bound
  )

  return(result)
}


