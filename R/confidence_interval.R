#' Calculate Confidence Intervals (Regular or Bootstrapped) and return summary data frame.
#'
#' This function calculates confidence intervals for a given numeric data set using
#' either regular-based (parametric) or bootstrapped (non-parametric) methods. Can be
#' used inside dplyr pipelines using reframe
#'
#' @param data A numeric vector containing the data for which the confidence interval is calculated.
#' @param conf.level A numeric value specifying the confidence level for the interval (default is 0.95).
#' @param method A character string specifying the method to use for calculating the confidence interval.
#'        Choose between "regular" (default) and "bootstrapped".
#' @param bootstrap_samples An integer specifying the number of bootstrap replicates to use when
#'        method = "bootstrapped" (default is 1000).
#' @return A data frame containing the number of samples, standard deviation, standard error, mean,
#'         and the lower and upper bounds of the confidence interval.
#' @examples
#' data <- c(5.1, 4.9, 5.0, 5.1, 5.2)
#' confidence_interval(data, method = "regular")
#' confidence_interval(data, method = "bootstrapped", bootstrap_samples = 1000)
#' @examples
#' df_long <- data.frame(
#' variable = rep(c("A", "B"), each = 10),
#' value = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
#' )
#' df_ci <- df_long %>%
#'   group_by(variable) %>%
#'   reframe(confidence_intervals(value, method = "regular", conf.level = 0.95))
#' @import boot
#' @import dplyr
#' @export
confidence_interval <- function(data, conf.level = 0.95, method = "regular", bootstrap_samples = NULL) {
  # Ensure data is a numeric vector
  if (!is.numeric(data))
    stop("Data must be a numeric vector.")

  if (method %in% c("bootstrapped", "b", "boot") && is.null(bootstrap_samples))
    stop("A number of 'bootstrap_samples' must be given.")

  alpha <- 1 - conf.level
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
    boot_mean <- function(data, indices) {
      return(mean(data[indices]))
    }

    boot_results <- boot(data, statistic = boot_mean, R = bootstrap_samples)
    boot_ci <- boot.ci(boot_results, type = "perc", conf = conf.level)

    lower_bound <- boot_ci$percent[4]
    upper_bound <- boot_ci$percent[5]
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
