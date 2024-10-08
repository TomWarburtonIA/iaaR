% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confidence_interval.R
\name{confidence_interval}
\alias{confidence_interval}
\title{Calculate Confidence Intervals (Regular or Bootstrapped) and return summary data frame.}
\usage{
confidence_interval(
  data,
  conf_level = 0.95,
  method = "regular",
  bootstrap_samples = NULL
)
}
\arguments{
\item{data}{A numeric vector containing the data for which the confidence interval is calculated.}

\item{conf_level}{A numeric value specifying the confidence level for the interval (default is 0.95).}

\item{method}{A character string specifying the method to use for calculating the confidence interval.
Choose between "regular" (default) and "bootstrapped".}

\item{bootstrap_samples}{An integer specifying the number of bootstrap replicates to use when
method = "bootstrapped" (default is 1000).}
}
\value{
A data frame containing the number of samples, standard deviation, standard error, mean,
and the lower and upper bounds of the confidence interval.
}
\description{
This function calculates confidence intervals for a given numeric data set using
either regular-based (parametric) or bootstrapped (non-parametric) methods. Can be
used inside \code{dplyr} pipelines using \code{reframe}.
}
\details{
Note that bootstrapped confidence intervals may differ slightly when compared to other
bootstrapped confidence intervals derived from other functions, such as \code{boot} from the
boot library. Set seeds between functions likely differ, so ensure you use the same package
throughout the work to ensure reproducibility within your work. For transparency, in this
function, bootstrapped confidence intervals are calculated using the 'percentile' method,
using \code{set.seed(123)}. More information on percentile bootstrapping confidence intervals
can be found in DOI: 10.1177/2515245920911881.
}
\examples{
library(iaaR)

data <- c(5.1, 4.9, 5.0, 5.1, 5.2)
confidence_interval(data, method = "regular")
confidence_interval(data, method = "bootstrapped", bootstrap_samples = 1000)

library(iaaR)
library(dplyr)

df_long <- data.frame(
variable = rep(c("A", "B"), each = 10),
value = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
)
df_ci <- df_long \%>\%
  group_by(variable) \%>\%
  reframe(confidence_intervals(value, method = "regular", conf_level = 0.95))
  
}
\author{
Thomas Warburton
}
