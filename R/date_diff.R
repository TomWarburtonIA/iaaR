#' Find the difference between two dates
#' 
#' This function will find the different between two dates in number of days.
#' This only find the day difference, and does not account for times, that is
#' each date is taken as `YYYY-DD-MM 00:00:00`.
#' 
#' @author Thomas Warburton
#' 
#' @param initial_date  The first date to be considered, generally the 'start' date
#'                      of the time scale. Must be same format as 'final_date'.
#' @param final_date    The final date to be considered, generally the 'end' date
#'                      of the time scale. Must be same format as 'initial_date'.
#' @param date_format        The date format. Must be identical across 'initial_date' and
#'                      'final_date'. Use Y, M and D in quotes to express the date
#'                      format (e.g "%Y-%m-%d" or "%d-%m-%Y"). Taken as 
#'                      "YYYY-MM-DD" as default. Can also use 
#' @return              A numeric value giving the difference between the two dates.
#' @examples
#' initial_date <- "2023-01-01"
#' final__date <- "2023-04-24"
#' result <- date_diff(initial_date, final_date, date_format="%Y-%m-%d")
#' print(result)
#' 
#' @export

date_diff <- function(initial_date,
                      final_date,
                      date_format="%Y-%m-%d") {
  
  date_format = ifelse(is.null(format), "%Y-%m-%d", date_format)
  
  initial_date <- as.Date(initial_date, format=date_format)
  
  final_date <- as.Date(final_date, format=date_format)
  
  result <- as.numeric(abs(final_date-initial_date))
  
  return(result)
}