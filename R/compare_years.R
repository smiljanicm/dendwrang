#' Compare yearly seasonality
#'
#' @description compares all of the years
#'
#' @usage compare_years(data, timestamp_column = 1)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom dendrometeR is.dendro
#' @importFrom lubridate yday
#' @export
#'
compare_years <- function(data, timestamp_column = 1) {
  # stopifnot(ncol(data) == 2)
  # sensor_column = 2-timestamp_column+1
  out <- data %>% mutate(Years = as.numeric(substring(TIMESTAMP, 1, 4)),
                         TIMESTAMP = lubridate::yday(TIMESTAMP))
  out
}
