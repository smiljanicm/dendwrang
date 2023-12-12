#' Read sensor data from plain-text files
#'
#' @description reading data
#'
#' @usage read_series(filename, timestamp_column, sensor_column,
#'             data_row, header_row, col_names = NULL, ...)
#'
#' @param filename f
#' @param timestamp_column t
#' @param sensor_column s
#' @param data_row d
#' @param header_row h
#' @param col_names c
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select_at
#' @importFrom dplyr arrange_at
#' @importFrom dplyr distinct_at
#' @importFrom tidyr `%>%`
#' @importFrom purrr set_names
#'
#' @export
read_series <- function(filename, timestamp_column = 1, sensor_column = 3, data_row = 5, header_row = 2, col_names = NULL, ...) {
  nms <- scan(filename, skip=header_row-1, nlines=1, what=character(), sep=',', quiet=TRUE)
  out <- readr::read_csv(filename, skip=data_row - 1, na=c("NA", "NAN", "NaN"),
                  guess_max = Inf, col_names = FALSE, ...)
  out <- set_names(out, nms)
  out <- out %>% select_at(c(timestamp_column, sensor_column)) %>%
    arrange_at(timestamp_column) #%>%
  #  distinct_at(timestamp_column) # keeps first value of the sensor for given timestamp -

  if(is.null(col_names)) {
    col_names = c('TIMESTAMP', 'Sensor')
  }
  colnames(out) <- col_names
  as.data.frame(out)
}

