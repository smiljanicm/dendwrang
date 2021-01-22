#' Glimpse sensor first few lines
#'
#' @description transform series timestamps to the uniform resolution
#'
#' @usage glimpse_sensor(filenames, n=10, ...)
#'
#' @return A \code{list} containing firt few lines of the sensor filenames
#'
#' @examples
#' # extract filepath of the example dataset
#' file_path <- system.file("extdata", 'Vilm-3_Sensors.dat', package = "dendwrang", mustWork = TRUE)
#' glimpse_sensor(file_path)
#'
#' @export

glimpse_sensor <- function(filenames, n=10, ...) {
  lapply(filenames, read_csv, n_max = n, col_names = FALSE, ...)
}

