#' Get possible resolutions and their frequencies
#'
#' @description extract temporal resolution frequencies
#'
#' @usage get_resolution(data, timestamp_column = 1)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @export
#'

get_resolution <- function(data, timestamp_column = 1) {
  timestamps <- data[[timestamp_column]]
  table(diff(timestamps))
}
