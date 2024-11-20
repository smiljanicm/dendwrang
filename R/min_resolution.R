#' Return minimal resolution
#'
#' @description return minimal resolution of the dendrometer series
#'
#' @usage min_resolution(data, timestamp_column = 1)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#'
#' @export
min_resolution <- function(data, timestamp_column = 1) {
  timestamps <- data[[timestamp_column]] %>% as.vector()
  unq_timestamps <- timestamps %>% diff() %>% unique()
  min(unq_timestamps)
}
