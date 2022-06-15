#' Modifying series resolutions
#'
#' @description transform series timestamps to the uniform resolution
#'
#' @usage sub_sample(data, timestamp_column = 1, resolution = 1800, start = NULL, end = NULL)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @export

sub_sample <- function(data, timestamp_column = 1, resolution = 1800, start = NULL, end = NULL) {
  timestamps <- data[[timestamp_column]]
  timestamp_name <- colnames(data)[[timestamp_column]]

  if(is.null(start)) {
    time_start <- head(na.omit(timestamps),1)
  } else {
    time_zone <- attr(timestamps, "tzone")
    time_start <- as.POSIXct(start, tz=time_zone)
  }
  if(is.null(end)) {
    time_end <- tail(na.omit(timestamps),1)
  } else {
    time_zone <- attr(timestamps, "tzone")
    time_end <- as.POSIXct(end, tz=time_zone)
  }

  if(is.null(resolution)) {
    out <- data %>%
      filter_at(timestamp_column %><% c(start, end)) %>%
      distinct_at(timestamp_column, .keep_all = TRUE)
  } else {
    timeindex <- as.data.frame(seq(time_start, time_end, resolution))
    colnames(timeindex) <- timestamp_name
    out <- left_join(timeindex, data) %>% distinct_at(timestamp_column, .keep_all = TRUE)
  }

  out
}
