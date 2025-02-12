#' Convert data.frame/tibble to dendrometeR format
#'
#' @description convert to the dendrometeR format
#'
#' @usage as_dendrometeR(data, timestamp_column = 1)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#'
#' @export
as_dendrometeR <- function(data, timestamp_column = 1) {
  data <- data %>% arrange(pick((timestamp_column)))
  out <- as.data.frame(data, stringsAsFactors = FALSE)
  timestamps <- data[[timestamp_column]] %>% as.vector()
  unq_timestamps <- timestamps %>% diff() %>% unique()

  if(length(unq_timestamps) != 1) {
    min_timestamp = min(timestamps, na.rm =T)
    max_timestamp = max(timestamps, na.rm =T)
    timestamps = seq(min_timestamp, max_timestamp, min(unq_timestamps))
  } else {
    timestamps = data[[timestamp_column]] %>% as.vector()
  }
  timestamp_name = colnames(data)[[timestamp_column]]
  # print(timestamp_name)
  ts_df <- data.frame(as.POSIXct(timestamps))
  colnames(ts_df) <- timestamp_name
  # print(ts_df %>% head())
  out <- ts_df %>% left_join(out) %>% tibble::column_to_rownames(var = timestamp_name)
  # print(out %>% head())
  out
}

