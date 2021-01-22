#' Normalize the series
#'
#' @description compares all of the years
#'
#' @usage normalize(data, timestamp_column = 1)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @export
#'
normalize <- function(data, timestamp_column = 1) {
  sensor_columns <- 1:ncol(data)
  ff <- function(x) x - quantile(x, 0.05, na.rm =TRUE)

    if('Years' %in% colnames(data)) {
    years_column <- which(colnames(data) == 'Years')
    sensor_columns <- sensor_columns[c(-timestamp_column,-years_column)]
    out <- data %>% group_by(Years) %>% mutate_at(vars(all_of(sensor_columns)), funs(ff))
  } else {
    sensor_columns <- sensor_columns[-timestamp_column]
    out <- data %>% mutate_at(vars(all_of(sensor_columns)), funs(ff))
  }

  out
}
