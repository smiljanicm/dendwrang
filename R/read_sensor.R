#' Read entire sensor
#'
#' @description reading sensor data
#'
#' @usage read_sensor(filenames, timestamp_columns, sensor_columns,
#'             data_rows, header_rows, col_names = NULL, ...)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom tidyr `%>%`
#' @importFrom purrr pmap_dfr
#'
#' @export
read_sensor <- function(filenames, timestamp_columns = 1, sensor_columns = 3, data_rows = 5, header_rows = 2, col_names = NULL, verbose = FALSE, ...) {
  arg_list = data.frame(filename = filenames,
                  timestamp_column = timestamp_columns,
                  sensor_column = sensor_columns,
                  data_row = data_rows,
                  header_row = header_rows,
                  stringsAsFactors = FALSE)
  if(verbose){
    print(arg_list)
  }
  out <- pmap_dfr(arg_list, .f=read_series) %>%
    arrange_at(1)
  if(is.null(col_names)) {
    col_names = c('TIMESTAMP', 'Sensor')
  }
  colnames(out) <- col_names
  as.data.frame(out)
}
