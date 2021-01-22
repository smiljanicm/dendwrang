#' Assign sensor data to the data collection
#'
#' @description transform series timestamps to the uniform resolution
#'
#' @usage assign_data(data, collection, index = timeindex,
#'             by_vector = c('TIMESTAMP' = 'TIMESTAMP'),
#'             column = 'Sensor', sensorID = column)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom dendrometeR is.dendro
#' @importFrom dplyr left_join
#' @importFrom dplyr distinct_at
#'
#' @export

assign_data <- function(data, collection, index = timeindex,
                        by_vector = c('TIMESTAMP' = 'TIMESTAMP'),
                        column = 'Sensor', sensorID = column) {
  # quo_col = dplyr::quo_name(column)
  # data <- index %>% dplyr::left_join(data, by=by_vector) #%>%
  # mutate(!!quo_col := zoo::na.locf(get(column), na.rm=FALSE))
  colnames(data) <- gsub(column, sensorID, colnames(data))

  collection <- dplyr::left_join(as.data.frame(collection), dplyr::distinct_at(data, 1, .keep_all = TRUE), by=by_vector)

  collection
}
