#' Recursive jump correction
#'
#' @description auto correct jump and outlier corrections
#'
#' @usage jumper(data, timestamp_column = 1, name = 'Sensor', ...)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom tidyr `%>%`
#' @importFrom RCop `%><%`
#' @importFrom RCop `%+=%`
#'
#' @export
jumper <- function (data, timestamp_column = 1, name = 'Sensor', ...) {
  data <- as_tibble(data) %>% rename('TIMESTAMP' = timestamp_column)

  if(is.numeric(name)) {
    name <- colnames(data)[name]
  }
#  print(name)
#  print(head(data))
  if (!inherits(data[[timestamp_column]], "Date") && !inherits(data[[timestamp_column]], "POSIXct")) {
    data[[timestamp_column]] <- ymd_hms(data[[timestamp_column]])
  }

  df_out <- auto_jump(data, timestamp_column = timestamp_column, name = name, ...)
#  print(df_out)
  if(is.null(df_out)) {
    return(data)
  }
  correction <- df_out$TIMESTAMP[1]
  data <- data %>% jump_correction(jump = correction, name=name)
  df_out <- auto_jump(data, timestamp_column = timestamp_column, name = name, ...)
  if(is.null(df_out)) {
    return(data)
  }

  if(nrow(df_out) == 0) {
    return(data)
  } else {
    jumper(data, timestamp_column, name, ...)
  }
}
