#' Detect jumps in the signal with the simple algorithm
#'
#' @description detect jump corrections
#'
#' @usage auto_jump(data, threshold, timestamp_column = 1, name = 'Sensor')
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
auto_jump <- function (data, threshold, timestamp_column = 1, name = 'Sensor') {
  if(is.numeric(name)) {
    name <- colnames(data)[name]
  }

  if (!inherits(data[[timestamp_column]], "Date") && !inherits(data[[timestamp_column]], "POSIXct")) {
    data[[timestamp_column]] <- ymd_hms(data[[timestamp_column]])
  }
  value_diffs <- c(0, diff(data[[name]], na.rm = T))
  jumps <- which(abs(value_diffs) >= threshold)
  if(length(jumps) != 0) {
    df_out <- data.frame('Index' = jumps,
                         'Jump_time' = data[jumps, timestamp_column],
                         'Jump_value' = value_diffs[jumps])
    print(df_out)
    return(df_out)
  } else {
    return(NULL)
  }
}
