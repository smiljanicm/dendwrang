#' Correct signal jumps due to base shifts
#'
#' @description setting jump corrections
#'
#' @usage jump_correction(data, pre, post, jump, offset = 0, name = 'Sensor')
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
jump_correction <- function(data, pre, post, jump, tz='UTC', offset = 0, name = 'Sensor') {
  if(is.numeric(name)) {
    name <- colnames(data)[name]
  }

  if(is.character(pre)) {
    pre <- as.POSIXct(pre, tz=tz)
  }

  if(is.character(post)) {
    post <- as.POSIXct(post, tz=tz)
  }

  if(is.character(jump)) {
    jump <- as.POSIXct(jump, tz=tz)
  }
  min_res <- min_resolution(data)

  # print(min_res) should be a warning of duplicates if min_res = 0
  if(min_res > 0) {
    pre[[1]] <- lubridate::as_datetime(as.numeric(pre[[1]]) %/% min_res * min_res, tz=tz)
    pre[[2]] <-  lubridate::as_datetime(as.numeric(pre[[2]] + min_res) %/% min_res * min_res, tz=tz)

    post[[1]] <- lubridate::as_datetime(as.numeric(post[[1]]) %/% min_res * min_res, tz=tz)
    post[[2]] <-  lubridate::as_datetime(as.numeric(post[[2]] + min_res) %/% min_res * min_res, tz=tz)

    jump[[1]] <- lubridate::as_datetime(as.numeric(jump[[1]]) %/% min_res * min_res, tz=tz)
    jump[[2]] <-  lubridate::as_datetime(as.numeric(jump[[2]] + min_res) %/% min_res * min_res, tz=tz)
  }

  # print(jump)
  data[data$TIMESTAMP %>=<% jump, name] <- NA
  # print(range(data$TIMESTAMP))
  correction <-
    data %>%
    filter(TIMESTAMP %><% pre) %>%
    summarize('med' = median(get(name), na.rm=TRUE)) %>%
    as.double() -
    data %>%
    filter(TIMESTAMP %><% post) %>%
    summarize('med' = median(get(name), na.rm=TRUE)) %>%
    as.double()
  data[data$TIMESTAMP > min(jump),name] %+=% (correction + offset)
  return(data)
}
