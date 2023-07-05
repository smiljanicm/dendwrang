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

  data[data$TIMESTAMP %><% jump, name] <- NA
  correction <-
    data %>%
    filter(TIMESTAMP %><% pre) %>%
    summarize('med' = median(get(name), na.rm=TRUE)) %>%
    as.double() -
    data %>%
    filter(TIMESTAMP %><% post) %>%
    summarize('med' = median(get(name), na.rm=TRUE)) %>%
    as.double()
  data[data$TIMESTAMP > mean(jump),name] %+=% (correction + offset)
  return(data)
}
