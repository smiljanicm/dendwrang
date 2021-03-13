#' plot(inspect) sensor
#'
#' @description transform series timestamps to the uniform resolution
#'
#' @usage plot_sensor(data, plotly = FALSE, line = TRUE)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export

plot_sensor <- function(data, plotly = FALSE, line = TRUE, ...){
  tt <- data %>% ggplot(aes(x=TIMESTAMP, y=Sensor))
  if(line) {
    tt <- tt + geom_line()
  }
  else {
    tt <- tt + geom_point()
  }
  if(plotly) {
    plotly::ggplotly(tt, dynamicTicks=TRUE, ...)
  } else {
    plot(tt)
  }
}

