#' Assign sensor data to the data collection
#'
#' @description transform series timestamps to the uniform resolution
#'
#' @usage report(timeindex, cleaning_script, plot_name, filename, scales = 'fixed')
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom dendrometeR is.dendro
#' @importFrom readr write_csv
#' @importFrom ggplot2 ggsave
#'
#' @export

report <- function(timeindex, cleaning_script, plot_name, filename = NULL, ...) {
  out <- timeindex
  for(i in 1:length(cleaning_script)) {
    source(cleaning_script[[i]], local = TRUE)
  }
  out %>% report_plot(facets=T, ...) %>% ggplot2::ggsave(plot_name, plot=., width=20, height=12, scale = 0.5)
  if(!is.null(filename)) {
    readr::write_csv(out, filename)
  }
}
