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

report <- function(timeindex, cleaning_script, plot_name, filename = NULL, scales = 'fixed') {
  out <- timeindex
  source(cleaning_script, local = TRUE)
  out %>% report_plot(facets=T, scales) %>% ggplot2::ggsave(plot_name, plot=., width=20, height=12, scale = 0.5)
  if(!is.null(filename)) {
    readr::write_csv(out, filename)
  }
}
