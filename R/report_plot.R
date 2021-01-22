#' plot collection
#'
#' @description transform series timestamps to the uniform resolution
#'
#' @usage report_plot(dat=out, facets = F, scales = 'fixed')
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom tidyr pivot_longer

report_plot <- function(dat=out, facets = F, scales = 'fixed', station_names = 'Trees', sensor_name = 'Dendrometer') {
  TIMESTAMP = "TIMESTAMP"
  gg <- tidyr::pivot_longer(dat, -TIMESTAMP, names_to = station_names, values_to = sensor_name) %>%
    ggplot2::ggplot(ggplot2::aes_(x=TIMESTAMP, y=station_names, colour = sensor_name)) +
    ggplot2::geom_point()
  if(facets) {
    gg <- gg + ggplot2::facet_wrap(~Trees, scales = scales) + ggplot2::scale_color_discrete(guide=FALSE)
  }
  gg
}
