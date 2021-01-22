#' Convert data.frame/tibble to dendrometeR format
#'
#' @description convert to the dendrometeR format
#'
#' @usage as_dendrometeR(data, timestamp_column = 1)
#'
#' @return A \code{data.frame}
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom dendrometeR is.dendro
#'
#' @export
as_dendrometeR <- function(data, timestamp_column = 1) {
  out <- as.data.frame(data, stringsAsFactors = FALSE)
  timestamps <- data[timestamp_column]
  timestmaps_diffs <- unique(diff(timestamps))

  #if() {

#  }
  rownames(out) <- as.characater(data[timestamp_column])
  out <- out[-timestamp_column]
  stopifnot(dendrometeR::is.dendro(out))
  out
}

