#' Remove suspect data
#'
#' @description function sets single suspect data point or data sequence to \code{NA}
#'
#' @usage na_set(data, seq, column = 'Sensor', tz = 'UTC', seq_override = TRUE)
#'
#' @param data A \code{data.frame} or \code{tibble}
#' @param seq either a character string or a \code{POSIXct} vector.
#' @param column name(s)
#' @param tz A \code{character} indicating time zone parameter forwarded to \code{as.POSIXct} function
#' @param seq_override a boolean indicating whether the \code{seq} parameter should be treated as a sequence no matter the length
#'
#' @details
#'
#' @return original \code{data.frame} or \code{tibble} with the \code{NA} in data
#'
#' @author Marko Smiljanic
#'
#' @examples
#'
#' @importFrom tidyr `%>%`
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom rlang quo_name
#'
#' @export
na_set <- function(data, seq, column = 'Sensor', tz = 'UTC', seq_override = FALSE) {
  if(is.character(seq)) {
    seq <- as.POSIXct(seq, tz=tz)
  }
  quo_col <- rlang::quo_name(column)
  if(length(seq) == 2 & !seq_override) {
    data %>%
      dplyr::mutate(!!quo_col := dplyr::case_when((TIMESTAMP >= seq[1]) & (TIMESTAMP < seq[2]) ~ NA_real_,
                                    TRUE ~ !!rlang::sym(quo_col))) %>%
      return()
  } else {
    data %>%
      dplyr::mutate(!!quo_col := dplyr::case_when(TIMESTAMP %in% seq ~ NA_real_,
                                    TRUE ~ !!rlang::sym(quo_col))) %>%
      return()
  }
}
