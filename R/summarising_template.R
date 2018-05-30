#' @title Template of function summarising the data
#' @description This function is a template showing the general structure of
#' a function designed to compute summary statistics on the analysed data
#' @param x data (to be summarised)
#' @param g grouping variable provided as a name (symbol)
#' @details In a function call as a parameter \code{g} typically will be given
#' an object returned by \code{\link[rlang]{ensym}} or
#' \code{\link[rlang]{enexpr}} functions.
#' @return Tibble.
#' @importFrom dplyr group_by n summarise ungroup
#' @export
summarising_template <- function(x, g) {
  stopifnot(is.data.frame(x),
            is.name(g))
  x %>%
    group_by(!!g) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    return()
}
