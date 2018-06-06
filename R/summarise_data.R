#' @title Performing summarising computations
#' @description This function is designed as a mid-level interface to perform
#' summarising computations. It requaries to define grouping (with the \code{g}
#' parameter, which should be given as a name/symbol) and allows to define
#' filtering, that will be done before performing computations (with the \code{f}
#' parameter, which should be given as a call). Computations to be performed
#' should be given in a dplyr style, that is as calls.
#' @param x data (to be summarised)
#' @param g grouping variable provided as a name (symbol)
#' @param f optionally a call defining filtering to be done before computations
#' @param joinTo optionally a data frame to which computed summary statistics
#' should be (left-) joined (if defined, function will return a joined object)
#' @param ... calls describing computations to be performed
#' @details In a function call as a parameter \code{g} typically will be given
#' an object returned by \code{\link[rlang:quotation]{ensym}} or
#' \code{\link[rlang:quotation]{enexpr}} functions.
#' @return Tibble.
#' @importFrom dplyr filter group_by summarise ungroup
#' @importFrom rlang enexpr enexprs
summarise_data <- function(x, g, f = TRUE, joinTo = NULL, ...) {
  stopifnot(is.data.frame(x), is.name(g),
            is.data.frame(joinTo) | is.null(joinTo))
  f = enexpr(f)
  s = enexprs(...)

  result <- tryCatch(x %>%
                       filter(!!f) %>%
                       group_by(!!g) %>%
                       summarise(!!!s) %>%
                       ungroup())
  if (!is.null(joinTo)) {
    result <- join_with_check(joinTo, result)
  }
  return(result)
}
