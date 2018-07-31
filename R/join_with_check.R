#' @title Joining two data frames
#' @description Simple routine designed to find out (and report) whether two
#' data frames have unique combinations of values of common variables and
#' whether all rows in \code{x} has pair(s) in \code{y} and \emph{vice versa}
#' before performing full join.
#' @param x data frame
#' @param y data frame
#' @param xDescription optionally a description of what is \code{x} (for
#' purpose of providing informative warninig message)
#' @param yDescription optionally a description of what is \code{y}  (for
#' purpose of providing informative warninig message)
#' @param xCheckAllMatchesY logical value (\code{TRUE} or \code{FALSE}) - should
#' function check whether all rows in \code{x} match some row in \code{y}
#' @param yCheckAllMatchesX logical value (\code{TRUE} or \code{FALSE}) - should
#' function check whether all rows in \code{y} match some row in \code{x}
#' @param rowsOrObservations "rows" or "observations" (abbreviations "r" and "o"
#' are accepted) - indicates how numbers of rows that do not match should be
#' reported: "rows" is suitable for data coming directly from input files
#' (adds 1 to reported numbers of rows to account that there is also a header
#' row) while "observations" for data that has already been processed
#' @return Data frame returned by \code{full_join(x, y)}.
#' @importFrom dplyr anti_join distinct full_join select
join_with_check <- function(x, y, xDescription = "x", yDescription = "y",
                            xCheckAllMatchesY = TRUE, yCheckAllMatchesX = TRUE,
                            rowsOrObservations = "rows") {
  stopifnot(is.data.frame(x), is.data.frame(y))
  stopifnot(is.character(xDescription), length(xDescription) == 1,
            is.character(yDescription), length(yDescription) == 1,
            is.logical(xCheckAllMatchesY), length(xCheckAllMatchesY) == 1,
            xCheckAllMatchesY %in% c(TRUE, FALSE),
            is.logical(yCheckAllMatchesX), length(yCheckAllMatchesX) == 1,
            yCheckAllMatchesX %in% c(TRUE, FALSE),
            is.character(rowsOrObservations), length(rowsOrObservations) == 1,
            rowsOrObservations %in% c("r", "o", "rows", "observations"))
  if (grepl("^r", rowsOrObservations)) {
    doNotMatchMessage <- "Numery wierszy (w pliku), których dotyczy ten problem"
    add <- 1
  } else {
    doNotMatchMessage <- "Numery obserwacji (wystąpień w zbiorze), których dotyczy ten problem"
    add <- 0
  }

  joiningBy = intersect(names(x), names(y))
  if (length(joiningBy) == 0) {
    stop("Nie można przyłączyć ", yDescription, " do ", xDescription,
         " bo w obu zbiorach nie ma wspólnych kolumn.", call. = FALSE)
  }
  cat("Łączenie zostanie dokonane na podstawie wartości zmiennej/zmiennych: '",
      paste(joiningBy, collapse = "', '"), "'.\n", sep = "")
  joiningByNotDistinctX = x %>%
    select(joiningBy) %>%
    distinct %>%
    nrow() != nrow(x)
  joiningByNotDistinctY = y %>%
    select(joiningBy) %>%
    distinct %>%
    nrow() != nrow(y)
  if (joiningByNotDistinctX & joiningByNotDistinctY) {
    stop(paste0("Łączenie nie może zostać przeprowadzone, bo ",
                ifelse(length(joiningBy) > 1, "kombinacje wartości ww. kolumn",
                       "wartości w ww. kolumnie"),
                " nie są unikalne zarówno w ", xDescription, ", jak i w ",
                yDescription, "."))
  }

  checkY <- suppressMessages(anti_join(y, x))
  if (nrow(checkY) > 0 & yCheckAllMatchesX) {
    warning(paste0("W ", yDescription, " występuje/ą ",
                   format(nrow(checkY), big.mark = "'"),
                   " wiersz(e/y), który/e nie ma(ją) odpowiednika w ",
                   xDescription, ".\n\n",
                   doNotMatchMessage, ":\n",
                   paste(strwrap(paste(as.numeric(rownames(checkY)) + add,
                                       collapse = ", "),
                                 prefix = " "),
                         collapse = "\n")),
            call. = FALSE, immediate. = TRUE)
    cat("\n")
  }
  checkX <- suppressMessages(anti_join(x, y))
  if (nrow(checkX) > 0 & xCheckAllMatchesY) {
    warning(paste0("W ", xDescription, " występuje/ą ",
                   format(nrow(checkX), big.mark = "'"),
                   " wiersz(e/y), który/e nie ma(ją) odpowiednika w ",
                   yDescription, ".\n\n",
                   doNotMatchMessage, ":\n",
                   paste(strwrap(paste(as.numeric(rownames(checkX)) + add,
                                       collapse = ", "),
                                 prefix = " "),
                         collapse = "\n")),
            call. = FALSE, immediate. = TRUE)
    cat("\n")
  }
  cat("Dane zostaną przyłączone do ",
      format(nrow(x) - nrow(checkX), big.mark = "'"), " (spośród ",
      format(nrow(x), big.mark = "'"), ") wierszy w ", xDescription, " i ",
      format(nrow(y) - nrow(checkY), big.mark = "'"), " (spośród ",
      format(nrow(y), big.mark = "'"), ") wierszy w ", yDescription, ".\n", sep = "")
  suppressMessages(full_join(x, y)) %>%
    return()
}
