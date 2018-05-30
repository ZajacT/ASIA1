#' @title Joining two data frames
#' @description Simple routine designed to find out (and report) whether two
#' data frames join each other 1:1 or not before performing left join.
#' @param x data frame
#' @param y data frame
#' @param xDescription optionally a description of what is \code{x} (for
#' purpose of providing informative warninig message)
#' @param yDescription optionally a description of what is \code{y}  (for
#' purpose of providing informative warninig message)
#' @return Data frame returned by \code{left_join(x, y)}.
#' @importFrom dplyr anti_join distinct left_join select
join_with_check <- function(x, y, xDescription = "x", yDescription = "y") {
  stopifnot(is.data.frame(x), is.data.frame(y))
  stopifnot(is.character(xDescription), length(xDescription) == 1,
            is.character(yDescription), length(yDescription) == 1)
  joiningBy = intersect(names(x), names(y))
  if (length(joiningBy) == 0) {
    stop("Nie można przyłączyć ", yDescription, " do ", xDescription,
         " bo w obu zbiorach nie ma wspólnych kolumn.", call. = FALSE)
  }
  cat("Łączenie zostanie dokonane na podstawie wartości zmiennej/zmiennych: '",
      paste(joiningBy, collapse = "', '"), "'.\n", sep = "")
  if (select(x, joiningBy) %>% distinct %>% nrow() != nrow(x)) {
    stop(paste0("Łączenie nie może zostać przeprowadzone, bo ",
                ifelse(length(joiningBy) > 1, "kombinacje wartości ww. kolumn",
                       "wartości w ww. kolumnie"),
                " nie są unikalne w ", xDescription, "."))
  }
  if (select(y, joiningBy) %>% distinct %>% nrow() != nrow(y)) {
    stop(paste0("Łączenie nie może zostać przeprowadzone, bo ",
                ifelse(length(joiningBy) > 1, "kombinacje wartości ww. kolumn",
                       "wartości w ww. kolumnie"),
                " nie są unikalne w ", yDescription, "."))
  }

  check <- suppressMessages(anti_join(y, x))
  if (nrow(check) > 0) {
    warning(paste0("W ", yDescription, " występuje ",
                   format(nrow(check), big.mark = "'"),
                   " wiersz(e/y), które nie mają odpowiednika w ",
                   xDescription,
                   ".\n\nNumery wierszy (w pliku), których dotyczy ten problem:\n",
                   paste(strwrap(paste(as.numeric(rownames(check)) + 1,
                                       collapse = ", "),
                                 prefix = " "),
                         collapse = "\n")),
            call. = FALSE, immediate. = TRUE)
    cat("\n")
  }
  check <- suppressMessages(anti_join(x, y))
  if (nrow(check) > 0) {
    warning(paste0("W ", xDescription, " występuje ",
                   format(nrow(check), big.mark = "'"),
                   " wiersz(e/y), które nie mają odpowiednika w ",
                   yDescription,
                   ".\n\nNumery wierszy (w pliku), których dotyczy ten problem:\n",
                   paste(strwrap(paste(as.numeric(rownames(check)) + 1,
                                       collapse = ", "),
                                 prefix = " "),
                         collapse = "\n")),
            call. = FALSE, immediate. = TRUE)
    cat("\n")
  }
  cat("Dane zostaną przyłączone do",
      format(nrow(x) - nrow(check), big.mark = "'"), "wierszy.\n")
  suppressMessages(left_join(x, y)) %>%
    return()
}
