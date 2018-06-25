#' @title Checks if data on scores are complete and correct
#' @description Function checks if data on scores are complete and correct.
#' If no critical eroors are detected, functions gives user a choice whether to
#' correct detected minor issues detected in the data, or to add columns
#' indicating which observations should be fixed. Finally it returns a data
#'  frame with corrected data or columns indicating problems.
#' @param scores optionally path to the file with data on scores
#' @return if errors are detected: data frame with added columns indicating
#' which observations should be fixed if data are correct (corrected with the
#' function) a data frame ready for further processing.
#' @importFrom dplyr mutate if_else filter
#' @importFrom utils menu
#' @export
check_scores <- function(scores = scores) {
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------

  reqVarNames <- c("pesel", "studia", "wynik")
  missVarNames  <- setdiff(reqVarNames, names(scores))
  if (length(missVarNames) > 0) {
    stop(paste0("W danych o punktach rekrutacyjnych brakuje ",
                ifelse(length(missVarNames) > 1, "zmiennych", "zmiennej"), ": '",
                paste0(missVarNames, collapse = "', '"),
                "'."), call. = FALSE)
  }

  #-----------------------------------------------------------------------------
  #|-> checks whether variables contain any illegal values and offers fixes
  #-----------------------------------------------------------------------------

  if (any(is.na(scores$wynik) | grepl("^-",scores$wynik))) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_wynik' wskazującą obserwacje wymagające poprawy ",
                  "Zamień wszystkie wartości inne niż liczby równe lub większe od 0 na braki danych"),
                title = "Zmienna 'wynik' nie może przyjmować wartości mniejszych niż 0"),
           scores <- scores %>%
             mutate(blad_wynik = if_else(is.na(wynik) | grepl("^-",wynik), 0, 1)),
           scores <- scores %>%
             mutate(wynik = if_else(is.na(wynik) | grepl("^-",wynik) < 0, wynik, NA))
    )
  }

  return(scores)
}
