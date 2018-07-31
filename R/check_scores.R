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
#' @importFrom dplyr mutate if_else
#' @importFrom utils menu
#' @export
check_scores <- function(scores) {
  stopifnot(is.data.frame(scores))
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------
  check_variable_names(scores,
                       c("pesel", "studia", "wynik"),
                       "danych o punktach rekrutacyjnych")
  #-----------------------------------------------------------------------------
  #|-> checks whether variables contain any illegal values and offers fixes
  #-----------------------------------------------------------------------------
  scores$wynik = as.character(scores$wynik)
  problems = is.na(scores$wynik) | grepl("^-", scores$wynik) | grepl("^NULL$", scores$wynik)
  if (any(problems)) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_wynik', wskazującą obserwacje wymagające poprawy.",
                  "Zamień wszystkie wartości inne niż liczby równe lub większe od 0 na braki danych."),
                title = "W danych o punktach rekrutacyjnych zmienna 'wynik' nie może przyjmować wartości mniejszych niż 0."),
           scores <- scores %>%
             mutate(blad_wynik = if_else(problems, 1, 0)),
           scores <- scores %>%
             mutate(wynik = if_else(problems, NA_character_, wynik))
    )
  }
  #-----------------------------------------------------------------------------
  #|-> coverts `wynik` to numeric
  #-----------------------------------------------------------------------------
  scores %>%
    mutate(wynik = as.numeric(wynik)) %>%
    return()
}
