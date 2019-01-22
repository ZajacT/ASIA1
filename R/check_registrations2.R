#' @title Checks if data on registretions are complete and correct
#' @description Function checks if data on registrations are complete and
#' correct. If no critical eroors are detected, functions gives user a choice
#' whether to correct detected minor issues detected in the data, or to add
#' columns indicating which observations should be fixed. Finally it returns
#' a data frame with corrected data or columns indicating problems.
#' @param registrations data frame with data regarding registrations
#' @return data frame
#' @importFrom dplyr mutate if_else filter
#' @importFrom utils menu
#' @export
check_registrations2 <- function(registrations) {
  stopifnot(is.data.frame(registrations))
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------
  check_variable_names(registrations,
                       c("pesel", "studia", "czy_oplacony"),
                       "danych o rekrutacjach")
  #-----------------------------------------------------------------------------
  #|-> checks whether a variable contains any illegal values and offers fixes
  #-----------------------------------------------------------------------------
  if (!all(registrations$czy_oplacony %in% c("0", "1"))) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_oplac', wskazującą obserwacje wymagające poprawy.",
                  "Zamień wszystkie wartości inne niż '1' na '0'.",
                  "Usuń ze zbioru obserwacje bez opłaconej rekrutacji."),
                title = "Zmienna 'czy_oplacony' nie może przyjmować wartości innych niż 0 lub 1."),
           registrations <- registrations %>%
             mutate(blad_oplac = if_else(czy_oplacony %in% c("0", "1"), 0, 1)),
           registrations <- registrations %>%
             mutate(czy_oplacony = if_else(czy_oplacony %in% c("0", "1"), czy_oplacony, "0")),
           registrations <- registrations %>%
             filter(czy_oplacony %in% "1")
    )
  }
  
  return(registrations)
}
