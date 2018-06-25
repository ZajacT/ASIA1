#' @title Checks if data on scores are complete and correct
#' @description We should write something here
#' @param registrations optionally path to the file with data on scores
#' @return if errors are detected: data frame with added columns indicating which observations should be fixed
#' if data are correct (corrected with the function) a data frame ready for further processing.
#' @importFrom dplyr mutate if_else filter
check_scores <- function(scores = scores) {
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------

  if (!("pesel" %in% names(scores))) {
    stop(paste0("Zmienna 'pesel' nie występuje w danych o punktach rekrutacyjnych"))
  }

  if (!("studia" %in% names(scores))) {
    stop(paste0("Zmienna 'studia' nie występuje w danych o punktach rekrutacyjnych"))
  }
  
  if (!("wynik" %in% names(scores))) {
    stop(paste0("Zmienna 'wynik' nie występuje w danych o punktach rekrutacyjnych"))
  }

  #-----------------------------------------------------------------------------
  #|-> checks whether variables contain any illegal values and offers fixes
  #-----------------------------------------------------------------------------
  
  if(sum(is.na(scores$wynik) | grepl("^-",scores$wynik))>0) {
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