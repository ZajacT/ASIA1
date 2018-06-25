#' @title Checks if data on exams are complete and correct
#' @description We should write something here
#' @param registrations optionally path to the file with data on exams
#' @return if errors are detected: data frame with added columns indicating which observations should be fixed
#' if data are correct (corrected with the function) a data frame ready for further processing.
#' @importFrom dplyr mutate if_else filter
check_exams <- function(exams = exams) {
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------

  if (!("pesel" %in% names(exams))) {
    stop(paste0("Zmienna 'pesel' nie występuje w danych o wynikach egzaminów"))
  }

  if (!("egzamin" %in% names(exams))) {
    stop(paste0("Zmienna 'egzamin' nie występuje w danych o wynikach egzaminów"))
  }
  
  if (!("wynik_p" %in% names(exams))) {
    stop(paste0("Zmienna 'wynik_p' nie występuje w danych o punktach rekrutacyjnych"))
  }
  
  if (!("wynik_r" %in% names(exams))) {
    stop(paste0("Zmienna 'wynik_r' nie występuje w danych o punktach rekrutacyjnych"))
  }

  #-----------------------------------------------------------------------------
  #|-> checks whether variables contain any illegal values and offers fixes
  #-----------------------------------------------------------------------------
  
  if(sum(!grepl("^M_",exams$egzamin))>0) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_egzamin' wskazującą obserwacje wymagające poprawy ",
                  "Usuń ze zbioru wszystkie wyniki egzaminów, których opis nie zaczyna się od 'M_'"),
                title = "Nie wszystkie opisy egzaminów w zmiennej 'egzamin' zaczynają się od 'M_'"),
           exams <- exams %>% 
             mutate(blad_wynik = if_else(grepl("^M_",exams$egzamin), 0, 1)),
           exams <- exams %>% 
             filter(grepl("^M_",exams$egzamin))
    )
  }
  
  return(exams)
}