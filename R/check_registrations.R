#' @title Checks if data on registretions are complete and correct
#' @description We should write something here
#' @param registrations optionally path to the file with data on registrations
#' @return if errors are detected: data frame with added columns indicating which observations should be fixed
#' if data are correct (corrected with the function) a data frame ready for further processing.
#' @importFrom dplyr mutate if_else filter
#' @export
check_registrations <- function(registrations = registrations) {
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------

  if (!("pesel" %in% names(registrations))) {
    stop(paste0("Zmienna 'pesel' nie występuje w danych o rekrutacjach."))
  }

  if (!("studia" %in% names(registrations))) {
    stop(paste0("Zmienna 'studia' nie występuje w danych o rekrutacjach."))
  }
  
  if (!("czy_oplacony" %in% names(registrations))) {
    stop(paste0("Zmienna 'czy_oplacony' nie występuje w danych o rekrutacjach."))
  }
  
  if (!("zakwalifikowany" %in% names(registrations))) {
    stop(paste0("Zmienna 'zakwalifikowany' nie występuje w danych o rekrutacjach."))
  }
  
  if (!("przyjety" %in% names(registrations))) {
    stop(paste0("Zmienna 'przyjety' nie występuje w danych o rekrutacjach."))
  } 

  #-----------------------------------------------------------------------------
  #|-> checks whether variables contain any illegal values and offers fixes
  #-----------------------------------------------------------------------------
  
  if(sum(!(registrations$przyjety %in% c("0", "1")) > 0)) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_przyj' wskazującą obserwacje wymagające poprawy ",
                  "Zamień wszystkie wartości inne niż '1' na '0'"),
                title = "Zmienna 'przyjety' nie może przyjmować wartości innych niż 0 lub 1."),
           registrations <- registrations %>% 
             mutate(blad_przyj = if_else(przyjety %in% c("0", "1"), 0, 1)),
           registrations <- registrations %>% 
             mutate(przyjety = if_else(przyjety %in% c("0", "1"), przyjety, "0"))
    )
  }

  if(sum(!(registrations$zakwalifikowany %in% c("0", "1")) > 0)) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_zakw' wskazującą obserwacje wymagające poprawy ",
                  "Zamień wszystkie wartości inne niż '1' na '0'"),
                title = "Zmienna 'zakwalifikowany' nie może przyjmować wartości innych niż 0 lub 1."),
           registrations <- registrations %>% 
             mutate(blad_zakw = if_else(zakwalifikowany %in% c("0", "1"), 0, 1)),
           registrations <- registrations %>% 
             mutate(zakwalifikowany = if_else(zakwalifikowany %in% c("0", "1"), zakwalifikowany, "0"))
    )
  }
  
  if(sum(!(registrations$czy_oplacony %in% c("0", "1")) > 0)) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_oplac' wskazującą obserwacje wymagające poprawy ",
                  "Zamień wszystkie wartości inne niż '1' na '0'",
                  "Usuń ze zbioru obserwacje bez opłaconej rekrutacji"),
                title = "Zmienna 'czy_oplacony' nie może przyjmować wartości innych niż 0 lub 1."),
           registrations <- registrations %>% 
             mutate(blad_oplac=if_else(czy_oplacony %in% c("0", "1"), 0, 1)),
           registrations <- registrations %>% 
             mutate(czy_oplacony = if_else(czy_oplacony %in% c("0", "1"), czy_oplacony, "0")),
           registrations <- registrations %>%
             filter(czy_oplacony %in% "1")
    )
  }
  
  if(sum((registrations$przyjety %in% "1") & !(registrations$zakwalifikowany %in% "1")) > 0) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_prz_zak' wskazującą obserwacje wymagające poprawy ",
                  "Skoryguj wartości zmiennej 'zakwalifikowany' - wstaw '1', jeśli 'przyjety' jest równa '1'."),
                title = "W zbiorze występują osoby przyjęte, ale nie zakwalifikowane na studia"),
           registrations <- registrations %>% 
             mutate(blad_prz_zak = if_else((przyjety %in% "1") & !(zakwalifikowany %in% "1"), 1, 0)),
           registrations <- registrations %>% 
             mutate(zakwalifikowany = if_else(przyjety %in% "1", "1", zakwalifikowany))
    )
  }
  
  if(sum((registrations$zakwalifikowany %in% "1") & !(registrations$czy_oplacony %in% "1")) > 0) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_zak_opl' wskazującą obserwacje wymagające poprawy ",
                  "Skoryguj wartości zmiennej 'czy_oplacony' - wstaw '1', jeśli 'zakwalifikowany' jest równa '1'."),
                title = "W zbiorze występują osoby zakwalifikowane, które nie opłaciły rekrutajci."),
           registrations <- registrations %>% 
             mutate(blad_zak_opl = if_else((zakwalifikowany %in% "1") & !(czy_oplacony %in% "1"), 1, 0)),
           registrations <- registrations %>% 
             mutate(czy_oplacony = if_else(zakwalifikowany %in% "1", "1", czy_oplacony))
    )
  }
  
  return(registrations)
}