#' @title Checks if data on exams are complete and correct
#' @description Function checks if data on exams are complete and
#' correct. If no critical eroors are detected, functions gives user a choice
#' whether to correct detected minor issues detected in the data, or to add
#' columns indicating which observations should be fixed. Finally it returns
#' a data frame with corrected data or columns indicating problems.
#' @param exams data frame with data regarding exams
#' @return data frame
#' @importFrom dplyr mutate if_else filter
check_exams <- function(exams = exams) {
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------

  reqVarNames <- c("pesel", "egzamin", "wynik_p","wynik_r")
  missVarNames  <- setdiff(reqVarNames, names(exams))
  if (length(missVarNames) > 0) {
    stop(paste0("W danych o wynikach egzaminów ",
                ifelse(length(missVarNames) > 1, "zmiennych", "zmiennej"), ": '",
                paste0(missVarNames, collapse = "', '"),
                "'."), call. = FALSE)
  }

  #-----------------------------------------------------------------------------
  #|-> checks whether variables contain any illegal values and offers fixes
  #-----------------------------------------------------------------------------
  
  if(!all(grepl("^M_",exams$egzamin))) {
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