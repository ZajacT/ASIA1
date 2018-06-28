#' @title Preparing recrutation data for analysis
#' @description Function creates a corrected dataset on registrations that
#' is suitable for final analysis. Operations: checking against 
#' USOS records on acepted applicants, checking and correcting multiple registrations 
#' i.e. cases that an applicant applies more than one time to a given program, 
#' adding data on scores to the data on registrations.
#' @return Data frame with corrected data on registrations.
#' @examples
#' @importFrom dplyr group_by mutate n summarise filter
#' @importFrom rlang ensym
#' @importFrom utils write.csv2
#' @export
prepare_data <- function(registrations = NULL, scores = NULL, 
                         usosAdmitssion = NULL, recRegistrations = NULL,
                         recUsos = NULL) {
  
  #-----------------------------------------------------------------------------
  #|-> Here users have to determine if they want to use USOS data on enrolment. 
  #-----------------------------------------------------------------------------
  
  switch(menu(c("Wykorzystywane mają być tylko dane z IRK.",
                "Wykorzystywane mają być dane o przyjęciach pochodzące z USOS."),
              title = "Czy obliczenia będą prowadzone tylko z wykorzystaniem danych IRK,
              czy też daen o przyjęciach pochodzą z USOS."),
         mergeType <- 1,
         mergeType <- 2
         )
  
  #-----------------------------------------------------------------------------
  #|-> Here is the begining of data import and checks
  #-----------------------------------------------------------------------------
  
  # to do
  # myślałem, żeby tutaj były funkcje od sprawdzania, ale nie bardzo wiem, jak to zrobić, żeby ewentualnie można było zapisać zbiory do poprawki
  
  #-----------------------------------------------------------------------------
  #|-> Data merging begins here
  #-----------------------------------------------------------------------------
  
  cat("--------------------\n",
      "Łączenie pliku z danymi o rekrutacjach z danymi o punktach rekrutacyjnych.\n",
      sep = "")
  registrations <- join_with_check(registrations, scores,
                                   "danych o rekrutacjach",
                                   "danych o punktach rekrutacyjnych")
  
  #-----------------------------------------------------------------------------
  #|-> Here the consistency of data on scores is checked
  #-----------------------------------------------------------------------------

# a może z tego zrobić osobną funkcję - i tak nie do końca umiem to napisać.
  
  registrations <- registrations %>%
    group_by(studia) %>%
    # mutating
    mutate(wynik = suppressWarnings(as.numeric(wynik)),
           # below Inf will be assigned to groups
           # with no (qualified candidates with non-missing scores)
           MINWYN = suppressWarnings(
             min(wynik[zakwalifikowany %in% "1" & !is.na(wynik)]))
    ) %>%
    # summarising
    summarise(
      MINWYN = ifelse(is.finite(MINWYN[1]), MINWYN[1], NA),
      NBLZAKPKT = sum(zakwalifikowany %in% "1" & wynik >= MINWYN & !is.na(wynik))
    ) %>%
    ungroup()
  
  if (any(registrations$NBLZAKPKT > 0)) {
    switch(menu(c("Zignoruj to.",
                  "Wszystkim osobom o wyniku wyższym niż ostatni zakwalifikowany przypisz zakwalifikowanie, o ile rejestracja została opłacona."),
                title = "W zbiorze występują osoby niezakwalifikowane mimo wyższej liczby punktów niż ostatnia osoba zakwalifikowana."),
           registrations <- registrations %>%
             select (-MINWYN,-NBLZAKPKT),
           registrations <- registrations %>%
             mutate ( zakwalifikowany = if_else (NBLZAKPKT > 0 & czy_oplacony %in% 1, 1, zakwalifikowany)) %>%
             select (-MINWYN,-NBLZAKPKT)
    )
  }

  #-----------------------------------------------------------------------------
  #|-> Recoding programme codes in data on registrations
  #-----------------------------------------------------------------------------
  
  cat("--------------------\n",
      "Przekodowywanie kodów studiów w pliku z danymi o rekrutacjach.\n",
      sep = "")
  registrations <- join_with_check(registrations, recRegistrations,
                                   "danych o rekrutacjach",
                                   "zmienionych kodów IRK")
  
  #-----------------------------------------------------------------------------
  #|-> Here programme codes are recoded and USOS data replace IRK data on enrolment
  #-----------------------------------------------------------------------------
  
  if (mergeType == 2) {

## wyobrażam sobie, że mogą być nadmiarowe wiersze w pliku z USOS i one nie mają odpowiednika w słowniku
## możemy je zignorować. pytanie, czy join_with_check ma tutaj sens? 
    
    cat("--------------------\n",
        "Przekodowywanie kodów studiów w pliku z danymi USOS o przyjęciach.\n",
        sep = "")
    usosAdmitssion <- join_with_check(usosAdmitssion, recUsos,
                                     "danych o USOS o przyjęciach",
                                     "zmienionych kodów USOS") %>%
      select ("pesel","studia_rec") %>%
      mutate ( przyjety = 1) %>% 
      group_by (studia_rec) %>%
      summarise(
        przyjetyUsos = sum(przyjety %in% "1") # a person can apply to a double programme which means being a student at two programmes.
      ) %>%
      ungroup()
    
## tutaj to by mi pasował left_join bardziej    
    registrations <- join_with_check(registrations, usosAdmitssion,
                                     "damych o rejestracjach z IRK",
                                     "danych o przyjęciach z USOS") %>%
      mutate ( przyjetyUsos = if_else ( is.na(przyjetyUsos),0, przyjetyUsos)) %>%
      mutate ( przyjetyUsos = if_else ((przyjetyUsos > 0 & zakwalifikowany %in% "0" ),"0",przyjetyUsos ))
  }
  
  #-----------------------------------------------------------------------------
  #|-> Here the merging of the records starts
  #-----------------------------------------------------------------------------

  dataOnRegistrations <- registrations %>% 
    group_by("pesel","studia_rec") %>%
    summarise(
      REJ = sum(czy_oplacony %in% "1"), # how many times an applicant registered
      ZAK = sum(zakwalifikowany %in% "1"), # how many times an applicant was accepted 
      PRZ = sum(przyjety %in% "1"), # how many times an applicant enrolled
      PKT = suppressWarnings(
        max(wynik[!is.na(wynik)])) # the highest achieved score
    ) %>%
    ungroup()

  invisible(dataOnRegistrations)
}
