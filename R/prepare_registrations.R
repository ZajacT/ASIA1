#' @title Preparing recrutation data for analysis
#' @description Function creates a corrected dataset on registrations that is
#' suitable for final analysis. Operations: checking against USOS records on
#' acepted applicants, checking and correcting multiple registrations i.e. cases
#' that an applicant applies more than one time to a given program, adding data
#' on scores to the data on registrations.
#' @param registrations optionally a path to the file with data on registrations
#' @param scores optionally a path to the file with data on recruitment scores
#' @param output optionally a path to the file in which results will be saved
#' @param usosAdmission optionally a path to the file with USOS data on admissions
#' @param recRegistrations optionally a path to the file with instructions to
#' alter programme codes in IRK data
#' @param recUsos optionally a path to the file with instructions to alter
#' programme codes in USOS data
#' @return
#' \itemize{
#'   \item{If problems were found in registrations data or recruitment scores
#'   data and user choose to tag (at least some of) them, function returns
#'   invisibly a list with two elements that are data frames:
#'   \code{registrations} and \code{scores}. These are data frames containing
#'   input data with tagged problems (and possibly corrected other problems,
#'    that user choose not to tag, but to correct).}
#'   \item{In any other case function returns invisibly a data frame with
#'   corrected data on registrations.}
#' }
#' @importFrom dplyr group_by mutate n summarise filter semi_join
#' @importFrom utils write.csv2
#' @importFrom rlang ensym
#' @export
prepare_registrations <- function(registrations = NULL, scores = NULL,
                                  output = NULL, usosAdmission = NULL,
                                  recRegistrations = NULL, recUsos = NULL) {
  #-----------------------------------------------------------------------------
  #|-> Here is the begining of data import and checks
  #-----------------------------------------------------------------------------
  cat("--------------------\n")
  taggedProblems = FALSE

  if (is.null(registrations)) {
    registrations <- choose_file(" z danymi o rekrutacjach")
  }
  check_input_path(registrations, "registrations")
  registrations <- read_file(registrations)
  cat("---\nSprawdzanie poprawności danych o rekrutacjach.\n\n")
  registrationsChecked <- check_registrations(registrations)
  if (ncol(registrationsChecked) > ncol(registrations)) {
    cat("---\n")
    file <- choose_file(", w którym mają zostać zapisane dane o rekrutacjach z oznaczonymi problemami (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
                        errorOnCancel = FALSE)
    if (is.na(file)) {
      warning("Dane nie zostaną zapisane do pliku, ponieważ nie podano jego nazwy.",
              call. = FALSE, immediate. = TRUE)
    } else {
      write.csv2(registrationsChecked, file, row.names = FALSE, na = "",
                 fileEncoding = "UTF-8")
      cat("Zapisano dane do pliku '", file, "'.\n", sep = "")
    }
    taggedProblems <-  TRUE
  }
  registrations = registrationsChecked
  rm(registrationsChecked)
  cat("--------------------\n")

  if (is.null(scores)) {
    scores <- choose_file(" z danymi o punktach rekrutacyjnych")
  }
  check_input_path(scores, "scores")
  scores <- read_file(scores)
  cat("---\nSprawdzanie poprawności danych o punktach rekrutacyjnych.\n\n")
  scoresChecked <- check_scores(scores)
  if (ncol(scoresChecked) > ncol(scores)) {
    cat("---\n")
    file <- choose_file(", w którym mają zostać zapisane dane o punktach rekrutacyjnych z oznaczonymi problemami (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
                        errorOnCancel = FALSE)
    if (is.na(file)) {
      warning("Dane nie zostaną zapisane do pliku, ponieważ nie podano jego nazwy.",
              call. = FALSE, immediate. = TRUE)
    } else {
      write.csv2(scoresChecked, file, row.names = FALSE, na = "",
                 fileEncoding = "UTF-8")
      cat("Zapisano dane do pliku '", file, "'.\n", sep = "")
    }
    taggedProblems <-  TRUE
  }
  scores = scoresChecked
  rm(scoresChecked)

  if (taggedProblems) {
    return(invisible(list(registrations = registrations,
                          scores = scores)))
  }

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
  cat("--------------------\n",
      "Sprawdzanie poprawności połączonych danych o rekrutacjach i o punktach rekrutacyjnych.\n\n", sep = "")
  registrations = check_registrations_with_scores(registrations)
  
  #-----------------------------------------------------------------------------
  #|-> Here starts the merging of IRK and USOS records
  #-----------------------------------------------------------------------------
  cat("--------------------\n")

  if (is.null(recUsos)) {
    cat("---\n")
    switch(menu(c("Wykorzystywane mają być tylko dane z IRK.",
                  "Wykorzystywane mają być dane o przyjęciach pochodzące z USOS."),
                title = "Czy obliczenia będą prowadzone tylko z wykorzystaniem danych IRK, czy też dane o przyjęciach pochodzą z USOS."),
           mergeType <- 1,
           mergeType <- 2)
  } else if (is.na(recUsos)) {
    mergeType <- 1
    gruoupingVar <- "studia"
  }
  if (mergeType == 1) {
    gruoupingVar <- "studia"
    if (is.null(recRegistrations) ) {
      cat("---\n")
      switch(menu(c("Nie",
                    "Tak (konieczne będzie wczytanie pliku ze słownikiem"),
                  title = "Czy kody programów studiów mają zmienione z wykorzystaniem słownika"),
             recIRK <- 1,
             recIRK <- 2)
    }
    
  }
  
  if (is.null(recRegistrations) & (mergeType == 2 | recIRK == 2)) {
    recRegistrations <- choose_file(" z instrukcją przekształcenia kodów studiów w danych IRK")
    check_input_path(recRegistrations, "recRegistrations")
    recRegistrations <- read_file(recRegistrations, columnsToCharacter = FALSE)
    check_variable_names(recRegistrations,
                         c("studia","studia_rec"),
                         "słowniku do przekodowywania kodów IRK")
    recRegistrations <- recRegistrations %>%
      select(studia, studia_rec)
  }
  
  if (mergeType == 2) {
    gruoupingVar <- "studia_rec"
    
    if (is.null(usosAdmission)) {
      usosAdmission <- choose_file(" z danymi o przyjęciach na studiach weksportowanymi z USOS")
    }
    check_input_path(usosAdmission, "usosAdmission")
    usosAdmission <- read_file(usosAdmission)
    check_variable_names(usosAdmission,
                         as.character(c("PESEL","Program","Etap")),
                         "zbiorze z danymi o przyjęciach na studiach weksportowanymi z USOS")

    if (is.null(recUsos)) {
      recUsos <- choose_file(" z instrukcją przekształcenia kodów studiów w danych USOS")
    }
    check_input_path(recUsos, "recUsos")
    recUsos <- read_file(recUsos, columnsToCharacter = FALSE)
    check_variable_names(recUsos,
                         c("program", "etap", "studia_rec"),
                         "słowniku do przekodowywania danych USOS")
    recUsos <- recUsos %>%
      select(program, etap, studia_rec)
  }

  #-----------------------------------------------------------------------------
  #|-> Recoding programme codes in data on registrations
  #-----------------------------------------------------------------------------
  if (mergeType == 2 | recIRK == 2) {
    cat("--------------------\n",
        "Przekodowywanie kodów studiów w pliku z danymi o rekrutacjach.\n---\n",
        sep = "")
    registrations <- join_with_check(registrations,
                                     suppressMessages(semi_join(recRegistrations,
                                                                registrations)),
                                   "danych o rekrutacjach",
                                   "zmienionych kodach IRK")
  }
  #-----------------------------------------------------------------------------
  #|-> Here programme codes are recoded and USOS data replace IRK data on enrolment
  #-----------------------------------------------------------------------------
  if (mergeType == 2) {
    cat("--------------------\n",
        "Przekodowywanie kodów studiów w pliku z danymi USOS o przyjęciach.\n---\n",
        sep = "")
    usosAdmission <- join_with_check(usosAdmission,
                                     suppressMessages(semi_join(recUsos,
                                                                usosAdmission)),
                                     "danych z USOS o przyjęciach",
                                     "zmienionych kodach USOS") %>%
      select("pesel","studia_rec") %>%
      mutate(przyjety = 1) %>%
      group_by(studia_rec) %>%
      summarise(
        przyjetyUsos = sum(przyjety %in% "1") # a person can apply to a double programme which means being a student at two programmes.
      ) %>%
      ungroup()
    cat("---\n")
    registrations <- join_with_check(registrations,
                                     suppressMessages(semi_join(usosAdmission,
                                                                registrations)),
                                     "danych o rejestracjach z IRK",
                                     "danych o przyjęciach z USOS") %>%
      mutate(przyjetyUsos = ifelse(is.na(przyjetyUsos), "0", przyjetyUsos)) %>%
      mutate(przyjetyUsos = ifelse((przyjetyUsos > 0 & zakwalifikowany %in% "0" ),
                                   "0", przyjetyUsos ))
  }
  #-----------------------------------------------------------------------------
  #|-> Here the merging of the records starts
  #-----------------------------------------------------------------------------
  cat("--------------------\nPrzekształcanie danych.\n")
  gruoupingVar <- ensym(gruoupingVar)
  dataOnRegistrations <- registrations %>%
    group_by(!!gruoupingVar,pesel) %>%
    summarise(
      REJ = sum(czy_oplacony %in% "1"), # how many times an applicant registered
      ZAK = sum(zakwalifikowany %in% "1"), # how many times an applicant was accepted
      PRZ = sum(przyjety %in% "1"), # how many times an applicant enrolled
      PKT = suppressWarnings(
        max(wynik[!is.na(wynik)])) # the highest achieved score
    ) %>%
    ungroup()
  #-----------------------------------------------------------------------------
  #|-> Here writing results to a file starts
  #-----------------------------------------------------------------------------
  cat("--------------------\n")
  if (is.null(output)) {
    output <- choose_file(", w którym mają zostać zapisane dane o rekrutacjach (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
                          errorOnCancel = FALSE)
  }
  cat("---\nZapisywanie przygotowanego pliku z danymi o rekrutacjach.\n")
  if (is.na(output)) {
    warning("Dane nie zostaną zapisane do pliku, ponieważ nie podano jego nazwy.",
            call. = FALSE, immediate. = TRUE)
  } else {
    write.csv2(dataOnRegistrations, output, row.names = FALSE, na = "",
               fileEncoding = "UTF-8")
    cat("Zapisano dane do pliku '", output, "'.\n", sep = "")
  }
  invisible(dataOnRegistrations)
}
