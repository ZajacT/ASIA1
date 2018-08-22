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
#'   invisibly a data frame with tagged (and possibly corrected with respect to
#'   other problems, that user chose to correct, not to tag) dataset.}
#'   \item{In any other case function returns invisibly a data frame with
#'   corrected data on registrations.}
#' }
#' @importFrom dplyr filter group_by mutate n one_of rename semi_join summarise
#' @importFrom utils write.csv2
#' @examples
#' \dontrun{
#' prepare_registrations("inst/pr_zapisy.xlsx", "inst/pr_wyniki.xlsx",
#'                       "dane/dane-IRK poprawione.csv", "inst/pr_usos.xlsx",
#'                       "inst/pr_rek_irk.xlsx", "inst/pr_rek_usos.xlsx")
#' }
#' @export
prepare_registrations <- function(registrations = NULL, scores = NULL,
                                  output = NULL, usosAdmission = NULL,
                                  recRegistrations = NULL, recUsos = NULL) {
  #-----------------------------------------------------------------------------
  #|-> Here is the begining of data import and checks
  #-----------------------------------------------------------------------------
  cat("--------------------\n")
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
    return(invisible(registrationsChecked))
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
    return(invisible(scoresChecked))
  }
  scores = scoresChecked %>%
    select(-one_of(setdiff(intersect(names(scores), names(registrations)),
                           c("pesel", "studia"))))
  rm(scoresChecked)
  #-----------------------------------------------------------------------------
  #|-> Data merging begins here
  #-----------------------------------------------------------------------------
  cat("--------------------\n",
      "Łączenie pliku z danymi o rekrutacjach z danymi o punktach rekrutacyjnych.\n",
      sep = "")
  registrations <- join_with_check(registrations,
                                   suppressMessages(semi_join(scores,
                                                              registrations)),
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
  } else {
    mergeType <- 2
  }
  if (mergeType == 1) {
    if (is.null(recRegistrations)) {
      cat("---\n")
      switch(menu(c("Nie",
                    "Tak (konieczne będzie wczytanie pliku ze słownikiem"),
                  title = "Czy kody programów studiów mają zostać zmienione z wykorzystaniem słownika"),
             recIRK <- 1,
             recIRK <- 2)
    } else if (is.na(recRegistrations)) {
      recIRK <- 1
    } else {
      recIRK <- 2
    }
  } else {
    recIRK <- 2
  }

  if (recIRK == 2) {
    if (is.null(recRegistrations)) {
      recRegistrations <- choose_file(" z instrukcją przekształcenia kodów studiów w danych IRK")
      check_input_path(recRegistrations, "recRegistrations")
    }
    recRegistrations <- read_file(recRegistrations, columnsToCharacter = FALSE)
    check_variable_names(recRegistrations,
                         c("studia", "studia_rec"),
                         "słowniku do przekodowywania kodów IRK")
    recRegistrations <- recRegistrations %>%
      select(studia, studia_rec)
  }

  if (mergeType == 2) {
    if (is.null(usosAdmission)) {
      usosAdmission <- choose_file(" z danymi o przyjęciach na studiach weksportowanymi z USOS")
    }
    check_input_path(usosAdmission, "usosAdmission")
    usosAdmission <- read_file(usosAdmission)
    check_variable_names(usosAdmission,
                         c("pesel", "program", "etap"),
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
  if (recIRK == 2) {
    cat("--------------------\n",
        "Przekodowywanie kodów studiów w pliku z danymi o rekrutacjach.\n",
        "---\nŁączenie danych o rejestracjach z instrukcją przekształcenia kodów studiów w danych IRK.\n",
        sep = "")
    registrations <- join_with_check(registrations,
                                     suppressMessages(semi_join(recRegistrations,
                                                                registrations)),
                                   "danych o rekrutacjach",
                                   "zmienionych kodach IRK")
    irkMer <- check_rec_joining(registrations, recRegistrations,
                                "pliku z rekrutacjami", "słowniku kodów IRK")
    if (irkMer == 2) {
      registrations <- registrations %>%
        mutate(studia_rec = ifelse(is.na(studia_rec), studia, studia_rec))
    }
    registrations <- registrations %>%
      select(-studia) %>%
      rename(studia = studia_rec) %>%
      filter(!is.na(studia))
  }
  #-----------------------------------------------------------------------------
  #|-> Here programme codes are recoded and USOS data replace IRK data on enrolment
  #-----------------------------------------------------------------------------
  if (mergeType == 2) {
    cat("--------------------\n",
        "Przekodowywanie kodów studiów w pliku z danymi USOS o przyjęciach.\n",
        "---\nŁączenie danych o przyjęciach z instrukcją przekształcenia kodów studiów w danych USOS.\n",
        sep = "")
    usosAdmission <- join_with_check(usosAdmission,
                                     suppressMessages(semi_join(recUsos,
                                                                usosAdmission)),
                                     "danych z USOS o przyjęciach",
                                     "zmienionych kodach USOS")
    usosMer <- check_rec_joining(usosAdmission, recUsos,
                                 "pliku z przyjętymi wg USOS",
                                 "słowniku kodów USOS")
    if (usosMer == 2) {
      usosAdmission <- usosAdmission %>%
        mutate(studia_rec = ifelse(is.na(studia_rec), program, studia_rec))
    }
    usosAdmission <- usosAdmission %>%
      select(pesel, studia = studia_rec) %>%
      filter(!is.na(studia)) %>%
      mutate(przyjety = 1) %>%
      group_by(pesel, studia) %>%
      summarise(
        przyjetyUsos = sum(przyjety %in% "1") # a person can apply to a double programme which means being a student at two programmes.
      ) %>%
      ungroup()
    cat("---\nŁączenie danych o przyjęciach z danymi o rejestracjach.\n")
    registrations <- join_with_check(registrations,
                                     suppressMessages(semi_join(usosAdmission,
                                                                registrations)),
                                     "danych o rejestracjach z IRK",
                                     "danych o przyjęciach z USOS") %>%
      mutate(przyjetyUsos = ifelse(is.na(przyjetyUsos), "0", przyjetyUsos)) %>%
      mutate(przyjety = ifelse(przyjetyUsos > 0 & zakwalifikowany %in% "0" ,
                                   "0", przyjetyUsos ))
  }
  #-----------------------------------------------------------------------------
  #|-> Here the merging of the records starts
  #-----------------------------------------------------------------------------
  cat("--------------------\nPrzekształcanie danych.\n")
  dataOnRegistrations <- registrations %>%
    group_by(studia,pesel) %>%
    summarise(
      rej = sum(czy_oplacony %in% "1"), # how many times an applicant registered
      zak = sum(zakwalifikowany %in% "1"), # how many times an applicant was accepted
      prz = suppressWarnings(sum(as.numeric(przyjety))), # how many times an applicant enrolled
      pkt = suppressWarnings(
        max(wynik[!is.na(wynik)])) # the highest achieved score
    ) %>%
    mutate(pkt = ifelse(pkt == -Inf, NA, pkt)) %>%
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
