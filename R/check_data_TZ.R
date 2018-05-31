#' @title Checking the correctness of admissions data
#' @description We should write something here
#' @param groupingVariable optionally name of a grouping variable that should be
#' used in analysis given as a string or as a name/symbol
#' @param registrations optionally path to the file with data on registrations
#' @param scores optionally path to the file with data on recruitment scores
#' @param exams optionally path to the file with data on examination scores
#' @param dictionary optionally path to the file with data containing additional
#' informations about registrations
#' @param baseGroupingVariable name of (to be described) variable given as
#' a string or as a name/symbol
#' @details
#' Location of files contaninig the data to be checked can be described
#' noninteractively with function arguments described above or - if any of this
#' arguments is omitted - interactively with a system file-selection dialog.
#' @return Function returns nothing.
#' @examples
#' \dontrun{
#'   check_data()
#' }
#' @importFrom dplyr group_by mutate n summarise
#' @importFrom rlang ensym
#' @export
check_data <- function(groupingVariable, registrations = NULL, scores = NULL,
                       exams = NULL, dictionary = NULL,
                       baseGroupingVariable = "studia") {
  if (is.null(registrations)) {
    registrations <- choose_file("o rejestracjach")
  }
  check_path(registrations, "registrations")
  registrations <- read_file(registrations)

  if (is.null(scores)) {
    scores <- choose_file("o punktach rekrutacyjnych")
  }
  check_path(scores, "scores")
  scores <- read_file(scores)

  if (is.null(exams)) {
    exams <- choose_file("o wynikach egzaminów")
  }
  check_path(exams, "exams")
  exams <- read_file(exams)

  if (is.null(dictionary)) {
    dictionary <- choose_file("o słowniku")
  }
  check_path(dictionary, "dictionary")
  dictionary <- read_file(dictionary)

  errorGroupingVariableFormatMessage = "Zmienna grupująca musi zostać podana jako ciąg znaków (jednoelementowy wektor typu character) lub jako wyrażenie (nazwa zmiennej nie ujęta w cudzysłów)."
  groupingVariable <-
    tryCatch(ensym(groupingVariable),
             error = function(e) {
               stop(paste0("Nieprawidłowy format argumentu groupingVariable.\n",
                           errorGroupingVariableFormatMessage))})
  if (!(as.character(groupingVariable) %in% names(registrations))) {
    stop(paste0("Zmienna grupująca podana argumentem groupingVariable ('",
                groupingVariable, "') nie występuje w danych o rejestracjach."))
  }
  if (!(as.character(groupingVariable) %in% names(dictionary))) {
    stop(paste0("Zmienna grupująca podana argumentem groupingVariable ('",
                groupingVariable, "') nie występuje w słowniku kierunków."))
  }
  baseGroupingVariable <-
    tryCatch(ensym(baseGroupingVariable),
             error = function(e) {
               stop(paste0("Nieprawidłowy format argumentu baseGroupingVariable.\n",
                           errorGroupingVariableFormatMessage))})
  if (!(as.character(baseGroupingVariable) %in% names(registrations))) {
    stop(paste0("Zmienna grupująca podana argumentem baseGroupingVariable ('",
                baseGroupingVariable, "') nie występuje w danych o rejestracjach."))
  }

  cat("--------------------\n",
      "Łączenie pliku z danymi o rejestracjach z danymi o punktach rekrutacyjnych.\n",
      sep = "")
  registrations <- join_with_check(registrations, scores,
                                   "danych o rejestracjach",
                                   "danych o punktach rekrutacyjnych")
  
  cat("--------------------\n",
      "Łączenie pliku z danymi o rejestracjach z danymi ze słownika kierunków.\n",
      sep = "")  
  dict <- dictionary %>% 
    select(!!baseGroupingVariable,!!groupingVariable)
  
  registrations <- join_with_check(registrations, dict,
                                   "danych o rejestracjach",
                                   "danych ze słownika")

  cat("--------------------\n",
      "Obliczanie statystyk.\n",
      sep = "")
  #-----------------------------------------------------------------------------
  #|-> Here starts summarising the data
  #-----------------------------------------------------------------------------
  
  results <- registrations %>%
    group_by(!!groupingVariable) %>%
    # mutating
    mutate(wynik = suppressWarnings(as.numeric(wynik)),
           # below Inf will be assigned to groups
           # with no (qualified candidates with non-missing scores)
           MINWYN = suppressWarnings(
             min(wynik[zakwalifikowany %in% "1" & !is.na(wynik)]))
    ) %>%
    # summarising
    summarise(
      NREJ = n(),
      NKAN = sum(czy_oplacony %in% "1"),
      NZAK_0 = sum(zakwalifikowany %in% "0"),
      NZAK_1 = sum(zakwalifikowany %in% "1"),
      NZAK_R = sum(zakwalifikowany %in% "R"),
      NZAK_BD = NREJ - NZAK_0 - NZAK_1 - NZAK_R,
      NPRZ_0 = sum(przyjety %in% "0"),
      NPRZ_1 = sum(przyjety %in% "1"),
      NPRZ_R = sum(przyjety %in% "R"),
      NPRZ_BD = NREJ - NPRZ_0 - NPRZ_1 - NPRZ_R,
      NBLPKT = sum(wynik < 0, na.rm = TRUE),
      NBLZAKKAN = sum(!(czy_oplacony %in% "1") & zakwalifikowany %in% "1"),
      NBLPRZZAK = sum(!(zakwalifikowany %in% "1") & przyjety %in% "1"),
      MINWYN = ifelse(is.finite(MINWYN[1]), MINWYN[1], NA),
      NBLZAKPKT = sum(zakwalifikowany %in% "1" & wynik >= MINWYN & !is.na(wynik))
    ) %>%
    ungroup()
  
  # adding limits
  results <- dictionary %>% 
    group_by(!!groupingVariable) %>%
    summarise(
      LIM_OG = sum(limitOG, na.rm = TRUE),
      LIM_P = sum(limitP, na.rm = TRUE),
      LIM_C = sum(limitC, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    full_join(results)
  
  #-----------------------------------------------------------------------------
  #|-> Here ends summarising the data 
  #-----------------------------------------------------------------------------
  return(results)
}
