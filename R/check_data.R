#' @title Checking the correctness of a recrutation data
#' @description We should write something here
#' @param groupingVariable optionally name of a grouping variable that should be
#' used in analysis given as a string or as an expression
#' @param registrations optionally path to the file with data on registrations
#' @param scores optionally path to the file with data on recruitment scores
#' @param exams optionally path to the file with data on examination scores
#' @param dictionary optionally path to the file with data containing additional
#' informations about registrations
#' @param baseGroupingVariable name of (to be described) variable given as
#' a string or as an expression
#' @details
#' Location of files contaninig the data to be checked can be described
#' noninteractively with function arguments described above or - if any of this
#' arguments is omitted - interactively with a system file-selection dialog.
#' @return Function returns nothing.
#' @examples
#' \dontrun{
#'   check_data()
#' }
#' @importFrom rlang ensym
#' @export
check_data <- function(groupingVariable, registrations = NULL, scores = NULL,
                       exams = NULL, dictionary = NULL,
                       baseGroupingVariable = "studia") {
  if (is.null(registrations)) {
    registrations <- choose_file("o rekrutacjach")
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
                groupingVariable, "') nie występuje w danych o rekrutacjach."))
  }
  baseGroupingVariable <-
    tryCatch(ensym(baseGroupingVariable),
             error = function(e) {
               stop(paste0("Nieprawidłowy format argumentu baseGroupingVariable.\n",
                           errorGroupingVariableFormatMessage))})
  if (!(as.character(baseGroupingVariable) %in% names(registrations))) {
    stop(paste0("Zmienna grupująca podana argumentem baseGroupingVariable ('",
                baseGroupingVariable, "') nie występuje w danych o rekrutacjach."))
  }

  cat("--------------------\n",
      "Łączenie pliku z danymi o rekrutacjach z danymi o punktach rekrutacyjnych.\n",
      sep = "")
  registrations <- join_with_check(registrations, scores,
                                   "danych o rekrutacjach",
                                   "danych o punktach rekrutacyjnych")

  summarising_template(registrations, groupingVariable) %>%
    return()
}
