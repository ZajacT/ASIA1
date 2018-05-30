#' @title Checking the correctness of a recrutation data
#' @description We should write something here
#' @param registrations optionally path to the file with data on registrations
#' @param scores optionally path to the file with data on recruitment scores
#' @param exams optionally path to the file with data on examination scores
#' @param dictionary optionally path to the file with data containing additional
#' informations about registrations
#' @param groupingVariable optionally name of a grouping variable that should be
#' used in analysis given as a string or as an expression
#' @param baseGroupingVariable name of (to be described) variable given as
#' a string or as an expression
#' @details
#' Location of files contaninig the data to be checked can be described
#' noninteractively with function arguments described above or - if any of this
#' arguments is omitted - interactively with a system file-selection dialog.
#' @return Function retruns nothing.
#' @examples
#' \dontrun{
#'   check_data()
#' }
#' @export
check_data <- function(registrations = NULL, scores = NULL, exams = NULL,
                       dictionary = NULL, groupingVariable = NULL,
                       baseGroupingVariable = "studia") {
  if (is.null(registrations)) {
    registrations <- choose_file("o rekrutacjach")
  }
  check_path(registrations, "registrations")
  registrations = read_file(registrations)
  if (is.null(scores)) {
    scores <- choose_file("o punktach rekrutacyjnych")
  }
  check_path(scores, "scores")
  if (is.null(exams)) {
    exams <- choose_file("o wynikach egzaminów")
  }
  check_path(exams, "exams")
  if (is.null(dictionary)) {
    dictionary <- choose_file("o słowniku")
  }
  check_path(dictionary, "dictionary")



  return(registrations)
}
