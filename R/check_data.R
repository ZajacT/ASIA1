#' @title Checking the correctness of a recrutation data
#' @description We should write something here
#' @param registrations optionally path to the file with data on registrations
#' @param scores optionally path to the file with data on recruitment scores
#' @param exams optionally path to the file with data on examination scores
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
check_data <- function(registrations = NULL, scores = NULL, exams = NULL) {
  if (is.null(registrations)) {
    registrations <- file_choose("o rekrutacjach")
  }
  check_path(registrations, "registrations")
  if (is.null(scores)) {
    scores <- file_choose("o punktach rekrutacyjnych")
  }
  check_path(scores, "scores")
  if (is.null(exams)) {
    exams <- file_choose("o wynikach egzaminów")
  }
  check_path(exams, "exams")



  return(NULL)
}
