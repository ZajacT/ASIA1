#' @title Selecting file to read
#' @description Simple routine regarding choosing interactively files to read
#' @param about optionally a string describing what should contain a file that
#' user is supposed to select
#' @param errorOnCancel logical value (TRUE or FALSE) - should canceling
#' a file selection dialog should throw an error?
#' @details If no file is selected, function throws an error (parameter
#' \code{about} can help to make this error more informative).
#' @return String containing a path to a file.
#' @importFrom magrittr %>%
choose_file <- function(about = "", errorOnCancel = TRUE) {
  stopifnot(is.character(about), length(about) == 1,
            is.logical(errorOnCancel), length(errorOnCancel) == 1,
            errorOnCancel %in% c(TRUE, FALSE))

  cat("Wybierz plik", about, ".\n", sep = "")
  if (errorOnCancel) {
    tryCatch(enc2native(file.choose()),
             error = function(e) {
               stop(paste0("Nie wskazano pliku z danymi",
                           ifelse(nchar(about) > 0, " ", ""), about, "."),
                    call. = FALSE)}) %>%
      return()
  } else {
    tryCatch(enc2native(file.choose()),
             error = function(e) {return(NA)}) %>%
      return()
  }
}
