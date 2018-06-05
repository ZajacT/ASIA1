#' @title Selecting file to read
#' @description Simple routine regarding choosing interactively files to read
#' @param about optionally a string describing what should contain a file that
#' user is supposed to select
#' @details If no file is selected, function throws an error (parameter
#' \code{about} can help to make this error more informative).
#' @return String containing a path to a file.
#' @importFrom magrittr %>%
choose_file <- function(about = "") {
  stopifnot(is.character(about), length(about) == 1)

  cat("Wybierz plik z danymi", ifelse(nchar(about) > 0, " ", ""), about, ".\n",
      sep = "")
  tryCatch(file.choose(),
           error = function(e) {
             stop(paste0("Nie wskazano pliku z danymi",
                         ifelse(nchar(about) > 0, " ", ""), about, "."),
                  call. = FALSE)}) %>%
    return()
}
