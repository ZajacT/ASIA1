#' @title Checking correctness of parameter describing input file's location
#' @description Simple routine developed to check correctness of parameters
#' that should desribe paths to input files
#' @param file value of a parameter that should be checked
#' @param paramName optionally name of a parameter (to provide informative
#' error messages)
#' @details Function throws an error if it detects problem.
#' @return Function returns \code{TRUE} (if no problems are detected).
check_input_path <- function(file, paramName = "") {
  paramName <- ifelse(nchar(paramName) > 0,
                       paste0(" '", paramName, "'"),
                       paramName)
  stopifnot(is.character(paramName), length(paramName) == 1)
  if (!is.character(file) | length(file) != 1) {
    stop("Argument",  paramName,
         " musi zostać podany jako ciąg znaków (ściśle: jednoelementowy wektor tekstowy) opisujący ścieżkę do pliku.",
         call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("Plik, na który wskazuje argument", paramName, " nie istnieje.\n('",
         file, "')", call. = FALSE)
  }
  if (file.access(file, 4) != 0) {
    stop("Plik, na który wskazuje argument", paramName,
         " nie może zostać wczytany z powodu braku uprawnień.\n('",
         file, "')", call. = FALSE)
  }
  return(TRUE)
}
