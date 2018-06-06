#' @title Checking correctness of parameter describing output file's location
#' @description Simple routine developed to check correctness of parameters
#' that should desribe paths to output files
#' @param file value of a parameter that should be checked
#' @param paramName optionally name of a parameter (to provide informative
#' error messages)
#' @details Function throws an error if it detects problem.
#' @return Function returns \code{TRUE} if it doesn't detect problems. It
#' returns \code{FALSE} or \code{NA} if a file described by \code{file}
#' parameteralready exists and user choose not to overwrite it.
#' @importFrom utils askYesNo
check_output_path <- function(file, paramName = "") {
  paramName <- ifelse(nchar(paramName) > 0,
                       paste0(" '", paramName, "'"),
                       paramName)
  stopifnot(is.character(paramName), length(paramName) == 1)
  result <- TRUE
  if (!is.character(file) | length(file) != 1) {
    stop("Argument",  paramName,
         " musi zostać podany jako ciąg znaków (ściśle: jednoelementowy wektor tekstowy) opisujący ścieżkę do pliku.",
         call. = FALSE)
  }
  if (file.exists(file)) {
    result <- askYesNo(paste0("Plik wskazany do zapisu wyników już istnieje ('",
                    file, "'). Czy chcesz nadpisać jego zawartość?"),
                    default = FALSE)
  }
  if (file.access(sub("[\\/][^\\/]*$", "", file), 2) != 0) {
    stop("Nie można zapisać danych do pliku, na który wskazuje argument",
         paramName, " z powodu braku uprawnień.\n('",
         file, "')", call. = FALSE)
  }
  return(result)
}
