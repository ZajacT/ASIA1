#' @title Checking correctness of parameter describing file's location
#' @description Simple routine developed to check correctness of parameters
#' that should desribe paths to files
#' @param file value of a parameter that should be checked
#' @param param_name optionally name of a parameter (to provide informative
#' error messages)
#' @details Function throws an error if it detects problem.
#' @return Function returns nothing.
check_path <- function(file, param_name = "") {
  param_name <- ifelse(nchar(param_name) > 0,
                       paste0(" '", param_name, "'"),
                       param_name)
  if (!is.character(file) | length(file) != 1) {
    stop("Argument",  param_name,
         " musi zostać podany jako ciąg znaków (ściśle: jednoelementowy wektor tekstowy) opisujący ścieżkę do pliku.",
         call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("Plik, na który wskazuje argument", param_name, " nie istnieje.\n('",
         file, "')", call. = FALSE)
  }
  if (file.access(file, 4) != 0) {
    stop("Plik, na który wskazuje argument", param_name,
         " nie może zostać wczytany z powodu braku uprawnień.\n('",
         file, "')", call. = FALSE)
  }
  return(NULL)
}
