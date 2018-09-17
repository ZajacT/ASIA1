#' @title Checks if a variable has values within a defined set
#' @description Function checks if a variable has values within a defined set
#' and checks the number of NAs.
#' @param x a variable to be checked
#' @param valMin a minimal allowed value, an optional parameter
#' @param valMax a maximal allowed value, an optional parameter
#' @param valSet a set of allowed values, an optional parameter
#' @return if errors are detected: a warrning message listing errors
#' and a menu offering an option to stop the program
#' @importFrom utils menu
check_variable_values <- function(x, valMin = NULL, valMax = NULL, valSet = NULL) {
  stopifnot(is.vector(x),
            is.vector(valSet) | is.null(valSet),
            is.numeric(valMin) | is.null(valMin),
            is.numeric(valMax) | is.null(valMax))
  xName = sys.call()
  if (!is.null(names(xName))) {
    if ("x" %in% names(xName)) {
      xName = as.character(xName)["x" == names(xName)]
    } else {
      xName = as.character(xName)[2]
    }
  } else {
    xName = as.character(xName)[2]
  }
  xName = sub("^.*[$]", "", xName)

  problems <- 0

  if (!is.null(valMin)) {
    if (!all(x >= valMin | is.na(x))) {
      warning(paste0("Zmienna `", xName, "` przyjmuje wartości niższe niż ",
                     valMin, ".\n"),
              call. = FALSE, immediate. = TRUE)
      problems <- problems + 1
    }
  }
  if (!is.null(valMax)) {
    if (!all(x <= valMax | is.na(x))) {
      warning(paste0("Zmienna `", xName, "` przyjmuje wartości wyższe niż ",
                     valMax, ".\n"),
              call. = FALSE, immediate. = TRUE)
      problems <- problems + 1
    }
  }
  if (!is.null(valSet)) {
    if (!all(x %in% valSet | is.na(x))) {
      warning(paste0("Zmienna `", xName, "` przymuje wartości spoza zbioru: ",
                     paste(valSet, collapse = "; "), ".\n"),
              call. = FALSE, immediate. = TRUE)
      problems <- problems + 1
    }
  }

  if (any(is.na(x))) {
    warning(paste0("W zmiennej `", xName, "` występują braki danych.\n" ),
            call. = FALSE, immediate. = TRUE)
    problems <- problems + 1
  }

  if (problems > 0) {
    switch(menu(c(paste0("Kontynuują mimo problemów ze zmienną `", xName, "`."),
                  "Zatrzymaj działanie programu."),
                title = paste0("Czy pomimo wyżej wymienionych problemów ze zmięnną `",
                               xName, "` chcesz kontynuować działanie programu?")),
           cat("Kontynuacja.\n"),
           stop(paste0("Zatrzymanie przez użytkownika."), call. = FALSE)
    )
  }
  invisible()
}
