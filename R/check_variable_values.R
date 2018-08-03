#' @title Checks if a variable has values within a defined set
#' @description Function checks if a variable has values within a defined set and checks
#' the number of NAs. 
#' @param x a variable to be checked
#' @param valMin a minimal allowed value, an optional parameter
#' @param valMax a maximal allowed value, an optional parameter
#' @param valSet a set of allowed values, an optional parameter
#' @return if errors are detected: a warrning message listing errors 
#' and a menu offering an option to stop the program
#' @importFrom utils menu
#' @export
check_variable_values <- function(x,valMin = NULL, valMax = NULL, valSet = NULL) {
  stopifnot(is.vector(x))
  stopifnot(is.vector(valSet) | is.null(valSet))
  stopifnot(is.numeric(valMin) | is.null(valMin))
  stopifnot(is.numeric(valMax) | is.null(valMax))
  #-----------------------------------------------------------------------------
  #|-> checks whether the varibale has correct values
  #-----------------------------------------------------------------------------
  problems <- 0 
  
  if(!is.null(valMin)) {
    if(!all(x >= valMin | is.na(x) )){
      warning(paste0("Zmienna ",substitute(x)[3]," przymuje wartości niższe niż ",valMin,"\n"), 
              call. = FALSE, immediate. = TRUE)
      problems <- problems + 1
    }
  }
  
  if(!is.null(valMax)) {
    if(!all(x <= valMax | is.na(x) )){
      warning(paste0("Zmienna ",substitute(x)[3]," przymuje wartości wyższe niż ",valMax,"\n"), 
              call. = FALSE, immediate. = TRUE)
      problems <- problems + 1
    }
  }
  
  if(!is.null(valSet)) {
    if(!all(x %in% valSet | is.na(x) )){
      warning(paste0("Zmienna ",substitute(x)[3]," przymuje wartości spoza zbioru: ",paste(valSet, collapse = "; "),"\n"), 
              call. = FALSE, immediate. = TRUE)
      problems <- problems + 1
    }
  }
  
  if(any(is.na(x))){
    warning(paste0("W zmiennej ",substitute(x)[3], " występują braki danych\n" ), 
            call. = FALSE, immediate. = TRUE)
    problems <- problems + 1
  }
  
  if(problems > 0){
    switch(menu(c(paste0("Kontynuują mimo problemów ze zmienną ",substitute(x)[3]),
                  "Zatrzymaj"),
                title = paste0("Czy pomimo wyżej wymienionych problemów ze zmięnną " ,substitute(x)[3], " chcesz kontynuować działanie programu?" )),
           print("kontynuacja"),
           stop(paste0("zatrzymanie przez użytkownika"), call. = FALSE)
    )
  }
  invisible()
}
