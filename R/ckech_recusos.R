#' @title Function checks USOS recoding dictionary
#' @description Function checks if all required variables in USOS recoding dictionary are present.
#' @param  recRegistrations data frame with recoding dictionary for USOS data
#' @return An error message if any variable is missing
check_recusos <- function(recUsos = recUsos) {
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------

  reqVarNames <- c("program", "etap", "studia_rec")
  missVarNames  <- setdiff(reqVarNames, names(recUsos))
  if (length(missVarNames) > 0) {
    stop(paste0("W słowniku do przekodowywania danych USOS brakuje ",
                ifelse(length(missVarNames) > 1, "zmiennych", "zmiennej"), ": '",
                paste0(missVarNames, collapse = "', '"),
                "'."), call. = FALSE)
  }
  return(recUsos)
}
