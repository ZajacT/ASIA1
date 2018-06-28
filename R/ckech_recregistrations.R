#' @title Function checks IRK recoding dictionary
#' @description Function checks if all required variables in IRK recoding dictionary are present.
#' @param  recRegistrations data frame with recoding dictionary for registrations data
#' @return An error message if any variable is missing
check_recregistrations <- function(recRegistrations = recRegistrations) {
  #-----------------------------------------------------------------------------
  #|-> checks whether the file contains all the required variables (critical errors)
  #-----------------------------------------------------------------------------

  reqVarNames <- c("studia","studia_rec")
  missVarNames  <- setdiff(reqVarNames, names(recRegistrations))
  if (length(missVarNames) > 0) {
    stop(paste0("W słowniku do przekodowywania kodów IRK brakuje ",
                ifelse(length(missVarNames) > 1, "zmiennych", "zmiennej"), ": '",
                paste0(missVarNames, collapse = "', '"),
                "'."), call. = FALSE)
  }
  return()
}
