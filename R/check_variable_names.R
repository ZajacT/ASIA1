#' @title Function checks IRK recoding dictionary
#' @description Function checks if all required variables in IRK recoding
#' dictionary are present.
#' @param x data frame to be checked
#' @param requiredNames character vector containing names of variables that are
#' required in a checked data frame
#' @param xDescription optionally a string describing \code{x} in a user-friendly
#' way, used to construct an error message that is thrown thrown if some of
#' required variables are not found
#' @return An error message if any variable is missing
check_variable_names <- function(x, requiredNames, xDescription = "ramce danych") {
  stopifnot(is.data.frame(x),
            is.character(requiredNames),
            is.character(xDescription), length(xDescription) == 1)
  missNames  <- setdiff(requiredNames, names(x))
  if (length(missNames) > 0) {
    stop(paste0("W ", xDescription, " brakuje ",
                ifelse(length(missNames) > 1, "zmiennych", "zmiennej"), ": '",
                paste0(missNames, collapse = "', '"),
                "'."), call. = FALSE)
  }
  return()
}
