#' @title Checks if there are codes in tha data that aren't in a recoding dictionary
#' @description Function checks if there are codes in tha data that aren't in
#' a recoding dictionary and if they are provides user with choice, how to deal
#' with them.
#' @param data data frame with data
#' @param rec data frame with dictionary
#' @param dataDescription desription of the data
#' @param recDescription description of the dictionary
#' @return numeric value: 1 if user choose to remove rows that don't appear
#' in a dictionary (or no such rows were detected); 2 if user choose to use
#' this rows with codes as they appear in the data
#' @importFrom dplyr anti_join
#' @importFrom utils menu
#' @export
check_rec_joining <- function(data, rec, dataDescription = "pliku z danymi",
                              recDescription = "pliku ze slownikiem") {
  stopifnot(is.data.frame(data),
            is.data.frame(rec),
            is.character(dataDescription), length(dataDescription) == 1,
            is.character(recDescription), length(recDescription) == 1)
  nNotMatch <- suppressMessages(
    anti_join(data, rec) %>%
      nrow())
  if (nNotMatch > 0) {
    choice <- menu(c("Zignoruj - kody niewymienione w słowniku zostaną usunięte.",
                     paste0("Użyj kodów niewymienionych w słowniku w formie, w jakiej występują w ",
                            dataDescription, "."),
                     "Zatrzymaj działanie programu."),
                   title = paste0("W ", dataDescription,
                                  " występują kody studiów, które nie występują w ",
                                  recDescription, ".\n",
                                  "W jaki sposób mają być potraktowane brakujące kody?"))
    if (choice == 3) {
      stop("Zatrzymanie przez użytkownika.", call. = FALSE)
    }
    else{
      return(choice)
    }
  } else {
    return(1)
  }
}
