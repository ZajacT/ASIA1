#' Liczy osoby przyjęte, ale bez wpisanego zakwalifikowania
#' @description
#' You can put extended description here
#' @details
#' If your function is complicated, you may consider adding a details section
#' @param values numeric vector of values
#' @param groups vector of groups
#' @return data.frame with groups names in first column and average values per group in the second one
#' @importFrom dplyr filter group_by summarise ungroup
#' @export
nblprzzak <- function(daneIRK, kierunki){
  daneIRK %>%
    filter(!(zakwalifikowany %in% "1") & przyjety %in% "1") %>%
    group_by(.[,kierunki]) %>%
    summarise(NBLPRZZAK = n()) %>%
    ungroup() %>%
    return()
}
