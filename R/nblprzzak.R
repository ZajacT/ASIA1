#' Liczy osoby przyjęte, ale bez wpisanego zakwalifikowania
#' @description
#' You can put extended description here
#' @details
#' If your function is complicated, you may consider adding a details section
#' @param values numeric vector of values
#' @param groups vector of groups
#' @return data.frame with groups names in first column and average values per group in the second one
#' @export

nblprzzak = function(daneIRK,kierunki){
  require(dplyr)

  REKKIER<-daneIRK %>% filter(zakwalifikowany!="1" & przyjety=="1") %>% group_by(.[,kierunki]) %>% 
    summarise(
      NBLPRZZAK=n()
    )
  return(as.data.frame(REKKIER))
}
