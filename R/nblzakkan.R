#' Liczy osoby zakwalifikowane, ale nie mające opłaty
#' @description
#' You can put extended description here
#' @details
#' If your function is complicated, you may consider adding a details section
#' @param values numeric vector of values
#' @param groups vector of groups
#' @return data.frame with groups names in first column and average values per group in the second one
#' @export

nblzakkan = function(daneIRK,kierunki){
  require(dplyr)

  REKKIER<-daneIRK %>% filter(czy_oplacony!=1 & zakwalifikowany=="1") %>% group_by(.[,kierunki]) %>% 
    summarise(
      NBLZAKKAN=n()
    )
  return(as.data.frame(REKKIER))
}
