#' Liczy osoby niezakwalifikowane mimo wyższego wyniku niż ostatni zakwalifikowany
#' @description
#' You can put extended description here
#' @details
#' If your function is complicated, you may consider adding a details section
#' @param values numeric vector of values
#' @param groups vector of groups
#' @return data.frame with groups names in first column and average values per group in the second one
#' @export

nblzakpkt = function(daneIRK,kierunki){
  require(dplyr)

  daneIRK$wynik<-as.numeric(daneIRK$wynik)
  
  REKKIER<-daneIRK %>% group_by(.[,kierunki]) %>% mutate(MINWYN=min(wynik[zakwalifikowany=="1"],na.rm=TRUE)) %>%
    filter(zakwalifikowany!="1" & wynik>=MINWYN) %>%
    summarise(
      NBLZAKPKT=n()
    )
  return(as.data.frame(REKKIER))
}
