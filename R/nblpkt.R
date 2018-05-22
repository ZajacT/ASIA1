#' Liczy osoby z nieprawidlową liczbą (ujemną) punktów rekrutacyjnych 
#' @description
#' You can put extended description here
#' @details
#' If your function is complicated, you may consider adding a details section
#' @param values numeric vector of values
#' @param groups vector of groups
#' @return data.frame with groups names in first column and average values per group in the second one
#' @export

nblpkt = function(daneIRK,kierunki){
  require(dplyr)
  
  daneIRK$wynik<-as.numeric(daneIRK$wynik)
  REKKIER<-daneIRK %>% group_by(.[,kierunki]) %>% 
    summarise(
      NBLPKT=sum(wynik<0, na.rm = TRUE)
    )
  return(as.data.frame(REKKIER))
}

