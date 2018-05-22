#' Liczy osoby przyjęte wg IRK
#' @description
#' You can put extended description here
#' @details
#' If your function is complicated, you may consider adding a details section
#' @param values numeric vector of values
#' @param groups vector of groups
#' @return data.frame with groups names in first column and average values per group in the second one
#' @export

nprz = function(daneIRK,kierunki){
  require(dplyr)
  
  REKKIER<-daneIRK %>% group_by(.[,kierunki]) %>% 
    summarise(
      NPRZ_0=sum(przyjety=="0", na.rm = TRUE),
      NPRZ_1=sum(przyjety=="1", na.rm = TRUE),
      NPRZ_R=sum(przyjety=="R", na.rm = TRUE),
      NPRZ_BD=sum(przyjety!="R" & przyjety!="0" & przyjety!="1", na.rm = TRUE)
    )
  return(as.data.frame(REKKIER))
}


