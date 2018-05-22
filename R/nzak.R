#' Liczy osoby o różnych statusach przyjęcia
#' @description
#' You can put extended description here
#' @details
#' If your function is complicated, you may consider adding a details section
#' @param values numeric vector of values
#' @param groups vector of groups
#' @return data.frame with groups names in first column and average values per group in the second one
#' @export

nzak = function(daneIRK,kierunki){
  require(dplyr)

  REKKIER<-daneIRK %>% group_by(.[,kierunki]) %>% 
    summarise(
      NZAK_0=sum(zakwalifikowany=="0", na.rm = TRUE),
      NZAK_1=sum(zakwalifikowany=="1", na.rm = TRUE),
      NZAK_R=sum(zakwalifikowany=="R", na.rm = TRUE),
      NZAK_BD=sum(zakwalifikowany!="R" & zakwalifikowany!="0" & zakwalifikowany!="1", na.rm = TRUE)
    )
  return(as.data.frame(REKKIER))
}
