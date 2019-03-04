#' @title Checks if data on registretions are complete, correct, and consistent with data on scores
#' @description Function checks if data on registrations and data on recruitment
#' scores joined together are correct. Function gives user a choice whether to
#' correct detected minor issues detected in the data, or to add column
#' indicating which observations should be fixed. Finally it returns a data
#' frame with corrected data or columns indicating problems.
#' @param registrations data frame with joined data regarding registrations and
#' regarding recruitment scores
#' @return data frame
#' @importFrom dplyr group_by if_else mutate ungroup
#' @importFrom utils menu
#' @export
check_registrations_with_scores <- function(registrations) {
  stopifnot(is.data.frame(registrations))

  # checks if all admitted were first accepted
  if (any((registrations$przyjety %in% "1") & !(registrations$zakwalifikowany %in% "1"))) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_prz_zak', wskazującą obserwacje wymagające poprawy.",
                  "Skoryguj wartości zmiennej 'zakwalifikowany' - wstaw '1', jeśli 'przyjety' jest równa '1'."),
                title = "W zbiorze występują osoby przyjęte, ale nie zakwalifikowane na studia."),
           registrations <- registrations %>%
             mutate(blad_prz_zak = if_else((przyjety %in% "1") & !(zakwalifikowany %in% "1"), 1, 0)),
           registrations <- registrations %>%
             mutate(zakwalifikowany = if_else(przyjety %in% "1", "1", zakwalifikowany))
    )
  }
  if (any((registrations$zakwalifikowany %in% "1") & !(registrations$czy_oplacony %in% "1"))) {
    switch(menu(c("Dodaj do zbioru kolumnę 'blad_zak_opl', wskazującą obserwacje wymagające poprawy.",
                  "Skoryguj wartości zmiennej 'czy_oplacony' - wstaw '1', jeśli 'zakwalifikowany' jest równa '1'."),
                title = "W zbiorze występują osoby zakwalifikowane, które nie opłaciły rekrutacji."),
           registrations <- registrations %>%
             mutate(blad_zak_opl = if_else((zakwalifikowany %in% "1") & !(czy_oplacony %in% "1"), 1, 0)),
           registrations <- registrations %>%
             mutate(czy_oplacony = if_else(zakwalifikowany %in% "1", "1", czy_oplacony))
    )
  }  
  
  # checks if there are students who are not admitted despite having more points that the last admitted applicant.
    
  registrations <- registrations %>%
    group_by(studia) %>%
    # below Inf will be assigned to groups with no (qualified candidates with non-missing scores)
    mutate(
      MINWYN = suppressWarnings(
        min(wynik[zakwalifikowany %in% "1" & !is.na(wynik)], na.rm = TRUE)),
      MINWYN = ifelse(is.finite(MINWYN), MINWYN, NA),
      BLZAKPKT = !(zakwalifikowany %in% "1") & wynik >= MINWYN & !is.na(wynik)
    ) %>%
    ungroup()

  if (any(registrations$BLZAKPKT)) {
    switch(menu(c("Zignoruj to.",
                  "Wszystkim osobom o wyniku wyższym niż ostatni zakwalifikowany przypisz zakwalifikowanie, o ile rejestracja została opłacona."),
                title = "W zbiorze występują osoby niezakwalifikowane mimo wyższej liczby punktów niż ostatnia osoba zakwalifikowana."),
           registrations <- registrations %>%
             select(-MINWYN,-BLZAKPKT),
           registrations <- registrations %>%
             mutate(zakwalifikowany = if_else(BLZAKPKT & czy_oplacony %in% "1",
                                              "1", zakwalifikowany)) %>%
             select(-MINWYN,-BLZAKPKT)
    )
  }

  return(registrations)
}
