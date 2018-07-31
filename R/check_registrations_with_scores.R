#' @title Checks if data on registretions are complete and correct
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
