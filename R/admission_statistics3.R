#' @title Summarises admissions without matura exams and minimal number of observations
#' @description The function computes a set of indicators describing admission process. 
#' The function differs from admissions_statistics by focusing on the number of administrative 
#' processes instead of on the number of people. It does not use data on exams and does not check 
#' if there is at least 10 observations when raporting the results.
#' @param groupingVariable optionally name of a grouping variable that should be
#' used in analysis given as a string or as a name/symbol
#' @param registrations optionally path to the file with data on registrations
#' @param limits optionally path to the file with data on limits
#' @param output optionally path to the file in which results will be saved;
#' if \code{NA} is given as a value of this parameter, results won't be saved
#' to a file
#' @details
#' Location of files contaninig the data to be checked and location of file
#' in which results will be saved can be described noninteractively with
#' function arguments described above or - if any of this arguments is omitted -
#' interactively with a system file-selection dialog.
#' @return Data frame (tibble) with computed summary statistics - the same as
#' written to a file described with the \code{output} parameter (data frame is
#' returned invisibly).
#' @examples
#' \dontrun{
#'   admission_statistics2("studia", "inst/pr_pop_rek.csv",
#'                        "inst/as_egzaminy.xlsx", "inst/as_limity.xlsx",
#'                        "statistics.csv")
#' }
#' @importFrom dplyr arrange filter group_by inner_join mutate mutate_all n semi_join summarise first n_distinct
#' @importFrom rlang ensym
#' @importFrom stats quantile
#' @importFrom tidyr gather spread
#' @importFrom utils write.table
#' @export
admission_statistics3 <- function(groupingVariable = "studia", registrations = NULL, exams = NULL,
                                 limits = NULL, output = NULL) {
  errorGroupingVariableFormatMessage = "Zmienna grupująca musi zostać podana jako ciąg znaków (jednoelementowy wektor typu character) lub jako wyrażenie (nazwa zmiennej nie ujęta w cudzysłów)."
  groupingVariable <-
    tryCatch(ensym(groupingVariable),
             error = function(e) {
               stop(paste0("Nieprawidłowy format argumentu groupingVariable.\n",
                           errorGroupingVariableFormatMessage))})
  tryCatch(get("groupingVariable"),
           error = function(e) {
             stop("Nie podano zmiennej grupującej (argumentu 'groupingVariable').",
                  call. = FALSE)})
  #-----------------------------------------------------------------------------
  #|-> Data loading and checks start here
  #-----------------------------------------------------------------------------
  if (is.null(registrations)) {
    registrations <- choose_file(" z danymi o rekrutacjach")
  }
  check_input_path(registrations, "registrations")
  registrations <- read_file(registrations)
  cat("--------------------\n",
      "Sprawdzanie poprawności danych w pliku z rejestracjami.\n",
      sep = "")
  check_variable_names(registrations,
                       c("pesel", as.character(groupingVariable), "rej", "zak",
                         "prz", "pkt_kan","pkt_zak"),
                       "danych o rekrutacjach")
  registrations <- registrations %>%
    select(!!groupingVariable, pesel, rej, zak, prz, pkt_kan,pkt_zak)
  registrations[, 2:6] <- suppressWarnings(
    sapply(registrations[, 2:6], as.numeric))
  check_variable_values(registrations$rej, valMin = 0, valMax = 10)
  check_variable_values(registrations$zak, valMin = 0, valMax = 10)
  check_variable_values(registrations$prz, valMin = 0, valMax = 10)
  check_variable_values(registrations$pkt_zak, valMin = 0)
  check_variable_values(registrations$pkt_kan, valMin = 0)

  if (is.null(exams)) {
    exams <- choose_file(" z danymi o wynikach egzaminów")
  }
  check_input_path(exams, "exams")
  exams <- read_file(exams)
  cat("--------------------\n",
      "Sprawdzanie poprawności danych w pliku z wynikami egzaminów.\n",
      sep = "")
  check_variable_names(exams,
                       c("pesel", "egzamin", "wynik_p", "wynik_r"),
                       "danych o egzaminach")
  exams <- exams %>%
    select(egzamin, pesel, wynik_p, wynik_r)
  exams[, 2:4] <- suppressWarnings(
    sapply(exams[, 2:4], as.numeric))
  check_variable_values(exams$wynik_p, valMin = 0, valMax = 100)
  check_variable_values(exams$wynik_r, valMin = 0, valMax = 100)
  
  if (is.null(limits)) {
    limits <- choose_file(" limitami przyjęć")
  }
  check_input_path(limits, "limits")
  limits <- read_file(limits, columnsToCharacter = FALSE)
  cat("--------------------\n",
      "Sprawdzanie poprawności danych w pliku z limitami.\n",
      sep = "")
  check_variable_names(limits,
                       c(as.character(groupingVariable), "limitog", "jednostka",
                         "maxpkt"),
                       "danych o limitach")
  limits <- limits %>%
    select(!!groupingVariable, limitog, maxpkt, jednostka)
  limits[, 2:3] <- suppressWarnings(
    sapply(limits[, 2:3], as.numeric))
  check_variable_values(limits$limitog, valMin = 0, valMax = 10000)
  check_variable_values(limits$maxpkt, valMin = 0)

  if (is.null(output)) {
    output <- choose_file(", w którym mają zostać zapisane statystyki rekrutacyjne (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
                          errorOnCancel = FALSE)
  }
  if (!is.na(output)) {
    output <- sub("[.]csv$", "", output) %>% paste0(".csv")
    if (!(check_output_path(output, "output") %in% TRUE)) {
      output <- NA
    }
  }
  #-----------------------------------------------------------------------------
  #|-> Here starts summarising the data
  #-----------------------------------------------------------------------------
  cat("--------------------\n",
      "Obliczanie statystyk.\n",
      sep = "")
  results <- registrations %>%
    group_by(!!groupingVariable) %>%
    mutate(PKT_PRZ = as.numeric(ifelse(prz > 0 & !is.na(pkt_zak), pkt_zak, NA)),
           PKT_KAN = as.numeric(ifelse(rej > 0 & !is.na(pkt_kan), pkt_kan, NA))) %>%
  # summarising
  summarise(
    NREJ = sum(rej), # total number of registrations - it is possible that a candidate registers more than once
    NZAK = sum(zak), # number of registrations resulting in admission
    NPRZ = sum(prz), # number of registrations resulting in enrollment.
    KAN_PKT_NNa = sum(!is.na(PKT_KAN) & rej > 0),
    KAN_PKT_MIN = round(min(PKT_KAN, na.rm = TRUE), 0),
    KAN_PKT_D1 = round(quantile(PKT_KAN, probs = 0.10,  na.rm = TRUE), 0),
    KAN_PKT_Q1 = round(quantile(PKT_KAN, probs = 0.25,  na.rm = TRUE), 0),
    KAN_PKT_Q3 = round(quantile(PKT_KAN, probs = 0.75,  na.rm = TRUE), 0),
    KAN_PKT_D9 = round(quantile(PKT_KAN, probs = 0.90,  na.rm = TRUE), 0),
    KAN_PKT_MAX = round(max(PKT_KAN, na.rm = TRUE), 0),
    KAN_PKT_RANGE = round(max(PKT_KAN, na.rm = TRUE)-min(PKT_KAN, na.rm = TRUE), 0),
    KAN_PKT_IQR = round(IQR(PKT_KAN, na.rm = TRUE), 0),
    KAN_PKT_MEA = round(mean(PKT_KAN, na.rm = TRUE), 0),
    PRZ_PKT_NNa = sum(!is.na(PKT_PRZ) & prz > 0),
    PRZ_PKT_MIN = round(min(PKT_PRZ, na.rm = TRUE), 0),
    PRZ_PKT_D1 = round(quantile(PKT_PRZ, probs = 0.10,  na.rm = TRUE), 0),
    PRZ_PKT_Q1 = round(quantile(PKT_PRZ, probs = 0.25,  na.rm = TRUE), 0),
    PRZ_PKT_Q3 = round(quantile(PKT_PRZ, probs = 0.75,  na.rm = TRUE), 0),
    PRZ_PKT_D9 = round(quantile(PKT_PRZ, probs = 0.90,  na.rm = TRUE), 0),
    PRZ_PKT_MAX = round(max(PKT_PRZ, na.rm = TRUE), 0),
    PRZ_PKT_RANGE = round(max(PKT_PRZ, na.rm = TRUE)-min(PKT_PRZ, na.rm = TRUE), 0),
    PRZ_PKT_IQR = round(IQR(PKT_PRZ, na.rm = TRUE), 0),
    PRZ_PKT_MEA = round(mean(PKT_PRZ, na.rm = TRUE), 0)
  ) %>%
    ungroup()
  
  # adding limits
  limits <- limits %>%
    group_by(!!groupingVariable) %>%
    summarise(
      LIM_OG = sum(limitog, na.rm = TRUE),
      JEDNOSTKA = ifelse(dplyr::n_distinct(jednostka) == 1, dplyr::first(jednostka),NA),
      MAXPKT = max(maxpkt, na.rm = TRUE)
    ) %>%
    ungroup()

  cat("\n---\nPrzyłączanie danych o limitach przyjęć.\n")
  results <- join_with_check(limits, results,
                             "danych o limitach przyjęć",
                             "danych o rekrutacjach")

  # adding selectivity indices
  results <- results %>%
    mutate(
      KM = round(NREJ/LIM_OG, 2),
      ZK = round(NZAK/NREJ, 2),
      PZ = round(NPRZ/NZAK, 2),
      WYP_LIM = round(NPRZ/LIM_OG,2)
    ) %>%
    mutate_all(dplyr::funs(ifelse(. == -Inf, NA, .))) %>%
    mutate_all(dplyr::funs(ifelse(. == Inf, NA, .)))

  #-----------------------------------------------------------------------------
  #|-> Exam scores
  #-----------------------------------------------------------------------------
  exams <- exams %>%
    filter(grepl("^M_", egzamin)) %>%
    select(pesel, egzamin, wynik_p, wynik_r) %>%
    gather(poziom, wynik, -pesel, -egzamin) %>%
    mutate(egzamin = paste0(egzamin, sub("wynik", "", poziom))) %>%
    select(-poziom) %>%
    group_by(pesel, egzamin) %>%
    summarise(wynik = max(wynik)) %>%
    ungroup()
  exams <- suppressMessages(semi_join(exams, registrations))
  
  matResults <- suppressMessages(registrations %>%
                                   filter(prz > 0) %>%
                                   group_by(!!groupingVariable) %>%
                                   mutate(N = sum(prz)) %>%
                                   ungroup() %>%
                                   inner_join(exams)) %>%
    group_by(!!groupingVariable, egzamin) %>%
    mutate(LICZ_Q = sum(!is.na(wynik)) > 9,
           wynik = ifelse(LICZ_Q, wynik, NA)) %>%
    summarise(PN = sum(!is.na(wynik)),
              PPROC = round(PN / max(N,na.rm = TRUE), 2)*100,
              PSR = round(mean(wynik, na.rm = TRUE), 0),
              PD1 = round(quantile(wynik, probs = 0.10, na.rm = TRUE), 0),
              PQ1 = round(quantile(wynik, probs = 0.25, na.rm = TRUE), 0),
              PQ3 = round(quantile(wynik, probs = 0.75, na.rm = TRUE), 0),
              PD9 = round(quantile(wynik, probs = 0.90, na.rm = TRUE), 0)) %>%
    mutate(PN = ifelse(PN == 0, NA, PN),
           PPROC = ifelse(PN == 0, NA, PPROC)) %>%
    ungroup() %>%
    gather(statystyka, wartosc, -!!groupingVariable, -egzamin) %>%
    arrange(egzamin) %>% # this is meant to arrange records in a way that spread command does not change the order of columns
    mutate(statystyka = paste0(egzamin, "_", statystyka),
           statystyka = factor(statystyka, levels = unique(statystyka))) %>% # this is meant to fix the desired order of columns after spread is executed
    select(-egzamin) %>%
    spread(statystyka, wartosc)
  
  cat("\n---\nPrzyłączanie danych o wynikach egzaminów maturalnych.\n")
  results <- join_with_check(results, matResults,
                             "danych o przyjęciach",
                             "danych o wynikach maturalnych")  
  
  #-----------------------------------------------------------------------------
  #|-> Here ends summarising the data
  #-----------------------------------------------------------------------------
  cat("--------------------\n",
      "Zapisywanie statystyk.\n",
      sep = "")
  if (is.na(output)) {
    warning("Statystyki nie zostaną zapisane do pliku, ponieważ nie podano jego nazwy.",
            call. = FALSE, immediate. = TRUE)
  } else {
    write.table(results, output, row.names = FALSE, na = "", dec = ".", sep = ";", quote = FALSE,
               fileEncoding = "UTF-8")
    cat("Zapisano Statystyki do pliku '", output, "'.\n", sep = "")
  }
  invisible(results)
}
