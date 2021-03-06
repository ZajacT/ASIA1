#' @title Summarises admissions
#' @description The function computes a set of indicators describing admission process.
#' @param groupingVariable optionally name of a grouping variable that should be
#' used in analysis given as a string or as a name/symbol
#' @param registrations optionally path to the file with data on registrations
#' @param exams optionally path to the file with data on examination scores
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
#'   admission_statistics("studia", "inst/pr_pop_rek.csv",
#'                        "inst/as_egzaminy.xlsx", "inst/as_limity.xlsx",
#'                        "statistics.csv")
#' }
#' @importFrom dplyr arrange filter group_by inner_join mutate mutate_all n semi_join summarise
#' @importFrom rlang ensym
#' @importFrom stats quantile
#' @importFrom tidyr gather spread
#' @importFrom utils write.csv2
#' @export
admission_statistics <- function(groupingVariable = "studia", registrations = NULL,
                                 exams = NULL, limits = NULL, output = NULL) {
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
                         "prz", "pkt"),
                       "danych o rekrutacjach")
  registrations <- registrations %>%
    select(!!groupingVariable, pesel, rej, zak, prz, pkt)
  registrations[, 2:6] <- suppressWarnings(
    sapply(registrations[, 2:6], as.numeric))
  check_variable_values(registrations$rej, valMin = 0, valMax = 10)
  check_variable_values(registrations$zak, valMin = 0, valMax = 10)
  check_variable_values(registrations$prz, valMin = 0, valMax = 10)
  check_variable_values(registrations$pkt, valMin = 0)

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
                       c(as.character(groupingVariable), "limitog", "limitp",
                         "limitc", "maxpkt"),
                       "danych o limitach")
  limits <- limits %>%
    select(!!groupingVariable, limitog, limitp, limitc, maxpkt)
  limits[, 2:5] <- suppressWarnings(
    sapply(limits[, 2:5], as.numeric))
  check_variable_values(limits$limitog, valMin = 0, valMax = 10000)
  check_variable_values(limits$limitp, valMin = 0, valMax = 10000)
  check_variable_values(limits$limitc, valMin = 0, valMax = 10000)
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
    mutate(PKT_PRZ = ifelse(prz > 0 & !is.na(pkt), pkt, NA),
           LICZ_Q = sum(!is.na(PKT_PRZ)) > 9,
           PKT_PRZ = ifelse(LICZ_Q, PKT_PRZ, NA)) %>%
  # summarising
  summarise(
    NREJ = sum(rej > 0), # number of registrations
    NZAK = sum(zak > 0), # number of qualified cand.
    NPRZ = sum(prz > 0), # number of admitted cand.
    PRZ_PKT_NNa = sum(!is.na(pkt)),
    PRZ_PKT_D1 = round(quantile(PKT_PRZ, probs = 0.10,  na.rm = TRUE), 0),
    PRZ_PKT_Q1 = round(quantile(PKT_PRZ, probs = 0.25,  na.rm = TRUE), 0),
    PRZ_PKT_Q3 = round(quantile(PKT_PRZ, probs = 0.75,  na.rm = TRUE), 0),
    PRZ_PKT_D9 = round(quantile(PKT_PRZ, probs = 0.90,  na.rm = TRUE), 0),
    PRZ_PKT_MEA = round(mean(PKT_PRZ, na.rm = TRUE), 0)
  ) %>%
    ungroup()
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
    mutate(wynik = max(wynik)) %>%
    ungroup()
  exams <- suppressMessages(semi_join(exams, registrations))

  matResults <- suppressMessages(registrations %>%
                                   filter(prz > 0) %>%
                                   inner_join(exams)) %>%
    group_by(!!groupingVariable, egzamin) %>%
    mutate(LICZ_Q = sum(!is.na(wynik)) > 9,
           wynik = ifelse(LICZ_Q, wynik, NA)) %>%
    summarise(PN = sum(!is.na(wynik)),
              PPROC = round(PN / n(), 2)*100,
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
  # adding limits
  limits <- limits %>%
    group_by(!!groupingVariable) %>%
    summarise(
      LIM_OG = sum(limitog, na.rm = TRUE),
      LIM_P = sum(limitp, na.rm = TRUE),
      LIM_C = sum(limitc, na.rm = TRUE),
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
      PZ = round(NPRZ/NZAK, 2)
    ) %>%
    mutate_all(funs(ifelse(. == -Inf, NA, .)))

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
    write.csv2(results, output, row.names = FALSE, na = "",
               fileEncoding = "UTF-8")
    cat("Zapisano Statystyki do pliku '", output, "'.\n", sep = "")
  }
  invisible(results)
}
