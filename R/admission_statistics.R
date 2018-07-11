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
#'   admisssion_statistics()
#' }
#' @importFrom dplyr filter group_by mutate n semi_join summarise
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
  if (is.null(registrations)) {
    registrations <- choose_file(" z danymi o rekrutacjach")
  }
  check_input_path(registrations, "registrations")
  registrations <- read_file(registrations)

  if (is.null(exams)) {
    exams <- choose_file(" z danymi o PKTach egzaminów")
  }
  check_input_path(exams, "exams")
  exams <- read_file(exams)

  if (is.null(limits)) {
    limits <- choose_file(" limitami przyjęć")
  }
  check_input_path(limits, "limits")
  limits <- read_file(limits, columnsToCharacter = FALSE)

  if (is.null(output)) {
    output <- choose_file(", w którym mają zostać zapisane PKTy (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
                          errorOnCancel = FALSE)
  }
  if (!is.na(output)) {
    output <- sub("[.]csv$", "", output) %>% paste0(".csv")
    if (!(check_output_path(output, "output") %in% TRUE)) {
      output <- NA
    }
  }

  if (!(as.character(groupingVariable) %in% names(registrations))) {
    stop(paste0("Zmienna grupująca podana argumentem groupingVariable ('",
                groupingVariable, "') nie występuje w danych o rekrutacjach."))
  }
  #-----------------------------------------------------------------------------
  #|-> Here starts summarising the data
  #-----------------------------------------------------------------------------
  cat("--------------------\n",
      "Obliczanie statystyk.\n",
      sep = "")
  results <- registrations %>%
    group_by(!!groupingVariable) %>%
    mutate(PKT_PRZ = ifelse(PRZ > 0 & !is.na(PKT), PKT, NA),
           LICZ_Q = sum(!is.na(PKT_PRZ)) > 9,
           PKT_PRZ = ifelse(LICZ_Q, PKT, NA)) %>%
    # summarising
    summarise(
      NREJ = sum(REJ > 0), # number of registrations
      NZAK = sum(ZAK > 0), # number of qualified cand.
      NPRZ = sum(PRZ > 0), # number of admitted cand.
      PRZ_PKT_NNa = sum(!is.na(PKT)),
      PRZ_PKT_D1 = round(quantile(PKT_PRZ, probs = 0.10,  na.rm = TRUE), 0),
      PRZ_PKT_Q1 = round(quantile(PKT_PRZ, probs = 0.25,  na.rm = TRUE), 0),
      PRZ_PKT_Q3 = round(quantile(PKT_PRZ, probs = 0.75,  na.rm = TRUE), 0),
      PRZ_PKT_D9 = round(quantile(PKT_PRZ, probs = 0.90,  na.rm = TRUE), 0),
      PRZ_PKT_MEA = round(mean(PKT_PRZ), 0)) %>%
    ungroup()
    #-----------------------------------------------------------------------------
    #|-> Exam scores
    #-----------------------------------------------------------------------------
    exams$wynik_p <- as.numeric(ifelse(exams$wynik_p == "NULL",
                                       "", exams$wynik_p))
    exams$wynik_r <- as.numeric(ifelse(exams$wynik_r == "NULL",
                                       "", exams$wynik_r))
    exams <- exams %>%
      select(pesel, egzamin, wynik_p, wynik_r) %>%
      gather(poziom, wynik, -pesel, -egzamin) %>%
      mutate(egzamin = paste0(egzamin, sub("wynik", "", poziom))) %>%
      select(-poziom) %>%
      group_by(pesel, egzamin) %>%
      mutate(wynik = max(wynik)) %>%
      ungroup()
    exams <- suppressMessages(semi_join(exams, registrations))

    matResults <- suppressMessages(registrations %>%
                                     filter(PRZ > 0) %>%
                                     left_join(exams)) %>%
      group_by(!!groupingVariable, egzamin) %>%
      mutate(LICZ_Q = sum(!is.na(wynik)) > 9) %>%
      summarise(PN = sum(!is.na(wynik)),
                PPROC = round(PN / n(), 2),
                PSR = round(mean(wynik, na.rm = TRUE), 0),
                PD1 = round(quantile(wynik, probs = 0.10, na.rm = TRUE), 0),
                PQ1 = round(quantile(wynik, probs = 0.25, na.rm = TRUE), 0),
                PQ3 = round(quantile(wynik, probs = 0.75, na.rm = TRUE), 0),
                PD9 = round(quantile(wynik, probs = 0.90, na.rm = TRUE), 0)) %>%
      ungroup() %>%
      gather(statystyka, wartosc, -!!groupingVariable, -egzamin) %>%
      mutate(statystyka = paste0(egzamin, "_", statystyka)) %>%
      select(-egzamin) %>%
      spread(statystyka, wartosc)

    cat("Przyłączanie danych o wynikach egzaminów maturalnych.\n")
    results <- join_with_check(results, matResults,
                               "danych o przyjęciach",
                               "danych o wynikach maturalnych")
  # adding limits
  limits <- limits %>%
    group_by(!!groupingVariable) %>%
    summarise(
      LIM_OG = sum(limitog, na.rm = TRUE),
      LIM_P = sum(limitp, na.rm = TRUE),
      LIM_C = sum(limitc, na.rm = TRUE)
    ) %>%
    ungroup()

    cat("Przyłączanie danych o limitach przyjęć.\n")
    results <- join_with_check(limits, results,
                               "danych o limitach przyjęć",
                               "danych o rekrutacjach")
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
