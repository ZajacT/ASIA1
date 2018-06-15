#' @title Checking the correctness of a recrutation data
#' @description We should write something here
#' @param groupingVariable optionally name of a grouping variable that should be
#' used in analysis given as a string or as a name/symbol
#' @param registrations optionally path to the file with data on registrations
#' @param scores optionally path to the file with data on recruitment scores
#' @param exams optionally path to the file with data on examination scores
#' @param dictionary optionally path to the file with data containing additional
#' informations about registrations
#' @param output optionally path to the file in which results will be saved;
#' if \code{NA} is given as a value of this parameter, results won't be saved
#' to a file
#' @param baseGroupingVariable name of (to be described) variable given as
#' a string or as a name/symbol
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
#'   check_data()
#' }
#' @importFrom dplyr group_by mutate n summarise filter
#' @importFrom rlang ensym
#' @importFrom utils write.csv2
#' @export
check_data <- function(groupingVariable, registrations = NULL, scores = NULL,
                       exams = NULL, dictionary = NULL, output = NULL,
                       baseGroupingVariable = "studia") {
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

  if (is.null(scores)) {
    scores <- choose_file(" z danymi o punktach rekrutacyjnych")
  }
  check_input_path(scores, "scores")
  scores <- read_file(scores)

  if (is.null(exams)) {
    exams <- choose_file(" z danymi o wynikach egzaminów")
  }
  check_input_path(exams, "exams")
  exams <- read_file(exams)

  if (is.null(dictionary)) {
    dictionary <- choose_file(" ze słownikiem")
  }
  check_input_path(dictionary, "dictionary")
  dictionary <- read_file(dictionary, columnsToCharacter = FALSE)

  if (is.null(output)) {
    output <- choose_file(", w którym mają zostać zapisane wyniki (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
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
  baseGroupingVariable <-
    tryCatch(ensym(baseGroupingVariable),
             error = function(e) {
               stop(paste0("Nieprawidłowy format argumentu baseGroupingVariable.\n",
                           errorGroupingVariableFormatMessage))})
  if (!(as.character(baseGroupingVariable) %in% names(registrations))) {
    stop(paste0("Zmienna grupująca podana argumentem baseGroupingVariable ('",
                baseGroupingVariable, "') nie występuje w danych o rekrutacjach."))
  }

  ## foreign applicants with scholarships
  cat("--------------------\n",
      "Dopisanie informacji o byciu stypendystą zagranicznym do danych o rekrutacjach.\n",
      sep = "")
  foreignSholarships <- exams %>%
    filter(is.na(egzamin) & grepl("-C$", studia) & (grepl("^100(|[.]0+)$", wynik) | grepl("^100(|[,]0+)$", wynik))) %>%
    select(pesel, studia) %>%
    mutate(styp = "1")
  registrations <- join_with_check(registrations, foreignSholarships,
                                   "danych o rekrutacjach",
                                   "danych o stypendystach zagranicznych",
                                   xCheckAllMatchesY = FALSE,
                                   rowsOrObservations = "o") %>%
    mutate(styp = ifelse(is.na(styp), "0" , styp))

  cat("--------------------\n",
      "Łączenie pliku z danymi o rekrutacjach z danymi o punktach rekrutacyjnych.\n",
      sep = "")
  registrations <- join_with_check(registrations, scores,
                                   "danych o rekrutacjach",
                                   "danych o punktach rekrutacyjnych")

  cat("--------------------\n",
      "Obliczanie statystyk.\n",
      sep = "")
  #-----------------------------------------------------------------------------
  #|-> Here starts summarising the data
  #-----------------------------------------------------------------------------
  results <- registrations %>%
    group_by(!!groupingVariable) %>%
    # mutating
    mutate(wynik = suppressWarnings(as.numeric(wynik)),
           # below Inf will be assigned to groups
           # with no (qualified candidates with non-missing scores)
           MINWYN = suppressWarnings(
             min(wynik[zakwalifikowany %in% "1" & !is.na(wynik)]))
    ) %>%
    # summarising
    summarise(
      NREJ = n(), # number of registrations
      NKAN = sum(czy_oplacony %in% "1"), # number of candidates
      NZAK_0 = sum(zakwalifikowany %in% "0"), # number of qualified cand.
      NZAK_1 = sum(zakwalifikowany %in% "1"),
      NZAK_R = sum(zakwalifikowany %in% "R"),
      NZAK_BD = NREJ - NZAK_0 - NZAK_1 - NZAK_R,
      NPRZ_0 = sum(przyjety %in% "0"), # number of admitted cand.
      NPRZ_1 = sum(przyjety %in% "1"),
      NPRZ_R = sum(przyjety %in% "R"),
      NPRZ_BD = NREJ - NPRZ_0 - NPRZ_1 - NPRZ_R,
      NPRZ_OBC_STYP = sum(przyjety %in% "1" & styp %in% "1"),
      NBLPKT = sum(wynik < 0, na.rm = TRUE), # errors
      NBLZAKKAN = sum(!(czy_oplacony %in% "1") & zakwalifikowany %in% "1"),
      NBLPRZZAK = sum(!(zakwalifikowany %in% "1") & przyjety %in% "1"),
      MINWYN = ifelse(is.finite(MINWYN[1]), MINWYN[1], NA),
      NBLZAKPKT = sum(zakwalifikowany %in% "1" & wynik >= MINWYN & !is.na(wynik))
    ) %>%
    ungroup()
  # adding limits
  dictionary <- dictionary %>%
    group_by(!!groupingVariable) %>%
    summarise(
      LIM_OG = sum(limitog, na.rm = TRUE),
      LIM_P = sum(limitp, na.rm = TRUE),
      LIM_C = sum(limitc, na.rm = TRUE)
    ) %>%
    ungroup()
  cat("Przyłączanie danych o limitach przyjęć.\n")
  results <- join_with_check(dictionary, results,
                             "słowniku (danych o limitach przyjęć)",
                             "danych o rekrutacjach")
  #-----------------------------------------------------------------------------
  #|-> Here ends summarising the data
  #-----------------------------------------------------------------------------

  cat("--------------------\n",
      "Zapisywanie wyników.\n",
      sep = "")
  if (is.na(output)) {
    warning("Wyniki nie zostaną zapisane do pliku, ponieważ nie podano jego nazwy.",
            call. = FALSE, immediate. = TRUE)
  } else {
    write.csv2(results, output, row.names = FALSE, na = "",
               fileEncoding = "UTF-8")
    cat("Zapisano wyniki do pliku '", output, "'.\n", sep = "")
  }
  invisible(results)
}
