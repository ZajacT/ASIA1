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
#' @importFrom dplyr group_by mutate n summarise filter
#' @importFrom rlang ensym
#' @importFrom utils write.csv2
#' @export
admission_statistics <- function(groupingVariable = "studia", registrations = NULL, scores = NULL,
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
    output <- choose_file(", w którym mają zostać zapisane PKTi (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
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

  cat("--------------------\n",
      "Obliczanie statystyk.\n",
      sep = "")
  #-----------------------------------------------------------------------------
  #|-> Here starts summarising the data
  #-----------------------------------------------------------------------------
  results <- registrations %>%
    group_by(!!groupingVariable) %>% 
    # summarising
    summarise(
      NREJ = sum(REJ > 0), # number of registrations
      NZAK = sum(ZAK > 0), # number of qualified cand.
      NPRZ = sum(PRZ > 0), # number of admitted cand.
      PRZ_PKT_NNa=sum(!is.na(PKT)),
      PRZ_PKT_D1=ifelse(sum(!is.na(PKT[PRZ > 0]))>9,round(quantile(PKT[PRZ > 0 & !is.na(PKT)], probs=0.1, na.rm=TRUE),0),NA),
      PRZ_PKT_Q1=ifelse(sum(!is.na(PKT[PRZ > 0]))>9,round(quantile(PKT[PRZ > 0 & !is.na(PKT)], probs=0.25, na.rm=TRUE),0),NA),
      PRZ_PKT_Q3=ifelse(sum(!is.na(PKT[PRZ > 0]))>9,round(quantile(PKT[PRZ > 0 & !is.na(PKT)], probs=0.75, na.rm=TRUE),0),NA),
      PRZ_PKT_D9=ifelse(sum(!is.na(PKT[PRZ > 0]))>9,round(quantile(PKT[PRZ > 0 & !is.na(PKT)], probs=0.9, na.rm=TRUE),0),NA),
      PRZ_PKT_MEA=ifelse(sum(!is.na(PKT[PRZ > 0])>9,round(mean(PKT[PRZ > 0 & !is.na(PKT)]),0),NA))
      ) %>% 
    ungroup()

    #-----------------------------------------------------------------------------
    #|-> Exam scores
    #-----------------------------------------------------------------------------
    
# przepraszam, ale nie umiałem bez zrobienia postaci szerokiej    
    exams$wynik_p <- as.numeric(exams$wynik_p)
    exams$wynik_r <- as.numeric(exams$wynik_r)
    
    mat_p <- exams %>% 
      select(pesel,egzamin,wynik_p) %>%
      filter(wynik_p > 0)
   
    mat_p<- dcast(mat_p, pesel ~ egzamin, max)
    
    colnames(mat_p)[2:length(colnames(mat_p))] <- paste(colnames(mat_p)[2:length(colnames(mat_p))],"P", sep = "_")
    
    mat_r <- exams %>% 
      select(pesel,egzamin,wynik_r) %>%
      filter(wynik_r > 0)

    mat_r <- dcast(mat_r, pesel ~ egzamin, max)
    
    colnames(mat_r)[2:length(colnames(mat_r))] <- paste(colnames(mat_r)[2:length(colnames(mat_r))],"R", sep = "_")
    
    exams <- full_join(mat_p,mat_r)
    
    mat_res <- registrations %>% filter(PRZ > 0) %>% 
      left_join(exams) %>%
      select(!!groupingVariable,starts_with("M_")) %>%
      group_by(!!groupingVariable) %>%
      summarise_at(vars(matches("M_")),
                   funs(PN=sum(.>0, na.rm=TRUE),
                        PPROC=round((sum(.>0, na.rm=TRUE)/n()),2),
                        PSR=ifelse(sum(.>0, na.rm=TRUE)>9,round(mean(.,na.rm=TRUE),0),NA),
                        PD1=ifelse(sum(.>0, na.rm=TRUE)>9,round(quantile(., probs=0.1, na.rm=TRUE),0),NA),
                        PQ1=ifelse(sum(.>0, na.rm=TRUE)>9,round(quantile(., probs=0.25, na.rm=TRUE),0),NA),
                        PQ3=ifelse(sum(.>0, na.rm=TRUE)>9,round(quantile(., probs=0.75, na.rm=TRUE),0),NA),
                        PD9=ifelse(sum(.>0, na.rm=TRUE)>9,round(quantile(., probs=0.9, na.rm=TRUE),0),NA)
                   )
      ) %>% # liczba, średnia, kwartyle
    ungroup()

    cat("Przyłączanie danych o wynikach egzaminów maturalnych.\n")    
    results <- join_with_check(results, mat_res,
                               "danych o rekrutacjach",
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
