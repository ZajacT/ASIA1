#' @title Lists foreigners and their admissions status
#' @description Function lists foreigners and their admissions status and writes
#' results to a file
#' @param registrations optionally path to the file with data on registrations
#' @param scores optionally path to the file with data on recruitment scores
#' @param output optionally path to the file in which results will be saved;
#' if \code{NA} is given as a value of this parameter, results won't be saved
#' to a file
#' @details
#' Location of files contaninig the data to be checked and location of file
#' in which results will be saved can be described noninteractively with
#' function arguments described above or - if any of this arguments is omitted -
#' interactively with a system file-selection dialog.
#' @return Data frame (tibble) with the list of foreign applicants - the same as
#' written to a file described with the \code{output} parameter (data frame is
#' returned invisibly).
#' @importFrom dplyr filter select
#' @importFrom rlang ensym
#' @importFrom utils write.csv2
#' @export
foreigners <- function(registrations = NULL, scores = NULL, output = NULL) {
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

  cat("--------------------\n",
      "Łączenie pliku z danymi o rekrutacjach z danymi o punktach rekrutacyjnych.\n",
      sep = "")
  registrations <- join_with_check(registrations, scores,
                                   "danych o rekrutacjach",
                                   "danych o punktach rekrutacyjnych")

  cat("--------------------\n",
      "Wyszukiwanie obserwacji.\n",
      sep = "")
  #-----------------------------------------------------------------------------
  #|-> Here starts filtering data
  #-----------------------------------------------------------------------------
  results <- registrations %>%
    select(pesel, studia, sciezka, imie, imie2, nazwisko, obywatelstwo,
           czy_oplacony, zakwalifikowany, przyjety, wynik) %>%
    filter(!(obywatelstwo %in% "PL"))
  #-----------------------------------------------------------------------------
  #|-> Here ends filtering data
  #-----------------------------------------------------------------------------

  cat("--------------------\n",
      "Zapisywanie listy\n",
      sep = "")
  if (is.null(output)) {
    output <- choose_file(", w którym ma zostać zapisana lista obcokrajowców (plik zostanie zapisany w formacie CSV ze średnikiem jako separatorem pola)",
                          errorOnCancel = FALSE)
  }
  if (!is.na(output)) {
    output <- sub("[.]csv$", "", output) %>% paste0(".csv")
    if (!(check_output_path(output, "output") %in% TRUE)) {
      output <- NA
    }
  }
  if (is.na(output)) {
    warning("Lista nie zostanie zapisana do pliku, ponieważ nie podano jego nazwy.",
            call. = FALSE, immediate. = TRUE)
  } else {
    write.csv2(results, output, row.names = FALSE, na = "",
               fileEncoding = "UTF-8")
    cat("Zapisano listę do pliku '", output, "'.\n", sep = "")
  }
  invisible(results)
}
