#' @title Reading file
#' @description Routine for reading files
#' @param file path to a file
#' @param columnsToCharacter logical value (TRUE or FALSE) - should all columns
#' be read as text (character)?
#' @details Only .xlsx or .csv files are permitted. CSV files can be comma,
#' semicolon or tab separated and should be UTF-8 encoded.
#' @return Data frame with a data that has been read from a file.
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @importFrom rlang set_names
#' @importFrom utils read.table
read_file <- function(file, columnsToCharacter = TRUE) {
  stopifnot(is.character(file), length(file) == 1,
            is.logical(columnsToCharacter), length(columnsToCharacter) == 1,
            columnsToCharacter %in% c(TRUE, FALSE))
  colTypes = ifelse(columnsToCharacter, "text", "guess")
  colClasses = ifelse(columnsToCharacter, "character", NA)

  fileType <- sub(".*[.]", "", file)
  if (!(fileType %in% c("xlsx", "csv"))) {
    stop(paste0("Pliki z danymi muszą być zapisane w formacie .xlsx lub .csv.\n",
                "(Problem dotyczy pliku: '", sub(".*[/\\]", "", file), "')"),
         call. = FALSE)
  }

  if (fileType %in% "xlsx") {
    file <- read_xlsx(file, col_types = colTypes)
  } else if (fileType %in% "csv") {
    # guessing the separator
    for (sep in c(";", ",", "\t")) {
      try <- tryCatch(read.table(file, header = TRUE, sep = sep, nrows = 1,
                                 colClasses = colClasses, encoding = "UTF-8"),
                      error = function(e) {stop("Nie udało się wczytać pliku.")})
      if (ncol(try) > 1) break
    }
    if (ncol(try) <= 1) {
      stop(paste0("Nieznany separator pól w pliku CSV (obsługiwane sepratory to: ',', ';' i tabulacja).",
                  "(Problem dotyczy pliku: '", sub(".*[/\\]", "", file), "')"),
           call. = FALSE)
    }
    # reading the whole file
    file <- read.table(file, header = TRUE, sep = sep, colClasses = colClasses,
                       encoding = "UTF-8")
  }
  file %>%
    set_names(names(file) %>% tolower()) %>%
    return()
}
