#' @title Reading file
#' @description Routine for reading files
#' @param file path to a file
#' @details Only .xlsx or .csv files are permitted. CSV files can be comma,
#' semicolon or tab separated and should be UTF-8 encoded.
#' @return Data frame with a data that has been read from a file.
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @importFrom utils read.table
read_file <- function(file) {
  stopifnot(is.character(file), length(file) == 1)
  fileType <- sub(".*[.]", "", file)
  if (!(fileType %in% c("xlsx", "csv"))) {
    stop(paste0("Pliki z danymi muszą być zapisane w formacie .xlsx lub .csv.\n",
                "(Problem dotyczy pliku: '", sub(".*[/\\]", "", file), "')"),
         call. = FALSE)
  }

  if (fileType %in% "xlsx") {
    read_xlsx(file, col_types = "text") %>%
      return()
  } else if (fileType %in% "csv") {
    # guessing the separator
    for (sep in c(";", ",", "\t")) {
      try <- tryCatch(read.table(file, header = TRUE, sep = sep, nrows = 1,
                                 colClasses = "character", encoding = "UTF-8"),
                      error = function(e) {stop("Nie udało się wczytać pliku.")})
      if (ncol(try) > 1) break
    }
    # reading the whole file
    read.table(file, header = TRUE, sep = sep, colClasses = "character",
               encoding = "UTF-8") %>%
      return()
  }
}
