# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# ---TROUBLESHOOTING---
# If getting "file does not exist" or similar errors:
# Switch the function used to read in the file e.g., if the relevant file is being read in using read_sav() or read_sav_short(), switch to read_sav_short() or read_sav() respectively.
# If that didn't work and you're not using Windows: Either modify this code chunk for your OS (note the "if" statement at the start of each function) or manually shorten the error-inducing file paths.
# Purpose of this chunk: To ensure R can identify and read in all necessary files.
# This project found me making frequent use of file paths over 260 characters in length.
# Out of my naivete, I didn't realize Windows File Explorer automatically shortens such file paths by rewriting them in the 8.3 filename convention---meaning R can no longer access their standard "long name."

#' read with short path
#'
#' @return Reader output from the supplied path.
read_with_short_path <- function(path, reader, ..., binary_connection = TRUE) {
  if (!file.exists(path)) stop("File does not exist: ", path, call. = FALSE)
  if (Sys.info()[["sysname"]] != "Windows") return(reader(path, ...))
  short <- get_windows_short_path(path)
  if (!binary_connection) return(reader(short, ...))
  # Open a connection ("con") in binary mode ("rb" = "read binary"; SPSS files are binary i.e. non-text files, and thus best read in as such) to the 8.3 filename
  con <- file(short, "rb")
  on.exit(close(con), add = TRUE)
  reader(con, ...)
}

#' read sav short
#'
#' @return Data frame read from an SPSS file.
read_sav_short <- function(long_path, ...) {
  need_pkg("haven", "SPSS files")
  read_with_short_path(long_path, haven::read_sav, ...)
}

#' read csv short
#'
#' @return Data frame read from a CSV file.
read_csv_short <- function(long_path, ...) {
  if (requireNamespace("readr", quietly = TRUE)) {
    read_with_short_path(long_path, readr::read_csv, ..., show_col_types = FALSE)
  } else {
    utils::read.csv(long_path, stringsAsFactors = FALSE, ...)
  }
}

#' read excel short
#'
#' @return Data frame read from an Excel file.
read_excel_short <- function(long_path, sheet = 1, ...) {
  need_pkg("readxl", "Excel files")
  readxl::read_excel(normalize_path_for_os(long_path), sheet = sheet, ...)
}

#' read ODS using a path normalized for this OS
#'
#' @return Data frame read from an ODS file.
read_ods_short <- function(long_path, ...) {
  need_pkg("readODS", "ODS files")
  readODS::read_ods(long_path, ...)
}

#' normalize path for os
#'
#' @return Normalized path string.
normalize_path_for_os <- function(path) {
  normalizePath(path, mustWork = FALSE)
}

#' get windows short path
#'
#' @return Windows 8.3 short path on Windows, otherwise the original path.
get_windows_short_path <- function(long_path) {
  if (Sys.info()[["sysname"]] != "Windows") return(long_path)
  shell(sprintf('for %%I in ("%s") do @echo %%~sI', long_path), intern = TRUE)[[1]]
}
