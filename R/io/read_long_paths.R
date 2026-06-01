# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# Long troubleshooting notes belong in docs/long-paths-and-8-3-filenames.qmd.

#' read with short path
#'
#' @return A tibble, model object, list, or file path depending on context.
read_with_short_path <- function(path, reader, ...) {
  reader(normalizePath(path, mustWork = FALSE), ...)
}

#' read sav short
#'
#' @return A tibble, model object, list, or file path depending on context.
read_sav_short <- function(path, ...) {
  read_with_short_path(path, haven::read_sav, ...)
}

#' read csv short
#'
#' @return A tibble, model object, list, or file path depending on context.
read_csv_short <- function(path, ...) {
  read_with_short_path(path, readr::read_csv, ...)
}

#' read excel short
#'
#' @return A tibble, model object, list, or file path depending on context.
read_excel_short <- function(path, sheet = NULL, ...) {
  if (is.null(sheet)) read_with_short_path(path, readxl::read_excel, ...) else read_with_short_path(path, readxl::read_excel, sheet = sheet, ...)
}

#' normalize path for os
#'
#' @return A tibble, model object, list, or file path depending on context.
normalize_path_for_os <- function(path) {
  normalizePath(path, mustWork = FALSE)
}

#' get windows short path
#'
#' @return A tibble, model object, list, or file path depending on context.
get_windows_short_path <- function(path) {
  if (.Platform$OS.type != "windows") return(path)
  utils::shortPathName(path)
}

