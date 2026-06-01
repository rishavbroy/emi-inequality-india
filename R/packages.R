# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' project packages
#'
#' @return A tibble, model object, list, or file path depending on context.
project_packages <- function() {
  c("tidyverse", "haven", "readxl", "sf", "spdep", "survey", "marginaleffects", "ivreg", "sandwich", "lmtest", "modelsummary", "kableExtra", "stringdist", "fuzzyjoin", "magick", "yaml", "targets", "tarchetypes", "testthat", "broom", "quarto", "readODS", "car")
}

#' load project packages
#'
#' @return A tibble, model object, list, or file path depending on context.
load_project_packages <- function() {
  invisible(lapply(project_packages(), require, character.only = TRUE))
}

#' check project packages
#'
#' @return A tibble, model object, list, or file path depending on context.
check_project_packages <- function() {
  missing <- project_packages()[!vapply(project_packages(), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) warning("Missing packages: ", paste(missing, collapse = ", "))
  invisible(missing)
}

