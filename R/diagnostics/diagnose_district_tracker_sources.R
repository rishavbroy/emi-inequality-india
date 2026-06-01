# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose district tracker sources
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_district_tracker_sources <- function(raw_district_changes, district_tracker, cfg) {
  list(raw_sources = names(raw_district_changes), tracker = district_tracker)
}

#' compare tracker source coverage
#'
#' @return A tibble, model object, list, or file path depending on context.
compare_tracker_source_coverage <- function(...) {
  tibble::tibble()
}

#' find source disagreements
#'
#' @return A tibble, model object, list, or file path depending on context.
find_source_disagreements <- function(...) {
  tibble::tibble()
}

#' summarize tracker source errors
#'
#' @return A tibble, model object, list, or file path depending on context.
summarize_tracker_source_errors <- function(...) {
  tibble::tibble()
}

#' save tracker source diagnostics
#'
#' @return A tibble, model object, list, or file path depending on context.
save_tracker_source_diagnostics <- function(diagnostics) {
  diagnostics
}

