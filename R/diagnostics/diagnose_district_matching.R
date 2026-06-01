# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose district matching
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_district_matching <- function(district_panel, district_join_map, cfg) {
  list(panel = district_panel, join_map = district_join_map)
}

#' extract unmatched districts
#'
#' @return A tibble, model object, list, or file path depending on context.
extract_unmatched_districts <- function(...) {
  tibble::tibble()
}

#' extract manual matches
#'
#' @return A tibble, model object, list, or file path depending on context.
extract_manual_matches <- function(...) {
  tibble::tibble()
}

#' extract many to many cases
#'
#' @return A tibble, model object, list, or file path depending on context.
extract_many_to_many_cases <- function(...) {
  tibble::tibble()
}

#' compare tracker to matched panel
#'
#' @return A tibble, model object, list, or file path depending on context.
compare_tracker_to_matched_panel <- function(...) {
  tibble::tibble()
}

#' save district matching diagnostics
#'
#' @return A tibble, model object, list, or file path depending on context.
save_district_matching_diagnostics <- function(diagnostics) {
  diagnostics
}

