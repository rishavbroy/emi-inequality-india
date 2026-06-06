# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose district matching
#'
#' @return Function-specific return value.
diagnose_district_matching <- function(district_panel, district_join_map, cfg) {
  data.frame(
    n_panel_rows = nrow(as.data.frame(district_panel)),
    n_join_rows = nrow(as.data.frame(district_join_map))
  )
}

#' extract unmatched districts
#'
#' @return Function-specific return value.
extract_unmatched_districts <- function(...) {
  tibble::tibble()
}

#' extract manual matches
#'
#' @return Function-specific return value.
extract_manual_matches <- function(...) {
  tibble::tibble()
}

#' extract many to many cases
#'
#' @return Function-specific return value.
extract_many_to_many_cases <- function(...) {
  tibble::tibble()
}

#' compare tracker to matched panel
#'
#' @return Function-specific return value.
compare_tracker_to_matched_panel <- function(...) {
  tibble::tibble()
}

#' save district matching diagnostics
#'
#' @return Function-specific return value.
save_district_matching_diagnostics <- function(diagnostics) {
  diagnostics
}
