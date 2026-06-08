# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose district matching
#'
diagnose_district_matching <- function(district_panel, district_join_map, cfg) {
  data.frame(
    n_panel_rows = nrow(as.data.frame(district_panel)),
    n_join_rows = nrow(as.data.frame(district_join_map))
  )
}

#' extract unmatched districts
#'
extract_unmatched_districts <- function(...) {
  tibble::tibble()
}

#' extract manual matches
#'
extract_manual_matches <- function(...) {
  tibble::tibble()
}

#' extract many to many cases
#'
extract_many_to_many_cases <- function(...) {
  tibble::tibble()
}

#' compare tracker to matched panel
#'
compare_tracker_to_matched_panel <- function(...) {
  tibble::tibble()
}

#' save district matching diagnostics
#'
save_district_matching_diagnostics <- function(diagnostics) {
  diagnostics
}
