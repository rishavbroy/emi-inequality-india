# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose district matching
#'
#' @return Internal pipeline output used by the targets graph.
diagnose_district_matching <- function(district_panel, district_join_map, cfg) {
  data.frame(
    n_panel_rows = nrow(as.data.frame(district_panel)),
    n_join_rows = nrow(as.data.frame(district_join_map))
  )
}

#' extract unmatched districts
#'
#' @return Internal pipeline output used by the targets graph.
extract_unmatched_districts <- function(...) {
  tibble::tibble()
}

#' extract manual matches
#'
#' @return Internal pipeline output used by the targets graph.
extract_manual_matches <- function(...) {
  tibble::tibble()
}

#' extract many to many cases
#'
#' @return Internal pipeline output used by the targets graph.
extract_many_to_many_cases <- function(...) {
  tibble::tibble()
}

#' compare tracker to matched panel
#'
#' @return Internal pipeline output used by the targets graph.
compare_tracker_to_matched_panel <- function(...) {
  tibble::tibble()
}

#' save district matching diagnostics
#'
#' @return Internal pipeline output used by the targets graph.
save_district_matching_diagnostics <- function(diagnostics) {
  diagnostics
}
