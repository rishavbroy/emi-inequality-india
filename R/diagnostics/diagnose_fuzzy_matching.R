# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose fuzzy matching
#'
#' @return Function-specific return value.
diagnose_fuzzy_matching <- function(district_tracker, district_join_map, cfg) {
  data.frame(
    n_tracker_rows = nrow(as.data.frame(district_tracker)),
    n_join_rows = nrow(as.data.frame(district_join_map))
  )
}

#' benchmark string distance methods
#'
#' @return Function-specific return value.
benchmark_string_distance_methods <- function(pairs, methods, thresholds) {
  evaluate_distances(pairs, methods, thresholds)
}

#' test troublesome name pairs
#'
#' @return Function-specific return value.
test_troublesome_name_pairs <- function(...) {
  tibble::tibble()
}

#' summarize threshold sensitivity
#'
#' @return Function-specific return value.
summarize_threshold_sensitivity <- function(...) {
  tibble::tibble()
}

#' save fuzzy matching diagnostics
#'
#' @return Function-specific return value.
save_fuzzy_matching_diagnostics <- function(diagnostics) {
  diagnostics
}
