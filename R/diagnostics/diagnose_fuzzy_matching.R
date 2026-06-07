# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose fuzzy matching
#'
#' @return Internal pipeline output used by the targets graph.
diagnose_fuzzy_matching <- function(district_tracker, district_join_map, cfg) {
  data.frame(
    n_tracker_rows = nrow(as.data.frame(district_tracker)),
    n_join_rows = nrow(as.data.frame(district_join_map))
  )
}

#' benchmark string distance methods
#'
#' @return Internal pipeline output used by the targets graph.
benchmark_string_distance_methods <- function(pairs, methods, thresholds) {
  evaluate_distances(pairs, methods, thresholds)
}

#' test troublesome name pairs
#'
#' @return Internal pipeline output used by the targets graph.
test_troublesome_name_pairs <- function(...) {
  tibble::tibble()
}

#' summarize threshold sensitivity
#'
#' @return Internal pipeline output used by the targets graph.
summarize_threshold_sensitivity <- function(...) {
  tibble::tibble()
}

#' save fuzzy matching diagnostics
#'
#' @return Internal pipeline output used by the targets graph.
save_fuzzy_matching_diagnostics <- function(diagnostics) {
  diagnostics
}
