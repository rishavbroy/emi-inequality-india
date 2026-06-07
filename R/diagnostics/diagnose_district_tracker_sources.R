# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose district tracker sources
#'
#' @return Internal pipeline output used by the targets graph.
diagnose_district_tracker_sources <- function(raw_district_changes, district_tracker, cfg) {
  data.frame(
    source_file_id = names(raw_district_changes),
    n_rows = vapply(raw_district_changes, function(x) nrow(as.data.frame(x)), integer(1)),
    stringsAsFactors = FALSE
  )
}

#' compare tracker source coverage
#'
#' @return Internal pipeline output used by the targets graph.
compare_tracker_source_coverage <- function(...) {
  tibble::tibble()
}

#' find source disagreements
#'
#' @return Internal pipeline output used by the targets graph.
find_source_disagreements <- function(...) {
  tibble::tibble()
}

#' summarize tracker source errors
#'
#' @return Internal pipeline output used by the targets graph.
summarize_tracker_source_errors <- function(...) {
  tibble::tibble()
}

#' save tracker source diagnostics
#'
#' @return Internal pipeline output used by the targets graph.
save_tracker_source_diagnostics <- function(diagnostics) {
  diagnostics
}
