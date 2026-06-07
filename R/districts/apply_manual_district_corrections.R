# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' apply manual district corrections
#'
#' @return Internal pipeline output used by the targets graph.
apply_manual_district_corrections <- function(tracker, corrections_path = "data/metadata/manual_district_corrections.csv") {
  if (!file.exists(corrections_path)) return(tracker)
  corrections <- utils::read.csv(corrections_path, stringsAsFactors = FALSE)
  validate_manual_corrections(corrections, tracker)
  attr(tracker, "manual_corrections") <- corrections
  tracker
}

#' validate manual corrections
#'
#' @return Internal pipeline output used by the targets graph.
validate_manual_corrections <- function(corrections, tracker) {
  required <- c("correction_id", "source_year", "source_dataset", "state_raw", "district_raw", "correction_type", "reason")
  missing <- setdiff(required, names(corrections))
  if (length(missing)) stop("Manual corrections missing columns: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

#' apply rename corrections
#'
#' @return Internal pipeline output used by the targets graph.
apply_rename_corrections <- function(tracker, corrections) {
  tracker
}

#' apply split merge corrections
#'
#' @return Internal pipeline output used by the targets graph.
apply_split_merge_corrections <- function(tracker, corrections) {
  tracker
}

#' apply typo corrections
#'
#' @return Internal pipeline output used by the targets graph.
apply_typo_corrections <- function(tracker, corrections) {
  tracker
}
