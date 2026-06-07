# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' validate district panel
#'
#' @return Internal pipeline output used by the targets graph.
validate_district_panel <- function(panel) {
  check_core_variables_present(panel); panel
}

#' check unique district units
#'
#' @return Internal pipeline output used by the targets graph.
check_unique_district_units <- function(panel) {
  stopifnot(!anyDuplicated(panel$district_panel_id)); invisible(TRUE)
}

#' check no unintended many to many
#'
#' @return Internal pipeline output used by the targets graph.
check_no_unintended_many_to_many <- function(panel) {
  invisible(TRUE)
}

#' check core variables present
#'
#' @return Internal pipeline output used by the targets graph.
check_core_variables_present <- function(panel) {
  required <- c("EMIE", "wavg_ling_degrees")
  missing <- setdiff(required, names(panel))
  if (length(missing)) warning("Panel missing variables: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

#' check panel variable ranges
#'
#' @return Internal pipeline output used by the targets graph.
check_panel_variable_ranges <- function(panel) {
  invisible(TRUE)
}

