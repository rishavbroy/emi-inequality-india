# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-table-generation

#' make tables
#'
#' @return A tibble, model object, list, or file path depending on context.
make_tables <- function(selection_data, ame_results, district_panel, iv_models, first_stage_tests, cfg) {
  list()
}

#' make selection summary numeric table
#'
#' @return A tibble, model object, list, or file path depending on context.
make_selection_summary_numeric_table <- function(selection_data) {
  selection_data
}

#' make selection summary categorical table
#'
#' @return A tibble, model object, list, or file path depending on context.
make_selection_summary_categorical_table <- function(selection_data) {
  selection_data
}

#' make probit ame table
#'
#' @return A tibble, model object, list, or file path depending on context.
make_probit_ame_table <- function(ame_results) {
  ame_results
}

#' make iv summary table
#'
#' @return A tibble, model object, list, or file path depending on context.
make_iv_summary_table <- function(district_panel) {
  district_panel
}

#' make first stage table
#'
#' @return A tibble, model object, list, or file path depending on context.
make_first_stage_table <- function(first_stage_tests) {
  first_stage_tests
}

#' make second stage table
#'
#' @return A tibble, model object, list, or file path depending on context.
make_second_stage_table <- function(iv_models) {
  iv_models
}

#' make diagnostic tables
#'
#' @return A tibble, model object, list, or file path depending on context.
make_diagnostic_tables <- function(...) {
  list()
}

