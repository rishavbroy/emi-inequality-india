# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-ame-autodiff
# Use automatic differentiation for SE delta-method gradients, as in the legacy Rmd.

#' compute average marginal effects
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_average_marginal_effects <- function(selection_model, selection_data, cfg) {
  if (isFALSE(cfg$run_full_ame)) selection_data <- dplyr::slice_sample(selection_data, n = min(nrow(selection_data), cfg$sample_rows$selection %||% nrow(selection_data)))
  compute_ames_autodiff(selection_model, selection_data)
}

#' compute ames autodiff
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_ames_autodiff <- function(model, newdata) {
  marginaleffects::avg_slopes(model, newdata = newdata, wts = "weight", type = "response")
}

#' compute ames fast draft
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_ames_fast_draft <- function(model, newdata, n = 200) {
  marginaleffects::avg_slopes(model, newdata = dplyr::slice_sample(newdata, n = min(n, nrow(newdata))), wts = "weight", type = "response")
}

#' format ame results
#'
#' @return A tibble, model object, list, or file path depending on context.
format_ame_results <- function(ame_results) {
  ame_results
}

#' save ame results
#'
#' @return A tibble, model object, list, or file path depending on context.
save_ame_results <- function(ame_results, path = "outputs/tables/diagnostics/ame_results.csv") {
  readr::write_csv(tibble::as_tibble(ame_results), path); path
}

