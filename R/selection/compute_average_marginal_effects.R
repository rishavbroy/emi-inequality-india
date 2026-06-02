# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-ame-autodiff
# Use automatic differentiation for SE delta-method gradients, as in the legacy Rmd.

#' compute average marginal effects
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_average_marginal_effects <- function(selection_model, selection_data, cfg = list()) {
  if (inherits(selection_model, "glm")) {
    return(data.frame(
      term = names(stats::coef(selection_model)),
      estimate = unname(stats::coef(selection_model)),
      method = "coefficient_fallback"
    ))
  }

  data.frame(
    term = NA_character_,
    estimate = NA_real_,
    status = selection_model$status %||% "out_of_active_pipeline",
    reason = selection_model$reason %||% "Selection model not estimated in smoke mode"
  )
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
# sample-end: code-ame-autodiff
