# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-ame-autodiff
# Use automatic differentiation for SE delta-method gradients, as in the legacy Rmd.

#' compute average marginal effects
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_average_marginal_effects <- function(selection_model, selection_data, cfg = list()) {
  if (!inherits(selection_model, "glm")) {
    return(ame_out_of_pipeline(
      selection_model$status %||% "out_of_active_pipeline",
      selection_model$reason %||% "Selection model not estimated in smoke mode"
    ))
  }

  if (!isTRUE(cfg$run_full_ame)) {
    return(format_ame_results(data.frame(
      term = names(stats::coef(selection_model)),
      estimate = unname(stats::coef(selection_model)),
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      method = "coefficient_fallback",
      status = "estimated",
      reason = "Draft config uses coefficient fallback instead of full AME computation"
    )))
  }

  if (!requireNamespace("marginaleffects", quietly = TRUE)) {
    return(ame_out_of_pipeline(
      "out_of_active_pipeline",
      "Package marginaleffects is required for full AME computation"
    ))
  }

  format_ame_results(compute_ames_autodiff(selection_model, selection_data))
}

ame_out_of_pipeline <- function(status, reason) {
  data.frame(
    term = NA_character_,
    estimate = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    method = "not_run",
    status = status,
    reason = reason
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
  out <- tibble::as_tibble(ame_results)
  if (!"term" %in% names(out) && "variable" %in% names(out)) out$term <- out$variable
  if (!"method" %in% names(out)) out$method <- "autodiff"
  if (!"status" %in% names(out)) out$status <- "estimated"
  if (!"reason" %in% names(out)) out$reason <- NA_character_

  required <- c(
    "term", "estimate", "std.error", "statistic", "p.value",
    "conf.low", "conf.high", "method", "status", "reason"
  )
  for (nm in setdiff(required, names(out))) out[[nm]] <- NA
  out[, required, drop = FALSE]
}

#' save ame results
#'
#' @return A tibble, model object, list, or file path depending on context.
save_ame_results <- function(ame_results, path = "outputs/tables/diagnostics/ame_results.csv") {
  readr::write_csv(tibble::as_tibble(ame_results), path); path
}
# sample-end: code-ame-autodiff
