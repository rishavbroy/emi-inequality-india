# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose weak instruments
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_weak_instruments <- function(iv_models, district_panel, cfg) {
  tibble::tibble(metric = character(), value = numeric())
}

#' jackknife first stage by state
#'
#' @return A tibble, model object, list, or file path depending on context.
jackknife_first_stage_by_state <- function(...) {
  stop("TODO")
}

#' jackknife first stage by region
#'
#' @return A tibble, model object, list, or file path depending on context.
jackknife_first_stage_by_region <- function(...) {
  stop("TODO")
}

#' summarize weak iv metrics
#'
#' @return A tibble, model object, list, or file path depending on context.
summarize_weak_iv_metrics <- function(...) {
  stop("TODO")
}

