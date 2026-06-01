# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate 2sls
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_2sls <- function(district_panel, formulas, cfg) {
  purrr::map(formulas, ~ ivreg::ivreg(.x, data = district_panel))
}

#' estimate consumption iv models
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_consumption_iv_models <- function(district_panel, formulas, cfg) {
  ivreg::ivreg(formulas$consumption, data = district_panel)
}

#' estimate gini iv models
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_gini_iv_models <- function(district_panel, formulas, cfg) {
  ivreg::ivreg(formulas$gini, data = district_panel)
}

#' estimate non iv comparisons
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_non_iv_comparisons <- function(district_panel, cfg) {
  list()
}

#' estimate model set
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_model_set <- function(district_panel, formulas, cfg) {
  estimate_2sls(district_panel, formulas, cfg)
}

