# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate selection probit
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_selection_probit <- function(selection_data, cfg) {
  stop("TODO: migrate survey-weighted probit from legacy chunk 9")
}

#' build survey design selection
#'
#' @return A tibble, model object, list, or file path depending on context.
build_survey_design_selection <- function(selection_df) {
  survey::svydesign(ids = ~1, data = selection_df)
}

#' fit selection probit
#'
#' @return A tibble, model object, list, or file path depending on context.
fit_selection_probit <- function(selection_design, f_probit) {
  survey::svyglm(f_probit, design = selection_design, family = binomial(link = "probit"))
}

#' compute inverse mills ratio
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_inverse_mills_ratio <- function(model, selection_df, f_probit) {
  selection_df
}

#' tidy selection model
#'
#' @return A tibble, model object, list, or file path depending on context.
tidy_selection_model <- function(model) {
  broom::tidy(model)
}

