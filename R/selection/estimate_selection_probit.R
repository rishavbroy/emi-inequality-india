# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-survey-probit-imr

#' estimate selection probit
#'
#' @return Function-specific return value.
estimate_selection_probit <- function(selection_data, cfg) {
  if (!"enrolled" %in% names(selection_data) || all(is.na(selection_data$enrolled))) {
    return(list(status = "out_of_active_pipeline", reason = "No enrolled variable."))
  }
  covars <- intersect(
    c(
      "AGE", "SEX", "HH_SIZE", "RELIGION", "SOCIAL_GROUP", "SECTOR",
      "age", "sex", "hh_size", "religion", "social_group", "sector",
      "DIST_FROM_NEAREST_PRIMARY_CLASS",
      "dmean_num_IS_EDU_FREE", "dmean_num_RECD_TXT_BOOKS"
    ),
    names(selection_data)
  )
  if (!length(covars)) {
    return(list(status = "out_of_active_pipeline", reason = "No probit covariates."))
  }
  # Keep the probit descriptive if it is not currently part of the causal design.
  stats::glm(
    stats::as.formula(paste("enrolled ~", paste(covars, collapse = "+"))),
    data = selection_data,
    family = stats::binomial(link = "probit")
  )
}

#' build survey design selection
#'
#' @return Function-specific return value.
build_survey_design_selection <- function(selection_df) {
  survey::svydesign(ids = ~1, data = selection_df)
}

#' fit selection probit
#'
#' @return Function-specific return value.
fit_selection_probit <- function(selection_design, f_probit) {
  survey::svyglm(f_probit, design = selection_design, family = binomial(link = "probit"))
}

#' compute inverse mills ratio
#'
#' @return Function-specific return value.
compute_inverse_mills_ratio <- function(model, selection_df, f_probit) {
  selection_df
}

#' tidy selection model
#'
#' @return Function-specific return value.
tidy_selection_model <- function(model) {
  broom::tidy(model)
}

# sample-end: code-survey-probit-imr
