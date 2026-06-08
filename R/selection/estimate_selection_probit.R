# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-survey-probit-imr

#' estimate selection probit
#'
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
  f_probit <- stats::as.formula(paste("enrolled ~", paste(covars, collapse = "+")))
  if (identical(cfg$mode, "final") && requireNamespace("survey", quietly = TRUE)) {
    design <- build_survey_design_selection(selection_data)
    if (!is.null(design)) {
      return(fit_selection_probit(design, f_probit))
    }
  }
  stats::glm(f_probit, data = selection_data, family = stats::binomial(link = "probit"))
}

#' build survey design selection
#'
build_survey_design_selection <- function(selection_df) {
  psu <- first_col(selection_df, c("FSU_SL_NO", "fsu", "PSU", "psu"))
  weight <- first_col(selection_df, c("weight", "WEIGHT", "Multiplier", "multiplier"))
  strata_cols <- intersect(c("STATE", "state_std", "STRATUM", "SUB_STRATUM_NO"), names(selection_df))
  if (is.null(psu) || is.null(weight) || !length(strata_cols)) return(NULL)
  options(survey.lonely.psu = "average")
  strata <- interaction(selection_df[strata_cols], drop = TRUE)
  selection_df$.survey_strata <- strata
  survey::svydesign(
    ids = stats::as.formula(paste0("~", psu)),
    strata = ~.survey_strata,
    weights = stats::as.formula(paste0("~", weight)),
    data = selection_df,
    nest = TRUE
  )
}

#' fit selection probit
#'
fit_selection_probit <- function(selection_design, f_probit) {
  survey::svyglm(f_probit, design = selection_design, family = quasibinomial(link = "probit"))
}

#' compute inverse mills ratio
#'
compute_inverse_mills_ratio <- function(model, selection_df, f_probit) {
  selection_df
}

#' tidy selection model
#'
tidy_selection_model <- function(model) {
  broom::tidy(model)
}

# sample-end: code-survey-probit-imr
