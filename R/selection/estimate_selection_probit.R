# sample-start: code-survey-probit

selection_probit_variables <- function(selection_data, require_all = FALSE) {
  variables <- c(
    "AGE", "SEX", "HH_SIZE", "RELIGION", "SOCIAL_GROUP", "SECTOR",
    "DIST_FROM_NEAREST_PRIMARY_CLASS", "father_educ",
    "dmean_num_IS_EDU_FREE", "dmean_num_TUTION_FEE_WAIVED",
    "dmean_num_RECD_SCHOLARSHIP_STIPEND", "dmean_num_RECD_TXT_BOOKS",
    "dmean_num_RECD_STATIONERY", "dmean_num_MID_DAY_MEAL_ETC_RECD",
    "dmean_num_ENROLLMENT_COST"
  )

  missing <- setdiff(variables, names(selection_data))
  if (isTRUE(require_all) && length(missing)) {
    stop(
      "Selection probit is missing required final analysis covariates: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  variables[variables %in% names(selection_data)]
}

selection_survey_design_variables <- function() {
  c("FSU_SL_NO", "weight", "STATE", "SECTOR", "STRATUM", "SUB_STRATUM_NO")
}

#' Retain the variables used by the selection model and survey design
#'
#' Earlier builds stored unrelated Block 5 schooling fields with the survey
#' design. Editing one of those fields therefore recomputed the marginal effects
#' even when the probit inputs were unchanged. Retain only variables used by the
#' model and survey design.
project_selection_model_data <- function(selection_data) {
  selection_data <- safe_df(selection_data)
  design <- c("enrolled", selection_survey_design_variables())
  keep <- unique(c(design, selection_probit_variables(selection_data)))
  selection_data[intersect(keep, names(selection_data))]
}

#' Estimate selection probit
#'
estimate_selection_probit <- function(selection_data, cfg) {
  if (!"enrolled" %in% names(selection_data) || all(is.na(selection_data$enrolled))) {
    return(list(status = "out_of_active_pipeline", reason = "No enrolled variable."))
  }

  final_mode <- is_final_mode(cfg)
  covars <- selection_probit_variables(selection_data, require_all = final_mode)
  if (!length(covars)) {
    return(list(status = "out_of_active_pipeline", reason = "No probit covariates."))
  }
  f_probit <- stats::reformulate(covars, response = "enrolled")

  if (final_mode) {
    if (!requireNamespace("survey", quietly = TRUE)) {
      stop("The final selection probit requires the survey package.", call. = FALSE)
    }
    design <- build_survey_design_selection(selection_data, require_all = TRUE)
    out <- with_survey_lonely_psu(
      fit_selection_probit(design, f_probit)
    )
    return(stabilize_selection_model_formula(out, f_probit))
  }

  out <- stats::glm(
    f_probit,
    data = selection_data,
    family = stats::binomial(link = "probit")
  )
  stabilize_selection_model_formula(out, f_probit)
}

#' Store the fitted selection formula inside the saved model call
#'
#' Programmatic fitting originally left the local symbol `f_probit` in the saved
#' call. After {targets} serialized the model, packages reconstructing model data
#' could no longer resolve that local name. Store the formula itself in the call
#' and keep the formula attribute used by downstream validation.
stabilize_selection_model_formula <- function(model, formula) {
  if (!inherits(formula, "formula")) {
    stop("Selection model formula must inherit from formula.", call. = FALSE)
  }
  if (!is.null(model$call)) model$call$formula <- formula
  attr(model, "selection_probit_formula") <- formula
  model
}

#' Build survey design selection
#'
build_survey_design_selection <- function(selection_df, require_all = FALSE) {
  required <- selection_survey_design_variables()
  missing <- setdiff(required, names(selection_df))
  if (isTRUE(require_all) && length(missing)) {
    stop(
      "Selection survey design is missing required final analysis fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  psu <- first_col(selection_df, c("FSU_SL_NO", "fsu", "PSU", "psu"))
  weight <- first_col(selection_df, c("weight", "WEIGHT", "Multiplier", "multiplier"))
  strata_cols <- intersect(c("STATE", "SECTOR", "STRATUM", "SUB_STRATUM_NO"), names(selection_df))
  if (is.null(psu) || is.null(weight) || !length(strata_cols)) {
    if (isTRUE(require_all)) {
      stop("The final selection survey design could not be constructed.", call. = FALSE)
    }
    return(NULL)
  }
  selection_df$.survey_strata <- interaction(selection_df[strata_cols], drop = TRUE)
  survey::svydesign(
    ids = stats::as.formula(paste0("~", psu)),
    strata = ~.survey_strata,
    weights = stats::as.formula(paste0("~", weight)),
    data = selection_df,
    nest = TRUE
  )
}

#' Fit selection probit
#'
fit_selection_probit <- function(selection_design, f_probit) {
  survey::svyglm(
    f_probit,
    design = selection_design,
    family = stats::quasibinomial(link = "probit")
  )
}

# sample-end: code-survey-probit
