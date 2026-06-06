# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate 2sls
#'
#' @return Function-specific return value.
estimate_2sls <- function(district_panel, formulas, cfg) {
  lapply(formulas, function(formula) {
    vars <- all.vars(formula)
    missing <- setdiff(vars, names(district_panel))
    if (length(missing)) {
      return(list(
        status = "out_of_active_pipeline",
        reason = paste("Missing variables:", paste(missing, collapse = ", "))
      ))
    }
    if (!requireNamespace("ivreg", quietly = TRUE)) {
      return(list(status = "out_of_active_pipeline", reason = "Package ivreg not installed."))
    }
    ivreg::ivreg(formula, data = district_panel)
  })
}

#' estimate consumption iv models
#'
#' @return Function-specific return value.
estimate_consumption_iv_models <- function(district_panel, formulas, cfg) {
  ivreg::ivreg(formulas$consumption, data = district_panel)
}

#' estimate gini iv models
#'
#' @return Function-specific return value.
estimate_gini_iv_models <- function(district_panel, formulas, cfg) {
  ivreg::ivreg(formulas$gini, data = district_panel)
}

#' estimate non iv comparisons
#'
#' @return Function-specific return value.
estimate_non_iv_comparisons <- function(district_panel, cfg) {
  list()
}

#' estimate model set
#'
#' @return Function-specific return value.
estimate_model_set <- function(district_panel, formulas, cfg) {
  estimate_2sls(district_panel, formulas, cfg)
}
