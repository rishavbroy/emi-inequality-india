# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate first stage
#'
#' @return A tibble, model object, list, or file path depending on context.
estimate_first_stage <- function(iv_models, district_panel, cfg) {
  rows <- lapply(names(iv_models), function(model_name) {
    model <- iv_models[[model_name]]
    if (!inherits(model, "ivreg")) {
      return(data.frame(
        model = model_name,
        term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        status = model$status %||% "out_of_active_pipeline",
        reason = model$reason %||% NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    regressor_formula <- tryCatch(stats::formula(model, component = "regressors"), error = function(e) NULL)
    instrument_formula <- tryCatch(stats::formula(model, component = "instruments"), error = function(e) NULL)
    if (is.null(regressor_formula) || is.null(instrument_formula)) {
      return(data.frame(
        model = model_name,
        term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        status = "out_of_active_pipeline",
        reason = "Could not parse first-stage IV formula.",
        stringsAsFactors = FALSE
      ))
    }

    regressors <- all.vars(regressor_formula[[3]])
    instruments <- all.vars(instrument_formula[[3]])
    endogenous <- setdiff(regressors, instruments)
    if (!length(endogenous)) endogenous <- regressors[[1]]

    first_stage_formula <- stats::as.formula(paste(
      endogenous[[1]],
      "~",
      paste(instruments, collapse = " + ")
    ))
    fit <- stats::lm(first_stage_formula, data = district_panel)
    coefs <- as.data.frame(summary(fit)$coefficients)
    coefs$term <- rownames(coefs)
    rownames(coefs) <- NULL
    data.frame(
      model = model_name,
      term = coefs$term,
      estimate = coefs$Estimate,
      std.error = coefs$`Std. Error`,
      statistic = coefs$`t value`,
      p.value = coefs$`Pr(>|t|)`,
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' tidy first stage results
#'
#' @return A tibble, model object, list, or file path depending on context.
tidy_first_stage_results <- function(first_stage) {
  first_stage
}

#' compute partial f statistics
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_partial_f_statistics <- function(first_stage) {
  first_stage
}

#' compute partial r2
#'
#' @return A tibble, model object, list, or file path depending on context.
compute_partial_r2 <- function(first_stage) {
  first_stage
}
