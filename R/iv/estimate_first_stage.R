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

    iv_terms <- parse_iv_formula_terms(model)
    if (is.null(iv_terms)) {
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

    endogenous <- setdiff(iv_terms$regressors, iv_terms$instruments)
    if (!length(endogenous)) endogenous <- iv_terms$regressors[[1]]

    first_stage_formula <- stats::as.formula(paste(
      endogenous[[1]],
      "~",
      paste(iv_terms$instruments, collapse = " + ")
    ))
    fit <- stats::lm(first_stage_formula, data = district_panel)
    coefs <- as.data.frame(summary(fit)$coefficients)
    coefs$term <- rownames(coefs)
    rownames(coefs) <- NULL
    excluded <- setdiff(iv_terms$instruments, iv_terms$regressors)
    excluded_term <- if (length(excluded)) excluded[[1]] else NA_character_
    excluded_row <- match(excluded_term, coefs$term)
    partial_f <- if (!is.na(excluded_row)) coefs$`t value`[[excluded_row]]^2 else NA_real_
    partial_p <- if (!is.na(excluded_row)) coefs$`Pr(>|t|)`[[excluded_row]] else NA_real_
    data.frame(
      model = model_name,
      term = coefs$term,
      estimate = coefs$Estimate,
      std.error = coefs$`Std. Error`,
      statistic = coefs$`t value`,
      p.value = coefs$`Pr(>|t|)`,
      partial_f = partial_f,
      partial_p = partial_p,
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

parse_iv_formula_terms <- function(model) {
  f <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (is.null(f) || length(f) < 3L || !is.call(f[[3]]) || !identical(f[[3]][[1]], as.name("|"))) {
    return(NULL)
  }
  list(
    regressors = all.vars(f[[3]][[2]]),
    instruments = all.vars(f[[3]][[3]])
  )
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
