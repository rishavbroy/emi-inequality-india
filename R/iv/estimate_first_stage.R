# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' estimate first stage
#'
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
    vc <- first_stage_vcov(fit, district_panel)
    coefs <- if (is.null(vc)) {
      as.data.frame(summary(fit)$coefficients)
    } else {
      as.data.frame(lmtest::coeftest(fit, vcov. = vc))
    }
    coefs$term <- rownames(coefs)
    rownames(coefs) <- NULL
    excluded <- setdiff(iv_terms$instruments, iv_terms$regressors)
    excluded_term <- if (length(excluded)) excluded[[1]] else NA_character_
    excluded_row <- match(excluded_term, coefs$term)
    wald <- first_stage_wald_test(fit, excluded_term, vc)
    statistic_col <- intersect(c("t value", "z value", "t"), names(coefs))[[1]]
    p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)", "Pr(>|t|)"), names(coefs))[[1]]
    partial_f <- wald$partial_f %||% if (!is.na(excluded_row)) coefs[[statistic_col]][[excluded_row]]^2 else NA_real_
    partial_p <- wald$partial_p %||% if (!is.na(excluded_row)) coefs[[p_col]][[excluded_row]] else NA_real_
    data.frame(
      model = model_name,
      term = coefs$term,
      estimate = coefs$Estimate,
      std.error = coefs$`Std. Error`,
      statistic = coefs[[statistic_col]],
      p.value = coefs[[p_col]],
      partial_f = partial_f,
      partial_p = partial_p,
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

first_stage_vcov <- function(fit, district_panel) {
  if (!"state_std" %in% names(district_panel) || !requireNamespace("sandwich", quietly = TRUE)) return(NULL)
  mf_rows <- suppressWarnings(as.integer(rownames(stats::model.frame(fit))))
  if (!length(mf_rows) || any(is.na(mf_rows))) return(NULL)
  cluster <- district_panel$state_std[mf_rows]
  if (length(unique(cluster)) < 2L) return(NULL)
  sandwich::vcovCL(fit, cluster = cluster)
}

first_stage_wald_test <- function(fit, excluded_term, vc) {
  if (is.na(excluded_term) || is.null(vc) || !requireNamespace("car", quietly = TRUE)) {
    return(list(partial_f = NA_real_, partial_p = NA_real_))
  }
  out <- tryCatch(
    car::linearHypothesis(fit, paste0(excluded_term, " = 0"), vcov. = vc, test = "F"),
    error = function(e) NULL
  )
  if (is.null(out)) return(list(partial_f = NA_real_, partial_p = NA_real_))
  list(partial_f = out[["F"]][[2]], partial_p = out[["Pr(>F)"]][[2]])
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
tidy_first_stage_results <- function(first_stage) {
  first_stage
}

#' compute partial f statistics
#'
compute_partial_f_statistics <- function(first_stage) {
  first_stage
}

#' compute partial r2
#'
compute_partial_r2 <- function(first_stage) {
  first_stage
}
