# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

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
        partial_f = NA_real_,
        partial_p = NA_real_,
        status = model$status %||% "out_of_active_pipeline",
        reason = model$reason %||% NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    iv_terms <- parse_iv_formula_terms(model)
    if (is.null(iv_terms) || !length(iv_terms$regressors) || !length(iv_terms$instruments)) {
      return(first_stage_status_row(model_name, "Could not parse first-stage IV formula."))
    }

    endogenous <- setdiff(iv_terms$regressors, iv_terms$instruments)
    if (!length(endogenous)) endogenous <- iv_terms$regressors[[1]]
    if (!length(endogenous) || is.na(endogenous[[1]]) || !nzchar(endogenous[[1]])) {
      return(first_stage_status_row(model_name, "No endogenous regressor could be identified for the first stage."))
    }

    first_stage_formula <- stats::as.formula(paste(
      endogenous[[1]],
      "~",
      paste(iv_terms$instruments, collapse = " + ")
    ))

    missing <- setdiff(all.vars(first_stage_formula), names(as.data.frame(district_panel)))
    if (length(missing)) {
      return(first_stage_status_row(model_name, paste("Missing first-stage variables:", paste(missing, collapse = ", "))))
    }

    fit <- tryCatch(stats::lm(first_stage_formula, data = district_panel), error = function(e) e)
    if (inherits(fit, "error")) return(first_stage_status_row(model_name, conditionMessage(fit)))

    vc <- first_stage_vcov(fit, district_panel)
    coefs <- tryCatch({
      if (is.null(vc)) as.data.frame(summary(fit)$coefficients) else as.data.frame(lmtest::coeftest(fit, vcov. = vc))
    }, error = function(e) data.frame())
    if (!nrow(coefs)) return(first_stage_status_row(model_name, "First-stage coefficient table is empty."))

    coefs$term <- rownames(coefs)
    rownames(coefs) <- NULL
    estimate_col <- first_existing_column(coefs, c("Estimate", "estimate"))
    se_col <- first_existing_column(coefs, c("Std. Error", "std.error"))
    statistic_col <- first_existing_column(coefs, c("t value", "z value", "t", "statistic"))
    p_col <- first_existing_column(coefs, c("Pr(>|t|)", "Pr(>|z|)", "p.value", "Pr(>F)"))

    excluded <- setdiff(iv_terms$instruments, iv_terms$regressors)
    excluded_term <- if (length(excluded)) excluded[[1]] else NA_character_
    excluded_row <- if (!is.na(excluded_term)) match(excluded_term, coefs$term) else NA_integer_
    wald <- first_stage_wald_test(fit, excluded_term, vc)

    statistic_values <- column_or_na(coefs, statistic_col)
    p_values <- column_or_na(coefs, p_col)
    partial_f <- wald$partial_f %||% if (!is.na(excluded_row) && excluded_row <= length(statistic_values)) statistic_values[[excluded_row]]^2 else NA_real_
    partial_p <- wald$partial_p %||% if (!is.na(excluded_row) && excluded_row <= length(p_values)) p_values[[excluded_row]] else NA_real_

    data.frame(
      model = model_name,
      term = coefs$term,
      estimate = column_or_na(coefs, estimate_col),
      std.error = column_or_na(coefs, se_col),
      statistic = statistic_values,
      p.value = p_values,
      partial_f = partial_f,
      partial_p = partial_p,
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  safe_bind_rows(rows)
}

first_stage_status_row <- function(model_name, reason) {
  data.frame(
    model = model_name,
    term = NA_character_,
    estimate = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    partial_f = NA_real_,
    partial_p = NA_real_,
    status = "out_of_active_pipeline",
    reason = reason,
    stringsAsFactors = FALSE
  )
}

first_existing_column <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit)) hit[[1]] else NA_character_
}

column_or_na <- function(df, col) {
  if (is.na(col) || !col %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

first_stage_vcov <- function(fit, district_panel) {
  if (!"state_std" %in% names(district_panel) || !requireNamespace("sandwich", quietly = TRUE)) return(NULL)
  mf_rows <- suppressWarnings(as.integer(rownames(stats::model.frame(fit))))
  if (!length(mf_rows) || any(is.na(mf_rows))) return(NULL)
  cluster <- district_panel$state_std[mf_rows]
  if (length(unique(stats::na.omit(cluster))) < 2L) return(NULL)
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
  if (is.null(out) || !all(c("F", "Pr(>F)") %in% names(out)) || nrow(out) < 2L) {
    return(list(partial_f = NA_real_, partial_p = NA_real_))
  }
  list(partial_f = suppressWarnings(as.numeric(out[["F"]][[2]])), partial_p = suppressWarnings(as.numeric(out[["Pr(>F)"]][[2]])))
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

tidy_first_stage_results <- function(first_stage) {
  first_stage
}

compute_partial_f_statistics <- function(first_stage) {
  first_stage
}

compute_partial_r2 <- function(first_stage) {
  first_stage
}
