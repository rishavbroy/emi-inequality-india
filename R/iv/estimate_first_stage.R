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
        effective_f = NA_real_,
        effective_f_critical_value = NA_real_,
        effective_f_p_value = NA_real_,
        effective_f_df = NA_real_,
        effective_f_status = "not_estimated",
        effective_f_reason = "IV model is unavailable.",
        model_f = NA_real_,
        model_p = NA_real_,
        nobs = NA_real_,
        r.squared = NA_real_,
        adj.r.squared = NA_real_,
        sigma = NA_real_,
        status = model$status %||% "out_of_active_pipeline",
        reason = model$reason %||% NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    first_stage <- iv_first_stage_fit(model, district_panel)
    if (is.null(first_stage)) {
      return(first_stage_status_row(
        model_name,
        "Could not recover the fitted IV first-stage specification and sample."
      ))
    }
    fit <- first_stage$model
    analysis_data <- first_stage$data
    iv_terms <- parse_iv_formula_terms(model)

    vc <- first_stage_vcov(fit, analysis_data)
    coef_mat <- tryCatch({
      if (is.null(vc)) summary(fit)$coefficients else clustered_coefficient_frame(fit, vc)
    }, error = function(e) NULL)
    if (is.null(coef_mat) || !NROW(coef_mat)) return(first_stage_status_row(model_name, "First-stage coefficient inference is unavailable."))

    coef_terms <- rownames(coef_mat)
    coefs <- as.data.frame(coef_mat, check.names = FALSE)
    if (is.null(coef_terms) || !length(coef_terms) || all(grepl("^[0-9]+$", coef_terms))) {
      coef_terms <- names(stats::coef(fit))
    }
    coefs$term <- coef_terms
    rownames(coefs) <- NULL
    estimate_col <- first_existing_column(coefs, c("Estimate", "estimate"))
    se_col <- first_existing_column(coefs, c("Std. Error", "std.error"))
    statistic_col <- first_existing_column(coefs, c("t value", "z value", "t", "statistic"))
    p_col <- first_existing_column(coefs, c("Pr(>|t|)", "Pr(>|z|)", "p.value", "Pr(>F)"))

    excluded <- setdiff(iv_terms$instruments, iv_terms$regressors)
    excluded_term <- if (length(excluded)) excluded[[1]] else NA_character_
    excluded_row <- if (!is.na(excluded_term)) match(excluded_term, coefs$term) else NA_integer_
    wald <- first_stage_wald_test(fit, excluded_term, vc)
    effective <- mop_effective_f(model, analysis_data)
    if (is_final_mode(cfg) && !identical(effective$status, "estimated")) {
      stop(
        paste0(
          "Montiel Olea-Pflueger effective F is unavailable for ",
          model_name, ": ", effective$reason
        ),
        call. = FALSE
      )
    }
    model_f <- model_f_statistic(fit)
    sm <- tryCatch(summary(fit), error = function(e) NULL)
    nobs_value <- tryCatch(stats::nobs(fit), error = function(e) NA_real_)
    r2_value <- tryCatch(sm$r.squared, error = function(e) NA_real_)
    adj_r2_value <- tryCatch(sm$adj.r.squared, error = function(e) NA_real_)
    sigma_value <- tryCatch(sm$sigma, error = function(e) NA_real_)

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
      effective_f = effective$statistic,
      effective_f_critical_value = effective$critical_value,
      effective_f_p_value = effective$p.value,
      effective_f_df = effective$effective_df,
      effective_f_status = effective$status,
      effective_f_reason = effective$reason,
      model_f = model_f$statistic,
      model_p = model_f$p.value,
      nobs = nobs_value,
      r.squared = r2_value,
      adj.r.squared = adj_r2_value,
      sigma = sigma_value,
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  safe_bind_rows(rows)
}

# Build the inferential first-stage regression from ivreg's own fitted
# instrument formula and estimation sample. This preserves factor expansion,
# interactions, transformations, contrasts, and row selection defined by ivreg
# instead of reconstructing the first stage from parsed term strings.
iv_first_stage_fit <- function(model, data) {
  if (!inherits(model, "ivreg")) return(NULL)

  endogenous <- plain_chr(model$endogenous)
  endogenous <- endogenous[!is.na(endogenous) & nzchar(endogenous)]
  if (length(endogenous) != 1L) return(NULL)

  panel <- as.data.frame(data)
  rows <- iv_model_row_indices(model, panel)
  if (!length(rows) || !endogenous[[1L]] %in% names(panel)) return(NULL)
  analysis_data <- panel[rows, , drop = FALSE]

  formula <- tryCatch(
    stats::formula(model, component = "instruments"),
    error = function(e) NULL
  )
  if (is.null(formula)) return(NULL)
  if (length(formula) == 2L) {
    formula <- stats::as.formula(
      call("~", as.name(endogenous[[1L]]), formula[[2L]]),
      env = environment(formula)
    )
  } else if (length(formula) >= 3L) {
    formula[[2L]] <- as.name(endogenous[[1L]])
  } else {
    return(NULL)
  }

  fit <- tryCatch(
    stats::lm(formula, data = analysis_data, na.action = stats::na.fail),
    error = function(e) NULL
  )
  if (is.null(fit) || stats::nobs(fit) != stats::nobs(model)) return(NULL)

  list(model = fit, data = analysis_data, endogenous = endogenous[[1L]])
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
    effective_f = NA_real_,
    effective_f_critical_value = NA_real_,
    effective_f_p_value = NA_real_,
    effective_f_df = NA_real_,
    effective_f_status = "not_estimated",
    effective_f_reason = reason,
    model_f = NA_real_,
    model_p = NA_real_,
    nobs = NA_real_,
    r.squared = NA_real_,
    adj.r.squared = NA_real_,
    sigma = NA_real_,
    status = "out_of_active_pipeline",
    reason = reason,
    stringsAsFactors = FALSE
  )
}


column_or_na <- function(df, col) {
  if (is.na(col) || !col %in% names(df)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

model_f_statistic <- function(fit) {
  fstat <- tryCatch(summary(fit)$fstatistic, error = function(e) NULL)
  if (is.null(fstat) || length(fstat) < 3L) {
    return(list(statistic = NA_real_, p.value = NA_real_))
  }
  f <- suppressWarnings(as.numeric(fstat[[1]]))
  df1 <- suppressWarnings(as.numeric(fstat[[2]]))
  df2 <- suppressWarnings(as.numeric(fstat[[3]]))
  p <- if (all(is.finite(c(f, df1, df2)))) stats::pf(f, df1, df2, lower.tail = FALSE) else NA_real_
  list(statistic = f, p.value = p)
}

first_stage_vcov <- function(fit, district_panel) {
  cluster <- iv_model_cluster(fit, district_panel)
  if (is.null(cluster)) return(NULL)
  iv_clustered_inference(fit, cluster)$vcov
}

first_stage_wald_test <- function(fit, excluded_term, vc) {
  if (is.na(excluded_term) || is.null(vc)) {
    return(list(partial_f = NA_real_, partial_p = NA_real_))
  }
  out <- wald_test_from_vcov(fit, excluded_term, vc)
  list(
    partial_f = suppressWarnings(as.numeric(out[["statistic"]])),
    partial_p = suppressWarnings(as.numeric(out[["p.value"]]))
  )
}

parse_iv_formula_terms <- function(model) {
  if (inherits(model, "ivreg")) {
    regressors <- tryCatch(
      attr(stats::terms(model, component = "regressors"), "term.labels"),
      error = function(e) NULL
    )
    instruments <- tryCatch(
      attr(stats::terms(model, component = "instruments"), "term.labels"),
      error = function(e) NULL
    )
    if (!is.null(regressors) && !is.null(instruments)) {
      return(list(regressors = regressors, instruments = instruments))
    }
  }

  # Experimental formula builders are inspected before fitting. Preserve full
  # term labels so transformations and factor expressions are not collapsed to
  # bare variable names. Fitted ivreg objects use the package's component terms
  # method above.
  f <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (is.null(f) || length(f) < 3L || !is.call(f[[3]]) || !identical(f[[3]][[1]], as.name("|"))) {
    return(NULL)
  }
  reg_formula <- stats::as.formula(call("~", f[[2]], f[[3]][[2]]), env = environment(f))
  inst_formula <- stats::as.formula(call("~", f[[2]], f[[3]][[3]]), env = environment(f))
  list(
    regressors = attr(stats::terms(reg_formula), "term.labels"),
    instruments = attr(stats::terms(inst_formula), "term.labels")
  )
}
