# Specification-aware IV validity diagnostics.

balance_nuisance_controls <- function(specification, tested_variable) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  setdiff(controls, tested_variable)
}

estimate_iv_balance_spec <- function(data, specification, tested_variable) {
  controls <- balance_nuisance_controls(specification, tested_variable)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  rhs <- unique(c(excluded, included, controls, iv_fixed_effect_terms(fixed_effect)))
  needed <- all.vars(stats::reformulate(rhs, response = tested_variable))
  needed <- unique(c(needed, "state_code_2001"))
  x <- data[stats::complete.cases(data[needed]), , drop = FALSE]
  if (!nrow(x)) {
    return(data.frame(
      specification_id = specification$specification_id,
      tested_variable = tested_variable,
      status = "not_estimated",
      reason = "No complete observations.",
      stringsAsFactors = FALSE
    ))
  }
  fit <- stats::lm(stats::reformulate(rhs, response = tested_variable), data = x)
  joint <- clustered_joint_wald_test(fit, excluded, x$state_code_2001)
  scalar <- length(excluded) == 1L
  coefficient <- if (scalar) clustered_first_stage_inference(fit, excluded[[1]], x$state_code_2001) else NULL
  estimate <- if (scalar) unname(stats::coef(fit)[excluded[[1]]]) else NA_real_
  standardized_effect <- NA_real_
  if (scalar) {
    z_resid <- residualize_iv_variable(x, excluded[[1]], unique(c(included, controls)), fixed_effect)
    y_resid <- residualize_iv_variable(x, tested_variable, unique(c(included, controls)), fixed_effect)
    y_sd <- stats::sd(y_resid)
    z_sd <- stats::sd(z_resid)
    if (is.finite(estimate) && is.finite(y_sd) && y_sd > 0 && is.finite(z_sd)) {
      standardized_effect <- estimate * z_sd / y_sd
    }
  }
  data.frame(
    specification_id = specification$specification_id,
    adjustment_id = specification$adjustment_id,
    construction_id = specification$construction_id,
    fixed_effect = fixed_effect,
    tested_variable = tested_variable,
    n_excluded_instruments = length(excluded),
    excluded_instruments = paste(excluded, collapse = ";"),
    estimate = estimate,
    std.error = if (scalar) unname(coefficient[["std.error"]]) else NA_real_,
    p.value = if (scalar) unname(coefficient[["p.value"]]) else NA_real_,
    standardized_effect = standardized_effect,
    joint_f = unname(joint[["statistic"]]),
    joint_p = unname(joint[["p.value"]]),
    n = stats::nobs(fit),
    status = "estimated",
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

run_iv_balance_diagnostics <- function(
  panel,
  specifications = iv_specification_registry(),
  variables = census_2001_diagnostic_controls()
) {
  data <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  needed <- unique(c(
    variables,
    "state_code_2001", "region",
    unlist(specifications$controls, use.names = FALSE),
    unlist(specifications$included_language_controls, use.names = FALSE),
    unlist(specifications$excluded_instruments, use.names = FALSE)
  ))
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    stop("IV balance diagnostics are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    spec <- specifications[i, , drop = FALSE]
    safe_bind_rows(lapply(variables, function(variable) {
      estimate_iv_balance_spec(data, spec, variable)
    }))
  }))
}
