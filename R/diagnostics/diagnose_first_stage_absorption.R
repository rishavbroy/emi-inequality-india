# Diagnose where the preferred linguistic-distance first stage loses relevance.

first_stage_control_blocks <- function(control_registry = NULL) iv_control_blocks(control_registry)

first_stage_control_block_membership <- function(control_registry = NULL) iv_control_block_membership(control_registry)

first_stage_included_control_blocks <- function(controls, control_registry = NULL) iv_included_control_blocks(controls, control_registry)

order_first_stage_controls <- function(controls, canonical = census_2001_diagnostic_controls()) {
  order_iv_controls(controls, canonical)
}

first_stage_without_human_capital <- function(controls, control_registry = NULL) iv_without_human_capital(controls, control_registry)

first_stage_absorption_registry <- function(control_registry = NULL) {
  registry <- iv_absorption_specification_registry(control_registry = control_registry)
  data.frame(
    specification_id = sub("^absorption__", "", registry$specification_id),
    label = registry$adjustment,
    fixed_effect = registry$fixed_effect,
    controls = I(registry$controls),
    sequence = seq_len(nrow(registry)),
    stringsAsFactors = FALSE
  )
}


first_stage_absorption_aliases <- function(control_registry = NULL) {
  aliases <- iv_absorption_specification_aliases(control_registry = control_registry)
  data.frame(
    semantic_specification_id = sub("^absorption__", "", aliases$semantic_specification_id),
    execution_specification_id = sub("^absorption__", "", aliases$execution_specification_id),
    is_execution_alias = aliases$is_execution_alias,
    stringsAsFactors = FALSE
  )
}

first_stage_absorption_variables <- function(
  treatment = "emi_exposure_all_children_0708",
  instrument = "ling_distance_nonzero_mean",
  control_registry = NULL
) {
  unique(c(
    treatment, instrument, "state_code_2001", "district_code_2001", "region",
    census_2001_diagnostic_controls(control_registry)
  ))
}

prepare_first_stage_absorption_panel <- function(
  panel,
  treatment = "emi_exposure_all_children_0708",
  instrument = "ling_distance_nonzero_mean",
  control_registry = NULL
) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  needed <- first_stage_absorption_variables(treatment, instrument, control_registry)
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("First-stage absorption panel is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)

  numeric_vars <- unique(c(
    treatment, instrument, census_2001_diagnostic_controls(control_registry)
  ))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$district_code_2001 <- plain_chr(x$district_code_2001)
  x$region <- as.character(x$region)
  keep <- stats::complete.cases(x[needed]) & nzchar(x$state_code_2001) &
    nzchar(x$district_code_2001) & nzchar(x$region)
  x <- x[keep, , drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) stop("No complete common support is available for first-stage absorption diagnostics.", call. = FALSE)
  if (length(unique(x$region)) != length(panel_region_levels())) {
    stop("First-stage absorption common support does not contain all six panel regions.", call. = FALSE)
  }
  x
}

first_stage_absorption_formula <- function(treatment, instrument, controls = character(), fixed_effect = "none") {
  stats::reformulate(
    c(instrument, iv_nuisance_terms(controls, fixed_effect)),
    response = treatment
  )
}

first_stage_nuisance_terms <- function(controls = character(), fixed_effect = "none") {
  iv_nuisance_terms(controls, fixed_effect)
}

residualize_first_stage_variable <- function(data, variable, controls = character(), fixed_effect = "none") {
  residualize_iv_variable(data, variable, controls, fixed_effect)
}

clustered_lm_term_inference <- function(fit, term, cluster, inference = NULL) {
  if (is.null(inference)) {
    inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  }
  if (is.null(inference) || is.null(inference$vcov)) {
    coefs <- summary(fit)$coefficients
    row <- match(term, rownames(coefs))
    if (is.na(row)) return(c(std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, partial_f = NA_real_))
    return(c(
      std.error = coefs[row, "Std. Error"], statistic = coefs[row, "t value"],
      p.value = coefs[row, "Pr(>|t|)"], partial_f = coefs[row, "t value"]^2
    ))
  }
  table <- tryCatch(lmtest::coeftest(fit, vcov. = inference$vcov), error = function(e) NULL)
  row <- if (!is.null(table)) match(term, rownames(table)) else NA_integer_
  if (is.na(row)) return(c(std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, partial_f = NA_real_))
  statistic <- suppressWarnings(as.numeric(table[row, 3]))
  c(
    std.error = suppressWarnings(as.numeric(table[row, 2])), statistic = statistic,
    p.value = suppressWarnings(as.numeric(table[row, 4])), partial_f = statistic^2
  )
}

clustered_first_stage_inference <- function(fit, instrument, cluster, inference = NULL) {
  clustered_lm_term_inference(fit, instrument, cluster, inference)
}

first_stage_positive_variation <- function(x) {
  sd_x <- stats::sd(num(x))
  is.finite(sd_x) && sd_x > 0
}

first_stage_residual_metrics_from_vectors <- function(instrument, treatment) {
  z_resid <- num(instrument)
  d_resid <- num(treatment)
  z_sd <- stats::sd(z_resid)
  d_sd <- stats::sd(d_resid)
  correlation <- if (
    first_stage_positive_variation(z_resid) &&
      first_stage_positive_variation(d_resid)
  ) {
    stats::cor(z_resid, d_resid)
  } else {
    NA_real_
  }
  list(
    instrument = z_resid,
    treatment = d_resid,
    instrument_sd = z_sd,
    treatment_sd = d_sd,
    correlation = correlation,
    partial_r_squared = if (is.finite(correlation)) correlation^2 else NA_real_
  )
}

first_stage_residual_metrics <- function(
    data, treatment, instrument, controls = character(), fixed_effect = "none") {
  residualized <- residualize_iv_variables(
    data, c(instrument, treatment), controls, fixed_effect
  )
  first_stage_residual_metrics_from_vectors(
    residualized[, instrument], residualized[, treatment]
  )
}

first_stage_variance_remaining <- function(residual, original) {
  original <- num(original)
  denominator <- stats::var(original)
  if (!is.finite(denominator) || denominator <= .Machine$double.eps) return(NA_real_)
  numerator <- stats::var(num(residual))
  if (!is.finite(numerator)) return(NA_real_)
  numerator / denominator
}

first_stage_estimability <- function(
    fit, instrument, inference, residuals, original_instrument, original_treatment,
    minimum_variance_share = sqrt(.Machine$double.eps)) {
  estimate <- unname(stats::coef(fit)[[instrument]])
  if (stats::df.residual(fit) <= 0L) {
    return(c(status = "not_estimable", reason = "no_residual_degrees_of_freedom"))
  }
  instrument_variance <- first_stage_variance_remaining(
    residuals$instrument, original_instrument
  )
  if (!is.finite(instrument_variance) || instrument_variance <= minimum_variance_share) {
    return(c(status = "not_estimable", reason = "no_residual_instrument_variation"))
  }
  treatment_variance <- first_stage_variance_remaining(
    residuals$treatment, original_treatment
  )
  if (!is.finite(treatment_variance) || treatment_variance <= minimum_variance_share) {
    return(c(status = "not_estimable", reason = "no_residual_treatment_variation"))
  }
  if (!is.finite(estimate)) {
    return(c(status = "not_estimable", reason = "non_finite_instrument_coefficient"))
  }
  inference_values <- num(inference[c("std.error", "statistic", "p.value", "partial_f")])
  if (length(inference_values) != 4L || any(!is.finite(inference_values))) {
    return(c(status = "not_estimable", reason = "non_finite_clustered_inference"))
  }
  c(status = "estimated", reason = NA_character_)
}

estimate_first_stage_coefficient <- function(data, specification, treatment, instrument) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  fit <- stats::lm(first_stage_absorption_formula(treatment, instrument, controls, fixed_effect), data = data)
  estimate <- unname(stats::coef(fit)[[instrument]])
  inference <- if (stats::df.residual(fit) > 0L && is.finite(estimate)) {
    clustered_first_stage_inference(fit, instrument, data$state_code_2001)
  } else {
    c(std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, partial_f = NA_real_)
  }
  list(fit = fit, inference = inference)
}


estimate_first_stage_absorption_spec <- function(
    data, specification, treatment, instrument, control_registry = NULL) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  coefficient <- estimate_first_stage_coefficient(data, specification, treatment, instrument)
  fit <- coefficient$fit
  inference <- coefficient$inference
  residuals <- first_stage_residual_metrics(data, treatment, instrument, controls, fixed_effect)

  estimability <- first_stage_estimability(
    fit, instrument, inference, residuals, data[[instrument]], data[[treatment]]
  )
  summary <- data.frame(
    specification_id = specification$specification_id,
    specification = specification$label,
    sequence = specification$sequence,
    treatment = treatment,
    instrument = instrument,
    fixed_effect = fixed_effect,
    control_blocks = paste(
      first_stage_included_control_blocks(controls, control_registry),
      collapse = ";"
    ),
    n_controls = length(controls),
    estimate = unname(stats::coef(fit)[instrument]),
    std.error = unname(inference[["std.error"]]),
    statistic = unname(inference[["statistic"]]),
    p.value = unname(inference[["p.value"]]),
    excluded_instrument_f = unname(inference[["partial_f"]]),
    partial_r_squared = residuals$partial_r_squared,
    residual_instrument_sd = residuals$instrument_sd,
    residual_treatment_sd = residuals$treatment_sd,
    residual_correlation = residuals$correlation,
    instrument_variance_remaining = first_stage_variance_remaining(
      residuals$instrument, data[[instrument]]
    ),
    n = stats::nobs(fit),
    n_states = length(unique(data$state_code_2001)),
    n_regions = length(unique(data$region)),
    status = estimability[["status"]],
    reason = estimability[["reason"]],
    stringsAsFactors = FALSE
  )
  list(
    summary = summary, fit = fit, inference = inference,
    instrument_residual = residuals$instrument, treatment_residual = residuals$treatment
  )
}

first_stage_state_residual_ranges <- function(data, estimates) {
  safe_bind_rows(lapply(estimates, function(estimate) {
    spec_id <- estimate$summary$specification_id[[1L]]
    tmp <- data.frame(
      state_code_2001 = data$state_code_2001,
      z = estimate$instrument_residual,
      d = estimate$treatment_residual,
      stringsAsFactors = FALSE
    )
    safe_bind_rows(lapply(split(tmp, tmp$state_code_2001), function(x) {
      data.frame(
        specification_id = spec_id,
        state_code_2001 = x$state_code_2001[[1]],
        n_districts = nrow(x),
        instrument_min = min(x$z), instrument_max = max(x$z),
        instrument_range = diff(range(x$z)), instrument_sd = stats::sd(x$z),
        treatment_min = min(x$d), treatment_max = max(x$d),
        treatment_range = diff(range(x$d)), treatment_sd = stats::sd(x$d),
        stringsAsFactors = FALSE
      )
    }))
  }))
}

first_stage_full_specification <- function(registry) {
  registry[registry$specification_id == "state_fe_expanded_controls", , drop = FALSE]
}

first_stage_state_deletion <- function(
    data, specification, treatment, instrument, full_estimate = NULL,
    control_registry = NULL) {
  if (is.null(full_estimate)) {
    full_estimate <- estimate_first_stage_absorption_spec(
      data, specification, treatment, instrument, control_registry
    )
  }
  full_coefficient <- unname(stats::coef(full_estimate$fit)[instrument])
  full_f <- unname(full_estimate$inference[["partial_f"]])
  safe_bind_rows(lapply(sort(unique(data$state_code_2001)), function(state) {
    reduced <- data[data$state_code_2001 != state, , drop = FALSE]
    coefficient <- estimate_first_stage_coefficient(reduced, specification, treatment, instrument)
    estimate <- unname(stats::coef(coefficient$fit)[instrument])
    partial_f <- unname(coefficient$inference[["partial_f"]])
    data.frame(
      specification_id = specification$specification_id,
      specification = specification$label,
      treatment = treatment,
      instrument = instrument,
      omitted_state = state,
      estimate = estimate,
      excluded_instrument_f = partial_f,
      estimate_change = estimate - full_coefficient,
      f_change = partial_f - full_f,
      stringsAsFactors = FALSE
    )
  }))
}

first_stage_district_influence <- function(data, fit, instrument) {
  dfb <- stats::dfbeta(fit)
  instrument_dfbeta <- if (instrument %in% colnames(dfb)) dfb[, instrument] else rep(NA_real_, nrow(data))
  data.frame(
    state_code_2001 = data$state_code_2001,
    district_code_2001 = data$district_code_2001,
    leverage = stats::hatvalues(fit),
    cooks_distance = stats::cooks.distance(fit),
    studentized_residual = stats::rstudent(fit),
    instrument_dfbeta = instrument_dfbeta,
    stringsAsFactors = FALSE
  )
}

first_stage_vif_diagnostics <- function(estimates) {
  ids <- c(
    "region_fe_census_controls", "region_fe_expanded_controls",
    "state_fe_census_controls", "state_fe_expanded_controls"
  )
  safe_bind_rows(lapply(ids, function(id) {
    estimate <- estimates[[match(id, vapply(
      estimates, function(x) x$summary$specification_id[[1L]], character(1)
    ))]]
    out <- compute_vif_if_applicable(estimate$fit)
    out$specification_id <- id
    out
  }))
}

#' Estimate a fixed-support first-stage absorption ladder
#'
#' The preferred public treatment and full-distribution scalar instrument are
#' defaults. These diagnostics decompose the same first stage used by the public IV outputs.
diagnose_first_stage_absorption <- function(
  panel,
  treatment = "emi_exposure_all_children_0708",
  instrument = "ling_distance_nonzero_mean",
  control_registry = NULL
) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  data <- prepare_first_stage_absorption_panel(
    panel, treatment, instrument, control_registry
  )
  registry <- first_stage_absorption_registry(control_registry)
  estimates <- lapply(seq_len(nrow(registry)), function(i) {
    estimate_first_stage_absorption_spec(
      data, registry[i, , drop = FALSE], treatment, instrument, control_registry
    )
  })
  summary <- safe_bind_rows(lapply(estimates, `[[`, "summary"))
  full_spec <- first_stage_full_specification(registry)
  full_id <- full_spec$specification_id[[1L]]
  full_estimate <- estimates[[match(full_id, vapply(
    estimates, function(x) x$summary$specification_id[[1L]], character(1)
  ))]]
  structure(
    list(
      summary = summary,
      registry = registry,
      aliases = first_stage_absorption_aliases(control_registry),
      common_support = data.frame(
        treatment = treatment, instrument = instrument, n = nrow(data),
        n_states = length(unique(data$state_code_2001)),
        n_regions = length(unique(data$region)), stringsAsFactors = FALSE
      ),
      state_residual_ranges = first_stage_state_residual_ranges(data, estimates),
      state_deletion = first_stage_state_deletion(
        data, full_spec, treatment, instrument, full_estimate = full_estimate,
        control_registry = control_registry
      ),
      district_influence = first_stage_district_influence(data, full_estimate$fit, instrument),
      vif = first_stage_vif_diagnostics(estimates)
    ),
    class = "emi_first_stage_absorption"
  )
}

save_first_stage_absorption_diagnostics <- function(
  diagnostics,
  dir = "outputs/diagnostics/extended/instrument_relevance"
) {
  if (!inherits(diagnostics, "emi_first_stage_absorption")) stop("Expected first-stage absorption diagnostics.", call. = FALSE)
  registry <- collapse_diagnostic_list_columns(diagnostics$registry, "controls")
  output_manifest(c(
    specification_ladder = write_diagnostic_csv(diagnostics$summary, file.path(dir, "first_stage_absorption_ladder.csv")),
    specification_registry = write_diagnostic_csv(registry, file.path(dir, "first_stage_absorption_registry.csv")),
    specification_aliases = write_diagnostic_csv(diagnostics$aliases, file.path(dir, "first_stage_absorption_aliases.csv")),
    common_support = write_diagnostic_csv(diagnostics$common_support, file.path(dir, "first_stage_absorption_common_support.csv")),
    state_residual_ranges = write_diagnostic_csv(diagnostics$state_residual_ranges, file.path(dir, "first_stage_state_residual_ranges.csv")),
    state_deletion = write_diagnostic_csv(diagnostics$state_deletion, file.path(dir, "first_stage_state_deletion.csv")),
    district_influence = write_diagnostic_csv(diagnostics$district_influence, file.path(dir, "first_stage_district_influence.csv")),
    vif = write_diagnostic_csv(diagnostics$vif, file.path(dir, "first_stage_vif.csv"))
  ))
}
