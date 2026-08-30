# Specification-aware IV validity diagnostics.

candidate_iv_balance_specifications <- function(
    outcome = "real_log_consumption_change",
    treatment = preferred_iv_variables()$treatment) {
  registry <- iv_diagnostic_specification_registry(outcome = outcome, treatment = treatment)
  construction_ids <- unname(alternative_distance_design_constructions())
  keep <- registry$adjustment_id %in% iv_candidate_design_adjustments() &
    registry$construction_id %in% construction_ids
  out <- registry[keep, , drop = FALSE]
  expected <- as.vector(outer(
    iv_candidate_design_adjustments(), construction_ids, paste, sep = "__"
  ))
  if (!setequal(out$specification_id, expected) || anyDuplicated(out$specification_id)) {
    stop("Could not recover the candidate IV design registry for balance diagnostics.", call. = FALSE)
  }
  out
}

add_iv_balance_holm <- function(balance) {
  out <- safe_df(balance)
  if (!nrow(out)) return(out)
  out$p_holm_within_spec <- NA_real_
  groups <- split(seq_len(nrow(out)), out$specification_id)
  for (index in groups) {
    estimated <- index[
      out$status[index] == "estimated" & is.finite(num(out$p.value[index]))
    ]
    if (length(estimated)) {
      out$p_holm_within_spec[estimated] <- stats::p.adjust(
        num(out$p.value[estimated]), method = "holm"
      )
    }
  }
  out
}

prepare_census_2001_balance_panel <- function(panel, measures, variables, label) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  measures <- safe_df(measures)
  panel_keys <- c("state_code_2001", "district_code_2001")
  source_keys <- c("state_code", "district_code")
  missing_panel <- setdiff(panel_keys, names(x))
  missing_measures <- setdiff(c(source_keys, variables), names(measures))
  if (length(missing_panel) || length(missing_measures)) {
    stop(label, " balance panel is missing required district or measure columns.", call. = FALSE)
  }
  measures <- measures[c(source_keys, variables)]
  names(measures)[1:2] <- panel_keys
  measures$state_code_2001 <- normalize_census_code(measures$state_code_2001, 2L)
  measures$district_code_2001 <- normalize_census_code(measures$district_code_2001, 2L)
  if (anyDuplicated(measures[panel_keys])) {
    stop(label, " balance inputs are not unique by Census-2001 district.", call. = FALSE)
  }
  x$state_code_2001 <- normalize_census_code(x$state_code_2001, 2L)
  x$district_code_2001 <- normalize_census_code(x$district_code_2001, 2L)
  out <- merge(x, measures, by = panel_keys, all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(x) || any(!stats::complete.cases(out[variables]))) {
    stop(label, " measures do not cover the full IV panel.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

balance_nuisance_controls <- function(specification, tested_variable) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  exclusions <- c(
    tested_variable,
    census_2001_balance_linked_controls(tested_variable)
  )
  setdiff(controls, exclusions)
}

estimate_iv_balance_spec <- function(data, specification, tested_variable) {
  controls <- balance_nuisance_controls(specification, tested_variable)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  rhs <- unique(c(excluded, included, controls, iv_fixed_effect_terms(fixed_effect)))
  cluster_variable <- iv_specification_cluster_variable(specification)
  needed <- unique(c(
    all.vars(stats::reformulate(rhs, response = tested_variable)),
    cluster_variable
  ))
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
  cluster <- iv_specification_cluster(x, specification)
  joint <- clustered_joint_wald_test(fit, excluded, cluster)
  scalar <- length(excluded) == 1L
  coefficient <- if (scalar) clustered_first_stage_inference(fit, excluded[[1]], cluster) else NULL
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
  specifications = iv_diagnostic_specification_registry(),
  variables = census_2001_diagnostic_controls()
) {
  data <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  needed <- unique(c(
    variables,
    plain_chr(specifications$cluster), "region",
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


joint_balance_test_variables <- function(specification, variables = census_2001_diagnostic_controls()) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  candidates <- census_2001_joint_balance_controls(variables)
  setdiff(candidates, controls)
}

estimate_iv_joint_balance_spec <- function(
  data,
  specification,
  variables = census_2001_diagnostic_controls()
) {
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  tested <- joint_balance_test_variables(specification, variables)

  if (!length(tested)) {
    return(data.frame(
      specification_id = specification$specification_id,
      instrument = NA_character_,
      n_tested_covariates = 0L,
      status = "not_applicable",
      reason = "All diagnostic balance covariates are already included as nuisance controls.",
      stringsAsFactors = FALSE
    ))
  }

  linked <- unique(unlist(lapply(tested, census_2001_balance_linked_controls), use.names = FALSE))
  nuisance <- setdiff(controls, c(tested, linked))
  rhs <- unique(c(tested, included, nuisance, iv_fixed_effect_terms(fixed_effect)))
  needed <- unique(c(
    all.vars(stats::reformulate(rhs, response = excluded[[1]])),
    excluded,
    iv_specification_cluster_variable(specification)
  ))
  x <- data[stats::complete.cases(data[needed]), , drop = FALSE]
  if (!nrow(x)) {
    return(data.frame(
      specification_id = specification$specification_id,
      instrument = excluded,
      n_tested_covariates = length(tested),
      status = "not_estimated",
      reason = "No complete observations.",
      stringsAsFactors = FALSE
    ))
  }

  if (length(excluded) != 1L) {
    return(data.frame(
      specification_id = specification$specification_id,
      instrument = NA_character_,
      n_tested_covariates = length(tested),
      status = "not_applicable",
      reason = "Omnibus reverse-regression balance is defined only for scalar instruments.",
      stringsAsFactors = FALSE
    ))
  }

  instrument <- excluded[[1]]
  fit <- stats::lm(stats::reformulate(rhs, response = instrument), data = x)
  joint <- clustered_joint_wald_test(
    fit, tested, iv_specification_cluster(x, specification)
  )
  data.frame(
    specification_id = specification$specification_id,
    adjustment_id = specification$adjustment_id,
    construction_id = specification$construction_id,
    fixed_effect = fixed_effect,
    instrument = instrument,
    tested_covariates = paste(tested, collapse = ";"),
    n_tested_covariates = length(tested),
    joint_f = unname(joint[["statistic"]]),
    joint_p = unname(joint[["p.value"]]),
    n = stats::nobs(fit),
    status = "estimated",
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

run_iv_joint_balance_diagnostics <- function(
  panel,
  specifications = iv_diagnostic_specification_registry(),
  variables = census_2001_diagnostic_controls()
) {
  data <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  needed <- unique(c(
    variables,
    plain_chr(specifications$cluster), "region",
    unlist(specifications$controls, use.names = FALSE),
    unlist(specifications$included_language_controls, use.names = FALSE),
    unlist(specifications$excluded_instruments, use.names = FALSE)
  ))
  missing <- setdiff(needed, names(data))
  if (length(missing)) {
    stop("IV joint-balance diagnostics are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  applicability <- iv_diagnostic_applicability(specifications)
  ids <- applicability$specification_id[
    applicability$diagnostic_id == "balance_joint" & applicability$will_run
  ]
  specs <- specifications[specifications$specification_id %in% ids, , drop = FALSE]
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    estimate_iv_joint_balance_spec(data, specs[i, , drop = FALSE], variables)
  }))
}
