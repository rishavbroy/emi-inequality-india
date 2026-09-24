candidate_design_columns <- function() {
  c(
    "candidate_id", "reference_section", "analysis_family", "design_role",
    "scientific_question", "design_axis", "outcome_scope", "treatment_scope",
    "instrument_scope", "adjustment_scope", "estimator_scope",
    "execution_policy", "multiplicity_family", "prerequisite",
    "admissible", "admissibility_reason", "implementation_status", "candidate_cells",
    "implemented_cells", "execution_cells", "rationale"
  )
}

candidate_design_row <- function(
    candidate_id,
    reference_section,
    analysis_family,
    design_role,
    scientific_question,
    design_axis,
    outcome_scope,
    treatment_scope,
    instrument_scope,
    adjustment_scope,
    estimator_scope,
    execution_policy,
    multiplicity_family = "not_applicable",
    prerequisite = "none",
    admissible = TRUE,
    admissibility_reason = NULL,
    implementation_status = "unimplemented",
    candidate_cells = NA_integer_,
    implemented_cells = 0L,
    execution_cells = implemented_cells,
    rationale) {
  if (is.null(admissibility_reason)) {
    admissibility_reason <- if (isTRUE(admissible)) {
      paste0("admissible_", design_role)
    } else {
      ""
    }
  }
  data.frame(
    candidate_id = candidate_id,
    reference_section = reference_section,
    analysis_family = analysis_family,
    design_role = design_role,
    scientific_question = scientific_question,
    design_axis = design_axis,
    outcome_scope = outcome_scope,
    treatment_scope = treatment_scope,
    instrument_scope = instrument_scope,
    adjustment_scope = adjustment_scope,
    estimator_scope = estimator_scope,
    execution_policy = execution_policy,
    multiplicity_family = multiplicity_family,
    prerequisite = prerequisite,
    admissible = admissible,
    admissibility_reason = admissibility_reason,
    implementation_status = implementation_status,
    candidate_cells = as.integer(candidate_cells),
    implemented_cells = as.integer(implemented_cells),
    execution_cells = as.integer(execution_cells),
    rationale = rationale,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

candidate_design_frame <- function(rows) {
  out <- safe_bind_rows(rows)
  missing <- setdiff(candidate_design_columns(), names(out))
  if (length(missing)) {
    stop(
      "Candidate-design rows are missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  out <- out[candidate_design_columns()]
  allowed_status <- c(
    "implemented", "partial", "unimplemented", "data_unavailable",
    "deferred", "not_applicable"
  )
  allowed_policy <- c(
    "estimate", "estimate_if_registered", "diagnostic_only", "report_only",
    "requires_data", "do_not_estimate"
  )
  if (!nrow(out) || anyDuplicated(out$candidate_id) || any(!nzchar(out$candidate_id)) ||
      any(!out$implementation_status %in% allowed_status) ||
      any(!out$execution_policy %in% allowed_policy) ||
      any(!nzchar(out$reference_section)) ||
      any(!nzchar(out$scientific_question)) ||
      any(!nzchar(out$admissibility_reason)) ||
      any(!nzchar(out$rationale))) {
    stop("Candidate-design ledger is malformed.", call. = FALSE)
  }
  policy_conflict <- (!out$admissible & out$execution_policy != "do_not_estimate") |
    (out$admissible & out$execution_policy == "do_not_estimate")
  if (any(policy_conflict)) {
    stop(
      "Candidate-design admissibility must agree with execution_policy.",
      call. = FALSE
    )
  }
  impossible <- out$implemented_cells > out$candidate_cells &
    is.finite(out$implemented_cells) & is.finite(out$candidate_cells)
  invalid_execution <- out$execution_cells > out$implemented_cells &
    is.finite(out$execution_cells) & is.finite(out$implemented_cells)
  if (any(impossible) || any(invalid_execution)) {
    stop(
      "Candidate-design cell counts must satisfy execution <= implemented <= candidate.",
      call. = FALSE
    )
  }
  out
}

count_alternative_consumption_welfare_designs <- function(
    consumption_specifications,
    welfare_registry) {
  nrow(build_consumption_alternative_welfare_registry(
    consumption_specifications,
    welfare_registry
  ))
}

candidate_design_metadata_path <- function(
    root = Sys.getenv("EMI_PROJECT_ROOT", ".")) {
  file.path(root, "data", "metadata", "iv_candidate_designs.csv")
}

read_iv_candidate_design_declarations <- function(
    path = candidate_design_metadata_path()) {
  if (!file.exists(path)) {
    stop("Candidate-design metadata not found: ", path, call. = FALSE)
  }

  out <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  expected <- candidate_design_columns()
  missing <- setdiff(expected, names(out))
  extra <- setdiff(names(out), expected)
  if (length(missing) || length(extra)) {
    stop(
      "Candidate-design metadata columns do not match the declared schema.",
      call. = FALSE
    )
  }

  out <- out[expected]
  out$admissible <- as.logical(out$admissible)
  for (nm in c("candidate_cells", "implemented_cells", "execution_cells")) {
    out[[nm]] <- as.integer(out[[nm]])
  }
  candidate_design_frame(list(out))
}

candidate_design_runtime_row <- function(
    candidate_id,
    candidate_cells,
    implemented_cells,
    execution_cells = implemented_cells,
    implementation_status = NA_character_) {
  data.frame(
    candidate_id = candidate_id,
    candidate_cells = as.integer(candidate_cells),
    implemented_cells = as.integer(implemented_cells),
    execution_cells = as.integer(execution_cells),
    implementation_status = implementation_status,
    stringsAsFactors = FALSE
  )
}

apply_candidate_design_runtime <- function(declarations, runtime) {
  if (!nrow(runtime)) {
    return(candidate_design_frame(list(declarations)))
  }
  if (anyDuplicated(runtime$candidate_id)) {
    stop("Candidate-design runtime updates contain duplicate candidate_id values.", call. = FALSE)
  }

  idx <- match(runtime$candidate_id, declarations$candidate_id)
  if (anyNA(idx)) {
    stop(
      "Candidate-design runtime updates reference undeclared candidate_id values: ",
      paste(runtime$candidate_id[is.na(idx)], collapse = ", "),
      call. = FALSE
    )
  }

  for (nm in c("candidate_cells", "implemented_cells", "execution_cells")) {
    declarations[[nm]][idx] <- runtime[[nm]]
  }
  has_status <- !is.na(runtime$implementation_status)
  declarations$implementation_status[idx[has_status]] <-
    runtime$implementation_status[has_status]

  candidate_design_frame(list(declarations))
}

build_iv_candidate_design_ledger <- function(
    public_specifications,
    consumption_specifications,
    control_registry = NULL,
    welfare_registry = NULL,
    english_opportunity_registry = NULL,
    consumption_scalar_iv_robustness_specifications = NULL,
    consumption_treatment_robustness_specifications = NULL,
    consumption_alternative_welfare_specifications = NULL,
    consumption_control_strategy_specifications = NULL,
    consumption_control_parameterization_specifications = NULL,
    consumption_historical_adjustment_specifications = NULL,
    consumption_historical_concept_matched_specifications = NULL,
    consumption_exclusion_sensitivity_specifications = NULL,
    falsification_adaptive_specifications = NULL,
    metadata_path = candidate_design_metadata_path()) {
  public_specs <- as_iv_specifications(public_specifications)
  consumption_specs <- as_iv_specifications(consumption_specifications)
  control_registry <- resolve_census_2001_control_registry(control_registry)
  diagnostic_specs <- iv_diagnostic_specification_registry(
    control_registry = control_registry
  )
  canonical_specs <- iv_specification_registry(control_registry = control_registry)

  n_candidate_iv <- length(iv_candidate_design_adjustments()) *
    length(iv_candidate_design_constructions())
  n_consumption <- nrow(consumption_specs)
  n_or_zero <- function(x) {
    if (is.null(x)) 0L else nrow(as_iv_specifications(x))
  }
  n_consumption_scalar <- n_or_zero(consumption_scalar_iv_robustness_specifications)
  n_consumption_treatment <- n_or_zero(consumption_treatment_robustness_specifications)
  n_consumption_welfare <- n_or_zero(consumption_alternative_welfare_specifications)
  n_consumption_control_strategy <- n_or_zero(consumption_control_strategy_specifications)
  n_consumption_control_parameterization <-
    n_or_zero(consumption_control_parameterization_specifications)
  n_consumption_historical_adjustment <-
    n_or_zero(consumption_historical_adjustment_specifications)
  n_consumption_historical_concept_matched <-
    n_or_zero(consumption_historical_concept_matched_specifications)
  n_consumption_exclusion_sensitivity <-
    n_or_zero(consumption_exclusion_sensitivity_specifications)
  n_falsification_adaptive <- if (is.null(falsification_adaptive_specifications)) {
    0L
  } else {
    nrow(iv_falsification_adaptive_specifications(falsification_adaptive_specifications))
  }

  n_diagnostic_iv <- nrow(diagnostic_specs)
  n_scalar <- sum(canonical_specs$n_excluded_instruments == 1L)
  n_multishare <- sum(canonical_specs$n_excluded_instruments > 1L)
  n_absorption_candidates <- length(iv_absorption_adjustments(control_registry))
  n_absorption_executions <- nrow(
    iv_absorption_specification_registry(control_registry = control_registry)
  )
  n_hindi_belt <- nrow(
    iv_hindi_belt_first_stage_specifications(control_registry = control_registry)
  )
  n_child_population <- nrow(
    iv_child_population_first_stage_specifications(control_registry = control_registry)
  )
  n_block_interventions <- length(iv_block_intervention_adjustments(control_registry))
  n_control_strategies <- length(iv_causal_control_strategy_adjustments(control_registry))
  n_relevance_parameterizations <- length(iv_main_parameterization_adjustments(control_registry))
  n_control_parameterizations <- length(
    iv_causal_control_parameterization_adjustments(control_registry)
  )
  n_dise_relevance <- nrow(dise_construct_registry()) * n_diagnostic_iv
  n_historical_vintage <- nrow(
    historical_linguistic_predetermined_first_stage_registry()
  ) * 4L
  n_c17 <- nrow(census_c17_mechanism_registry())
  n_district_mechanism <- if (is.null(english_opportunity_registry)) {
    NA_integer_
  } else {
    nrow(preferred_district_mechanism_registry(english_opportunity_registry)) *
      nrow(district_mechanism_adjustment_registry(control_registry))
  }
  n_alt_welfare <- if (is.null(welfare_registry)) {
    NA_integer_
  } else {
    count_alternative_consumption_welfare_designs(consumption_specs, welfare_registry)
  }

  runtime <- safe_bind_rows(list(
    candidate_design_runtime_row(
      "relevance_geography_control_absorption",
      n_absorption_candidates,
      n_absorption_candidates,
      n_absorption_executions
    ),
    candidate_design_runtime_row(
      "relevance_control_block_interventions",
      n_block_interventions,
      n_block_interventions
    ),
    candidate_design_runtime_row(
      "relevance_control_parameterizations",
      n_relevance_parameterizations,
      n_relevance_parameterizations
    ),
    candidate_design_runtime_row(
      "relevance_scalar_linguistic_constructions",
      n_scalar,
      n_scalar
    ),
    candidate_design_runtime_row(
      "relevance_multishare_instruments",
      n_multishare,
      n_multishare
    ),
    candidate_design_runtime_row(
      "five_share_falsification_adaptive_set",
      6L,
      n_falsification_adaptive,
      implementation_status = if (n_falsification_adaptive > 0L) {
        "implemented"
      } else {
        "unimplemented"
      }
    ),
    candidate_design_runtime_row(
      "relevance_dise_treatment_definitions",
      n_dise_relevance,
      n_dise_relevance
    ),
    candidate_design_runtime_row(
      "relevance_historical_distance_vintage",
      n_historical_vintage,
      n_historical_vintage
    ),
    candidate_design_runtime_row("c17_behavioral_mechanism", n_c17, n_c17),
    candidate_design_runtime_row(
      "district_schooling_three_geography_grid",
      n_district_mechanism,
      if (is.finite(n_district_mechanism)) n_district_mechanism else 0L,
      implementation_status = if (is.finite(n_district_mechanism)) {
        "implemented"
      } else {
        "unimplemented"
      }
    ),
    candidate_design_runtime_row(
      "public_headline_registered",
      nrow(public_specs),
      nrow(public_specs)
    ),
    candidate_design_runtime_row(
      "consumption_registered_dynamics",
      n_consumption,
      n_consumption
    ),
    candidate_design_runtime_row(
      "consumption_exclusion_sensitivity",
      4L,
      n_consumption_exclusion_sensitivity,
      implementation_status = if (n_consumption_exclusion_sensitivity == 4L) {
        "implemented"
      } else {
        "partial"
      }
    ),
    candidate_design_runtime_row(
      "consumption_candidate_scalar_iv_grid",
      n_consumption * n_candidate_iv,
      n_consumption_scalar,
      implementation_status = if (n_consumption_scalar == n_consumption * n_candidate_iv) {
        "implemented"
      } else {
        "partial"
      }
    ),
    candidate_design_runtime_row(
      "consumption_alternative_welfare_outcomes",
      if (is.finite(n_alt_welfare)) n_alt_welfare * n_candidate_iv else NA_integer_,
      n_consumption_welfare,
      implementation_status = if (
        is.finite(n_alt_welfare) &&
          n_consumption_welfare == n_alt_welfare * n_candidate_iv
      ) {
        "implemented"
      } else {
        "partial"
      }
    ),
    candidate_design_runtime_row(
      "emi_intensive_margin_robustness",
      n_consumption * n_candidate_iv,
      n_consumption_treatment,
      implementation_status = if (n_consumption_treatment == n_consumption * n_candidate_iv) {
        "implemented"
      } else if (n_consumption_treatment > 0L) {
        "partial"
      } else {
        "unimplemented"
      }
    ),
    candidate_design_runtime_row(
      "consumption_control_strategy_robustness",
      n_consumption * n_control_strategies,
      n_consumption_control_strategy
    ),
    candidate_design_runtime_row(
      "consumption_control_parameterization_robustness",
      n_consumption * n_control_parameterizations,
      n_consumption_control_parameterization
    ),
    candidate_design_runtime_row(
      "historical_1991_adjustment_robustness",
      n_consumption *
        length(iv_historical_adjustment_comparison_adjustments(control_registry)),
      n_consumption_historical_adjustment
    ),
    candidate_design_runtime_row(
      "historical_1991_concept_matched_robustness",
      n_consumption *
        length(iv_historical_concept_matched_adjustments(control_registry)),
      n_consumption_historical_concept_matched
    ),
    candidate_design_runtime_row(
      "shastry_geographic_access_controls",
      n_consumption * 2L,
      0L
    ),
    candidate_design_runtime_row(
      "shastry_hindi_belt_region_comparison",
      2L,
      n_hindi_belt,
      implementation_status = if (n_hindi_belt == 2L) "implemented" else "partial"
    ),
    candidate_design_runtime_row(
      "shastry_child_population_5_19_comparison",
      2L,
      n_child_population,
      implementation_status = if (n_child_population == 2L) "implemented" else "partial"
    ),
    candidate_design_runtime_row(
      "consumption_full_diagnostic_cartesian",
      n_consumption * n_diagnostic_iv,
      n_consumption
    )
  ))

  declarations <- read_iv_candidate_design_declarations(metadata_path)
  apply_candidate_design_runtime(declarations, runtime)
}
