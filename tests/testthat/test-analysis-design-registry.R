test_that("analysis-design ontology inventories registered families without Cartesian expansion", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  consumption <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  consumption_specs <- compile_consumption_iv_specifications(consumption, controls)
  consumption_scalar_specs <- compile_consumption_scalar_iv_robustness_specifications(
    consumption, controls
  )
  consumption_treatment_specs <- compile_consumption_treatment_robustness_specifications(
    consumption, controls
  )
  welfare <- read_consumption_welfare_outcomes(
    file.path(root, "data", "metadata", "consumption_welfare_outcomes.csv")
  )
  consumption_welfare_registry <- build_consumption_alternative_welfare_registry(
    consumption, welfare
  )
  consumption_welfare_specs <- compile_consumption_alternative_welfare_specifications(
    consumption_welfare_registry, controls
  )
  consumption_control_specs <- compile_consumption_control_strategy_specifications(
    consumption, controls
  )
  consumption_parameterization_specs <- compile_consumption_control_parameterization_specifications(
    consumption, controls
  )
  consumption_historical_specs <- compile_consumption_historical_adjustment_specifications(
    consumption, controls
  )
  mechanism_measures <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )

  registry <- compile_analysis_design_registry(
    consumption_specs, mechanism_measures, controls,
    consumption_scalar_iv_robustness_specifications = consumption_scalar_specs,
    consumption_treatment_robustness_specifications = consumption_treatment_specs,
    consumption_alternative_welfare_specifications = consumption_welfare_specs,
    consumption_control_strategy_specifications = consumption_control_specs,
    consumption_control_parameterization_specifications = consumption_parameterization_specs,
    consumption_historical_adjustment_specifications = consumption_historical_specs,
    consumption_registry = consumption
  )

  expect_identical(names(registry), analysis_design_columns())
  expect_equal(anyDuplicated(registry$analysis_id), 0L)
  expect_true(all(registry$implemented))
  expect_true(all(registry$admissible))
  expect_true(all(nzchar(registry$reason)))
  expect_true(all(c("distance_measure_id", "language_adjustment_id") %in% names(registry)))
  expect_setequal(
    unique(registry$family),
    c(
      "public_iv", "district_iv_diagnostic", "hindi_belt_first_stage", "child_population_first_stage",
      "consumption_iv", "district_mechanism",
      "c17_mechanism", "dise_first_stage", "dise_weak_iv",
      "census_migration_mechanism", "census_housing_mechanism",
      "economic_census_mechanism", "labor_mechanism",
      "historical_first_stage", "historical_predetermined_first_stage",
      "schooling_consumption_bridge", "nss64_social_group",
      "st_concentration_heterogeneity", "census_1991_st_language"
    )
  )

  expect_equal(
    sum(registry$family == "public_iv"),
    nrow(public_iv_specification_registry(controls))
  )
  public_rows <- registry[registry$family == "public_iv", , drop = FALSE]
  expect_setequal(
    public_rows$specification_id,
    c("consumption", "consumption_ancova", "consumption_nominal", "consumption_legacy_controls")
  )
  expect_setequal(unique(public_rows$estimand), c("change", "ancova", "nominal_change"))

  expect_equal(
    sum(registry$family == "district_iv_diagnostic"),
    nrow(iv_diagnostic_specification_registry(control_registry = controls))
  )
  expect_equal(
    sum(registry$family == "hindi_belt_first_stage"),
    nrow(iv_hindi_belt_first_stage_specifications(control_registry = controls))
  )
  hindi <- registry[registry$family == "hindi_belt_first_stage", , drop = FALSE]
  expect_identical(hindi$fixed_effect, c("none", "region"))
  expect_true(all(hindi$analysis_role == "regional_institutional_robustness"))
  expect_false(any(hindi$fixed_effect == "state"))
  expect_equal(
    sum(registry$family == "consumption_iv"),
    nrow(consumption_specs) + nrow(consumption_scalar_specs) + nrow(consumption_treatment_specs) +
      nrow(consumption_welfare_specs) +
      nrow(consumption_control_specs) + nrow(consumption_parameterization_specs) +
      nrow(consumption_historical_specs)
  )
  expect_equal(
    sum(registry$family == "district_mechanism"),
    nrow(preferred_district_mechanism_registry(mechanism_measures)) * 3L
  )
  expect_equal(sum(registry$family == "c17_mechanism"), nrow(census_c17_mechanism_registry()))
  expect_equal(
    sum(registry$family == "economic_census_mechanism"),
    nrow(economic_census_mechanism_registry()) *
      nrow(economic_census_mechanism_specifications(control_registry = controls))
  )
  expect_equal(
    sum(registry$family == "labor_mechanism"),
    3L * nrow(labor_mechanism_registry("nss66")) *
      nrow(labor_mechanism_specifications("nss66", control_registry = controls))
  )

  expect_equal(
    sum(registry$family == "schooling_consumption_bridge"),
    nrow(schooling_consumption_bridge_specifications(consumption, controls))
  )
  expect_equal(
    sum(registry$family == "nss64_social_group"),
    nrow(nss64_schooling_social_group_specifications())
  )
  expect_equal(
    sum(registry$family == "st_concentration_heterogeneity"),
    nrow(english_opportunity_st_heterogeneity_specifications())
  )
  expect_equal(
    sum(registry$family == "census_1991_st_language"),
    nrow(census_1991_st_language_specifications())
  )

  constructs <- dise_construct_registry()
  n_iv_designs <- nrow(iv_diagnostic_specification_registry(control_registry = controls))
  expect_equal(
    sum(registry$family == "dise_first_stage"),
    nrow(constructs) * n_iv_designs
  )
  expect_equal(
    sum(registry$family == "dise_weak_iv"),
    sum(constructs$analysis_scope == "structural_iv") * n_iv_designs
  )
})


test_that("analysis-design ontology exposes linguistic measurement and adjustment as separate axes", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  opportunity <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )
  registry <- compile_analysis_design_registry(
    compile_consumption_iv_specifications(
      read_consumption_iv_outcome_registry(
        file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
      ),
      controls
    ),
    opportunity,
    controls
  )
  rows <- registry[registry$family == "district_iv_diagnostic", , drop = FALSE]

  base <- rows[rows$specification_id == "state_main__nonzero_mean", , drop = FALSE]
  adjusted <- rows[
    rows$specification_id == "state_main__nonzero_mean_shastry",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(base), 1L)
  expect_equal(nrow(adjusted), 1L)
  expect_identical(base$instrument, adjusted$instrument)
  expect_identical(base$distance_measure_id, "shastry_nonzero_mean")
  expect_identical(adjusted$distance_measure_id, "shastry_nonzero_mean")
  expect_identical(base$language_adjustment_id, "none")
  expect_identical(adjusted$language_adjustment_id, "shastry_composition")
})


test_that("analysis-design ontology separates conditioning philosophy from control parameterization", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  opportunity <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )
  consumption_registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  consumption <- compile_consumption_iv_specifications(consumption_registry, controls)
  registry <- compile_analysis_design_registry(consumption, opportunity, controls)

  diagnostic <- registry[
    registry$family == "district_iv_diagnostic" &
      registry$specification_id %in% c("state_main__nonzero_mean", "state_expanded__nonzero_mean"),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(diagnostic), 2L)

  main <- diagnostic[diagnostic$adjustment_set == "state_main", , drop = FALSE]
  expanded <- diagnostic[diagnostic$adjustment_set == "state_expanded", , drop = FALSE]
  expect_identical(main$control_strategy_id, "observed_exclusion_threat_adjustment")
  expect_identical(main$control_parameterization_id, "secondary_compact_economic")
  expect_identical(expanded$control_strategy_id, "expanded_absorption_diagnostic")
  expect_identical(expanded$control_parameterization_id, "expanded_registry")

  consumption_rows <- registry[registry$family == "consumption_iv", , drop = FALSE]
  expect_setequal(
    unique(consumption_rows$functional_form_id),
    unique(consumption$estimand)
  )
})


test_that("analysis-design ontology preserves estimator and sample distinctions", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  consumption_registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  consumption <- compile_consumption_iv_specifications(consumption_registry, controls)
  consumption_scalar <- compile_consumption_scalar_iv_robustness_specifications(
    consumption_registry, controls
  )
  consumption_treatment <- compile_consumption_treatment_robustness_specifications(
    consumption_registry, controls
  )
  welfare <- read_consumption_welfare_outcomes(
    file.path(root, "data", "metadata", "consumption_welfare_outcomes.csv")
  )
  consumption_welfare <- compile_consumption_alternative_welfare_specifications(
    build_consumption_alternative_welfare_registry(consumption_registry, welfare),
    controls
  )
  consumption_control <- compile_consumption_control_strategy_specifications(
    consumption_registry, controls
  )
  consumption_parameterization <- compile_consumption_control_parameterization_specifications(
    consumption_registry, controls
  )
  consumption_historical <- compile_consumption_historical_adjustment_specifications(
    consumption_registry, controls
  )
  consumption_historical_matched <- compile_consumption_historical_concept_matched_specifications(
    consumption_registry, controls
  )
  mechanisms <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )
  registry <- compile_analysis_design_registry(
    consumption, mechanisms, controls,
    consumption_scalar_iv_robustness_specifications = consumption_scalar,
    consumption_treatment_robustness_specifications = consumption_treatment,
    consumption_alternative_welfare_specifications = consumption_welfare,
    consumption_control_strategy_specifications = consumption_control,
    consumption_control_parameterization_specifications = consumption_parameterization,
    consumption_historical_adjustment_specifications = consumption_historical,
    consumption_historical_concept_matched_specifications = consumption_historical_matched,
    consumption_registry = consumption_registry
  )

  consumption_rows <- registry[registry$family == "consumption_iv", , drop = FALSE]
  expect_setequal(unique(consumption_rows$estimand), c("ancova", "change"))
  expect_true(all(consumption_rows$estimator == "first_stage+reduced_form+2sls+anderson_rubin"))
  preferred_consumption <- consumption_rows[
    !consumption_rows$analysis_role %in% c(
      "scalar_iv_robustness", "treatment_definition_robustness", "welfare_definition_robustness",
      "control_strategy_robustness",
      "control_parameterization_robustness", "historical_adjustment_robustness",
      "historical_concept_matched_robustness"
    ),
    , drop = FALSE
  ]
  scalar_consumption <- consumption_rows[
    consumption_rows$analysis_role == "scalar_iv_robustness", , drop = FALSE
  ]
  expect_true(all(preferred_consumption$inference == "state_clustered+anderson_rubin"))
  expect_equal(nrow(scalar_consumption), nrow(consumption_scalar))
  expect_true(all(scalar_consumption$inference == "state_clustered+anderson_rubin+holm"))
  expect_true(all(scalar_consumption$sample_rule == "consumption_scalar_iv_common_support"))
  treatment_consumption <- consumption_rows[
    consumption_rows$analysis_role == "treatment_definition_robustness", , drop = FALSE
  ]
  expect_equal(nrow(treatment_consumption), nrow(consumption_treatment))
  expect_true(all(treatment_consumption$treatment == intensive_margin_emi_treatment()))
  expect_true(all(treatment_consumption$sample_rule == "consumption_treatment_iv_common_support"))
  welfare_consumption <- consumption_rows[
    consumption_rows$analysis_role == "welfare_definition_robustness", , drop = FALSE
  ]
  expect_equal(nrow(welfare_consumption), nrow(consumption_welfare))
  control_consumption <- consumption_rows[
    consumption_rows$analysis_role == "control_strategy_robustness", , drop = FALSE
  ]
  expect_equal(nrow(control_consumption), nrow(consumption_control))
  expect_true(all(control_consumption$inference == "state_clustered+anderson_rubin+holm"))
  expect_true(all(control_consumption$sample_rule == "consumption_control_strategy_common_support"))
  parameterization_consumption <- consumption_rows[
    consumption_rows$analysis_role == "control_parameterization_robustness", , drop = FALSE
  ]
  expect_equal(nrow(parameterization_consumption), nrow(consumption_parameterization))
  expect_true(all(parameterization_consumption$inference == "state_clustered+anderson_rubin+holm"))
  expect_true(all(
    parameterization_consumption$sample_rule == "consumption_control_parameterization_common_support"
  ))
  historical_consumption <- consumption_rows[
    consumption_rows$analysis_role == "historical_adjustment_robustness", , drop = FALSE
  ]
  expect_equal(nrow(historical_consumption), nrow(consumption_historical))
  expect_true(all(historical_consumption$inference == "state_clustered+anderson_rubin+holm"))
  expect_true(all(
    historical_consumption$sample_rule == "consumption_historical_adjustment_common_support"
  ))
  matched_historical_consumption <- consumption_rows[
    consumption_rows$analysis_role == "historical_concept_matched_robustness", , drop = FALSE
  ]
  expect_equal(nrow(matched_historical_consumption), nrow(consumption_historical_matched))
  expect_true(all(
    matched_historical_consumption$sample_rule == "consumption_historical_concept_matched_common_support"
  ))
  expect_true(all(welfare_consumption$inference == "state_clustered+anderson_rubin+holm"))
  expect_true(all(welfare_consumption$sample_rule == "consumption_welfare_iv_common_support"))

  c17 <- registry[registry$family == "c17_mechanism", , drop = FALSE]
  expect_true(all(c17$treatment == ""))
  expect_true(all(c17$fixed_effect == "state"))
  expect_true(all(c17$estimator == "native_speaker_weighted_ols"))
  expect_true(all(c17$inference == "HC1"))

  district <- registry[registry$family == "district_mechanism", , drop = FALSE]
  expect_setequal(unique(district$adjustment_set), c("unadjusted", "region_main", "state_main"))
  expect_true(all(district$estimand == "reduced_form_association"))
  expect_false(any(grepl("2sls", district$estimator, fixed = TRUE)))

  labor <- registry[registry$family == "labor_mechanism", , drop = FALSE]
  expect_setequal(
    unique(labor$analysis_role),
    c("early_post_mechanism", "long_run_mechanism", "geography_robustness")
  )
  expect_true(all(labor$estimand == "post_treatment_labor_mechanism"))
  expect_true(all(labor$estimator == "reduced_form+2sls+anderson_rubin"))
  expect_true(all(labor$inference == "state_clustered+anderson_rubin"))
})

test_that("canonical IV rows do not duplicate scalar metadata columns", {
  row <- iv_specification_row(
    specification_id = "test",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Preferred distance",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = character(),
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "test"
  )

  expect_equal(sum(names(row) == "adjustment"), 1L)
  expect_equal(anyDuplicated(names(row)), 0L)
})


test_that("candidate-design admissibility and execution policy fail closed on contradictions", {
  make <- function(
      execution_policy = "diagnostic_only",
      admissible = TRUE,
      admissibility_reason = NULL) {
    candidate_design_row(
      candidate_id = "toy",
      reference_section = "test",
      analysis_family = "test",
      design_role = "diagnostic",
      scientific_question = "test question",
      design_axis = "test_axis",
      outcome_scope = "y",
      treatment_scope = "d",
      instrument_scope = "z",
      adjustment_scope = "none",
      estimator_scope = "ols",
      execution_policy = execution_policy,
      admissible = admissible,
      admissibility_reason = admissibility_reason,
      rationale = "test rationale"
    )
  }

  expect_error(
    candidate_design_frame(list(make(
      admissible = FALSE,
      admissibility_reason = "not_scientifically_admissible"
    ))),
    "admissibility must agree"
  )
  expect_error(
    candidate_design_frame(list(make(
      execution_policy = "do_not_estimate",
      admissible = TRUE
    ))),
    "admissibility must agree"
  )
})


test_that("candidate-design ledger records bounded robustness choices without Cartesian search", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  consumption_registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  consumption <- compile_consumption_iv_specifications(consumption_registry, controls)
  consumption_scalar <- compile_consumption_scalar_iv_robustness_specifications(
    consumption_registry, controls
  )
  consumption_treatment <- compile_consumption_treatment_robustness_specifications(
    consumption_registry, controls
  )
  public <- public_iv_specification_registry(controls)
  welfare <- read_consumption_welfare_outcomes(
    file.path(root, "data", "metadata", "consumption_welfare_outcomes.csv")
  )
  opportunity <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )
  consumption_welfare <- compile_consumption_alternative_welfare_specifications(
    build_consumption_alternative_welfare_registry(consumption_registry, welfare),
    controls
  )
  consumption_control <- compile_consumption_control_strategy_specifications(
    consumption_registry, controls
  )
  consumption_parameterization <- compile_consumption_control_parameterization_specifications(
    consumption_registry, controls
  )
  consumption_historical <- compile_consumption_historical_adjustment_specifications(
    consumption_registry, controls
  )
  consumption_historical_matched <- compile_consumption_historical_concept_matched_specifications(
    consumption_registry, controls
  )
  ledger <- build_iv_candidate_design_ledger(
    public, consumption, controls, welfare, opportunity, consumption_scalar,
    consumption_treatment, consumption_welfare, consumption_control, consumption_parameterization,
    consumption_historical, consumption_historical_matched
  )

  expect_identical(names(ledger), candidate_design_columns())
  expect_equal(anyDuplicated(ledger$candidate_id), 0L)
  expect_true(all(nzchar(ledger$rationale)))
  expect_true(all(nzchar(ledger$reference_section)))
  expect_true(all(nzchar(ledger$scientific_question)))
  expect_true(all(nzchar(ledger$admissibility_reason)))
  expect_true(all(ledger$execution_policy[!ledger$admissible] == "do_not_estimate"))
  expect_false(any(ledger$execution_policy[ledger$admissible] == "do_not_estimate"))
  finite_counts <- is.finite(ledger$candidate_cells) & is.finite(ledger$implemented_cells) &
    is.finite(ledger$execution_cells)
  expect_true(all(ledger$execution_cells[finite_counts] <= ledger$implemented_cells[finite_counts]))
  expect_true(all(ledger$implemented_cells[finite_counts] <= ledger$candidate_cells[finite_counts]))

  absorption <- ledger[
    ledger$candidate_id == "relevance_geography_control_absorption",
    , drop = FALSE
  ]
  expect_equal(absorption$candidate_cells, length(iv_absorption_adjustments(controls)))
  expect_equal(absorption$implemented_cells, absorption$candidate_cells)
  expect_equal(
    absorption$execution_cells,
    nrow(iv_absorption_specification_registry(control_registry = controls))
  )
  expect_lt(absorption$execution_cells, absorption$candidate_cells)

  rejected <- ledger[!ledger$admissible, , drop = FALSE]
  expect_setequal(
    rejected$admissibility_reason,
    c(
      "mechanical_cartesian_without_scientific_estimand",
      "cross_axis_interaction_lacks_independent_theory",
      "post_treatment_bad_control_changes_estimand"
    )
  )

  bounded <- ledger[ledger$candidate_id == "consumption_candidate_scalar_iv_grid", , drop = FALSE]
  expect_true(bounded$admissible)
  expect_equal(bounded$implementation_status, "implemented")
  expect_equal(bounded$execution_policy, "estimate")
  expect_equal(bounded$candidate_cells, nrow(consumption) * 6L)
  expect_equal(bounded$implemented_cells, nrow(consumption_scalar))
  expect_equal(bounded$execution_cells, nrow(consumption_scalar))

  welfare_row <- ledger[
    ledger$candidate_id == "consumption_alternative_welfare_outcomes",
    ,
    drop = FALSE
  ]
  expect_equal(welfare_row$candidate_cells, 20L * 6L)
  expect_equal(welfare_row$implemented_cells, nrow(consumption_welfare))
  expect_equal(welfare_row$execution_cells, nrow(consumption_welfare))
  expect_equal(welfare_row$implementation_status, "implemented")
  expect_equal(welfare_row$execution_policy, "estimate")
  expect_equal(welfare_row$multiplicity_family, "consumption_welfare_robustness")

  hindi_belt <- ledger[
    ledger$candidate_id == "shastry_hindi_belt_region_comparison", , drop = FALSE
  ]
  expect_equal(hindi_belt$candidate_cells, 2L)
  expect_equal(hindi_belt$implemented_cells, 2L)
  expect_equal(hindi_belt$execution_cells, 2L)
  expect_equal(hindi_belt$implementation_status, "implemented")
  expect_equal(hindi_belt$execution_policy, "estimate")

  child_population <- ledger[
    ledger$candidate_id == "shastry_child_population_5_19_comparison", , drop = FALSE
  ]
  expect_equal(child_population$candidate_cells, 2L)
  expect_equal(child_population$implemented_cells, 2L)
  expect_equal(child_population$execution_cells, 2L)
  expect_equal(child_population$implementation_status, "implemented")
  expect_equal(child_population$execution_policy, "estimate")

  future_goals <- ledger[ledger$candidate_id %in% c(
    "ihds_emi_capability_mobility_followup",
    "low_cost_private_school_followup",
    "mother_tongue_vs_emi_learning_followup"
  ), , drop = FALSE]
  expect_equal(nrow(future_goals), 3L)
  expect_true(all(future_goals$design_role == "future_goal"))
  expect_true(all(future_goals$implementation_status == "deferred"))
  expect_true(all(future_goals$execution_policy == "requires_data"))
  expect_true(all(future_goals$implemented_cells == 0L))

  emi_supply <- ledger[ledger$candidate_id == "genuine_emi_school_supply", , drop = FALSE]
  expect_equal(nrow(emi_supply), 1L)
  expect_equal(emi_supply$design_role, "future_goal")
  expect_equal(emi_supply$implementation_status, "data_unavailable")
  expect_equal(emi_supply$execution_policy, "requires_data")

  treatment <- ledger[ledger$candidate_id == "emi_intensive_margin_robustness", , drop = FALSE]
  expect_equal(treatment$candidate_cells, nrow(consumption) * 6L)
  expect_equal(treatment$implemented_cells, nrow(consumption_treatment))
  expect_equal(treatment$execution_cells, nrow(consumption_treatment))
  expect_equal(treatment$implementation_status, "implemented")
  expect_equal(treatment$execution_policy, "estimate")
  expect_equal(treatment$multiplicity_family, "consumption_treatment_robustness")

  blocks <- ledger[ledger$candidate_id == "relevance_control_block_interventions", , drop = FALSE]
  expect_equal(blocks$implementation_status, "implemented")
  expect_equal(blocks$candidate_cells, length(iv_block_intervention_adjustments(controls)))

  control_strategy <- ledger[
    ledger$candidate_id == "consumption_control_strategy_robustness",
    ,
    drop = FALSE
  ]
  expect_equal(
    control_strategy$candidate_cells,
    nrow(consumption) * length(iv_causal_control_strategy_adjustments(controls))
  )
  expect_equal(control_strategy$implementation_status, "implemented")
  expect_equal(control_strategy$execution_policy, "estimate")
  expect_equal(control_strategy$implemented_cells, nrow(consumption_control))
  expect_equal(control_strategy$execution_cells, nrow(consumption_control))
  expect_equal(control_strategy$multiplicity_family, "consumption_control_strategy")

  control_parameterization <- ledger[
    ledger$candidate_id == "consumption_control_parameterization_robustness",
    ,
    drop = FALSE
  ]
  expect_equal(
    control_parameterization$candidate_cells,
    nrow(consumption) * length(iv_causal_control_parameterization_adjustments(controls))
  )
  expect_equal(control_parameterization$implementation_status, "implemented")
  expect_equal(control_parameterization$execution_policy, "estimate")
  expect_equal(control_parameterization$implemented_cells, nrow(consumption_parameterization))
  expect_equal(control_parameterization$execution_cells, nrow(consumption_parameterization))
  expect_equal(
    control_parameterization$multiplicity_family,
    "consumption_control_parameterization"
  )

  historical_adjustment <- ledger[
    ledger$candidate_id == "historical_1991_adjustment_robustness",
    , drop = FALSE
  ]
  expect_equal(
    historical_adjustment$candidate_cells,
    nrow(consumption) * length(iv_historical_adjustment_comparison_adjustments(controls))
  )
  expect_equal(historical_adjustment$implementation_status, "implemented")
  expect_equal(historical_adjustment$execution_policy, "estimate")
  expect_equal(historical_adjustment$implemented_cells, nrow(consumption_historical))
  expect_equal(historical_adjustment$execution_cells, nrow(consumption_historical))
  expect_equal(
    historical_adjustment$multiplicity_family,
    "consumption_historical_adjustment"
  )
  historical_matched <- ledger[
    ledger$candidate_id == "historical_1991_concept_matched_robustness",
    , drop = FALSE
  ]
  expect_equal(
    historical_matched$candidate_cells,
    nrow(consumption) * length(iv_historical_concept_matched_adjustments(controls))
  )
  expect_equal(historical_matched$implementation_status, "implemented")
  expect_equal(historical_matched$implemented_cells, nrow(consumption_historical_matched))
  expect_equal(historical_matched$execution_cells, nrow(consumption_historical_matched))
  expect_equal(
    historical_matched$multiplicity_family,
    "consumption_historical_concept_matched"
  )

  parameterizations <- ledger[
    ledger$candidate_id == "relevance_control_parameterizations",
    ,
    drop = FALSE
  ]
  expect_equal(
    parameterizations$candidate_cells,
    length(iv_main_parameterization_adjustments(controls))
  )

  schooling <- ledger[
    ledger$candidate_id == "district_schooling_three_geography_grid",
    ,
    drop = FALSE
  ]
  expect_equal(
    schooling$candidate_cells,
    nrow(preferred_district_mechanism_registry(opportunity)) * 3L
  )

  cartesian <- ledger[ledger$candidate_id == "consumption_full_diagnostic_cartesian", , drop = FALSE]
  expect_false(cartesian$admissible)
  expect_equal(cartesian$execution_policy, "do_not_estimate")
  expect_equal(
    cartesian$candidate_cells,
    nrow(consumption) * nrow(iv_diagnostic_specification_registry(control_registry = controls))
  )

  cross_axes <- ledger[
    ledger$candidate_id == "cross_robustness_axes_without_interaction_rationale",
    ,
    drop = FALSE
  ]
  expect_false(cross_axes$admissible)

  post <- ledger[ledger$candidate_id == "posttreatment_mechanisms_as_controls", , drop = FALSE]
  expect_false(post$admissible)
  expect_equal(post$implementation_status, "not_applicable")
})
