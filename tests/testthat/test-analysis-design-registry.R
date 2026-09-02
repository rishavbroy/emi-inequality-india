test_that("analysis-design ontology inventories registered families without Cartesian expansion", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  consumption <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  consumption_specs <- compile_consumption_iv_specifications(consumption, controls)
  mechanism_measures <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )

  registry <- compile_analysis_design_registry(
    consumption_specs, mechanism_measures, controls
  )

  expect_identical(names(registry), analysis_design_columns())
  expect_equal(anyDuplicated(registry$analysis_id), 0L)
  expect_true(all(registry$implemented))
  expect_true(all(registry$admissible))
  expect_true(all(nzchar(registry$reason)))
  expect_setequal(
    unique(registry$family),
    c(
      "public_iv", "district_iv_diagnostic", "consumption_iv", "district_mechanism",
      "c17_mechanism", "dise_first_stage", "dise_weak_iv",
      "census_migration_mechanism", "census_housing_mechanism",
      "economic_census_mechanism", "labor_mechanism",
      "historical_first_stage", "historical_predetermined_first_stage"
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
  expect_equal(sum(registry$family == "consumption_iv"), nrow(consumption_specs))
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

test_that("analysis-design ontology preserves estimator and sample distinctions", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  consumption <- compile_consumption_iv_specifications(
    read_consumption_iv_outcome_registry(
      file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
    ),
    controls
  )
  mechanisms <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )
  registry <- compile_analysis_design_registry(consumption, mechanisms, controls)

  consumption_rows <- registry[registry$family == "consumption_iv", , drop = FALSE]
  expect_setequal(unique(consumption_rows$estimand), c("ancova", "change"))
  expect_true(all(consumption_rows$estimator == "first_stage+reduced_form+2sls+anderson_rubin"))
  expect_true(all(consumption_rows$inference == "state_clustered+anderson_rubin"))

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


test_that("candidate-design ledger records bounded robustness choices without Cartesian search", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  consumption <- compile_consumption_iv_specifications(
    read_consumption_iv_outcome_registry(
      file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
    ),
    controls
  )
  public <- public_iv_specification_registry(controls)
  welfare <- read_consumption_welfare_outcomes(
    file.path(root, "data", "metadata", "consumption_welfare_outcomes.csv")
  )
  opportunity <- read_english_opportunity_measure_registry(
    file.path(root, "data", "metadata", "english_opportunity_measures.csv")
  )
  ledger <- build_iv_candidate_design_ledger(
    public, consumption, controls, welfare, opportunity
  )

  expect_identical(names(ledger), candidate_design_columns())
  expect_equal(anyDuplicated(ledger$candidate_id), 0L)
  expect_true(all(nzchar(ledger$rationale)))
  expect_true(all(nzchar(ledger$reference_section)))
  expect_true(all(nzchar(ledger$scientific_question)))

  bounded <- ledger[ledger$candidate_id == "consumption_candidate_scalar_iv_grid", , drop = FALSE]
  expect_true(bounded$admissible)
  expect_equal(bounded$implementation_status, "partial")
  expect_equal(bounded$candidate_cells, nrow(consumption) * 6L)
  expect_equal(bounded$implemented_cells, nrow(consumption))

  welfare_row <- ledger[
    ledger$candidate_id == "consumption_alternative_welfare_outcomes",
    ,
    drop = FALSE
  ]
  expect_equal(welfare_row$candidate_cells, 20L * 6L)
  expect_equal(welfare_row$multiplicity_family, "consumption_welfare_robustness")

  treatment <- ledger[ledger$candidate_id == "emi_intensive_margin_robustness", , drop = FALSE]
  expect_equal(treatment$candidate_cells, nrow(consumption) * 6L)

  blocks <- ledger[ledger$candidate_id == "relevance_control_block_interventions", , drop = FALSE]
  expect_equal(blocks$implementation_status, "implemented")
  expect_equal(blocks$candidate_cells, length(iv_block_intervention_adjustments(controls)))

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
