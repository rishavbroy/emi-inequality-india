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
      "district_iv_diagnostic", "consumption_iv", "district_mechanism",
      "c17_mechanism", "dise_first_stage", "dise_weak_iv",
      "census_migration_mechanism", "census_housing_mechanism",
      "historical_first_stage", "historical_predetermined_first_stage"
    )
  )

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
