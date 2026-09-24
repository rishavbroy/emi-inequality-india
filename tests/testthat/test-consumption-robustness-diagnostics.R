test_that("consumption scalar-IV robustness compiles exactly the registered six-design family", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data/metadata/consumption_iv_outcomes.csv")
  )
  specs <- compile_consumption_scalar_iv_robustness_specifications(registry)
  expect_identical(specs$analysis_id, paste("consumption_iv", specs$specification_id, sep = "__"))

  expect_equal(nrow(specs), nrow(registry) * 6L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_setequal(unique(specs$adjustment_id), c("region_main", "state_main"))
  expect_setequal(
    unique(specs$construction_id),
    c("nonzero_mean", "glottolog_mean", "dyen_noncognate")
  )
  expect_true(all(specs$sample_rule == "consumption_scalar_iv_common_support"))
  expect_true(all(specs$tier == "B"))
  expect_true(all(specs$n_excluded_instruments == 1L))
})

test_that("consumption scalar-IV robustness uses one common sample within each welfare design", {
  specs <- bind_iv_specification_rows(lapply(c("z1", "z2"), function(z) {
    row <- iv_specification_row(
      specification_id = paste0("s_", z), adjustment_id = "state_main",
      adjustment = "State", construction_id = z, construction = z,
      outcome = "y", treatment = "d", fixed_effect = "state",
      controls = "x", included_language_controls = character(),
      excluded_instruments = z, mapping_coverage_variable = NA_character_,
      panel_variant = "primary", sample_rule = "consumption_scalar_iv_common_support",
      cluster = "state_code_2001"
    )
    row$welfare_specification_id <- "welfare_a"
    row
  }))
  panel <- data.frame(
    y = 1:6, d = 2:7, x = 3:8,
    z1 = c(1, 2, NA, 4, 5, 6), z2 = c(1, NA, 3, 4, 5, 6),
    state_code_2001 = c("01", "01", "02", "02", "03", "03"),
    stringsAsFactors = FALSE
  )

  support <- consumption_iv_common_sample_support(panel, specs, "welfare_specification_id")
  restricted <- restrict_consumption_iv_to_common_samples(
    panel, specs, "welfare_specification_id"
  )

  expect_equal(support$n_common, 4L)
  expect_equal(sum(is.finite(restricted$y)), 4L)
  expect_true(all(is.na(restricted$y[c(2, 3)])))
})

test_that("consumption scalar-IV multiplicity is frozen within welfare design and full family", {
  dynamics <- list(summary = data.frame(
    welfare_specification_id = rep(c("a", "b"), each = 3),
    reduced_form_p.value = c(.01, .02, .9, .03, .04, .8),
    anderson_rubin_p_beta0 = c(.02, .03, .8, .01, .05, .7),
    stringsAsFactors = FALSE
  ))
  out <- add_consumption_scalar_iv_multiplicity(dynamics)$summary

  expect_equal(
    out$reduced_form_p_holm_within_welfare[1:3],
    stats::p.adjust(c(.01, .02, .9), method = "holm")
  )
  expect_equal(
    out$anderson_rubin_p_beta0_holm_family,
    stats::p.adjust(dynamics$summary$anderson_rubin_p_beta0, method = "holm")
  )
  expect_true(all(out$multiplicity_family == "consumption_scalar_iv_robustness"))
})

test_that("consumption scalar-IV saver persists summaries but not pointwise AR grids", {
  root <- tempfile("consumption-scalar-iv-")
  dynamics <- list(
    summary = data.frame(specification_id = "s", estimate = 1),
    anderson_rubin_grid = data.frame(specification_id = "s", beta = 0, p.value = .5)
  )
  support <- data.frame(group_id = "w", n_common = 10, status = "ready")

  paths <- save_consumption_scalar_iv_robustness(dynamics, support, root)

  expect_setequal(
    basename(paths),
    c(
      "consumption_scalar_iv_robustness.csv",
      "consumption_scalar_iv_robustness_common_support.csv"
    )
  )
  expect_false(file.exists(file.path(root, "consumption_scalar_iv_robustness_anderson_rubin_grid.csv")))
})

test_that("consumption scalar-IV validation enforces six-design realized common samples", {
  summary <- data.frame(
    welfare_specification_id = rep(c("a", "b"), each = 6),
    n = rep(c(40, 35), each = 6),
    first_stage_n = rep(c(40, 35), each = 6),
    reduced_form_n = rep(c(40, 35), each = 6),
    second_stage_n = rep(c(40, 35), each = 6),
    stringsAsFactors = FALSE
  )
  support <- data.frame(
    group_id = c("a", "b"), n_common = c(40, 35), status = "ready",
    stringsAsFactors = FALSE
  )
  dynamics <- list(summary = summary)

  expect_identical(validate_consumption_scalar_iv_robustness(dynamics, support), dynamics)
  bad <- dynamics
  bad$summary$n[[3L]] <- 39
  expect_error(
    validate_consumption_scalar_iv_robustness(bad, support),
    "registered common support"
  )
})

test_that("alternative welfare registry is survey-compatible and scale-explicit", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  consumption <- read_consumption_iv_outcome_registry(
    file.path(root, "data/metadata/consumption_iv_outcomes.csv")
  )
  welfare <- read_consumption_welfare_outcomes(
    file.path(root, "data/metadata/consumption_welfare_outcomes.csv")
  )
  registry <- build_consumption_alternative_welfare_registry(consumption, welfare)

  expect_equal(nrow(registry), 20L)
  expect_equal(anyDuplicated(registry$welfare_specification_id), 0L)
  expect_setequal(
    unique(registry$outcome_id),
    c("mean_log_real_mpce", "weighted_median_real_mpce", "bottom40_mean_real_mpce")
  )
  expect_equal(sum(registry$outcome_id == "mean_log_real_mpce"), 8L)
  expect_equal(sum(registry$outcome_id == "weighted_median_real_mpce"), 8L)
  expect_equal(sum(registry$outcome_id == "bottom40_mean_real_mpce"), 4L)
  expect_true(all(registry$analysis_transform[registry$outcome_id == "mean_log_real_mpce"] == "identity"))
  expect_true(all(registry$analysis_transform[registry$outcome_id != "mean_log_real_mpce"] == "log"))
  expect_true(all(registry$tier == "C"))
  expect_false(any(registry$outcome_round == "nss_2009_10_type2" & registry$outcome_id == "bottom40_mean_real_mpce"))
  expect_false(any(registry$outcome_round == "nss_2011_12_type2" & registry$outcome_id == "bottom40_mean_real_mpce"))
})

test_that("alternative welfare robustness compiles the predeclared 120-cell family", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  consumption <- read_consumption_iv_outcome_registry(
    file.path(root, "data/metadata/consumption_iv_outcomes.csv")
  )
  welfare <- read_consumption_welfare_outcomes(
    file.path(root, "data/metadata/consumption_welfare_outcomes.csv")
  )
  registry <- build_consumption_alternative_welfare_registry(consumption, welfare)
  specs <- compile_consumption_alternative_welfare_specifications(registry)

  expect_equal(nrow(specs), 120L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_setequal(unique(specs$adjustment_id), c("region_main", "state_main"))
  expect_setequal(
    unique(specs$construction_id),
    c("nonzero_mean", "glottolog_mean", "dyen_noncognate")
  )
  expect_true(all(specs$sample_rule == "consumption_welfare_iv_common_support"))
  expect_true(all(specs$tier == "C"))
})

test_that("consumption robustness family helpers are reusable across multiplicity families", {
  dynamics <- list(summary = data.frame(
    welfare_specification_id = rep(c("a", "b"), each = 2),
    reduced_form_p.value = c(.01, .2, .03, .8),
    anderson_rubin_p_beta0 = c(.02, .3, .04, .9),
    stringsAsFactors = FALSE
  ))
  out <- add_consumption_iv_family_multiplicity(
    dynamics, "consumption_welfare_robustness"
  )$summary

  expect_equal(
    out$reduced_form_p_holm_within_welfare[1:2],
    stats::p.adjust(c(.01, .2), method = "holm")
  )
  expect_equal(
    out$anderson_rubin_p_beta0_holm_family,
    stats::p.adjust(dynamics$summary$anderson_rubin_p_beta0, method = "holm")
  )
  expect_true(all(out$multiplicity_family == "consumption_welfare_robustness"))
})

test_that("alternative welfare saver persists only compact family artifacts", {
  root <- tempfile("consumption-welfare-iv-")
  dynamics <- list(
    summary = data.frame(specification_id = "s", estimate = 1),
    anderson_rubin_grid = data.frame(specification_id = "s", beta = 0, p.value = .5)
  )
  support <- data.frame(group_id = "w", n_common = 10, status = "ready")

  paths <- save_consumption_iv_robustness_family(
    dynamics, support, "consumption_alternative_welfare_robustness", root
  )

  expect_setequal(
    basename(paths),
    c(
      "consumption_alternative_welfare_robustness.csv",
      "consumption_alternative_welfare_robustness_common_support.csv"
    )
  )
  expect_false(file.exists(file.path(
    root, "consumption_alternative_welfare_robustness_anderson_rubin_grid.csv"
  )))
})

test_that("causal control strategies distinguish adjustment philosophies", {
  strategies <- iv_causal_control_strategy_adjustments()
  expect_equal(length(strategies), 6L)
  expect_setequal(
    names(strategies),
    c(
      "region_fe_only", "region_compact_2001",
      "region_compact_2001_no_human_capital",
      "state_fe_only", "state_compact_2001",
      "state_compact_2001_no_human_capital"
    )
  )
  expect_length(strategies$region_fe_only$controls, 0L)
  expect_length(strategies$state_fe_only$controls, 0L)
  expect_identical(
    strategies$state_compact_2001$controls,
    census_2001_main_controls()
  )
  expect_length(
    intersect(
      strategies$state_compact_2001_no_human_capital$controls,
      iv_control_block_membership()$human_capital
    ),
    0L
  )
  expect_true(all(vapply(
    strategies, function(x) nzchar(x$theoretical_role), logical(1)
  )))
  expect_true(all(vapply(
    strategies, function(x) nzchar(x$caution), logical(1)
  )))
  expect_setequal(
    vapply(strategies, `[[`, character(1), "control_strategy_id"),
    c(
      "geography_only", "observed_exclusion_threat_adjustment",
      "potential_pathway_robustness"
    )
  )
  expect_setequal(
    vapply(strategies, `[[`, character(1), "control_parameterization_id"),
    c("none", "secondary_compact_economic")
  )
})

test_that("consumption control-strategy robustness isolates six theory-based adjustments", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  consumption <- read_consumption_iv_outcome_registry(
    file.path(root, "data/metadata/consumption_iv_outcomes.csv")
  )
  specs <- compile_consumption_control_strategy_specifications(consumption)

  expect_equal(nrow(specs), 48L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_setequal(
    unique(specs$adjustment_id),
    names(iv_causal_control_strategy_adjustments())
  )
  expect_true(all(specs$construction_id == "nonzero_mean"))
  expect_true(all(specs$sample_rule == "consumption_control_strategy_common_support"))
  expect_true(all(specs$tier == "C"))
  expect_setequal(
    unique(specs$control_strategy_id),
    c(
      "geography_only", "observed_exclusion_threat_adjustment",
      "potential_pathway_robustness"
    )
  )
  expect_setequal(
    unique(specs$control_parameterization_id),
    c("none", "secondary_compact_economic")
  )
  expect_true(all(vapply(specs$excluded_instruments, function(x) {
    identical(plain_chr(x), preferred_iv_variables()$instrument)
  }, logical(1))))

  strategies <- iv_causal_control_strategy_adjustments()
  fe_only <- specs[specs$adjustment_id == "state_fe_only", , drop = FALSE]
  compact <- specs[specs$adjustment_id == "state_compact_2001", , drop = FALSE]
  no_hc <- specs[specs$adjustment_id == "state_compact_2001_no_human_capital", , drop = FALSE]
  expect_true(all(vapply(fe_only$controls, length, integer(1)) %in% c(0L, 1L)))
  expect_true(all(vapply(compact$controls, function(x) {
    all(strategies$state_compact_2001$controls %in% plain_chr(x))
  }, logical(1))))
  expect_true(all(vapply(no_hc$controls, function(x) {
    !any(iv_control_block_membership()$human_capital %in% plain_chr(x))
  }, logical(1))))
})


test_that("IV adjustment registries share one named execution contract", {
  registries <- list(
    canonical = iv_adjustment_sets(),
    absorption = iv_absorption_adjustments(),
    blocks = iv_block_intervention_adjustments(),
    strategies = iv_causal_control_strategy_adjustments(),
    relevance_parameterizations = iv_main_parameterization_adjustments(),
    causal_parameterizations = iv_causal_control_parameterization_adjustments(),
    historical_adjustments = iv_historical_adjustment_comparison_adjustments()
  )
  for (registry in registries) {
    expect_true(length(registry) > 0L)
    expect_true(all(vapply(registry, function(adjustment) {
      all(c(
        "label", "fixed_effect", "controls",
        "control_strategy_id", "control_parameterization_id"
      ) %in% names(adjustment))
    }, logical(1))))
    expect_true(all(vapply(registry, function(adjustment) {
      adjustment$fixed_effect %in% c("none", "region", "state") && is.character(adjustment$controls)
    }, logical(1))))
  }
})

test_that("consumption control parameterization family includes common-sample benchmark and substitutions", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  adjustments <- iv_causal_control_parameterization_adjustments(controls)
  specs <- compile_consumption_control_parameterization_specifications(registry, controls)

  expect_equal(length(adjustments), 8L)
  expect_setequal(
    names(adjustments),
    c(
      "region_main", "region_main_literacy", "region_main_decomposed_economic",
      "region_main_literacy_decomposed_economic",
      "state_main", "state_main_literacy", "state_main_decomposed_economic",
      "state_main_literacy_decomposed_economic"
    )
  )
  expect_equal(nrow(specs), nrow(registry) * 8L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_true(all(specs$construction_id == "nonzero_mean"))
  expect_true(all(specs$sample_rule == "consumption_control_parameterization_common_support"))
  expect_true(all(specs$tier == "C"))

  benchmark <- census_2001_main_controls(controls)
  expect_setequal(adjustments$region_main$controls, benchmark)
  expect_setequal(adjustments$state_main$controls, benchmark)
  expect_true("literacy_share_2001" %in% adjustments$region_main_literacy$controls)
  expect_false("adult_secondary_plus_share_2001" %in% adjustments$region_main_literacy$controls)
  expect_true(all(c(
    "worker_share_2001", "cultivator_share_workers_2001",
    "agricultural_labourer_share_workers_2001"
  ) %in% adjustments$state_main_decomposed_economic$controls))
  expect_false("agricultural_worker_share_2001" %in% adjustments$state_main_decomposed_economic$controls)
})


test_that("production 1991 controls select the frozen population-interpolation threshold", {
  variables <- historical_baseline_1991_pca_variables()
  make_rows <- function(threshold, district) {
    out <- data.frame(
      state_code_2001 = "01",
      district_code_2001 = district,
      geography_spec_id = "G2_population_interpolated",
      source_coverage_threshold = threshold,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(variables)) out[[variables[[i]]]] <- i + threshold
    out
  }
  g2 <- list(controls = rbind(make_rows(.95, "01"), make_rows(.99, "02")))
  out <- production_historical_baseline_1991_controls(g2, .99)
  expect_equal(nrow(out), 1L)
  expect_identical(out$district_code_2001, "02")
  expect_identical(names(out), c(census_2001_keys(), variables))
})

test_that("historical adjustment robustness re-estimates 1991 and 2001 benchmarks on common-support designs", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  adjustments <- iv_historical_adjustment_comparison_adjustments(controls)
  specs <- compile_consumption_historical_adjustment_specifications(registry, controls)

  expect_equal(length(adjustments), 4L)
  expect_setequal(
    names(adjustments),
    c(
      "region_compact_2001", "region_predetermined_1991",
      "state_compact_2001", "state_predetermined_1991"
    )
  )
  expect_equal(nrow(specs), nrow(registry) * 4L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_true(all(specs$construction_id == "nonzero_mean"))
  expect_true(all(specs$sample_rule == "consumption_historical_adjustment_common_support"))
  expect_setequal(
    adjustments$state_compact_2001$controls,
    census_2001_main_controls(controls)
  )
  expect_setequal(
    adjustments$state_predetermined_1991$controls,
    historical_baseline_1991_pca_variables()
  )
  expect_identical(adjustments$state_predetermined_1991$adjustment_vintage, "1991")
  expect_true(nzchar(adjustments$state_predetermined_1991$caution))
})

test_that("Vanneman concept-matched historical adjustments preserve benchmark and source roles", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  adjustments <- iv_historical_concept_matched_adjustments(controls)
  specs <- compile_consumption_historical_concept_matched_specifications(registry, controls)

  expect_equal(length(adjustments), 6L)
  expect_setequal(
    names(adjustments),
    c(
      "region_compact_2001", "region_pca_1991", "region_vanneman_1991",
      "state_compact_2001", "state_pca_1991", "state_vanneman_1991"
    )
  )
  expect_equal(nrow(specs), nrow(registry) * 6L)
  expect_true(all(specs$construction_id == "nonzero_mean"))
  expect_true(all(specs$sample_rule == "consumption_historical_concept_matched_common_support"))
  expect_setequal(adjustments$state_compact_2001$controls, census_2001_main_controls(controls))
  expect_setequal(adjustments$state_pca_1991$controls, historical_baseline_1991_pca_variables())
  expect_setequal(adjustments$state_vanneman_1991$controls, vanneman_historical_baseline_1991_variables())
})

test_that("Vanneman dist91 control reader excludes aggregate administrative rows before key normalization", {
  record_ids <- c(population = "100", muslim = "332")
  line <- function(state, district, record, total, rural = total) {
    sprintf("%s%s%s91%d%9d%9d", state, district, record, 2L, total, rural)
  }
  path <- tempfile(fileext = ".data.gz")
  con <- gzfile(path, open = "wt")
  writeLines(c(
    line("01", "00", "100", 1000, 700),
    line("01", "00", "332", 200, 150),
    line("01", "01", "100", 100, 60),
    line("01", "01", "332", 20, 12)
  ), con)
  close(con)

  out <- read_vanneman_dist91_control_counts(path, record_ids)

  expect_equal(nrow(out), 1L)
  expect_identical(out$state_code_1991, "01")
  expect_identical(out$district_code_1991, "01")
  expect_equal(out$population, 100)
  expect_equal(out$rural_population, 60)
  expect_equal(out$muslim, 20)
  expect_false(anyNA(out[c("state_code_1991", "district_code_1991")]))
})

test_that("Vanneman 1991 control measures preserve ratio accounting", {
  counts <- data.frame(
    population_1991_count = 100,
    rural_population_1991_count = 60,
    sc_population_1991_count = 10,
    st_population_1991_count = 5,
    muslim_population_1991_count = 20,
    matriculate_plus_1991_count = 18,
    population_10plus_1991_count = 80,
    main_workers_1991_count = 40,
    farm_workers_1991_count = 24,
    dependent_population_1991_count = 45,
    working_age_population_1991_count = 55,
    households_h4_1991_count = 20,
    households_electricity_1991_count = 12
  )
  out <- vanneman_historical_baseline_1991_measures_from_counts(counts)
  expect_equal(out$vanneman_urban_share_1991, 40)
  expect_equal(out$vanneman_muslim_share_1991, 20)
  expect_equal(out$vanneman_matriculate_plus_share_10plus_1991, 100 * 18 / 80)
  expect_equal(out$vanneman_agricultural_worker_share_main_1991, 60)
  expect_equal(out$vanneman_dependency_ratio_1991, 100 * 45 / 55)
  expect_equal(out$vanneman_electricity_access_share_1991, 60)
})


test_that("intensive-margin consumption robustness uses the frozen six-design scalar grid", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  specs <- compile_consumption_treatment_robustness_specifications(registry)

  expect_equal(nrow(specs), nrow(registry) * 6L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_true(all(specs$treatment == intensive_margin_emi_treatment()))
  expect_true(all(specs$sample_rule == "consumption_treatment_iv_common_support"))
  expect_true(all(specs$tier == "C"))
  expect_setequal(unique(specs$adjustment_id), iv_candidate_design_adjustments())
  expect_setequal(unique(specs$construction_id), unname(iv_candidate_design_constructions()))
  expect_true(all(vapply(specs$excluded_instruments, function(x) length(x) == 1L, logical(1))))
})

test_that("consumption robustness evidence summarizes realized families without changing inference", {
  specs <- iv_specification_registry(outcome = "y", treatment = "t")[1:2, , drop = FALSE]
  specs$specification_id <- c("a", "b")
  dynamics <- list(summary = data.frame(
    analysis_id = c("consumption_iv__a", "consumption_iv__b"),
    specification_id = c("a", "b"),
    welfare_specification_id = c("w1", "w1"),
    welfare_outcome_id = "real_mean_mpce",
    outcome_round = "hces_2022_23",
    estimand = "change",
    effective_f = c(30, 2),
    effective_f_critical_value = c(23, 23),
    reduced_form_p_holm_family = c(.01, .6),
    anderson_rubin_p_beta0_holm_family = c(.02, .7),
    ar_95_empty = FALSE,
    ar_95_disconnected = c(FALSE, TRUE),
    ar_95_left_truncated = c(FALSE, TRUE),
    ar_95_right_truncated = FALSE,
    n = c(500, 500),
    multiplicity_family = "fixture_family",
    stringsAsFactors = FALSE
  ))

  out <- build_consumption_robustness_evidence(list(
    fixture = list(
      dynamics = dynamics,
      specifications = specs,
      analysis_role = "fixture_robustness"
    )
  ))

  expect_equal(nrow(out$grid), 2L)
  expect_identical(out$grid$analysis_id, dynamics$summary$analysis_id)
  expect_identical(out$grid$first_stage_strong, c(TRUE, FALSE))
  expect_identical(out$grid$reduced_form_family_signal, c(TRUE, FALSE))
  expect_identical(out$grid$ar_family_signal, c(TRUE, FALSE))
  expect_identical(out$grid$ar_95_bounded, c(TRUE, FALSE))
  expect_identical(out$family_summary$n_models, 2L)
  expect_identical(out$family_summary$n_strong_first_stage, 1L)
  expect_identical(out$family_summary$n_ar_family_signals, 1L)
  expect_identical(out$family_summary$n_grid_truncated_ar_sets, 1L)
})
