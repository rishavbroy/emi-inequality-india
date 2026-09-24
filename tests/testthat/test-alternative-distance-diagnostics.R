test_that("shared language preparation preserves mapped and unmapped rows for downstream diagnostics", {
  census <- data.frame(
    state_std = rep("01", 5), district_std = rep("001", 5),
    mother_tongue = c("English", "Dogri", "Hindi", "Bhojpuri", "Kashmiri"),
    canonical_language = c("English", "Dogri", "Hindi", "Hindi", "Kashmiri"),
    spkr_tot = c(10, 20, 60, 10, 5),
    ling_degrees = c(NA, NA, 0, NA, 4),
    stringsAsFactors = FALSE
  )
  panel <- data.frame(district_panel_id = "2001__01__001", stringsAsFactors = FALSE)

  prepared <- prepare_language_rows_for_decomposition(census, panel)
  expect_equal(nrow(prepared), 5)
  expect_true(any(!is.finite(num(prepared$ling_degrees))))
  expect_true(any(is.finite(num(prepared$ling_degrees))))

  unmapped <- unmapped_language_decomposition(census, panel)
  expect_setequal(unmapped$mother_tongue, c("Dogri", "Bhojpuri"))
  expect_false(any(unmapped$mother_tongue == "English"))
  expect_equal(sum(unmapped$unmapped_speakers), 30)

  distance4 <- distance_four_language_decomposition(census, panel)
  expect_identical(distance4$mother_tongue, "Kashmiri")
  expect_equal(sum(distance4$speakers), 5)
})

test_that("alternative linguistic-distance registry covers scalar, nonlinear, and joint constructions", {
  registry <- alternative_distance_registry()

  expect_equal(nrow(registry),
    length(alternative_distance_adjustments()) * length(alternative_distance_constructions()))
  expect_true(all(c(
    "nonzero_mean", "distant_share", "top3_legacy", "nonzero_mean_hindi_urdu",
    "nonzero_mean_shastry", "nonzero_mean_hindi_urdu_separate", "distance_shares_all",
    "distance_shares_all_unmapped", "distance_shares_mapped",
    "glottolog_mean", "glottolog_mean_shastry",
    "dyen_noncognate", "dyen_noncognate_shastry",
    "nonzero_mean_sensitivity_low", "nonzero_mean_sensitivity_high"
  ) %in% registry$construction_id))
  joint <- registry[registry$construction_id == "distance_shares_all", , drop = FALSE]
  expect_true(all(vapply(joint$excluded_instruments, function(x) {
    identical(x, linguistic_distance_excluded_instruments("all"))
  }, logical(1))))
  combined <- registry[registry$construction_id == "nonzero_mean_hindi_urdu", , drop = FALSE]
  expect_true(all(vapply(combined$included_language_controls, identical, logical(1), "hindi_urdu_share")))
  shastry <- registry[registry$construction_id == "nonzero_mean_shastry", , drop = FALSE]
  expect_true(all(vapply(
    shastry$included_language_controls,
    identical, logical(1), c("hindi_urdu_share", "native_english_share")
  )))
  shares <- registry[registry$construction_id == "distance_shares_all_unmapped", , drop = FALSE]
  expect_true(all(vapply(
    shares$included_language_controls,
    identical, logical(1), c("ling_unmapped_speaker_share", "native_english_share")
  )))
  expected_coverage <- c(
    nonzero_mean_sensitivity_low = "ling_sensitivity_mapped_speaker_share",
    nonzero_mean_sensitivity_high = "ling_sensitivity_mapped_speaker_share",
    glottolog_mean = "ling_glottolog_mapped_speaker_share",
    glottolog_mean_shastry = "ling_glottolog_mapped_speaker_share",
    dyen_noncognate = "ling_dyen_mapped_speaker_share",
    dyen_noncognate_shastry = "ling_dyen_mapped_speaker_share"
  )
  for (construction_id in names(expected_coverage)) {
    rows <- registry[registry$construction_id == construction_id, , drop = FALSE]
    expect_true(all(
      rows$mapping_coverage_variable == expected_coverage[[construction_id]]
    ))
  }

  default_ids <- setdiff(
    unique(registry$construction_id),
    names(expected_coverage)
  )
  expect_true(all(
    registry$mapping_coverage_variable[
      registry$construction_id %in% default_ids
    ] == "ling_mapped_speaker_share"
  ))
})

test_that("alternative-distance panels retain only explicitly requested outcomes", {
  n <- 24L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 4),
    district_code_2001 = sprintf("%03d", seq_len(n)),
    region = rep(panel_region_levels(), each = 4),
    emi_exposure_all_children_0708 = seq_len(n),
    future_hces_outcome = seq_len(n),
    real_log_consumption_change = c(NA_real_, seq_len(n - 1L)),
    stringsAsFactors = FALSE
  )
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- seq_len(n)
  for (variable in alternative_distance_variables()) panel[[variable]] <- 100

  projected <- project_alternative_distance_panel(panel)
  retained <- project_alternative_distance_panel(
    panel,
    retain = "real_log_consumption_change"
  )

  expect_false("future_hces_outcome" %in% names(projected))
  expect_false("real_log_consumption_change" %in% names(projected))
  expect_true("real_log_consumption_change" %in% names(retained))
  expect_equal(nrow(retained), nrow(panel))
  expect_true(is.na(retained$real_log_consumption_change[[1]]))
})

test_that("alternative-distance first-stage estimators handle scalar and joint instruments", {
  set.seed(19)
  n <- 48L
  states <- rep(sprintf("%02d", 1:12), each = 4)
  shares <- matrix(stats::runif(n * 6), ncol = 6)
  shares <- 100 * shares / rowSums(shares)
  panel <- data.frame(
    state_code_2001 = states,
    region = factor(rep(panel_region_levels(), each = 8), levels = panel_region_levels()),
    ling_distance_nonzero_mean = rowSums(
      shares[, 2:6, drop = FALSE] * rep(1:5, each = n)
    ) / rowSums(shares[, 2:6, drop = FALSE]),
    stringsAsFactors = FALSE
  )
  for (degree in 0:5) {
    panel[[paste0("ling_share_distance_", degree)]] <- shares[, degree + 1L]
  }
  panel$emi_exposure_all_children_0708 <-
    0.15 * panel$ling_share_distance_5 +
    0.08 * panel$ling_share_distance_4 + stats::rnorm(n)

  registry <- alternative_distance_registry()
  scalar_spec <- registry[
    registry$adjustment_id == "unadjusted" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  joint_spec <- registry[
    registry$adjustment_id == "unadjusted" &
      registry$construction_id == "distance_shares_all",
    , drop = FALSE
  ]

  scalar <- estimate_alternative_distance_spec(
    panel, scalar_spec, "emi_exposure_all_children_0708"
  )
  joint <- estimate_alternative_distance_spec(
    panel, joint_spec, "emi_exposure_all_children_0708"
  )

  expect_equal(scalar$summary$n_excluded_instruments, 1L)
  expect_equal(joint$summary$n_excluded_instruments, 5L)
  expect_true(is.finite(scalar$summary$joint_excluded_f))
  expect_true(is.finite(joint$summary$joint_excluded_f))
  expect_true(is.finite(joint$summary$partial_r_squared))
  expect_equal(
    joint$coefficients$term,
    linguistic_distance_excluded_instruments("all")
  )
})

test_that("alternative-distance assembly attaches registered analysis identity without refitting", {
  registry <- alternative_distance_registry()
  registry <- registry[
    registry$adjustment_id == "unadjusted" &
      registry$construction_id %in% c("nonzero_mean", "distance_shares_all"),
    , drop = FALSE
  ]
  data <- data.frame(
    state_code_2001 = rep(c("01", "02"), each = 2),
    region = rep(c("Northern", "North-Eastern"), each = 2),
    stringsAsFactors = FALSE
  )
  branches <- lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry[i, , drop = FALSE]
    excluded <- unlist(spec$excluded_instruments[[1]], use.names = FALSE)
    list(
      summary = data.frame(
        specification_id = spec$specification_id,
        n_excluded_instruments = length(excluded),
        stringsAsFactors = FALSE
      ),
      coefficients = data.frame(
        specification_id = spec$specification_id,
        term = excluded,
        stringsAsFactors = FALSE
      ),
      coverage_sensitivity = data.frame(
        specification_id = spec$specification_id,
        stringsAsFactors = FALSE
      )
    )
  })

  out <- assemble_alternative_distance_first_stages(data, registry, branches)
  expected_ids <- iv_analysis_id(
    "district_iv_diagnostic", registry$specification_id
  )

  expect_s3_class(out, "emi_alternative_distance_first_stages")
  expect_setequal(out$summary$analysis_id, expected_ids)
  expect_true(all(out$coefficients$analysis_id %in% expected_ids))
  expect_equal(out$common_support$n, nrow(data))
  expect_equal(out$common_support$n_states, 2L)
})

test_that("alternative linguistic-distance diagnostics save explicit outputs without recomputation", {
  registry <- alternative_distance_registry()[1, , drop = FALSE]
  diagnostic_specifications <- iv_diagnostic_specification_registry()[1, , drop = FALSE]
  diagnostics <- structure(
    list(
      summary = data.frame(status = "test"),
      coefficients = data.frame(status = "test"),
      registry = registry,
      diagnostic_specifications = diagnostic_specifications,
      common_support = data.frame(status = "test"),
      coverage_sensitivity = data.frame(status = "test"),
      distance4_languages = data.frame(status = "test"),
      unmapped_languages = data.frame(status = "test"),
      distance4_leave_one_out = data.frame(status = "test"),
      weak_iv_outcomes = data.frame(status = "test"),
      anderson_rubin_grid = data.frame(status = "test"),
      diagnostic_applicability = data.frame(status = "test"),
      diagnostic_registry = data.frame(status = "test"),
      overidentification = data.frame(status = "test"),
      falsification_adaptive_summary = data.frame(status = "test"),
      falsification_adaptive_components = data.frame(status = "test"),
      monotonicity_summary = data.frame(status = "test"),
      monotonicity_bins = data.frame(status = "test"),
      monotonicity_state_slopes = data.frame(status = "test"),
      basis_comparison = data.frame(status = "test"),
      design_evidence = data.frame(status = "test"),
      design_comparison = data.frame(status = "test")
    ),
    class = "emi_alternative_distance_first_stages"
  )
  dir <- tempfile("alternative-distance-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  manifest <- save_alternative_distance_first_stages(diagnostics, dir)

  expect_setequal(basename(manifest$path), c(
    "alternative_distance_first_stage_summary.csv",
    "alternative_distance_first_stage_coefficients.csv",
    "alternative_distance_first_stage_registry.csv",
    "alternative_distance_first_stage_common_support.csv",
    "alternative_distance_mapping_coverage_sensitivity.csv",
    "distance4_language_decomposition.csv",
    "unmapped_language_decomposition.csv",
    "distance4_leave_one_language_out.csv",
    "alternative_distance_weak_iv_outcomes.csv",
    "iv_diagnostic_applicability.csv",
    "iv_diagnostic_registry.csv",
    "iv_specification_registry.csv",
    "iv_overidentification.csv",
    "iv_falsification_adaptive_set_summary.csv",
    "iv_falsification_adaptive_set_components.csv",
    "iv_monotonicity_summary.csv",
    "iv_monotonicity_bins.csv",
    "iv_monotonicity_state_slopes.csv",
    "linguistic_distance_basis_comparison.csv",
    "alternative_distance_design_evidence.csv",
    "alternative_distance_design_comparison.csv"
  ))
  expect_true(all(file.exists(manifest$path)))
  expect_false(file.exists(file.path(dir, "alternative_distance_anderson_rubin_grid.csv")))
  expect_true(nrow(diagnostics$anderson_rubin_grid) > 0L)
})

test_that("alternative-distance design comparison preserves both FE candidates", {
  constructions <- iv_candidate_design_constructions()
  adjustments <- iv_candidate_design_adjustments()
  target <- expand.grid(
    adjustment_id = adjustments,
    construction_id = unname(constructions),
    stringsAsFactors = FALSE
  )
  ids <- paste(target$adjustment_id, target$construction_id, sep = "__")
  fixed_effect <- ifelse(target$adjustment_id == "state_main", "state", "region")
  primary <- target$construction_id == "nonzero_mean"

  diagnostics <- structure(
    list(
      summary = data.frame(
        specification_id = ids,
        adjustment_id = target$adjustment_id,
        construction_id = target$construction_id,
        fixed_effect = fixed_effect,
        joint_excluded_f = ifelse(
          primary,
          ifelse(target$adjustment_id == "region_main", 6, .75),
          .25
        ),
        partial_r_squared = ifelse(primary, .01, .001),
        n = 573L,
        stringsAsFactors = FALSE
      ),
      weak_iv_outcomes = data.frame(
        specification_id = ids,
        effective_f = ifelse(
          primary,
          ifelse(target$adjustment_id == "region_main", 7, .82),
          .3
        ),
        effective_f_critical_value = 23.1,
        anderson_rubin_p_beta0 = .25,
        ar_95_contains_zero = TRUE,
        ar_95_disconnected = FALSE,
        ar_95_left_truncated = FALSE,
        ar_95_right_truncated = FALSE,
        n = 573L,
        status = "estimated",
        stringsAsFactors = FALSE
      )
    ),
    class = "emi_alternative_distance_first_stages"
  )

  out <- summarize_alternative_distance_design_evidence(diagnostics)

  expect_setequal(out$comparison$adjustment_id, adjustments)
  expect_equal(nrow(out$evidence), length(adjustments) * length(constructions))
  expect_false(any(out$evidence$meets_effective_f_critical_value))
  primary_relative <- out$evidence$effective_f_relative_to_primary[
    out$evidence$construction_id == "nonzero_mean"
  ]
  expect_false(anyNA(primary_relative))
  expect_true(all(primary_relative == 1))
  expect_false(any(
    out$comparison$any_construction_meets_effective_f_critical_value
  ))
})

test_that("candidate IV adjustments do not encode an FE winner", {
  adjustments <- iv_adjustment_sets()
  candidates <- iv_candidate_design_adjustments()

  expect_setequal(candidates, c("region_main", "state_main"))
  expect_true(all(vapply(
    adjustments[candidates],
    `[[`,
    character(1),
    "tier"
  ) == "A"))
  expect_identical(
    adjustments$region_main$controls,
    adjustments$state_main$controls
  )
  expect_identical(adjustments$region_main$fixed_effect, "region")
  expect_identical(adjustments$state_main$fixed_effect, "state")
})


test_that("instrument constructions separate distance measures from language adjustment", {
  registry <- iv_instrument_construction_registry()
  measures <- iv_distance_measure_registry()
  adjustments <- iv_language_adjustment_registry()
  constructions <- iv_instrument_constructions()

  expect_equal(nrow(registry), 15L)
  expect_equal(anyDuplicated(registry$construction_id), 0L)
  expect_true(all(registry$distance_measure_id %in% names(measures)))
  expect_true(all(registry$language_adjustment_id %in% names(adjustments)))
  expect_identical(names(constructions), registry$construction_id)

  base <- constructions$nonzero_mean
  hindi_urdu <- constructions$nonzero_mean_hindi_urdu
  shastry <- constructions$nonzero_mean_shastry
  expect_identical(base$excluded, hindi_urdu$excluded)
  expect_identical(base$excluded, shastry$excluded)
  expect_identical(base$distance_measure_id, hindi_urdu$distance_measure_id)
  expect_identical(base$distance_measure_id, shastry$distance_measure_id)
  expect_identical(base$language_adjustment_id, "none")
  expect_identical(hindi_urdu$language_adjustment_id, "hindi_urdu")
  expect_identical(shastry$language_adjustment_id, "shastry_composition")
  expect_identical(hindi_urdu$included, "hindi_urdu_share")
  expect_setequal(shastry$included, c("hindi_urdu_share", "native_english_share"))

  glottolog <- constructions$glottolog_mean
  glottolog_adjusted <- constructions$glottolog_mean_shastry
  expect_identical(glottolog$excluded, glottolog_adjusted$excluded)
  expect_identical(glottolog$distance_measure_id, glottolog_adjusted$distance_measure_id)
  expect_false(identical(
    glottolog$language_adjustment_id,
    glottolog_adjusted$language_adjustment_id
  ))
})

test_that("canonical IV registry drives alternative-distance specifications", {
  registry <- iv_specification_registry()

  expect_identical(alternative_distance_registry(), registry)
  expect_equal(nrow(registry), length(iv_adjustment_sets()) * length(iv_instrument_constructions()))
  expect_true(all(c(
    "outcome", "treatment", "fixed_effect", "controls", "excluded_instruments",
    "n_endogenous", "n_excluded_instruments", "panel_variant", "sample_rule",
    "cluster", "tier", "mapping_coverage_variable",
    "distance_measure_id", "language_adjustment_id"
  ) %in% names(registry)))
  expect_true(all(registry$n_endogenous == 1L))
  expect_true(all(registry$cluster == "state_code_2001"))
})

test_that("canonical IV diagnostic registry includes MOP effective F", {
  registry <- iv_diagnostic_registry()
  row <- registry[registry$diagnostic_id == "effective_f", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$family, "relevance")
  expect_true(row$implemented)
  expect_true(row$requires_outcome)
  expect_false(row$requires_overidentified)
})

test_that("canonical IV registries preserve vector-valued specification fields", {
  registry <- iv_specification_registry()
  absorption <- iv_absorption_specification_registry()
  combined <- iv_diagnostic_specification_registry()

  expect_true(is.list(registry$controls))
  expect_true(is.list(registry$included_language_controls))
  expect_true(is.list(registry$excluded_instruments))
  expect_true(is.list(absorption$controls))
  expect_true(is.list(combined$controls))

  main <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  expanded <- absorption[
    absorption$adjustment_id == "state_fe_expanded_controls",
    , drop = FALSE
  ]
  joint <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "distance_shares_all",
    , drop = FALSE
  ]

  expect_identical(main$controls[[1]], census_2001_main_controls())
  expect_identical(expanded$controls[[1]], census_2001_absorption_controls())
  expect_identical(
    joint$excluded_instruments[[1]],
    linguistic_distance_excluded_instruments("all")
  )
})

test_that("diagnostic applicability follows identification structure", {
  registry <- iv_specification_registry()
  applicability <- iv_diagnostic_applicability(registry)
  scalar_id <- registry$specification_id[registry$construction_id == "nonzero_mean" & registry$adjustment_id == "state_main"][[1]]
  multi_id <- registry$specification_id[registry$construction_id == "distance_shares_all" & registry$adjustment_id == "state_main"][[1]]

  scalar_overid <- applicability[
    applicability$specification_id == scalar_id & applicability$diagnostic_id == "overidentification", , drop = FALSE
  ]
  multi_overid <- applicability[
    applicability$specification_id == multi_id & applicability$diagnostic_id == "overidentification", , drop = FALSE
  ]

  expect_false(scalar_overid$applicable)
  expect_identical(scalar_overid$reason, "exactly_identified")
  expect_true(multi_overid$applicable)
  expect_true(multi_overid$implemented)
  expect_true(multi_overid$will_run)
  balance_joint <- applicability[applicability$diagnostic_id == "balance_joint", , drop = FALSE]
  n_inst <- registry$n_excluded_instruments[match(balance_joint$specification_id, registry$specification_id)]
  expect_true(all(balance_joint$applicable == (n_inst == 1L)))
  expect_true(all(balance_joint$will_run == (n_inst == 1L)))
  expect_true(all(applicability$applicable[applicability$diagnostic_id == "anderson_rubin"]))
  expect_true(all(applicability$will_run[applicability$diagnostic_id == "anderson_rubin"]))

  monotonicity <- applicability[applicability$diagnostic_id == "monotonicity_shape", , drop = FALSE]
  scalar_specs <- registry$n_excluded_instruments == 1L
  expect_true(all(monotonicity$applicable == scalar_specs))
  expect_true(all(monotonicity$will_run == scalar_specs))
})

test_that("IV specification variables resolve transformed fixed-effect terms", {
  registry <- iv_specification_registry()

  region_spec <- registry[
    registry$adjustment_id == "region_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  state_spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]

  region_vars <- iv_specification_variables(region_spec)
  state_vars <- iv_specification_variables(state_spec)

  expect_true("region" %in% region_vars)
  expect_false("factor(region)" %in% region_vars)
  expect_true("state_code_2001" %in% state_vars)
  expect_false("factor(state_code_2001)" %in% state_vars)
  expect_true(region_spec$outcome[[1]] %in% region_vars)
  expect_true(region_spec$treatment[[1]] %in% region_vars)
  expect_true(all(unlist(region_spec$excluded_instruments[[1]]) %in% region_vars))
  expect_true(region_spec$cluster[[1]] %in% region_vars)
})

test_that("conditional balance removes tested and accounting-linked controls", {
  registry <- iv_specification_registry()

  main_spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  ordinary <- census_2001_main_controls()[[1]]
  expect_false(ordinary %in% balance_nuisance_controls(main_spec, ordinary))
  expect_setequal(
    balance_nuisance_controls(main_spec, ordinary),
    setdiff(census_2001_main_controls(), ordinary)
  )

  expanded_spec <- registry[
    registry$adjustment_id == "state_expanded" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  aggregate <- "agricultural_worker_share_2001"
  nuisance <- balance_nuisance_controls(expanded_spec, aggregate)

  expect_false(any(c(
    aggregate,
    "cultivator_share_workers_2001",
    "agricultural_labourer_share_workers_2001"
  ) %in% nuisance))
})

test_that("conditional balance does not fit an accounting identity as an outcome", {
  set.seed(901)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  for (variable in census_2001_diagnostic_controls()) {
    panel[[variable]] <- stats::rnorm(n)
  }
  panel$cultivator_share_workers_2001 <- stats::runif(n, 0, 60)
  panel$agricultural_labourer_share_workers_2001 <- stats::runif(n, 0, 40)
  panel$agricultural_worker_share_2001 <-
    panel$cultivator_share_workers_2001 +
    panel$agricultural_labourer_share_workers_2001

  spec <- iv_specification_registry()
  spec <- spec[
    spec$adjustment_id == "state_expanded" &
      spec$construction_id == "nonzero_mean",
    , drop = FALSE
  ]

  expect_warning(
    out <- estimate_iv_balance_spec(
      panel,
      spec,
      "agricultural_worker_share_2001"
    ),
    NA
  )
  expect_equal(out$status, "estimated")
  expect_true(is.finite(out$joint_f))
})

test_that("registry-driven clustered diagnostics honor the declared cluster variable", {
  set.seed(905)
  n <- 120L
  panel <- data.frame(
    cluster_id = rep(sprintf("c%02d", 1:12), each = 10),
    real_log_consumption_change = stats::rnorm(n),
    emi_exposure_all_children_0708 = stats::rnorm(n),
    ling_distance_nonzero_mean = stats::rnorm(n),
    log_population_2001 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )

  registry <- iv_specification_registry()
  spec <- registry[
    registry$adjustment_id == "unadjusted" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  spec$cluster <- "cluster_id"

  expect_identical(iv_specification_cluster_variable(spec), "cluster_id")
  expect_identical(iv_specification_cluster(panel, spec), panel$cluster_id)
  expect_true("cluster_id" %in% iv_specification_variables(spec))
  expect_false("state_code_2001" %in% iv_specification_variables(spec))

  balance <- estimate_iv_balance_spec(panel, spec, "log_population_2001")
  expect_identical(balance$status[[1]], "estimated")
  expect_true(is.finite(balance$joint_f[[1]]))

  ar <- estimate_anderson_rubin_spec(panel, spec, points = 41L)
  expect_identical(ar$summary$status[[1]], "estimated")
  expect_true(is.finite(ar$summary$anderson_rubin_p_beta0[[1]]))
})

test_that("conditional balance uses specification fixed effects and clustered joint tests", {
  set.seed(902)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$emi_exposure_all_children_0708 <- 2 * panel$ling_distance_nonzero_mean + stats::rnorm(n)
  for (v in census_2001_diagnostic_controls()) panel[[v]] <- stats::rnorm(n)
  tested <- census_2001_main_controls()[[1]]
  panel[[tested]] <- 0.5 * panel$ling_distance_nonzero_mean + stats::rnorm(n)
  spec <- iv_specification_registry()
  spec <- spec[spec$adjustment_id == "state_main" & spec$construction_id == "nonzero_mean", , drop = FALSE]

  out <- estimate_iv_balance_spec(panel, spec, tested)

  expect_equal(out$status, "estimated")
  expect_equal(out$fixed_effect, "state")
  expect_equal(out$n_excluded_instruments, 1L)
  expect_true(is.finite(out$joint_f))
  expect_true(is.finite(out$standardized_effect))
})

test_that("absorption registry separates scientific aliases from unique execution cells", {
  candidates <- iv_absorption_specification_candidates()
  registry <- iv_absorption_specification_registry()
  aliases <- iv_absorption_specification_aliases()

  expect_equal(nrow(candidates), length(iv_absorption_adjustments()))
  expect_equal(nrow(registry), sum(!aliases$is_execution_alias))
  expect_lt(nrow(registry), nrow(candidates))
  expect_equal(anyDuplicated(vapply(seq_len(nrow(registry)), function(i) {
    iv_specification_signature(registry[i, , drop = FALSE])
  }, character(1))), 0L)

  expected_aliases <- c(
    region_fe_plus_basic_development = "region_fe_expanded_controls",
    state_fe_plus_basic_development = "state_fe_expanded_controls",
    region_block_only_basic_scale_geography = "region_fe_plus_basic_scale_geography",
    state_block_only_basic_scale_geography = "state_fe_plus_basic_scale_geography",
    region_main_without_human_capital = "region_fe_main_without_human_capital",
    state_main_without_human_capital = "state_fe_main_without_human_capital"
  )
  observed <- stats::setNames(
    aliases$execution_adjustment_id[aliases$is_execution_alias],
    aliases$semantic_adjustment_id[aliases$is_execution_alias]
  )
  expect_equal(unname(observed[names(expected_aliases)]), unname(expected_aliases))
  expect_equal(sum(aliases$is_execution_alias), length(expected_aliases))
})

test_that("IV diagnostic results receive family identity at the projection boundary", {
  specs <- iv_diagnostic_specification_registry()
  expect_false("analysis_id" %in% names(specs))

  result <- data.frame(
    specification_id = specs$specification_id[1:2],
    estimate = c(1, 2),
    stringsAsFactors = FALSE
  )
  linked <- attach_iv_analysis_id(result, specs, "district_iv_diagnostic")
  expect_identical(
    linked$analysis_id,
    iv_analysis_id("district_iv_diagnostic", specs$specification_id[1:2])
  )
  expect_error(
    attach_iv_analysis_id(
      data.frame(specification_id = "not_registered"), specs, "district_iv_diagnostic"
    ),
    "outside the supplied registry",
    fixed = TRUE
  )
})

test_that("diagnostic specification registry absorbs the control-block ladder without duplicates", {
  registry <- iv_diagnostic_specification_registry()
  signatures <- vapply(seq_len(nrow(registry)), function(i) {
    iv_specification_signature(registry[i, , drop = FALSE])
  }, character(1))

  expect_false(anyDuplicated(signatures) > 0L)
  expect_true(any(grepl("^absorption__", registry$specification_id)))
  expect_true(all(iv_specification_registry()$specification_id %in% registry$specification_id))
  expect_true(all(registry$cluster == "state_code_2001"))
})

test_that("joint balance tests only predetermined covariates not already conditioned on", {
  registry <- iv_specification_registry()
  spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  tested <- joint_balance_test_variables(spec)

  expect_setequal(tested, setdiff(
    census_2001_diagnostic_controls(),
    census_2001_main_controls()
  ))
})

test_that("joint balance is one omnibus test per scalar specification", {
  set.seed(904)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$emi_exposure_all_children_0708 <- stats::rnorm(n)
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)

  registry <- iv_specification_registry()
  scalar <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  multi <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "distance_shares_all",
    , drop = FALSE
  ]
  for (instrument in unlist(multi$excluded_instruments[[1]], use.names = FALSE)) {
    panel[[instrument]] <- stats::rnorm(n)
  }

  out <- run_iv_joint_balance_diagnostics(panel, rbind(scalar, multi))

  expect_equal(nrow(out), 1L)
  expect_identical(out$specification_id[[1]], scalar$specification_id[[1]])
  expect_identical(out$status[[1]], "estimated")
  expect_true(is.finite(out$joint_f[[1]]))
})

test_that("monotonicity shape diagnostic recognizes an increasing residual first stage", {
  set.seed(903)
  n <- 180L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:18), each = 10),
    region = rep(panel_region_levels(), each = 30),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$emi_exposure_all_children_0708 <-
    2 * panel$ling_distance_nonzero_mean + stats::rnorm(n, sd = 0.2)
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)

  registry <- iv_specification_registry()
  spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  out <- estimate_iv_monotonicity_shape(panel, spec, bins = 8L)

  expect_equal(out$summary$status, "estimated")
  expect_gt(out$summary$linear_slope, 0)
  expect_gt(out$summary$spearman_rho, 0)
  expect_gt(out$summary$share_nondecreasing_bin_steps, 0.5)
  expect_true(all(c("bin", "instrument", "treatment", "n") %in% names(out$bins)))
  expect_true(all(c("state_code_2001", "slope", "status") %in% names(out$state_slopes)))
})

test_that("Anderson-Rubin inference accepts scalar and multi-instrument registry specifications", {
  set.seed(904)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    real_log_consumption_change = stats::rnorm(n),
    emi_exposure_all_children_0708 = stats::rnorm(n),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  shares <- matrix(stats::runif(n * 6), ncol = 6)
  shares <- 100 * shares / rowSums(shares)
  for (degree in 0:5) {
    panel[[paste0("ling_share_distance_", degree)]] <- shares[, degree + 1]
    panel[[paste0("ling_mapped_share_distance_", degree)]] <- shares[, degree + 1]
  }
  panel$ling_share_distance_ge3 <- rowSums(shares[, 4:6, drop = FALSE])
  panel$ling_distance_top3_legacy <- stats::runif(n, 0, 5)
  panel$hindi_share <- stats::runif(n, 0, 60)
  panel$urdu_share <- stats::runif(n, 0, 20)
  panel$hindi_urdu_share <- panel$hindi_share + panel$urdu_share
  panel$native_english_share <- 0
  panel$ling_unmapped_speaker_share <- 0
  panel$ling_mapped_speaker_share <- 100
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)

  registry <- iv_specification_registry()
  scalar <- registry[
    registry$adjustment_id == "state_main" & registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  multi <- registry[
    registry$adjustment_id == "state_main" & registry$construction_id == "distance_shares_all",
    , drop = FALSE
  ]

  scalar_ar <- estimate_anderson_rubin_spec(panel, scalar, points = 21L)
  multi_ar <- estimate_anderson_rubin_spec(panel, multi, points = 21L)

  expect_equal(scalar_ar$summary$status, "estimated")
  expect_equal(multi_ar$summary$status, "estimated")
  expect_equal(nrow(scalar_ar$grid), 21L)
  expect_equal(nrow(multi_ar$grid), 21L)
  expect_true(all(c("anderson_rubin_f_beta0", "anderson_rubin_p_beta0") %in% names(multi_ar$summary)))
})


test_that("bounded exclusion AR nests exact exclusion and expands monotonically", {
  skip_if_not_installed("sandwich")
  set.seed(905)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    z = stats::rnorm(n),
    control = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$d <- 0.08 * panel$z + 0.3 * panel$control + stats::rnorm(n)
  panel$y <- 0.25 * panel$d + 0.04 * panel$z + 0.2 * panel$control + stats::rnorm(n)

  spec <- iv_specification_row(
    specification_id = "bounded_exclusion_toy",
    adjustment_id = "toy", adjustment = "Toy",
    construction_id = "toy", construction = "Toy",
    outcome = "y", treatment = "d", fixed_effect = "none",
    controls = "control", included_language_controls = character(),
    excluded_instruments = "z", mapping_coverage_variable = NA_character_,
    panel_variant = "primary", sample_rule = "public_model_specific_complete_case",
    cluster = "state_code_2001"
  )

  profile <- estimate_bounded_exclusion_ar_profile_spec(panel, spec, points = 21L)
  exact <- bounded_exclusion_ar_grid(profile, 0, 0)
  ordinary <- estimate_anderson_rubin_spec(panel, spec, points = 21L)$grid
  wider <- bounded_exclusion_ar_grid(profile, -0.1, 0.1)

  expect_equal(exact$beta, ordinary$beta)
  expect_equal(exact$statistic, ordinary$statistic, tolerance = 1e-8)
  expect_equal(exact$p.value, ordinary$p.value, tolerance = 1e-8)
  expect_true(all(!exact$accepted | wider$accepted))
  expect_true(all(wider$statistic <= exact$statistic + 1e-12, na.rm = TRUE))
})

test_that("headline consumption exclusion sensitivity is bounded and transparent", {
  skip_if_not_installed("sandwich")
  set.seed(906)
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  all_specs <- compile_consumption_iv_specifications(registry, controls)
  specs <- consumption_exclusion_sensitivity_specifications(all_specs)

  needed <- unique(unlist(lapply(seq_len(nrow(specs)), function(i) {
    iv_specification_variables(specs[i, , drop = FALSE])
  })))
  n <- 120L
  panel <- as.data.frame(
    setNames(replicate(length(needed), stats::rnorm(n), simplify = FALSE), needed),
    stringsAsFactors = FALSE
  )
  panel$state_code_2001 <- rep(sprintf("%02d", 1:12), each = 10)
  z <- preferred_iv_variables()$instrument
  d <- preferred_iv_variables()$treatment
  panel[[d]] <- 0.08 * panel[[z]] + stats::rnorm(n)
  for (outcome in unique(plain_chr(specs$outcome))) {
    panel[[outcome]] <- 0.05 * panel[[z]] + 0.2 * panel[[d]] + stats::rnorm(n)
  }

  reduced <- safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    estimate_iv_reduced_form_spec(panel, specs[i, , drop = FALSE])
  }))
  dynamics <- list(summary = data.frame(
    specification_id = reduced$specification_id,
    reduced_form_estimate = reduced$estimate,
    stringsAsFactors = FALSE
  ))
  out <- validate_consumption_exclusion_sensitivity(
    estimate_consumption_exclusion_sensitivity(
      panel, specs, dynamics, points = 21L
    ),
    specs
  )

  expect_equal(nrow(out$summary), 4L * 7L)
  expect_identical(
    specs$analysis_id,
    paste("consumption_exclusion_sensitivity", specs$specification_id, sep = "__")
  )
  expect_setequal(unique(out$summary$analysis_id), specs$analysis_id)
  expect_setequal(unique(out$grid$analysis_id), specs$analysis_id)
  expect_setequal(
    unique(out$summary$welfare_specification_id),
    c("long_2022__ancova", "long_2022__change", "long_2023__ancova", "long_2023__change")
  )
  expect_true(all(out$summary$calibration_role[out$summary$calibration_id != "exact_exclusion"] ==
    "observed_reduced_form_scale_fragility"))
  full_same_sign <- out$summary$calibration_id == "same_sign_rf_100"
  expect_true(all(out$summary$exclusion_ar_95_contains_zero[full_same_sign]))
  expect_true(all(
    out$summary$minimum_gamma_share_of_reduced_form_for_zero_95 >= 0 &
      out$summary$minimum_gamma_share_of_reduced_form_for_zero_95 <= 1
  ))
})

test_that("linguistic basis comparison remains pairwise under partial lexical coverage", {
  panel <- data.frame(
    ling_distance_nonzero_mean = c(1, 2, 3, 4),
    ling_distance_glottolog_nonhindi_mean = c(2, 4, 6, 8),
    ling_distance_dyen_noncognate_pct = c(20, 30, NA, 50)
  )

  out <- compare_linguistic_distance_bases(panel)

  expect_setequal(
    paste(out$basis_a, out$basis_b, sep = "__"),
    c("shastry__glottolog", "shastry__dyen", "glottolog__dyen")
  )
  expect_equal(out$n[out$basis_b == "dyen"], c(3, 3))
  expect_equal(
    out$pearson_correlation[out$basis_a == "shastry" & out$basis_b == "glottolog"],
    1
  )
})


test_that("language decomposition uses the same adjudicated Shastry resolver as production", {
  census <- data.frame(
    state_std = "01",
    district_std = "001",
    mother_tongue_code = c("000001", "006045"),
    mother_tongue = c("Hindi", "Bhojpuri"),
    canonical_language = c("Hindi", "Hindi"),
    spkr_tot = c(50, 50),
    stringsAsFactors = FALSE
  )
  panel <- data.frame(district_panel_id = make_district_key("01", "001", 2001L))

  rows <- prepare_language_rows_for_decomposition(census, panel)

  expect_equal(rows$ling_degrees[rows$language_identity == "Bhojpuri"], 3)
})


test_that("language decomposition can use the same non-Indo-European resolution as production", {
  census <- data.frame(
    state_std = "01",
    district_std = "001",
    mother_tongue_code = "000001",
    mother_tongue = "Korku",
    canonical_language = "Korku",
    spkr_tot = 100,
    stringsAsFactors = FALSE
  )
  panel <- data.frame(district_panel_id = make_district_key("01", "001", 2001L))
  g <- data.frame(
    id = c("aust1307", "korku"),
    family_id = c("", "aust1307"),
    parent_id = c("", "aust1307"),
    name = c("Austroasiatic", "Korku"),
    bookkeeping = FALSE,
    level = c("family", "language"),
    iso639P3code = c("", "kfq"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    mother_tongue_code = "000001",
    mother_tongue = "Korku",
    canonical_language = "Korku",
    language_glottocode = "korku",
    family_id = "aust1307",
    iso639P3code = "kfq",
    match_basis = "manual",
    review_status = "accepted_manual",
    stringsAsFactors = FALSE
  )

  prepared <- prepare_language_rows_for_decomposition(
    census, panel, list(languoids = g), crosswalk
  )

  expect_equal(prepared$ling_degrees, 5)
  expect_equal(nrow(unmapped_language_decomposition(
    census, panel, list(languoids = g), crosswalk
  )), 0)
})


test_that("Shastry adjudication sensitivities reuse the canonical IV registry machinery", {
  registry <- iv_specification_registry()
  rows <- registry[
    registry$construction_id %in%
      c("nonzero_mean_sensitivity_low", "nonzero_mean_sensitivity_high"),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 2L * length(iv_adjustment_sets()))
  expect_true(all(rows$mapping_coverage_variable == "ling_sensitivity_mapped_speaker_share"))
  expect_true(all(vapply(
    rows$included_language_controls,
    function(x) setequal(x, c("hindi_urdu_share", "native_english_share")),
    logical(1)
  )))
})

test_that("clustered first-stage diagnostics reuse a cached covariance without changing inference", {
  set.seed(42)
  data <- data.frame(
    y = rnorm(80),
    z1 = rnorm(80),
    z2 = rnorm(80),
    cluster = rep(seq_len(20), each = 4)
  )
  fit <- stats::lm(y ~ z1 + z2, data = data)
  cached <- iv_clustered_inference(fit, data$cluster)

  direct_joint <- clustered_joint_wald_test(fit, c("z1", "z2"), data$cluster)
  cached_joint <- clustered_joint_wald_test(
    fit, c("z1", "z2"), data$cluster, inference = cached
  )
  direct_coef <- clustered_first_stage_inference(fit, "z1", data$cluster)
  cached_coef <- clustered_first_stage_inference(
    fit, "z1", data$cluster, inference = cached
  )

  expect_equal(cached_joint, direct_joint, tolerance = 1e-12)
  expect_equal(cached_coef, direct_coef, tolerance = 1e-12)
})

test_that("IV control ordering preserves explicitly declared noncanonical controls", {
  canonical <- c("a", "b", "c")
  expect_identical(
    order_iv_controls(c("custom_baseline", "c", "a"), canonical),
    c("a", "c", "custom_baseline")
  )
  expect_identical(
    order_iv_controls(c("c", "a"), canonical),
    c("a", "c")
  )
})

test_that("Shastry Hindi-belt definition is frozen on Census-2001 state codes", {
  definition <- shastry_hindi_belt_state_definition()
  expect_identical(
    definition$state_code_2001,
    c("02", "03", "04", "05", "06", "07", "08", "09", "10", "20", "22", "23")
  )
  expect_setequal(
    definition$state_name_2001,
    c(
      "Himachal Pradesh", "Punjab", "Chandigarh", "Uttaranchal", "Haryana",
      "Delhi", "Rajasthan", "Uttar Pradesh", "Bihar", "Jharkhand",
      "Chhattisgarh", "Madhya Pradesh"
    )
  )
})

test_that("Hindi-belt first-stage registry is exactly the two non-state designs", {
  specs <- iv_hindi_belt_first_stage_specifications()
  expect_equal(nrow(specs), 2L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_identical(plain_chr(specs$fixed_effect), c("none", "region"))
  expect_true(all(specs$treatment == preferred_iv_variables()$treatment))
  expect_true(all(vapply(
    specs$excluded_instruments,
    function(x) identical(unname(unlist(x, use.names = FALSE)), "ling_distance_nonzero_mean"),
    logical(1)
  )))
  expect_true(all(vapply(
    specs$controls,
    function(x) shastry_hindi_belt_variable() %in% unlist(x, use.names = FALSE),
    logical(1)
  )))
  expect_false(any(specs$fixed_effect == "state"))
  expect_true(all(specs$sample_rule == "hindi_belt_first_stage_common_support"))
})

test_that("Hindi-belt first-stage comparison changes only the registered state-level control", {
  set.seed(903)
  state_region <- data.frame(
    state_code_2001 = c("01", "02", "18", "23", "19", "24", "28"),
    region = c("Northern", "Northern", "North Eastern", "Central", "Eastern", "Western", "Southern"),
    stringsAsFactors = FALSE
  )
  panel <- state_region[rep(seq_len(nrow(state_region)), each = 8L), , drop = FALSE]
  panel$district_code_2001 <- sprintf("%03d", seq_len(nrow(panel)))
  panel$region <- factor(panel$region, levels = panel_region_levels())
  panel$ling_distance_nonzero_mean <- stats::rnorm(nrow(panel))
  belt <- as.integer(panel$state_code_2001 %in% shastry_hindi_belt_state_codes())
  panel$emi_exposure_all_children_0708 <-
    10 + 2 * panel$ling_distance_nonzero_mean + 3 * belt + stats::rnorm(nrow(panel), sd = 0.5)
  for (variable in census_2001_main_controls()) panel[[variable]] <- stats::rnorm(nrow(panel))

  out <- diagnose_hindi_belt_first_stage(panel)

  expect_s3_class(out, "emi_hindi_belt_first_stage")
  expect_equal(nrow(out$summary), 2L)
  expect_identical(out$summary$fixed_effect, c("none", "region"))
  expect_true(all(out$summary$n == nrow(panel)))
  expect_true(all(out$summary$status == "estimated"))
  expect_true(all(is.finite(out$summary$estimate_change)))
  expect_identical(out$common_support$n_hindi_belt, 16L)
  expect_identical(out$common_support$n_non_hindi_belt, nrow(panel) - 16L)
})
