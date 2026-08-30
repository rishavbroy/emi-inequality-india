historical_persistence_fixture <- function() {
  state <- rep(c("02", "03"), each = 4)
  district <- rep(sprintf("%02d", 1:4), 2)
  d91 <- c(1, 2, 3, 4, 1.2, 2.2, 3.2, 4.2)
  historical <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    atlas_population_1991 = c(100, 120, 140, 160, 110, 130, 150, 170),
    min_accepted_coverage = 0.99,
    max_distance_bound_width = 0.5,
    historical_language_status = "eligible",
    ling_distance_nonzero_mean_1991 = d91,
    ling_share_distance_ge3_1991 = 10 * d91,
    ling_share_distance_ge3_upper_bound_1991 = 10 * d91 + 1,
    ling_distance_nonzero_bound_width_1991 = 0.1,
    ling_share_distance_ge3_bound_width_1991 = 1,
    stringsAsFactors = FALSE
  )
  geography <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    population_coverage = c(rep(1, 3), 0.995, rep(1, 3), 0.80),
    n_target_2001_districts = c(rep(1L, 7), 2L),
    exact_language_persistence = c(rep(TRUE, 3), FALSE, rep(TRUE, 3), FALSE),
    preferred_language_persistence = c(rep(TRUE, 7), FALSE),
    stringsAsFactors = FALSE
  )
  one_target <- data.frame(
    state_code_1991 = state[1:7],
    district_code_1991 = district[1:7],
    state_code_2001 = state[1:7],
    district_code_2001 = district[1:7],
    stringsAsFactors = FALSE
  )
  split_target <- data.frame(
    state_code_1991 = rep("03", 2),
    district_code_1991 = rep("04", 2),
    state_code_2001 = rep("03", 2),
    district_code_2001 = c("04", "05"),
    stringsAsFactors = FALSE
  )
  transition <- rbind(one_target, split_target)
  current <- data.frame(
    state_code = c(state, "03"),
    district_code = c(district, "05"),
    ling_distance_nonzero_mean = c(
      2 * d91[1:4],
      2 * d91[5:8] + 1,
      10
    ),
    ling_share_distance_ge3 = c(
      20 * d91[1:4],
      20 * d91[5:8] + 5,
      100
    ),
    stringsAsFactors = FALSE
  )
  list(
    historical = historical,
    current = current,
    geography = list(source_districts = geography, transition = transition)
  )
}

test_that("historical persistence normalizes the production Census-2001 distance schema", {
  current <- data.frame(
    state_code = c(2, 2),
    district_code = c(1, 2),
    ling_distance_nonzero_mean = c(1.5, 2.5),
    ling_share_distance_ge3 = c(25, 50),
    state_std = c("state", "state"),
    district_std = c("one", "two"),
    stringsAsFactors = FALSE
  )
  out <- historical_linguistic_distance_2001_panel(current)

  expect_identical(
    names(out),
    c(
      "state_code_2001", "district_code_2001",
      "ling_distance_nonzero_mean_2001", "ling_share_distance_ge3_2001"
    )
  )
  expect_equal(out$state_code_2001, c("02", "02"))
  expect_equal(out$district_code_2001, c("01", "02"))
  expect_equal(out$ling_distance_nonzero_mean_2001, c(1.5, 2.5))
  expect_equal(out$ling_share_distance_ge3_2001, c(25, 50))
  expect_error(
    historical_linguistic_distance_2001_panel(transform(current, district_code = 1)),
    "duplicate district keys"
  )
  expect_error(
    historical_linguistic_distance_2001_panel(current[c(
      "state_std", "district_std", "ling_distance_nonzero_mean", "ling_share_distance_ge3"
    )]),
    "lacks fields: state_code, district_code"
  )
})

test_that("historical persistence R-squared matches lm without summary-time warnings", {
  data <- data.frame(
    y = c(1, 3, 2, 6, 5),
    x = c(0, 1, 2, 3, 4),
    w = c(1, 2, 1, 3, 2)
  )
  weighted <- stats::lm(y ~ x, data = data, weights = w)
  ordinary <- stats::lm(y ~ x, data = data)

  expect_equal(
    historical_linguistic_lm_r_squared(weighted),
    summary(weighted)$r.squared,
    tolerance = 1e-12
  )
  expect_equal(
    historical_linguistic_lm_r_squared(ordinary),
    summary(ordinary)$r.squared,
    tolerance = 1e-12
  )

  perfect <- stats::lm(y ~ x, data = data.frame(y = 2 * (1:6), x = 1:6), weights = 1:6)
  expect_warning(summary(perfect), "essentially perfect fit")
  expect_warning(value <- historical_linguistic_lm_r_squared(perfect), NA)
  expect_equal(value, 1, tolerance = 1e-12)
})

test_that("historical persistence registry separates canonical measures from source-endpoint sensitivity", {
  registry <- historical_linguistic_persistence_measure_registry()
  expect_identical(
    registry$measure_id,
    c("nonzero_mean", "accepted_distant_share_ge3", "distant_share_ge3_upper_endpoint")
  )
  expect_identical(
    registry$measure_role,
    c("primary", "lower_bound", "upper_endpoint_sensitivity")
  )
  expect_identical(
    registry$current_variable,
    c(
      "ling_distance_nonzero_mean_2001",
      "ling_share_distance_ge3_2001",
      "ling_share_distance_ge3_2001"
    )
  )
  expect_identical(
    registry$historical_variable[registry$measure_role == "upper_endpoint_sensitivity"],
    "ling_share_distance_ge3_upper_bound_1991"
  )
  expect_true(all(grepl("bound_width_1991$", registry$historical_bound_width_variable)))
})

test_that("historical persistence keeps preferred and exact geography distinct", {
  fixture <- historical_persistence_fixture()
  out <- build_historical_linguistic_persistence_validation(
    fixture$historical, fixture$current, fixture$geography
  )

  split <- out$panel[
    out$panel$state_code_1991 == "03" & out$panel$district_code_1991 == "04",
    , drop = FALSE
  ]
  high_coverage <- out$panel[
    out$panel$state_code_1991 == "02" & out$panel$district_code_1991 == "04",
    , drop = FALSE
  ]
  expect_identical(split$persistence_status, "geography_not_preferred")
  expect_true(high_coverage$persistence_status == "eligible")
  expect_false(high_coverage$exact_language_persistence)

  preferred <- out$summary[
    out$summary$sample == "preferred_geography" & out$summary$measure_id == "nonzero_mean",
    , drop = FALSE
  ]
  exact <- out$summary[
    out$summary$sample == "exact_one_to_one" & out$summary$measure_id == "nonzero_mean",
    , drop = FALSE
  ]
  distant <- out$summary[
    out$summary$sample == "preferred_geography" &
      out$summary$measure_id == "accepted_distant_share_ge3",
    , drop = FALSE
  ]
  distant_upper <- out$summary[
    out$summary$sample == "preferred_geography" &
      out$summary$measure_id == "distant_share_ge3_upper_endpoint",
    , drop = FALSE
  ]
  expect_equal(preferred$n_districts, 7L)
  expect_equal(exact$n_districts, 6L)
  expect_equal(distant$n_districts, 7L)
  expect_equal(distant_upper$n_districts, 7L)
  expect_identical(distant$measure_role, "lower_bound")
  expect_identical(distant_upper$measure_role, "upper_endpoint_sensitivity")
  expect_equal(distant$state_fe_population_weighted_slope, 2, tolerance = 1e-10)
  expect_equal(distant$historical_bound_width_variable, "ling_share_distance_ge3_bound_width_1991")
  expect_equal(preferred$min_accepted_coverage, 0.99)
  expect_equal(preferred$max_distance_bound_width, 0.5)
  expect_equal(preferred$state_fe_population_weighted_slope, 2, tolerance = 1e-10)
  expect_true(is.finite(preferred$population_weighted_pearson))
  expect_true(is.finite(preferred$population_weighted_spearman))
  expect_equal(sum(out$quintile_transition$n_districts[out$quintile_transition$sample == "preferred_geography"]), 7L)
  expect_equal(sum(out$quintile_transition$n_districts[out$quintile_transition$sample == "exact_one_to_one"]), 6L)
})

test_that("historical persistence fails closed on stale thresholds and bridge summaries", {
  fixture <- historical_persistence_fixture()
  mixed_threshold <- fixture$historical
  mixed_threshold$min_accepted_coverage[[1]] <- 0.98
  expect_error(
    build_historical_linguistic_persistence_validation(
      mixed_threshold, fixture$current, fixture$geography
    ),
    "one explicit accepted-speaker coverage threshold"
  )

  mixed_bound <- fixture$historical
  mixed_bound$max_distance_bound_width[[1]] <- 0.25
  expect_error(
    build_historical_linguistic_persistence_validation(
      mixed_bound, fixture$current, fixture$geography
    ),
    "one explicit distance-bound threshold"
  )

  stale <- fixture$geography
  stale$source_districts$n_target_2001_districts[[1]] <- 2L
  stale$source_districts$preferred_language_persistence[[1]] <- FALSE
  expect_error(
    build_historical_linguistic_persistence_validation(
      fixture$historical, fixture$current, stale
    ),
    "summary and transition disagree"
  )

  bad_nesting <- fixture$geography
  bad_nesting$source_districts$preferred_language_persistence[[1]] <- FALSE
  expect_error(
    build_historical_linguistic_persistence_validation(
      fixture$historical, fixture$current, bad_nesting
    ),
    "subset of preferred geography"
  )

  bad_preferred <- fixture$geography
  bad_preferred$source_districts$preferred_language_persistence[[8]] <- TRUE
  expect_error(
    build_historical_linguistic_persistence_validation(
      fixture$historical, fixture$current, bad_preferred
    ),
    "map to exactly one Census-2001 district"
  )
})

test_that("weighted historical correlations are defined from population weights", {
  x <- c(1, 2, 4, 5)
  y <- c(2, 1, 5, 4)
  w <- c(1, 2, 3, 4)
  expected <- stats::cov.wt(cbind(x, y), wt = w, cor = TRUE)$cor[1, 2]
  expected_rank <- stats::cov.wt(
    cbind(rank(x), rank(y)), wt = w, cor = TRUE
  )$cor[1, 2]

  expect_equal(historical_linguistic_weighted_correlation(x, y, w), expected)
  expect_equal(
    historical_linguistic_weighted_correlation(x, y, w, rank_values = TRUE),
    expected_rank
  )
  expect_true(is.na(historical_linguistic_weighted_correlation(rep(1, 4), y, w)))
})

historical_first_stage_fixture <- function() {
  n <- 60L
  state <- sprintf("%02d", rep(2:11, each = 6))
  district <- sprintf("%02d", seq_len(n))
  region <- rep(panel_region_levels(), length.out = n)
  z91 <- seq(0.4, 4.0, length.out = n) + rep(c(-0.05, 0.03, 0.01), length.out = n)
  z01 <- z91 + 0.5
  treatment <- 2 * z91 + 3 + 0.05 * sin(seq_len(n) / 3)
  historical <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    atlas_population_1991 = 1000 + seq_len(n),
    min_accepted_coverage = 0.99,
    max_distance_bound_width = 0.5,
    historical_language_status = "eligible",
    ling_distance_nonzero_mean_1991 = z91,
    ling_share_distance_ge3_1991 = 10 * z91,
    ling_distance_nonzero_bound_width_1991 = 0.1,
    ling_share_distance_ge3_bound_width_1991 = 0.5,
    stringsAsFactors = FALSE
  )
  current <- data.frame(
    state_code = state,
    district_code = district,
    ling_distance_nonzero_mean = z01,
    ling_share_distance_ge3 = 10 * z01,
    stringsAsFactors = FALSE
  )
  source_districts <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    exact_language_persistence = c(FALSE, rep(TRUE, n - 1L)),
    preferred_language_persistence = TRUE,
    population_coverage = c(0.995, rep(1, n - 1L)),
    n_target_2001_districts = 1L,
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    state_code_2001 = state,
    district_code_2001 = district,
    stringsAsFactors = FALSE
  )
  controls <- census_2001_diagnostic_controls()
  panel <- data.frame(
    state_code_2001 = state,
    district_code_2001 = district,
    region = region,
    emi_exposure_all_children_0708 = treatment,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(controls)) {
    panel[[controls[[i]]]] <- sin(seq_len(n) * (i + 1) / 17) + cos(seq_len(n) / (i + 2))
  }
  list(
    historical = historical,
    current = current,
    geography = list(source_districts = source_districts, transition = transition),
    panel = panel
  )
}

test_that("historical first-stage robustness compares instrument vintages on common support", {
  fixture <- historical_first_stage_fixture()
  out <- build_historical_linguistic_first_stage_robustness(
    fixture$historical, fixture$current, fixture$geography, fixture$panel
  )

  expect_s3_class(out, "emi_historical_linguistic_first_stage")
  expect_setequal(out$estimates$instrument_vintage, c("historical_1991", "census_2001"))
  expect_setequal(out$estimates$sample, c("preferred_geography", "exact_one_to_one"))
  expect_equal(unique(out$panel$min_accepted_coverage), 0.99)
  expect_equal(unique(out$panel$max_distance_bound_width), 0.5)
  expect_equal(nrow(out$panel), 60L)

  unadjusted <- out$estimates[
    out$estimates$sample == "preferred_geography" &
      out$estimates$specification_id == "instrument_only",
    , drop = FALSE
  ]
  expect_equal(unadjusted$n, c(60, 60))
  expected <- unname(stats::coef(stats::lm(
    emi_exposure_all_children_0708 ~ ling_distance_nonzero_mean_1991,
    data = out$panel
  ))[["ling_distance_nonzero_mean_1991"]])
  expect_equal(unadjusted$estimate, rep(expected, 2), tolerance = 1e-10)

  comparison <- out$comparison[
    out$comparison$sample == "preferred_geography" &
      out$comparison$specification_id == "instrument_only",
    , drop = FALSE
  ]
  expect_equal(comparison$n_1991, comparison$n_2001)
  expect_equal(comparison$estimate_change_1991_vs_2001, 0, tolerance = 1e-10)
})

test_that("historical first-stage robustness excludes nonpreferred geography and uses one common sample", {
  fixture <- historical_first_stage_fixture()
  fixture$geography$source_districts$preferred_language_persistence[[2L]] <- FALSE
  fixture$geography$source_districts$exact_language_persistence[[2L]] <- FALSE
  fixture$panel[[census_2001_diagnostic_controls()[[1L]]]][[3L]] <- NA_real_

  out <- build_historical_linguistic_first_stage_robustness(
    fixture$historical, fixture$current, fixture$geography, fixture$panel
  )

  expect_equal(nrow(out$panel), 58L)
  expect_false(any(out$panel$district_code_2001 %in% c("02", "03")))
  by_spec <- split(out$estimates$n, interaction(out$estimates$sample, out$estimates$specification_id, drop = TRUE))
  expect_true(all(vapply(by_spec, function(n) length(unique(n)) == 1L, logical(1))))
})

test_that("historical first-stage robustness fails on duplicate destination districts", {
  fixture <- historical_first_stage_fixture()
  duplicate <- fixture$panel[1L, , drop = FALSE]
  fixture$panel <- rbind(fixture$panel, duplicate)
  expect_error(
    build_historical_linguistic_first_stage_robustness(
      fixture$historical, fixture$current, fixture$geography, fixture$panel
    ),
    "duplicate Census-2001 district keys"
  )
})

test_that("historical geography benchmark uses only uniquely population-identified sources", {
  geography <- list(
    source_districts = data.frame(
      state_code_1991 = c("02", "02", "03"),
      district_code_1991 = c("01", "02", "01"),
      population_1991_total = c(100, 200, 200),
      stringsAsFactors = FALSE
    ),
    transition = data.frame(
      state_code_1991 = c("02", "02", "02"),
      district_code_1991 = c("01", "01", "02"),
      state_code_2001 = c("28", "28", "28"),
      district_code_2001 = c("01", "02", "03"),
      population_share_to_2001 = c(0.59, 0.41, 1),
      stringsAsFactors = FALSE
    )
  )
  carveouts <- data.frame(
    district_1991 = c("Old", "Old", "Ambiguous"),
    pop_1991 = c(100, 100, 200),
    district_2001 = c("Alpha", "Beta", "Gamma"),
    pct_01in91 = c(60, 40, 100),
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = "28",
    district_code = c("01", "02", "03"),
    district_std = c("alpha", "beta", "gamma"),
    stringsAsFactors = FALSE
  )

  out <- build_historical_linguistic_geography_external_benchmark(
    geography, carveouts, admin
  )

  expect_equal(sum(out$edges$benchmark_status == "matched_edge"), 2L)
  ambiguous <- out$edges[out$edges$district_1991 == "Ambiguous", , drop = FALSE]
  expect_identical(ambiguous$benchmark_status, "source_population_not_unique")
  expect_equal(sort(out$edges$share_abs_diff[out$edges$benchmark_status == "matched_edge"]), c(0.01, 0.01))
  expect_equal(out$summary$n_population_identified_source_districts, 1L)
  expect_equal(out$summary$n_population_not_found_source_districts, 0L)
  expect_equal(out$summary$n_population_ambiguous_source_districts, 1L)
  expect_equal(out$summary$share_source_districts_population_identified, 0.5)
  expect_equal(out$summary$n_matched_edges, 2L)
  expect_equal(out$summary$share_source_edges_matched, 2 / 3)
  expect_equal(out$summary$share_matched_edges_within_1pp, 1)
})

test_that("historical geography benchmark distinguishes absent from ambiguous populations", {
  geography <- list(
    source_districts = data.frame(
      state_code_1991 = c("02", "03"),
      district_code_1991 = c("01", "01"),
      population_1991_total = c(200, 200),
      stringsAsFactors = FALSE
    ),
    transition = data.frame(
      state_code_1991 = character(), district_code_1991 = character(),
      state_code_2001 = character(), district_code_2001 = character(),
      population_share_to_2001 = numeric(), stringsAsFactors = FALSE
    )
  )
  carveouts <- data.frame(
    district_1991 = c("Missing", "Ambiguous"),
    pop_1991 = c(100, 200),
    district_2001 = c("Alpha", "Beta"),
    pct_01in91 = c(100, 100),
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = character(), district_code = character(), district_std = character(),
    stringsAsFactors = FALSE
  )

  out <- build_historical_linguistic_geography_external_benchmark(
    geography, carveouts, admin
  )

  expect_identical(
    out$edges$benchmark_status,
    c("source_population_not_found", "source_population_not_unique")
  )
  expect_equal(out$summary$n_population_not_found_source_districts, 1L)
  expect_equal(out$summary$n_population_ambiguous_source_districts, 1L)
})

test_that("historical geography benchmark does not fuzzy-match destination names", {
  geography <- list(
    source_districts = data.frame(
      state_code_1991 = "02", district_code_1991 = "01",
      population_1991_total = 100, stringsAsFactors = FALSE
    ),
    transition = data.frame(
      state_code_1991 = "02", district_code_1991 = "01",
      state_code_2001 = "28", district_code_2001 = "01",
      population_share_to_2001 = 1, stringsAsFactors = FALSE
    )
  )
  carveouts <- data.frame(
    district_1991 = "Old", pop_1991 = 100,
    district_2001 = "Alfa", pct_01in91 = 100,
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = "28", district_code = "01", district_std = "alpha",
    stringsAsFactors = FALSE
  )

  out <- historical_linguistic_carveout_benchmark(geography, carveouts, admin)
  expect_identical(out$benchmark_status, "target_name_not_in_shrug_transition")
  expect_true(is.na(out$shrug_population_share_to_2001))
})

historical_first_stage_baseline_fixture <- function(fixture) {
  metadata <- historical_baseline_1991_metadata()
  baseline <- data.frame(
    state_code_1991 = fixture$historical$state_code_1991,
    district_code_1991 = fixture$historical$district_code_1991,
    stringsAsFactors = FALSE
  )
  n <- nrow(baseline)
  for (i in seq_len(nrow(metadata))) {
    baseline[[metadata$variable[[i]]]] <- sin(seq_len(n) * (i + 2) / 13) + i / 5
  }
  baseline
}

test_that("historical first-stage robustness adds predetermined 1991 control sensitivities", {
  fixture <- historical_first_stage_fixture()
  baseline <- historical_first_stage_baseline_fixture(fixture)

  out <- build_historical_linguistic_first_stage_robustness(
    fixture$historical, fixture$current, fixture$geography, fixture$panel,
    baseline_1991 = baseline
  )

  expect_setequal(
    out$predetermined_registry$specification_id,
    c("state_fe_1991_pca_controls", "state_fe_1991_all_controls")
  )
  expect_setequal(out$predetermined_estimates$instrument_vintage, c("historical_1991", "census_2001"))
  expect_setequal(out$predetermined_estimates$sample, c("preferred_geography", "exact_one_to_one"))
  expect_true(all(out$predetermined_estimates$fixed_effect == "state_1991"))

  by_spec <- split(
    out$predetermined_estimates$n,
    interaction(
      out$predetermined_estimates$sample,
      out$predetermined_estimates$specification_id,
      drop = TRUE
    )
  )
  expect_true(all(vapply(by_spec, function(n) length(unique(n)) == 1L, logical(1))))
})

test_that("historical predetermined controls use specification-specific common support", {
  fixture <- historical_first_stage_fixture()
  baseline <- historical_first_stage_baseline_fixture(fixture)
  fixture$panel[[census_2001_diagnostic_controls()[[1L]]]][[3L]] <- NA_real_
  baseline$rural_phc_per_100k_1991[[4L]] <- NA_real_

  out <- build_historical_linguistic_first_stage_robustness(
    fixture$historical, fixture$current, fixture$geography, fixture$panel,
    baseline_1991 = baseline
  )

  expect_equal(nrow(out$panel), 59L)
  preferred <- out$predetermined_estimates[
    out$predetermined_estimates$sample == "preferred_geography",
    , drop = FALSE
  ]
  pca <- preferred[preferred$specification_id == "state_fe_1991_pca_controls", , drop = FALSE]
  all_controls <- preferred[preferred$specification_id == "state_fe_1991_all_controls", , drop = FALSE]
  expect_equal(unique(pca$n), 60L)
  expect_equal(unique(all_controls$n), 59L)
  expect_equal(length(unique(pca$n)), 1L)
  expect_equal(length(unique(all_controls$n)), 1L)
})

test_that("historical predetermined first stage retains saturated specifications as not estimable", {
  data <- data.frame(
    y = seq_len(6), z = c(1, 2, 3, 4, 5, 6),
    state_code_1991 = c("01", "01", "02", "02", "03", "03"),
    region = rep("Northern", 6),
    x1 = c(0, 1, 0, 0, 0, 0),
    x2 = c(0, 0, 0, 1, 0, 0),
    x3 = c(0, 0, 0, 0, 0, 1),
    stringsAsFactors = FALSE
  )
  specification <- data.frame(
    specification_id = "state_fe_1991_all_controls",
    label = "Saturated historical specification", sequence = 1L,
    fixed_effect = "state_1991", stringsAsFactors = FALSE
  )
  specification$controls <- I(list(c("x1", "x2", "x3")))

  expect_warning(
    out <- historical_linguistic_predetermined_first_stage_one(
      data, specification, "y", "z"
    ),
    NA
  )
  expect_identical(out$status, "not_estimable")
  expect_identical(out$reason, "no_residual_degrees_of_freedom")
  expect_true(is.na(out$residual_correlation))
})

test_that("historical first-stage comparison does not compare non-estimable rows", {
  estimates <- data.frame(
    sample = rep("exact_one_to_one", 2), specification_id = rep("spec", 2),
    specification = rep("Spec", 2), sequence = 1L, treatment = rep("y", 2),
    fixed_effect = rep("state", 2), control_blocks = rep("all", 2), n_controls = 2L,
    min_accepted_coverage = 0.99, max_distance_bound_width = 0.5,
    estimate = c(2, 5), excluded_instrument_f = c(4, NA), partial_r_squared = c(0.2, NA),
    n = 10L, n_states = 5L, n_regions = 4L,
    instrument_vintage = c("historical_1991", "census_2001"),
    status = c("estimated", "not_estimable"), reason = c(NA, "no_residual_degrees_of_freedom"),
    stringsAsFactors = FALSE
  )
  out <- historical_linguistic_first_stage_comparison(estimates)
  expect_identical(out$status_1991, "estimated")
  expect_identical(out$status_2001, "not_estimable")
  expect_true(is.na(out$estimate_change_1991_vs_2001))
  expect_true(is.na(out$f_change_1991_vs_2001))
  expect_true(is.na(out$partial_r_squared_change_1991_vs_2001))
})

test_that("historical source-quality grid keeps source and geography support distinct", {
  candidates <- data.frame(
    state_code_1991 = c("02", "02", "03"),
    district_code_1991 = c("01", "02", "01"),
    atlas_population_1991 = c(100, 200, 300),
    atlas_source_status = "candidate",
    complete_atlas_alignment_1991 = c(TRUE, FALSE, FALSE),
    accepted_speaker_coverage_1991 = c(0.995, 0.995, 0.98),
    ling_distance_nonzero_bound_width_1991 = c(0.1, 0.4, 0.1),
    stringsAsFactors = FALSE
  )
  geography <- data.frame(
    state_code_1991 = c("02", "02", "03"),
    district_code_1991 = c("01", "02", "01"),
    preferred_language_persistence = c(TRUE, FALSE, TRUE),
    exact_language_persistence = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  out <- historical_linguistic_source_quality_geography_grid(
    candidates, geography,
    coverage_thresholds = c(0.99), bound_width_thresholds = c(0.5)
  )
  expect_equal(out$n_districts, 2L)
  expect_equal(out$n_preferred_geography, 1L)
  expect_equal(out$n_preferred_states_1991, 1L)
  expect_equal(out$n_exact_geography, 1L)
  expect_equal(out$preferred_geography_population_1991, 100)
})


test_that("Helms-Lim historical-distance reader enforces the published IPUM district key", {
  path <- tempfile(fileext=".csv")
  x <- data.frame(state_code_1991=c("02","09"),district_code_1991=c("01","03"),ipum1993=c(2001L,9003L),district_name_helms_lim=c("Srikakulam","Hamirpur"),linguistic_distance_1991_helms_lim=c(4.8,3.8),source_row_count=c(3L,3L),source_years=c("1991;2001;2011","1991;2001;2011"),stringsAsFactors=FALSE)
  write.csv(x,path,row.names=FALSE); out <- read_helms_lim_linguistic_distance_1991(path)
  expect_identical(out$state_code_1991,c("02","09")); expect_identical(out$district_code_1991,c("01","03"))
  x$ipum1993[[2L]] <- 9999L; write.csv(x,path,row.names=FALSE); expect_error(read_helms_lim_linguistic_distance_1991(path),"decompose exactly")
})

test_that("Helms-Lim benchmark uses exact Census-1991 keys and surfaces disagreements", {
  helms <- data.frame(state_code_1991=c("02","02","03"),district_code_1991=c("01","02","01"),linguistic_distance_1991_helms_lim=c(1.1,3.6,5),stringsAsFactors=FALSE)
  atlas <- data.frame(state_code_1991=c("02","02"),district_code_1991=c("01","02"),historical_language_status="eligible",ling_distance_nonzero_mean_1991=c(1,3),stringsAsFactors=FALSE)
  geography <- data.frame(state_code_1991=c("02","02","03"),district_code_1991=c("01","02","01"),stringsAsFactors=FALSE)
  out <- build_helms_lim_linguistic_distance_benchmark(helms,atlas,geography)
  expect_equal(out$summary$n_atlas_preferred_overlap,2L); expect_equal(out$summary$n_project_1991_geography_with_helms_lim,3L); expect_equal(nrow(out$review),1L); expect_equal(out$review$district_code_1991,"02")
})


test_that("historical SHRUG geography saves deterministic harmonized-region artifacts", {
  body <- paste(deparse(body(build_historical_linguistic_geography_1991_2001)), collapse = "\n")
  saver <- paste(deparse(body(save_historical_linguistic_geography_1991_2001)), collapse = "\n")

  expect_match(body, "build_harmonized_region_crosswalk", fixed = TRUE)
  expect_match(body, "summarize_harmonized_region_crosswalk", fixed = TRUE)
  expect_match(
    saver,
    "historical_linguistic_harmonized_crosswalk_1991_2001.csv",
    fixed = TRUE
  )
  expect_match(
    saver,
    "historical_linguistic_harmonized_crosswalk_summary_1991_2001.csv",
    fixed = TRUE
  )
})


test_that("Kumar-Somanathan transition uses only unique exact district names", {
  carveouts <- data.frame(
    district_1991 = c("Old A", "Old A", "Old B"),
    pop_1991 = c(100, 100, 200),
    district_2001 = c("Alpha", "Beta", "Gamma"),
    pct_01in91 = c(60, 40, 100),
    pct_91in01 = c(100, 100, 100),
    stringsAsFactors = FALSE
  )
  historical <- data.frame(
    state_code_1991 = c("01", "01"),
    district_code_1991 = c("01", "02"),
    district_name_helms_lim = c("Old A", "Old B"),
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = "01",
    district_code = c("01", "02", "03"),
    district_std = c("Alpha", "Beta", "Gamma"),
    stringsAsFactors = FALSE
  )

  out <- build_historical_linguistic_kumar_somanathan_geography(
    carveouts, historical, admin
  )

  expect_true(all(out$edges$transition_status == "matched_exact_names"))
  expect_equal(nrow(out$canonical_transition), 3L)
  expect_true(all(out$canonical_transition$source_coverage == 1))
  expect_true(all(out$canonical_transition$target_coverage == 1))
  expect_equal(out$summary$n_deterministic_components, 2L)
  expect_equal(out$summary$n_nontrivial_deterministic_components, 1L)
})

test_that("Kumar-Somanathan transition fails closed on ambiguous names", {
  carveouts <- data.frame(
    district_1991 = "Old",
    pop_1991 = 100,
    district_2001 = "Alpha",
    pct_01in91 = 100,
    pct_91in01 = 100,
    stringsAsFactors = FALSE
  )
  historical <- data.frame(
    state_code_1991 = c("01", "02"),
    district_code_1991 = c("01", "01"),
    district_name_helms_lim = c("Old", "Old"),
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = "01",
    district_code = "01",
    district_std = "Alpha",
    stringsAsFactors = FALSE
  )

  out <- historical_linguistic_kumar_somanathan_transition(
    carveouts, historical, admin
  )

  expect_identical(
    out$edges$transition_status,
    "source_name_not_unique_exact"
  )
  expect_equal(nrow(out$canonical_transition), 0L)
})

test_that("Kumar-Somanathan reverse margin gates target completeness", {
  carveouts <- data.frame(
    district_1991 = c("Old A", "Old B"),
    pop_1991 = c(100, 200),
    district_2001 = c("Alpha", "Alpha"),
    pct_01in91 = c(100, 100),
    pct_91in01 = c(60, 30),
    stringsAsFactors = FALSE
  )
  historical <- data.frame(
    state_code_1991 = c("01", "01"),
    district_code_1991 = c("01", "02"),
    district_name_helms_lim = c("Old A", "Old B"),
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = "01",
    district_code = "01",
    district_std = "Alpha",
    stringsAsFactors = FALSE
  )

  out <- build_historical_linguistic_kumar_somanathan_geography(
    carveouts, historical, admin
  )

  expect_equal(nrow(out$component_summary), 1L)
  expect_true(out$component_summary$source_coverage_complete)
  expect_false(out$component_summary$target_coverage_complete)
  expect_false(out$component_summary$deterministic_amalgamation_eligible)
  expect_equal(nrow(out$harmonized_crosswalk), 0L)
})

test_that("Kumar-Somanathan transition never fuzzy-matches labels", {
  carveouts <- data.frame(
    district_1991 = "Old A",
    pop_1991 = 100,
    district_2001 = "Alfa",
    pct_01in91 = 100,
    pct_91in01 = 100,
    stringsAsFactors = FALSE
  )
  historical <- data.frame(
    state_code_1991 = "01",
    district_code_1991 = "01",
    district_name_helms_lim = "Old A",
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = "01",
    district_code = "01",
    district_std = "Alpha",
    stringsAsFactors = FALSE
  )

  out <- historical_linguistic_kumar_somanathan_transition(
    carveouts, historical, admin
  )

  expect_identical(
    out$edges$transition_status,
    "target_name_not_unique_exact"
  )
  expect_equal(nrow(out$canonical_transition), 0L)
})


test_that("Kumar-Somanathan exact transition declares population input contract", {
  carveouts <- data.frame(
    district_1991 = "Old A",
    district_2001 = "Alpha",
    pct_01in91 = 100,
    pct_91in01 = 100,
    stringsAsFactors = FALSE
  )
  historical <- data.frame(
    state_code_1991 = "01",
    district_code_1991 = "01",
    district_name_helms_lim = "Old A",
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    state_code = "01",
    district_code = "01",
    district_std = "Alpha",
    stringsAsFactors = FALSE
  )
  expect_error(
    historical_linguistic_kumar_somanathan_transition(
      carveouts, historical, admin
    ),
    "pop_1991"
  )
})

test_that("exact transition comparison keeps evidence sources separate", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = "01",
    source_district_code = "01",
    source_unit_id = "s1",
    target_state_code = "01",
    target_district_code = "01",
    target_unit_id = "t1",
    population_weight = 1,
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "shrug",
    stringsAsFactors = FALSE
  )
  ks <- transition
  ks$evidence_source <- "kumar"
  out <- build_historical_linguistic_exact_transition_comparison(
    list(canonical_transition = transition),
    list(canonical_transition = ks)
  )
  expect_identical(out$summary$reference_label, "shrug_shrid")
  expect_identical(
    out$summary$candidate_label,
    "kumar_somanathan_exact_names"
  )
  expect_equal(out$summary$n_exact_target_set_agreements, 1L)
})
