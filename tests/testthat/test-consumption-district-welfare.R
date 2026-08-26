test_that("consumption survey design uses person weights and nested NSS identifiers", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h2", "h3", "h4"),
    source_state_code = c("01", "01", "02", "02"),
    sector = c("Rural", "Rural", "1", "1"), subround = "1",
    fsu = c("1", "2", "1", "2"), stratum = "1", sub_stratum = "1",
    household_size = c(1, 3, 1, 1), target_unit_2001 = c("a", "a", "b", "b"),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = c(1, 3, 1, 1), real_mpce = c(100, 200, 300, 500),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_equal(length(unique(rows$.design_stratum)), 2L)
  expect_equal(length(unique(rows$.design_psu)), 4L)

  out <- estimate_consumption_district_mean(x)
  a <- out[out$district_2001 == "a", ]
  expect_equal(a$estimate, 175, tolerance = 1e-8)
  expect_equal(a$n_households, 2L)
  expect_equal(a$n_fsu, 2L)
  expect_equal(a$kish_effective_n, 16 / 10, tolerance = 1e-8)
  expect_equal(a$status, "estimated")
})

test_that("district support uses allocation-adjusted person weights", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h1", "h2"),
    source_state_code = "01", sector = "1", subround = "1",
    fsu = c("1", "1", "2"), stratum = "1", sub_stratum = "1",
    household_size = c(2, 2, 1), target_unit_2001 = c("a", "b", "a"),
    lineage_status = "resolved_reviewed_consensus", lineage_weight = c(0.5, 0.5, 1),
    lineage_person_weight = c(2, 2, 1), real_mpce = c(100, 100, 200),
    stringsAsFactors = FALSE
  )
  out <- consumption_district_support(x)
  a <- out[out$district_2001 == "a", ]
  expect_equal(a$n_households, 2L)
  expect_equal(a$n_fsu, 2L)
  expect_equal(a$kish_effective_n, 9 / 5, tolerance = 1e-8)
  expect_equal(a$n_sample_person_equiv, 2)
  expect_equal(a$sum_person_weight, 3)
})

test_that("unresolved households never enter the survey design", {
  x <- data.frame(
    survey_id = "wave", household_id = c("resolved", "unresolved"),
    source_state_code = "01", sector = "1", subround = "1",
    fsu = c("1", "2"), stratum = "1", sub_stratum = "1",
    household_size = 1, target_unit_2001 = c("a", NA),
    lineage_status = c("resolved_exact_2001", "unresolved_no_stable_lineage"), lineage_weight = c(1, NA),
    lineage_person_weight = c(1, NA), real_mpce = c(100, 999),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_equal(rows$household_id, "resolved")
  expect_equal(rows$real_mpce, 100)
})

test_that("Round 66 urban blank sub-strata represent no sub-stratification", {
  x <- data.frame(
    survey_id = "nss_2009_10_type1", household_id = c("u1", "u2"),
    source_state_code = "07", sector = "Urban", subround = "1",
    fsu = c("1", "2"), stratum = "10", sub_stratum = c("", NA),
    household_size = 1, target_unit_2001 = "delhi",
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = c(100, 200),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_identical(unique(rows$.design_sub_stratum), "__none__")
  expect_equal(length(unique(rows$.design_stratum)), 1L)
})

test_that("rural blank sub-strata remain invalid design data", {
  x <- data.frame(
    survey_id = "nss_2009_10_type1", household_id = "r1",
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = "1", stratum = "1", sub_stratum = "", household_size = 1,
    target_unit_2001 = "a", lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = 100, stringsAsFactors = FALSE
  )
  expect_error(consumption_design_rows(x), "sub_stratum=1")
})

test_that("district means intentionally handle lonely-PSU survey warnings", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h2"),
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = c("1", "2"), stratum = c("1", "2"), sub_stratum = "1",
    household_size = 1, target_unit_2001 = c("a", "a"),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = c(100, 200), stringsAsFactors = FALSE
  )
  expect_warning(out <- estimate_consumption_district_mean(x), NA)
  expect_equal(out$estimate, 150)
})

test_that("welfare registry keeps thin quantile points but gates uncertainty on support", {
  registry <- data.frame(
    outcome_id = c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce"),
    estimand = c("survey_mean", "survey_mean", "survey_quantile"),
    transform = c("identity", "log", "identity"), quantile = c(NA, NA, 0.5),
    quantile_interval = c("", "", "xlogit"), quantile_rule = c("", "", "math"),
    role = c("primary", "robustness", "robustness"), min_households = 4,
    min_fsu = 2, min_kish_effective_n = 1, max_relative_se = c(0.50, NA, 0.50),
    stringsAsFactors = FALSE
  )
  x <- data.frame(
    survey_id = "wave", household_id = paste0("h", 1:10), source_state_code = "01",
    sector = "Rural", subround = "1", fsu = as.character(1:10), stratum = "1",
    sub_stratum = "1", household_size = 1,
    target_unit_2001 = c(rep("a", 8), rep("b", 2)),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1,
    real_mpce = c(100, 100, 150, 150, 200, 200, 250, 250, 300, 500),
    stringsAsFactors = FALSE
  )
  expect_warning(out <- estimate_consumption_district_welfare(x, registry), NA)
  expect_setequal(out$outcome_id, registry$outcome_id)

  mean_a <- out[out$outcome_id == "real_mean_mpce" & out$district_2001 == "a", ]
  log_a <- out[out$outcome_id == "mean_log_real_mpce" & out$district_2001 == "a", ]
  median_a <- out[out$outcome_id == "weighted_median_real_mpce" & out$district_2001 == "a", ]
  median_b <- out[out$outcome_id == "weighted_median_real_mpce" & out$district_2001 == "b", ]
  expect_equal(mean_a$estimate, 175, tolerance = 1e-8)
  expect_equal(
    log_a$estimate,
    mean(log(c(100, 100, 150, 150, 200, 200, 250, 250))),
    tolerance = 1e-8
  )

  rows <- consumption_design_rows(x[x$target_unit_2001 == "a", , drop = FALSE])
  design <- consumption_survey_design_from_rows(rows)
  expect_warning(direct_median <- with_consumption_survey_adjustment(survey::svyquantile(
    ~real_mpce, design, quantiles = 0.5, ci = TRUE, interval.type = "xlogit",
    qrule = "math", na.rm = TRUE
  )), NA)
  expect_equal(median_a$estimate, unname(stats::coef(direct_median))[[1L]], tolerance = 1e-8)
  expect_equal(median_a$std_error, unname(survey::SE(direct_median))[[1L]], tolerance = 1e-8)
  expect_true(median_a$uncertainty_requested)
  expect_true(median_a$sample_support_ok)
  expect_equal(median_a$status, "estimated")
  expect_true(is.na(median_a$cv))

  expect_true(is.finite(median_b$estimate))
  expect_true(is.na(median_b$std_error))
  expect_false(median_b$uncertainty_requested)
  expect_false(median_b$sample_support_ok)
  expect_false(median_b$preferred_eligible)
  expect_equal(median_b$status, "point_estimate_only")
  expect_equal(median_b$reason, "uncertainty_not_requested_thin_support")
  expect_true(grepl("thin_household_sample", median_b$support_reason, fixed = TRUE))
  expect_true(is.na(median_b$precision_ok))
  expect_true(is.na(log_a$cv))
})


test_that("quantile estimator handles all-supported and all-thin domain partitions", {
  registry <- data.frame(
    outcome_id = "weighted_median_real_mpce", estimand = "survey_quantile",
    transform = "identity", quantile = 0.5, quantile_interval = "xlogit",
    quantile_rule = "math", role = "robustness", min_households = 2,
    min_fsu = 2, min_kish_effective_n = 1, max_relative_se = 0.5,
    stringsAsFactors = FALSE
  )
  x <- data.frame(
    survey_id = "wave", household_id = paste0("h", 1:8), source_state_code = "01",
    sector = "Rural", subround = "1", fsu = as.character(1:8), stratum = "1",
    sub_stratum = "1", household_size = 1,
    target_unit_2001 = rep(c("a", "b"), each = 4),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = seq(100, 800, by = 100),
    stringsAsFactors = FALSE
  )

  supported <- estimate_consumption_district_welfare(x, registry)
  expect_equal(nrow(supported), 2L)
  expect_true(all(supported$uncertainty_requested))
  expect_true(all(supported$status == "estimated"))

  registry$min_households <- 10
  expect_warning(thin <- estimate_consumption_district_welfare(x, registry), NA)
  expect_equal(nrow(thin), 2L)
  expect_true(all(is.finite(thin$estimate)))
  expect_true(all(is.na(thin$std_error)))
  expect_true(all(!thin$uncertainty_requested))
  expect_true(all(thin$status == "point_estimate_only"))
})


test_that("consumption welfare registry validates outcome contracts", {
  path <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    outcome_id = c(
      "real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce",
      "bottom20_mean_real_mpce"
    ),
    estimand = c("survey_mean", "survey_mean", "survey_quantile", "survey_bottom_mean"),
    transform = c("identity", "log", "identity", "identity"),
    quantile = c(NA, NA, 0.5, 0.2),
    quantile_interval = c("", "", "xlogit", ""),
    quantile_rule = c("", "", "math", ""),
    role = c("primary", "robustness", "robustness", "robustness"),
    min_households = c(50, 50, 50, 100),
    min_fsu = 2,
    min_kish_effective_n = c(20, 20, 20, 100),
    max_relative_se = c(0.2, NA, 0.2, 0.25), stringsAsFactors = FALSE
  ), path, row.names = FALSE, na = "")
  out <- read_consumption_welfare_outcomes(path)
  expect_equal(out$outcome_id, c(
    "real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce",
    "bottom20_mean_real_mpce"
  ))
  expect_true(is.na(out$max_relative_se[[2L]]))
  expect_equal(out$quantile[[3L]], 0.5)
  expect_equal(out$quantile_interval[[3L]], "xlogit")
  expect_equal(out$quantile_rule[[3L]], "math")
  expect_equal(out$estimand[[4L]], "survey_bottom_mean")
  expect_equal(out$quantile[[4L]], 0.2)

  bad_quantile <- out
  bad_quantile$quantile[[3L]] <- 1
  write.csv(bad_quantile, path, row.names = FALSE, na = "")
  expect_error(read_consumption_welfare_outcomes(path), "invalid quantile declarations")

  bad_interval <- out
  bad_interval$quantile_interval[[3L]] <- "unknown"
  write.csv(bad_interval, path, row.names = FALSE, na = "")
  expect_error(read_consumption_welfare_outcomes(path), "invalid quantile uncertainty declarations")

  bad_bottom_method <- out
  bad_bottom_method$quantile_rule[[4L]] <- "math"
  write.csv(bad_bottom_method, path, row.names = FALSE, na = "")
  expect_error(
    read_consumption_welfare_outcomes(path),
    "invalid quantile uncertainty declarations"
  )

  bad <- out
  bad$outcome_id[[2L]] <- bad$outcome_id[[1L]]
  write.csv(bad, path, row.names = FALSE, na = "")
  expect_error(read_consumption_welfare_outcomes(path), "unique outcome_id")
})

test_that("district welfare changes preserve common-support and directional change semantics", {
  make_round <- function(round_id, outcome_id, estimate, eligible) {
    data.frame(
      district_2001 = letters[1:3],
      round_id = round_id,
      outcome_id = outcome_id,
      estimate = estimate,
      preferred_eligible = eligible,
      n_households = c(100, 90, 80),
      n_fsu = c(10, 9, 8),
      kish_effective_n = c(80, 70, 60),
      stringsAsFactors = FALSE
    )
  }
  welfare <- rbind(
    make_round("left", "real_mean_mpce", c(100, 200, NA), c(TRUE, TRUE, TRUE)),
    make_round("right", "real_mean_mpce", c(110, 180, 300), c(TRUE, FALSE, TRUE))
  )
  outcomes <- data.frame(
    outcome_id = "real_mean_mpce", transform = "identity",
    stringsAsFactors = FALSE
  )
  comparison <- data.frame(
    comparison_id = "left_vs_right",
    left_round = "left", right_round = "right",
    comparison_family = "test", stringsAsFactors = FALSE
  )

  out <- consumption_welfare_pair_rows(welfare, outcomes, comparison)
  a <- out[out$district_2001 == "a", , drop = FALSE]
  b <- out[out$district_2001 == "b", , drop = FALSE]
  c <- out[out$district_2001 == "c", , drop = FALSE]

  expect_equal(a$absolute_change, 10)
  expect_equal(a$proportional_change, 0.1, tolerance = 1e-12)
  expect_true(a$finite_common)
  expect_true(a$preferred_common)

  expect_equal(b$absolute_change, -20)
  expect_equal(b$proportional_change, -0.1, tolerance = 1e-12)
  expect_true(b$finite_common)
  expect_false(b$preferred_common)

  expect_false(c$finite_common)
  expect_false(c$preferred_common)
  expect_true(is.na(c$proportional_change))
})

test_that("comparability summary is derived from the district welfare-change object", {
  make <- function(round_id, estimate) {
    data.frame(
      district_2001 = letters[1:3],
      round_id = round_id,
      outcome_id = "real_mean_mpce",
      estimate = estimate,
      preferred_eligible = TRUE,
      n_households = 100,
      n_fsu = 10,
      kish_effective_n = 80,
      stringsAsFactors = FALSE
    )
  }
  welfare <- rbind(make("left", c(100, 200, 300)), make("right", c(110, 220, 330)))
  outcomes <- data.frame(
    outcome_id = "real_mean_mpce", transform = "identity",
    stringsAsFactors = FALSE
  )
  comparisons <- data.frame(
    comparison_id = "left_vs_right",
    left_round = "left", right_round = "right",
    comparison_family = "test", stringsAsFactors = FALSE
  )

  changes <- build_consumption_welfare_changes(welfare, outcomes, comparisons)
  summary_from_changes <- summarize_consumption_welfare_pair(changes)
  summary_direct <- compare_consumption_welfare(
    welfare, outcomes, comparisons
  )

  expect_equal(summary_direct, summary_from_changes, ignore_attr = TRUE)
})

test_that("welfare comparability uses preferred common support and directional level changes", {
  make_round <- function(round_id, outcome_id, estimate, eligible) {
    data.frame(
      district_2001 = letters[1:4],
      round_id = round_id,
      outcome_id = outcome_id,
      estimate = estimate,
      preferred_eligible = eligible,
      n_households = c(100, 90, 80, 20),
      n_fsu = c(10, 9, 8, 2),
      kish_effective_n = c(80, 70, 60, 10),
      stringsAsFactors = FALSE
    )
  }
  welfare <- rbind(
    make_round("left", "real_mean_mpce", c(100, 200, 300, 900), c(TRUE, TRUE, TRUE, FALSE)),
    make_round("right", "real_mean_mpce", c(110, 220, 330, 100), c(TRUE, TRUE, TRUE, FALSE))
  )
  outcomes <- data.frame(
    outcome_id = "real_mean_mpce", transform = "identity",
    stringsAsFactors = FALSE
  )
  comparison <- data.frame(
    comparison_id = "left_vs_right",
    left_round = "left",
    right_round = "right",
    comparison_family = "test",
    stringsAsFactors = FALSE
  )

  out <- compare_consumption_welfare_pair(welfare, outcomes, comparison)
  expect_equal(out$common_districts, 4L)
  expect_equal(out$common_preferred_districts, 3L)
  expect_equal(out$comparison_districts, 3L)
  expect_equal(out$comparison_basis, "preferred_common_support")
  expect_equal(out$pearson_correlation, 1, tolerance = 1e-12)
  expect_equal(out$spearman_correlation, 1, tolerance = 1e-12)
  expect_equal(out$median_absolute_difference, 20)
  expect_equal(out$median_proportional_change, 0.1, tolerance = 1e-12)
  expect_equal(out$status, "estimated_preferred_common_support")
})

test_that("welfare comparability does not treat log-scale differences as proportional changes", {
  make <- function(round_id, estimate) {
    data.frame(
      district_2001 = letters[1:3],
      round_id = round_id,
      outcome_id = "mean_log_real_mpce",
      estimate = estimate,
      preferred_eligible = TRUE,
      n_households = 100,
      n_fsu = 10,
      kish_effective_n = 80,
      stringsAsFactors = FALSE
    )
  }
  welfare <- rbind(make("left", c(7, 8, 9)), make("right", c(7.1, 8.1, 9.1)))
  outcomes <- data.frame(
    outcome_id = "mean_log_real_mpce", transform = "log",
    stringsAsFactors = FALSE
  )
  comparison <- data.frame(
    comparison_id = "left_vs_right",
    left_round = "left", right_round = "right",
    comparison_family = "test", stringsAsFactors = FALSE
  )

  out <- compare_consumption_welfare_pair(welfare, outcomes, comparison)
  expect_true(is.na(out$median_proportional_change))
  expect_equal(out$median_absolute_difference, 0.1, tolerance = 1e-12)
})

test_that("welfare comparison registry validates unique directional round pairs", {
  path <- tempfile(fileext = ".csv")
  good <- data.frame(
    comparison_id = "a_vs_b",
    left_round = "a",
    right_round = "b",
    comparison_family = "test",
    stringsAsFactors = FALSE
  )
  utils::write.csv(good, path, row.names = FALSE, na = "")
  expect_equal(read_consumption_welfare_comparisons(path), good, ignore_attr = TRUE)

  bad <- good
  bad$right_round <- "a"
  utils::write.csv(bad, path, row.names = FALSE, na = "")
  expect_error(
    read_consumption_welfare_comparisons(path),
    "invalid round pairs"
  )
})

test_that("welfare comparability reports insufficient common support without failing", {
  welfare <- data.frame(
    district_2001 = rep(c("a", "b"), 2),
    round_id = rep(c("left", "right"), each = 2),
    outcome_id = "real_mean_mpce",
    estimate = c(100, 200, 110, 210),
    preferred_eligible = FALSE,
    n_households = 20,
    n_fsu = 2,
    kish_effective_n = 10,
    stringsAsFactors = FALSE
  )
  outcomes <- data.frame(
    outcome_id = "real_mean_mpce", transform = "identity",
    stringsAsFactors = FALSE
  )
  comparison <- data.frame(
    comparison_id = "left_vs_right",
    left_round = "left", right_round = "right",
    comparison_family = "test", stringsAsFactors = FALSE
  )

  out <- compare_consumption_welfare_pair(welfare, outcomes, comparison)
  expect_equal(out$comparison_districts, 2L)
  expect_equal(out$status, "insufficient_common_support")
  expect_true(is.na(out$pearson_correlation))
  expect_true(is.na(out$spearman_correlation))
})

test_that("quantile adjustment muffles only documented non-finite interval warnings", {
  expect_warning(
    value <- with_consumption_quantile_adjustment({
      warning("NaNs produced")
      7
    }),
    NA
  )
  expect_equal(value, 7)

  expect_warning(
    with_consumption_quantile_adjustment({
      warning("unexpected quantile warning")
      7
    }),
    "unexpected quantile warning"
  )
})

test_that("finite welfare points with failed design uncertainty remain explicit point estimates", {
  estimates <- data.frame(
    district_2001 = "a",
    round_id = "hces_2023_24",
    outcome_id = "weighted_median_real_mpce",
    estimate = 2000,
    std_error = NaN,
    uncertainty_requested = TRUE,
    stringsAsFactors = FALSE
  )
  support <- data.frame(
    district_2001 = "a",
    n_households = 100L,
    n_fsu = 5L,
    n_sample_person_equiv = 400,
    sum_person_weight = 1000,
    kish_effective_n = 60,
    stringsAsFactors = FALSE
  )
  rule <- data.frame(
    outcome_id = "weighted_median_real_mpce",
    min_households = 50,
    min_fsu = 2,
    min_kish_effective_n = 20,
    max_relative_se = 0.20,
    stringsAsFactors = FALSE
  )

  out <- consumption_finalize_district_estimate(
    estimates, support, rule, cv_applicable = FALSE
  )

  expect_equal(out$status, "point_estimate_only")
  expect_equal(out$reason, "non_finite_design_uncertainty")
  expect_true(out$uncertainty_requested)
  expect_true(out$sample_support_ok)
  expect_false(out$preferred_eligible)
  expect_true(is.na(out$std_error))
  expect_true(is.na(out$precision_ok))
  expect_true(is.na(out$support_reason))
})

test_that("non-finite welfare points remain not estimable", {
  estimates <- data.frame(
    district_2001 = "a",
    round_id = "hces_2023_24",
    outcome_id = "weighted_median_real_mpce",
    estimate = NaN,
    std_error = NaN,
    uncertainty_requested = TRUE,
    stringsAsFactors = FALSE
  )
  support <- data.frame(
    district_2001 = "a",
    n_households = 100L,
    n_fsu = 5L,
    n_sample_person_equiv = 400,
    sum_person_weight = 1000,
    kish_effective_n = 60,
    stringsAsFactors = FALSE
  )
  rule <- data.frame(
    outcome_id = "weighted_median_real_mpce",
    min_households = 50,
    min_fsu = 2,
    min_kish_effective_n = 20,
    max_relative_se = 0.20,
    stringsAsFactors = FALSE
  )

  out <- consumption_finalize_district_estimate(
    estimates, support, rule, cv_applicable = FALSE
  )
  expect_equal(out$status, "not_estimable")
  expect_equal(out$reason, "non_finite_point_estimate")
  expect_false(out$preferred_eligible)
})

test_that("finite high relative SE is distinguished from unavailable precision", {
  estimates <- data.frame(
    district_2001 = c("high_rse", "no_se"),
    round_id = "hces_2023_24",
    outcome_id = "real_mean_mpce",
    estimate = c(100, 100),
    std_error = c(30, NA_real_),
    uncertainty_requested = TRUE,
    stringsAsFactors = FALSE
  )
  support <- data.frame(
    district_2001 = c("high_rse", "no_se"),
    n_households = 100L,
    n_fsu = 5L,
    n_sample_person_equiv = 400,
    sum_person_weight = 1000,
    kish_effective_n = 60,
    stringsAsFactors = FALSE
  )
  rule <- data.frame(
    outcome_id = "real_mean_mpce",
    min_households = 50,
    min_fsu = 2,
    min_kish_effective_n = 20,
    max_relative_se = 0.20,
    stringsAsFactors = FALSE
  )

  out <- consumption_finalize_district_estimate(
    estimates, support, rule, cv_applicable = TRUE
  )

  high <- out[out$district_2001 == "high_rse", , drop = FALSE]
  missing <- out[out$district_2001 == "no_se", , drop = FALSE]

  expect_false(high$precision_ok)
  expect_equal(high$status, "estimated")
  expect_equal(high$support_reason, "high_relative_se")
  expect_false(high$preferred_eligible)

  expect_true(is.na(missing$precision_ok))
  expect_equal(missing$status, "point_estimate_only")
  expect_equal(missing$reason, "non_finite_design_uncertainty")
  expect_true(is.na(missing$support_reason))
  expect_false(missing$preferred_eligible)
})

test_that("bottom-share mean uses convey lower-tail linearization", {
  values <- 1:10
  design <- survey::svydesign(
    ids = ~1,
    weights = ~weight,
    data = data.frame(
      .welfare_value = values,
      weight = 1
    )
  )
  design <- convey::convey_prep(design)

  bottom20 <- consumption_bottom_mean_stat(design, 0.2)
  bottom40 <- consumption_bottom_mean_stat(design, 0.4)

  expect_equal(bottom20[["estimate"]], mean(1:2), tolerance = 1e-10)
  expect_equal(bottom40[["estimate"]], mean(1:4), tolerance = 1e-10)
  expect_true(is.finite(bottom20[["std_error"]]))
  expect_true(is.finite(bottom40[["std_error"]]))
  expect_gt(bottom20[["std_error"]], 0)
  expect_gt(bottom40[["std_error"]], 0)
})

test_that("bottom-share welfare retains district support and precision contracts", {
  x <- data.frame(
    survey_id = "wave",
    household_id = paste0("h", 1:20),
    source_state_code = "01",
    sector = "Rural",
    subround = "1",
    fsu = as.character(1:20),
    stratum = "1",
    sub_stratum = "1",
    household_size = 1,
    target_unit_2001 = rep(c("a", "b"), each = 10),
    lineage_status = "resolved_exact_2001",
    lineage_weight = 1,
    lineage_person_weight = 1,
    real_mpce = c(1:10, 11:20) * 100,
    stringsAsFactors = FALSE
  )
  registry <- data.frame(
    outcome_id = c("bottom40_mean_real_mpce", "bottom20_mean_real_mpce"),
    estimand = "survey_bottom_mean",
    transform = "identity",
    quantile = c(0.4, 0.2),
    quantile_interval = "",
    quantile_rule = "",
    role = "robustness",
    min_households = 5,
    min_fsu = 2,
    min_kish_effective_n = 5,
    max_relative_se = 10,
    stringsAsFactors = FALSE
  )

  out <- estimate_consumption_district_welfare(x, registry)

  expect_equal(nrow(out), 4L)
  expect_true(all(out$status == "estimated"))
  expect_true(all(out$uncertainty_requested))
  expect_true(all(out$sample_support_ok))
  expect_true(all(out$preferred_eligible))
  expect_true(all(is.finite(out$estimate)))
  expect_true(all(is.finite(out$std_error)))
  expect_true(all(is.finite(out$relative_se)))
  expect_true(all(is.finite(out$cv)))
  expect_true(all(
    out$estimate[out$outcome_id == "bottom20_mean_real_mpce"] <=
      out$estimate[out$outcome_id == "bottom40_mean_real_mpce"]
  ))
})

test_that("bottom-share welfare rejects transformed tail means", {
  x <- data.frame(
    survey_id = "wave", household_id = paste0("h", 1:4),
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = as.character(1:4), stratum = "1", sub_stratum = "1",
    household_size = 1, target_unit_2001 = "a",
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = 1:4 * 100,
    stringsAsFactors = FALSE
  )
  rule <- data.frame(
    outcome_id = "bad_tail", estimand = "survey_bottom_mean",
    transform = "log", quantile = 0.2, quantile_interval = "",
    quantile_rule = "", role = "robustness", min_households = 1,
    min_fsu = 1, min_kish_effective_n = 1, max_relative_se = 1,
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  design <- consumption_survey_design_from_rows(rows)
  support <- consumption_district_support_from_rows(rows)

  expect_error(
    estimate_consumption_district_bottom_mean(rows, design, support, rule),
    "identity MPCE transform"
  )
})
