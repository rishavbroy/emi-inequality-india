mechanism_result_fixture <- function() {
  registry <- data.frame(
    outcome_id = c("a", "b"), source_id = "source", variable = c("a", "b"),
    mechanism_family = c("family_a", "family_b"), tier = "core", denominator = "people",
    stringsAsFactors = FALSE
  )
  reduced_form <- data.frame(
    outcome_id = c("a", "b"), outcome_variable = c("a", "b"),
    mechanism_family = c("family_a", "family_b"), tier = "core", denominator = "people",
    specification_id = "state_main__nonzero_mean", adjustment_id = "state_main",
    construction_id = "nonzero_mean", fixed_effect = "state",
    term = "ling_distance_nonzero_mean", estimate = c(0.1, 0.2), std.error = 0.1,
    statistic = c(1, 2), p.value = c(0.04, 0.4), p_holm_within_spec = c(0.08, 0.4),
    n = 100L, status = "estimated", reason = NA_character_, stringsAsFactors = FALSE
  )
  weak_iv <- data.frame(
    outcome_id = c("a", "b"), outcome_variable = c("a", "b"),
    mechanism_family = c("family_a", "family_b"), tier = "core", denominator = "people",
    specification_id = "state_main__nonzero_mean", adjustment_id = "state_main",
    construction_id = "nonzero_mean", fixed_effect = "state",
    estimate_2sls = c(1, 2), std_error_clustered = 1, p_value_clustered = c(0.2, 0.3),
    p_value_clustered_holm_within_spec = c(0.4, 0.4),
    effective_f = c(5, 30), effective_f_critical_value = c(23, 23),
    effective_f_p_value = c(0.8, 0.01), effective_f_df = 1,
    reduced_form_joint_f = c(1, 4), reduced_form_joint_p = c(0.4, 0.05),
    anderson_rubin_f_beta0 = c(1, 5), anderson_rubin_p_beta0 = c(0.4, 0.01),
    anderson_rubin_p_beta0_holm_within_spec = c(0.4, 0.02),
    ar_95_lower = c(NA, 0.1), ar_95_upper = c(NA, 2), ar_95_empty = FALSE,
    ar_95_n_components = 1L, ar_95_disconnected = FALSE,
    ar_95_contains_zero = c(TRUE, FALSE), ar_95_grid_accepted_min = c(-10, 0.1),
    ar_95_grid_accepted_max = c(10, 2), ar_95_left_truncated = c(TRUE, FALSE),
    ar_95_right_truncated = c(TRUE, FALSE), ar_95_components = c("grid", "[0.1,2]"),
    n = 100L, status = "estimated", reason = NA_character_, stringsAsFactors = FALSE
  )
  list(
    registry = registry,
    sample_coverage = data.frame(n_common_analysis_districts = 100L),
    sample_support = data.frame(target_unit_2001 = sprintf("d%03d", 1:2)),
    first_stage = data.frame(specification_id = "state_main__nonzero_mean", n = 100L),
    reduced_form = reduced_form,
    weak_iv = weak_iv,
    anderson_rubin_grid = data.frame(beta = -2:2, p.value = seq(0.1, 0.5, 0.1))
  )
}

test_that("post-treatment mechanism saver retains AR grids only as cached objects", {
  result <- mechanism_result_fixture()
  dir <- tempfile("mechanism-retention-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  stale <- file.path(dir, "mechanism_anderson_rubin_grid.csv")
  writeLines("stale", stale)

  paths <- save_posttreatment_mechanism_outputs(result, dir)

  expect_equal(length(paths), 6L)
  expect_true(all(file.exists(paths)))
  expect_false(file.exists(stale))
  expect_equal(nrow(result$anderson_rubin_grid), 5L)
  expect_setequal(
    basename(paths),
    paste0("mechanism_", posttreatment_mechanism_persisted_components(), ".csv")
  )
})

test_that("cross-family mechanism evidence distinguishes identification from robust signal", {
  result <- mechanism_result_fixture()
  evidence <- build_posttreatment_mechanism_evidence(list(
    example = list(
      result = result,
      temporal_role = "long_run_post",
      analysis_role = "causal_mechanism"
    )
  ))

  expect_equal(nrow(evidence$grid), 2L)
  expect_identical(
    evidence$grid$evidence_status,
    c("weak_iv_underidentified", "weak_iv_robust_signal")
  )
  expect_identical(evidence$grid$first_stage_strong, c(FALSE, TRUE))
  expect_identical(evidence$grid$ar_95_bounded, c(FALSE, TRUE))
  expect_false(evidence$grid$reduced_form_holm_signal[[1L]])
  expect_equal(evidence$family_summary$n_models, 2L)
  expect_equal(evidence$family_summary$n_outcomes, 2L)
  expect_equal(evidence$family_summary$n_strong_first_stage, 1L)
  expect_equal(evidence$family_summary$n_ar_holm_signals, 1L)
  expect_equal(evidence$family_summary$n_underidentified, 1L)
})

test_that("flattened domain diagnostics use the same mechanism contract", {
  result <- mechanism_result_fixture()
  flattened <- c(
    list(measurement = data.frame(x = 1)),
    stats::setNames(
      result[posttreatment_mechanism_persisted_components()],
      paste0("mechanism_", posttreatment_mechanism_persisted_components())
    ),
    list(mechanism_anderson_rubin_grid = result$anderson_rubin_grid)
  )
  extracted <- extract_posttreatment_mechanism_result(flattened)
  expect_equal(extracted$weak_iv, result$weak_iv)
  expect_equal(extracted$anderson_rubin_grid, result$anderson_rubin_grid)
})

test_that("mechanism evidence summary reports optional unavailable families without failing", {
  result <- mechanism_result_fixture()
  evidence <- build_posttreatment_mechanism_evidence(list(
    available = list(result = result, temporal_role = "post"),
    unavailable = list(result = NULL, temporal_role = "post")
  ))
  unavailable <- evidence$family_summary[evidence$family_summary$family == "unavailable", , drop = FALSE]
  expect_identical(unavailable$availability_status[[1L]], "not_available")
  expect_equal(unavailable$n_models[[1L]], 0L)
  expect_false("unavailable" %in% evidence$grid$family)
})
