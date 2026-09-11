mechanism_fixture <- function(n_outcomes) {
  outcomes <- paste0("outcome_", seq_len(n_outcomes))
  grid <- expand.grid(
    outcome_id = outcomes,
    adjustment_id = c("region_main", "state_main"),
    construction_id = c("nonzero_mean", "glottolog_mean", "dyen_noncognate"),
    stringsAsFactors = FALSE
  )
  reduced <- transform(
    grid,
    outcome_variable = outcome_id, mechanism_family = "family", tier = "core",
    denominator = "population", specification_id = paste(adjustment_id, construction_id, sep = "__"),
    fixed_effect = ifelse(adjustment_id == "state_main", "state", "region"),
    term = "ling_distance", estimate = seq_len(nrow(grid)) / 100,
    std.error = 0.01, p.value = 0.20, p_holm_within_spec = 0.40,
    n = 500L, status = "estimated"
  )
  empty <- data.frame()
  list(
    registry = empty, sample_coverage = empty, sample_support = empty,
    first_stage = empty, reduced_form = reduced, weak_iv = empty
  )
}

test_that("Appendix D registered mechanism tables reconcile complete model families", {
  migration <- mechanism_fixture(8L)
  migration$hindi_belt_skilled_migration <- data.frame(
    outcome_id = "skilled_recent_work_migration", estimate = 0.1, std.error = 0.05,
    p.value = 0.1, n = 250L, n_states = 12L, status = "estimated",
    stringsAsFactors = FALSE
  )
  # Rename two fixture outcomes to the semantic D2 outcomes.
  migration$reduced_form$outcome_id[migration$reduced_form$outcome_id == "outcome_1"] <- "skilled_recent_work_migration"
  migration$reduced_form$outcome_id[migration$reduced_form$outcome_id == "outcome_2"] <- "outside_state_recent_work_migration"

  d1 <- appendix_d1_migration(migration)
  expect_equal(nrow(attr(d1, "csv_data")), 48L)
  expect_equal(nrow(appendix_d3_housing_assets(mechanism_fixture(8L))), 48L)
  expect_equal(nrow(appendix_d4_economic_census(mechanism_fixture(6L))), 36L)
  d5 <- appendix_d5_labor(mechanism_fixture(2L), mechanism_fixture(2L), mechanism_fixture(2L))
  expect_equal(nrow(attr(d5, "csv_data")), 36L)
  expect_equal(sort(unique(attr(d5, "csv_data")$source)), sort(c(
    "NSS 2009-10", "PLFS 2017-18 primary", "PLFS 2017-18 conservative lineage"
  )))
  d2 <- appendix_d2_migration_context(migration)
  expect_equal(nrow(attr(d2, "csv_data")), 3L)
})

test_that("Appendix D household and heterogeneity tables preserve bounded registered families", {
  household <- list(estimates = data.frame(
    outcome_id = rep(c("literacy_depth", "matriculate_access", "graduate_access", "female_graduate_access"), each = 2),
    predictor_id = rep(c("linguistic_opportunity", "schooling_exposure"), 4),
    predictor_role = rep(c("instrument", "treatment"), 4), estimand = "descriptive",
    estimate = 0.01, std_error_state_clustered = 0.005, p_value_state_clustered = 0.1,
    p_value_holm_predictor_family = 0.2, n = 355L, n_states = 34L, status = "estimated",
    stringsAsFactors = FALSE
  ))
  d6 <- appendix_d6_household_capacity(household)
  expect_equal(nrow(attr(d6, "csv_data")), 8L)

  social <- list(estimates = data.frame(
    sample = "all_states", social_group = c("Scheduled Tribe", "Scheduled Caste"),
    outcome = "enrollment_rate_0708", estimate = c(-1, 1), std_error_state_clustered = 1,
    p_value_state_clustered = 0.5, p_value_holm_family = 1, n_districts = 300L,
    stringsAsFactors = FALSE
  ))
  st <- list(estimates = data.frame(
    sample = "all_states", outcome_id = "emi_all_children", heterogeneity = "continuous_interaction",
    term = "ling_distance_nonzero_mean:st_share_10pp", estimate = 0.1,
    std_error_state_clustered = 0.1, p_value_state_clustered = 0.5,
    p_value_holm_family = 1, n_districts = 573L, status = "estimated", stringsAsFactors = FALSE
  ))
  d7 <- appendix_d7_social_heterogeneity(social, st)
  expect_equal(nrow(attr(d7, "csv_data")), 3L)
  expect_setequal(unique(attr(d7, "csv_data")$family), c("Social-group gap x distance", "ST concentration"))
})

test_that("Appendix D residual spatial table uses only preferred residual diagnostics", {
  spatial <- data.frame(
    estimand = c("linguistic_distance", "consumption_iv_residual", "consumption_first_stage_residual"),
    estimate = c(0.8, 0.01, -0.01), p.value = c(0.001, 0.4, 0.6), n = 500L,
    contiguity = "rook", weights_style = "W", status = "estimated", stringsAsFactors = FALSE
  )
  d9 <- appendix_d9_residual_spatial_diagnostics(spatial)
  csv <- attr(d9, "csv_data")
  expect_equal(nrow(csv), 2L)
  expect_setequal(csv$estimand, c("consumption_iv_residual", "consumption_first_stage_residual"))
})
