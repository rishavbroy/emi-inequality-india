make_district_mechanism_fixture <- function() {
  set.seed(2026)
  n_states <- 6L
  per_state <- 8L
  n <- n_states * per_state
  state <- rep(sprintf("%02d", seq_len(n_states)), each = per_state)
  distance <- rep(seq(0.2, 1.6, length.out = per_state), n_states) +
    stats::rnorm(n, sd = 0.03)
  out <- data.frame(
    target_unit_2001 = paste0("pc2001__", state, "__", sprintf("%02d", rep(seq_len(per_state), n_states))),
    state_code_2001 = state,
    region = rep(panel_region_levels(), length.out = n),
    ling_distance_nonzero_mean = distance,
    enrollment_rate_0708 = 70 + 4 * distance + stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  for (control in census_2001_main_controls()) {
    out[[control]] <- stats::rnorm(n)
  }
  out
}

test_that("district mechanism grid is limited to the predeclared geography adjustments", {
  registry <- district_mechanism_adjustment_registry()

  expect_identical(
    registry$specification_id,
    c("unadjusted", "region_main", "state_main")
  )
  expect_identical(registry$fixed_effect, c("none", "region", "state"))
  expect_length(registry$controls[[1L]], 0L)
  expect_identical(registry$controls[[2L]], census_2001_main_controls())
  expect_identical(registry$controls[[3L]], census_2001_main_controls())
})

test_that("district mechanism estimates hold the outcome sample fixed across geography adjustments", {
  panel <- make_district_mechanism_fixture()
  measure <- data.frame(
    measure_id = "nss_enrollment",
    variable = "enrollment_rate_0708",
    source = "nss_64_education",
    stage = "schooling_access",
    source_side = "household_realized",
    paper_role = "schooling_access",
    interpretation = "Realized enrollment",
    stringsAsFactors = FALSE
  )
  out <- estimate_district_mechanism_grid(panel, measure)

  expect_equal(nrow(out), 3L)
  expect_length(unique(out$n), 1L)
  expect_true(all(out$status == "estimated"))
  expect_true(all(is.finite(out$partial_r_squared)))
  expect_equal(
    out$standardized_estimate^2,
    out$partial_r_squared,
    tolerance = 1e-10
  )
})

test_that("district mechanism reporting uses only preferred district measures", {
  panel <- make_district_mechanism_fixture()
  registry <- data.frame(
    measure_id = c("nss_enrollment", "c17_language", "nss_not_preferred"),
    variable = c("enrollment_rate_0708", "english_share_multilingual", "unused"),
    source = c("nss_64_education", "census_2001_c17", "nss_64_education"),
    unit = c("district", "state_language", "district"),
    stage = c("schooling_access", "capability_acquisition", "schooling_access"),
    source_side = c("household_realized", "linguistic_behavior", "household_realized"),
    paper_role = c("schooling_access", "linguistic_behavioral_mechanism", "schooling_access"),
    interpretation = c("Enrollment", "English acquisition", "Unused"),
    preferred = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  out <- diagnose_english_opportunity_district_mechanisms(panel, registry)

  expect_identical(out$measures$measure_id, "nss_enrollment")
  expect_equal(nrow(out$estimates), 3L)
  expect_identical(unique(out$estimates$measure_id), "nss_enrollment")
})

make_english_opportunity_reporting_fixture <- function() {
  panel <- make_district_mechanism_fixture()
  district_registry <- data.frame(
    measure_id = "nss_enrollment",
    variable = "enrollment_rate_0708",
    source = "nss_64_education",
    unit = "district",
    stage = "schooling_access",
    source_side = "household_realized",
    paper_role = "schooling_access",
    interpretation = "Realized enrollment",
    preferred = TRUE,
    stringsAsFactors = FALSE
  )
  district <- diagnose_english_opportunity_district_mechanisms(panel, district_registry)
  c17 <- list(
    registry = data.frame(
      specification_id = "english_linear",
      outcome = "english_share_multilingual",
      preferred = TRUE,
      stringsAsFactors = FALSE
    ),
    coefficients = data.frame(
      specification_id = "english_linear",
      term = "shastry_degree",
      signed_partial_correlation = 0.4,
      partial_r_squared = 0.16,
      status = "estimated",
      stringsAsFactors = FALSE
    ),
    model_summary = data.frame(
      specification_id = "english_linear",
      n = 120,
      stringsAsFactors = FALSE
    )
  )
  registry <- safe_bind_rows(list(
    data.frame(
      measure_id = "c17_english_multilingual",
      variable = "english_share_multilingual",
      source = "census_2001_c17",
      unit = "state_language",
      stage = "capability_acquisition",
      source_side = "linguistic_behavior",
      paper_role = "linguistic_behavioral_mechanism",
      interpretation = "English acquisition among multilingual speakers",
      preferred = TRUE,
      stringsAsFactors = FALSE
    ),
    district_registry
  ))
  list(c17 = c17, district = district, registry = registry)
}

test_that("mechanism figure combines C-17 and district evidence without mixing observational units", {
  fixture <- make_english_opportunity_reporting_fixture()
  out <- english_opportunity_mechanism_figure_data(
    fixture$c17, fixture$district, fixture$registry
  )

  expect_equal(nrow(out), 4L)
  expect_equal(sum(out$measure_id == "c17_english_multilingual"), 1L)
  expect_identical(
    out$geography[out$measure_id == "c17_english_multilingual"],
    "Within state"
  )
  expect_equal(
    out$signal[out$measure_id == "c17_english_multilingual"],
    0.4
  )
  district_rows <- out$measure_id == "nss_enrollment"
  expect_setequal(
    out$geography[district_rows],
    unname(english_opportunity_mechanism_geography_labels())
  )
  expect_false(anyDuplicated(out[c("measure_id", "geography_id")]) > 0L)
})

test_that("mechanism table is a compact wide view of the validated signal data", {
  fixture <- make_english_opportunity_reporting_fixture()
  out <- english_opportunity_mechanism_table(
    fixture$c17, fixture$district, fixture$registry
  )

  expect_identical(
    names(out),
    c("measure_id", "source", "stage", "interpretation", "Unadjusted", "Region + controls", "Within state")
  )
  expect_equal(nrow(out), 2L)
  c17_row <- out$measure_id == "c17_english_multilingual"
  expect_true(is.na(out$Unadjusted[c17_row]))
  expect_true(is.na(out[["Region + controls"]][c17_row]))
  expect_equal(out[["Within state"]][c17_row], 0.4)
  district_row <- out$measure_id == "nss_enrollment"
  district_signals <- unlist(
    out[district_row, c("Unadjusted", "Region + controls", "Within state")]
  )
  expect_true(all(is.finite(district_signals)))
})

test_that("ST-concentration heterogeneity family stays small and predetermined", {
  registry <- english_opportunity_st_heterogeneity_registry()
  expect_identical(
    registry$outcome,
    c(
      "emi_exposure_all_children_0708",
      "private_emi_exposure_all_children_0708",
      "dise_girls_toilet_school_share_0708"
    )
  )
  controls <- english_opportunity_st_heterogeneity_controls()
  expect_false("st_share_2001" %in% controls)
  expect_setequal(c(controls, "st_share_2001"), census_2001_main_controls())
})

test_that("high-ST sensitivity cutoff is fixed from the full district panel", {
  panel <- data.frame(st_share_2001 = c(0, 10, 20, 30, 40), stringsAsFactors = FALSE)
  expect_equal(
    english_opportunity_high_st_cutoff(panel),
    unname(stats::quantile(panel$st_share_2001, 0.75, names = FALSE, type = 7))
  )
  expect_error(
    english_opportunity_high_st_cutoff(data.frame(x = 1)),
    "requires st_share_2001"
  )
})

test_that("continuous ST-share interaction recovers within-state heterogeneity", {
  set.seed(2409)
  n_states <- 8L
  per_state <- 20L
  n <- n_states * per_state
  state <- rep(sprintf("%02d", seq_len(n_states)), each = per_state)
  distance <- rep(seq(0.2, 1.8, length.out = per_state), n_states) + stats::rnorm(n, sd = 0.02)
  st_share <- rep(seq(2, 42, length.out = per_state), n_states) + stats::rnorm(n, sd = 1)
  st_10 <- st_share / 10
  state_effect <- rep(seq(-3, 4, length.out = n_states), each = per_state)
  outcome <- 20 + 1.1 * distance + 0.4 * st_10 + 1.25 * distance * st_10 +
    state_effect + stats::rnorm(n, sd = 0.15)
  sample <- data.frame(
    target_unit_2001 = paste0("d", seq_len(n)),
    state_code_2001 = state,
    ling_distance_nonzero_mean = distance,
    st_share_2001 = st_share,
    st_share_10pp = st_10,
    hindi_belt_2001 = TRUE,
    outcome = outcome,
    stringsAsFactors = FALSE
  )
  names(sample)[names(sample) == "outcome"] <- "emi_exposure_all_children_0708"

  fit <- fit_english_opportunity_st_heterogeneity(
    sample,
    outcome_id = "emi_all_children",
    outcome = "emi_exposure_all_children_0708",
    sample_id = "all_states",
    heterogeneity = "continuous_interaction",
    high_st_cutoff = 30,
    controls = character()
  )

  expect_identical(fit$status, "estimated")
  expect_equal(fit$estimate, 1.25, tolerance = 0.08)
  expect_equal(fit$n_states, n_states)
  expect_true(is.finite(fit$std_error_state_clustered))
})

test_that("ST heterogeneity diagnostic uses one cutoff across outcomes and samples", {
  set.seed(2410)
  n_states <- 8L
  per_state <- 12L
  n <- n_states * per_state
  state <- rep(sprintf("%02d", seq_len(n_states)), each = per_state)
  panel <- data.frame(
    target_unit_2001 = paste0("d", seq_len(n)),
    state_code_2001 = state,
    ling_distance_nonzero_mean = stats::runif(n, 0.2, 2),
    st_share_2001 = stats::runif(n, 0, 60),
    emi_exposure_all_children_0708 = stats::runif(n, 0, 40),
    private_emi_exposure_all_children_0708 = stats::runif(n, 0, 20),
    dise_girls_toilet_school_share_0708 = stats::runif(n, 10, 100),
    stringsAsFactors = FALSE
  )
  for (control in english_opportunity_st_heterogeneity_controls()) {
    panel[[control]] <- stats::rnorm(n)
  }
  out <- diagnose_english_opportunity_st_heterogeneity(panel)

  expect_equal(nrow(out$estimates), 12L)
  expect_length(unique(out$estimates$high_st_cutoff_percent), 1L)
  expect_equal(
    unique(out$estimates$high_st_cutoff_percent),
    english_opportunity_high_st_cutoff(panel)
  )
  expect_setequal(out$estimates$sample, c("all_states", "hindi_belt"))
  expect_setequal(
    out$estimates$heterogeneity,
    c("continuous_interaction", "high_st_subset")
  )
})
