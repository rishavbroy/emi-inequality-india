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
