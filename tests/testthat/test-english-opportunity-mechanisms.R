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
