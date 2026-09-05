test_that("FAS components match the corresponding just-identified IV regressions", {
  testthat::skip_if_not_installed("ivreg")
  testthat::skip_if_not_installed("sandwich")
  set.seed(271)
  n <- 360L
  data <- data.frame(
    z1 = stats::rnorm(n),
    z2 = stats::rnorm(n),
    z3 = stats::rnorm(n),
    w = stats::rnorm(n),
    state_code_2001 = rep(seq_len(18L), each = 20L)
  )
  data$x <- 0.8 * data$z1 + 0.5 * data$z2 - 0.4 * data$z3 + 0.3 * data$w + stats::rnorm(n)
  data$y <- 1.4 * data$x + 0.35 * data$z2 + 0.2 * data$w + stats::rnorm(n)
  spec <- iv_specification_row(
    specification_id = "toy_fas", adjustment_id = "toy", adjustment = "Toy",
    construction_id = "toy", construction = "Toy instruments",
    outcome = "y", treatment = "x", fixed_effect = "none", controls = "w",
    included_language_controls = character(), excluded_instruments = c("z1", "z2", "z3"),
    mapping_coverage_variable = "", panel_variant = "primary",
    sample_rule = "toy", cluster = "state_code_2001"
  )

  component <- estimate_iv_fas_component(data, spec, "z1")
  direct <- ivreg::ivreg(y ~ x + w + z2 + z3 | z1 + w + z2 + z3, data = data)

  expect_equal(component$status, "estimated")
  expect_equal(component$estimate, unname(stats::coef(direct)["x"]), tolerance = 1e-10)
  expect_true(is.finite(component$first_stage_f))
  expect_gt(component$first_stage_f, 0)
})

test_that("FAS is exactly the interval spanned by all registered just-identified constituents", {
  testthat::skip_if_not_installed("ivreg")
  testthat::skip_if_not_installed("sandwich")
  set.seed(272)
  n <- 300L
  data <- data.frame(
    z1 = stats::rnorm(n), z2 = stats::rnorm(n), z3 = stats::rnorm(n),
    state_code_2001 = rep(seq_len(15L), each = 20L)
  )
  data$x <- data$z1 + 0.6 * data$z2 - 0.5 * data$z3 + stats::rnorm(n)
  data$y <- 1.8 * data$x + 0.5 * data$z3 + stats::rnorm(n)
  spec <- iv_specification_row(
    specification_id = "toy_fas", adjustment_id = "toy", adjustment = "Toy",
    construction_id = "toy", construction = "Toy instruments",
    outcome = "y", treatment = "x", fixed_effect = "none", controls = character(),
    included_language_controls = character(), excluded_instruments = c("z1", "z2", "z3"),
    mapping_coverage_variable = "", panel_variant = "primary",
    sample_rule = "toy", cluster = "state_code_2001"
  )

  out <- estimate_iv_falsification_adaptive_set_spec(data, spec)
  estimates <- out$components$estimate

  expect_equal(out$summary$status, "estimated")
  expect_equal(out$summary$n_components_estimated, 3L)
  expect_equal(out$summary$fas_lower, min(estimates))
  expect_equal(out$summary$fas_upper, max(estimates))
  expect_equal(out$summary$fas_width, max(estimates) - min(estimates))
  expect_identical(
    out$summary$lower_endpoint_instrument,
    out$components$instrument[which.min(estimates)]
  )
  expect_identical(
    out$summary$upper_endpoint_instrument,
    out$components$instrument[which.max(estimates)]
  )
})

test_that("FAS does not silently discard weak constituent instruments", {
  testthat::skip_if_not_installed("ivreg")
  testthat::skip_if_not_installed("sandwich")
  set.seed(273)
  n <- 400L
  data <- data.frame(
    z1 = stats::rnorm(n), z2 = stats::rnorm(n), z3 = stats::rnorm(n),
    state_code_2001 = rep(seq_len(20L), each = 20L)
  )
  data$x <- data$z1 + 0.7 * data$z2 + 0.01 * data$z3 + stats::rnorm(n)
  data$y <- 1.2 * data$x + stats::rnorm(n)
  spec <- iv_specification_row(
    specification_id = "toy_fas", adjustment_id = "toy", adjustment = "Toy",
    construction_id = "toy", construction = "Toy instruments",
    outcome = "y", treatment = "x", fixed_effect = "none", controls = character(),
    included_language_controls = character(), excluded_instruments = c("z1", "z2", "z3"),
    mapping_coverage_variable = "", panel_variant = "primary",
    sample_rule = "toy", cluster = "state_code_2001"
  )

  out <- estimate_iv_falsification_adaptive_set_spec(data, spec)

  expect_equal(nrow(out$components), 3L)
  expect_setequal(out$components$instrument, c("z1", "z2", "z3"))
  expect_equal(out$summary$n_components_estimated, 3L)
  expect_equal(out$summary$fas_lower, min(out$components$estimate))
  expect_equal(out$summary$fas_upper, max(out$components$estimate))
})

test_that("project FAS scope is a bounded six-design five-share family", {
  specs <- iv_falsification_adaptive_specifications()

  expect_equal(nrow(specs), 6L)
  expect_setequal(unique(specs$adjustment_id), c("region_main", "state_main"))
  expect_setequal(
    unique(specs$construction_id),
    c("distance_shares_all", "distance_shares_all_unmapped", "distance_shares_mapped")
  )
  expect_true(all(specs$n_endogenous == 1L))
  expect_true(all(specs$n_excluded_instruments == 5L))
})
