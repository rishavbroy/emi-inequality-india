test_that("first-stage absorption registry is ordered and exhausts main Census controls", {
  registry <- first_stage_absorption_registry()

  expect_identical(registry$specification_id[1:13], c(
    "instrument_only", "region_fe", "state_fe", "census_controls",
    "region_fe_census_controls", "state_fe_census_controls", "expanded_controls",
    "region_fe_expanded_controls", "state_fe_expanded_controls",
    "region_fe_main_without_human_capital", "state_fe_main_without_human_capital",
    "region_fe_expanded_without_human_capital", "state_fe_expanded_without_human_capital"
  ))
  expect_identical(registry$sequence, seq_len(nrow(registry)))
  expect_setequal(unlist(first_stage_control_blocks(), use.names = FALSE), census_2001_absorption_controls())
  expect_identical(
    unlist(registry$controls[registry$specification_id == "state_fe_expanded_controls"][[1]], use.names = FALSE),
    census_2001_absorption_controls()
  )
  expect_true(all(vapply(registry$controls[14:nrow(registry)], function(x) {
    identical(x, order_first_stage_controls(x))
  }, logical(1))))
  expect_identical(
    first_stage_included_control_blocks(census_2001_main_controls()),
    names(first_stage_control_blocks())
  )
  expect_identical(
    first_stage_included_control_blocks(census_2001_absorption_controls()),
    names(first_stage_control_blocks())
  )
})

test_that("first-stage residual metrics treat zero residual variation as not identified", {
  expect_warning(
    residual <- first_stage_residual_metrics_from_vectors(rep(0, 6), seq_len(6)),
    NA
  )
  expect_true(is.na(residual$correlation))
  expect_true(is.na(residual$partial_r_squared))
  expect_equal(residual$instrument_sd, 0)

  data <- data.frame(y = seq_len(6), z = 1, stringsAsFactors = FALSE)
  specification <- data.frame(
    specification_id = "instrument_only", label = "Instrument only",
    fixed_effect = "none", sequence = 1L, stringsAsFactors = FALSE
  )
  specification$controls <- I(list(character()))
  expect_warning(
    estimate <- estimate_first_stage_absorption_spec(data, specification, "y", "z"),
    NA
  )
  expect_identical(estimate$summary$status, "not_estimable")
  expect_identical(estimate$summary$reason, "no_residual_instrument_variation")
  expect_true(is.na(estimate$summary$partial_r_squared))
})

test_that("first-stage estimability rejects saturated fits", {
  fit <- stats::lm(y ~ z + x, data = data.frame(
    y = c(1, 2, 3), z = c(0, 1, 0), x = c(0, 0, 1)
  ))
  residuals <- first_stage_residual_metrics_from_vectors(c(1, -1, 0), c(1, 0, -1))
  status <- first_stage_estimability(
    fit, "z",
    c(std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, partial_f = NA_real_),
    residuals, c(0, 1, 0), c(1, 2, 3)
  )
  expect_identical(status[["status"]], "not_estimable")
  expect_identical(status[["reason"]], "no_residual_degrees_of_freedom")
})

test_that("first-stage residual metrics reproduce nested-model partial R-squared", {
  set.seed(41)
  n <- 120L
  data <- data.frame(
    y = stats::rnorm(n),
    z = stats::rnorm(n),
    x = stats::rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    stringsAsFactors = FALSE
  )
  data$y <- 0.8 * data$z + 0.5 * data$x + data$y
  residual <- first_stage_residual_metrics(
    data, treatment = "y", instrument = "z", controls = "x", fixed_effect = "none"
  )
  restricted <- stats::lm(y ~ x, data = data)
  full <- stats::lm(y ~ z + x, data = data)
  expected <- (stats::deviance(restricted) - stats::deviance(full)) / stats::deviance(restricted)

  expect_equal(residual$partial_r_squared, expected, tolerance = 1e-12)
  expect_equal(residual$partial_r_squared, residual$correlation^2, tolerance = 1e-12)
})

test_that("first-stage absorption helper diagnostics preserve support contracts", {
  set.seed(42)
  states <- rep(sprintf("%02d", 1:6), each = 4)
  z <- rep(seq(-1, 1, length.out = 4), 6) + rep(seq(-2, 2, length.out = 6), each = 4)
  data <- data.frame(
    state_code_2001 = states,
    district_code_2001 = sprintf("%02d", seq_along(states)),
    region = rep(panel_region_levels(), each = 4),
    z = z,
    y = 12 + 3 * z + stats::rnorm(length(z), sd = 0.2),
    stringsAsFactors = FALSE
  )
  specification <- data.frame(
    specification_id = "instrument_only",
    label = "Instrument only",
    fixed_effect = "none",
    sequence = 1L,
    stringsAsFactors = FALSE
  )
  specification$controls <- I(list(character()))

  estimate <- estimate_first_stage_absorption_spec(data, specification, "y", "z")
  ranges <- first_stage_state_residual_ranges(data, list(estimate))
  deletion <- first_stage_state_deletion(
    data, specification, "y", "z", full_estimate = estimate
  )
  influence <- first_stage_district_influence(data, estimate$fit, "z")

  expect_gt(estimate$summary$partial_r_squared, 0.9)
  expect_equal(nrow(ranges), length(unique(states)))
  expect_true(all(c("instrument_range", "treatment_range") %in% names(ranges)))
  expect_equal(nrow(deletion), length(unique(states)))
  expect_setequal(
    names(deletion),
    c(
      "specification_id", "specification", "treatment", "instrument", "omitted_state",
      "estimate", "excluded_instrument_f", "estimate_change", "f_change"
    )
  )
  expect_equal(nrow(influence), nrow(data))
  expect_true(all(c("leverage", "cooks_distance", "instrument_dfbeta") %in% names(influence)))
})

test_that("first-stage absorption aliases resolve without refitting diagnostics", {
  registry <- first_stage_absorption_registry()
  aliases <- first_stage_absorption_aliases()
  execution_summary <- data.frame(
    specification_id = registry$specification_id,
    estimate = seq_len(nrow(registry)),
    partial_r_squared = seq_len(nrow(registry)) / 100,
    excluded_instrument_f = seq_len(nrow(registry)) / 10,
    stringsAsFactors = FALSE
  )

  semantic <- first_stage_absorption_semantic_summary(execution_summary, aliases)

  expect_equal(nrow(semantic), nrow(aliases))
  expect_identical(
    semantic$semantic_specification_id,
    aliases$semantic_specification_id
  )
  expect_true(all(semantic$execution_specification_id %in% registry$specification_id))
  expect_true(any(semantic$is_execution_alias))
})

test_that("first-stage absorption diagnostics fail rather than changing support silently", {
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 2),
    region = rep(panel_region_levels(), each = 2),
    ling_distance_nonzero_mean = seq_len(12),
    emi_exposure_all_children_0708 = seq_len(12),
    stringsAsFactors = FALSE
  )
  panel$district_code_2001 <- sprintf("%02d", seq_len(nrow(panel)))
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- 1
  panel$st_share_2001[1] <- NA_real_

  prepared <- prepare_first_stage_absorption_panel(panel)
  expect_equal(nrow(prepared), 11L)
  expect_error(
    prepare_first_stage_absorption_panel(transform(panel, region = "Northern")),
    "all six panel regions"
  )
})

test_that("first-stage absorption diagnostics save a compact manifest without recomputation", {
  registry <- first_stage_absorption_registry()[1, , drop = FALSE]
  diagnostics <- structure(
    list(
      summary = data.frame(specification_id = registry$specification_id, stringsAsFactors = FALSE),
      semantic_summary = data.frame(
        semantic_specification_id = registry$specification_id,
        execution_specification_id = registry$specification_id,
        stringsAsFactors = FALSE
      ),
      registry = registry,
      aliases = data.frame(
        semantic_specification_id = registry$specification_id,
        execution_specification_id = registry$specification_id,
        is_execution_alias = FALSE,
        stringsAsFactors = FALSE
      ),
      common_support = data.frame(n = 24L),
      state_residual_ranges = data.frame(specification_id = registry$specification_id),
      state_deletion = data.frame(specification_id = registry$specification_id),
      district_influence = data.frame(district_code_2001 = "001"),
      vif = data.frame(specification_id = registry$specification_id),
      stringsAsFactors = FALSE
    ),
    class = "emi_first_stage_absorption"
  )
  dir <- tempfile("first-stage-absorption-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  manifest <- save_first_stage_absorption_diagnostics(diagnostics, dir)

  expect_setequal(basename(manifest$path), c(
    "first_stage_absorption_ladder.csv",
    "first_stage_absorption_semantic_summary.csv",
    "first_stage_absorption_registry.csv",
    "first_stage_absorption_aliases.csv", "first_stage_absorption_common_support.csv",
    "first_stage_state_residual_ranges.csv",
    "first_stage_state_deletion.csv", "first_stage_district_influence.csv",
    "first_stage_vif.csv"
  ))
  expect_true(all(file.exists(manifest$path)))
  saved_registry <- utils::read.csv(
    manifest$path[basename(manifest$path) == "first_stage_absorption_registry.csv"],
    stringsAsFactors = FALSE
  )
  expect_false(is.list(saved_registry$controls))
  expected_controls <- unlist(registry$controls[[1L]], use.names = FALSE)
  expect_identical(
    saved_registry$controls,
    if (length(expected_controls)) paste(expected_controls, collapse = ";") else "none"
  )
})

test_that("diagnostic list-column serialization preserves empty and populated contracts", {
  x <- data.frame(id = c("empty", "populated"), stringsAsFactors = FALSE)
  x$controls <- I(list(character(), c("control_a", "control_b")))

  out <- collapse_diagnostic_list_columns(x, "controls")

  expect_false(is.list(out$controls))
  expect_identical(out$controls, c("none", "control_a;control_b"))
})
