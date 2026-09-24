test_that("consumption IV registry compiles ANCOVA into the canonical IV specification contract", {
  registry <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    treatment = preferred_iv_variables()$treatment,
    instrument = preferred_iv_variables()$instrument,
    adjustment_id = "state_main",
    construction_id = "nonzero_mean",
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    tier = "A",
    stringsAsFactors = FALSE
  )

  out <- compile_consumption_iv_specifications(registry)
  baseline <- consumption_iv_variable_name("long_2023__ancova", "baseline")
  expect_equal(nrow(out), 1L)
  expect_identical(out$fixed_effect[[1]], "state")
  expect_identical(out$outcome_round[[1]], "hces_2023_24")
  expect_identical(out$baseline_round[[1]], "nss_2004_05")
  expect_identical(out$estimand[[1]], "ancova")
  expect_true(baseline %in% unlist(out$controls[[1]], use.names = FALSE))
  expect_true(all(census_2001_main_controls() %in% unlist(out$controls[[1]], use.names = FALSE)))
  expect_identical(
    unlist(out$excluded_instruments[[1]], use.names = FALSE),
    preferred_iv_variables()$instrument
  )
})

test_that("Tier-A consumption IV specifications cannot select on outcome precision", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  registry <- data.frame(
    welfare_specification_id = "headline",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    treatment = preferred_iv_variables()$treatment,
    instrument = preferred_iv_variables()$instrument,
    adjustment_id = "state_main",
    construction_id = "nonzero_mean",
    panel_variant = "primary",
    sample_rule = "preferred_welfare_support",
    tier = "A",
    stringsAsFactors = FALSE
  )
  utils::write.csv(registry, path, row.names = FALSE)

  expect_error(
    read_consumption_iv_outcome_registry(path),
    "Tier-A consumption IV specifications must use ex-ante"
  )

  registry$tier <- "B"
  utils::write.csv(registry, path, row.names = FALSE)
  expect_identical(
    read_consumption_iv_outcome_registry(path)$sample_rule,
    "preferred_welfare_support"
  )
})

test_that("consumption IV outcome data separate analysis support from precision sensitivity", {
  welfare <- data.frame(
    district_2001 = rep(c("pc2001__01__01", "pc2001__01__02", "pc2001__01__03"), 2),
    round_id = rep(c("nss_2004_05", "hces_2023_24"), each = 3),
    outcome_id = "real_mean_mpce",
    estimate = c(100, 200, 300, 200, 400, 600),
    analysis_eligible = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    preferred_eligible = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )

  out <- build_consumption_iv_specification_data(welfare, spec)
  expect_equal(out$outcome_value[[1]], log(200))
  expect_equal(out$baseline_value[[1]], log(100))
  expect_true(out$welfare_support[[1]])
  # District 2 fails the stricter RSE/reporting flag but remains in the primary
  # causal sample because both rounds pass predeclared survey-support rules.
  expect_equal(out$outcome_value[[2]], log(400))
  expect_equal(out$baseline_value[[2]], log(200))
  expect_true(out$welfare_support[[2]])
  expect_true(is.na(out$outcome_value[[3]]))
  expect_false(out$welfare_support[[3]])

  spec$sample_rule <- "preferred_welfare_support"
  strict <- build_consumption_iv_specification_data(welfare, spec)
  expect_true(is.na(strict$outcome_value[[2]]))
  expect_true(is.na(strict$baseline_value[[2]]))
  expect_false(strict$welfare_support[[2]])
})

test_that("consumption IV change outcomes use the same transformed baseline and endpoint", {
  welfare <- data.frame(
    district_2001 = rep(c("pc2001__01__01", "pc2001__01__02"), 2),
    round_id = rep(c("nss_2004_05", "nss_2011_12_type2"), each = 2),
    outcome_id = "real_mean_mpce",
    estimate = c(100, 200, 150, 100),
    analysis_eligible = TRUE,
    preferred_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    welfare_specification_id = "medium_2011__change",
    outcome_id = "real_mean_mpce",
    outcome_round = "nss_2011_12_type2",
    baseline_round = "nss_2004_05",
    estimand = "change",
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )

  out <- build_consumption_iv_specification_data(welfare, spec)
  expect_equal(
    out$outcome_value,
    c(log(150) - log(100), log(100) - log(200)),
    tolerance = 1e-12
  )
  expect_equal(out$baseline_value, log(c(100, 200)), tolerance = 1e-12)
})

test_that("consumption IV panel augmentation preserves canonical panel rows", {
  panel <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    keep = c("a", "b"),
    stringsAsFactors = FALSE
  )
  welfare <- data.frame(
    district_2001 = rep(panel$target_unit_2001, 2),
    round_id = rep(c("nss_2004_05", "hces_2023_24"), each = 2),
    outcome_id = "real_mean_mpce",
    estimate = c(100, 200, 150, 300),
    analysis_eligible = TRUE,
    preferred_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  registry <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )

  out <- attach_consumption_iv_outcomes(panel, welfare, registry)
  expect_identical(out$target_unit_2001, panel$target_unit_2001)
  expect_identical(out$keep, panel$keep)
  expect_equal(
    out[[consumption_iv_variable_name("long_2023__ancova", "outcome")]],
    log(c(150, 300))
  )
  expect_equal(
    out[[consumption_iv_variable_name("long_2023__ancova", "baseline")]],
    log(c(100, 200))
  )
})

test_that("IV specification rows derive registered instrument axes centrally", {
  row <- iv_specification_row(
    specification_id = "derived_axes",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean_shastry",
    construction = "Nonzero mean with Shastry composition controls",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = character(),
    included_language_controls = c("hindi_urdu_share", "native_english_share"),
    excluded_instruments = "ling_distance_nonzero_mean",
    mapping_coverage_variable = "ling_map_coverage",
    panel_variant = "primary",
    sample_rule = "support"
  )

  expect_identical(row$distance_measure_id, "shastry_nonzero_mean")
  expect_identical(row$language_adjustment_id, "shastry_composition")

  synthetic <- iv_specification_row(
    specification_id = "synthetic_axes",
    adjustment_id = "state_main",
    adjustment = "State FE",
    construction_id = "toy_instrument",
    construction = "Toy",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = character(),
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = NA_character_,
    panel_variant = "primary",
    sample_rule = "support"
  )
  expect_true(is.na(synthetic$distance_measure_id))
  expect_true(is.na(synthetic$language_adjustment_id))

  expect_error(
    iv_specification_row(
      specification_id = "conflicting_axes",
      adjustment_id = "state_main",
      adjustment = "State FE",
      construction_id = "nonzero_mean",
      construction = "Preferred distance",
      outcome = "y",
      treatment = "d",
      fixed_effect = "state",
      controls = character(),
      included_language_controls = character(),
      excluded_instruments = "ling_distance_nonzero_mean",
      mapping_coverage_variable = "ling_map_coverage",
      panel_variant = "primary",
      sample_rule = "support",
      distance_measure_id = "glottolog_nonhindi_mean"
    ),
    "declares distance_measure_id='shastry_nonzero_mean'"
  )
})

test_that("IV specification row binding preserves list-column contracts", {
  row_a <- iv_specification_row(
    specification_id = "a",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y_a",
    treatment = "d",
    fixed_effect = "state",
    controls = c("literacy_rate_2001", "custom_baseline"),
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support"
  )
  row_b <- row_a
  row_b$specification_id <- "b"
  row_b$outcome <- "y_b"

  out <- bind_iv_specification_rows(list(row_a, row_b))
  expect_true(is.list(out$controls))
  expect_true(is.list(out$excluded_instruments))
  expect_true(is.list(out$included_language_controls))
  expect_true("custom_baseline" %in% unlist(out$controls[[1]], use.names = FALSE))
  expect_identical(unlist(out$excluded_instruments[[2]], use.names = FALSE), "z")
})

test_that("IV analysis frames drop sf geometry without flattening analysis columns", {
  skip_if_not_installed("sf")
  panel <- data.frame(
    y = c(1, 2),
    d = c(0.2, 0.4),
    z = c(0.1, 0.3),
    state_code_2001 = c("01", "02"),
    stringsAsFactors = FALSE
  )
  panel$x_coord <- c(0, 1)
  panel$y_coord <- c(0, 1)
  panel <- sf::st_as_sf(
    panel,
    coords = c("x_coord", "y_coord"),
    crs = 4326
  )

  out <- iv_analysis_frame(panel, c("y", "d", "z", "state_code_2001"))

  expect_false(inherits(out, "sf"))
  expect_false("geometry" %in% names(out))
  expect_identical(out$y, c(1, 2))
  expect_identical(out$state_code_2001, c("01", "02"))
})

test_that("IV analysis frames reject non-atomic registered variables", {
  panel <- data.frame(y = 1:2)
  panel$z <- I(list(1, 2))

  expect_error(
    iv_analysis_frame(panel, c("y", "z")),
    "must be atomic"
  )
})

test_that("consumption IV coverage resolves fixed-effect formula terms to panel variables", {
  registry <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    treatment = preferred_iv_variables()$treatment,
    instrument = preferred_iv_variables()$instrument,
    adjustment_id = "state_main",
    construction_id = "nonzero_mean",
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    tier = "A",
    stringsAsFactors = FALSE
  )
  spec <- compile_consumption_iv_specifications(registry)
  required <- iv_specification_variables(spec, include_outcome = TRUE)
  panel <- as.data.frame(
    setNames(
      replicate(length(required), rep(1, 5), simplify = FALSE),
      required
    ),
    stringsAsFactors = FALSE
  )
  panel$state_code_2001 <- c("01", "01", "02", "02", "03")

  out <- summarize_consumption_iv_outcome_coverage(panel, spec)
  expect_equal(out$status, "ready")
  expect_equal(out$n_analysis_complete, 5L)
  expect_true(is.na(out$missing_columns))
  expect_false(grepl("factor\\(", paste(required, collapse = ";")))
})

test_that("consumption IV coverage validation blocks non-ready registered specifications", {
  good <- data.frame(
    specification_id = "ready",
    n_analysis_complete = 10L,
    analysis_share = 0.5,
    status = "ready",
    missing_columns = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_equal(
    validate_consumption_iv_outcome_coverage(good),
    good,
    ignore_attr = TRUE
  )

  bad <- good
  bad$specification_id <- "broken"
  bad$status <- "missing_columns"
  bad$missing_columns <- "state_code_2001"
  expect_error(
    validate_consumption_iv_outcome_coverage(bad),
    "broken=missing_columns\\[state_code_2001\\]"
  )
})

test_that("canonical IV specification coercion preserves list columns", {
  row <- iv_specification_row(
    specification_id = "one",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = c("literacy_rate_2001", "custom_baseline"),
    included_language_controls = "hindi_urdu_share",
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support",
    cluster = "state_code_2001"
  )
  out <- as_iv_specifications(row)
  expect_true(is.list(out$controls))
  expect_true(is.list(out$included_language_controls))
  expect_true(is.list(out$excluded_instruments))
  expect_identical(
    unlist(out$controls[[1]], use.names = FALSE),
    unlist(row$controls[[1]], use.names = FALSE)
  )
})

test_that("canonical IV specification coercion rejects flattened list columns", {
  row <- iv_specification_row(
    specification_id = "one",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "literacy_rate_2001",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support"
  )
  expect_error(
    as_iv_specifications(safe_df(row)),
    "list-column contract was lost"
  )
})

test_that("IV variable extraction preserves formula and cluster contracts after binding", {
  rows <- lapply(c("y1", "y2"), function(outcome) {
    iv_specification_row(
      specification_id = outcome,
      adjustment_id = "state_main",
      adjustment = "State FE + main controls",
      construction_id = "nonzero_mean",
      construction = "Nonzero mean",
      outcome = outcome,
      treatment = "d",
      fixed_effect = "state",
      controls = c("literacy_rate_2001", "custom_baseline"),
      included_language_controls = character(),
      excluded_instruments = "z",
      mapping_coverage_variable = "coverage",
      panel_variant = "primary",
      sample_rule = "support",
      cluster = "state_code_2001"
    )
  })
  specs <- bind_iv_specification_rows(rows)
  vars <- iv_specification_variables(specs[1, , drop = FALSE])
  expect_true(all(c(
    "y1", "d", "z", "literacy_rate_2001", "custom_baseline", "state_code_2001"
  ) %in% vars))
  expect_false(any(grepl("factor\\(", vars)))
  expect_identical(
    iv_specification_cluster_variable(specs[1, , drop = FALSE]),
    "state_code_2001"
  )
})

test_that("IV formula helpers reject multi-row specifications explicitly", {
  row <- iv_specification_row(
    specification_id = "one",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = character(),
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support"
  )
  row2 <- row
  row2$specification_id <- "two"
  specs <- bind_iv_specification_rows(list(row, row2))
  expect_error(
    iv_specification_variables(specs),
    "single canonical IV specification"
  )
})

test_that("registered consumption IV dynamics share one specification sample across estimators", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  set.seed(503)
  n <- 96
  panel <- data.frame(
    y = rnorm(n), d = rnorm(n), z = rnorm(n), control = rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:8), each = 12),
    stringsAsFactors = FALSE
  )
  panel$d <- 0.7 * panel$z + 0.3 * panel$control + rnorm(n)
  panel$y <- 0.8 * panel$d + 0.2 * panel$control + rnorm(n)
  panel$y[c(4, 13, 51)] <- NA_real_
  rownames(panel) <- paste0("d_", seq_len(n))

  spec <- iv_specification_row(
    specification_id = "consumption__toy",
    adjustment_id = "state_main",
    adjustment = "State FE",
    construction_id = "nonzero_mean",
    construction = "Toy",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "control",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = NA_character_,
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    cluster = "state_code_2001"
  )
  spec$welfare_specification_id <- "toy"
  spec$welfare_outcome_id <- "real_mean_mpce"
  spec$outcome_round <- "hces_2023_24"
  spec$baseline_round <- "nss_2004_05"
  spec$estimand <- "ancova"
  spec$analysis_transform <- "log"
  spec$analysis_id <- "consumption_iv__consumption__toy"

  out <- estimate_consumption_iv_dynamics(panel, spec, list(), ar_points = 31L)
  row <- out$summary

  expect_equal(nrow(row), 1L)
  expect_identical(row$analysis_id, spec$analysis_id)
  expect_equal(row$first_stage_n, row$reduced_form_n)
  expect_equal(row$first_stage_n, row$second_stage_n)
  expect_equal(row$first_stage_n, row$n)
  expect_true(is.finite(row$reduced_form_estimate))
  expect_true(is.finite(row$second_stage_estimate))
  expect_true(is.finite(row$partial_f))
  expect_true(is.finite(row$anderson_rubin_p_beta0))
  expect_true(nrow(out$anderson_rubin_grid) > 0L)
  expect_true(all(out$anderson_rubin_grid$analysis_id == spec$analysis_id))
})

test_that("consumption reduced forms preserve canonical controls and fixed effects", {
  skip_if_not_installed("sandwich")
  set.seed(504)
  n <- 72
  panel <- data.frame(
    y = rnorm(n), d = rnorm(n), z = rnorm(n), baseline = rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 12),
    stringsAsFactors = FALSE
  )
  spec <- iv_specification_row(
    specification_id = "rf",
    adjustment_id = "state_main",
    adjustment = "State FE",
    construction_id = "nonzero_mean",
    construction = "Toy",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "baseline",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = NA_character_,
    panel_variant = "primary",
    sample_rule = "support",
    cluster = "state_code_2001"
  )

  out <- estimate_iv_reduced_form_spec(panel, spec, list())
  expect_equal(out$status, "estimated")
  expect_equal(out$n, n)
  expect_identical(out$term, "z")
  expect_true(all(is.finite(unlist(
    out[c("estimate", "std.error", "p.value")],
    use.names = FALSE
  ))))
})

test_that("dynamic consumption IV validation enforces the common-sample inference contract", {
  spec <- iv_specification_row(
    specification_id = "consumption__toy",
    adjustment_id = "state_main",
    adjustment = "State FE",
    construction_id = "nonzero_mean",
    construction = "Toy",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "control",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = NA_character_,
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    cluster = "state_code_2001"
  )
  spec$welfare_specification_id <- "toy"
  spec$welfare_outcome_id <- "real_mean_mpce"
  spec$outcome_round <- "hces_2023_24"
  spec$baseline_round <- "nss_2004_05"
  spec$estimand <- "ancova"
  spec$analysis_transform <- "log"

  summary <- data.frame(
    specification_id = "consumption__toy",
    first_stage_n = 50L,
    first_stage_status = "estimated",
    reduced_form_n = 50L,
    reduced_form_status = "estimated",
    second_stage_n = 50L,
    second_stage_status = "estimated",
    n = 50L,
    status = "estimated",
    partial_f = 4,
    effective_f = 3.5,
    effective_f_critical_value = 10,
    effective_f_p_value = 0.2,
    effective_f_df = 1,
    effective_f_status = "estimated",
    reduced_form_estimate = 0.1,
    reduced_form_std.error = 0.04,
    reduced_form_p.value = 0.02,
    second_stage_estimate = 0.2,
    second_stage_std.error = 0.1,
    second_stage_p.value = 0.05,
    anderson_rubin_p_beta0 = 0.03,
    stringsAsFactors = FALSE
  )
  dynamics <- list(
    summary = summary,
    anderson_rubin_grid = data.frame(
      specification_id = "consumption__toy",
      beta = c(-1, 0, 1),
      accepted = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  )

  out <- validate_consumption_iv_dynamics(dynamics, spec)
  expect_equal(out$summary$specification_id, "consumption__toy")

  dynamics$summary$reduced_form_n <- 49L
  expect_error(
    validate_consumption_iv_dynamics(dynamics, spec),
    "not analysis-ready"
  )
})

test_that("dynamic consumption IV validation rejects missing registered AR grids", {
  specs <- bind_iv_specification_rows(lapply(c("one", "two"), function(id) {
    row <- iv_specification_row(
      specification_id = id,
      adjustment_id = "state_main",
      adjustment = "State FE",
      construction_id = "nonzero_mean",
      construction = "Toy",
      outcome = "y",
      treatment = "d",
      fixed_effect = "state",
      controls = "control",
      included_language_controls = character(),
      excluded_instruments = "z",
      mapping_coverage_variable = NA_character_,
      panel_variant = "primary",
      sample_rule = "support",
      cluster = "state_code_2001"
    )
    row$welfare_specification_id <- id
    row$welfare_outcome_id <- "real_mean_mpce"
    row$outcome_round <- "hces_2023_24"
    row$baseline_round <- "nss_2004_05"
    row$estimand <- "ancova"
    row$analysis_transform <- "log"
    row
  }))

  summary <- data.frame(
    specification_id = c("one", "two"),
    first_stage_n = 50L,
    first_stage_status = "estimated",
    reduced_form_n = 50L,
    reduced_form_status = "estimated",
    second_stage_n = 50L,
    second_stage_status = "estimated",
    n = 50L,
    status = "estimated",
    partial_f = 4,
    effective_f = 3.5,
    effective_f_critical_value = 10,
    effective_f_p_value = 0.2,
    effective_f_df = 1,
    effective_f_status = "estimated",
    reduced_form_estimate = 0.1,
    reduced_form_std.error = 0.04,
    reduced_form_p.value = 0.02,
    second_stage_estimate = 0.2,
    second_stage_std.error = 0.1,
    second_stage_p.value = 0.05,
    anderson_rubin_p_beta0 = 0.03,
    stringsAsFactors = FALSE
  )
  dynamics <- list(
    summary = summary,
    anderson_rubin_grid = data.frame(
      specification_id = "one",
      beta = c(-1, 0, 1),
      accepted = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    validate_consumption_iv_dynamics(dynamics, specs),
    "lack Anderson-Rubin grids"
  )
})

test_that("Anderson-Rubin acceptance components normalize list-column-like inputs", {
  grid <- data.frame(id = 1:7)
  grid$beta <- I(as.list(-3:3))
  grid$accepted <- I(as.list(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)))

  components <- anderson_rubin_acceptance_components(grid)

  expect_equal(nrow(components), 2L)
  expect_type(components$lower, "double")
  expect_type(components$upper, "double")
  expect_equal(components$lower, c(-3, 2))
  expect_equal(components$upper, c(-2, 3))
  expect_false(any(components$contains_zero))
})

test_that("Anderson-Rubin acceptance grid rejects malformed public inputs", {
  expect_error(
    anderson_rubin_acceptance_components(
      data.frame(beta = c(-1, NA, 1), accepted = TRUE)
    ),
    "non-finite beta"
  )
  expect_error(
    anderson_rubin_acceptance_components(
      data.frame(beta = -1:1, accepted = c("TRUE", "maybe", "FALSE"))
    ),
    "invalid accepted flags"
  )
})

test_that("consumption distribution benchmark preserves serial/configured estimates", {
  set.seed(702)
  n <- 40
  x <- data.frame(
    survey_id = "wave", household_id = paste0("h", seq_len(n)),
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = as.character(seq_len(n)), stratum = "1", sub_stratum = "1",
    household_size = 1, target_unit_2001 = rep(c("a", "b"), each = n / 2),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = exp(rnorm(n, log(200), .3)),
    stringsAsFactors = FALSE
  )
  registry <- data.frame(
    outcome_id = "bottom40", estimand = "survey_bottom_mean", transform = "identity",
    quantile = 0.4, quantile_interval = "", quantile_rule = "",
    role = "robustness", min_households = 1,
    min_fsu = 1, min_kish_effective_n = 1, max_relative_se = 10,
    survey_ids = "*", stringsAsFactors = FALSE
  )
  old <- Sys.getenv("EMI_CONSUMPTION_DOMAIN_CORES", unset = NA_character_)
  Sys.setenv(EMI_CONSUMPTION_DOMAIN_CORES = "1")
  on.exit({
    if (is.na(old)) Sys.unsetenv("EMI_CONSUMPTION_DOMAIN_CORES") else
      Sys.setenv(EMI_CONSUMPTION_DOMAIN_CORES = old)
  }, add = TRUE)
  out <- benchmark_consumption_distribution_domains(x, registry, max_districts = 2L)
  expect_equal(out$mode, c("serial", "configured"))
  expect_true(all(is.finite(out$elapsed_seconds)))
  expect_lte(out$max_abs_estimate_diff[[2L]], 1e-10)
  expect_lte(out$max_abs_se_diff[[2L]], 1e-10)
})

test_that("dynamic consumption IV estimation is stable across multiple registered specifications", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("car")

  set.seed(507)
  n <- 96L
  panel <- data.frame(
    z = rnorm(n),
    control = rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:8), each = 12),
    stringsAsFactors = FALSE
  )
  panel$d <- 0.8 * panel$z + 0.2 * panel$control + rnorm(n)
  panel$baseline_a <- rnorm(n)
  panel$baseline_b <- rnorm(n)
  panel$y_a <- 0.5 * panel$d + 0.3 * panel$baseline_a + rnorm(n)
  panel$y_b <- 0.7 * panel$d + 0.3 * panel$baseline_b + rnorm(n)

  make_spec <- function(id, outcome, baseline) {
    spec <- iv_specification_row(
      specification_id = id,
      adjustment_id = "state_main",
      adjustment = "State FE",
      construction_id = "nonzero_mean",
      construction = "Toy",
      outcome = outcome,
      treatment = "d",
      fixed_effect = "state",
      controls = c("control", baseline),
      included_language_controls = character(),
      excluded_instruments = "z",
      mapping_coverage_variable = NA_character_,
      panel_variant = "primary",
      sample_rule = "analysis_welfare_support",
      cluster = "state_code_2001"
    )
    spec$welfare_specification_id <- sub("^consumption__", "", id)
    spec$welfare_outcome_id <- "real_mean_mpce"
    spec$outcome_round <- "toy_endpoint"
    spec$baseline_round <- "toy_baseline"
    spec$estimand <- "ancova"
    spec$analysis_transform <- "log"
    spec
  }

  specs <- bind_iv_specification_rows(list(
    make_spec("consumption__toy_a", "y_a", "baseline_a"),
    make_spec("consumption__toy_b", "y_b", "baseline_b")
  ))

  if (requireNamespace("sf", quietly = TRUE)) {
    panel$x_coord <- seq_len(nrow(panel))
    panel$y_coord <- 0
    panel <- sf::st_as_sf(
      panel,
      coords = c("x_coord", "y_coord"),
      crs = 4326
    )
  }

  out <- estimate_consumption_iv_dynamics(
    panel, specs, list(), ar_points = 31L
  )

  expect_identical(
    out$summary$specification_id,
    c("consumption__toy_a", "consumption__toy_b")
  )
  expect_equal(nrow(out$summary), 2L)
  expect_true(all(out$summary$first_stage_status == "estimated"))
  expect_true(all(out$summary$reduced_form_status == "estimated"))
  expect_true(all(out$summary$second_stage_status == "estimated"))
  expect_true(all(out$summary$status == "estimated"))
  expect_setequal(
    unique(out$anderson_rubin_grid$specification_id),
    out$summary$specification_id
  )
})

test_that("optional pretrend welfare rounds remain outside the causal IV registry", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  survey_registry <- read_consumption_survey_registry(build_paths(root))
  iv_registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )

  pretrend <- survey_registry$survey_id[
    survey_registry$analysis_role == "optional_pretrend"
  ]
  registered_rounds <- unique(c(
    plain_chr(iv_registry$outcome_round),
    plain_chr(iv_registry$baseline_round)
  ))

  expect_setequal(pretrend, c("nss_2000_01", "nss_2001_02"))
  expect_length(intersect(pretrend, registered_rounds), 0L)
})


test_that("joint Wald estimability distinguishes valid, saturated, and unavailable inference", {
  x <- data.frame(
    y = c(2, 3, 5, 8, 9, 12, 14, 17),
    a = c(0, 1, 0, 1, 2, 2, 3, 4),
    b = c(1, 0, 2, 1, 0, 3, 2, 4)
  )
  fit <- stats::lm(y ~ a + b, data = x)
  valid <- c(statistic = 2, p.value = 0.1, df = 2)
  expect_identical(
    unname(joint_wald_estimability(fit, c("a", "b"), valid)[["status"]]),
    "estimated"
  )

  saturated <- stats::lm(y ~ factor(seq_along(y)) - 1, data = x)
  saturated_status <- joint_wald_estimability(
    saturated, names(stats::coef(saturated))[1L], valid
  )
  expect_identical(unname(saturated_status[["status"]]), "not_estimable")
  expect_identical(unname(saturated_status[["reason"]]), "no_residual_degrees_of_freedom")

  unavailable <- joint_wald_estimability(
    fit, c("a", "b"), c(statistic = NA_real_, p.value = NA_real_, df = 2)
  )
  expect_identical(unname(unavailable[["status"]]), "not_estimable")
  expect_identical(unname(unavailable[["reason"]]), "clustered_joint_inference_unavailable")
})
