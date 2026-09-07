test_that("schooling-consumption bridge registers only planned margins and long-run welfare", {
  treatments <- schooling_consumption_bridge_treatment_registry()
  expect_identical(
    treatments$treatment,
    c(
      "enrollment_rate_0708",
      "emi_share_enrolled_0708",
      "emi_exposure_all_children_0708",
      "public_emi_exposure_all_children_0708",
      "private_emi_exposure_all_children_0708"
    )
  )

  registry <- data.frame(
    welfare_specification_id = c(
      "early_2009__change", "long_2022__ancova", "long_2022__change",
      "long_2023__ancova", "long_2023__change"
    ),
    outcome_id = "real_mean_mpce",
    outcome_round = c(
      "nss_2009_10_type2", "hces_2022_23", "hces_2022_23",
      "hces_2023_24", "hces_2023_24"
    ),
    baseline_round = "nss_2004_05",
    estimand = c("change", "ancova", "change", "ancova", "change"),
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )
  welfare <- schooling_consumption_bridge_welfare_registry(registry)
  expect_identical(
    welfare$welfare_specification_id,
    c(
      "long_2022__ancova", "long_2022__change",
      "long_2023__ancova", "long_2023__change"
    )
  )
})

test_that("schooling-consumption bridge keeps one sample across its adjustment ladder", {
  set.seed(101)
  n <- 36L
  panel <- data.frame(
    target_unit_2001 = sprintf("d%03d", seq_len(n)),
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 6),
    region = rep(paste0("r", 1:6), each = 6),
    treatment = stats::rnorm(n),
    outcome = stats::rnorm(n),
    baseline = stats::rnorm(n),
    control_a = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$control_a[[1L]] <- NA_real_
  welfare <- data.frame(
    welfare_specification_id = "long_2022__ancova",
    estimand = "ancova",
    stringsAsFactors = FALSE
  )
  outcome_name <- consumption_iv_variable_name("long_2022__ancova", "outcome")
  baseline_name <- consumption_iv_variable_name("long_2022__ancova", "baseline")
  names(panel)[names(panel) == "outcome"] <- outcome_name
  names(panel)[names(panel) == "baseline"] <- baseline_name
  adjustments <- data.frame(
    specification_id = c("unadjusted", "region_main", "state_main"),
    label = c("Unadjusted", "Region", "State"),
    fixed_effect = c("none", "region", "state"),
    controls = I(list(character(), "control_a", "control_a")),
    sequence = 1:3,
    stringsAsFactors = FALSE
  )

  sample <- prepare_schooling_consumption_bridge_sample(
    panel, "treatment", welfare, adjustments
  )
  expect_equal(nrow(sample), n - 1L)
  fitted <- lapply(seq_len(nrow(adjustments)), function(i) {
    fit_schooling_consumption_bridge_specification(
      sample, "treatment", welfare, adjustments[i, , drop = FALSE]
    )
  })
  expect_identical(vapply(fitted, `[[`, numeric(1), "n"), rep(n - 1, 3))
})

test_that("schooling-consumption ANCOVA retains baseline while change does not", {
  n <- 90L
  state <- rep(sprintf("%02d", 1:9), each = 10)
  within_state <- seq(-2, 2, length.out = 10)
  treatment <- rep(within_state, times = 9)
  # Correlate baseline with schooling so omitting it changes the schooling slope,
  # while using an orthogonal polynomial residual keeps the ANCOVA coefficient
  # exactly identified at the constructed value.
  baseline <- rep(within_state + within_state^2, times = 9)
  noise <- rep(stats::poly(within_state, degree = 3)[, 3], times = 9) / 20
  endpoint <- 0.04 * treatment + 0.8 * baseline + noise
  change <- endpoint - baseline
  panel <- data.frame(
    target_unit_2001 = sprintf("d%03d", seq_len(n)),
    state_code_2001 = state,
    region = rep(paste0("r", 1:9), each = 10),
    schooling = treatment,
    stringsAsFactors = FALSE
  )
  panel[[consumption_iv_variable_name("long_2022__ancova", "outcome")]] <- endpoint
  panel[[consumption_iv_variable_name("long_2022__ancova", "baseline")]] <- baseline
  panel[[consumption_iv_variable_name("long_2022__change", "outcome")]] <- change
  adjustment <- data.frame(
    specification_id = "state_main",
    label = "State FE",
    fixed_effect = "state",
    controls = I(list(character())),
    sequence = 1L,
    stringsAsFactors = FALSE
  )
  ancova <- data.frame(
    welfare_specification_id = "long_2022__ancova",
    estimand = "ancova",
    stringsAsFactors = FALSE
  )
  change_spec <- data.frame(
    welfare_specification_id = "long_2022__change",
    estimand = "change",
    stringsAsFactors = FALSE
  )

  ancova_fit <- fit_schooling_consumption_bridge_specification(
    panel, "schooling", ancova, adjustment
  )
  change_fit <- fit_schooling_consumption_bridge_specification(
    panel, "schooling", change_spec, adjustment
  )
  expect_equal(ancova_fit$estimate_per_percentage_point, 0.04, tolerance = 1e-10)
  expect_true(abs(change_fit$estimate_per_percentage_point - 0.04) > 1e-3)
})

test_that("schooling-consumption specification family stays bounded at 60 cells", {
  registry <- data.frame(
    welfare_specification_id = c(
      "long_2022__ancova", "long_2022__change",
      "long_2023__ancova", "long_2023__change"
    ),
    outcome_id = "real_mean_mpce",
    outcome_round = c("hces_2022_23", "hces_2022_23", "hces_2023_24", "hces_2023_24"),
    baseline_round = "nss_2004_05",
    estimand = c("ancova", "change", "ancova", "change"),
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )
  specs <- schooling_consumption_bridge_specifications(registry)
  expect_equal(nrow(specs), 60L)
  expect_identical(
    specs$analysis_id,
    paste("schooling_consumption_bridge", specs$specification_id, sep = "__")
  )
  expect_equal(length(unique(specs$treatment_id)), 5L)
  expect_equal(length(unique(specs$welfare_specification_id)), 4L)
  expect_identical(
    unique(specs$adjustment_id),
    c("unadjusted", "region_main", "state_main")
  )
})

test_that("schooling-consumption treatments derive shared semantics from canonical constructs", {
  constructs <- read_analysis_construct_registry()
  treatments <- schooling_consumption_bridge_treatment_registry(constructs)
  canonical <- analysis_construct_rows(constructs, treatments$treatment)

  expect_identical(treatments$label, canonical$label)
  expect_true(all(canonical$role %in% c("endogenous_treatment", "descriptive_treatment")))
})

test_that("schooling-consumption bridge can enforce common support across treatments", {
  n <- 24L
  panel <- data.frame(
    target_unit_2001 = sprintf("d%02d", seq_len(n)),
    state_code_2001 = rep(sprintf("%02d", 1:4), each = 6),
    region = rep(paste0("r", 1:4), each = 6),
    treatment_a = seq_len(n),
    treatment_b = seq_len(n) / 2,
    control_a = seq_len(n) / 3,
    stringsAsFactors = FALSE
  )
  panel$treatment_b[[2L]] <- NA_real_
  panel[[consumption_iv_variable_name("long_2022__change", "outcome")]] <- seq_len(n) / 10
  welfare <- data.frame(
    welfare_specification_id = "long_2022__change",
    estimand = "change",
    stringsAsFactors = FALSE
  )
  adjustments <- data.frame(
    specification_id = c("unadjusted", "state_main"),
    label = c("Unadjusted", "State"),
    fixed_effect = c("none", "state"),
    controls = I(list(character(), "control_a")),
    stringsAsFactors = FALSE
  )

  sample <- prepare_schooling_consumption_bridge_sample(
    panel, c("treatment_a", "treatment_b"), welfare, adjustments
  )
  expect_equal(nrow(sample), n - 1L)
  expect_false(any(sample$target_unit_2001 == "d02"))
})

test_that("conversion-gradient fit reports the change in schooling slope per modifier SD", {
  n_states <- 8L
  per_state <- 12L
  n <- n_states * per_state
  state <- rep(sprintf("%02d", seq_len(n_states)), each = per_state)
  treatment <- rep(seq(0, 20, length.out = per_state), n_states)
  # Keep the moderator non-affine in treatment within states. An earlier fixture
  # used two linear sequences, so treatment and the standardized moderator were
  # collinear after state FE and the main schooling slope was not identified.
  modifier_pattern <- c(-2, 0.5, -1.5, 1.5, -0.5, 2, 0, -1, 1, -1.8, 1.8, 0.2)
  modifier <- rep(modifier_pattern, n_states) +
    rep(seq(-0.4, 0.4, length.out = n_states), each = per_state)
  modifier_z <- as.numeric(scale(modifier))
  # Add deterministic residual variation so clustered inference is exercised on
  # a regular, non-perfect linear-model fixture. The behavioral check below
  # compares the reported scaling with the equivalent base-R model rather than
  # depending on a zero-residual data-generating process.
  residual_pattern <- c(
    -1.0, 0.4, 0.7, -0.3, 0.9, -0.8,
    0.2, 0.5, -0.6, 0.1, 0.8, -0.9
  )
  outcome <- 0.02 * treatment + 0.03 * treatment * modifier_z +
    rep(seq(-0.1, 0.1, length.out = n_states), each = per_state) +
    0.002 * rep(residual_pattern, n_states)
  panel <- data.frame(
    target_unit_2001 = sprintf("d%03d", seq_len(n)),
    state_code_2001 = state,
    region = state,
    schooling = treatment,
    moderator = modifier,
    stringsAsFactors = FALSE
  )
  panel[[consumption_iv_variable_name("long_2022__change", "outcome")]] <- outcome
  welfare <- data.frame(
    welfare_specification_id = "long_2022__change",
    estimand = "change",
    stringsAsFactors = FALSE
  )
  adjustment <- data.frame(
    specification_id = "state_main",
    label = "State",
    fixed_effect = "state",
    controls = I(list("moderator")),
    stringsAsFactors = FALSE
  )

  out <- fit_schooling_consumption_conversion_specification(
    panel, "schooling", welfare, adjustment, "moderator"
  )
  reference <- stats::lm(
    outcome ~ schooling * modifier_z + factor(state),
    data = data.frame(outcome, schooling = treatment, modifier_z, state)
  )
  reference_coef <- stats::coef(reference)
  expect_equal(
    out$schooling_slope_at_mean_modifier_per_10pp,
    10 * unname(reference_coef[["schooling"]]),
    tolerance = 1e-10
  )
  expect_equal(
    out$interaction_per_10pp_schooling_per_modifier_sd,
    10 * unname(reference_coef[["schooling:modifier_z"]]),
    tolerance = 1e-10
  )
  expect_true(is.finite(out$interaction_std_error_state_clustered))
})

test_that("conversion-gradient family is a six-cell common-support design", {
  registry <- data.frame(
    welfare_specification_id = c(
      "long_2022__ancova", "long_2022__change",
      "long_2023__ancova", "long_2023__change"
    ),
    outcome_id = "real_mean_mpce",
    outcome_round = c("hces_2022_23", "hces_2022_23", "hces_2023_24", "hces_2023_24"),
    baseline_round = "nss_2004_05",
    estimand = c("ancova", "change", "ancova", "change"),
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )
  specs <- schooling_consumption_conversion_specifications(registry)
  expect_equal(nrow(specs), 6L)
  expect_equal(anyDuplicated(specs$analysis_id), 0L)
  expect_setequal(specs$treatment_id, c("emi_all_children", "private_emi_all_children"))
  expect_setequal(
    specs$modifier_id,
    c("baseline_human_capital", "urbanization", "st_concentration")
  )
  expect_true(all(specs$welfare_specification_id == "long_2022__change"))
})
