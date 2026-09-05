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
  set.seed(202)
  n <- 90L
  state <- rep(sprintf("%02d", 1:9), each = 10)
  treatment <- rep(seq(-2, 2, length.out = 10), times = 9)
  baseline <- stats::rnorm(n)
  noise <- rep(c(-1, 0, 1, 0, -1, 1, 0, 1, -1, 0), times = 9) / 20
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
  expect_equal(length(unique(specs$treatment_id)), 5L)
  expect_equal(length(unique(specs$welfare_specification_id)), 4L)
  expect_identical(
    unique(specs$adjustment_id),
    c("unadjusted", "region_main", "state_main")
  )
})
