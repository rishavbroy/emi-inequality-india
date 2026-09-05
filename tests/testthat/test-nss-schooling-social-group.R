test_that("NSS schooling margins reuse the canonical child universe within social groups", {
  children <- data.frame(
    district_code_0708 = rep("01001", 8),
    AGE = rep(10, 8),
    SOCIAL_GROUP = factor(
      c(rep("Scheduled Tribe", 4), rep("Other", 4)),
      levels = nss_2007_schooling_social_groups()
    ),
    enrolled = factor(c("Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes"),
                      levels = c("No", "Yes")),
    MEDIUM_INSTRUCTION = c("02", "01", NA, "02", "02", "01", "01", "01"),
    TYPE_OF_INSTT = c(1, 4, NA, 4, 3, 4, 1, 2),
    weight = rep(1, 8),
    stringsAsFactors = FALSE
  )

  out <- build_education_exposure_2007_by_social_group(children)
  st <- out[out$social_group == "Scheduled Tribe", , drop = FALSE]
  other <- out[out$social_group == "Other", , drop = FALSE]

  expect_equal(st$enrollment_rate_0708, 75)
  expect_equal(other$enrollment_rate_0708, 100)
  expect_equal(st$emi_share_enrolled_0708, 2 / 3 * 100)
  expect_equal(other$emi_share_enrolled_0708, 25)
  expect_silent(validate_education_exposure_identity(st))
  expect_silent(validate_education_exposure_identity(other))
})

test_that("NSS schooling access gaps are within-district contrasts against Other children", {
  margins <- data.frame(
    district_code_0708 = rep(c("a", "b"), each = 2),
    social_group = rep(c("Scheduled Tribe", "Other"), 2),
    stringsAsFactors = FALSE
  )
  registry <- nss64_schooling_social_group_margin_registry()
  for (outcome in registry$outcome) margins[[outcome]] <- c(20, 30, 40, 35)
  margins$state_code_2001 <- rep(c("09", "13"), each = 2)
  margins$district_code_2001 <- rep(c("01", "02"), each = 2)
  margins$ling_distance_nonzero_mean <- rep(c(1, 2), each = 2)
  margins$hindi_belt_2001 <- TRUE
  margins$baseline_control <- rep(c(0.2, 0.4), each = 2)

  gaps <- build_nss64_schooling_social_group_gaps(
    margins, covariates = "baseline_control"
  )
  enrollment <- gaps[
    gaps$social_group == "Scheduled Tribe" & gaps$outcome == "enrollment_rate_0708",
    , drop = FALSE
  ]
  expect_equal(enrollment$gap_percentage_points, c(-10, 5))
  expect_equal(enrollment$baseline_control, c(0.2, 0.4))
  expect_error(
    build_nss64_schooling_social_group_gaps(margins, covariates = "missing_control"),
    "covariates are missing"
  )

  summary <- nss64_schooling_social_group_access_summary(gaps)
  row <- summary[
    summary$social_group == "Scheduled Tribe" & summary$outcome == "enrollment_rate_0708",
    , drop = FALSE
  ]
  expect_equal(row$n_common_districts, 2L)
  expect_equal(row$mean_district_gap_percentage_points, -2.5)
  expect_equal(row$share_districts_group_below_other, 0.5)
})

test_that("distance heterogeneity estimates how within-district group gaps vary with distance", {
  gaps <- expand.grid(
    state_code_2001 = c("09", "13", "21"),
    ling_distance_nonzero_mean = c(0, 2, 4),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  gaps$district_code_0708 <- sprintf("d%02d", seq_len(nrow(gaps)))
  gaps$district_code_2001 <- sprintf("%02d", seq_len(nrow(gaps)))
  gaps$social_group <- "Scheduled Tribe"
  gaps$reference_group <- "Other"
  gaps$outcome <- "enrollment_rate_0708"
  gaps$group_value <- 50
  gaps$reference_value <- 50
  state_effect <- c("09" = 1, "13" = -2, "21" = 3)
  orthogonal_noise <- rep(c(1, -2, 1), each = 3)
  gaps$gap_percentage_points <-
    1.5 * gaps$ling_distance_nonzero_mean +
    unname(state_effect[gaps$state_code_2001]) + orthogonal_noise
  gaps$hindi_belt_2001 <- TRUE

  fit <- fit_nss64_schooling_social_group_gap(
    gaps, "Scheduled Tribe", "enrollment_rate_0708", controls = character()
  )
  expect_equal(fit$estimate, 1.5, tolerance = 1e-10)
  expect_equal(fit$n_states, 3L)
  expect_true(is.finite(fit$std_error_state_clustered))
})


test_that("NSS-64 social-group model grid is canonical and complete", {
  specs <- nss64_schooling_social_group_specifications()
  expect_equal(nrow(specs), 30L)
  expect_equal(anyDuplicated(specs$specification_id), 0L)
  expect_setequal(specs$social_group, nss64_schooling_disadvantaged_groups())
  expect_setequal(specs$sample, c("all_states", "hindi_belt"))
  expect_true(all(specs$hindi_belt_only == (specs$sample == "hindi_belt")))
})
