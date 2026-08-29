historical_persistence_fixture <- function() {
  state <- rep(c("02", "03"), each = 4)
  district <- rep(sprintf("%02d", 1:4), 2)
  d91 <- c(1, 2, 3, 4, 1.2, 2.2, 3.2, 4.2)
  historical <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    atlas_population_1991 = c(100, 120, 140, 160, 110, 130, 150, 170),
    min_accepted_coverage = 0.99,
    historical_language_status = "eligible",
    ling_distance_nonzero_mean_1991 = d91,
    stringsAsFactors = FALSE
  )
  geography <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    population_coverage = c(rep(1, 3), 0.995, rep(1, 3), 0.80),
    n_target_2001_districts = c(rep(1L, 7), 2L),
    exact_language_persistence = c(rep(TRUE, 3), FALSE, rep(TRUE, 3), FALSE),
    preferred_language_persistence = c(rep(TRUE, 7), FALSE),
    stringsAsFactors = FALSE
  )
  one_target <- data.frame(
    state_code_1991 = state[1:7],
    district_code_1991 = district[1:7],
    state_code_2001 = state[1:7],
    district_code_2001 = district[1:7],
    stringsAsFactors = FALSE
  )
  split_target <- data.frame(
    state_code_1991 = rep("03", 2),
    district_code_1991 = rep("04", 2),
    state_code_2001 = rep("03", 2),
    district_code_2001 = c("04", "05"),
    stringsAsFactors = FALSE
  )
  transition <- rbind(one_target, split_target)
  current <- data.frame(
    state_code_2001 = c(state, "03"),
    district_code_2001 = c(district, "05"),
    ling_distance_nonzero_mean = c(
      2 * d91[1:4],
      2 * d91[5:8] + 1,
      10
    ),
    stringsAsFactors = FALSE
  )
  list(
    historical = historical,
    current = current,
    geography = list(source_districts = geography, transition = transition)
  )
}

test_that("historical persistence keeps preferred and exact geography distinct", {
  fixture <- historical_persistence_fixture()
  out <- build_historical_linguistic_persistence_validation(
    fixture$historical, fixture$current, fixture$geography
  )

  split <- out$panel[
    out$panel$state_code_1991 == "03" & out$panel$district_code_1991 == "04",
    , drop = FALSE
  ]
  high_coverage <- out$panel[
    out$panel$state_code_1991 == "02" & out$panel$district_code_1991 == "04",
    , drop = FALSE
  ]
  expect_identical(split$persistence_status, "geography_not_preferred")
  expect_true(high_coverage$persistence_status == "eligible")
  expect_false(high_coverage$exact_language_persistence)

  preferred <- out$summary[out$summary$sample == "preferred_geography", , drop = FALSE]
  exact <- out$summary[out$summary$sample == "exact_one_to_one", , drop = FALSE]
  expect_equal(preferred$n_districts, 7L)
  expect_equal(exact$n_districts, 6L)
  expect_equal(preferred$min_accepted_coverage, 0.99)
  expect_equal(preferred$state_fe_population_weighted_slope, 2, tolerance = 1e-10)
  expect_true(is.finite(preferred$population_weighted_pearson))
  expect_true(is.finite(preferred$population_weighted_spearman))
  expect_equal(sum(out$quintile_transition$n_districts[out$quintile_transition$sample == "preferred_geography"]), 7L)
  expect_equal(sum(out$quintile_transition$n_districts[out$quintile_transition$sample == "exact_one_to_one"]), 6L)
})

test_that("historical persistence fails closed on stale thresholds and bridge summaries", {
  fixture <- historical_persistence_fixture()
  mixed_threshold <- fixture$historical
  mixed_threshold$min_accepted_coverage[[1]] <- 0.98
  expect_error(
    build_historical_linguistic_persistence_validation(
      mixed_threshold, fixture$current, fixture$geography
    ),
    "one explicit accepted-speaker coverage threshold"
  )

  stale <- fixture$geography
  stale$source_districts$n_target_2001_districts[[1]] <- 2L
  stale$source_districts$preferred_language_persistence[[1]] <- FALSE
  expect_error(
    build_historical_linguistic_persistence_validation(
      fixture$historical, fixture$current, stale
    ),
    "summary and transition disagree"
  )
})

test_that("weighted historical correlations are defined from population weights", {
  x <- c(1, 2, 4, 5)
  y <- c(2, 1, 5, 4)
  w <- c(1, 2, 3, 4)
  expected <- stats::cov.wt(cbind(x, y), wt = w, cor = TRUE)$cor[1, 2]
  expected_rank <- stats::cov.wt(
    cbind(rank(x), rank(y)), wt = w, cor = TRUE
  )$cor[1, 2]

  expect_equal(historical_linguistic_weighted_correlation(x, y, w), expected)
  expect_equal(
    historical_linguistic_weighted_correlation(x, y, w, rank_values = TRUE),
    expected_rank
  )
  expect_true(is.na(historical_linguistic_weighted_correlation(rep(1, 4), y, w)))
})
