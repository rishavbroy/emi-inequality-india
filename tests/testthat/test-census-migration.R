test_that("Census D02 district summary preserves origin and duration accounting", {
  raw <- data.frame(matrix("", nrow = 6, ncol = 28), stringsAsFactors = FALSE)
  raw[, 1] <- "D0302"
  raw[, 2] <- "09"
  raw[, 3] <- "01"
  raw[, 4] <- "District - Alpha * 01"
  raw[, 5] <- unname(census_d02_origin_labels())
  raw[, 6] <- "Total"
  raw[, 7] <- "Total"
  raw[, 8] <- c(100, 70, 40, 30, 20, 5)
  raw[1, 11] <- 10
  raw[1, 14] <- 20
  raw[1, 17] <- 15
  raw[1, 20] <- 25
  raw[1, 23] <- 20
  raw[1, 26] <- 10

  parsed <- parse_census_d02_sheet(raw, 2001)
  out <- summarise_census_d02_district(parsed, 2001)

  expect_equal(nrow(out), 1L)
  expect_identical(out$district_name[[1L]], "Alpha")
  expect_equal(out$migrants_total, 100)
  expect_equal(out$migrants_recent_0_9, 45)
  expect_equal(
    out$migrants_within_state_outside_place,
    out$migrants_within_district + out$migrants_other_district_same_state
  )
  expect_equal(out$migrants_interstate, 20)
})

test_that("Census D02 rejects inconsistent within-state migration accounting", {
  raw <- data.frame(matrix("", nrow = 6, ncol = 28), stringsAsFactors = FALSE)
  raw[, 1] <- "D0302"
  raw[, 2] <- "09"
  raw[, 3] <- "132"
  raw[, 4] <- "Alpha"
  raw[, 5] <- unname(census_d02_origin_labels())
  raw[, 6] <- "Total"
  raw[, 7] <- "Total"
  raw[, 8] <- c(100, 71, 40, 30, 20, 5)
  raw[1, c(11, 14, 17)] <- c(10, 20, 15)

  parsed <- parse_census_d02_sheet(raw, 2011)
  expect_error(
    summarise_census_d02_district(parsed, 2011),
    "within-state migration identity fails"
  )
})

test_that("Census 2011 D03 reasons and recent-work validation rows preserve accounting", {
  raw <- data.frame(matrix("", nrow = 7, ncol = 32), stringsAsFactors = FALSE)
  raw[, 1] <- "D0603"
  raw[, 2] <- "09"
  raw[, 3] <- "132"
  raw[, 4] <- "Alpha"
  raw[, 5] <- "Total"

  raw[1, 6] <- "All durations of residence"
  raw[1, 7] <- "Total"
  raw[1, 8] <- "Total"
  raw[1, 9] <- 100
  raw[1, c(12, 15, 18, 21, 24, 27, 30)] <- c(20, 5, 10, 30, 5, 20, 10)

  recent <- expand.grid(
    duration = census_recent_duration_labels(),
    type = c("Rural", "Urban"),
    stringsAsFactors = FALSE
  )
  raw[2:7, 6] <- recent$duration
  raw[2:7, 7] <- "Last residence within India"
  raw[2:7, 8] <- recent$type
  raw[2:7, 12] <- c(1, 2, 3, 4, 5, 6)

  parsed <- parse_census_d03_2011_sheet(raw)
  out <- summarise_census_d03_2011_district(parsed)
  measures <- add_census_d03_reason_shares(out)

  expect_equal(out$migrants_total, 100)
  expect_equal(sum(unlist(out[c(
    "work_employment", "business", "education", "marriage",
    "moved_after_birth", "moved_with_household", "other_reason"
  )], use.names = FALSE)), 100)
  expect_equal(out$recent_0_9_work_employment_within_india_classified_origin, 21)
  expect_equal(measures$work_employment_share_among_migrants, 0.2)
  expect_equal(measures$education_share_among_migrants, 0.1)

  parsed$other_reason[parsed$duration == "All durations of residence"] <- 9
  expect_error(
    summarise_census_d03_2011_district(parsed),
    "do not sum exactly"
  )

  incomplete <- parsed[
    !(parsed$duration == census_recent_duration_labels()[[1L]] &
      parsed$last_residence_type == "Rural"),
    ,
    drop = FALSE
  ]
  expect_error(
    summarise_census_d03_2011_district(incomplete),
    "recent-work validation rows are incomplete"
  )
})

test_that("Census 2001 D02 uses the validated Census population denominator", {
  d02 <- data.frame(
    census_year = 2001L, state_code = "09", district_code = "01", district_name = "Alpha",
    migrants_total = 100, migrants_recent_0_9 = 40,
    migrants_within_state_outside_place = 70, migrants_within_district = 40,
    migrants_other_district_same_state = 30, migrants_interstate = 20,
    migrants_outside_india = 5, stringsAsFactors = FALSE
  )
  population <- data.frame(
    state_code_2001 = "09", district_code_2001 = "01", population_total = 1000,
    stringsAsFactors = FALSE
  )

  out <- build_census_d02_2001_measures(d02, population)
  expect_equal(out$migrant_stock_share_population, 0.1)
  expect_equal(out$recent_0_9_migrant_share_population, 0.04)
  expect_equal(out$interstate_migrant_share_population, 0.02)
  expect_equal(out$interstate_share_among_migrants, 0.2)

  expect_error(
    build_census_d02_2001_measures(d02, population[FALSE, , drop = FALSE]),
    "district coverage differ"
  )
  bad_population <- population
  bad_population$population_total <- 50
  expect_error(
    build_census_d02_2001_measures(d02, bad_population),
    "incompatible with district population denominators"
  )
})

test_that("Census 2011 D02 and D03 agree on district migrant totals", {
  d02 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"),
    migrants_total = c(100, 200), stringsAsFactors = FALSE
  )
  d03 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"),
    migrants_total = c(100, 200), stringsAsFactors = FALSE
  )

  out <- validate_census_2011_migration_totals(d02, d03)
  expect_equal(out$n_districts, 2L)
  expect_equal(out$max_abs_total_difference, 0)

  d03$migrants_total[[2L]] <- 199
  expect_error(
    validate_census_2011_migration_totals(d02, d03),
    "totals disagree or district coverage differs"
  )
  expect_error(
    validate_census_2011_migration_totals(d02, d03[-1L, , drop = FALSE]),
    "totals disagree or district coverage differs"
  )
})

test_that("Census 2011 counts are pooled before migration shares are recomputed", {
  d02 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"),
    district_name = c("Child A", "Child B"), census_year = 2011L,
    migrants_total = c(100, 300), migrants_recent_0_9 = c(50, 150),
    migrants_within_state_outside_place = c(70, 180),
    migrants_within_district = c(40, 100),
    migrants_other_district_same_state = c(30, 80),
    migrants_interstate = c(10, 90), migrants_outside_india = c(5, 15),
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("09", "09"), district_code_2011 = c("132", "133"),
    state_code_2001 = c("09", "09"), district_code_2001 = c("01", "01"),
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = c("official_lgd_census_code_bridge", "deterministic_containment"),
    stringsAsFactors = FALSE
  )

  out <- build_census_d02_2011_measures(d02, transition)

  expect_equal(nrow(out), 1L)
  expect_equal(out$migrants_total, 400)
  expect_equal(out$migrants_interstate, 100)
  expect_equal(out$interstate_share_among_migrants, 0.25)
  expect_false(isTRUE(all.equal(out$interstate_share_among_migrants, mean(c(0.1, 0.3)))))
  expect_equal(out$census_2011_source_district_count, 2L)
})

test_that("Census migration harmonization withholds partial 2001 parent reconstructions", {
  d02 <- data.frame(
    state_code = c("01", "01"), district_code = c("008", "009"),
    district_name = c("Retained child", "New child"), census_year = 2011L,
    migrants_total = c(100, 50), migrants_recent_0_9 = c(40, 20),
    migrants_within_state_outside_place = c(70, 30),
    migrants_within_district = c(40, 20),
    migrants_other_district_same_state = c(30, 10),
    migrants_interstate = c(20, 10), migrants_outside_india = c(5, 2),
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("01", "01"), district_code_2011 = c("008", "009"),
    state_code_2001 = c("01", "01"), district_code_2001 = c("02", "02"),
    population_share_to_2001 = c(0.9995, 1), area_share_to_2001 = c(0.995, 1),
    shrid_coverage = c(0.996, 1),
    mapping_class = c("non_nested_or_incomplete", "deterministic_containment"),
    stringsAsFactors = FALSE
  )

  out <- build_census_d02_2011_measures(d02, transition)
  expect_equal(nrow(out), 0L)
  expect_identical(
    names(out),
    c(
      "target_unit_2001", "census_2011_source_district_count",
      "census_2011_source_districts", "census_2011_parent_reconstruction_complete",
      census_d02_count_columns(), "census_year",
      "recent_0_9_share_among_migrants", "within_district_share_among_migrants",
      "other_district_same_state_share_among_migrants", "interstate_share_among_migrants",
      "outside_india_share_among_migrants"
    )
  )
  expect_identical(out$census_year, integer())
})

test_that("Census D03 harmonization preserves its empty output schema", {
  d03 <- data.frame(
    state_code = "01", district_code = "008", district_name = "Partial child",
    census_year = 2011L, migrants_total = 100, work_employment = 20,
    business = 5, education = 10, marriage = 30, moved_after_birth = 5,
    moved_with_household = 20, other_reason = 10,
    recent_0_9_work_employment_within_india_classified_origin = 12,
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = "01", district_code_2011 = "008",
    state_code_2001 = "01", district_code_2001 = "02",
    population_share_to_2001 = 0.9995, area_share_to_2001 = 0.995,
    shrid_coverage = 0.996, mapping_class = "non_nested_or_incomplete",
    stringsAsFactors = FALSE
  )

  out <- build_census_d03_2011_measures(d03, transition)
  expect_equal(nrow(out), 0L)
  expect_identical(out$census_year, integer())
  expect_true(all(census_d03_reason_count_columns() %in% names(out)))
  expect_true(all(
    paste0(setdiff(census_d03_reason_count_columns(), "migrants_total"), "_share_among_migrants") %in%
      names(out)
  ))
})

test_that("Census 2001 D03 is not exposed as a district migration source", {
  expect_error(
    census_migration_manifest_files(build_paths(tempdir()), 2001, "D03"),
    "do not provide district rows"
  )
})

test_that("Census 2011 D04 retains a complete education partition and explicit residual", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 32), stringsAsFactors = FALSE)
  raw[, 1] <- "D0904"
  raw[, 2] <- "09"
  raw[, 3] <- "132"
  raw[, 4] <- "Alpha"
  raw[, 5] <- "Total"
  raw[, 6] <- "All durations of residence"
  raw[, 7] <- "All ages"
  raw[, 8] <- "Total"
  raw[, c(9, 12, 15, 18, 21, 24, 27, 30)] <- c(100, 20, 80, 30, 20, 5, 15, 5)

  out <- summarise_census_d04_2011_district(parse_census_d04_2011_sheet(raw))
  measures <- add_census_d04_education_shares(out)

  expect_equal(out$migrants_total, out$migrants_illiterate + out$migrants_literate)
  expect_equal(out$migrants_literate_education_not_classified, 5)
  expect_equal(measures$literate_share_among_migrants, 0.8)
  expect_equal(measures$graduate_or_technical_degree_share_among_migrants, 0.2)
  expect_equal(measures$technical_credential_share_among_migrants, 0.1)

  raw[, 15] <- 79
  expect_error(
    summarise_census_d04_2011_district(parse_census_d04_2011_sheet(raw)),
    "inconsistent migrant education counts"
  )
})

test_that("Census 2011 D07 pools four origin cells before computing skill shares", {
  raw <- data.frame(matrix("", nrow = 4, ncol = 31), stringsAsFactors = FALSE)
  raw[, 1] <- "D1207"
  raw[, 2] <- "09"
  raw[, 3] <- "132"
  raw[, 4] <- "Alpha"
  raw[, 5] <- "Total"
  raw[, 6] <- unname(census_d07_origin_labels())
  raw[, 7] <- "All ages"
  raw[, 8] <- c(10, 20, 30, 40)
  raw[, 11] <- c(2, 4, 6, 8)
  raw[, 14] <- c(8, 16, 24, 32)
  raw[, 17] <- c(3, 6, 9, 12)
  raw[, 20] <- c(2, 4, 6, 8)
  raw[, 23] <- c(1, 2, 3, 4)
  raw[, 26] <- c(1, 2, 3, 4)
  raw[, 29] <- c(1, 1, 1, 1)

  out <- summarise_census_d07_2011_district(parse_census_d07_2011_sheet(raw))
  measures <- add_census_d07_work_migrant_shares(out)

  expect_equal(out$recent_work_migrants_total, 100)
  expect_equal(out$recent_work_migrants_within_state, 30)
  expect_equal(out$recent_work_migrants_outside_state, 70)
  expect_equal(out$recent_work_migrants_rural_origin, 40)
  expect_equal(out$recent_work_migrants_urban_origin, 60)
  expect_equal(measures$outside_state_share_among_recent_work_migrants, 0.7)
  expect_equal(measures$rural_origin_share_among_recent_work_migrants, 0.4)
  expect_equal(
    measures$graduate_or_technical_degree_share_among_recent_work_migrants,
    (10 + 4) / 100
  )

  incomplete <- raw[-1L, , drop = FALSE]
  expect_error(
    summarise_census_d07_2011_district(parse_census_d07_2011_sheet(incomplete)),
    "missing one or more work-migrant origin rows"
  )
})

test_that("Census migration cross-table validators compare observed source counts", {
  d02 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"),
    migrants_total = c(100, 200), stringsAsFactors = FALSE
  )
  d04 <- d02
  expect_equal(
    validate_census_2011_d02_d04_totals(d02, d04)$max_abs_difference,
    0
  )

  d03 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"),
    recent_0_9_work_employment_within_india_classified_origin = c(40, 80),
    stringsAsFactors = FALSE
  )
  d07 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"),
    recent_work_migrants_total = c(40, 80), stringsAsFactors = FALSE
  )
  expect_equal(
    validate_census_2011_d03_d07_recent_work(d03, d07)$max_abs_difference,
    0
  )

  d07$recent_work_migrants_total[[2L]] <- 79
  expect_error(
    validate_census_2011_d03_d07_recent_work(d03, d07),
    "counts disagree or district coverage differs"
  )
})

test_that("Census 2001 migration validity inputs cover the IV panel exactly", {
  panel <- data.frame(
    state_code_2001 = c("09", "09"),
    district_code_2001 = c("01", "02"),
    outcome = c(1, 2),
    stringsAsFactors = FALSE
  )
  d02 <- data.frame(
    state_code = c("09", "09"), district_code = c("01", "02"),
    migrant_stock_share_population = c(0.1, 0.2),
    recent_0_9_migrant_share_population = c(0.03, 0.04),
    interstate_migrant_share_population = c(0.01, 0.02),
    other_district_same_state_share_among_migrants = c(0.2, 0.3),
    stringsAsFactors = FALSE
  )
  out <- prepare_census_migration_validity_panel(panel, d02)
  expect_equal(nrow(out), nrow(panel))
  expect_equal(out$migrant_stock_share_population, c(0.1, 0.2))

  expect_error(
    prepare_census_migration_validity_panel(panel, d02[-1L, , drop = FALSE]),
    "do not cover the full IV panel"
  )
})

test_that("migration balance diagnostics apply Holm correction within specification", {
  balance <- data.frame(
    specification_id = rep(c("a", "b"), each = 3),
    p.value = c(0.01, 0.03, 0.20, 0.01, NA, 0.5),
    status = c(rep("estimated", 4), "not_estimated", "estimated"),
    stringsAsFactors = FALSE
  )
  out <- add_census_migration_balance_multiplicity(balance)
  expect_equal(out$p_holm_within_spec[1:3], stats::p.adjust(balance$p.value[1:3], "holm"))
  expect_true(is.na(out$p_holm_within_spec[[5L]]))
})

test_that("migration first-stage sensitivity uses one common sample for baseline and augmented controls", {
  set.seed(123)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = sprintf("%02d", rep(1:12, each = 10)),
    district_code_2001 = rep(sprintf("%02d", 1:10), 12),
    region = rep(panel_region_levels(), length.out = n),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  for (variable in census_2001_diagnostic_controls()) {
    panel[[variable]] <- stats::rnorm(n)
  }
  for (variable in census_migration_balance_variables()) {
    panel[[variable]] <- stats::runif(n, 0, 0.4)
  }
  treatment <- preferred_iv_variables()$treatment
  panel[[treatment]] <- 0.4 * panel$ling_distance_nonzero_mean +
    0.2 * panel$migrant_stock_share_population + stats::rnorm(n)

  out <- estimate_census_migration_first_stage_sensitivity(panel)
  expect_setequal(out$migration_adjustment, c("baseline", "plus_migration"))
  expect_equal(nrow(out), 4L)
  expect_equal(length(unique(out$n)), 1L)
  expect_true(all(out$n_migration_controls[out$migration_adjustment == "plus_migration"] == 3L))
  expect_true(all(is.finite(out$joint_excluded_f)))
  expect_true(all(is.finite(out$partial_r_squared)))
})
