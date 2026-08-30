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

test_that("Census 2011 D03 reasons form an exhaustive migrant partition", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 32), stringsAsFactors = FALSE)
  raw[, 1] <- "D0603"
  raw[, 2] <- "09"
  raw[, 3] <- "132"
  raw[, 4] <- "Alpha"
  raw[, 5] <- "Total"
  raw[, 6] <- "All durations of residence"
  raw[, 7] <- "Total"
  raw[, 8] <- "Total"
  raw[, 9] <- 100
  raw[, c(12, 15, 18, 21, 24, 27, 30)] <- c(20, 5, 10, 30, 5, 20, 10)

  parsed <- parse_census_d03_2011_sheet(raw)
  out <- summarise_census_d03_2011_district(parsed)
  measures <- add_census_d03_reason_shares(out)

  expect_equal(out$migrants_total, 100)
  expect_equal(sum(unlist(out[c(
    "work_employment", "business", "education", "marriage",
    "moved_after_birth", "moved_with_household", "other_reason"
  )], use.names = FALSE)), 100)
  expect_equal(measures$work_employment_share_among_migrants, 0.2)
  expect_equal(measures$education_share_among_migrants, 0.1)

  parsed$other_reason <- parsed$other_reason - 1
  expect_error(
    summarise_census_d03_2011_district(parsed),
    "do not sum exactly"
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
})

test_that("Census 2001 D03 is not exposed as a district migration source", {
  expect_error(
    census_migration_manifest_files(build_paths(tempdir()), 2001, "D03"),
    "do not provide district rows"
  )
})
