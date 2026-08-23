test_that("Census C-13 parser keeps district total-person rows for completed ages 6-13", {
  raw_2001 <- data.frame(matrix("", nrow = 10, ncol = 7), stringsAsFactors = FALSE)
  raw_2001[, 1] <- "C3713"
  raw_2001[, 2] <- "09"
  raw_2001[, 3] <- c("00", rep("01", 9))
  raw_2001[, 4] <- c("0000", rep("0000", 8), "0001")
  raw_2001[, 5] <- c("State - TEST", rep("District - Alpha 01", 9))
  raw_2001[, 6] <- c("6", as.character(6:13), "6")
  raw_2001[, 7] <- c("999", as.character(seq(100, 800, 100)), "777")

  parsed_2001 <- parse_census_c13_sheet(raw_2001, 2001)
  expect_equal(parsed_2001$age, 6:13)
  expect_true(all(parsed_2001$district_code == "01"))
  expect_true(all(parsed_2001$subdistrict_code == "0000"))
  expect_equal(sum(parsed_2001$persons), 3600)

  raw_2011 <- data.frame(matrix("", nrow = 9, ncol = 6), stringsAsFactors = FALSE)
  raw_2011[, 1] <- "C3713"
  raw_2011[, 2] <- "09"
  raw_2011[, 3] <- c("000", rep("132", 8))
  raw_2011[, 4] <- c("State - TEST", rep("District - Alpha (01)", 8))
  raw_2011[, 5] <- c("6", as.character(6:13))
  raw_2011[, 6] <- c("999", as.character(seq(10, 80, 10)))

  parsed_2011 <- parse_census_c13_sheet(raw_2011, 2011)
  expect_equal(parsed_2011$age, 6:13)
  expect_true(all(parsed_2011$district_code == "132"))
  expect_equal(sum(parsed_2011$persons), 360)
})

test_that("Census C-13 age denominator requires every single age exactly once", {
  good <- data.frame(
    state_code = "09", district_code = "01", district_name = "Alpha",
    age = 6:13, persons = seq(10, 80, 10), stringsAsFactors = FALSE
  )
  out <- summarise_census_c13_age_6_13(good, 2001)
  expect_equal(out$census_age_6_13_population, 360)

  missing_age <- good[good$age != 9, , drop = FALSE]
  expect_error(
    summarise_census_c13_age_6_13(missing_age, 2001),
    "exactly one observation for every age 6-13"
  )
  duplicate_age <- rbind(good, good[good$age == 6, , drop = FALSE])
  expect_error(
    summarise_census_c13_age_6_13(duplicate_age, 2001),
    "duplicate district-by-single-age"
  )
})

test_that("2011 C-13 counts are summed only through deterministic 2001 containments", {
  age_2011 <- data.frame(
    state_code = c("09", "09", "09"),
    district_code = c("132", "133", "134"),
    district_name = c("Child A", "Child B", "Non-nested"),
    census_age_6_13_population = c(100, 200, 900),
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("09", "09", "09"),
    district_code_2011 = c("132", "133", "134"),
    state_code_2001 = c("09", "09", "09"),
    district_code_2001 = c("01", "01", "02"),
    population_share_to_2001 = c(1, 1, 0.9),
    area_share_to_2001 = c(1, 1, 0.9),
    shrid_coverage = 1,
    mapping_class = c(
      "official_lgd_census_code_bridge",
      "deterministic_containment",
      "non_nested_or_incomplete"
    ),
    stringsAsFactors = FALSE
  )

  out <- harmonize_census_2011_age_6_13_to_2001(age_2011, transition)
  expect_equal(nrow(out), 1L)
  expect_identical(out$target_unit_2001[[1]], "pc2001__09__01")
  expect_equal(out$census_age_6_13_population_2011, 300)
  expect_equal(out$census_2011_source_district_count, 2L)
  expect_true(out$census_2011_parent_reconstruction_complete)
})

test_that("2011 C-13 parent anchors are withheld when deterministic children reconstruct only part of a 2001 district", {
  age_2011 <- data.frame(
    state_code = c("01", "01"),
    district_code = c("008", "009"),
    district_name = c("Retained child", "New child"),
    census_age_6_13_population = c(200, 100),
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("01", "01"),
    district_code_2011 = c("008", "009"),
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("02", "02"),
    population_share_to_2001 = c(0.9995, 1),
    area_share_to_2001 = c(0.995, 1),
    shrid_coverage = c(0.996, 1),
    mapping_class = c("non_nested_or_incomplete", "deterministic_containment"),
    stringsAsFactors = FALSE
  )

  bridge <- build_census_2011_to_2001_age_bridge(transition)
  out <- harmonize_census_2011_age_6_13_to_2001(age_2011, transition)

  expect_equal(nrow(bridge), 0L)
  expect_equal(nrow(out), 0L)
})

test_that("age-6-13 population uses log-linear Census-anchor interpolation and explicit extrapolation", {
  anchors <- data.frame(
    target_unit_2001 = "pc2001__09__01",
    census_2001_district_name = "Alpha",
    census_age_6_13_population_2001 = 100,
    census_age_6_13_population_2011 = 200,
    census_2011_source_district_count = 1L,
    census_2011_source_districts = "Alpha",
    census_age_6_13_anchor_status = "two_census_anchors",
    census_age_6_13_annual_log_growth_2001_2011 = log(2) / 10,
    stringsAsFactors = FALSE
  )
  out <- project_census_age_6_13_population(
    anchors,
    c("2005-06", "2011-12")
  )
  expected_2005 <- 100 * exp(log(2) / 10 * 4.5)
  expect_equal(
    out$census_age_6_13_population[out$academic_year == "2005-06"],
    expected_2005
  )
  expect_identical(
    out$census_age_6_13_population_method[out$academic_year == "2005-06"],
    "log_linear_interpolation_2001_2011"
  )
  expect_identical(
    out$census_age_6_13_population_method[out$academic_year == "2011-12"],
    "log_linear_extrapolation_post_2011"
  )
})

test_that("DISE age-denominator construct is a gross enrollment ratio and is not capped at 100", {
  dise <- data.frame(
    target_unit_2001 = "pc2001__09__01",
    academic_year = "2007-08",
    dise_english_enrollment = 120,
    dise_total_enrollment = 180,
    stringsAsFactors = FALSE
  )
  population <- data.frame(
    target_unit_2001 = "pc2001__09__01",
    academic_year = "2007-08",
    census_age_6_13_population = 100,
    census_age_6_13_population_2001 = 90,
    census_age_6_13_population_2011 = 110,
    census_age_6_13_annual_log_growth_2001_2011 = log(110 / 90) / 10,
    census_age_6_13_population_method = "log_linear_interpolation_2001_2011",
    stringsAsFactors = FALSE
  )

  out <- attach_dise_age_6_13_exposure(dise, population)
  expect_equal(out$dise_emi_gross_enrollment_ratio_age_6_13, 120)
  expect_equal(out$dise_elementary_gross_enrollment_ratio_age_6_13, 180)
})
