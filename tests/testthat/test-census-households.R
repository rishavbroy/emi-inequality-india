test_that("HH08 parser enforces household-size exhaustion", {
  raw <- data.frame(matrix("", nrow = 6, ncol = 13), stringsAsFactors = FALSE)
  labels <- c("Total", "None", "1", "2", "3", "4+")
  totals <- c(100, 10, 20, 25, 20, 25)
  for (i in seq_along(labels)) {
    raw[i, 1:7] <- c("HH08", "01", "001", "District - Alpha", "Total", labels[[i]], totals[[i]])
    raw[i, 8:13] <- c(totals[[i]], 0, 0, 0, 0, 0)
  }
  out <- parse_census_hh08_2011_sheet(raw)
  expect_equal(nrow(out), 6L)
  raw[1, 8] <- 99
  expect_error(parse_census_hh08_2011_sheet(raw), "exhaust each row total")
})

test_that("HH10 parser uses the age-15+ household-size denominator", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 13), stringsAsFactors = FALSE)
  raw[1, 1:8] <- c("HH10", "01", "001", "District - Alpha", "Total",
    "Households with No matriculate and above", 80, 70)
  raw[1, 9:13] <- c(10, 10, 30, 15, 5)
  out <- parse_census_hh10_2011_sheet(raw)
  expect_equal(out$households, 80)
  expect_equal(out$households_age15_plus, 70)
  raw[1, 13] <- 4
  expect_error(parse_census_hh10_2011_sheet(raw), "denominator contract")
})

test_that("HH11 parser independently exhausts households and worker statuses", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 16), stringsAsFactors = FALSE)
  raw[1, 1:11] <- c("HH11", "01", "001", "District - Alpha", "Total", "Total",
    100, 190, 150, 25, 15)
  raw[1, 12:16] <- c(5, 15, 60, 15, 5)
  out <- parse_census_hh11_2011_sheet(raw)
  expect_equal(out$workers_total, 190)
  raw[1, 11] <- 14
  expect_error(parse_census_hh11_2011_sheet(raw), "accounting identity")
})

toy_hh08 <- function(state = "01", district = "001", total = 100) {
  data.frame(
    state_code = state, district_code = district, district_name = paste("District", district),
    households_total = total, households_no_literate = 10, households_1_literate = 20,
    households_2_literates = 25, households_3_literates = 20, households_4_plus_literates = 25,
    households_with_literate_member = 90, stringsAsFactors = FALSE
  )
}

toy_hh10 <- function(state = "01", district = "001") {
  data.frame(
    state_code = state, district_code = district, district_name = paste("District", district),
    households_with_literate_member = 90, households_no_matriculate = 60, households_age15_plus = 90,
    households_with_matriculate = 40, households_with_male_matriculate = 30,
    households_with_female_matriculate = 25, households_with_graduate = 15,
    households_with_male_graduate = 12, households_with_female_graduate = 8,
    stringsAsFactors = FALSE
  )
}

toy_hh11 <- function(state = "01", district = "001", total = 100) {
  data.frame(
    state_code = state, district_code = district, district_name = paste("District", district),
    households_total = total, households_no_workers = 10, households_1_worker = 30,
    households_2_workers = 35, households_3_workers = 15, households_4_plus_workers = 10,
    workers_total = 190, main_workers = 150, marginal_workers_3_6_months = 25,
    marginal_workers_lt3_months = 15, stringsAsFactors = FALSE
  )
}

test_that("HH08 literacy categories exhaust the household universe", {
  rows <- data.frame(
    state_code = "01", district_code = "001", district_name = "Alpha",
    category = c("Total", "None", "1", "2", "3", "4+"),
    households = c(100, 10, 20, 25, 20, 25), stringsAsFactors = FALSE
  )
  out <- summarise_census_hh08_2011_district(rows)
  expect_equal(out$households_total, 100)
  expect_equal(out$households_with_literate_member, 90)

  bad <- rows
  bad$households[bad$category == "4+"] <- 24
  expect_error(summarise_census_hh08_2011_district(bad), "do not exhaust")
})

test_that("HH10 education counts obey subset nesting without assuming sex categories are exclusive", {
  labels <- c(
    "Households with atleast one member literate", "Households with No matriculate and above",
    "Households with at least one  matriculate and above", "Households with at least one male  matriculate and above",
    "Households with at least one female  matriculate and above", "Households with at least one  graduate and above",
    "Households with at least one male graduate and above", "Households with at least one female graduate and above"
  )
  rows <- data.frame(
    state_code = "01", district_code = "001", district_name = "Alpha", category = labels,
    households = c(90, 60, 40, 30, 25, 15, 12, 8),
    households_age15_plus = c(80, 50, 40, 30, 25, 15, 12, 8), stringsAsFactors = FALSE
  )
  out <- summarise_census_hh10_2011_district(rows)
  expect_equal(out$households_with_matriculate, 40)
  expect_equal(out$households_with_female_graduate, 8)
  expect_gt(out$households_with_male_matriculate + out$households_with_female_matriculate,
            out$households_with_matriculate)

  bad <- rows
  bad$households[bad$category == "Households with at least one  graduate and above"] <- 41
  expect_error(summarise_census_hh10_2011_district(bad), "subset counts")
})

test_that("HH11 worker-count categories exhaust households and worker statuses exhaust workers", {
  rows <- data.frame(
    state_code = "01", district_code = "001", district_name = "Alpha",
    category = c("Total", "None", "1", "2", "3", "4+"),
    households = c(100, 10, 30, 35, 15, 10), stringsAsFactors = FALSE
  )
  rows$workers_total <- c(190, 0, 30, 70, 45, 45)
  rows$main_workers <- c(150, 0, 25, 55, 35, 35)
  rows$marginal_workers_3_6_months <- c(25, 0, 3, 10, 7, 5)
  rows$marginal_workers_lt3_months <- c(15, 0, 2, 5, 3, 5)
  out <- summarise_census_hh11_2011_district(rows)
  expect_equal(out$workers_total, out$main_workers + out$marginal_workers_3_6_months + out$marginal_workers_lt3_months)
  expect_equal(out$households_total, 100)
})

test_that("HH08 HH10 and HH11 reconcile independent household universes", {
  validation <- validate_census_2011_household_sources(toy_hh08(), toy_hh10(), toy_hh11())
  expect_equal(nrow(validation), 3L)
  expect_true(all(validation$max_abs_difference == 0))

  bad <- toy_hh10()
  bad$households_no_matriculate <- 59
  expect_error(validate_census_2011_household_sources(toy_hh08(), bad, toy_hh11()), "counts disagree")
})

test_that("household measures pool counts before constructing shares and intensities", {
  hh08 <- rbind(toy_hh08("01", "001", 100), toy_hh08("01", "002", 100))
  hh10 <- rbind(toy_hh10("01", "001"), toy_hh10("01", "002"))
  hh11 <- rbind(toy_hh11("01", "001", 100), toy_hh11("01", "002", 100))
  # Make the second child very different while preserving source identities.
  hh08[2, c("households_no_literate", "households_1_literate", "households_2_literates", "households_3_literates", "households_4_plus_literates", "households_with_literate_member")] <- c(30, 10, 20, 20, 20, 70)
  hh10[2, c("households_with_literate_member", "households_no_matriculate", "households_with_matriculate", "households_with_male_matriculate", "households_with_female_matriculate", "households_with_graduate", "households_with_male_graduate", "households_with_female_graduate")] <- c(70, 80, 20, 15, 12, 8, 6, 4)
  hh11[2, c("households_no_workers", "households_1_worker", "households_2_workers", "households_3_workers", "households_4_plus_workers", "workers_total", "main_workers", "marginal_workers_3_6_months", "marginal_workers_lt3_months")] <- c(30, 40, 20, 5, 5, 120, 90, 20, 10)

  transition <- data.frame(
    state_code_2011 = c("01", "01"), district_code_2011 = c("001", "002"),
    state_code_2001 = c("01", "01"), district_code_2001 = c("01", "01"),
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "deterministic_containment", stringsAsFactors = FALSE
  )
  out <- build_census_2011_household_measures(hh08, hh10, hh11, transition)
  expect_equal(nrow(out), 1L)
  expect_equal(out$no_literate_share_households, (10 + 30) / 200)
  expect_equal(out$matriculate_access_share_households_age15_plus, (40 + 20) / (90 + 90))
  expect_equal(out$workerless_share_households, (10 + 30) / 200)
  expect_equal(out$workers_per_household, (190 + 120) / 200)
  expect_equal(out$marginal_worker_share_workers, (40 + 30) / (190 + 120))
})


test_that("2001 household parsers preserve exact published accounting", {
  hh09 <- data.frame(matrix("", nrow = 6, ncol = 14), stringsAsFactors = FALSE)
  labels <- c("Total", "None", "1", "2", "3", "4+")
  totals <- c(100, 10, 20, 25, 20, 25)
  for (i in seq_along(labels)) {
    hh09[i, 1:8] <- c("HH09", "01", "01", "0000", "District - Alpha (01)", "TOTAL", labels[[i]], totals[[i]])
    hh09[i, 9:14] <- c(totals[[i]], 0, 0, 0, 0, 0)
  }
  expect_equal(nrow(parse_census_hh09_2001_sheet(hh09)), 6L)
  hh09[1, 9] <- 99
  expect_error(parse_census_hh09_2001_sheet(hh09), "exhaust each row total")

  hh13 <- data.frame(matrix("", nrow = 5, ncol = 14), stringsAsFactors = FALSE)
  categories <- c(
    "Households with No matriculate and above", "Households with at least one  matriculate and above",
    "Households with at least one female  matriculate and above", "Households with at least one  graduate and above",
    "Households with at least one female graduate and above"
  )
  values <- c(60, 40, 25, 15, 8)
  for (i in seq_along(categories)) {
    hh13[i, 1:8] <- c("HH13", "01", "01", "0000", "District - Alpha (01)", "TOTAL", categories[[i]], values[[i]])
    hh13[i, 9:14] <- c(values[[i]], 0, 0, 0, 0, 0)
  }
  hh13[1, 9:14] <- c(5, 5, 35, 10, 3, 2)
  parsed_hh13 <- parse_census_hh13_2001_sheet(hh13)
  expect_equal(nrow(parsed_hh13), 5L)
  expect_equal(summarise_census_hh13_2001_district(parsed_hh13)$households_age15_plus, 98)

  # Some published workbooks omit the redundant row total while retaining all
  # six age-15+ buckets. The buckets remain the authoritative denominator.
  hh13_without_totals <- hh13
  hh13_without_totals[, 8] <- ""
  parsed_without_totals <- parse_census_hh13_2001_sheet(hh13_without_totals)
  expect_true(all(is.na(parsed_without_totals$households)))
  expect_equal(
    summarise_census_hh13_2001_district(parsed_without_totals)$households_age15_plus,
    summarise_census_hh13_2001_district(parsed_hh13)$households_age15_plus
  )

  hh15 <- data.frame(matrix("", nrow = 6, ncol = 13), stringsAsFactors = FALSE)
  for (i in seq_along(labels)) {
    hh15[i, 1:8] <- c("HH15", "01", "01", "0000", "District - Alpha (01)", "TOTAL", labels[[i]], totals[[i]])
    hh15[i, 9:13] <- c(totals[[i]], 0, 0, 0, 0)
  }
  expect_equal(nrow(parse_census_hh15_2001_sheet(hh15)), 6L)
})

toy_hh09_2001 <- function() {
  data.frame(
    state_code = "01", district_code = "01", district_name = "Alpha",
    households_total = 100, households_no_literate = 10, households_1_literate = 20,
    households_2_literates = 25, households_3_literates = 20, households_4_plus_literates = 25,
    households_with_literate_member = 90, stringsAsFactors = FALSE
  )
}

toy_hh13_2001 <- function() {
  data.frame(
    state_code = "01", district_code = "01", district_name = "Alpha",
    households_age15_plus = 98, households_with_matriculate = 40,
    households_with_female_matriculate = 25, households_with_graduate = 15,
    households_with_female_graduate = 8, stringsAsFactors = FALSE
  )
}

toy_hh15_2001 <- function() {
  data.frame(
    state_code = "01", district_code = "01", district_name = "Alpha",
    households_total = 100, households_no_workers = 10, households_1_worker = 30,
    households_2_workers = 35, households_3_workers = 15, households_4_plus_workers = 10,
    stringsAsFactors = FALSE
  )
}

test_that("2001 HH13 denominator is bounded by the household universe", {
  bad <- toy_hh13_2001()
  bad$households_age15_plus <- 101
  expect_error(
    validate_census_2001_household_sources(toy_hh09_2001(), bad, toy_hh15_2001(), toy_hh15_2001()),
    "subset of the HH09 household universe"
  )
})

test_that("2001 HH15 Appendix validates worker categories without inventing worker totals", {
  validation <- validate_census_2001_household_sources(
    toy_hh09_2001(), toy_hh13_2001(), toy_hh15_2001(), toy_hh15_2001()
  )
  expect_true(all(validation$max_abs_difference == 0))

  bad <- toy_hh15_2001()
  bad$households_4_plus_workers <- 9
  expect_error(
    validate_census_2001_household_sources(toy_hh09_2001(), toy_hh13_2001(), toy_hh15_2001(), bad),
    "counts disagree"
  )
})

test_that("longitudinal household changes include only exact common concepts", {
  baseline <- build_census_2001_household_measures(
    toy_hh09_2001(), toy_hh13_2001(), toy_hh15_2001(), toy_hh15_2001()
  )
  followup <- data.frame(
    target_unit_2001 = "pc2001__01__01",
    census_2011_source_district_count = 1L,
    census_2011_parent_reconstruction_complete = TRUE,
    households_total = 120,
    no_literate_share_households = .05,
    two_plus_literate_share_households = .80,
    four_plus_literate_share_households = .40,
    matriculate_access_share_households_age15_plus = .50,
    female_matriculate_access_share_households_age15_plus = .30,
    graduate_access_share_households_age15_plus = .20,
    female_graduate_access_share_households_age15_plus = .10,
    workerless_share_households = .08,
    two_plus_worker_share_households = .70,
    four_plus_worker_share_households = .15,
    workers_per_household = 2.5,
    marginal_worker_share_workers = .2,
    stringsAsFactors = FALSE
  )
  out <- build_census_household_change_measures(baseline, followup)
  expect_equal(out$no_literate_share_households_change_2011_2001, .05 - .10)
  expect_equal(out$workerless_share_households_change_2011_2001, .08 - .10)
  expect_false(any(c(
    "workers_per_household_change_2011_2001",
    "marginal_worker_share_workers_change_2011_2001"
  ) %in% names(out)))
  expect_setequal(
    sub("_change_2011_2001$", "", grep("_change_2011_2001$", names(out), value = TRUE)),
    census_household_longitudinal_share_columns()
  )
})

test_that("household diagnostics retain both vintages, exact changes, and source validation", {
  baseline <- build_census_2001_household_measures(
    toy_hh09_2001(), toy_hh13_2001(), toy_hh15_2001(), toy_hh15_2001()
  )
  followup <- baseline[c("target_unit_2001", census_household_longitudinal_share_columns())]
  followup$census_2011_source_district_count <- 1L
  followup$census_2011_parent_reconstruction_complete <- TRUE
  followup$households_total <- 100
  change <- build_census_household_change_measures(baseline, followup)
  diagnostics <- build_census_household_diagnostics(
    toy_hh09_2001(), toy_hh13_2001(), toy_hh15_2001(), toy_hh15_2001(), baseline,
    toy_hh08(), toy_hh10(), toy_hh11(), followup, change
  )
  expect_setequal(names(diagnostics), c(
    "household_2001", "household_2011_harmonized_2001", "household_change_2011_2001",
    "change_coverage", "source_validation_2001", "source_validation_2011"
  ))
})
