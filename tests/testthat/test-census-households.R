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

test_that("household diagnostics persist only measures and source validation", {
  diagnostics <- build_census_household_diagnostics(
    toy_hh08(), toy_hh10(), toy_hh11(),
    data.frame(target_unit_2001 = "pc2001__01__01", households_total = 100)
  )
  expect_setequal(names(diagnostics), c("household_2011_harmonized_2001", "source_validation_2011"))
})
