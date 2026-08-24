test_that("consumption survey design uses person weights and nested NSS identifiers", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h2", "h3", "h4"),
    source_state_code = c("01", "01", "02", "02"),
    sector = c("Rural", "Rural", "1", "1"), subround = "1",
    fsu = c("1", "2", "1", "2"), stratum = "1", sub_stratum = "1",
    household_size = c(1, 3, 1, 1), target_unit_2001 = c("a", "a", "b", "b"),
    lineage_status = "resolved_exact_2001",
    lineage_person_weight = c(1, 3, 1, 1), real_mpce = c(100, 200, 300, 500),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_equal(length(unique(rows$.design_stratum)), 2L)
  expect_equal(length(unique(rows$.design_psu)), 4L)

  out <- estimate_consumption_district_mean(x)
  a <- out[out$district_2001 == "a", ]
  expect_equal(a$estimate, 175, tolerance = 1e-8)
  expect_equal(a$n_households, 2L)
  expect_equal(a$n_fsu, 2L)
  expect_equal(a$kish_effective_n, 16 / 10, tolerance = 1e-8)
  expect_equal(a$status, "estimated")
})

test_that("district support uses allocation-adjusted person weights", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h1", "h2"),
    source_state_code = "01", sector = "1", subround = "1",
    fsu = c("1", "1", "2"), stratum = "1", sub_stratum = "1",
    household_size = c(2, 2, 1), target_unit_2001 = c("a", "b", "a"),
    lineage_status = "resolved_reviewed_consensus",
    lineage_person_weight = c(2, 2, 1), real_mpce = c(100, 100, 200),
    stringsAsFactors = FALSE
  )
  out <- consumption_district_support(x)
  a <- out[out$district_2001 == "a", ]
  expect_equal(a$n_households, 2L)
  expect_equal(a$n_fsu, 2L)
  expect_equal(a$kish_effective_n, 9 / 5, tolerance = 1e-8)
})

test_that("unresolved households never enter the survey design", {
  x <- data.frame(
    survey_id = "wave", household_id = c("resolved", "unresolved"),
    source_state_code = "01", sector = "1", subround = "1",
    fsu = c("1", "2"), stratum = "1", sub_stratum = "1",
    household_size = 1, target_unit_2001 = c("a", NA),
    lineage_status = c("resolved_exact_2001", "unresolved_no_stable_lineage"),
    lineage_person_weight = c(1, NA), real_mpce = c(100, 999),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_equal(rows$household_id, "resolved")
  expect_equal(rows$real_mpce, 100)
})

test_that("Round 66 urban blank sub-strata represent no sub-stratification", {
  x <- data.frame(
    survey_id = "nss_2009_10_type1", household_id = c("u1", "u2"),
    source_state_code = "07", sector = "Urban", subround = "1",
    fsu = c("1", "2"), stratum = "10", sub_stratum = c("", NA),
    household_size = 1, target_unit_2001 = "delhi",
    lineage_status = "resolved_exact_2001",
    lineage_person_weight = 1, real_mpce = c(100, 200),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_identical(unique(rows$.design_sub_stratum), "__none__")
  expect_equal(length(unique(rows$.design_stratum)), 1L)
})

test_that("rural blank sub-strata remain invalid design data", {
  x <- data.frame(
    survey_id = "nss_2009_10_type1", household_id = "r1",
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = "1", stratum = "1", sub_stratum = "", household_size = 1,
    target_unit_2001 = "a", lineage_status = "resolved_exact_2001",
    lineage_person_weight = 1, real_mpce = 100, stringsAsFactors = FALSE
  )
  expect_error(consumption_design_rows(x), "sub_stratum=1")
})

test_that("district means intentionally handle lonely-PSU survey warnings", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h2"),
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = c("1", "2"), stratum = c("1", "2"), sub_stratum = "1",
    household_size = 1, target_unit_2001 = c("a", "a"),
    lineage_status = "resolved_exact_2001",
    lineage_person_weight = 1, real_mpce = c(100, 200), stringsAsFactors = FALSE
  )
  expect_warning(out <- estimate_consumption_district_mean(x), NA)
  expect_equal(out$estimate, 150)
})
