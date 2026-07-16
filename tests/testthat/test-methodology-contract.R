test_that("selection probit covariate contract matches the legacy Rmd", {
  legacy_vars <- c(
    "AGE", "SEX", "HH_SIZE", "RELIGION", "SOCIAL_GROUP", "SECTOR",
    "DIST_FROM_NEAREST_PRIMARY_CLASS", "father_educ",
    "dmean_num_IS_EDU_FREE", "dmean_num_TUTION_FEE_WAIVED",
    "dmean_num_RECD_SCHOLARSHIP_STIPEND", "dmean_num_RECD_TXT_BOOKS",
    "dmean_num_RECD_STATIONERY", "dmean_num_MID_DAY_MEAL_ETC_RECD",
    "dmean_num_ENROLLMENT_COST"
  )
  selection_data <- as.data.frame(
    stats::setNames(replicate(length(legacy_vars), numeric(), simplify = FALSE), legacy_vars)
  )

  expect_equal(selection_probit_variables(selection_data), legacy_vars)
})

test_that("selection survey design preserves legacy stratification and lonely-PSU policy", {
  skip_if_not_installed("survey")
  old_lonely <- getOption("survey.lonely.psu")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  selection_data <- data.frame(
    enrolled = factor(c("No", "Yes", "No", "Yes"), levels = c("No", "Yes")),
    AGE = c(8, 9, 10, 11),
    FSU_SL_NO = c(101, 102, 201, 202),
    weight = c(1.5, 2, 1.25, 1.75),
    STATE = c("10", "10", "11", "11"),
    STRATUM = c("01", "01", "02", "02"),
    SUB_STRATUM_NO = c("1", "1", "2", "2"),
    stringsAsFactors = FALSE
  )

  design <- build_survey_design_selection(selection_data)

  expect_true(any(c("survey.design", "survey.design2") %in% class(design)))
  expect_equal(getOption("survey.lonely.psu"), "average")
  expect_true(".survey_strata" %in% names(design$variables))
  expect_equal(length(unique(design$variables$.survey_strata)), 2L)
})

test_that("EMIE uses Block 5 English-medium code 02 among children at most 19", {
  b5 <- data.frame(
    district_code = c("01001", "01001", "01001", "01002"),
    AGE = c(10, 12, 20, 9),
    MEDIUM_INSTRUCTION = c("02", "01", "02", "02"),
    weight = c(1, 3, 100, 2),
    stringsAsFactors = FALSE
  )

  out <- compute_emie_2007(b5)

  expect_equal(out$emie_2007[out$district_code_0708 == "01001"], 25)
  expect_equal(out$emie_2007[out$district_code_0708 == "01002"], 100)
})

test_that("2007 household measures retain the district-HHID de-duplication correction", {
  b3 <- data.frame(
    district_code = c("01001", "01001", "01001"),
    HHID = c("h1", "h1", "h2"),
    HH_SIZE = c(2, 2, 4),
    TOTAL = c(200, 200, 800),
    weight = c(1, 1, 2),
    stringsAsFactors = FALSE
  )

  out <- compute_education_household_measures_2007(b3)

  expect_equal(out$npeople_0708, 10)
  expect_equal(out$nhouses_0708, 3)
  expect_equal(out$consumption_2007, (1 * 100 + 2 * 200) / 3)
})

test_that("baseline controls preserve legacy weighted dependency-ratio semantics", {
  b4 <- data.frame(
    district_code = rep("01001", 4),
    weight = c(1, 2, 3, 4),
    AGE = c(10, 30, 70, 40),
    SEX = c(1, 2, 2, 1),
    RELATION_TO_HEAD = c(3, 1, 1, 1),
    SECTOR = c(1, 2, 1, 2),
    RELIGION = c(1, 2, 1, 3),
    SOCIAL_GROUP = c("1", "2", "3", "9"),
    HH_SIZE = c(4, 4, 2, 2),
    EDUCATION_LEVEL = c("1", "8", "3", "10"),
    LAND_POSSESSED_CODE = c("01", "05", "08", "03"),
    stringsAsFactors = FALSE
  )

  out <- compute_baseline_controls_2007(b4)

  expect_equal(out$dependency_ratio, 100 * (1 + 3) / (2 + 4))
  expect_equal(out$pct_fem_head, 100 * (2 + 3) / 10)
  expect_equal(out$pct_medium_land, 100 * 2 / 10)
  expect_equal(out$pct_large_land, 100 * 3 / 10)
})

test_that("district pseudo-panel analysis core requires the IV instrument, treatment, and outcomes", {
  panel <- data.frame(
    EMIE = c(10, 20, 30),
    wavg_ling_degrees = c(1, 2, NA),
    consumption_0708 = c(100, NA, 300),
    consumption_1718 = c(150, 250, 350)
  )

  expect_equal(panel_has_analysis_core(panel), c(TRUE, FALSE, FALSE))
})

test_that("district source matching keeps a one-to-one correction to the legacy fuzzy cascade", {
  skip_if_not_installed("stringdist")
  source <- data.frame(
    .source_row = c(1L, 2L),
    .source_state_key = c("bihar", "bihar"),
    .source_district_key = c("patna", "patna"),
    stringsAsFactors = FALSE
  )
  tracker <- data.frame(
    .tracker_row = 10L,
    .source_state_key = "bihar",
    .source_district_key = "patna",
    stringsAsFactors = FALSE
  )

  out <- select_source_tracker_matches(source, tracker)

  expect_equal(nrow(out), 1L)
  expect_equal(out$.tracker_row, 10L)
  expect_equal(out$.source_row, 1L)
})
