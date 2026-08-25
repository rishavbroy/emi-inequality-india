test_that("consumption survey registry is self-describing and distinguishes legacy from planned welfare sources", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))

  expect_equal(anyDuplicated(registry$survey_id), 0L)
  expect_true(all(vapply(seq_len(nrow(registry)), function(i) {
    length(survey_period_months(registry[i, , drop = FALSE])) == 12L
  }, logical(1))))
  expect_identical(
    consumption_survey_spec(registry, "nss_2007_08_education")$mpce_contract[[1]],
    "single_household_consumption_question"
  )
  expect_identical(
    consumption_survey_spec(registry, "nss_2007_08_consumption")$mpce_contract[[1]],
    "schedule_1_0_summary"
  )
  expect_identical(
    consumption_survey_spec(registry, "nss_2017_18_education")$mpce_contract[[1]],
    "single_shot_umpce_classification"
  )
  expect_true(all(
    registry$mpce_contract[registry$survey_id %in% c("hces_2022_23", "hces_2023_24")] ==
      "three_questionnaire_official_mpce"
  ))
  modern <- consumption_survey_spec(registry, "hces_2022_23")
  expect_equal(range(survey_period_months(modern)), as.Date(c("2022-08-01", "2023-07-01")))
})

test_that("registry validation rejects ambiguous or malformed survey contracts", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))

  duplicate <- rbind(registry, registry[1, , drop = FALSE])
  expect_error(validate_consumption_survey_registry(duplicate), "survey_id values must be non-empty and unique")

  bad_timing <- registry
  bad_timing$price_timing[[1]] <- "invented"
  expect_error(validate_consumption_survey_registry(bad_timing), "Unsupported consumption price_timing")

  bad_dates <- registry
  bad_dates$survey_end[[1]] <- as.Date("2000-12-31")
  expect_error(validate_consumption_survey_registry(bad_dates), "exactly 12 survey months")
})


test_that("registry defaults resolve from the project root rather than the working directory", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  expected <- read_consumption_survey_registry(build_paths(root))
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)

  actual <- read_consumption_survey_registry()
  expect_equal(actual, expected)
  expect_equal(nss_wave_months(2007), survey_period_months(consumption_survey_spec(expected, "nss_2007_08_education")))
})

test_that("registry reader rejects non-rectangular CSV input before column shifting", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "survey_id,survey_family",
    "one,family,unexpected"
  ), path)
  on.exit(unlink(path), add = TRUE)

  expect_error(
    read_consumption_survey_registry_file(path),
    "rectangular CSV"
  )
})

test_that("registered consumption price window follows implemented survey adapters", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  window <- registered_consumption_price_window(registry)

  expect_equal(window$start_period, as.Date("2004-07-01"))
  expect_equal(window$end_period, as.Date("2024-07-01"))
  expect_equal(window$first_survey_id, "nss_2004_05")
  expect_equal(window$last_survey_id, "hces_2023_24")

  implemented <- registry$household_adapter != "legacy_schedule_pending"
  expect_equal(
    window$start_period,
    as.Date(format(min(registry$survey_start[implemented]), "%Y-%m-01"))
  )
  expect_equal(
    window$end_period,
    as.Date(format(max(registry$survey_end[implemented]), "%Y-%m-01"))
  )
})

test_that("pending legacy survey adapters do not expand the production price window", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  pending <- registry$survey_id == "nss_2000_01"
  registry$survey_start[pending] <- as.Date("1998-07-01")
  registry$survey_end[pending] <- as.Date("1999-06-30")

  window <- registered_consumption_price_window(registry)
  expect_equal(window$start_period, as.Date("2004-07-01"))
})
