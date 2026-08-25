test_that("sector MPCE reconstruction uses person rather than household weights", {
  households <- data.frame(
    sector = c("Rural", "rural", "Urban"),
    household_size = c(1, 9, 2),
    survey_weight = c(1, 1, 1),
    nominal_mpce = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  out <- estimate_consumption_mpce_by_sector(households)
  expect_equal(out$estimate_mpce[out$sector == "rural"], 190)
  expect_equal(out$estimate_mpce[out$sector == "urban"], 300)
  expect_equal(out$sample_households, c(2L, 1L))
})


test_that("MPCE reconstruction reports the invalid canonical field", {
  households <- data.frame(
    sector = c("Rural", "mystery"),
    household_size = c(2, 2),
    survey_weight = c(1, 1),
    nominal_mpce = c(100, 100),
    stringsAsFactors = FALSE
  )
  expect_error(
    estimate_consumption_mpce_by_sector(households),
    "invalid values: sector=1"
  )
})

test_that("official MPCE reconstruction validation is blocking and returns diagnostics", {
  households <- data.frame(
    sector = c("1", "1", "2", "2"),
    household_size = c(1, 3, 2, 2),
    survey_weight = c(1, 1, 1, 1),
    nominal_mpce = c(100, 200, 300, 500),
    stringsAsFactors = FALSE
  )
  benchmarks <- data.frame(
    survey_id = c("round", "round"),
    sector = c("rural", "urban"),
    mpce_definition = c("MRP", "MRP"),
    expected_mpce = c(175, 400),
    tolerance_abs_rupees = c(0.01, 0.01),
    source_label = "fixture",
    source_url = "https://example.invalid",
    stringsAsFactors = FALSE
  )
  out <- validate_consumption_mpce_reconstruction(households, benchmarks, "round")
  expect_true(all(out$passed))
  expect_equal(out$estimate_mpce, c(175, 400))

  benchmarks$expected_mpce[[1]] <- 170
  expect_error(
    validate_consumption_mpce_reconstruction(households, benchmarks, "round"),
    "failed official benchmark"
  )
})

test_that("MPCE benchmark metadata require unique survey-sector identities", {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "survey_id,sector,mpce_definition,expected_mpce,tolerance_abs_rupees,source_label,source_url",
    "round,rural,MRP,100,1,fixture,https://example.invalid",
    "round,rural,MRP,100,1,fixture,https://example.invalid"
  ), path)
  expect_error(read_consumption_mpce_benchmarks(path), "unique by survey_id and sector")
})

test_that("modern HCES official benchmark metadata cover both sectors in both rounds", {
  benchmarks <- read_consumption_mpce_benchmarks(file.path(
    Sys.getenv("EMI_PROJECT_ROOT", "."),
    "data", "metadata", "consumption_mpce_benchmarks.csv"
  ))
  modern <- benchmarks[benchmarks$survey_id %in% c("hces_2022_23", "hces_2023_24"), , drop = FALSE]
  expect_equal(nrow(modern), 4L)
  expect_true(all(vapply(
    split(modern$sector, modern$survey_id),
    function(x) setequal(x, c("rural", "urban")),
    logical(1)
  )))
  expect_true(all(modern$tolerance_abs_rupees == 1))
})

test_that("consumption reconstruction savers use the shared diagnostic CSV contract", {
  validation <- data.frame(
    survey_id = "round",
    sector = "rural",
    estimate_mpce = 100,
    expected_mpce = 100,
    passed = TRUE,
    stringsAsFactors = FALSE
  )
  coverage <- data.frame(
    survey_id = "hces_2023_24",
    questionnaire = c("F", "C", "D"),
    n_households = c(10L, 10L, 10L),
    n_summary_present = c(10L, 10L, 9L),
    n_summary_zero_filled = c(0L, 0L, 1L),
    share_summary_zero_filled = c(0, 0, 0.1),
    stringsAsFactors = FALSE
  )

  dir <- tempfile("consumption-reconstruction-output-")
  mpce_path <- file.path(dir, "public", "mpce.csv")
  coverage_path <- file.path(dir, "extended", "coverage.csv")

  written_mpce <- save_consumption_mpce_validation(validation, mpce_path)
  written_coverage <- save_hces_summary_coverage(coverage, coverage_path)

  expect_true(file.exists(written_mpce))
  expect_true(file.exists(written_coverage))
  expect_equal(
    utils::read.csv(written_mpce, stringsAsFactors = FALSE),
    validation,
    ignore_attr = TRUE
  )
  expect_equal(
    utils::read.csv(written_coverage, stringsAsFactors = FALSE),
    coverage,
    ignore_attr = TRUE
  )
})

test_that("HCES summary coverage saver returns the normalized output path", {
  coverage <- data.frame(
    survey_id = "hces_2022_23",
    questionnaire = "F",
    n_households = 1L,
    n_summary_present = 1L,
    n_summary_zero_filled = 0L,
    share_summary_zero_filled = 0,
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".csv")
  written <- save_hces_summary_coverage(coverage, path)

  expect_identical(
    written,
    normalizePath(path, mustWork = FALSE)
  )
})
