test_that("manual corrections include reasons", {
  corrections <- readr::read_csv(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "data", "metadata", "manual_district_corrections.csv"), show_col_types = FALSE)
  expect_true("reason" %in% names(corrections))
})

test_that("manual correction application records auditable correction table", {
  path <- tempfile(fileext = ".csv")
  corrections <- data.frame(
    correction_id = "c1",
    source_year = 2007L,
    source_dataset = "toy",
    state_raw = "Bihar",
    district_raw = "Patna",
    correction_type = "typo",
    reason = "Toy correction for test",
    stringsAsFactors = FALSE
  )
  utils::write.csv(corrections, path, row.names = FALSE)

  tracker <- apply_manual_district_corrections(data.frame(x = 1), path)

  expect_equal(attr(tracker, "manual_corrections")$reason, "Toy correction for test")
})

test_that("manual correction validation requires audit columns", {
  expect_error(
    validate_manual_corrections(data.frame(correction_id = "c1"), data.frame()),
    "Manual corrections missing columns:"
  )
})
