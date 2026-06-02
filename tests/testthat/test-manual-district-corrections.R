test_that("manual corrections include reasons", {
  corrections <- readr::read_csv(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "data", "metadata", "manual_district_corrections.csv"), show_col_types = FALSE)
  expect_true("reason" %in% names(corrections))
})
