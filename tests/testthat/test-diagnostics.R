test_that("district tracker diagnostics summarize source row counts", {
  raw <- list(a = data.frame(x = 1:2), b = data.frame(x = 3))

  out <- diagnose_district_tracker_sources(raw, data.frame(), list())

  expect_equal(out$n_rows, c(2L, 1L))
})

test_that("district and fuzzy matching diagnostics return table counts", {
  district_panel <- data.frame(id = 1:2)
  join_map <- data.frame(id = 1:3)

  district_out <- diagnose_district_matching(district_panel, join_map, list())
  fuzzy_out <- diagnose_fuzzy_matching(data.frame(id = 1), join_map, list())

  expect_equal(district_out$n_panel_rows, 2L)
  expect_equal(district_out$n_join_rows, 3L)
  expect_equal(fuzzy_out$n_tracker_rows, 1L)
})

test_that("AME benchmark diagnostic is skipped unless enabled", {
  out <- diagnose_ame_benchmark(list(), data.frame(x = 1), list(run_diagnostics = list(ame_benchmark = FALSE)))

  expect_equal(out$status, "skipped")
})
