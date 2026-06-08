test_that("build_district_tracker preserves source file ids and row positions", {
  raw <- list(
    alluvial = data.frame(district = c("A", "B")),
    tracker = data.frame(district = "C")
  )

  out <- build_district_tracker(raw)

  expect_setequal(out$source_file_id, c("alluvial", "tracker"))
  expect_equal(out$.row_in_source[out$source_file_id == "alluvial"], c(1L, 2L))
})

test_that("join_district_panel delegates to active district-panel builder", {
  measures_2007 <- data.frame(state_std = "bihar", district_std = "patna", district_panel_id = "id")
  out <- join_district_panel(data.frame(), data.frame(), measures_2007, data.frame(), data.frame(), data.frame(), list())

  expect_equal(out$district_panel_id, "id")
})
