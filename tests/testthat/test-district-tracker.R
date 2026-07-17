test_that("build_district_tracker preserves source file ids and row positions", {
  raw <- list(
    alluvial = data.frame(district = c("A", "B")),
    tracker = data.frame(district = "C")
  )

  out <- build_district_tracker(raw)

  expect_setequal(out$source_file_id, c("alluvial", "tracker"))
  expect_equal(out$.row_in_source[out$source_file_id == "alluvial"], c(1L, 2L))
})


test_that("district tracker parsers expose canonical source and target keys", {
  raw <- list(alluvial = data.frame(
    state_from = "Bihar",
    district_from = "Patna",
    state_to = "Bihar",
    district_to = "Patna Rural",
    year_from = 2001,
    year_to = 2020,
    change_type = "name_change"
  ))

  out <- build_district_tracker(raw)

  expect_true(all(c("source_type", "source_state_key", "source_district_key", "target_state_key", "target_district_key") %in% names(out)))
  expect_equal(out$source_type, "alluvial")
  expect_equal(out$source_state_key, canonicalize_state_name("Bihar"))
  expect_equal(out$source_district_key, canon("Patna"))
  expect_equal(out$target_district_key, canon("Patna Rural"))
})
