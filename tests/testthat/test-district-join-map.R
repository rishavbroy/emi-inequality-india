test_that("evaluate_distances works on known pairs with method-specific backends", {
  skip_if_not_installed("stringdist")
  pairs <- tibble::tribble(~str1, ~str2, "Sikim", "Sikkim")
  out <- evaluate_distances(pairs, c("jw", "osa"), c(0.15, 1))

  expect_true(all(out$match))
  expect_setequal(out$method, c("jw", "osa"))
  expect_gt(length(unique(out$distance)), 1L)
})

test_that("prepare_district_join_map validates and annotates the reviewed crosswalk", {
  crosswalk <- data.frame(
    state_01 = "A", district_01 = "Old",
    state_07 = "A", district_07 = "Middle",
    state_17 = "A", district_17 = "New",
    state_20 = "A", district_20 = "New",
    stringsAsFactors = FALSE
  )

  out <- prepare_district_join_map(crosswalk)

  expect_equal(out$.tracker_row, 1L)
  expect_equal(out$source, "harmonization_crosswalk")
  expect_equal(out$match_status, "reviewed_crosswalk_row")
  expect_s3_class(attr(out, "unmatched_rows"), "data.frame")
  expect_equal(nrow(attr(out, "unmatched_rows")), 0L)
  expect_error(
    prepare_district_join_map(data.frame(state_01 = "A")),
    "missing required columns"
  )
})

test_that("many-to-many match flags mark all duplicated source and tracker links", {
  join_map <- data.frame(
    .source_row = c(1, 1, 2, 3),
    .tracker_row = c(10, 11, 10, 12)
  )

  out <- flag_many_to_many_matches(join_map)

  expect_true(out$many_to_many[1])
  expect_false(out$many_to_many[3])
  expect_equal(out$many_to_many_type[2], "one_source_to_many_tracker")
  expect_equal(out$many_to_many_type[3], "many_source_to_one_tracker")
  expect_equal(out$many_to_many_type[4], "one_to_one")
})
