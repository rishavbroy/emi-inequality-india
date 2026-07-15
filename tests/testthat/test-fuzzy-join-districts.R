test_that("evaluate_distances works on known pairs with method-specific backends", {
  skip_if_not_installed("stringdist")
  pairs <- tibble::tribble(~str1, ~str2, "Sikim", "Sikkim")
  out <- evaluate_distances(pairs, c("jw", "osa"), c(0.15, 1))

  expect_true(all(out$match))
  expect_setequal(out$method, c("jw", "osa"))
  expect_gt(length(unique(out$distance)), 1L)
})

test_that("fuzzy_join_districts exposes diagnostic columns and attributes", {
  keys_2007 <- data.frame(
    state_std = "bihar",
    district_std = "patna",
    source_year = 2007L,
    district_key = "2007__bihar__patna"
  )

  out <- fuzzy_join_districts(data.frame(), data.frame(), keys_2007, data.frame(), data.frame(), list())

  expect_true(all(c("match_status", "possible_false_positive", "many_to_many") %in% names(out)))
  expect_s3_class(attr(out, "unmatched_rows"), "data.frame")
  expect_s3_class(attr(out, "possible_false_positives"), "data.frame")
  expect_s3_class(attr(out, "many_to_many_cases"), "data.frame")
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
