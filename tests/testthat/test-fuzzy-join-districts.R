test_that("evaluate_distances works on known pairs", {
  pairs <- tibble::tribble(~str1, ~str2, "Sikim", "Sikkim")
  out <- evaluate_distances(pairs, c("osa"), c(1))
  expect_true(out$match[1])
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
