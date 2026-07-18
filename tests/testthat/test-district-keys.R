test_that("canonical names are stable", {
  expect_equal(canonicalize_district_name(" East  Godavari! "), "east godavari")
})

test_that("state canonicalization covers observed NSS spelling variants", {
  raw <- c("Andhra Pardesh", "Gujrat", "Maharastra", "Andaman & Nicober", "Pondicheri", "Uttaranchal", "Orissa")

  expect_equal(
    canonicalize_state_name(raw),
    c("andhra pradesh", "gujarat", "maharashtra", "andaman and nicobar islands", "puducherry", "uttarakhand", "odisha")
  )
})

test_that("key_df returns typed empty district keys for unrecognizable inputs", {
  out <- key_df(data.frame(other = character()), 2007L)

  expect_equal(names(out), c("state_std", "district_std", "source_year", "district_key"))
  expect_equal(nrow(out), 0L)
})

test_that("district key construction handles list and data-frame inputs", {
  education <- list(block = data.frame(State = "Bihar", District = "Patna"))
  consumption <- data.frame(State = "Bihar", District = "Gaya")

  out <- build_district_keys_2007(education, consumption)

  expect_setequal(out$district_std, c("patna", "gaya"))
  expect_true(all(out$source_year == 2007L))
  expect_false(anyDuplicated(out$district_key) > 0L)
})


test_that("district source matching configuration is internally consistent", {
  for (suffix in c("08", "18", "01")) {
    chain <- district_source_suffix_chain(suffix)
    expect_identical(chain[[1]], suffix)
    expect_false(anyDuplicated(chain) > 0L)
  }

  methods <- district_source_match_methods()
  thresholds <- district_source_match_thresholds()
  expect_gt(length(methods), 0L)
  expect_length(thresholds, length(methods))
  expect_false(anyDuplicated(methods) > 0L)
  expect_true(all(is.finite(thresholds) & thresholds >= 0))
})

test_that("district source matcher keeps deterministic one-to-one matches", {
  skip_if_not_installed("stringdist")
  source <- data.frame(
    .source_row = c(1L, 2L),
    .source_state_key = c("bihar", "bihar"),
    .source_district_key = c("patna", "gaya"),
    stringsAsFactors = FALSE
  )
  tracker <- data.frame(
    .tracker_row = c(10L, 11L),
    .source_state_key = c("bihar", "bihar"),
    .source_district_key = c("patna", "gaya"),
    stringsAsFactors = FALSE
  )

  out <- select_source_tracker_matches(source, tracker)

  expect_equal(out$.source_row, c(1L, 2L))
  expect_equal(out$.tracker_row, c(10L, 11L))
  expect_true(all(out$.source_match_distance == 0))
})
