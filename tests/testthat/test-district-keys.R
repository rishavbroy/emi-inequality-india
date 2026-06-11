test_that("canonical names are stable", {
  expect_equal(canonicalize_district_name(" East  Godavari! "), "east godavari")
})

test_that("state canonicalization covers legacy NSS spelling variants", {
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


test_that("legacy district source suffix chains start from the source survey year", {
  expect_equal(legacy_suffix_chain("08")[1:4], c("08", "07", "06", "05"))
  expect_equal(legacy_suffix_chain("18")[1:3], c("18", "17", "19"))
  expect_equal(legacy_suffix_chain("01")[1], "01")
})

test_that("legacy district source matcher exposes original fuzzy cascade", {
  expect_equal(legacy_source_match_methods(), c("soundex", "qgram", "jw", "dl", "osa"))
  expect_equal(legacy_source_match_thresholds(), c(0, 0, 0.15, 2, 1))
})

test_that("legacy district source matcher keeps deterministic one-to-one matches", {
  skip_if_not_installed("stringdist")
  source <- data.frame(
    .source_row = c(1L, 2L),
    .legacy_state_key = c("bihar", "bihar"),
    .legacy_district_key = c("patna", "gaya"),
    stringsAsFactors = FALSE
  )
  tracker <- data.frame(
    .tracker_row = c(10L, 11L),
    .legacy_state_key = c("bihar", "bihar"),
    .legacy_district_key = c("patna", "gaya"),
    stringsAsFactors = FALSE
  )

  out <- legacy_select_source_tracker_matches(source, tracker)

  expect_equal(out$.source_row, c(1L, 2L))
  expect_equal(out$.tracker_row, c(10L, 11L))
  expect_true(all(out$.legacy_match_distance == 0))
})
