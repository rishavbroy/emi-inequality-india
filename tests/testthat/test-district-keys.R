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
