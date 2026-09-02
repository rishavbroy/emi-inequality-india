test_that("safe_bind_rows unions columns and tolerates empty inputs", {
  out <- safe_bind_rows(list(
    data.frame(a = 1, b = "x"),
    NULL,
    data.frame(b = "y", c = 2),
    data.frame(a = integer(), d = logical())
  ))

  expect_equal(names(out), c("a", "b", "c", "d"))
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$a[2]))
  expect_true(is.na(out$c[1]))
  expect_true(all(is.na(out$d)))

  empty <- safe_bind_rows(list(
    data.frame(a = integer()),
    data.frame(b = character())
  ))
  expect_equal(names(empty), c("a", "b"))
  expect_equal(nrow(empty), 0L)
})

test_that("canon normalizes punctuation, case, and ampersands", {
  expect_equal(canon(" Jammu &  Kashmir!! "), "jammu and kashmir")
})

test_that("first_col finds exact and canonicalized names", {
  df <- data.frame("District Name" = "Patna", check.names = FALSE)

  expect_equal(first_col(df, c("missing", "District Name")), "District Name")
  expect_equal(first_col(df, c("district_name")), "District Name")
  expect_null(first_col(data.frame(), c("district")))
})

test_that("weighted mean and gini handle weights and invalid values", {
  expect_equal(wmean(c(1, 3), c(1, 3)), 2.5)
  expect_true(is.na(wmean(c(NA, 3), c(0, 0))))
  expect_equal(wgini(c(1, 1), c(1, 1)), 0)
  expect_gt(wgini(c(1, 3), c(1, 1)), 0)
})

test_that("duplicate-key collapsing permits exact repeats and rejects conflicts", {
  exact <- data.frame(id = c("a", "a", "b"), value = c(1, 1, 2), stringsAsFactors = FALSE)
  conflict <- data.frame(id = c("a", "a"), value = c(1, 2), stringsAsFactors = FALSE)

  collapsed <- collapse_identical_key_rows(exact, "id", context = "fixture")

  expect_equal(nrow(collapsed), 2L)
  expect_identical(collapsed$id, c("a", "b"))
  expect_error(
    collapse_identical_key_rows(conflict, "id", context = "fixture"),
    "fixture has duplicate keys with non-identical rows"
  )
})

test_that("safe_share uses the district-control percentage-point contract", {
  expect_equal(safe_share(c(2, 1), c(5, 4)), c(40, 25))
  expect_equal(safe_share(2, 5, scale = 1e5), 40000)
  expect_true(is.na(safe_share(1, 0)))
})
