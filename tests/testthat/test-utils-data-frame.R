test_that("safe_bind_rows unions columns and drops empty inputs", {
  out <- safe_bind_rows(list(
    data.frame(a = 1, b = "x"),
    NULL,
    data.frame(b = "y", c = 2)
  ))

  expect_equal(names(out), c("a", "b", "c"))
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$a[2]))
  expect_true(is.na(out$c[1]))
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
