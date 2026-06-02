test_that("make_iv_formula creates formula with IV separator", {
  f <- make_iv_formula("y", "x", c("z1"), "q")
  expect_s3_class(f, "formula")
  expect_true(grepl("|", paste(deparse(f), collapse = ""), fixed = TRUE))
})
