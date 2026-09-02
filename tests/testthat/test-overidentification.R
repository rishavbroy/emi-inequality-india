test_that("baseline exactly identified model is not overidentified", {
  spec <- list(endogenous_vars = "emi_exposure_all_children_0708", excluded_instruments = "ling_distance_nonzero_mean")
  expect_false(is_overidentified(spec))
})

test_that("overidentification diagnostics consume canonical public specifications", {
  specifications <- public_iv_specification_registry()
  out <- diagnose_overidentification(list(), specifications, list())

  expect_identical(plain_chr(out$model), plain_chr(specifications$specification_id))
  expect_true(all(out$status == "not_applicable"))
  expect_true(all(out$n_endogenous == 1L))
  expect_true(all(out$n_excluded_instruments == 1L))
})

test_that("overidentification diagnostics estimate the standard Sargan statistic when applicable", {
  testthat::skip_if_not_installed("ivreg")
  set.seed(51)
  n <- 300L
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  x <- 0.8 * z1 + 0.5 * z2 + stats::rnorm(n)
  y <- 1.5 * x + stats::rnorm(n)
  fit <- ivreg::ivreg(y ~ x | z1 + z2)

  spec <- list(endogenous_vars = "x", excluded_instruments = c("z1", "z2"))
  out <- diagnose_overidentification(
    list(model = fit),
    list(model = spec),
    list(overidentification = list(run = "auto"))
  )

  expect_equal(out$status, "estimated")
  expect_equal(out$test, "sargan")
  expect_equal(out$df, 1)
  expect_true(is.finite(out$statistic))
  expect_true(is.finite(out$p.value))
})
