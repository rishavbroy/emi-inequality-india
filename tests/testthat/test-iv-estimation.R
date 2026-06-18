test_that("estimate_2sls returns explicit fallback for missing variables", {
  formulas <- list(baseline = make_iv_formula("y", "x", "z"))

  out <- estimate_2sls(data.frame(y = 1), formulas, list())

  expect_equal(out$baseline$status, "out_of_active_pipeline")
  expect_match(out$baseline$reason, "Missing variables:")
})

test_that("estimate_2sls fits a toy exactly identified IV model when possible", {
  skip_if_not_installed("ivreg")
  set.seed(1)
  z <- rnorm(30)
  x <- z + rnorm(30)
  y <- 1 + 2 * x + rnorm(30)
  panel <- data.frame(y = y, x = x, z = z)
  formulas <- list(toy = make_iv_formula("y", "x", "z"))

  out <- estimate_2sls(panel, formulas, list())

  expect_s3_class(out$toy, "ivreg")
})

test_that("first-stage diagnostics preserve out-of-pipeline statuses", {
  models <- list(baseline = list(status = "out_of_active_pipeline", reason = "Missing variables: z"))

  out <- estimate_first_stage(models, data.frame(), list())

  expect_equal(out$model, "baseline")
  expect_equal(out$status, "out_of_active_pipeline")
})

test_that("experimental spatial IV returns explicit inactive status", {
  out <- estimate_spatial_iv_experimental(data.frame(), list(), list())

  expect_true("status" %in% names(out))
  expect_equal(out$status$status, "out_of_active_pipeline")
})
