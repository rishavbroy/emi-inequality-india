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


test_that("spatial IV formula attempts use current IV formula adapter", {
  path <- if (file.exists("R/iv/estimate_spatial_iv_experimental.R")) "R/iv/estimate_spatial_iv_experimental.R" else file.path("..", "..", "R", "iv", "estimate_spatial_iv_experimental.R")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(src, "make_iv_formula", fixed = TRUE)
  expect_match(src, "instruments = c(\"wavg_ling_degrees\", \"W_wLing\", \"W2_wLing\")", fixed = TRUE)
  expect_false(grepl("exog =", src, fixed = TRUE))
  expect_false(grepl("inst =", src, fixed = TRUE))
  expect_match(src, "cluster_se_status", fixed = TRUE)
  expect_match(src, "tidy_spatial_iv_diagnostics", fixed = TRUE)
})


test_that("spatial IV coefficient summaries fall back to point estimates", {
  fit <- structure(list(coefficients = c(`(Intercept)` = 1, x = 2)), class = "toy_ivreg_for_coefficients")
  coef.toy_ivreg_for_coefficients <- function(object, ...) object$coefficients
  summary.toy_ivreg_for_coefficients <- function(object, ...) stop("singular summary")

  out <- tidy_spatial_iv_coefficients(fit, "toy_model", "model_default")

  expect_equal(out$model, c("toy_model", "toy_model"))
  expect_equal(out$vcov_type, c("model_default", "model_default"))
  expect_equal(out$term, c("(Intercept)", "x"))
  expect_equal(out$estimate, c(1, 2))
})
