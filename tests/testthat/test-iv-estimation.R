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
  expect_true(is.list(out$toy$x))
  expect_true(all(c("regressors", "instruments", "projected") %in% names(out$toy$x)))
  expect_equal(length(out$toy$y), nrow(panel))
  expect_equal(nrow(out$toy$model), nrow(panel))
})

test_that("serialized IV models retain inputs required by diagnostics and clustered inference", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  set.seed(2)
  n <- 80
  state <- rep(LETTERS[1:8], each = 10)
  z <- rnorm(n)
  w <- rnorm(n)
  x <- 0.8 * z + 0.3 * w + rnorm(n)
  y <- 1 + 1.5 * x + 0.5 * w + rnorm(n)
  panel <- data.frame(y = y, x = x, w = w, z = z, state_20 = state)
  formulas <- list(toy = stats::as.formula("y ~ x + w | z + w"))

  fit <- estimate_2sls(panel, formulas, list())$toy
  restored <- unserialize(serialize(fit, NULL))
  design <- multicollinearity_design_matrix(restored)
  clustered <- clustered_model_coefficients(restored)
  report <- report_coefficient_frame(restored)

  expect_equal(nrow(design), n)
  expect_true(ncol(design) >= 3L)
  expect_true(all(is.finite(clustered$`Std. Error`)))
  expect_true(all(is.finite(clustered$`Pr(>|t|)`)))
  expect_true(all(is.finite(report$std.error)))
  expect_true(all(is.finite(report$p.value)))
  expect_equal(report$std.error, clustered$`Std. Error`)
  expect_equal(report$p.value, clustered$`Pr(>|t|)`)
  expect_true(is.finite(as.numeric(condition_number_value(restored))))
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

test_that("shared-support inference prefers canonical Census-2001 clusters", {
  dat <- data.frame(
    y = c(2.1, 3.4, 4.0, 5.8, 6.2, 7.9, 8.4, 9.7, 10.1, 11.6, 12.0, 13.5),
    x = c(1.2, 2.0, 2.8, 3.9, 4.7, 5.5, 6.4, 7.1, 8.0, 8.8, 9.7, 10.4),
    w = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    z = c(0.8, 1.7, 2.5, 3.1, 4.2, 4.9, 5.8, 6.6, 7.4, 8.1, 9.0, 9.8),
    state_2001_cluster = rep(sprintf("%02d", 1:6), each = 2),
    state_std = NA_character_,
    stringsAsFactors = FALSE
  )
  formulas <- list(model = y ~ x + w | z + w)

  expect_no_warning({
    models <- estimate_2sls(dat, formulas, list())
    first_stage <- estimate_first_stage(models, dat, list())
  })

  expect_identical(
    attr(models$model, "cluster_state"),
    dat$state_2001_cluster
  )
  expect_true(all(first_stage$status == "estimated"))
  expect_true(all(is.finite(first_stage$std.error)))
})

test_that("first-stage covariance declines incomplete clusters without error", {
  dat <- data.frame(
    y = 1:4,
    z = c(1, 2, 4, 5),
    state_2001_cluster = c("01", NA, "02", "02"),
    stringsAsFactors = FALSE
  )
  fit <- stats::lm(y ~ z, data = dat)

  expect_null(first_stage_vcov(fit, dat))
})
