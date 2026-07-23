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
  expect_identical(attr(restored, "cluster_inference_status"), "estimated")
  expect_true(is.matrix(attr(restored, "cluster_vcov")))
  expect_true(all(is.finite(diag(attr(restored, "cluster_vcov")))))
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

test_that("cluster alignment follows fitted model row names", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  set.seed(42)
  n <- 60
  dat <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    w = rnorm(n),
    z = rnorm(n),
    state_20 = rep(LETTERS[1:6], each = 10),
    stringsAsFactors = FALSE
  )
  dat$x <- 0.8 * dat$z + 0.3 * dat$w + rnorm(n)
  dat$y <- 1.2 * dat$x + 0.4 * dat$w + rnorm(n)
  rownames(dat) <- paste0("district_", seq_len(n))
  formulas <- list(model = y ~ x + w | z + w)

  fit <- estimate_2sls(dat, formulas, list())$model
  clustered <- tidy_iv_models(list(model = fit))

  expect_identical(attr(fit, "cluster_state"), dat$state_20)
  expect_true(all(is.finite(clustered$std.error)))
  expect_true(all(is.finite(clustered$p.value)))
})

test_that("cluster alignment follows complete-case model rows", {
  skip_if_not_installed("ivreg")
  set.seed(43)
  n <- 48
  dat <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    w = rnorm(n),
    z = rnorm(n),
    state_20 = rep(LETTERS[1:6], each = 8),
    stringsAsFactors = FALSE
  )
  dat$x <- 0.7 * dat$z + 0.2 * dat$w + rnorm(n)
  dat$y <- dat$x + dat$w + rnorm(n)
  dat$y[c(3, 17)] <- NA_real_
  rownames(dat) <- paste0("unit_", seq_len(n))

  fit <- estimate_2sls(
    dat,
    list(model = y ~ x + w | z + w),
    list()
  )$model
  expected_rows <- match(
    rownames(stats::model.frame(fit)),
    rownames(dat)
  )

  expect_identical(
    attr(fit, "cluster_state"),
    dat$state_20[expected_rows]
  )
  expect_equal(length(attr(fit, "cluster_state")), stats::nobs(fit))
})

test_that("data-aware IV summaries recover clustered inference without cached attributes", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  set.seed(44)
  n <- 72
  dat <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    w = rnorm(n),
    z = rnorm(n),
    state_20 = rep(LETTERS[1:8], each = 9),
    stringsAsFactors = FALSE
  )
  dat$x <- 0.7 * dat$z + 0.3 * dat$w + rnorm(n)
  dat$y <- 1.1 * dat$x + 0.4 * dat$w + rnorm(n)

  fit <- estimate_2sls(
    dat,
    list(model = y ~ x + w | z + w),
    list()
  )$model
  attr(fit, "cluster_state") <- NULL
  attr(fit, "cluster_vcov") <- NULL

  out <- tidy_iv_models(list(model = fit), dat)

  expect_true(all(out$status == "estimated"))
  expect_true(all(is.finite(out$std.error)))
  expect_true(all(is.finite(out$p.value)))
})

test_that("IV summaries expose unavailable inference instead of comparability", {
  production <- list(
    coefficients = data.frame(
      model = "model", term = "x", estimate = 1,
      std.error = NA_real_, p.value = NA_real_
    ),
    first_stage = data.frame(),
    panel_summary = data.frame()
  )
  candidate <- list(
    coefficients = data.frame(
      model = "model", term = "x", estimate = 2,
      std.error = 1, p.value = 0.1
    ),
    first_stage = data.frame(),
    panel_summary = data.frame()
  )

  out <- compare_lineage_v2_model_summaries(
    production,
    candidate,
    comparison_scope = "shared_unique_2001_support",
    comparable = TRUE
  )$coefficient_comparison

  expect_false(out$inference_available)
  expect_false(out$comparable)
})

test_that("public report coefficients recover clusters from panel data", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  set.seed(45)
  n <- 72
  dat <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    w = rnorm(n),
    z = rnorm(n),
    state_20 = rep(LETTERS[1:8], each = 9),
    stringsAsFactors = FALSE
  )
  dat$x <- 0.8 * dat$z + 0.2 * dat$w + rnorm(n)
  dat$y <- 1.1 * dat$x + 0.3 * dat$w + rnorm(n)

  fit <- estimate_2sls(
    dat,
    list(consumption = y ~ x + w | z + w),
    list()
  )$consumption
  attr(fit, "cluster_state") <- NULL
  attr(fit, "cluster_vcov") <- NULL

  expect_true(is.finite(p_value(fit, "x", data = dat)))
  expect_true(is.finite(
    coefficient_value(fit, "x", data = dat)
  ))
})

test_that("public second-stage tables recover clusters from panel data", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  set.seed(46)
  n <- 72
  dat <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    w = rnorm(n),
    z = rnorm(n),
    state_20 = rep(LETTERS[1:8], each = 9),
    stringsAsFactors = FALSE
  )
  dat$x <- 0.75 * dat$z + 0.25 * dat$w + rnorm(n)
  dat$y <- dat$x + 0.4 * dat$w + rnorm(n)

  models <- estimate_2sls(
    dat,
    list(consumption = y ~ x + w | z + w),
    list()
  )
  attr(models$consumption, "cluster_state") <- NULL
  attr(models$consumption, "cluster_vcov") <- NULL

  tidy <- tidy_iv_models(models, dat)
  table <- make_second_stage_table(models, dat)
  model_info <- second_stage_table_model(models, dat)

  expect_true(all(is.finite(tidy$std.error)))
  expect_true(all(is.finite(tidy$p.value)))
  expect_true(nrow(table) > 0L)
  expect_false(is.null(model_info$vcov))
})

test_that("IV structural matrices survive serialization without call re-evaluation", {
  skip_if_not_installed("ivreg")
  set.seed(49)
  n <- 64
  dat <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    w = rnorm(n),
    z = rnorm(n),
    state_20 = rep(LETTERS[1:8], each = 8),
    stringsAsFactors = FALSE
  )
  dat$x <- 0.8 * dat$z + 0.2 * dat$w + rnorm(n)
  dat$y <- dat$x + dat$w + rnorm(n)

  fit <- estimate_2sls(
    dat,
    list(model = y ~ x + w | z + w),
    list()
  )$model
  restored <- unserialize(serialize(fit, NULL))
  X <- iv_structural_model_matrix(restored)

  expect_equal(nrow(X), n)
  expect_true(ncol(X) >= 3L)
  expect_equal(qr(X)$rank, ncol(X))
})

test_that("coefficient frames preserve estimable inference with aliases", {
  fit <- structure(
    list(
      coefficients = c(
        `(Intercept)` = 1,
        x = 2,
        duplicate_x = NA_real_
      ),
      df.residual = 20
    ),
    class = "aliased_coefficient_fixture"
  )
  coef.aliased_coefficient_fixture <- function(object, ...) {
    object$coefficients
  }
  df.residual.aliased_coefficient_fixture <- function(object, ...) {
    object$df.residual
  }
  vc <- diag(c(0.04, 0.09))
  dimnames(vc) <- list(
    c("(Intercept)", "x"),
    c("(Intercept)", "x")
  )

  out <- coefficient_frame(fit, vc)

  expect_identical(
    rownames(out),
    c("(Intercept)", "x", "duplicate_x")
  )
  expect_equal(out["(Intercept)", "Std. Error"], 0.2)
  expect_equal(out["x", "Std. Error"], 0.3)
  expect_true(is.na(out["duplicate_x", "Std. Error"]))
  expect_true(all(is.finite(
    out[c("(Intercept)", "x"), "Pr(>|t|)"]
  )))
})

test_that("condition number uses the estimable regressor matrix", {
  set.seed(48)
  n <- 80
  dat <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    w = rnorm(n),
    stringsAsFactors = FALSE
  )
  dat$duplicate_w <- dat$w
  fit <- stats::lm(y ~ x + w + duplicate_w, data = dat)

  value <- suppressWarnings(as.numeric(condition_number_value(fit)))
  expect_true(is.finite(value))
  expect_gt(value, 0)
})
