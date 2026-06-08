test_that("AME fallback returns a typed out-of-pipeline row", {
  out <- compute_average_marginal_effects(
    list(status = "out_of_active_pipeline", reason = "No enrolled variable."),
    data.frame(),
    list(run_full_ame = FALSE)
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$status, "out_of_active_pipeline")
  expect_equal(out$reason, "No enrolled variable.")
})

test_that("draft AME path uses toy glm coefficient fallback", {
  selection_data <- data.frame(
    enrolled = c(0, 1, 0, 1, 0, 1),
    age = c(6, 7, 8, 9, 10, 11),
    weight = rep(1, 6)
  )
  model <- stats::glm(enrolled ~ age, data = selection_data, family = stats::binomial(link = "probit"))

  out <- compute_average_marginal_effects(model, selection_data, list(run_full_ame = FALSE))

  expect_equal(out$method, rep("coefficient_fallback", length(stats::coef(model))))
  expect_equal(out$status, rep("estimated", length(stats::coef(model))))
  expect_equal(out$term, names(stats::coef(model)))
})

test_that("AME results keep the active pipeline schema stable", {
  out <- compute_average_marginal_effects(
    list(status = "out_of_active_pipeline", reason = "No probit covariates."),
    data.frame(),
    list(run_full_ame = FALSE)
  )

  expect_equal(
    names(out),
    c(
      "term", "estimate", "std.error", "statistic", "p.value",
      "s.value", "conf.low", "conf.high", "method", "status", "reason"
    )
  )
})



test_that("AME formatting normalizes marginaleffects snake_case uncertainty columns", {
  out <- format_ame_results(data.frame(
    variable = "age",
    estimate = 0.1,
    std_error = 0.02,
    statistic = 5,
    p_value = 0.001,
    s_value = 9.965784,
    conf_low = 0.06,
    conf_high = 0.14
  ))

  expect_equal(names(out), c(
    "term", "estimate", "std.error", "statistic", "p.value",
    "s.value", "conf.low", "conf.high", "method", "status", "reason"
  ))
  expect_equal(out$term, "age")
  expect_equal(out$std.error, 0.02)
  expect_equal(out$p.value, 0.001)
  expect_equal(out$conf.low, 0.06)
  expect_equal(out$conf.high, 0.14)
})

test_that("full AME path uses marginaleffects uncertainty when available", {
  skip_if_not_installed("marginaleffects")
  selection_data <- data.frame(
    enrolled = c(0, 1, 0, 1, 0, 1, 1, 0),
    age = c(6, 7, 8, 9, 10, 11, 12, 13),
    weight = rep(1, 8)
  )
  model <- stats::glm(enrolled ~ age, data = selection_data, family = stats::binomial(link = "probit"))

  out <- compute_average_marginal_effects(model, selection_data, list(run_full_ame = TRUE))

  expect_true(any(is.finite(out$std.error)))
  expect_equal(unique(out$status), "estimated")
})

test_that("AME newdata uses model estimation rows and explicit model weights", {
  selection_data <- data.frame(
    enrolled = c(0, 1, 0, 1, 0, 1),
    age = c(6, 7, NA, 9, 10, 11),
    weight = c(1, 2, 3, 4, 5, 6)
  )
  model <- stats::glm(
    enrolled ~ age,
    data = selection_data,
    weights = weight,
    family = stats::binomial(link = "probit"),
    na.action = stats::na.omit
  )

  amed <- ame_model_data_and_weights(model)

  expect_equal(nrow(amed$data), stats::nobs(model))
  expect_equal(amed$wts, ".ame_weight")
  expect_true(".ame_weight" %in% names(amed$data))
  expect_equal(amed$data$.ame_weight, selection_data$weight[!is.na(selection_data$age)])
})
