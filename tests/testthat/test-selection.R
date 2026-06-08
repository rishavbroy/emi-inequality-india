test_that("selection data builder returns enrolled fallback and standardized keys", {
  raw <- list(block = data.frame(State = "Bihar", District = "Patna", age = 10))

  out <- build_selection_data(raw, data.frame(), list())

  expect_true("enrolled" %in% names(out))
  expect_true(all(is.na(out$enrolled)))
  expect_equal(out$district_std, "patna")
})

test_that("missingness diagnostics return a stable schema", {
  out <- diagnose_missingness(data.frame(a = c(1, NA), b = c(1, 2)), list())

  expect_equal(names(out), "missing_var")
  expect_equal(out$missing_var, "a")
})

test_that("selection probit returns out-of-pipeline fallback without covariates", {
  out <- estimate_selection_probit(data.frame(enrolled = c(0, 1)), list())

  expect_equal(out$status, "out_of_active_pipeline")
  expect_equal(out$reason, "No probit covariates.")
})

test_that("selection probit fits toy glm when covariates are present", {
  selection_data <- data.frame(
    enrolled = c(0, 1, 0, 1, 0, 1),
    age = c(6, 7, 8, 9, 10, 11)
  )

  model <- estimate_selection_probit(selection_data, list())

  expect_s3_class(model, "glm")
  expect_equal(model$family$link, "probit")
})
