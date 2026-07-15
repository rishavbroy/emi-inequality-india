test_that("baseline exactly identified model is not overidentified", {
  spec <- list(endogenous_vars = "EMIE", excluded_instruments = "wavg_ling_degrees")
  expect_false(is_overidentified(spec))
})

test_that("overidentification diagnostics infer exact identification from active formulas", {
  formulas <- build_iv_formulas(list())
  out <- diagnose_overidentification(list(), formulas, list())

  expect_true(all(out$status == "not_applicable"))
  expect_true(all(out$n_endogenous == 1L))
  expect_true(all(out$n_excluded_instruments == 1L))
})

test_that("overidentification diagnostics do not expose TODO branches", {
  spec <- list(endogenous_vars = "x", excluded_instruments = c("z1", "z2"))
  out <- diagnose_overidentification(list(), spec, list(overidentification = list(run = "auto")))

  expect_equal(out$status, "requires_overidentified_estimator")
  expect_false(any(grepl("todo", tolower(unlist(out)), fixed = TRUE)))
})
