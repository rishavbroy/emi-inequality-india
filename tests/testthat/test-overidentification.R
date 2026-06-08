test_that("baseline exactly identified model is not overidentified", {
  spec <- list(endogenous_vars = "EMIE", excluded_instruments = "wavg_ling_degrees")
  expect_false(is_overidentified(spec))
})

test_that("overidentification diagnostics skip when current graph supplies panel instead of specs", {
  out <- diagnose_overidentification(list(), data.frame(x = 1), list())

  expect_equal(out$status, "not_applicable")
  expect_match(out$reason, "No model_specs supplied")
})
