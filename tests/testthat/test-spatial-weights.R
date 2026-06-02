test_that("spatial weights diagnostics can be skipped on non-sf objects", {
  out <- build_spatial_weights(data.frame(x = 1), list())
  expect_type(out, "list")
  expect_equal(out$status, "out_of_active_pipeline")
})
