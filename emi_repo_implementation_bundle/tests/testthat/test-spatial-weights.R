test_that("spatial weights diagnostics can be skipped on non-sf objects", { expect_null(build_spatial_weights(data.frame(x=1), list())) })
