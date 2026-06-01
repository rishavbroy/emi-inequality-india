test_that("build_paths returns expected directories", { paths <- build_paths(tempdir()); expect_true(all(c("raw", "processed", "metadata", "outputs") %in% names(paths))) })
