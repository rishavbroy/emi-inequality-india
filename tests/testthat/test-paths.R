test_that("build_paths preserves relative project roots", {
  paths <- build_paths()

  expect_identical(paths$root, ".")
  expect_identical(paths$raw, file.path(".", "data", "raw"))
  expect_identical(
    path_project(paths, "outputs", "derived", "example.gpkg"),
    file.path(".", "outputs", "derived", "example.gpkg")
  )
})


test_that("build_paths respects an explicit project root", {
  root <- tempdir()
  paths <- build_paths(root)

  expect_identical(paths$root, root)
  expect_true(all(c("raw", "processed", "metadata", "outputs") %in% names(paths)))
  expect_identical(paths$raw, file.path(root, "data", "raw"))
})
