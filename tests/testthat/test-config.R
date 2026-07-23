test_that("draft config validates", {
  cfg <- read_config(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "config", "draft.yml"))
  expect_equal(cfg$mode, "draft")
})


test_that("renv status includes development dependencies by default", {
  path <- file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "renv", "settings.json")
  settings <- jsonlite::read_json(path, simplifyVector = TRUE)

  expect_true(isTRUE(settings[["snapshot.dev"]]))
  expect_true("Suggests" %in% settings[["package.dependency.fields"]])
})
