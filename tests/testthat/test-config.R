test_that("draft config validates", {
  cfg <- read_config(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "config", "draft.yml"))
  expect_equal(cfg$mode, "draft")
})


test_that("renv tracks project development dependencies without recursive Suggests", {
  path <- file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "renv", "settings.json")
  settings <- jsonlite::read_json(path, simplifyVector = TRUE)

  expect_true(isTRUE(settings[["snapshot.dev"]]))
  expect_identical(
    settings[["package.dependency.fields"]],
    c("Imports", "Depends", "LinkingTo")
  )
})


test_that("renv startup defers synchronization to the explicit audit", {
  profile <- readLines(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), ".Rprofile"), warn = FALSE)
  expect_true(any(grepl("renv.config.synchronized.check = FALSE", profile, fixed = TRUE)))
})

test_that("overidentification config does not advertise unimplemented estimators", {
  for (name in c("draft.yml", "diagnostics.yml", "final.yml")) {
    cfg <- read_config(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "config", name))
    expect_identical(names(cfg$overidentification), "run")
    expect_identical(cfg$overidentification$run, "auto")
  }
})
