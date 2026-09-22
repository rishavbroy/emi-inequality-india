test_that("fast and final configs validate", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  fast <- read_config(file.path(root, "config", "fast.yml"))
  final <- read_config(file.path(root, "config", "final.yml"))
  expect_equal(fast$mode, "fast")
  expect_equal(final$mode, "final")
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


test_that("overidentification config does not advertise unimplemented estimators", {
  for (name in c("fast.yml", "final.yml")) {
    cfg <- read_config(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "config", name))
    expect_identical(names(cfg$overidentification), "run")
    expect_identical(cfg$overidentification$run, "auto")
  }
})
