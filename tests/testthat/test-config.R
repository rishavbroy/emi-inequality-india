test_that("draft config validates", {
  cfg <- read_config(file.path(Sys.getenv("EMI_PROJECT_ROOT", "."), "config", "draft.yml"))
  expect_equal(cfg$mode, "draft")
})
