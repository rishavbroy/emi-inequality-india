test_that("draft config validates", { cfg <- read_config("config/draft.yml"); expect_equal(cfg$mode, "draft") })
