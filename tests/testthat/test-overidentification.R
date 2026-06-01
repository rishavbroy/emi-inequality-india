test_that("baseline exactly identified model is not overidentified", { spec <- list(endogenous_vars = "EMIE", excluded_instruments = "wavg_ling_degrees"); expect_false(is_overidentified(spec)) })
