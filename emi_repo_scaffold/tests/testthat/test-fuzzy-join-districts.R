test_that("evaluate_distances works on known pairs", { pairs <- tibble::tribble(~str1, ~str2, "Sikim", "Sikkim"); out <- evaluate_distances(pairs, c("osa"), c(1)); expect_true(out$match[1]) })
