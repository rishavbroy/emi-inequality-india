test_that("save_tables honors requested csv and tex formats", {
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  table <- data.frame(variable = "emie_2007", estimate = 1.23)

  paths <- save_tables(
    list(sum_tbl_iv = table),
    list(output_formats = list(tables = c("csv", "tex")))
  )

  expect_setequal(tools::file_ext(paths), c("csv", "tex"))
  expect_true(file.exists(file.path("outputs/tables/main/sum_tbl_iv.csv")))
  tex <- paste(readLines(file.path("outputs/tables/main/sum_tbl_iv.tex"), warn = FALSE), collapse = "\n")
  expect_match(tex, "Summary Statistics for 2SLS Model", fixed = TRUE)
  expect_match(tex, "longtable", fixed = TRUE)
})
