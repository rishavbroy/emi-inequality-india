test_that("read_csv_short reads an existing path", {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(x = 1:2), path, row.names = FALSE)

  out <- read_csv_short(path)

  expect_equal(nrow(out), 2L)
  expect_true("x" %in% names(out))
})

test_that("read_with_short_path reports missing files clearly", {
  expect_error(
    read_with_short_path(tempfile(fileext = ".csv"), utils::read.csv),
    "File does not exist:",
    fixed = TRUE
  )
})

test_that("Excel reader preserves raw headers without type or name guessing", {
  expect_identical(as.character(formals(read_excel_short)$col_types), "text")
  expect_identical(as.character(formals(read_excel_short)$.name_repair), "minimal")
})
