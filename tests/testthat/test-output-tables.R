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
  expect_match(tex, "landscape", fixed = TRUE)
  expect_false(grepl("longtable", tex, fixed = TRUE))
})


test_that("diagnostic table CSVs preserve machine-readable column names", {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  unlink("outputs", recursive = TRUE)

  ame <- data.frame(
    term = "AGE",
    estimate = 0.1,
    std.error = 0.01,
    statistic = 10,
    p.value = 0.001,
    s.value = 9.97,
    conf.low = 0.08,
    conf.high = 0.12,
    method = "autodiff",
    status = "estimated",
    reason = NA_character_,
    check.names = FALSE
  )

  save_tables(
    list(ame_results = ame),
    list(output_formats = list(tables = "csv"))
  )

  header <- names(utils::read.csv(
    file.path("outputs", "tables", "main", "ame_results.csv"),
    check.names = FALSE
  ))
  expect_true(all(c("std.error", "p.value", "conf.low", "conf.high") %in% header))
  expect_false(any(c("Std Error", "P Value", "Conf Low", "Conf High") %in% header))
})


test_that("status-only public tables write stable csv and tex outputs", {
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  unlink("outputs", recursive = TRUE)

  status_table <- data.frame(
    model = "first_stage",
    term = NA_character_,
    estimate = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    status = "out_of_active_pipeline",
    reason = "Missing variables: consumption_pct_change, wavg_ling_degrees",
    stringsAsFactors = FALSE
  )

  paths <- save_tables(
    list(fs_cons = status_table),
    list(output_formats = list(tables = c("csv", "tex")))
  )

  expect_setequal(tools::file_ext(paths), c("csv", "tex"))
  csv <- utils::read.csv(file.path("outputs", "tables", "main", "fs_cons.csv"), check.names = FALSE)
  expect_true(all(c("Term", "Estimate", "Std. Error") %in% names(csv)))
  expect_match(csv$`Std. Error`[[1]], "wavg_ling_degrees", fixed = TRUE)
  tex <- paste(readLines(file.path("outputs", "tables", "main", "fs_cons.tex"), warn = FALSE), collapse = "\n")
  expect_match(tex, "Missing variables", fixed = TRUE)
})


test_that("first-stage public table reports instrument partial F before model F", {
  first_stage <- data.frame(
    model = rep("consumption", 2),
    term = c("wavg_ling_degrees", "(Intercept)"),
    estimate = c(3.8386, 17.1288),
    std.error = c(1.2477, 23.6954),
    statistic = c(3.0765, 0.7229),
    p.value = c(0.0022, 0.4700),
    partial_f = c(9.4646, 9.4646),
    partial_p = c(0.0022, 0.0022),
    legacy_model_f = c(68.2013, 68.2013),
    legacy_model_p = c(3.9e-114, 3.9e-114),
    status = rep("estimated", 2),
    reason = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  out <- make_first_stage_table(first_stage, list(final = TRUE))

  f_row <- out[out$Term == "Instrument's F-Statistic", , drop = FALSE]
  value_col <- setdiff(names(out), "Term")[[1]]
  expect_equal(nrow(f_row), 1L)
  expect_match(f_row[[value_col]][[1]], "9.46", fixed = TRUE)
  expect_false(grepl("68.20", f_row[[value_col]][[1]], fixed = TRUE))
})

test_that("public summary tables use legacy display names and grouping rows", {
  df <- data.frame(
    var = c("AGE", ".group_district", "dmean_num_IS_EDU_FREE"),
    label = c("Age", "District-level aggregates:", "Educ. free available? (Yes = 1)"),
    N = c(10, NA, 10),
    Min = c("5.00", NA, "0.00"),
    `1Q` = c("8.00", NA, "0.50"),
    Med = c("12.00", NA, "0.60"),
    `3Q` = c("16.00", NA, "0.70"),
    Max = c("19.00", NA, "1.00"),
    Mean = c("11.89", NA, "0.65"),
    SD = c("4.22", NA, "0.23"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  out <- format_table_for_output(df, public = TRUE)

  expect_equal(names(out), c("Variable", "Min", "1Q", "Med", "3Q", "Max", "Mean", "SD", "N"))
  expect_false(any(c("var", "label") %in% names(out)))
  expect_equal(out$Variable[[2]], "District-level aggregates:")
  expect_true(all(!nzchar(as.character(out[2, -1]))))
})

test_that("regression public tables place standard errors below estimates", {
  first_stage <- data.frame(
    model = rep("consumption", 3),
    term = c("wavg_ling_degrees", "pct_urban", "(Intercept)"),
    estimate = c(3.825, 1.2, 17.7),
    std.error = c(1.237, 0.4, 23.5),
    statistic = c(3.1, 3, 0.75),
    p.value = c(0.002, 0.01, 0.45),
    partial_f = c(9.56, 9.56, 9.56),
    partial_p = c(0.002, 0.002, 0.002),
    legacy_model_f = c(60, 60, 60),
    legacy_model_p = c(0, 0, 0),
    status = rep("estimated", 3),
    reason = c(NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  out <- make_first_stage_table(first_stage, list(mode = "final"))

  expect_true("EMI Exposure" %in% names(out))
  expect_equal(out$Term[1:2], c("Linguistic distance", ""))
  expect_match(out$`EMI Exposure`[[1]], "3.825", fixed = TRUE)
  expect_equal(out$`EMI Exposure`[[2]], "(1.237)")
  expect_true("Pct. urban" %in% out$Term)
  expect_true("Instrument's F-Statistic" %in% out$Term)
  expect_false("Model's F-Statistic" %in% out$Term)
})
