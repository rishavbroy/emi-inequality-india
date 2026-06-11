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

test_that("IV summary table retains legacy description column", {
  panel <- data.frame(
    EMIE = c(0, 10),
    wavg_ling_degrees = c(0, 2),
    npeople_0708 = c(1000, 2000),
    consumption_0708 = c(700, 900),
    gini_cons_0708 = c(.2, .3),
    pct_urban = c(10, 20),
    avg_hh_size = c(5, 6),
    dependency_ratio = c(50, 60),
    pct_fem_head = c(18, 20),
    pct_hindu = c(80, 70),
    pct_muslim = c(10, 20),
    pct_other_religion = c(10, 10),
    pct_st = c(1, 2), pct_sc = c(10, 15), pct_obc = c(40, 45),
    pct_small_land = c(50, 55), pct_medium_land = c(30, 25), pct_large_land = c(2, 3),
    pct_head_illiterate = c(30, 40), pct_head_lit_to_primary = c(30, 20), pct_head_secondary_plus = c(40, 40),
    pct_pucca = c(50, 60),
    npeople_1718 = c(1200, 2100), consumption_1718 = c(800, 1000), gini_cons_1718 = c(.25, .35),
    consumption_pct_change = c(10, 20), gini_change = c(.01, .02)
  )

  out <- make_iv_summary_table(panel)
  public <- format_table_for_output(out, public = TRUE)

  expect_true("Description" %in% names(public))
  expect_equal(public$Description[public$Variable == "EMIE"][[1]], "EMI exposure")
})

test_that("probit table uses compact dependent-variable header and fit statistics", {
  out <- make_probit_ame_table(
    data.frame(Term = "Age", term = "AGE", estimate = -0.1, std.error = 0.02, p.value = 0.01),
    n = 100,
    selection_model = NULL
  )

  expect_true("Enrolled (1 = yes)" %in% names(out))
  expect_true("Observations" %in% out$Term)
})



test_that("survey-weighted probit table omits likelihood-based fit statistics", {
  svy_model <- structure(
    list(null.deviance = 100, deviance = 80),
    class = c("svyglm", "glm")
  )

  out <- probit_gof_rows(svy_model, 100, "Enrolled (1 = yes)")

  expect_equal(out$Term, "Observations")
  expect_false("Log Likelihood" %in% out$Term)
  expect_false("McFadden pseudo-R-squared" %in% out$Term)
})

test_that("GOF number formatting returns one cell for empty statistics", {
  expect_equal(format_gof_number(numeric()), "")
  expect_equal(format_gof_number(NULL), "")
  expect_equal(format_gof_number(c(NA_real_, 2.3456)), "2.346")
})


test_that("regression GOF rows omit residual standard error", {
  first_stage <- data.frame(
    model = rep("consumption", 2),
    term = c("wavg_ling_degrees", "(Intercept)"),
    estimate = c(3.825, 17.7),
    std.error = c(1.237, 23.5),
    statistic = c(3.1, 0.75),
    p.value = c(0.002, 0.45),
    partial_f = c(9.56, 9.56),
    partial_p = c(0.002, 0.002),
    legacy_model_f = c(60, 60),
    legacy_model_p = c(0, 0),
    nobs = c(482, 482),
    r.squared = c(.7, .7),
    adj.r.squared = c(.68, .68),
    sigma = c(13, 13),
    status = rep("estimated", 2),
    reason = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  out <- make_first_stage_table(first_stage, list(mode = "final"))

  expect_true("R-squared" %in% out$Term)
  expect_false("Residual Std. Error" %in% out$Term)
})

test_that("public table wrapping does not inject literal LaTeX line breaks", {
  df <- data.frame(Variable = "A very long public variable label which should wrap by column width", stringsAsFactors = FALSE)
  out <- wrap_table_text_columns(df, "sum_tbl_probit_cat")
  expect_false(grepl("\\\\", out$Variable[[1]], fixed = TRUE))
})

test_that("IV summary descriptions follow legacy prose and grouping order", {
  panel <- data.frame(
    EMIE = c(0, 10), wavg_ling_degrees = c(0, 2),
    npeople_0708 = c(1000, 2000), consumption_0708 = c(700, 900), gini_cons_0708 = c(.2, .3),
    pct_urban = c(10, 20), avg_hh_size = c(5, 6), dependency_ratio = c(50, 60), pct_fem_head = c(18, 20),
    pct_hindu = c(80, 70), pct_muslim = c(10, 20), pct_other_religion = c(10, 10),
    pct_st = c(1, 2), pct_sc = c(10, 15), pct_obc = c(40, 45),
    pct_small_land = c(50, 55), pct_medium_land = c(30, 25), pct_large_land = c(2, 3),
    pct_head_illiterate = c(30, 40), pct_head_lit_to_primary = c(30, 20), pct_head_secondary_plus = c(40, 40), pct_pucca = c(50, 60),
    npeople_1718 = c(1200, 2100), consumption_1718 = c(800, 1000), gini_cons_1718 = c(.25, .35),
    consumption_pct_change = c(10, 20), gini_change = c(.01, .02)
  )
  public <- format_table_for_output(make_iv_summary_table(panel), public = TRUE)
  expect_equal(public$Variable[[1]], "From 2001:")
  expect_equal(public$Variable[[3]], "From 2007-08:")
  expect_equal(public$Description[public$Variable == "EMIE"][[1]], "EMI exposure")
  expect_equal(public$Description[public$Variable == "Ling. Distance"][[1]], "Average linguistic distance of mother tongue from Hindi")
})

test_that("regression captions carry significance-star note", {
  expect_match(table_caption("fs_cons"), "\\* p < 0.05", fixed = FALSE)
  expect_match(table_caption("cons_iv"), "\\* p < 0.05", fixed = FALSE)
  expect_match(table_caption("probit_mfx"), "\\* p < 0.05", fixed = FALSE)
  expect_match(table_caption("fs_cons"), "shortstack", fixed = TRUE)
  expect_false(grepl("linebreak", table_caption("fs_cons"), fixed = TRUE))
})
