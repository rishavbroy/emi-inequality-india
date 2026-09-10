test_that("save_tables honors requested csv and tex formats", {
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  table <- data.frame(variable = "EMIE", estimate = 1.23)

  paths <- save_tables(
    list(sum_tbl_iv = table),
    list(output_formats = list(tables = c("csv", "tex")))
  )

  expect_setequal(tools::file_ext(paths), c("csv", "tex"))
  expect_true(file.exists(file.path("outputs/tables/main/sum_tbl_iv.csv")))
  tex <- paste(readLines(file.path("outputs/tables/main/sum_tbl_iv.tex"), warn = FALSE), collapse = "\n")
  expect_match(tex, "Summary Statistics for 2SLS Model", fixed = TRUE)
  expect_match(tex, "landscape", fixed = TRUE)
  expect_match(tex, "longtable", fixed = TRUE)
  expect_false(grepl("\\begin{table}", tex, fixed = TRUE))
})




test_that("table_formats accepts YAML-style list values without warnings", {
  cfg <- list(output_formats = list(tables = list("csv", "tex")))

  expect_warning(formats <- table_formats(cfg), NA)
  expect_equal(formats, c("csv", "tex"))
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
    reason = "Missing variables: real_log_consumption_change, ling_distance_nonzero_mean",
    stringsAsFactors = FALSE
  )

  paths <- save_tables(
    list(fs_cons = status_table),
    list(output_formats = list(tables = c("csv", "tex")))
  )

  expect_setequal(tools::file_ext(paths), c("csv", "tex"))
  csv <- utils::read.csv(file.path("outputs", "tables", "main", "fs_cons.csv"), check.names = FALSE)
  expect_true(all(c("Term", "Estimate", "Std. Error") %in% names(csv)))
  expect_match(csv$`Std. Error`[[1]], "ling_distance_nonzero_mean", fixed = TRUE)
  tex <- paste(readLines(file.path("outputs", "tables", "main", "fs_cons.tex"), warn = FALSE), collapse = "\n")
  expect_match(tex, "Missing variables", fixed = TRUE)
})


test_that("first-stage public table reports clustered Wald and MOP strength diagnostics", {
  first_stage <- data.frame(
    model = rep("consumption", 2),
    term = c("ling_distance_nonzero_mean", "(Intercept)"),
    estimate = c(3.8386, 17.1288),
    std.error = c(1.2477, 23.6954),
    statistic = c(3.0765, 0.7229),
    p.value = c(0.0022, 0.4700),
    partial_f = c(9.4646, 9.4646),
    partial_p = c(0.0022, 0.0022),
    effective_f = c(8.75, 8.75),
    effective_f_critical_value = c(10.23, 10.23),
    model_f = c(68.2013, 68.2013),
    model_p = c(3.9e-114, 3.9e-114),
    status = rep("estimated", 2),
    reason = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  out <- make_first_stage_table(first_stage, list(final = TRUE))

  f_row <- out[out$Term == "Instrument's clustered Wald F", , drop = FALSE]
  value_col <- setdiff(names(out), "Term")[[1]]
  expect_equal(nrow(f_row), 1L)
  expect_match(f_row[[value_col]][[1]], "9.46", fixed = TRUE)
  expect_false(grepl("68.20", f_row[[value_col]][[1]], fixed = TRUE))
  mop_row <- out[out$Term == "Montiel Olea-Pflueger effective F", , drop = FALSE]
  expect_equal(nrow(mop_row), 1L)
  expect_match(mop_row[[value_col]][[1]], "8.75", fixed = TRUE)
  critical_row <- out[out$Term == "MOP 5% critical value (10% relative bias)", , drop = FALSE]
  expect_equal(nrow(critical_row), 1L)
})

test_that("first-stage table model uses the fitted IV sample", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("momentfit")
  set.seed(901)
  n <- 60
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 10),
    z = stats::rnorm(n),
    w = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$x <- 0.8 * panel$z + 0.2 * panel$w + stats::rnorm(n)
  panel$y <- 1 + panel$x + panel$w + stats::rnorm(n)
  panel$y[c(2, 17, 41)] <- NA_real_

  model <- ivreg::ivreg(y ~ x + w | z + w, data = panel, model = TRUE, x = TRUE, y = TRUE)
  out <- first_stage_table_model(list(consumption = model), panel)

  expect_s3_class(out$model, "lm")
  expect_equal(stats::nobs(out$model), stats::nobs(model))
  expect_equal(nrow(stats::model.frame(out$model)), stats::nobs(model))
})

test_that("regression styling recognizes the renamed strength diagnostics as GOF rows", {
  table <- data.frame(
    Term = c(
      "Linguistic distance", "", "Observations",
      "Instrument's clustered Wald F",
      "Montiel Olea-Pflueger effective F",
      "MOP 5% critical value (10% relative bias)"
    ),
    value = c("1.00", "(0.50)", "500", "4.00", "3.50", "10.23"),
    stringsAsFactors = FALSE
  )

  expect_equal(regression_summary_start(table), 3L)
})

test_that("public summary tables use documented display names and grouping rows", {
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


revised_iv_summary_fixture <- function() {
  controls <- census_2001_main_controls()
  out <- data.frame(
    ling_distance_nonzero_mean = c(0, 2),
    emi_exposure_all_children_0708 = c(0, 10),
    real_consumption_0708 = c(700, 900),
    real_consumption_1718 = c(800, 1000),
    real_log_consumption_change = log(c(800, 1000)) - log(c(700, 900)),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(controls)) out[[controls[[i]]]] <- c(i, i + 1)
  out
}

test_that("model-backed public regression tables are true longtables", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")
  skip_if_not_installed("knitr")

  set.seed(712)
  n <- 80
  model <- stats::lm(
    y ~ z + x,
    data = data.frame(
      y = stats::rnorm(n),
      z = stats::rnorm(n),
      x = stats::rnorm(n)
    )
  )

  tex <- as.character(public_modelsummary_table(model, "fs_cons"))
  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
  expect_false(grepl("\\begin{table}", tex, fixed = TRUE))
  expect_match(tex, "Standard errors clustered by state in parentheses.", fixed = TRUE)
})

test_that("regression public tables place standard errors below estimates", {
  first_stage <- data.frame(
    model = rep("consumption", 3),
    term = c("ling_distance_nonzero_mean", "urban_share_2001", "(Intercept)"),
    estimate = c(3.825, 1.2, 17.7),
    std.error = c(1.237, 0.4, 23.5),
    statistic = c(3.1, 3, 0.75),
    p.value = c(0.002, 0.01, 0.45),
    partial_f = c(9.56, 9.56, 9.56),
    partial_p = c(0.002, 0.002, 0.002),
    effective_f = c(8.9, 8.9, 8.9),
    effective_f_critical_value = c(10.23, 10.23, 10.23),
    model_f = c(60, 60, 60),
    model_p = c(0, 0, 0),
    status = rep("estimated", 3),
    reason = c(NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  out <- make_first_stage_table(first_stage, list(mode = "final"))

  expect_true("EMI Exposure" %in% names(out))
  expect_equal(out$Term[1:2], c("Linguistic distance", ""))
  expect_match(out$`EMI Exposure`[[1]], "3.825", fixed = TRUE)
  expect_equal(out$`EMI Exposure`[[2]], "(1.237)")
  expect_true("Urban population share" %in% out$Term)
  expect_true("Instrument's clustered Wald F" %in% out$Term)
  expect_true("Montiel Olea-Pflueger effective F" %in% out$Term)
  expect_true("MOP 5% critical value (10% relative bias)" %in% out$Term)
  expect_false("Model's F-Statistic" %in% out$Term)
})

test_that("IV summary table retains its description column", {
  panel <- revised_iv_summary_fixture()
  out <- make_iv_summary_table(panel)
  public <- format_table_for_output(out, public = TRUE)

  expect_true("Description" %in% names(public))
  expect_equal(
    public$Description[public$Variable == "EMI exposure"][[1]],
    "Share of children ages 5-19 enrolled in English-medium instruction"
  )
})


test_that("population summary statistics use comma integers without artificial decimals", {
  out <- public_numeric_stats(
    data.frame(npeople_0708 = c(1234.4, 98765.6)),
    data.frame(var = "npeople_0708", label = "Population", stringsAsFactors = FALSE),
    count_vars = "npeople_0708"
  )

  expect_equal(out$Min[[1]], "1,234")
  expect_equal(out$Max[[1]], "98,766")
  expect_false(grepl("\\.00$", out$Mean[[1]]))
})

test_that("probit table uses documented AME estimate and standard-error columns", {
  out <- make_probit_ame_table(
    data.frame(Term = "Age", term = "AGE", estimate = -0.1, std.error = 0.02, p.value = 0.01),
    n = 100,
    selection_model = NULL
  )

  expect_equal(names(out), c("Term", "Estimate", "Std. Error"))
  expect_equal(out$Term, "Age")
  expect_match(out$Estimate, "-0.100", fixed = TRUE)
  expect_equal(out$`Std. Error`, "(0.020)")
  expect_false("Observations" %in% out$Term)
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


test_that("regression GOF map includes residual standard error", {
  first_stage <- data.frame(
    model = rep("consumption", 2),
    term = c("ling_distance_nonzero_mean", "(Intercept)"),
    estimate = c(3.825, 17.7),
    std.error = c(1.237, 23.5),
    statistic = c(3.1, 0.75),
    p.value = c(0.002, 0.45),
    partial_f = c(9.56, 9.56),
    partial_p = c(0.002, 0.002),
    model_f = c(60, 60),
    model_p = c(0, 0),
    nobs = c(482, 482),
    r.squared = c(.7, .7),
    adj.r.squared = c(.68, .68),
    sigma = c(13, 13),
    status = rep("estimated", 2),
    reason = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  gof <- public_modelsummary_gof_map("fs_cons")
  clean <- vapply(gof, `[[`, character(1), "clean")

  expect_true("Residual Std. Error" %in% clean)
  expect_true("Model's F-Statistic" %in% clean)
})

test_that("public table wrapping does not inject literal LaTeX line breaks", {
  df <- data.frame(Variable = "A very long public variable label which should wrap by column width", stringsAsFactors = FALSE)
  out <- wrap_table_text_columns(df, "sum_tbl_probit_cat")
  expect_false(grepl("\\\\", out$Variable[[1]], fixed = TRUE))
})

test_that("IV summary descriptions follow documented prose and grouping order", {
  panel <- revised_iv_summary_fixture()
  public <- format_table_for_output(make_iv_summary_table(panel), public = TRUE)
  expect_equal(public$Variable[[1]], "Treatment and instrument:")
  expect_equal(public$Variable[[4]], "Consumption outcomes:")
  expect_equal(public$Variable[[8]], "Census 2001 controls:")
  expect_equal(
    public$Description[public$Variable == "EMI exposure"][[1]],
    "Share of children ages 5-19 enrolled in English-medium instruction"
  )
  expect_equal(
    public$Description[public$Variable == "Linguistic distance"][[1]],
    "Population-weighted mean linguistic distance among mapped speakers with positive distance from Hindi"
  )
})

test_that("regression captions use plain public titles", {
  expect_equal(table_caption("fs_cons"), "First-Stage Regression: EMI Exposure on Linguistic Distance")
  expect_equal(table_caption("cons_iv"), "Second-Stage Regression: Real Log Consumption Growth on EMI Exposure (Fitted)")
  expect_equal(table_caption("probit_mfx"), "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit")
  expect_equal(
    table_caption("english_opportunity_mechanism"),
    "Linguistic-Distance Association Across Mechanism Stages"
  )
  expect_false(grepl("\\* p < 0.05", table_caption("fs_cons")))
  expect_false(grepl("\\n", table_caption("fs_cons"), fixed = TRUE))
  expect_false(grepl("parbox", table_caption("fs_cons"), fixed = TRUE))
  expect_false(grepl("tabular", table_caption("fs_cons"), fixed = TRUE))
  expect_false(grepl("shortstack", table_caption("fs_cons"), fixed = TRUE))
  expect_false(grepl("linebreak", table_caption("fs_cons"), fixed = TRUE))
})


test_that("widened categorical summary table headers stay unwrapped without scaling down", {
  df <- data.frame(
    Variable = "Urban", Values = "Rural, Urban", Mode = "Rural",
    `Pct. Mode` = "67.5", `Least Freq.` = "Urban", `Pct. Least Freq.` = "32.5", N = "127246",
    check.names = FALSE
  )
  labels <- table_header_labels(df, "sum_tbl_probit_cat")
  expect_false(any(grepl("\\\\", labels, fixed = FALSE)))
  expect_false(any(grepl("scale_down", labels, fixed = TRUE)))
})

test_that("regression table styling identifies standard-error rows", {
  df <- data.frame(Term = c("EMIE", "", "Observations"), `(1)` = c("0.406", "(0.612)", "482"), check.names = FALSE)
  expect_equal(regression_standard_error_rows(df), 2L)
})

test_that("table path target remains atomic when tables contain list-like cells", {
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  unlink("outputs", recursive = TRUE)

  table <- data.frame(Term = c("A", ""), stringsAsFactors = FALSE)
  table$`EMI Exposure` <- I(list(c("1.234", "extra"), "(0.123)"))

  expect_warning(
    paths <- save_tables(
      list(fs_cons = table),
      list(output_formats = list(tables = c("csv", "tex")))
    ),
    NA
  )

  expect_type(paths, "character")
  expect_false(is.object(paths))
  expect_true(all(file.exists(paths)))
})


test_that("status-only detection handles list columns without warnings", {
  out <- data.frame(
    model = I(list("first_stage")),
    status = I(list("unavailable")),
    reason = I(list(c("missing", "variables"))),
    term = I(list(character())),
    estimate = I(list(NA_real_)),
    check.names = FALSE
  )

  expect_warning(status <- is_status_only_table(out), NA)
  expect_true(status)
  expect_warning(public <- format_status_table_for_output(out, public = TRUE), NA)
  expect_equal(public$Term[[1]], "first_stage")
  expect_match(public$`Std. Error`[[1]], "missing; variables", fixed = TRUE)
})

test_that("save_tables returns plain unique character paths", {
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  unlink("outputs", recursive = TRUE)

  table <- data.frame(Variable = "Age", N = 1, stringsAsFactors = FALSE)
  paths <- save_tables(
    list(sum_tbl_probit_quant = table),
    list(output_formats = list(tables = list("csv", "tex")))
  )

  expect_type(paths, "character")
  expect_null(names(paths))
  expect_equal(length(paths), length(unique(paths)))
  expect_true(all(file.exists(paths)))
})


test_that("known kable coercion warning is muffled at table-write boundary", {
  expect_warning(
    suppress_atomic_vector_coercion_warning(warning("argument is not an atomic vector; coercing", call. = FALSE)),
    NA
  )
})

test_that("unrelated table-write warnings are still surfaced", {
  expect_warning(
    suppress_atomic_vector_coercion_warning(warning("unexpected table warning", call. = FALSE)),
    "unexpected table warning"
  )
})



test_that("long captions are kept plain and not linebreak-corrupted", {
  cap <- caption_for_latex("sum_tbl_probit_cat")

  expect_match(cap, "Summary Statistics for Enrollment Participation Model", fixed = TRUE)
  expect_match(cap, "Categorical Variables", fixed = TRUE)
  expect_false(grepl("&", cap, fixed = TRUE))
  expect_false(grepl("\\caption", cap, fixed = TRUE))
})

test_that("fallback regression TeX output does not expose placeholder term rows", {
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  unlink("outputs", recursive = TRUE)

  table <- data.frame(Term = c("EMIE", "", "Observations"), `Consumption Growth` = c("0.406", "(0.612)", "482"), check.names = FALSE)
  save_tables(list(cons_iv = table), list(output_formats = list(tables = "tex")))
  tex <- paste(readLines(file.path("outputs", "tables", "main", "cons_iv.tex"), warn = FALSE), collapse = "\n")

  expect_match(tex, "Second-Stage Regression: Real Log Consumption Growth on EMI Exposure", fixed = TRUE)
  expect_match(tex, "Standard errors clustered by state", fixed = TRUE)
  expect_false(grepl(">~<|& ~ &|^~$", tex))
})

test_that("modelsummary payload uses project-owned coefficient extraction", {
  model <- lm(mpg ~ wt, data = mtcars)
  vc <- stats::vcov(model) * 4

  out <- modelsummary_payload(model, vc)

  expect_s3_class(out, "modelsummary_list")
  expect_equal(out$tidy$term, names(stats::coef(model)))
  expect_equal(out$tidy$estimate, unname(stats::coef(model)))
  expect_equal(out$tidy$std.error, unname(sqrt(diag(vc))), tolerance = 1e-12)
  expect_equal(out$glance$nobs, stats::nobs(model))
})

test_that("public modelsummary writer renders ivreg through the custom payload", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")
  model <- ivreg::ivreg(mpg ~ wt | disp, data = mtcars)

  tex <- paste(as.character(public_modelsummary_table(model, "cons_iv")), collapse = "\n")

  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
  expect_false(grepl("\\begin{table}", tex, fixed = TRUE))
  expect_match(tex, "Consumption Growth", fixed = TRUE)
  expect_match(tex, "Standard errors clustered by state in parentheses.", fixed = TRUE)
  expect_false(grepl("\\\\*\n\\multicolumn", tex))
})

test_that("public modelsummary regression writer emits LaTeX rather than HTML", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")
  model <- lm(mpg ~ wt, data = mtcars)

  tex <- paste(as.character(public_modelsummary_table(model, "fs_cons")), collapse = "\n")
  tex <- paste(normalize_quarto_table_labels(tex, "fs_cons"), collapse = "\n")

  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
  expect_false(grepl("\\begin{table}", tex, fixed = TRUE))
  expect_match(tex, "\\label{tbl-fs-cons}", fixed = TRUE)
  expect_match(tex, regression_star_note(), fixed = TRUE)
  expect_match(tex, "Standard errors clustered by state in parentheses.", fixed = TRUE)
  expect_false(grepl("\\\\*\n\\multicolumn", tex))
  expect_false(grepl("<table", tex, fixed = TRUE))
  expect_false(grepl("<caption>", tex, fixed = TRUE))
})

test_that("generated table TeX labels are Quarto cross-reference labels", {
  tex <- "\\begin{table}\n\\caption{\\label{tab:sum-tbl-iv}Summary Statistics for 2SLS Model}\n\\end{table}"

  out <- normalize_quarto_table_labels(tex, "sum_tbl_iv")

  expect_match(out, "\\label{tbl-sum-tbl-iv}", fixed = TRUE)
  expect_false(grepl("\\label{tab:sum-tbl-iv}", out, fixed = TRUE))
})

test_that("probit TeX stacks standard errors below AME estimates", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  unlink("outputs", recursive = TRUE)
  table <- data.frame(Term = "Age", Estimate = "-0.100", `Std. Error` = "(0.020)", check.names = FALSE)

  save_tables(list(probit_mfx = table), list(output_formats = list(tables = "tex")))
  tex <- paste(readLines(file.path("outputs", "tables", "main", "probit_mfx.tex"), warn = FALSE), collapse = "\n")

  expect_match(tex, "Enrolled (1 = yes)", fixed = TRUE)
  expect_match(tex, "-0.100", fixed = TRUE)
  expect_match(tex, "(0.020)", fixed = TRUE)
  expect_false(grepl("Std. Error", tex, fixed = TRUE))
  expect_false(grepl("textcolor", tex, fixed = TRUE))
  expect_false(grepl("textit", tex, fixed = TRUE))
  expect_false(grepl("\\multicolumn{2}{c}{Enrolled in School (1 = yes)}", tex, fixed = TRUE))
})

test_that("report source loads caption setup for wrapping long table captions", {
  src <- paste(readLines(repo_file("paper", "paper.qmd"), warn = FALSE), collapse = "\n")

  expect_match(src, "\\usepackage{caption}", fixed = TRUE)
  expect_match(src, "captionsetup", fixed = TRUE)
})

test_that("wide summary tables hold their float inside landscape pages", {
  skip_if_not_installed("kableExtra")
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  unlink("outputs", recursive = TRUE)

  table <- data.frame(
    Variable = c("Population", "Consumption"),
    Description = c("Estimated via NSS sample weights", "Average household monthly consumption expenditures (Rs.)"),
    Min = c("12,285", "330.09"),
    `1Q` = c("823,676", "626.88"),
    Med = c("1,396,516", "768.60"),
    `3Q` = c("2,317,118", "999.13"),
    Max = c("9,922,640", "2923.14"),
    Mean = c("1,700,682", "850.21"),
    SD = c("1,307,716", "319.75"),
    N = c("482", "482"),
    check.names = FALSE
  )

  save_tables(list(sum_tbl_iv = table), list(output_formats = list(tables = "tex")))
  tex <- paste(readLines(file.path("outputs", "tables", "main", "sum_tbl_iv.tex"), warn = FALSE), collapse = "\n")

  expect_match(tex, "\\begin{landscape}", fixed = TRUE)
  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
  expect_false(grepl("\\begin{table}", tex, fixed = TRUE))
})


test_that("native marginaleffects objects are preserved for modelsummary rendering", {
  mfx <- structure(
    data.frame(
      term = "AGE",
      contrast = "dY/dX",
      estimate = -0.1,
      std.error = 0.02,
      statistic = -5,
      p.value = 0.001,
      s.value = 9.97,
      conf.low = -0.14,
      conf.high = -0.06,
      check.names = FALSE
    ),
    class = c("slopes", "marginaleffects", "data.frame")
  )

  attr(mfx, "model") <- "underlying model metadata"
  formatted <- format_ame_results(mfx)
  table <- make_probit_ame_table(formatted, n = 100)

  expect_s3_class(attr(formatted, "marginaleffects_object"), "marginaleffects")
  expect_equal(attr(attr(formatted, "marginaleffects_object"), "model"), "underlying model metadata")
  expect_s3_class(attr(table, "marginaleffects_object"), "marginaleffects")
  expect_equal(attr(table, "marginaleffects_n"), 100)
})

test_that("probit AME table has a native marginaleffects modelsummary path and no map side effects", {
  src <- paste(deparse(save_table_tex), collapse = "\n")
  ame_src <- paste(deparse(ame_modelsummary_table), collapse = "\n")
  expect_match(src, "ame_modelsummary_table", fixed = TRUE)
  expect_match(ame_src, "modelsummary::modelsummary", fixed = TRUE)
  expect_match(ame_src, "models = list(mfx)", fixed = TRUE)
  expect_match(ame_src, "gof_map = ame_gof_map", fixed = TRUE)
  expect_match(ame_src, "gof_function = ame_gof_function", fixed = TRUE)
  expect_false(grepl("gof_omit", ame_src, fixed = TRUE))
  expect_false(grepl("add_rows =", ame_src, fixed = TRUE))
  expect_false(grepl("modelsummary_list", ame_src, fixed = TRUE))
  expect_false(grepl("map_no_data_colour", src, fixed = TRUE))
})

test_that("probit summary table column widths are centralized and slightly narrowed", {
  src <- paste(deparse(save_table_tex), collapse = "\n")
  expect_match(src, "5.4cm", fixed = TRUE)
  expect_match(src, "public_table2_column_widths", fixed = TRUE)

  widths <- public_table2_column_widths()
  numeric_widths <- as.numeric(sub("cm", "", widths, fixed = TRUE))
  expect_equal(length(widths), 7)
  expect_lt(numeric_widths[[2]], 6.6)
  expect_lt(numeric_widths[[7]], 1.35)
})


test_that("modelsummary datasummary alignment is a single string", {
  df <- data.frame(Term = c("Urban", ""), `Enrolled (1 = yes)` = c("0.001", "(0.002)"), check.names = FALSE)
  expect_equal(table_alignments(df, "probit_mfx"), c("l", "c"))
  expect_equal(modelsummary_align_string(df, "probit_mfx"), "lc")
})


test_that("Table 2 categorical headers remain on one line", {
  df <- data.frame(
    Variable = "Urban", Values = "Rural, Urban", Mode = "Rural",
    `Pct. Mode` = "67.5", `Least Freq.` = "Urban", `Pct. Least Freq.` = "32.5", N = "127246",
    check.names = FALSE
  )
  labels <- table_header_labels(df, "sum_tbl_probit_cat")
  expect_true(all(!grepl("\\\\|newline|makecell", labels)))
})

test_that("Table 2 categorical column widths are slightly narrowed", {
  widths <- public_table2_column_widths()
  expect_equal(length(widths), 7)
  expect_true(all(grepl("cm", widths, fixed = TRUE)))
  numeric_widths <- as.numeric(sub("cm", "", widths, fixed = TRUE))
  expect_lt(sum(numeric_widths), 24.7)
  expect_lt(numeric_widths[[1]], 3.8)
  expect_lt(numeric_widths[[2]], 6.6)
})

test_that("public regression longtables use one non-floating styling contract", {
  opts <- public_longtable_latex_options()

  expect_equal(opts, c("repeat_header", "striped", "longtable"))
  expect_false("hold_position" %in% opts)
})

test_that("probit AME modelsummary table uses same standard styling path as IV tables", {
  src <- paste(deparse(ame_modelsummary_table), collapse = "\n")

  expect_match(src, "modelsummary::modelsummary", fixed = TRUE)
  expect_match(src, "models = list(mfx)", fixed = TRUE)
  expect_match(src, "coef_rename = ame_modelsummary_label", fixed = TRUE)
  expect_match(src, "gof_map = ame_gof_map", fixed = TRUE)
  expect_match(src, "gof_function = ame_gof_function", fixed = TRUE)
  expect_match(src, "longtable = TRUE", fixed = TRUE)
  expect_match(src, "kableExtra::kable_styling", fixed = TRUE)
  expect_match(src, "public_longtable_latex_options", fixed = TRUE)
  expect_match(src, "Enrolled (1 = yes)", fixed = TRUE)
  expect_match(src, "modelsummary_stars_note", fixed = TRUE)
  expect_match(src, "add_public_longtable_notes", fixed = TRUE)
  expect_match(src, "single_space_longtable_tex", fixed = TRUE)
  expect_false(grepl("p\\{[0-9.]+cm\\}", src))
  expect_false(grepl("add_rows =", src, fixed = TRUE))
})

test_that("labeled native marginaleffects object preserves public AME order and labels", {
  native <- structure(
    data.frame(
      term = c("SEX", "AGE"),
      estimate = c(0.2, -0.1),
      std.error = c(0.03, 0.02),
      statistic = c(6.7, -5),
      p.value = c(0.001, 0.001),
      conf.low = c(0.14, -0.14),
      conf.high = c(0.26, -0.06),
      check.names = FALSE
    ),
    class = c("slopes", "marginaleffects", "data.frame")
  )
  formatted <- data.frame(
    Term = c("Age (years)", "Female (ref: Male)"),
    term = c("AGE", "SEX"),
    contrast = c("dY/dX", "Female - Male"),
    estimate = c(-0.1, 0.2),
    std.error = c(0.02, 0.03),
    statistic = c(-5, 6.7),
    p.value = c(0.001, 0.001),
    conf.low = c(-0.14, 0.14),
    conf.high = c(-0.06, 0.26),
    check.names = FALSE
  )
  attr(formatted, "marginaleffects_object") <- modelsummary_marginaleffects_object(formatted, native)
  table <- make_probit_ame_table(formatted, n = 100)
  out <- ame_modelsummary_object(table)
  expect_s3_class(out, "marginaleffects")
  expect_false(inherits(out, "modelsummary_list"))
  expect_equal(out$term, formatted$Term)
  expect_equal(out$estimate, formatted$estimate)
  expect_equal(attr(table, "marginaleffects_n"), 100)
})


test_that("AME observations are supplied through modelsummary gof_function", {
  table <- data.frame(Term = "Age", Estimate = "-0.100", check.names = FALSE)
  attr(table, "marginaleffects_n") <- 127246
  gof_fun <- ame_gof_function(table)
  expect_true(is.function(gof_fun))
  expect_equal(gof_fun(NULL)$nobs[[1]], 127246)
  expect_equal(ame_gof_map()$raw[[1]], "nobs")
  expect_equal(ame_gof_map()$clean[[1]], "Observations")
})

test_that("AME modelsummary labels use colon separators", {
  expect_equal(ame_modelsummary_label("Religion × Muslim (ref × Hindu)"), "Religion: Muslim (ref: Hindu)")
  expect_equal(ame_modelsummary_label("Religion ×  Muslim (ref ×  Hindu)"), "Religion: Muslim (ref: Hindu)")
  expect_equal(ame_modelsummary_label("Social group × Scheduled Tribe (ref × Other)"), "Social group: Scheduled Tribe (ref: Other)")
})


test_that("public longtable notes combine significance and table-specific context", {
  expect_equal(public_table_note("probit_mfx"), "NSS 64th round; design-based SEs in parentheses.")
  expect_equal(
    public_longtable_notes("probit_mfx"),
    c(regression_star_note(), public_table_note("probit_mfx"))
  )
  expect_equal(
    public_longtable_notes("fs_cons"),
    c(regression_star_note(), public_table_note("fs_cons"))
  )
  wrapped <- single_space_longtable_tex("BODY")
  expect_true(grepl("singlespacing", wrapped, fixed = TRUE))
  expect_true(grepl("BODY", wrapped, fixed = TRUE))
})


test_that("paper core summary uses p10/p90 and preferred modern welfare support", {
  panel <- data.frame(
    enrollment_rate_0708 = c(50, 60, 70),
    emi_share_enrolled_0708 = c(10, 20, 30),
    emi_exposure_all_children_0708 = c(5, 12, 21),
    public_emi_exposure_all_children_0708 = c(2, 4, 6),
    private_emi_exposure_all_children_0708 = c(3, 8, 15),
    private_share_enrolled_0708 = c(20, 30, 40),
    ling_distance_nonzero_mean = c(1, 2, 3),
    ling_share_distance_ge3 = c(20, 30, 40),
    adult_secondary_plus_share_2001 = c(10, 20, 30),
    urban_share_2001 = c(20, 40, 60),
    st_share_2001 = c(0, 10, 20),
    stringsAsFactors = FALSE
  )
  welfare <- data.frame(
    round_id = c("nss_2004_05", "hces_2022_23", "hces_2022_23", "hces_2023_24"),
    outcome_id = "real_mean_mpce",
    estimate = c(1000, 2000, 9000, 2500),
    preferred_eligible = c(TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  out <- make_paper_core_summary_table(panel, welfare)

  expect_identical(names(out), c("Variable", "N", "Mean", "SD", "p10", "p90", "Year / unit"))
  expect_true(all(c(
    "Panel A. Schooling, 2007-08:",
    "Panel B. Inherited linguistic conditions:",
    "Panel C. Predetermined district capacity:",
    "Panel D. Later welfare:"
  ) %in% out$Variable))
  emi <- out[out$Variable == "All-child EMI exposure", , drop = FALSE]
  expect_equal(emi$N, "3")
  expect_equal(emi$p10, "6.40")
  expect_equal(emi$p90, "19.20")
  hces22 <- out[out$Variable == "Real mean MPCE" & grepl("2022-23", out$`Year / unit`, fixed = TRUE), , drop = FALSE]
  expect_equal(hces22$N, "1")
  expect_equal(hces22$Mean, "2000.00")
})


test_that("paper core summary caption and note describe district support", {
  expect_equal(public_table_caption_text("paper_core_summary"), "Core Variables and Summary Statistics")
  note <- public_table_note("paper_core_summary")
  expect_match(note, "District-level descriptive statistics", fixed = TRUE)
  expect_match(note, "preferred-eligible", fixed = TRUE)
  expect_match(note, "p10 and p90", fixed = TRUE)
})

test_that("public summary CSVs retain typed analytical values without display rows", {
  table <- public_numeric_stats(
    data.frame(x = c(1, 2, 9), y = c(1000, 2000, 3000)),
    data.frame(
      var = c("x", "y"),
      label = c("Measure X", "Measure Y"),
      stringsAsFactors = FALSE
    ),
    count_vars = "y"
  )
  csv_data <- attr(table, "csv_data", exact = TRUE)
  table <- insert_summary_group(table, "Grouped measures:", "y")
  attr(table, "csv_data") <- csv_data

  path <- tempfile(fileext = ".csv")
  save_table_csv(table, path, public = TRUE)
  out <- utils::read.csv(path, check.names = FALSE)

  expect_equal(out$var, c("x", "y"))
  expect_equal(out$label, c("Measure X", "Measure Y"))
  expect_true(is.numeric(out$Mean))
  expect_equal(out$Mean, c(4, 2000))
  expect_false(any(grepl("Grouped measures", out$label, fixed = TRUE)))
})


test_that("regression CSVs retain coefficient records rather than stacked display cells", {
  first_stage <- data.frame(
    model = rep("consumption", 2),
    term = c("ling_distance_nonzero_mean", "(Intercept)"),
    estimate = c(3.825, 17.7),
    std.error = c(1.237, 23.5),
    statistic = c(3.1, 0.75),
    p.value = c(0.002, 0.45),
    partial_f = c(9.56, 9.56),
    partial_p = c(0.002, 0.002),
    effective_f = c(8.9, 8.9),
    effective_f_critical_value = c(10.23, 10.23),
    status = rep("estimated", 2),
    reason = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  table <- make_first_stage_table(first_stage, list(mode = "final"))
  path <- tempfile(fileext = ".csv")
  save_table_csv(table, path, public = TRUE)
  out <- utils::read.csv(path, check.names = FALSE)

  expect_true(all(c("model", "term", "estimate", "std.error", "p.value", "effective_f") %in% names(out)))
  expect_equal(out$term, c("ling_distance_nonzero_mean", "(Intercept)"))
  expect_true(is.numeric(out$estimate))
  expect_false(any(grepl("\\(", as.character(out$estimate))))
  expect_false(any(grepl("\\*", as.character(out$estimate))))
})


test_that("paper core-summary CSV is tidy and separates panel, period, and unit", {
  panel <- data.frame(
    enrollment_rate_0708 = c(50, 60, 70),
    emi_share_enrolled_0708 = c(10, 20, 30),
    emi_exposure_all_children_0708 = c(5, 12, 21),
    public_emi_exposure_all_children_0708 = c(3, 7, 10),
    private_emi_exposure_all_children_0708 = c(2, 5, 11),
    private_share_enrolled_0708 = c(20, 30, 40),
    ling_distance_nonzero_mean = c(1, 2, 3),
    ling_share_distance_ge3 = c(10, 20, 30),
    adult_secondary_plus_share_2001 = c(15, 20, 25),
    urban_share_2001 = c(25, 35, 45),
    st_share_2001 = c(2, 4, 6),
    stringsAsFactors = FALSE
  )
  welfare <- data.frame(
    round_id = rep(c("nss_2004_05", "hces_2022_23", "hces_2023_24"), each = 2),
    outcome_id = "real_mean_mpce",
    estimate = c(800, 900, 1800, 2000, 1900, 2100),
    preferred_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  table <- make_paper_core_summary_table(panel, welfare)
  path <- tempfile(fileext = ".csv")
  save_table_csv(table, path, public = TRUE)
  out <- utils::read.csv(path, check.names = FALSE)

  expect_identical(
    names(out),
    c("panel", "variable", "n", "mean", "sd", "p10", "p90", "period", "unit")
  )
  expect_false(any(grepl("^Panel ", out$variable)))
  expect_true(all(vapply(out[c("n", "mean", "sd", "p10", "p90")], is.numeric, logical(1))))
  expect_true(all(nzchar(out$panel)))
  expect_true(all(nzchar(out$period)))
  expect_match(out$unit[out$period == "2022-23"][[1]], "price Rs/person/month", fixed = TRUE)
})

paper_schooling_welfare_fixture <- function(adjustments = "state_main") {
  columns <- paper_schooling_welfare_column_registry()
  treatments <- paper_schooling_welfare_treatment_labels()
  grid <- expand.grid(
    outcome_round = unique(columns$outcome_round),
    estimand = unique(columns$estimand),
    treatment_id = names(treatments),
    adjustment_id = adjustments,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[
    paste(grid$outcome_round, grid$estimand) %in% paste(columns$outcome_round, columns$estimand),
    , drop = FALSE
  ]
  grid$estimate_per_10_percentage_points <- seq_len(nrow(grid)) / 1000
  grid$std_error_state_clustered <- 0.001
  grid$p_value_state_clustered <- 0.02
  grid$p_value_holm_welfare <- 0.04
  grid$n <- ifelse(grid$outcome_round == "hces_2022_23", 524L, 522L)
  grid$status <- "estimated"
  grid
}

test_that("paper schooling-welfare evidence is the registered 5-by-4 common-support design", {
  columns <- paper_schooling_welfare_column_registry()
  treatments <- paper_schooling_welfare_treatment_labels()
  csv <- paper_schooling_welfare_csv_data(
    paper_schooling_welfare_fixture(c("unadjusted", "region_main", "state_main"))
  )

  expect_equal(nrow(csv), 20L)
  expect_setequal(csv$treatment_id, names(treatments))
  expect_setequal(paste(csv$outcome_round, csv$estimand), paste(columns$outcome_round, columns$estimand))
  expect_false("status" %in% names(csv))
  expect_equal(unique(csv$n[csv$outcome_round == "hces_2022_23"]), 524)
  expect_equal(unique(csv$n[csv$outcome_round == "hces_2023_24"]), 522)
  expect_true(all(csv$estimate_percent_per_10pp > 0))
})

test_that("paper schooling-welfare evidence rejects incomplete model status", {
  estimates <- paper_schooling_welfare_fixture()
  estimates$status[[1L]] <- "not_estimable"
  expect_error(
    paper_schooling_welfare_csv_data(estimates),
    "requires estimated state-main bridge results",
    fixed = TRUE
  )
})

test_that("paper schooling-market table preserves three distinct statistical panels", {
  measures <- paper_schooling_market_measure_registry()
  estimates <- do.call(rbind, lapply(seq_len(nrow(measures)), function(i) {
    data.frame(
      measure_id = measures$measure_id[[i]],
      specification_id = c("unadjusted", "region_main", "state_main"),
      standardized_estimate = c(0.4, 0.2, 0.1) + i / 1000,
      n = 500L,
      stringsAsFactors = FALSE
    )
  }))
  validation <- data.frame(
    comparison = c("enrolled_total_denominator", "all_child_context"),
    n = c(520L, 520L), pearson = c(0.896, 0.882),
    state_residual_pearson = c(0.607, 0.562), status = "estimated",
    stringsAsFactors = FALSE
  )
  panel <- data.frame(
    state_code_2001 = rep(c("01", "02"), each = 4L),
    ling_distance_nonzero_mean = c(1:4, 5:8),
    emi_exposure_all_children_0708 = c(1:4, 5:8),
    emi_share_enrolled_0708 = c(2:5, 6:9),
    emi_share_enrolled_public_0708 = c(3:6, 7:10),
    emi_share_enrolled_private_0708 = c(4:7, 8:11),
    enrollment_rate_0708 = c(60:63, 70:73),
    stringsAsFactors = FALSE
  )

  table <- make_paper_schooling_market_geography_table(
    list(estimates = estimates), validation, panel
  )
  csv <- table_csv_data(table)

  expect_equal(nrow(csv), 14L)
  expect_identical(
    unique(csv$panel),
    c("association", "state_organization", "administrative_validation")
  )
  expect_equal(c(
    sum(csv$panel == "association"),
    sum(csv$panel == "state_organization"),
    sum(csv$panel == "administrative_validation")
  ), c(6L, 6L, 2L))
  expect_false(any(c("status", "reason") %in% names(csv)))
  expect_equal(
    csv$raw[csv$measure_id == "enrolled_total_denominator"], 0.896
  )
  expect_equal(
    csv$state_controls_or_residual[csv$measure_id == "enrolled_total_denominator"],
    0.607
  )
  expect_equal(sum(grepl("^Panel [ABC]\\.", table$Measure)), 3L)
})

test_that("paper state-organization statistic is the state-indicator R-squared", {
  x <- data.frame(
    state_code_2001 = rep(c("01", "02"), each = 3L),
    perfectly_sorted = c(1, 1, 1, 4, 4, 4),
    no_state_signal = c(1, 2, 3, 1, 2, 3),
    stringsAsFactors = FALSE
  )
  expect_equal(paper_state_membership_r_squared(x, "perfectly_sorted"), 1)
  expect_equal(
    paper_state_membership_r_squared(x, "no_state_signal"), 0,
    tolerance = 1e-12
  )
})

test_that("paper schooling-market table fails closed when a canonical specification is missing", {
  measures <- paper_schooling_market_measure_registry()
  estimates <- do.call(rbind, lapply(seq_len(nrow(measures)), function(i) {
    data.frame(
      measure_id = measures$measure_id[[i]],
      specification_id = c("unadjusted", "region_main", "state_main"),
      standardized_estimate = c(0.4, 0.2, 0.1), n = 500L,
      stringsAsFactors = FALSE
    )
  }))
  estimates <- estimates[-1L, ]
  validation <- data.frame(
    comparison = c("enrolled_total_denominator", "all_child_context"),
    n = 500L, pearson = 0.8, state_residual_pearson = 0.6,
    status = "estimated", stringsAsFactors = FALSE
  )
  panel <- data.frame(
    state_code_2001 = c("01", "02"),
    ling_distance_nonzero_mean = c(1, 2),
    emi_exposure_all_children_0708 = c(1, 2),
    emi_share_enrolled_0708 = c(1, 2),
    emi_share_enrolled_public_0708 = c(1, 2),
    emi_share_enrolled_private_0708 = c(1, 2),
    enrollment_rate_0708 = c(1, 2), stringsAsFactors = FALSE
  )
  expect_error(
    make_paper_schooling_market_geography_table(
      list(estimates = estimates), validation, panel
    ),
    "requires all three canonical specifications",
    fixed = TRUE
  )
})



test_that("public LaTeX cells escape metacharacters before raw kable styling", {
  escaped <- latex_escape_text(c(
    "10% of children", "A & B", "x_y", "$100 #1", "{group}",
    "path\\name", "near~far", "x^2"
  ))
  expect_identical(escaped, c(
    "10\\% of children", "A \\& B", "x\\_y", "\\$100 \\#1", "\\{group\\}",
    "path\\textbackslash{}name", "near\\textasciitilde{}far", "x\\textasciicircum{}2"
  ))
  expect_identical(
    latex_escape_text("50% & x_y ~ z^2"),
    "50\\% \\& x\\_y \\textasciitilde{} z\\textasciicircum{}2"
  )

  df <- data.frame(Variable = "Enrollment", `Year / unit` = "2007-08; % of children", check.names = FALSE)
  out <- escape_table_cells_for_latex(df)
  expect_identical(out$`Year / unit`, "2007-08; \\% of children")
})

test_that("result tables preserve authored column order when N is present", {
  df <- data.frame(
    Result = "Continuous distance -> English acquisition",
    Estimate = "2.234", SE = "1.448", `p-value` = "0.123",
    `Partial R2` = "0.025", N = "1,566", Sample = "National",
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- format_public_summary_columns(df)
  expect_identical(names(out), names(df))
})

paper_language_behavior_fixture <- function() {
  registry <- paper_language_behavior_registry()
  coefficients <- data.frame(
    specification_id = registry$specification_id,
    term = registry$term,
    estimate = seq_len(nrow(registry)),
    std.error = rep(0.5, nrow(registry)),
    p.value = seq(0.01, 0.07, length.out = nrow(registry)),
    partial_r_squared = seq(0.01, 0.07, length.out = nrow(registry)),
    status = "estimated",
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
  model_summary <- data.frame(
    specification_id = registry$specification_id,
    n = c(rep(1566L, 3L), 1587L, rep(560L, 3L)),
    status = "estimated",
    stringsAsFactors = FALSE
  )
  list(coefficients = coefficients, model_summary = model_summary)
}

test_that("paper language-behavior table is the bounded national and Hindi-belt evidence set", {
  csv <- paper_language_behavior_csv_data(paper_language_behavior_fixture())

  expect_equal(nrow(csv), 7L)
  expect_equal(sum(csv$panel == "national"), 4L)
  expect_equal(sum(csv$panel == "hindi_belt"), 3L)
  expect_setequal(
    csv$specification_id,
    paper_language_behavior_registry()$specification_id
  )
  expect_equal(
    csv$term[csv$specification_id %in% c("english_distant", "english_distant_hindi_belt")],
    rep("distance_distant", 2L)
  )
  expect_true(all(vapply(
    csv[c("estimate", "std.error", "p.value", "partial_r_squared")],
    is.numeric, logical(1)
  )))
  expect_false(any(c("status", "reason") %in% names(csv)))

  table <- make_paper_language_behavior_table(paper_language_behavior_fixture())
  expect_identical(attr(table, "csv_data"), csv)
  expect_equal(sum(grepl("^Panel [AB]\\.", table$Result)), 2L)

  formatted <- format_table_for_output(table, public = TRUE)
  expect_identical(names(formatted), names(table))
  grouped <- summary_table_groups(formatted)
  expect_equal(nrow(grouped$groups), 2L)
  expect_equal(nrow(grouped$data), 7L)
})

test_that("paper language-behavior table fails closed when a registered result disappears", {
  diagnostic <- paper_language_behavior_fixture()
  diagnostic$coefficients <- diagnostic$coefficients[-1L, , drop = FALSE]

  expect_error(
    paper_language_behavior_csv_data(diagnostic),
    "requires one estimated row",
    fixed = TRUE
  )
})

paper_conversion_complements_fixture <- function() {
  complements <- paper_conversion_complement_registry()
  conversion <- merge(
    complements[c("modifier_id")],
    data.frame(
      treatment_id = c("emi_all_children", "private_emi_all_children"),
      stringsAsFactors = FALSE
    ),
    by = NULL
  )
  conversion$n <- 524L
  conversion$interaction_per_10pp_schooling_per_modifier_sd <- seq(0.01, 0.06, length.out = nrow(conversion))
  conversion$interaction_std_error_state_clustered <- 0.01
  conversion$interaction_p_value_state_clustered <- 0.02
  conversion$interaction_p_value_holm_family <- seq(0.01, 0.06, length.out = nrow(conversion))
  conversion$status <- "estimated"

  it <- data.frame(
    predictor_id = c("schooling_exposure", "linguistic_opportunity"),
    n = 514L,
    interaction_per_predictor_scale_per_modifier_sd = c(0.001, 0.008),
    interaction_std_error_clustered = c(0.004, 0.010),
    interaction_p_value_clustered = c(0.88, 0.43),
    interaction_p_value_holm_family = c(0.88, 0.87),
    status = "estimated",
    stringsAsFactors = FALSE
  )
  list(
    conversion = list(estimates = conversion),
    it = list(estimates = it)
  )
}

test_that("paper conversion-complements table is exactly the planned six-plus-two cells", {
  fixture <- paper_conversion_complements_fixture()
  csv <- paper_conversion_complements_csv_data(fixture$conversion, fixture$it)

  expect_equal(nrow(csv), 8L)
  expect_equal(sum(csv$panel == "predetermined_capacity"), 6L)
  expect_equal(sum(csv$panel == "predetermined_it_environment"), 2L)
  expect_setequal(
    csv$predictor_id[csv$panel == "predetermined_capacity"],
    c("all_child_emi", "private_emi")
  )
  expect_setequal(
    csv$predictor_id[csv$panel == "predetermined_it_environment"],
    c("all_child_emi", "linguistic_distance")
  )
  expect_true(all(vapply(
    csv[c("interaction", "std.error", "p.value", "p.value_holm")],
    is.numeric, logical(1)
  )))
  expect_false(any(c("status", "reason") %in% names(csv)))

})

test_that("paper conversion-complements table fails closed on incomplete registered evidence", {
  fixture <- paper_conversion_complements_fixture()
  fixture$it$estimates <- fixture$it$estimates[-1L, , drop = FALSE]
  expect_error(
    paper_conversion_complements_csv_data(fixture$conversion, fixture$it),
    "requires both estimated EC05 IT interactions",
    fixed = TRUE
  )
})


test_that("paper economic-conversion table consolidates welfare and complement evidence", {
  complement <- paper_conversion_complements_fixture()
  bridge <- list(estimates = paper_schooling_welfare_fixture())
  csv <- paper_economic_conversion_csv_data(bridge, complement$conversion, complement$it)

  expect_equal(nrow(csv), 28L)
  expect_equal(sum(csv$panel == "schooling_welfare"), 20L)
  expect_equal(sum(csv$panel == "predetermined_complements"), 8L)
  expect_false(any(c("status", "reason") %in% names(csv)))
  expect_true(all(vapply(csv[c("estimate", "std.error", "p.value", "p.value_holm", "n")], is.numeric, logical(1))))
  expect_true(all(csv$estimand[csv$panel == "predetermined_complements"] == "change"))
  expect_true(all(csv$outcome_round[csv$panel == "predetermined_complements"] == "hces_2022_23"))

  table <- make_paper_economic_conversion_table(bridge, complement$conversion, complement$it)
  expect_identical(attr(table, "csv_data"), csv)
  expect_equal(sum(grepl("^Panel [AB]\\.", table[[1L]])), 2L)
  expect_equal(nrow(table), 16L)
  expect_true(all(table[["2022 ANCOVA"]][9:16] == ""))
  expect_true(all(nzchar(table[["2004-2022 change"]][9:16])))
})

test_that("paper economic-conversion table fails closed when a registered welfare cell is absent", {
  complement <- paper_conversion_complements_fixture()
  estimates <- paper_schooling_welfare_fixture()
  estimates <- estimates[-1L, , drop = FALSE]
  expect_error(
    paper_economic_conversion_csv_data(list(estimates = estimates), complement$conversion, complement$it),
    "registered 4-by-5 state-main design",
    fixed = TRUE
  )
})

paper_identification_boundary_fixture <- function() {
  registry <- paper_identification_distance_registry()
  summary <- do.call(rbind, lapply(seq_len(nrow(registry)), function(i) {
    data.frame(
      construction_id = registry$construction_id[[i]],
      adjustment_id = c("unadjusted", "state_main"),
      joint_excluded_f = c(10 + i, i / 10),
      partial_r_squared = c(0.10, i / 1000),
      n = 573L,
      stringsAsFactors = FALSE
    )
  }))
  alternative <- structure(
    list(summary = summary),
    class = "emi_alternative_distance_first_stages"
  )
  dynamics <- data.frame(
    welfare_specification_id = c("long_2022__change", "long_2023__change"),
    second_stage_estimate = c(0.12, 0.14),
    second_stage_std.error = c(0.15, 0.23),
    second_stage_p.value = c(0.44, 0.56),
    effective_f = c(0.69, 0.41),
    anderson_rubin_p_beta0 = c(0.001, 0.014),
    ar_95_n_components = 2L,
    ar_95_disconnected = TRUE,
    ar_95_contains_zero = FALSE,
    ar_95_sign_identified = FALSE,
    ar_95_components = c("[-inf, -0.07] U [0.03, inf]", "[-inf, -0.05] U [0.01, inf]"),
    n = c(524L, 522L),
    status = "estimated",
    stringsAsFactors = FALSE
  )
  dynamics <- list(
    summary = dynamics,
    anderson_rubin_grid = data.frame(
      specification_id = character(), beta = numeric(), p.value = numeric(),
      stringsAsFactors = FALSE
    )
  )
  list(alternative = alternative, dynamics = dynamics)
}

test_that("paper identification boundary summarizes design classes rather than model permutations", {
  fixture <- paper_identification_boundary_fixture()
  csv <- paper_identification_boundary_csv_data(
    fixture$alternative, fixture$dynamics
  )

  expect_equal(nrow(csv), 8L)
  expect_equal(sum(csv$panel == "relevance_robustness"), 6L)
  expect_equal(sum(csv$panel == "weak_iv_outcomes"), 2L)
  expect_identical(
    csv$row_id[csv$panel == "relevance_robustness"],
    paper_identification_distance_registry()$construction_id
  )
  expect_true(all(vapply(
    csv[c("raw_f", "state_f", "effective_f", "n")],
    is.numeric, logical(1)
  )))
  expect_true(all(csv$ar_disconnected[csv$panel == "weak_iv_outcomes"] %in% TRUE))
  expect_false(any(c("status", "reason") %in% names(csv)))

  table <- make_paper_identification_boundary_table(
    fixture$alternative, fixture$dynamics
  )
  expect_identical(attr(table, "csv_data"), csv)
  expect_equal(sum(grepl("^Panel [AB]\\.", table[[1L]])), 2L)
  expect_false(any(grepl("408", table[[1L]], fixed = TRUE)))
})

test_that("paper identification boundary requires canonical dynamics object shape", {
  fixture <- paper_identification_boundary_fixture()
  expect_error(
    paper_identification_boundary_csv_data(
      fixture$alternative, fixture$dynamics$summary
    ),
    "requires canonical consumption-IV dynamics outputs",
    fixed = TRUE
  )
})

test_that("paper identification boundary fails closed when a registered design disappears", {
  fixture <- paper_identification_boundary_fixture()
  fixture$alternative$summary <- fixture$alternative$summary[
    !(fixture$alternative$summary$construction_id == "glottolog_mean" &
        fixture$alternative$summary$adjustment_id == "state_main"),
    , drop = FALSE
  ]

  expect_error(
    paper_identification_boundary_csv_data(fixture$alternative, fixture$dynamics),
    "requires unadjusted and state-main first stages",
    fixed = TRUE
  )
})

paper_local_development_fixture_result <- function(outcomes, adjustment, estimates, p_values, p_holm, n = 355L) {
  reduced <- data.frame(
    outcome_id = outcomes,
    adjustment_id = adjustment,
    construction_id = "nonzero_mean",
    estimate = estimates,
    std.error = rep(0.01, length(outcomes)),
    p.value = p_values,
    p_holm_within_spec = p_holm,
    n = as.integer(n),
    status = "estimated",
    stringsAsFactors = FALSE
  )
  list(
    registry = data.frame(), sample_coverage = data.frame(), sample_support = data.frame(),
    first_stage = data.frame(), reduced_form = reduced, weak_iv = data.frame()
  )
}

paper_local_development_fixture <- function() {
  household <- list(estimates = data.frame(
    predictor_id = "linguistic_opportunity",
    outcome_id = c("literacy_depth", "graduate_access"),
    estimate = c(0.009, 0.004),
    std_error_state_clustered = c(0.003, 0.001),
    p_value_state_clustered = c(0.01, 0.006),
    p_value_holm_predictor_family = c(0.03, 0.02),
    n = 355L, status = "estimated", stringsAsFactors = FALSE
  ))
  migration <- paper_local_development_fixture_result(
    c("skilled_recent_work_migration", "interstate_migrant_composition"),
    "state_main", c(0.013, 0.001), c(0.003, 0.8), c(0.03, 1)
  )
  housing <- paper_local_development_fixture_result(
    c("banking_access_change", "television_access_change"),
    "state_main", c(0.048, 0.014), c(0.0001, 0.005), c(0.001, 0.03)
  )
  economic_census <- paper_local_development_fixture_result(
    c("services_employment_share_change", "manufacturing_employment_share_change", "nonfarm_employment_growth"),
    "region_main", c(0.021, -0.018, -0.023), c(0.002, 0.004, 0.31), c(0.012, 0.021, 0.92), n = 354L
  )
  plfs <- paper_local_development_fixture_result(
    c("labor_force_participation_age15plus", "employment_rate_age15plus"),
    "state_main", c(-0.007, -0.009), c(0.20, 0.13), c(0.27, 0.27), n = 467L
  )
  nss66 <- paper_local_development_fixture_result(
    c("labor_force_participation_age15plus", "employment_rate_age15plus"),
    "state_main", c(0.005, 0.004), c(0.75, 0.80), c(1, 1), n = 458L
  )
  list(
    household = household, migration = migration, housing = housing,
    economic_census = economic_census, nss66 = nss66, plfs = plfs
  )
}

test_that("paper local-development table keeps a bounded signal-and-null constellation", {
  fixture <- paper_local_development_fixture()
  csv <- paper_local_development_csv_data(
    fixture$household, fixture$migration, fixture$housing,
    fixture$economic_census, fixture$nss66, fixture$plfs
  )

  expect_equal(nrow(csv), 11L)
  expect_identical(csv$row_id, paper_local_development_registry()$row_id)
  expect_setequal(
    csv$domain,
    c("Household capacity", "Migration", "Finance", "Assets", "Economic structure", "Scale", "Labor")
  )
  expect_true(all(vapply(
    csv[c("estimate", "std.error", "p.value", "p.value_holm", "n")],
    is.numeric, logical(1)
  )))
  expect_true(all(is.finite(csv$nss66_p_value_holm[csv$source_id == "plfs_2017_18"])))
  expect_true(all(is.na(csv$nss66_p_value_holm[csv$source_id != "plfs_2017_18"])))
  expect_true(any(csv$p.value_holm < 0.05))
  expect_true(any(csv$p.value_holm >= 0.05))
  expect_false(any(c("status", "reason") %in% names(csv)))

  table <- make_paper_local_development_table(
    fixture$household, fixture$migration, fixture$housing,
    fixture$economic_census, fixture$nss66, fixture$plfs
  )
  expect_identical(attr(table, "csv_data"), csv)
  expect_identical(
    names(table),
    c("Domain", "Representative outcome", "Estimate", "Raw p", "Holm p", "Interpretation")
  )
})

test_that("paper local-development table fails closed when registered evidence disappears", {
  fixture <- paper_local_development_fixture()
  fixture$migration$reduced_form <- fixture$migration$reduced_form[-1L, , drop = FALSE]

  expect_error(
    paper_local_development_csv_data(
      fixture$household, fixture$migration, fixture$housing,
      fixture$economic_census, fixture$nss66, fixture$plfs
    ),
    "requires one estimated state_main / nonzero_mean row",
    fixed = TRUE
  )
})

test_that("Appendix E selection exhibits consolidate existing evidence without new estimation", {
  selection <- data.frame(
    AGE = c(8, 10, 12), HH_SIZE = c(4, 5, 6),
    ENROLLMENT_COST = c(100, 200, 300),
    dmean_num_IS_EDU_FREE = c(.2, .3, .4),
    dmean_num_TUTION_FEE_WAIVED = c(.1, .2, .3),
    dmean_num_RECD_SCHOLARSHIP_STIPEND = c(.1, .1, .2),
    dmean_num_RECD_TXT_BOOKS = c(.3, .4, .5),
    dmean_num_RECD_STATIONERY = c(.2, .3, .4),
    dmean_num_MID_DAY_MEAL_ETC_RECD = c(.4, .5, .6),
    dmean_num_ENROLLMENT_COST = c(150, 180, 210),
    SEX = c("Male", "Female", "Male"),
    RELIGION = c("Hindu", "Muslim", "Hindu"),
    SOCIAL_GROUP = c("Other", "SC", "ST"),
    SECTOR = c("Rural", "Urban", "Rural"),
    DIST_FROM_NEAREST_PRIMARY_CLASS = c("<1 km", "1-2 km", "<1 km"),
    father_educ = c("Primary", "Secondary", "Graduate"),
    stringsAsFactors = FALSE
  )
  missingness <- structure(
    list(
      missing_counts = data.frame(
        missing_var = c("AGE", "father_educ", "Total probit-model with NA"),
        n_missing = c(0L, 1L, 1L),
        pct_missing = c(0, 1 / 3, 1 / 3),
        stringsAsFactors = FALSE
      ),
      logit_summary = data.frame(
        missing_var = "father_educ", n_sig = 2L, pseudoR2 = 0.25,
        stringsAsFactors = FALSE
      )
    ),
    class = c("emi_missingness_diagnostics", "list")
  )

  exhibits <- make_appendix_selection_exhibits(selection, missingness)
  expect_setequal(
    names(exhibits),
    c("appendix_e1_selection_sample", "appendix_e4_missingness", "missingness_plot_data")
  )
  e1 <- exhibits$appendix_e1_selection_sample
  expect_true(nrow(e1) >= 6L)
  expect_true(all(c("Variable", "Type", "N", "Summary") %in% names(e1)))
  expect_false(any(c("status", "reason") %in% names(attr(e1, "csv_data"))))

  e4 <- exhibits$appendix_e4_missingness
  e4_csv <- attr(e4, "csv_data")
  expect_equal(nrow(e4_csv), 2L)
  expect_false(any(grepl("^Total probit-model", e4_csv$variable)))
  expect_equal(e4_csv$pseudo_r_squared[e4_csv$variable == "father_educ"], 0.25)
  expect_true(is.na(e4_csv$pseudo_r_squared[e4_csv$variable == "AGE"]))
})

test_that("Appendix E missingness exhibit requires canonical diagnostics", {
  expect_error(
    appendix_missingness_diagnostics_table(data.frame()),
    "requires canonical missingness diagnostics",
    fixed = TRUE
  )
})

test_that("Appendix A construction bundle is registry-driven and methodologically bounded", {
  source_registry <- appendix_a1_source_registry()
  data_sources <- data.frame(
    source_id = source_registry$source_id,
    source_name = paste("Source", seq_len(nrow(source_registry))),
    used_in_current_pipeline = TRUE,
    stringsAsFactors = FALSE
  )
  lineage <- list(
    mapping_rule_sensitivity = data.frame(
      rule = c("conservative", "primary", "full_reviewed"),
      source_rows_2007_08 = c(580L, 588L, 588L),
      source_rows_2017_18 = c(476L, 645L, 691L),
      two_wave_target_districts = c(427L, 573L, 587L),
      stringsAsFactors = FALSE
    ),
    source_registry = data.frame(
      source_id = c("census2001_c16", "shrug_pc_keys", "manual_review"),
      citation = c("Census 2001", "SHRUG keys", "Manual adjudication"),
      stringsAsFactors = FALSE
    ),
    source_inventory = data.frame(
      source_id = c("census2001_c16", "shrug_pc_keys", "manual_review"),
      role = c("current_registry", "published_concordance", "adjudication"),
      stringsAsFactors = FALSE
    )
  )

  exhibits <- make_appendix_data_construction_exhibits(
    data_sources, lineage, read_consumption_survey_registry()
  )
  expect_setequal(names(exhibits), c(
    "appendix_a1_data_source_timing", "appendix_a2_lineage_logic",
    "appendix_a3_lineage_source_hierarchy", "appendix_a4_nss_schooling_constructs",
    "appendix_a5_dise_construction", "appendix_a6_linguistic_measures",
    "appendix_a7_consumption_construction", "appendix_a8_other_outcome_panels"
  ))
  expect_s3_class(exhibits$appendix_a2_lineage_logic, "ggplot")
  a3 <- attr(exhibits$appendix_a3_lineage_source_hierarchy, "csv_data")
  expect_setequal(a3$tier, c(
    "Official administrative source", "Independent concordance / geometry", "QA / adjudication evidence"
  ))

  a1 <- attr(exhibits$appendix_a1_data_source_timing, "csv_data")
  expect_identical(a1$source_id, source_registry$source_id)
  expect_equal(nrow(a1), 19L)

  a4 <- attr(exhibits$appendix_a4_nss_schooling_constructs, "csv_data")
  expect_identical(a4$construct_id, c(
    "enrollment", "emi_enrolled", "emi_all_children", "public_emi", "private_emi"
  ))
  expect_identical(
    a4$denominator[a4$construct_id == "emi_enrolled"],
    "Enrolled children with known medium"
  )
  expect_true(all(grepl("Eligible children age 5-19", a4$denominator[c(1, 3, 4, 5)], fixed = TRUE)))

  a5 <- attr(exhibits$appendix_a5_dise_construction, "csv_data")
  expect_equal(nrow(a5), 8L)
  expect_true("emi_total_0708" %in% a5$construct_id)

  a7 <- attr(exhibits$appendix_a7_consumption_construction, "csv_data")
  expect_true(all(c("nss_2004_05", "hces_2022_23", "hces_2023_24") %in% a7$survey_id))
  expect_false(any(c("status", "reason") %in% names(a7)))
})

test_that("Appendix A fails closed when publication source metadata drifts", {
  sources <- appendix_a1_source_registry()
  data_sources <- data.frame(
    source_id = sources$source_id[-1L],
    source_name = sources$source_id[-1L],
    used_in_current_pipeline = TRUE,
    stringsAsFactors = FALSE
  )
  expect_error(
    appendix_a1_data_source_timing(data_sources),
    "source IDs absent from data_sources.csv",
    fixed = TRUE
  )
  expect_error(
    appendix_a2_lineage_logic_data(list(mapping_rule_sensitivity = data.frame())),
    "mapping-rule sensitivity metadata",
    fixed = TRUE
  )
})

test_that("Appendix B/C historical validation tables preserve registered scientific families", {
  primary <- list(comparison_summary = data.frame(
    measure_id = paste0("m", 1:2), exact_required = TRUE,
    compared_districts = c(10L, 10L), exact_matches = c(10L, 10L), stringsAsFactors = FALSE
  ))
  helms <- list(summary = data.frame(
    n_atlas_preferred_overlap = 12L, pearson_correlation = .99,
    median_absolute_difference = .01, stringsAsFactors = FALSE
  ))
  persistence <- list(summary = data.frame(
    sample = "preferred_geography", measure_id = "nonzero_mean", n_districts = 9L,
    pearson = .90, population_weighted_pearson = .93, stringsAsFactors = FALSE
  ))
  panel <- data.frame(
    ling_distance_nonzero_mean = c(1, 2, 3), ling_mapped_speaker_share = c(.9, .8, .7),
    ling_distance_glottolog_nonhindi_mean = c(2, 3, NA), ling_glottolog_mapped_speaker_share = c(.8, .7, .6),
    ling_distance_dyen_noncognate_pct = c(40, NA, 60), ling_dyen_mapped_speaker_share = c(.7, .6, .5),
    stringsAsFactors = FALSE
  )
  b6 <- appendix_b6_language_source_validation(primary, helms, persistence, panel)
  b6_csv <- attr(b6, "csv_data")
  expect_equal(nrow(b6_csv), 6L)
  expect_identical(b6_csv$validation[1:3], c(
    "Shastry 2001 district coverage", "Glottolog 2001 district coverage", "Dyen 2001 district coverage"
  ))
  expect_identical(b6_csv$validation[4:6], c(
    "Official Census 1991 source checks", "Helms-Lim vs project 1991 distance",
    "Project 1991 vs 2001 distance"
  ))

  predictors <- c("eventual_emie", "census_2001_ld", "helms_lim_ld_1991")
  domains <- c("demography", "human_capital", "economic_structure", "rural_development", "urban_development")
  joint <- expand.grid(predictor_id = predictors, domain = domains, stringsAsFactors = FALSE)
  joint$sample <- "preferred_geography"; joint$predictor <- joint$predictor_id
  joint$n_tested_covariates <- 2L; joint$joint_f <- seq_len(nrow(joint)) / 10
  joint$joint_p <- .5; joint$n <- 90L; joint$n_states <- 20L
  joint$status <- "estimated"; joint$reason <- NA_character_
  c7 <- appendix_c7_historical_balance(list(joint_balance = joint))
  expect_equal(nrow(attr(c7, "csv_data")), 15L)
  expect_false(any(c("status", "reason") %in% names(attr(c7, "csv_data"))))

  ids <- c("instrument_only", "region_fe_census_controls", "state_fe_census_controls", "region_fe_expanded_controls", "state_fe_expanded_controls")
  comp <- data.frame(
    sample = "preferred_geography", specification_id = ids, specification = ids,
    excluded_instrument_f_1991 = 1:5, partial_r_squared_1991 = seq(.01, .05, .01), n_1991 = 89L,
    excluded_instrument_f_2001 = 2:6, partial_r_squared_2001 = seq(.02, .06, .01), n_2001 = 89L,
    status_1991 = "estimated", status_2001 = "estimated", stringsAsFactors = FALSE
  )
  c9 <- appendix_c9_historical_first_stage(list(comparison = comp))
  expect_equal(nrow(attr(c9, "csv_data")), 5L)
  expect_identical(attr(c9, "csv_data")$specification_id, ids)
})

test_that("Appendix B2 lineage sensitivity uses the same first-stage contract across variants", {
  variants <- c("conservative", "primary", "full_reviewed")
  review <- list(
    panel_summary = data.frame(
      panel_variant = variants,
      unique_districts = c(427L, 573L, 587L),
      complete_iv_rows = c(427L, 573L, 587L),
      stringsAsFactors = FALSE
    ),
    first_stage = data.frame(
      panel_variant = variants, model = "consumption",
      term = "ling_distance_nonzero_mean",
      partial_f = c(.22, .75, .76), effective_f = c(.25, .82, .82),
      nobs = c(427L, 573L, 587L), status = "estimated",
      stringsAsFactors = FALSE
    )
  )
  out <- appendix_b2_lineage_sensitivity(review)
  csv <- attr(out, "csv_data")
  expect_identical(csv$panel_variant, variants)
  expect_identical(csv$n_districts, c(427L, 573L, 587L))
  expect_error(
    appendix_b2_lineage_sensitivity(within(review, first_stage <- first_stage[-1L, ])),
    "all three registered lineage variants",
    fixed = TRUE
  )
})

test_that("Appendix B8 summarizes exact Census universe reconciliations and fails closed", {
  exact <- function(n = 640L, value = 0) data.frame(n_districts = n, max_abs_difference = value)
  migration <- list(
    d02_d03_2011_total_validation = data.frame(n_districts = 640L, max_abs_total_difference = 0),
    d03_d07_2011_recent_work_validation = exact(),
    d02_population_2011_validation = data.frame(n_districts = 640L, max_migrant_stock_share_population = .8)
  )
  housing <- list(
    source_validation_2001 = data.frame(n_reference_districts = 593L, n_overlap_districts = c(587L, 593L), max_abs_difference = 0),
    source_validation_2011 = data.frame(n_reference_districts = 640L, n_overlap_districts = 640L, max_abs_difference = 0)
  )
  households <- list(source_validation_2001 = exact(593L), source_validation_2011 = exact())
  workers <- list(
    b25_b26_2001_main_occupation_validation = exact(593L),
    b04_b25a_universe_validation = exact(),
    b06_b25b_universe_validation = exact()
  )
  out <- appendix_b8_census_universe_reconciliation(migration, housing, households, workers)
  csv <- attr(out, "csv_data")
  expect_equal(nrow(csv), 10L)
  expect_setequal(unique(csv$domain), c("Migration", "Housing", "Households", "Workers"))
  expect_true(all(csv$value[csv$diagnostic == "Max absolute count difference"] == 0))
  workers$b04_b25a_universe_validation$max_abs_difference <- 1
  expect_error(
    appendix_b8_census_universe_reconciliation(migration, housing, households, workers),
    "exact Census universe reconciliations",
    fixed = TRUE
  )
})

test_that("Appendix B validation plots enforce common registered support", {
  welfare <- expand.grid(
    district_2001 = c("d1", "d2", "d3"),
    round_id = c("hces_2022_23", "hces_2023_24"),
    outcome_id = c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce"),
    stringsAsFactors = FALSE
  )
  welfare$estimate <- seq_len(nrow(welfare)); welfare$preferred_eligible <- TRUE
  b4 <- appendix_b4_hces_consistency_data(welfare)
  expect_equal(as.integer(table(b4$outcome_id)), rep(3L, 3L))
  b4_summary <- appendix_b4_hces_consistency_summary(welfare)
  expect_equal(nrow(attr(b4_summary, "csv_data")), 3L)
  expect_true(all(attr(b4_summary, "csv_data")$n == 3L))

  panel <- data.frame(
    state_code_2001 = rep(c("01", "02"), each = 2),
    dise_emi_enrollment_share_total_0708 = c(10, 20, 30, 40),
    emi_share_enrolled_0708 = c(12, 18, 29, 43), stringsAsFactors = FALSE
  )
  validation <- data.frame(
    comparison = "enrolled_total_denominator", n = 4L, status = "estimated",
    stringsAsFactors = FALSE
  )
  b7 <- appendix_b7_nss_dise_data(panel, validation)
  expect_equal(nrow(b7), 4L)
  expect_lt(abs(mean(b7$dise_residual[b7$state == "01"])), 1e-12)
  expect_lt(abs(mean(b7$nss_residual[b7$state == "02"])), 1e-12)
})

test_that("Appendix B1 lineage readiness fails closed and reports the three registered panel variants", {
  lineage <- list(
    summary = data.frame(
      metric = c("admin_units_2001", "accepted_source_matches"),
      value = c(593, 1254), stringsAsFactors = FALSE
    ),
    readiness = data.frame(gate = c("a", "b"), passed = TRUE, stringsAsFactors = FALSE),
    blockers = data.frame(),
    panel_variant_summary = data.frame(
      panel_variant = c("conservative", "primary", "full_reviewed"),
      two_wave_target_districts = c(427, 573, 587), stringsAsFactors = FALSE
    )
  )
  out <- appendix_b1_lineage_readiness(lineage)
  csv <- attr(out, "csv_data")
  expect_equal(nrow(csv), 7L)
  expect_identical(csv$metric[5:7], c("Conservative", "Primary", "Full reviewed"))
  expect_identical(as.integer(csv$value[5:7]), c(427L, 573L, 587L))

  lineage$readiness$passed[[1]] <- FALSE
  expect_error(appendix_b1_lineage_readiness(lineage), "all lineage readiness gates pass", fixed = TRUE)
})


test_that("Appendix B benchmark and DISE publication tables fail closed on validation failures", {
  mpce <- data.frame(
    survey_id = rep(paste0("survey_", 1:9), each = 2L),
    sector = rep(c("rural", "urban"), 9L),
    mpce_definition = "registered detailed MPCE",
    expected_mpce = seq_len(18L) * 100,
    estimate_mpce = seq_len(18L) * 100,
    abs_difference = 0,
    passed = TRUE,
    stringsAsFactors = FALSE
  )
  b3 <- appendix_b3_consumption_reconstruction(mpce)
  expect_equal(nrow(attr(b3, "csv_data")), 18L)
  expect_error(
    appendix_b3_consumption_reconstruction(mpce[-1L, , drop = FALSE]),
    "all 18 registered national consumption benchmark cells",
    fixed = TRUE
  )
  mpce$passed[[2]] <- FALSE
  expect_error(appendix_b3_consumption_reconstruction(mpce), "may only publish passed consumption benchmarks", fixed = TRUE)

  dise <- data.frame(
    academic_year = "2005-06", state = "Jammu and Kashmir", district = "Kupwara",
    metric = "medium_slot_1", expected_value = 10, actual_value = 10, difference = 0,
    matches = TRUE, source_pdf = "report.pdf", source_page = 1L, stringsAsFactors = FALSE
  )
  b9 <- appendix_b9_dise_publication_validation(dise)
  expect_equal(nrow(attr(b9, "csv_data")), 1L)
  dise$matches[[1]] <- FALSE
  expect_error(appendix_b9_dise_publication_validation(dise), "requires passing DISE publication checks", fixed = TRUE)
})
