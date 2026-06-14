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


test_that("population summary statistics use comma integers without artificial decimals", {
  out <- legacy_numeric_stats(
    data.frame(npeople_0708 = c(1234.4, 98765.6)),
    data.frame(var = "npeople_0708", label = "Population", stringsAsFactors = FALSE),
    count_vars = "npeople_0708"
  )

  expect_equal(out$Min[[1]], "1,234")
  expect_equal(out$Max[[1]], "98,766")
  expect_false(grepl("\\.00$", out$Mean[[1]]))
})

test_that("probit table uses legacy AME estimate and standard-error columns", {
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


test_that("legacy regression GOF map includes residual standard error", {
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

  gof <- legacy_modelsummary_gof_map("fs_cons")
  clean <- vapply(gof, `[[`, character(1), "clean")

  expect_true("Residual Std. Error" %in% clean)
  expect_true("Model's F-Statistic" %in% clean)
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

test_that("regression captions are plain legacy titles", {
  expect_equal(table_caption("fs_cons"), "First-Stage Regression: EMI Exposure on Linguistic Distance")
  expect_equal(table_caption("cons_iv"), "Second-Stage Regression: Consumption Growth on EMIE (Fitted)")
  expect_equal(table_caption("probit_mfx"), "Average Marginal Effects and Counterfactual Comparisons for Enrollment Probit")
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

  expect_match(tex, "Second-Stage Regression: Consumption Growth on EMIE", fixed = TRUE)
  expect_match(tex, "Standard errors clustered by state", fixed = TRUE)
  expect_false(grepl(">~<|& ~ &|^~$", tex))
})

test_that("legacy modelsummary regression writer emits LaTeX rather than HTML", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")
  model <- lm(mpg ~ wt, data = mtcars)

  tex <- paste(as.character(legacy_modelsummary_table(model, "fs_cons")), collapse = "\n")
  tex <- paste(normalize_quarto_table_labels(tex, "fs_cons"), collapse = "\n")

  expect_match(tex, "\\begin{table}", fixed = TRUE)
  expect_match(tex, "\\label{tbl-fs-cons}", fixed = TRUE)
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

test_that("caption setup is inserted for wrapping long table captions", {
  path <- file.path("scripts", "postprocess_public_qmds.R")
  if (!file.exists(path)) path <- file.path("..", "..", "scripts", "postprocess_public_qmds.R")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")

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

  formatted <- format_ame_results(mfx)
  table <- make_probit_ame_table(formatted, n = 100)

  expect_s3_class(attr(formatted, "legacy_marginaleffects"), "marginaleffects")
  expect_s3_class(attr(table, "legacy_marginaleffects"), "marginaleffects")
  expect_equal(attr(table, "legacy_marginaleffects_n"), 100)
})

test_that("probit AME table has a modelsummary-native path and no map side effects", {
  src <- paste(deparse(save_table_tex), collapse = "\n")
  expect_match(src, "legacy_ame_modelsummary_table", fixed = TRUE)
  expect_match(paste(deparse(legacy_ame_modelsummary_table), collapse = "\n"), "modelsummary::modelsummary", fixed = TRUE)
  expect_false(grepl("legacy_no_data_colour", src, fixed = TRUE))
})

test_that("probit summary table column widths are closer to IV summary width", {
  src <- paste(deparse(save_table_tex), collapse = "\n")
  expect_match(src, "5.4cm", fixed = TRUE)
  expect_match(src, "6.6cm", fixed = TRUE)
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

test_that("probit AME modelsummary output is converted to a page-breaking longtable", {
  tex <- paste0(
    "\\begin{table}[!h]\n",
    "\\centering\\centering\n",
    "\\caption{Old}\n",
    "\\begin{tabular}[t]{lc}\n",
    "\\toprule\n",
    " & Enrolled (1 = yes)\\\\\n",
    "\\midrule\n",
    "Age & -0.100\\\\\n",
    " & (0.020)\\\\\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}"
  )
  out <- legacy_ame_longtable_tex(tex, "probit_mfx")
  expect_match(out, "\\begin{longtable}", fixed = TRUE)
  expect_match(out, "p{9.0cm}", fixed = TRUE)
  expect_false(grepl("\\begin{table}", out, fixed = TRUE))
})

test_that("labeled marginaleffects object preserves public AME order", {
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
  out <- legacy_modelsummary_marginaleffects_object(formatted, native)
  expect_equal(out$term, formatted$Term)
  expect_equal(out$estimate, formatted$estimate)
})
