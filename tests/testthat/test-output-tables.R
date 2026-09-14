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


test_that("public table wrapping does not inject literal LaTeX line breaks", {
  df <- data.frame(Variable = "A very long public variable label which should wrap by column width", stringsAsFactors = FALSE)
  out <- wrap_table_text_columns(df, "sum_tbl_probit_cat")
  expect_false(grepl("\\\\", out$Variable[[1]], fixed = TRUE))
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


test_that("modelsummary datasummary alignment is a single string", {
  df <- data.frame(Term = c("Urban", ""), `Enrolled (1 = yes)` = c("0.001", "(0.002)"), check.names = FALSE)
  expect_equal(table_alignments(df, "probit_mfx"), c("l", "c"))
  expect_equal(modelsummary_align_string(df, "probit_mfx"), "lc")
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

paper_schooling_market_fixture <- function() {
  measures <- paper_schooling_market_measure_registry()
  specs <- c("unadjusted", "region_main", "state_main")
  estimates <- safe_bind_rows(lapply(seq_len(nrow(measures)), function(i) {
    data.frame(
      measure_id = measures$measure_id[[i]],
      specification_id = specs,
      standardized_estimate = c(0.4, 0.2, 0.1) + i / 1000,
      standardized_std_error = c(0.04, 0.05, 0.06),
      p.value = c(0.005, 0.04, 0.20),
      n = if (measures$measure_id[[i]] == "dise_emi_enrollment") 520L else 500L,
      stringsAsFactors = FALSE
    )
  }))
  panel <- data.frame(
    state_code_2001 = rep(c("01", "02"), each = 4L),
    ling_distance_nonzero_mean = c(1:4, 5:8),
    enrollment_rate_0708 = c(60:63, 70:73),
    emi_share_enrolled_0708 = c(2:5, 6:9),
    emi_exposure_all_children_0708 = c(1:4, 5:8),
    emi_share_enrolled_public_0708 = c(3:6, 7:10),
    emi_share_enrolled_private_0708 = c(4:7, 8:11),
    dise_emi_enrollment_share_total_0708 = c(5, 7, 6, 8, 20, 22, 21, 23),
    stringsAsFactors = FALSE
  )
  list(
    district_mechanisms = list(estimates = estimates),
    panel = panel
  )
}

test_that("paper schooling-market target depends on the DISE-enriched district panel", {
  command <- parse(text = repo_target_command("paper_schooling_market_geography"))[[1L]]
  dependencies <- targets::tar_deps_raw(command)

  expect_true("district_panel_with_dise" %in% dependencies)
})

test_that("paper schooling-market table preserves regression inference and state organization", {
  fixture <- paper_schooling_market_fixture()
  table <- make_paper_schooling_market_geography_table(
    fixture$district_mechanisms, fixture$panel
  )
  csv <- table_csv_data(table)

  expect_equal(nrow(csv), 25L)
  expect_equal(c(
    sum(csv$panel == "association"),
    sum(csv$panel == "state_organization")
  ), c(18L, 7L))
  expect_false("source_agreement" %in% csv$panel)
  expect_true(all(vapply(
    csv[c("estimate", "std_error", "p_value")], is.numeric, logical(1)
  )))
  expect_true(all(is.finite(csv$std_error[csv$panel == "association"])))
  expect_true(all(is.finite(csv$p_value[csv$panel == "association"])))
  expect_setequal(
    unique(csv$specification_id[csv$panel == "association"]),
    c("unadjusted", "region_main", "state_main")
  )
  expect_false(any(c("raw", "state_residual") %in% csv$specification_id))

  schooling_ids <- paper_schooling_market_measure_registry()$measure_id
  schooling_state <- csv[
    csv$panel == "state_organization" & csv$measure_id %in% schooling_ids,
    , drop = FALSE
  ]
  expect_setequal(schooling_state$measure_id, schooling_ids)
  expect_true(all(is.finite(schooling_state$estimate)))
  expect_equal(
    schooling_state$estimate[schooling_state$measure_id == "dise_emi_enrollment"],
    paper_state_membership_r_squared(
      fixture$panel, "dise_emi_enrollment_share_total_0708"
    )
  )

  expect_identical(names(table), "Term")
  expect_identical(table$Term, "Linguistic distance from Hindi")
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

test_that("paper schooling-market table fails closed when regression inference is incomplete", {
  fixture <- paper_schooling_market_fixture()
  fixture$district_mechanisms$estimates <- fixture$district_mechanisms$estimates[-1L, ]
  expect_error(
    make_paper_schooling_market_geography_table(
      fixture$district_mechanisms, fixture$panel
    ),
    "requires all three registered specifications",
    fixed = TRUE
  )

  fixture <- paper_schooling_market_fixture()
  fixture$district_mechanisms$estimates$standardized_std_error <- NULL
  expect_error(
    paper_schooling_market_geography_csv_data(
      fixture$district_mechanisms, fixture$panel
    ),
    "missing regression fields",
    fixed = TRUE
  )
})

test_that("public table captions fall back safely for unregistered names", {
  expect_identical(
    public_table_caption_text("temporary_table"),
    "temporary_table"
  )
})

test_that("paper schooling-market renderer uses modelsummary regression blocks", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")
  fixture <- paper_schooling_market_fixture()
  table <- make_paper_schooling_market_geography_table(
    fixture$district_mechanisms, fixture$panel
  )
  dir <- tempfile("schooling-market-")
  dir.create(dir)
  path <- save_table_tex(
    table = table,
    path = file.path(dir, "paper_schooling_market_geography.tex"),
    name = "paper_schooling_market_geography",
    public = TRUE
  )
  tex <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(tex, "(0.040)", fixed = TRUE)
  expect_match(tex, "***", fixed = TRUE)
  expect_match(tex, "Region fixed effects", fixed = TRUE)
  expect_match(tex, "State fixed effects", fixed = TRUE)
  expect_match(tex, "Predetermined controls", fixed = TRUE)
  expect_match(tex, "State-membership $R^2$", fixed = TRUE)
  expect_false(grepl("NSS-DISE correlation", tex, fixed = TRUE))
})


test_that("public LaTeX table text escapes metacharacters before raw kable styling", {
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
  out <- escape_table_for_latex(df)
  expect_identical(names(out), c("Variable", "Year / unit"))
  expect_identical(out$`Year / unit`, "2007-08; \\% of children")

  header_df <- data.frame(`Zero in exact 95% set` = "Yes", check.names = FALSE)
  escaped_header <- escape_table_for_latex(header_df)
  expect_identical(names(escaped_header), "Zero in exact 95\\% set")
})

paper_language_behavior_fixture <- function() {
  registry <- paper_language_behavior_registry()
  coefficients <- data.frame(
    specification_id = registry$specification_id,
    term = registry$term,
    estimate = seq_len(nrow(registry)),
    std.error = rep(0.5, nrow(registry)),
    p.value = c(0.005, 0.02, 0.04, 0.06, 0.08, 0.20, 0.70),
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

test_that("paper language-behavior renderer uses standard economics regression layout", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")

  table <- make_paper_language_behavior_table(paper_language_behavior_fixture())
  tex <- paste(
    as.character(paper_language_behavior_modelsummary_table(table, "paper_language_behavior")),
    collapse = "\n"
  )

  expect_match(tex, "(0.500)", fixed = TRUE)
  expect_match(tex, "***", fixed = TRUE)
  expect_match(tex, "**", fixed = TRUE)
  expect_match(tex, "Outcome", fixed = TRUE)
  expect_match(tex, "Population", fixed = TRUE)
  expect_match(tex, "Partial $R^2$", fixed = TRUE)
})

test_that("regression stars use the manuscript-wide economics convention", {
  expect_equal(
    significance_stars(c(0.20, 0.08, 0.04, 0.009)),
    c("", "*", "**", "***")
  )
  expect_equal(
    regression_star_levels(),
    c("*" = 0.10, "**" = 0.05, "***" = 0.01)
  )
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

test_that("paper conversion-complements table fails closed on incomplete registered evidence", {
  fixture <- paper_conversion_complements_fixture()
  fixture$it$estimates <- fixture$it$estimates[-1L, , drop = FALSE]
  expect_error(
    paper_conversion_complements_csv_data(fixture$conversion, fixture$it),
    "requires both estimated EC05 IT interactions",
    fixed = TRUE
  )
})


test_that("paper economic-conversion renderer uses modelsummary regression structure", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")

  complement <- paper_conversion_complements_fixture()
  bridge <- list(estimates = paper_schooling_welfare_fixture())
  table <- make_paper_economic_conversion_table(bridge, complement$conversion)
  tex <- as.character(paper_economic_conversion_modelsummary_table(table, "paper_economic_conversion"))

  expect_match(tex, "\\([0-9]+\\.[0-9]{2}\\)")
  expect_match(tex, "\\*\\*")
  expect_match(tex, "State fixed effects", fixed = TRUE)
  expect_match(tex, "Predetermined controls", fixed = TRUE)
})

test_that("paper economic-conversion table fails closed when a registered welfare cell is absent", {
  complement <- paper_conversion_complements_fixture()
  estimates <- paper_schooling_welfare_fixture()
  estimates <- estimates[-1L, , drop = FALSE]
  expect_error(
    paper_economic_conversion_csv_data(list(estimates = estimates), complement$conversion),
    "registered 4-by-5 state-main design",
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

test_that("selection tables report the fitted estimation sample rather than the child roster", {
  # Balanced outcomes at every observed x value avoid separation while the NA
  # row still verifies that nobs() reflects the fitted complete-case sample.
  dat <- data.frame(
    y = c(0, 1, 0, 1, 0, 1, 1, 0, 1),
    x = c(0, 0, 1, 1, 2, 2, NA, 3, 3)
  )
  fit <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  expect_equal(selection_model_observations(fit, fallback = nrow(dat)), stats::nobs(fit))
  expect_lt(stats::nobs(fit), nrow(dat))
  expect_equal(selection_model_observations(NULL, fallback = nrow(dat)), nrow(dat))
})

iv_relevance_fixture <- function() {
  constructions <- c(
    "nonzero_mean", "top3_legacy", "glottolog_mean",
    "dyen_noncognate", "distant_share", "distance_shares_all"
  )
  adjustments <- c("unadjusted", "region_main", "state_main")
  modern <- expand.grid(
    adjustment_id = adjustments,
    construction_id = constructions,
    stringsAsFactors = FALSE
  )
  modern$joint_excluded_f <- seq_len(nrow(modern)) / 2
  modern$partial_r_squared <- seq_len(nrow(modern)) / 1000
  modern$n <- 573L

  historical <- data.frame(
    sample = "preferred_geography",
    specification_id = c(
      "instrument_only", "region_fe_census_controls", "state_fe_census_controls"
    ),
    excluded_instrument_f_1991 = c(9.9, 0.3, 0.04),
    partial_r_squared_1991 = c(.2, .01, .001),
    n_1991 = 89L,
    status_1991 = "estimated",
    stringsAsFactors = FALSE
  )

  list(
    modern = structure(list(summary = modern), class = "emi_alternative_distance_first_stages"),
    historical = list(comparison = historical)
  )
}


test_that("IV appendix relevance summary preserves registered geographic comparisons", {
  fixture <- iv_relevance_fixture()
  out <- appendix_iv_relevance_summary(fixture$modern, fixture$historical)
  csv <- attr(out, "csv_data")

  expect_setequal(
    csv$row_id,
    c(
      "nonzero_mean", "top3_legacy", "glottolog_mean", "dyen_noncognate",
      "distant_share", "distance_shares_all", "historical_1991"
    )
  )
  expect_true(all(csv$n[csv$row_id != "historical_1991"] == 573L))
  expect_equal(csv$n[csv$row_id == "historical_1991"], 89L)
  expect_true(all(is.finite(csv$state_f)))
  expect_true(all(is.finite(csv$state_partial_r2)))

  broken <- fixture$modern
  broken$summary <- broken$summary[!(
    broken$summary$construction_id == "nonzero_mean" &
      broken$summary$adjustment_id == "state_main"
  ), , drop = FALSE]
  expect_error(
    appendix_iv_relevance_summary(broken, fixture$historical),
    "requires all geographic adjustments",
    fixed = TRUE
  )
})


iv_weak_fixture <- function() {
  dynamics <- list(summary = data.frame(
    welfare_specification_id = c("long_2022__change", "long_2023__change"),
    outcome_round = c("hces_2022_23", "hces_2023_24"),
    second_stage_estimate = c(.12, .14),
    second_stage_std.error = c(.15, .23),
    second_stage_p.value = c(.44, .56),
    effective_f = c(.69, .41),
    anderson_rubin_p_beta0 = c(.001, .014),
    ar_95_components = c("[-.2,-.07] U [.03,.2]", "[-.19,-.05] U [.02,.19]"),
    ar_95_disconnected = TRUE,
    ar_95_sign_identified = FALSE,
    n = c(524L, 522L),
    status = "estimated",
    stringsAsFactors = FALSE
  ))

  exclusion <- list(summary = data.frame(
    specification_id = c(
      "consumption__long_2022__change", "consumption__long_2023__change"
    ),
    calibration_id = "exact_exclusion",
    minimum_gamma_for_zero_95 = c(.027, .011),
    minimum_gamma_share_of_reduced_form_for_zero_95 = c(.417, .199),
    stringsAsFactors = FALSE
  ))

  robustness <- list(family_summary = data.frame(
    family = c("scalar", "controls", "welfare"),
    n_models = c(10L, 10L, 10L),
    n_strong_first_stage = 0L,
    stringsAsFactors = FALSE
  ))

  fas <- data.frame(
    adjustment_id = "state_main",
    construction_id = c(
      "distance_shares_all", "distance_shares_all_unmapped", "distance_shares_mapped"
    ),
    status = "estimated",
    fas_contains_zero = TRUE,
    constituent_relevance_caution = TRUE,
    stringsAsFactors = FALSE
  )
  alternative <- structure(
    list(falsification_adaptive_summary = fas),
    class = "emi_alternative_distance_first_stages"
  )

  list(
    dynamics = dynamics,
    exclusion = exclusion,
    robustness = robustness,
    alternative = alternative
  )
}


test_that("IV appendix weak-inference summary fails closed on identification claims", {
  fixture <- iv_weak_fixture()
  out <- appendix_iv_weak_inference(
    fixture$dynamics, fixture$exclusion, fixture$robustness, fixture$alternative
  )
  csv <- attr(out, "csv_data")

  expect_setequal(csv$row_id, c("long_2022__change", "long_2023__change"))
  expect_equal(csv$effective_f, c(.69, .41))
  expect_equal(
    csv$minimum_gamma_share_of_reduced_form_for_zero_95,
    c(.417, .199)
  )
  expect_true(all(csv$ar_disconnected))
  expect_false(any(csv$ar_sign_identified))

  strong <- fixture$robustness
  strong$family_summary$n_strong_first_stage[[1L]] <- 1L
  expect_error(
    appendix_iv_weak_inference(
      fixture$dynamics, fixture$exclusion, strong, fixture$alternative
    ),
    "every registered robustness family to remain weak",
    fixed = TRUE
  )

  invalid_fas <- fixture$alternative
  invalid_fas$falsification_adaptive_summary$fas_contains_zero[[1L]] <- FALSE
  expect_error(
    appendix_iv_weak_inference(
      fixture$dynamics, fixture$exclusion, fixture$robustness, invalid_fas
    ),
    "state-FE rich-vector FAS results",
    fixed = TRUE
  )
})


test_that("weak-IV appendix table renders through modelsummary", {
  skip_if_not_installed("modelsummary")
  skip_if_not_installed("kableExtra")

  fixture <- iv_weak_fixture()
  table <- appendix_iv_weak_inference(
    fixture$dynamics, fixture$exclusion, fixture$robustness, fixture$alternative
  )

  tex <- paste(
    as.character(appendix_iv_weak_inference_modelsummary_table(
      table, "appendix_iv_weak_inference"
    )),
    collapse = "\n"
  )

  expect_match(tex, "EMI exposure", fixed = TRUE)
  expect_match(tex, "MOP effective F", fixed = TRUE)
  expect_match(tex, "41.7\\%", fixed = TRUE)
  expect_false(grepl("41.7%", tex, fixed = TRUE))
})


test_that("retained validation figures enforce registered common support", {
  welfare <- expand.grid(
    district_2001 = c("d1", "d2", "d3"),
    round_id = c("hces_2022_23", "hces_2023_24"),
    outcome_id = c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce"),
    stringsAsFactors = FALSE
  )
  welfare$estimate <- seq_len(nrow(welfare))
  welfare$preferred_eligible <- TRUE
  consistency <- hces_cross_round_consistency_data(welfare)
  expect_true(all(table(consistency$outcome_id) == 3L))

  panel <- data.frame(
    state_code_2001 = rep(c("01", "02"), each = 2),
    dise_emi_enrollment_share_total_0708 = c(10, 20, 30, 40),
    emi_share_enrolled_0708 = c(12, 18, 29, 43),
    stringsAsFactors = FALSE
  )
  validation <- data.frame(
    comparison = "enrolled_total_denominator", n = 4L, status = "estimated",
    stringsAsFactors = FALSE
  )
  agreement <- appendix_nss_dise_agreement_data(panel, validation)
  expect_equal(nrow(agreement), as.integer(validation$n[[1L]]))
  state_dise_means <- tapply(agreement$dise_residual, agreement$state, mean)
  state_nss_means <- tapply(agreement$nss_residual, agreement$state, mean)
  expect_true(all(abs(state_dise_means) < 1e-12))
  expect_true(all(abs(state_nss_means) < 1e-12))
})
