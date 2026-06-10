test_that("selection joins use household keys rather than many-to-many HHID joins", {
  block4 <- data.frame(
    STATE = c("01", "02"),
    FSU_SL_NO = c("10001", "20001"),
    STRATUM = c("01", "01"),
    SUB_STRATUM_NO = c("01", "01"),
    HHID = c("000000001", "000000001"),
    RELATION_TO_HEAD = c(1, 1),
    SEX = c(1, 1),
    EDUCATION_LEVEL = c("08", "05"),
    stringsAsFactors = FALSE
  )

  proxy <- build_father_education_proxy(block4)

  expect_equal(nrow(proxy), 2L)
  expect_equal(length(unique(proxy$.legacy_household_key)), 2L)
})

test_that("2007 household measures use weighted population rather than sample counts", {
  households <- data.frame(
    district_code = c("01001", "01001"),
    HHID = c("h1", "h2"),
    weight = c(10, 20),
    HH_SIZE = c(3, 4),
    TOTAL = c(300, 1000),
    stringsAsFactors = FALSE
  )

  out <- compute_education_household_measures_2007(households)

  expect_equal(out$npeople_0708, 110)
  expect_equal(out$nhouses_0708, 30)
  expect_equal(out$consumption_2007, weighted.mean(c(100, 250), c(10, 20)))
  expect_gt(out$gini_consumption_2007, 0)
})

test_that("2007 EMIE treats legacy Block 5 medium code 02 as English-medium", {
  block5 <- data.frame(
    district_code = c("01001", "01001", "01001"),
    MEDIUM_INSTRUCTION = c("02", "01", "02"),
    AGE = c(10, 11, 20),
    weight = c(1, 3, 100),
    stringsAsFactors = FALSE
  )

  out <- compute_emie_2007(block5)

  expect_equal(out$emie_2007, 25)
})

test_that("2017 measures use household-weighted per-capita consumption and population", {
  block3 <- data.frame(
    NSS_Region = c("01", "01"),
    District = c("001", "001"),
    HH_Con_exp_rs = c(300, 1000),
    Household_size = c(3, 4),
    MULT_Combined = c(10, 20),
    HHID = c("h1", "h2"),
    stringsAsFactors = FALSE
  )

  out <- build_2017_measures(list(nss1718edu_block3 = block3), list())

  expect_equal(out$npeople_1718, 110)
  expect_equal(out$nhouses_1718, 30)
  expect_equal(out$consumption_2017, weighted.mean(c(100, 250), c(10, 20)))
})

test_that("first-stage table rejects numeric coefficient terms in final mode", {
  malformed <- data.frame(
    model = "consumption",
    term = c("1", "2"),
    estimate = c(1, 2),
    std.error = c(0.1, 0.2),
    p.value = c(0.01, 0.02),
    partial_f = c(39.2, 39.2),
    partial_p = c(0.001, 0.001),
    status = "estimated",
    stringsAsFactors = FALSE
  )

  expect_error(
    make_first_stage_table(malformed, list(mode = "final")),
    "numeric row positions",
    fixed = TRUE
  )
})

test_that("first-stage table requires and formats the legacy instrument row", {
  fs <- data.frame(
    model = "consumption",
    term = c("wavg_ling_degrees", "(Intercept)"),
    estimate = c(2.945, 1.234),
    std.error = c(0.949, 0.500),
    p.value = c(0.004, 0.1),
    partial_f = c(39.20, 39.20),
    partial_p = c(0.0001, 0.0001),
    status = "estimated",
    stringsAsFactors = FALSE
  )

  out <- make_first_stage_table(fs, list(mode = "final"))

  expect_equal(out$Term, c("Linguistic distance", "Constant", "Instrument's F-Statistic"))
  expect_equal(out$Estimate[1], "2.945**")
  expect_equal(out$`Std. Error`[1], "(0.949)")
  expect_equal(out$Estimate[3], "39.20***")
})

test_that("final district panel validation records old wrong-N and wrong-scale failures without breaking targets", {
  bad <- data.frame(
    EMIE = c(4, 5),
    npeople_0708 = c(500, 600),
    consumption_0708 = c(1000, 1001),
    dependency_ratio = c(97, 96),
    pct_fem_head = c(12, 11)
  )

  expect_silent(checked <- validate_legacy_district_panel(bad, list(mode = "final")))
  expect_true(any(grepl("454 rows", attr(checked, "legacy_panel_validation_failures"), fixed = TRUE)))

  expect_silent(checked_explicit <- validate_legacy_district_panel(bad, list(mode = "final", warn_legacy_panel_validation = TRUE)))
  expect_true(any(grepl("454 rows", attr(checked_explicit, "legacy_panel_validation_failures"), fixed = TRUE)))

  expect_error(
    validate_legacy_district_panel(bad, list(mode = "final", strict_legacy_panel_validation = TRUE)),
    "454 rows",
    fixed = TRUE
  )
})


test_that("final figure specs degrade to status outputs instead of aborting when map inputs are incomplete", {
  panel <- data.frame(
    emie_2007 = c(10, 20),
    pucca_share_2007 = c(30, 40),
    head_secondary_plus_2007 = c(5, 6),
    region = c("North", "South")
  )
  figs <- make_figures(panel, character(), list(mode = "final"))
  expect_true("map_consumption_growth" %in% names(figs))
  expect_identical(figs$map_consumption_growth$kind, "status")
  expect_true(any(grepl("wavg_ling_degrees", attr(figs, "legacy_map_input_failures"), fixed = TRUE)))
})


test_that("final table generation records incomplete first-stage diagnostics without aborting", {
  first_stage <- data.frame(
    model = "consumption",
    term = NA_character_,
    estimate = NA_real_,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    partial_f = NA_real_,
    partial_p = NA_real_,
    status = "out_of_active_pipeline",
    reason = "Missing first-stage variables: consumption_pct_change, wavg_ling_degrees",
    stringsAsFactors = FALSE
  )

  fs_table <- make_first_stage_table(first_stage, list(mode = "final"))
  expect_identical(fs_table$status, "out_of_active_pipeline")
  expect_match(fs_table$reason, "wavg_ling_degrees", fixed = TRUE)
  expect_true(any(grepl("wavg_ling_degrees", attr(fs_table, "legacy_table_input_failures"), fixed = TRUE)))

  tables <- make_tables(
    selection_data = data.frame(AGE = 10, HH_SIZE = 4),
    ame_results = data.frame(term = "AGE", estimate = 0, std.error = 1, p.value = 1, status = "estimated"),
    district_panel = data.frame(EMIE = 10, consumption_0708 = 100, gini_cons_0708 = 0.3),
    iv_models = list(consumption = list(status = "out_of_active_pipeline", reason = "Missing variables: consumption_pct_change")),
    first_stage_tests = first_stage,
    cfg = list(mode = "final")
  )
  expect_true(any(grepl("first-stage regression", attr(tables, "legacy_table_input_failures"), fixed = TRUE)))
  expect_true(any(grepl("wavg_ling_degrees", attr(tables, "legacy_table_input_failures"), fixed = TRUE)))
})
