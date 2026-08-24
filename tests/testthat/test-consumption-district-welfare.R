test_that("consumption survey design uses person weights and nested NSS identifiers", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h2", "h3", "h4"),
    source_state_code = c("01", "01", "02", "02"),
    sector = c("Rural", "Rural", "1", "1"), subround = "1",
    fsu = c("1", "2", "1", "2"), stratum = "1", sub_stratum = "1",
    household_size = c(1, 3, 1, 1), target_unit_2001 = c("a", "a", "b", "b"),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = c(1, 3, 1, 1), real_mpce = c(100, 200, 300, 500),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_equal(length(unique(rows$.design_stratum)), 2L)
  expect_equal(length(unique(rows$.design_psu)), 4L)

  out <- estimate_consumption_district_mean(x)
  a <- out[out$district_2001 == "a", ]
  expect_equal(a$estimate, 175, tolerance = 1e-8)
  expect_equal(a$n_households, 2L)
  expect_equal(a$n_fsu, 2L)
  expect_equal(a$kish_effective_n, 16 / 10, tolerance = 1e-8)
  expect_equal(a$status, "estimated")
})

test_that("district support uses allocation-adjusted person weights", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h1", "h2"),
    source_state_code = "01", sector = "1", subround = "1",
    fsu = c("1", "1", "2"), stratum = "1", sub_stratum = "1",
    household_size = c(2, 2, 1), target_unit_2001 = c("a", "b", "a"),
    lineage_status = "resolved_reviewed_consensus", lineage_weight = c(0.5, 0.5, 1),
    lineage_person_weight = c(2, 2, 1), real_mpce = c(100, 100, 200),
    stringsAsFactors = FALSE
  )
  out <- consumption_district_support(x)
  a <- out[out$district_2001 == "a", ]
  expect_equal(a$n_households, 2L)
  expect_equal(a$n_fsu, 2L)
  expect_equal(a$kish_effective_n, 9 / 5, tolerance = 1e-8)
  expect_equal(a$n_sample_person_equiv, 2)
  expect_equal(a$sum_person_weight, 3)
})

test_that("unresolved households never enter the survey design", {
  x <- data.frame(
    survey_id = "wave", household_id = c("resolved", "unresolved"),
    source_state_code = "01", sector = "1", subround = "1",
    fsu = c("1", "2"), stratum = "1", sub_stratum = "1",
    household_size = 1, target_unit_2001 = c("a", NA),
    lineage_status = c("resolved_exact_2001", "unresolved_no_stable_lineage"), lineage_weight = c(1, NA),
    lineage_person_weight = c(1, NA), real_mpce = c(100, 999),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_equal(rows$household_id, "resolved")
  expect_equal(rows$real_mpce, 100)
})

test_that("Round 66 urban blank sub-strata represent no sub-stratification", {
  x <- data.frame(
    survey_id = "nss_2009_10_type1", household_id = c("u1", "u2"),
    source_state_code = "07", sector = "Urban", subround = "1",
    fsu = c("1", "2"), stratum = "10", sub_stratum = c("", NA),
    household_size = 1, target_unit_2001 = "delhi",
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = c(100, 200),
    stringsAsFactors = FALSE
  )
  rows <- consumption_design_rows(x)
  expect_identical(unique(rows$.design_sub_stratum), "__none__")
  expect_equal(length(unique(rows$.design_stratum)), 1L)
})

test_that("rural blank sub-strata remain invalid design data", {
  x <- data.frame(
    survey_id = "nss_2009_10_type1", household_id = "r1",
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = "1", stratum = "1", sub_stratum = "", household_size = 1,
    target_unit_2001 = "a", lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = 100, stringsAsFactors = FALSE
  )
  expect_error(consumption_design_rows(x), "sub_stratum=1")
})

test_that("district means intentionally handle lonely-PSU survey warnings", {
  x <- data.frame(
    survey_id = "wave", household_id = c("h1", "h2"),
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = c("1", "2"), stratum = c("1", "2"), sub_stratum = "1",
    household_size = 1, target_unit_2001 = c("a", "a"),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = c(100, 200), stringsAsFactors = FALSE
  )
  expect_warning(out <- estimate_consumption_district_mean(x), NA)
  expect_equal(out$estimate, 150)
})

test_that("welfare registry dispatches means and quantiles without dropping thin cells", {
  registry <- data.frame(
    outcome_id = c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce"),
    estimand = c("survey_mean", "survey_mean", "survey_quantile"),
    transform = c("identity", "log", "identity"), quantile = c(NA, NA, 0.5),
    role = c("primary", "robustness", "robustness"), min_households = 10,
    min_fsu = 2, min_kish_effective_n = 1, max_relative_se = c(0.50, NA, 0.50),
    stringsAsFactors = FALSE
  )
  x <- data.frame(
    survey_id = "wave", household_id = paste0("h", 1:8), source_state_code = "01",
    sector = "Rural", subround = "1", fsu = as.character(1:8), stratum = "1",
    sub_stratum = "1", household_size = 1, target_unit_2001 = "a",
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1,
    real_mpce = c(100, 100, 150, 150, 200, 200, 250, 250), stringsAsFactors = FALSE
  )
  out <- estimate_consumption_district_welfare(x, registry)
  expect_setequal(out$outcome_id, registry$outcome_id)
  expect_equal(out$estimate[out$outcome_id == "real_mean_mpce"], 175, tolerance = 1e-8)
  expect_equal(
    out$estimate[out$outcome_id == "mean_log_real_mpce"],
    mean(log(c(100, 100, 150, 150, 200, 200, 250, 250))),
    tolerance = 1e-8
  )
  rows <- consumption_design_rows(x)
  design <- consumption_survey_design_from_rows(rows)
  direct_median <- with_consumption_survey_adjustment(survey::svyquantile(
    ~real_mpce, design, quantiles = 0.5, ci = TRUE, na.rm = TRUE
  ))
  expect_equal(
    out$estimate[out$outcome_id == "weighted_median_real_mpce"],
    unname(stats::coef(direct_median))[[1L]],
    tolerance = 1e-8
  )
  expect_true(all(!out$sample_support_ok))
  expect_true(all(!out$preferred_eligible))
  expect_true(all(out$status[out$outcome_id != "weighted_median_real_mpce"] == "estimated"))
  expect_true(all(grepl("thin_household_sample", out$support_reason, fixed = TRUE)))
  expect_true(is.na(out$cv[out$outcome_id == "mean_log_real_mpce"]))
})

test_that("consumption welfare registry validates outcome contracts", {
  path <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    outcome_id = c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce"),
    estimand = c("survey_mean", "survey_mean", "survey_quantile"),
    transform = c("identity", "log", "identity"), quantile = c(NA, NA, 0.5),
    role = c("primary", "robustness", "robustness"),
    min_households = 50, min_fsu = 2, min_kish_effective_n = 20,
    max_relative_se = c(0.2, NA, 0.2), stringsAsFactors = FALSE
  ), path, row.names = FALSE, na = "")
  out <- read_consumption_welfare_outcomes(path)
  expect_equal(out$outcome_id, c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce"))
  expect_true(is.na(out$max_relative_se[[2L]]))
  expect_equal(out$quantile[[3L]], 0.5)

  bad_quantile <- out
  bad_quantile$quantile[[3L]] <- 1
  write.csv(bad_quantile, path, row.names = FALSE, na = "")
  expect_error(read_consumption_welfare_outcomes(path), "invalid quantile declarations")

  bad <- out
  bad$outcome_id[[2L]] <- bad$outcome_id[[1L]]
  write.csv(bad, path, row.names = FALSE, na = "")
  expect_error(read_consumption_welfare_outcomes(path), "unique outcome_id")
})
