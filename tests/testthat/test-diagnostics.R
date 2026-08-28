test_that("district tracker diagnostics summarize source row counts", {
  raw <- list(a = data.frame(x = 1:2), b = data.frame(x = 3))

  out <- diagnose_district_tracker_sources(raw, data.frame(), list())

  expect_equal(out$n_rows, c(2L, 1L))
})

test_that("district and fuzzy matching diagnostics return table counts", {
  district_panel <- data.frame(id = 1:2)
  join_map <- data.frame(id = 1:3)

  district_out <- diagnose_district_matching(district_panel, join_map, list())
  fuzzy_out <- diagnose_fuzzy_matching(data.frame(id = 1), join_map, list())

  expect_equal(district_out$n_panel_rows, 2L)
  expect_equal(district_out$n_join_rows, 3L)
  expect_equal(fuzzy_out$n_tracker_rows, 1L)
})

test_that("AME benchmark diagnostic is skipped unless enabled", {
  out <- diagnose_ame_benchmark(list(), data.frame(x = 1), list(run_diagnostics = list(ame_benchmark = FALSE)))

  expect_equal(out$status, "skipped")
})

test_that("rendered PDF text checks use the system extractor contract", {
  skipped <- c("paper/report.pdf", "docs/district-matching.pdf")

  expect_false(pdf_text_extractor_available(""))
  expect_true(is.na(extract_pdf_text(tempfile(fileext = ".pdf"), command = "")))
  expect_false(should_fail_pdf_text_skip(skipped, extractor_available = FALSE))
  expect_true(should_fail_pdf_text_skip(skipped, extractor_available = TRUE))
  expect_match(pdf_text_skip_message(skipped), "Poppler/pdftotext", fixed = TRUE)
  expect_match(pdf_text_failure_message(skipped), "pdftotext is available", fixed = TRUE)
})

test_that("Moran diagnostics compute legacy asymptotic p-values from spatial weights", {
  testthat::skip_if_not_installed("spdep")

  nb <- spdep::cell2nb(2, 2, type = "rook")
  weights <- list(
    status = "constructed",
    contiguity = "rook",
    style = "W",
    matrix_style = "B",
    zero_policy = TRUE,
    row_index = 1:4,
    nb = nb,
    W = spdep::nb2mat(nb, style = "B", zero.policy = TRUE),
    listw = spdep::nb2listw(nb, style = "W", zero.policy = TRUE),
    neighbor_counts = spdep::card(nb),
    n = 4L,
    n_islands = 0L,
    mean_neighbors = mean(spdep::card(nb)),
    warnings = character()
  )
  class(weights) <- c("emi_spatial_weights", class(weights))

  out <- compute_moran_tests(c(1, 2, 3, 4), weights, legacy_name = "m_cons", estimand = "consumption_growth", variable = "consumption_pct_change", source = "outcome")

  expect_equal(out$status, "estimated")
  expect_equal(out$legacy_name, "m_cons")
  expect_equal(out$contiguity %||% "rook", "rook")
  expect_true(is.finite(out$p.value))
})


test_that("public spatial autocorrelation diagnostics return tracked files", {
  dir <- tempfile("spatial-public-diagnostics-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  diagnostics <- data.frame(
    legacy_name = "m_cons",
    status = "estimated",
    p.value = 0.01,
    stringsAsFactors = FALSE
  )

  paths <- save_spatial_autocorrelation_diagnostics(diagnostics, dir = dir)

  expect_type(paths, "character")
  expect_setequal(basename(paths), c("spatial_moran_tests.csv", "spatial_moran_mc_reference.csv"))
  expect_true(all(file.exists(paths)))
})

test_that("report values read Moran p-values from spatial autocorrelation diagnostics", {
  diag <- data.frame(
    legacy_name = c("m_cons_resid", "m_cons"),
    estimand = c("consumption_iv_residual", "consumption_growth"),
    status = "estimated",
    p.value = c(0.01234, 0.98765),
    stringsAsFactors = FALSE
  )

  values <- build_report_values(data.frame(), data.frame(), list(), data.frame(), data.frame(), diag, list())

  expect_equal(values[["moran_iv_residual_p"]], signif(0.01234, 3))
  expect_equal(values[["moran_consumption_growth_p"]], signif(0.98765, 3))
})

test_that("report values expose MOP effective F from canonical first-stage diagnostics", {
  first_stage <- data.frame(
    effective_f = 0.81234,
    effective_f_critical_value = 10.5,
    effective_f_p_value = 0.42,
    stringsAsFactors = FALSE
  )
  values <- build_report_values(
    data.frame(), first_stage, list(), data.frame(), data.frame(), NULL, list()
  )

  expect_equal(values$effective_f, 0.81234)
  expect_equal(values$effective_f_report, 0.81)
  expect_equal(values$effective_f_critical_value, 10.5)
  expect_equal(values$effective_f_critical_value_report, 10.5)
  expect_equal(values$effective_f_p_value, 0.42)
})

test_that("missingness diagnostics preserve legacy diagnostic components", {
  df <- data.frame(
    enrolled = c("Yes", "No", "Yes", "Yes"),
    AGE = c(10, 11, 12, 13),
    HH_SIZE = c(4, 5, 4, 6),
    SEX = c("Female", "Male", "Female", "Male"),
    SECTOR = c("Urban", "Rural", "Urban", "Rural"),
    RELIGION = c("Hindu", "Muslim", "Hindu", "Hindu"),
    SOCIAL_GROUP = c("Scheduled Tribe", "Other", "Other Backward Class", "Other"),
    state_0708 = c("Rajasthan", "Rajasthan", "Bihar", "Bihar"),
    region_0708 = c("Southern", "Southern", "North", "North"),
    dmean_num_ENROLLMENT_COST = c(NA, 10, NA, 12),
    DIST_FROM_NEAREST_PRIMARY_CLASS = c(1, NA, 2, 3),
    father_educ = c(NA, 1, 1, NA),
    TUTION_FEE = c(NA, 1, 2, 3)
  )

  out <- diagnose_missingness(df, list())

  expect_s3_class(out, "emi_missingness_diagnostics")
  expect_true(all(c("missing_counts", "regional_cost", "corr_all", "logit_summary", "notes") %in% names(out)))
  expect_true("Total probit-model with NA" %in% out$missing_counts$missing_var)
  expect_false("Total probit-relevant with NA" %in% out$missing_counts$missing_var)
})

test_that("missingness regional diagnostics fall back to state-only rankings", {
  df <- data.frame(
    enrolled = c("Yes", "No", "Yes"),
    AGE = c(10, 11, 12),
    SEX = c("Female", "Male", "Female"),
    HH_SIZE = c(4, 5, 4),
    state_0708 = c("A", "A", "B"),
    dmean_num_ENROLLMENT_COST = c(NA, 1, NA),
    DIST_FROM_NEAREST_PRIMARY_CLASS = c(1, NA, 2),
    father_educ = c(NA, 1, NA)
  )

  out <- diagnose_missingness(df, list())

  expect_true(nrow(out$regional_cost) > 0L)
  expect_equal(unique(out$regional_cost$region_diagnostic_level), "state_only_fallback")
})

test_that("tracker diagnostics summarize current source changes", {
  tracker <- data.frame(
    state_01 = c("Andhra Pradesh", "Jammu & Kashmir"),
    district_01 = c("Same", "Old Name"),
    state_07 = c("Andhra Pradesh", "Jammu & Kashmir"),
    district_07 = c("Same", "Old Name"),
    state_08 = c("Andhra Pradesh", "Jammu & Kashmir"),
    district_08 = c("Same", "New Name"),
    state_20 = c("Telangana", "Ladakh"),
    district_20 = c("Same", "New Name")
  )
  raw <- list(source = data.frame(x = 1:2))

  out <- diagnose_district_tracker_sources(raw, tracker, list())

  expect_s3_class(out, "emi_tracker_source_diagnostics")
  expect_equal(out$n_rows, 2L)
  expect_true(nrow(attr(out, "state_changes")) >= 1L)
  expect_true(nrow(attr(out, "inperiod_district_changes")) >= 1L)
  expect_true(nrow(find_same_name_districts(data.frame(
    state_20 = c("A", "B", "A"),
    district_20 = c("Same", "same", "Different")
  ))) >= 1L)
})

test_that("district matching diagnostics separate source-key inventory from true unmatched rows", {
  join_map <- data.frame(state_std = "A", district_std = "B", source_year = 2007, match_status = "source_key_unmatched")
  attr(join_map, "unmatched_rows") <- join_map
  panel <- data.frame(state_20 = "A", district_20 = "B")

  out <- diagnose_district_matching(panel, join_map, list())

  expect_s3_class(out, "emi_district_matching_diagnostics")
  expect_equal(out$n_unmatched_rows, 0L)
  expect_equal(out$n_source_key_inventory_rows, 1L)
  expect_true(nrow(attr(out, "source_key_inventory")) >= 1L)
  expect_true(nrow(attr(out, "key_role_counts")) >= 1L)
  expect_true(nrow(attr(out, "all_rows_search")) >= 1L)
})

test_that("fuzzy diagnostics expose configured methods and candidate pairs", {
  testthat::skip_if_not_installed("stringdist")
  out <- diagnose_fuzzy_matching(data.frame(id = 1), data.frame(match_status = "harmonization_crosswalk_row"), list())

  expect_s3_class(out, "emi_fuzzy_matching_diagnostics")
  methods <- attr(out, "legacy_methods")
  expect_s3_class(methods, "data.frame")
  expect_true(all(c("method", "threshold") %in% names(methods)))
  expect_gt(nrow(methods), 0L)
  expect_false(anyDuplicated(methods$method) > 0L)
  expect_true(nrow(attr(out, "troublesome_pairs")) > 0L)
})

test_that("district matching diagnostics preserve matcher attributes before data-frame coercion", {
  join_map <- data.frame(state_20 = c("A", "B"), district_20 = c("One", "Two"), match_status = "source_key_unmatched")
  attr(join_map, "unmatched_rows") <- join_map[0, , drop = FALSE]
  panel <- data.frame(state_20 = c("A", "B"), district_20 = c("One", "Two"))

  out <- diagnose_district_matching(panel, join_map, list())

  expect_equal(out$n_unmatched_rows, 0L)
  expect_equal(out$n_join_unmatched_by_key, 0L)
  expect_true("key_comparison" %in% names(attributes(out)))
  expect_true("key_role" %in% names(attr(out, "key_comparison")))
})

test_that("tracker diagnostics expose current change tables and optional historical context", {
  tracker <- data.frame(
    state_05 = c("A", "A"), district_05 = c("Old", "Same"),
    state_06 = c("A", "A"), district_06 = c("New", "Same"),
    state_19 = c("Old State", "B"), district_19 = c("X", "Y"),
    state_20 = c("New State", "B"), district_20 = c("X", "Y")
  )
  out <- diagnose_district_tracker_sources(list(source = data.frame(x = 1)), tracker, list())

  expect_true(nrow(attr(out, "state_changes")) >= 1L)
  expect_true(nrow(attr(out, "state_change_events")) >= 1L)
  expect_true(nrow(attr(out, "inperiod_district_changes")) >= 1L)
  for (name in c(
    "legacy_reference",
    "legacy_expected_state_changes",
    "legacy_expected_inperiod_district_changes",
    "legacy_expected_same_name_districts"
  )) {
    expect_s3_class(attr(out, name), "data.frame")
  }
})

test_that("fuzzy benchmarking uses active tracker candidate pairs beyond toy examples", {
  testthat::skip_if_not_installed("stringdist")
  tracker <- data.frame(
    district_01 = c("Old Name", "Stable"),
    district_07 = c("New Name", "Stable"),
    district_17 = c("New Name", "Stable"),
    district_20 = c("Newest Name", "Stable")
  )

  pairs <- fuzzy_candidate_pairs(tracker, data.frame())
  sens <- summarize_threshold_sensitivity(pairs)

  expect_true(any(pairs$pair_source == "tracker_2001_to_2007"))
  expect_true(any(pairs$pair_source == "tracker_2017_to_2020"))
  expect_true(any(sens$pair_source == "tracker_2001_to_2007"))
  expect_true("candidate_pair_coverage" %in% names(attributes(diagnose_fuzzy_matching(tracker, data.frame(), list()))))
  expect_true(nrow(fuzzy_tuning_reference()) >= 4L)
})



test_that("fuzzy benchmarking expands fallback source-key inventory into active candidates", {
  tracker <- data.frame(
    state_07 = c("A", "A", "B"),
    district_07 = c("One", "Two", "Three"),
    state_20 = c("A", "A", "B"),
    district_20 = c("One New", "Two", "Three")
  )
  join_map <- data.frame(
    state_std = "A",
    district_std = "Onee",
    source_year = 2007,
    match_status = "source_key_unmatched"
  )

  pairs <- fuzzy_candidate_pairs(tracker, join_map)

  expect_true(any(grepl("active_source_key_inventory", pairs$pair_source)))
  expect_true(any(pairs$str1 == "Onee"))
})

test_that("spatial weights diagnostics compute reference deltas without fixing a historical result", {
  comp <- data.frame(contiguity = c("rook", "queen"), mean_neighbors = c(4, 4.1), stringsAsFactors = FALSE)
  out <- add_spatial_weight_reference(comp)

  expect_true("legacy_mean_neighbors" %in% names(out))
  expect_true("mean_neighbor_delta_from_legacy" %in% names(out))
})

test_that("instrument exploration diagnostics render target-backed dotplot artifacts", {
  panel <- data.frame(
    district_code_0708 = c(101L, 102L, 201L),
    state_07 = c("A", "A", "B"),
    district_07 = c("One", "Two", "Three"),
    emi_exposure_all_children_0708 = c(0, 10, 80),
    ling_distance_nonzero_mean = c(0, 1, 5),
    region = c("Northern", "Northern", "Southern")
  )

  out <- diagnose_instrument_exploration(panel, list())

  expect_true(is.list(out))
  expect_equal(nrow(out$dotplot_data), 3L)
  expect_true(all(c("district_order", "district_code", "emi_exposure_all_children_0708", "ling_distance_nonzero_mean", "state_prefix") %in% names(out$dotplot_data)))
  expect_s3_class(out$legacy_notes, "data.frame")
  expect_true(all(c("diagnostic", "legacy_note") %in% names(out$legacy_notes)))
  expect_gt(nrow(out$legacy_notes), 0L)
})

test_that("missingness diagnostics save logit plot outputs", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = normalizePath(file.path("..", ".."), mustWork = TRUE))
  src_path <- file.path(root, "R", "selection", "diagnose_missingness.R")
  src <- paste(readLines(src_path, warn = FALSE), collapse = "\n")

  expect_match(src, "missingness_logit_pseudo_r2.png", fixed = TRUE)
  expect_match(src, "save_missingness_logit_plot", fixed = TRUE)
})

test_that("missingness diagnostics distinguish probit-model and enrolled-only missingness", {
  df <- data.frame(
    enrolled = c("yes", "no", "yes", "no"),
    AGE = c(10, 11, 12, 13),
    SEX = c("Male", "Female", "Male", "Female"),
    HH_SIZE = c(4, 5, 6, 7),
    RELIGION = c("Hindu", "Muslim", "Hindu", "Muslim"),
    SOCIAL_GROUP = c("Other", "Other", "Scheduled Tribe", "Other"),
    SECTOR = c("Urban", "Rural", "Urban", "Rural"),
    state_0708 = c("Rajasthan", "Rajasthan", "Other", "Other"),
    region_0708 = c("Southern", "Southern", "Other", "Other"),
    DIST_FROM_NEAREST_PRIMARY_CLASS = c(1, NA, 2, 3),
    dmean_num_ENROLLMENT_COST = c(10, 11, NA, 13),
    father_educ = c(1, 2, 3, NA),
    TUTION_FEE = c(NA, NA, 20, NA)
  )
  out <- diagnose_missingness(df, list())

  counts <- out$missing_counts
  expect_true("Total probit-model with NA" %in% counts$missing_var)
  expect_false("Total probit-relevant with NA" %in% counts$missing_var)
  expect_true(nrow(out$case_study) >= 1L)
  expect_true(nrow(out$chi_square) >= 1L)
})

test_that("tracker diagnostics summarize same-name districts by year", {
  same <- data.frame(
    year = c(2001, 2001, 2007),
    district_key = c("a", "b", "a"),
    stringsAsFactors = FALSE
  )
  out <- summarize_same_name_districts_by_year(same)

  expect_true(all(c("year", "n_same_name_districts", "n_same_name_district_names") %in% names(out)))
  expect_equal(out$n_same_name_districts[out$year == 2001], 2L)
})

test_that("public IV-panel diagnostics return file paths for targets", {
  dir <- tempfile("public-iv-panel-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  panel <- data.frame(
    district_panel_id = c("a", "b"),
    state_20 = c("State A", "State B"),
    district_20 = c("District A", "District B"),
    emi_exposure_all_children_0708 = c(1, 2),
    ling_distance_nonzero_mean = c(3, 4),
    npeople_0708 = c(10, 20),
    consumption_0708 = c(100, 200),
    dependency_ratio = c(50, 60),
    .matched_2001 = c(TRUE, TRUE),
    .matched_2007 = c(TRUE, FALSE),
    .matched_2017 = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  paths <- save_public_iv_panel_diagnostics(panel, dir = dir)

  expect_type(paths, "character")
  expect_true(length(paths) >= 5L)
  expect_true(all(file.exists(paths)))
})

test_that("multicollinearity diagnostics report factor-aware GVIF output", {
  skip_if_not_installed("car")
  set.seed(42)
  df <- data.frame(
    y = rnorm(60),
    x = rnorm(60),
    group = factor(rep(c("a", "b", "c"), each = 20))
  )
  model <- stats::lm(y ~ x + group, data = df)

  out <- diagnose_multicollinearity(data.frame(), list(model), list())

  expect_true(all(c("design_matrix", "vif") %in% out$diagnostic))
  group_row <- out[out$diagnostic == "vif" & out$term == "group", , drop = FALSE]
  expect_equal(group_row$df, 2L)
  expect_true(is.finite(group_row$gvif_scaled))
})

test_that("multicollinearity diagnostics cover every supplied IV model", {
  set.seed(44)
  df <- data.frame(y = rnorm(60), x = rnorm(60), z = rnorm(60))
  models <- list(first = stats::lm(y ~ x, data = df), second = stats::lm(y ~ z, data = df))

  out <- diagnose_multicollinearity(data.frame(), models, list())

  expect_setequal(unique(out$model), c("first", "second"))
  expect_equal(sum(out$diagnostic == "design_matrix"), 2L)
})

test_that("IV VIF diagnostics use structural regressors rather than instruments", {
  skip_if_not_installed("car")
  skip_if_not_installed("ivreg")
  set.seed(43)
  df <- data.frame(
    y = rnorm(90),
    x = rnorm(90),
    z = rnorm(90),
    group = factor(rep(c("a", "b", "c"), each = 30))
  )
  model <- ivreg::ivreg(y ~ x + group | z + group, data = df)

  out <- compute_vif_if_applicable(model)

  expect_true(all(out$model_scope == "ivreg_structural_regressors"))
  expect_true("group" %in% out$term)
  expect_false("z" %in% out$term)
  expect_true(all(out$status == "estimated"))
})

test_that("IV VIF diagnostics load the ivreg namespace for cached model objects", {
  skip_if_not_installed("car")
  skip_if_not_installed("ivreg")
  set.seed(430)
  df <- data.frame(
    y = rnorm(90),
    x = rnorm(90),
    w = rnorm(90),
    z = rnorm(90)
  )
  model <- ivreg::ivreg(y ~ x + w | z + w, data = df)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(model, path)
  rm(model)
  try(unloadNamespace("ivreg"), silent = TRUE)

  cached <- readRDS(path)
  out <- compute_vif_if_applicable(cached)

  expect_true("ivreg" %in% loadedNamespaces())
  expect_setequal(out$term, c("x", "w"))
  expect_true(all(out$status == "estimated"))
  expect_true(all(is.finite(out$gvif_scaled)))
})

test_that("Anderson-Rubin inversion preserves disconnected confidence sets", {
  grid <- data.frame(
    beta = -3:3,
    p.value = c(0.20, 0.10, 0.01, 0.01, 0.01, 0.10, 0.20),
    stringsAsFactors = FALSE
  )
  grid$accepted <- grid$p.value >= 0.05

  components <- anderson_rubin_acceptance_components(grid)

  expect_equal(nrow(components), 2L)
  expect_equal(components$lower, c(-3, 2))
  expect_equal(components$upper, c(-2, 3))
  expect_true(components$touches_left_grid_edge[[1]])
  expect_true(components$touches_right_grid_edge[[2]])
  expect_false(any(components$contains_zero))
})

test_that("Anderson-Rubin summaries do not collapse noninterval sets to min-max bounds", {
  set.seed(451)
  n <- 180L
  z <- stats::rnorm(n)
  treatment <- 0.05 * z + stats::rnorm(n)
  outcome <- 0.45 * z + stats::rnorm(n)
  panel <- data.frame(
    y = outcome,
    d = treatment,
    z = z,
    state_code_2001 = rep(sprintf("%02d", 1:18), each = 10),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    specification_id = "ar_test",
    outcome = "y",
    treatment = "d",
    fixed_effect = "none",
    cluster = "state_code_2001",
    stringsAsFactors = FALSE
  )
  spec$controls <- I(list(character()))
  spec$included_language_controls <- I(list(character()))
  spec$excluded_instruments <- I(list("z"))

  out <- estimate_anderson_rubin_spec(panel, spec, points = 101L)
  summary <- out$summary

  expect_equal(summary$status, "estimated")
  expect_identical(
    summary$ar_95_contains_zero[[1]],
    summary$anderson_rubin_p_beta0[[1]] >= 0.05
  )
  if (summary$ar_95_disconnected[[1]] ||
      summary$ar_95_left_truncated[[1]] ||
      summary$ar_95_right_truncated[[1]]) {
    expect_true(is.na(summary$ar_95_lower[[1]]))
    expect_true(is.na(summary$ar_95_upper[[1]]))
  }
  expect_true("acceptance_component" %in% names(out$grid))
})

test_that("preferred public Anderson-Rubin diagnostic is registry-backed", {
  spec <- preferred_iv_diagnostic_specification()
  expect_equal(nrow(spec), 1L)
  expect_identical(spec$adjustment_id[[1]], "state_main")
  expect_identical(spec$construction_id[[1]], "nonzero_mean")
  expect_identical(spec$outcome[[1]], "real_log_consumption_change")
  expect_identical(spec$treatment[[1]], preferred_iv_variables()$treatment)
  expect_identical(spec$excluded_instruments[[1]], preferred_iv_variables()$instrument)
})

test_that("condition number is invariant to regressor units and excludes the intercept", {
  set.seed(45)
  x1 <- stats::rnorm(100)
  x2 <- 0.8 * x1 + stats::rnorm(100, sd = 0.4)
  X <- cbind(`(Intercept)` = 1, x1 = x1, x2 = x2)
  X_rescaled <- X
  X_rescaled[, "x1"] <- 1000000 * X_rescaled[, "x1"]

  expect_equal(
    standardized_design_condition_number(X),
    standardized_design_condition_number(X_rescaled),
    tolerance = 1e-10
  )
  expect_lt(standardized_design_condition_number(X), kappa(X_rescaled, exact = TRUE))
})

test_that("multicollinearity diagnostics save one tracked public CSV", {
  path <- tempfile(fileext = ".csv")
  diagnostics <- data.frame(diagnostic = "design_matrix", status = "estimated")

  written <- save_multicollinearity_diagnostics(diagnostics, path)

  expect_identical(written, normalizePath(path, mustWork = TRUE))
  expect_true(file.exists(written))
})

test_that("spatial island diagnostics use spdep cardinalities", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  square <- function(xmin, ymin) sf::st_polygon(list(rbind(
    c(xmin, ymin), c(xmin + 1, ymin), c(xmin + 1, ymin + 1),
    c(xmin, ymin + 1), c(xmin, ymin)
  )))
  panel <- sf::st_sf(
    district_panel_id = c("a", "b", "island"),
    geometry = sf::st_sfc(square(0, 0), square(1, 0), square(10, 10), crs = 3857)
  )

  weights <- build_spatial_weights_for_rows(panel, 1:3, queen = FALSE)
  connectivity <- summarize_spatial_connectivity(weights)

  expect_equal(weights$neighbor_counts, c(1L, 1L, 0L))
  expect_equal(weights$n_islands, 1L)
  islands <- summarize_islands(weights)
  expect_equal(islands$row_index, 3L)
  expect_equal(islands$district_panel_id, "island")
  expect_true(connectivity$snap_investigation_needed)
  expect_gt(weights$n_subgraphs, 1L)
})


test_that("missingness logits keep fit issues inside diagnostic output", {
  df <- data.frame(
    missing_input = c(NA, NA, 1, 1),
    predictor = c(0, 0, 1, 1)
  )

  expect_silent(
    out <- check_missing_logit_parallel(
      df,
      miss_vars = "missing_input",
      covars = "predictor"
    )
  )

  expect_true(nrow(out) > 0L)
  expect_true(all(out$status %in% c("estimated", "estimated_with_warning")))
  expect_identical(
    !is.na(out$reason) & nzchar(out$reason),
    out$status == "estimated_with_warning"
  )
})

test_that("binomial fit issues detect boundary probabilities deterministically", {
  fit <- structure(
    list(fitted.values = c(0, 0.5, 1), converged = TRUE),
    class = "glm"
  )

  issues <- binomial_fit_issues(fit)
  expect_match(issues, "near 0 or 1", fixed = TRUE)

  issues <- binomial_fit_issues(fit, "captured warning")
  expect_setequal(
    issues,
    c("captured warning", "fitted probabilities are numerically near 0 or 1")
  )
})

test_that("first-stage absorption registry is ordered and exhausts main Census controls", {
  registry <- first_stage_absorption_registry()

  expect_identical(registry$specification_id[1:13], c(
    "instrument_only", "region_fe", "state_fe", "census_controls",
    "region_fe_census_controls", "state_fe_census_controls", "expanded_controls",
    "region_fe_expanded_controls", "state_fe_expanded_controls",
    "region_fe_main_without_human_capital", "state_fe_main_without_human_capital",
    "region_fe_expanded_without_human_capital", "state_fe_expanded_without_human_capital"
  ))
  expect_identical(registry$sequence, seq_len(nrow(registry)))
  expect_setequal(unlist(first_stage_control_blocks(), use.names = FALSE), census_2001_absorption_controls())
  expect_identical(
    unlist(registry$controls[registry$specification_id == "state_fe_expanded_controls"][[1]], use.names = FALSE),
    census_2001_absorption_controls()
  )
  expect_true(all(vapply(registry$controls[14:nrow(registry)], function(x) {
    identical(x, order_first_stage_controls(x))
  }, logical(1))))
  expect_identical(
    first_stage_included_control_blocks(census_2001_main_controls()),
    names(first_stage_control_blocks())
  )
  expect_identical(
    first_stage_included_control_blocks(census_2001_absorption_controls()),
    names(first_stage_control_blocks())
  )
})

test_that("first-stage residual metrics reproduce nested-model partial R-squared", {
  set.seed(41)
  n <- 120L
  data <- data.frame(
    y = stats::rnorm(n),
    z = stats::rnorm(n),
    x = stats::rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    stringsAsFactors = FALSE
  )
  data$y <- 0.8 * data$z + 0.5 * data$x + data$y
  residual <- first_stage_residual_metrics(
    data, treatment = "y", instrument = "z", controls = "x", fixed_effect = "none"
  )
  restricted <- stats::lm(y ~ x, data = data)
  full <- stats::lm(y ~ z + x, data = data)
  expected <- (stats::deviance(restricted) - stats::deviance(full)) / stats::deviance(restricted)

  expect_equal(residual$partial_r_squared, expected, tolerance = 1e-12)
  expect_equal(residual$partial_r_squared, residual$correlation^2, tolerance = 1e-12)
})

test_that("first-stage absorption diagnostics use fixed support and report requested statistics", {
  set.seed(42)
  states <- rep(sprintf("%02d", 1:12), each = 8)
  regions <- rep(panel_region_levels(), each = 16)
  z <- rep(seq(-1, 1, length.out = 8), 12) + rep(seq(-2, 2, length.out = 12), each = 8)
  panel <- data.frame(
    state_code_2001 = states,
    district_code_2001 = sprintf("%02d", seq_along(states)),
    region = factor(regions, levels = panel_region_levels()),
    ling_distance_nonzero_mean = z,
    emi_exposure_all_children_0708 = 12 + 3 * z + stats::rnorm(length(z), sd = 0.2),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(census_2001_diagnostic_controls())) {
    panel[[census_2001_diagnostic_controls()[i]]] <- stats::rnorm(nrow(panel)) + i / 10
  }

  out <- diagnose_first_stage_absorption(panel)

  expect_s3_class(out, "emi_first_stage_absorption")
  expect_equal(nrow(out$summary), nrow(first_stage_absorption_registry()))
  expect_true(all(out$summary$n == nrow(panel)))
  expect_true(all(out$summary$n_regions == 6L))
  expect_true(all(c(
    "estimate", "std.error", "partial_r_squared", "excluded_instrument_f",
    "residual_instrument_sd", "n"
  ) %in% names(out$summary)))
  expect_identical(
    out$summary$control_blocks[out$summary$specification_id == "state_fe_census_controls"],
    paste(names(first_stage_control_blocks()), collapse = ";")
  )
  expect_true(all(c(
    "region_fe_census_controls", "region_fe_expanded_controls",
    paste0("region_fe_plus_", names(first_stage_control_blocks()))
  ) %in% out$summary$specification_id))
  expect_true(all(c(
    "region_fe_main_without_human_capital", "state_fe_main_without_human_capital",
    "region_fe_expanded_without_human_capital", "state_fe_expanded_without_human_capital"
  ) %in% out$summary$specification_id))
  no_hc <- out$registry$controls[out$registry$specification_id == "region_fe_expanded_without_human_capital"][[1]]
  expect_length(intersect(no_hc, first_stage_control_blocks()$human_capital), 0L)
  expect_gt(out$summary$partial_r_squared[1], 0.9)
  expect_equal(nrow(out$state_deletion), length(unique(states)))
  expect_setequal(
    names(out$state_deletion),
    c(
      "specification_id", "specification", "treatment", "instrument", "omitted_state",
      "estimate", "excluded_instrument_f", "estimate_change", "f_change"
    )
  )
  expect_equal(nrow(out$district_influence), nrow(panel))
  expect_true(all(c("instrument_range", "treatment_range") %in% names(out$state_residual_ranges)))
  expect_true(all(c("leverage", "cooks_distance", "instrument_dfbeta") %in% names(out$district_influence)))
})

test_that("first-stage absorption diagnostics fail rather than changing support silently", {
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 2),
    region = rep(panel_region_levels(), each = 2),
    ling_distance_nonzero_mean = seq_len(12),
    emi_exposure_all_children_0708 = seq_len(12),
    stringsAsFactors = FALSE
  )
  panel$district_code_2001 <- sprintf("%02d", seq_len(nrow(panel)))
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- 1
  panel$st_share_2001[1] <- NA_real_

  prepared <- prepare_first_stage_absorption_panel(panel)
  expect_equal(nrow(prepared), 11L)
  expect_error(
    prepare_first_stage_absorption_panel(transform(panel, region = "Northern")),
    "all six panel regions"
  )
})

test_that("first-stage absorption diagnostics save a compact manifest", {
  set.seed(1)
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 4),
    region = rep(panel_region_levels(), each = 4),
    ling_distance_nonzero_mean = stats::rnorm(24),
    emi_exposure_all_children_0708 = stats::rnorm(24),
    stringsAsFactors = FALSE
  )
  panel$district_code_2001 <- sprintf("%02d", seq_len(nrow(panel)))
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(24)
  out <- diagnose_first_stage_absorption(panel)
  dir <- tempfile("first-stage-absorption-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  manifest <- save_first_stage_absorption_diagnostics(out, dir)

  expect_setequal(basename(manifest$path), c(
    "first_stage_absorption_ladder.csv", "first_stage_absorption_registry.csv",
    "first_stage_absorption_common_support.csv", "first_stage_state_residual_ranges.csv",
    "first_stage_state_deletion.csv", "first_stage_district_influence.csv",
    "first_stage_vif.csv"
  ))
  expect_true(all(file.exists(manifest$path)))
})


test_that("shared language preparation preserves mapped and unmapped rows for downstream diagnostics", {
  census <- data.frame(
    state_std = rep("01", 5), district_std = rep("001", 5),
    mother_tongue = c("English", "Dogri", "Hindi", "Bhojpuri", "Kashmiri"),
    canonical_language = c("English", "Dogri", "Hindi", "Hindi", "Kashmiri"),
    spkr_tot = c(10, 20, 60, 10, 5),
    ling_degrees = c(NA, NA, 0, NA, 4),
    stringsAsFactors = FALSE
  )
  panel <- data.frame(district_panel_id = "2001__01__001", stringsAsFactors = FALSE)

  prepared <- prepare_language_rows_for_decomposition(census, panel)
  expect_equal(nrow(prepared), 5)
  expect_true(any(!is.finite(num(prepared$ling_degrees))))
  expect_true(any(is.finite(num(prepared$ling_degrees))))

  unmapped <- unmapped_language_decomposition(census, panel)
  expect_setequal(unmapped$mother_tongue, c("Dogri", "Bhojpuri"))
  expect_false(any(unmapped$mother_tongue == "English"))
  expect_equal(sum(unmapped$unmapped_speakers), 30)

  distance4 <- distance_four_language_decomposition(census, panel)
  expect_identical(distance4$mother_tongue, "Kashmiri")
  expect_equal(sum(distance4$speakers), 5)
})

test_that("alternative linguistic-distance registry covers scalar, nonlinear, and joint constructions", {
  registry <- alternative_distance_registry()

  expect_equal(nrow(registry),
    length(alternative_distance_adjustments()) * length(alternative_distance_constructions()))
  expect_true(all(c(
    "nonzero_mean", "distant_share", "top3_legacy", "nonzero_mean_hindi_urdu",
    "nonzero_mean_shastry", "nonzero_mean_hindi_urdu_separate", "distance_shares_all",
    "distance_shares_all_unmapped", "distance_shares_mapped",
    "glottolog_mean", "glottolog_mean_shastry",
    "dyen_noncognate", "dyen_noncognate_shastry",
    "nonzero_mean_sensitivity_low", "nonzero_mean_sensitivity_high"
  ) %in% registry$construction_id))
  joint <- registry[registry$construction_id == "distance_shares_all", , drop = FALSE]
  expect_true(all(vapply(joint$excluded_instruments, function(x) {
    identical(x, linguistic_distance_excluded_instruments("all"))
  }, logical(1))))
  combined <- registry[registry$construction_id == "nonzero_mean_hindi_urdu", , drop = FALSE]
  expect_true(all(vapply(combined$included_language_controls, identical, logical(1), "hindi_urdu_share")))
  shastry <- registry[registry$construction_id == "nonzero_mean_shastry", , drop = FALSE]
  expect_true(all(vapply(
    shastry$included_language_controls,
    identical, logical(1), c("hindi_urdu_share", "native_english_share")
  )))
  shares <- registry[registry$construction_id == "distance_shares_all_unmapped", , drop = FALSE]
  expect_true(all(vapply(
    shares$included_language_controls,
    identical, logical(1), c("ling_unmapped_speaker_share", "native_english_share")
  )))
  expected_coverage <- c(
    nonzero_mean_sensitivity_low = "ling_sensitivity_mapped_speaker_share",
    nonzero_mean_sensitivity_high = "ling_sensitivity_mapped_speaker_share",
    glottolog_mean = "ling_glottolog_mapped_speaker_share",
    glottolog_mean_shastry = "ling_glottolog_mapped_speaker_share",
    dyen_noncognate = "ling_dyen_mapped_speaker_share",
    dyen_noncognate_shastry = "ling_dyen_mapped_speaker_share"
  )
  for (construction_id in names(expected_coverage)) {
    rows <- registry[registry$construction_id == construction_id, , drop = FALSE]
    expect_true(all(
      rows$mapping_coverage_variable == expected_coverage[[construction_id]]
    ))
  }

  default_ids <- setdiff(
    unique(registry$construction_id),
    names(expected_coverage)
  )
  expect_true(all(
    registry$mapping_coverage_variable[
      registry$construction_id %in% default_ids
    ] == "ling_mapped_speaker_share"
  ))
})

test_that("alternative linguistic-distance first stages use fixed support and joint clustered tests", {
  set.seed(19)
  states <- rep(sprintf("%02d", 1:12), each = 8)
  regions <- rep(panel_region_levels(), each = 16)
  n <- length(states)
  shares <- matrix(stats::runif(n * 6), ncol = 6)
  shares <- 100 * shares / rowSums(shares)
  panel <- data.frame(
    state_code_2001 = states,
    district_code_2001 = sprintf("%03d", seq_len(n)),
    region = factor(regions, levels = panel_region_levels()),
    ling_distance_nonzero_mean = rowSums(shares[, 2:6, drop = FALSE] * rep(1:5, each = n)) /
      rowSums(shares[, 2:6, drop = FALSE]),
    ling_share_distance_ge3 = rowSums(shares[, 4:6, drop = FALSE]),
    ling_distance_top3_legacy = stats::runif(n, 0, 5),
    hindi_share = 0.8 * shares[, 1],
    urdu_share = 0.2 * shares[, 1],
    hindi_urdu_share = shares[, 1],
    stringsAsFactors = FALSE
  )
  for (degree in 0:5) {
    panel[[paste0("ling_share_distance_", degree)]] <- shares[, degree + 1]
    panel[[paste0("ling_mapped_share_distance_", degree)]] <- shares[, degree + 1]
  }
  panel$ling_mapped_speaker_share <- 100
  panel$ling_unmapped_speaker_share <- 0
  panel$ling_distance_glottolog_nonhindi_mean <- panel$ling_distance_nonzero_mean + 1
  panel$ling_glottolog_mapped_speaker_share <- 100
  panel$ling_glottolog_unmapped_speaker_share <- 0
  panel$ling_distance_dyen_noncognate_pct <- 100 - 10 * panel$ling_distance_nonzero_mean
  panel$ling_dyen_mapped_speaker_share <- 100
  panel$ling_distance_nonzero_mean_sensitivity_low <- panel$ling_distance_nonzero_mean
  panel$ling_distance_nonzero_mean_sensitivity_high <- panel$ling_distance_nonzero_mean
  panel$ling_sensitivity_mapped_speaker_share <- 100
  panel$ling_dyen_unmapped_speaker_share <- 0
  panel$native_english_share <- 0
  panel$emi_exposure_all_children_0708 <- 5 + 0.15 * panel$ling_share_distance_5 +
    0.08 * panel$ling_share_distance_4 + stats::rnorm(n)
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)

  out <- diagnose_alternative_distance_first_stages(panel)
  panel$future_hces_outcome <- seq_len(nrow(panel))
  panel$real_log_consumption_change <- stats::rnorm(nrow(panel))
  panel$real_log_consumption_change[[1]] <- NA_real_
  weak_iv <- withCallingHandlers(
    estimate_weak_iv_outcomes(panel),
    warning = function(w) {
      # This synthetic all-registry fixture intentionally creates rank-deficient
      # instrument sets for some alternative specifications. Consume only the
      # warning ivreg emits for that known fixture property; every other warning
      # must still reach testthat and fail the audit.
      if (identical(conditionMessage(w), "some instrumental variables are collinear")) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_true("effective_f" %in% names(weak_iv$summary))
  expect_true(all(is.finite(weak_iv$summary$effective_f)))
  expect_true(all(weak_iv$summary$effective_f > 0))
  projected <- prepare_alternative_distance_panel(panel)
  projected_with_outcome <- prepare_alternative_distance_panel(
    panel,
    retain = "real_log_consumption_change"
  )
  augmentation_panel <- project_alternative_distance_panel(
    panel,
    retain = "real_log_consumption_change"
  )
  registry <- alternative_distance_registry()
  branch_specification <- registry[
    registry$construction_id == "distance_shares_all" &
      registry$adjustment_id == "state_main",
    ,
    drop = FALSE
  ]
  expect_true(is.list(branch_specification$excluded_instruments))
  expect_equal(
    unlist(branch_specification$excluded_instruments[[1]], use.names = FALSE),
    linguistic_distance_excluded_instruments("all")
  )

  branches <- lapply(seq_len(nrow(registry)), function(i) {
    diagnose_alternative_distance_specification(
      projected, registry[i, , drop = FALSE]
    )
  })
  branched <- assemble_alternative_distance_first_stages(
    projected, registry, branches
  )

  expect_false("future_hces_outcome" %in% names(projected))
  expect_false("future_hces_outcome" %in% names(augmentation_panel))
  expect_true("real_log_consumption_change" %in% names(projected_with_outcome))
  expect_true("real_log_consumption_change" %in% names(augmentation_panel))
  expect_equal(nrow(projected_with_outcome), nrow(projected))
  expect_equal(nrow(augmentation_panel), nrow(panel))
  expect_true(is.na(projected_with_outcome$real_log_consumption_change[[1]]))
  expect_true(is.na(augmentation_panel$real_log_consumption_change[[1]]))
  expect_equal(branched$summary, out$summary)
  expect_equal(branched$coefficients, out$coefficients)
  expect_equal(branched$coverage_sensitivity, out$coverage_sensitivity)
  expect_s3_class(out, "emi_alternative_distance_first_stages")
  expect_equal(nrow(out$summary), nrow(alternative_distance_registry()))
  expect_true(all(out$summary$n == n))
  expect_true(all(c("joint_excluded_f", "joint_excluded_p", "partial_r_squared") %in% names(out$summary)))
  expect_equal(
    out$summary$n_excluded_instruments[out$summary$construction_id == "distance_shares_all"],
    rep(5L, length(alternative_distance_adjustments()))
  )
  expect_true(any(is.finite(out$summary$joint_excluded_f[out$summary$construction_id == "distance_shares_all"])))
  expect_setequal(unique(out$coverage_sensitivity$minimum_mapped_share), linguistic_mapping_coverage_thresholds())
  expect_setequal(
    unique(out$coverage_sensitivity$coverage_variable),
    c(
      "ling_mapped_speaker_share",
      "ling_sensitivity_mapped_speaker_share",
      "ling_glottolog_mapped_speaker_share",
      "ling_dyen_mapped_speaker_share"
    )
  )
  expect_equal(
    nrow(out$coefficients[out$coefficients$term %in% linguistic_distance_excluded_instruments("all"), ]),
    10L * length(alternative_distance_adjustments())
  )
})

test_that("alternative linguistic-distance diagnostics save four explicit outputs", {
  set.seed(23)
  n <- 48
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 4),
    district_code_2001 = sprintf("%03d", seq_len(n)),
    region = rep(panel_region_levels(), each = 8),
    emi_exposure_all_children_0708 = stats::rnorm(n),
    ling_distance_nonzero_mean = stats::runif(n, 1, 5),
    ling_share_distance_ge3 = stats::runif(n, 0, 100),
    ling_distance_top3_legacy = stats::runif(n, 0, 5),
    hindi_share = stats::runif(n, 0, 60),
    urdu_share = stats::runif(n, 0, 20),
    stringsAsFactors = FALSE
  )
  panel$hindi_urdu_share <- panel$hindi_share + panel$urdu_share
  panel$native_english_share <- 0
  distance_shares <- matrix(stats::runif(n * 6), ncol = 6)
  distance_shares <- 100 * distance_shares / rowSums(distance_shares)
  for (degree in 0:5) {
    panel[[paste0("ling_share_distance_", degree)]] <- distance_shares[, degree + 1]
    panel[[paste0("ling_mapped_share_distance_", degree)]] <- distance_shares[, degree + 1]
  }
  panel$ling_mapped_speaker_share <- 100
  panel$ling_unmapped_speaker_share <- 0
  panel$ling_distance_glottolog_nonhindi_mean <- panel$ling_distance_nonzero_mean + 1
  panel$ling_glottolog_mapped_speaker_share <- 100
  panel$ling_glottolog_unmapped_speaker_share <- 0
  panel$ling_distance_dyen_noncognate_pct <- 100 - 10 * panel$ling_distance_nonzero_mean
  panel$ling_dyen_mapped_speaker_share <- 100
  panel$ling_distance_nonzero_mean_sensitivity_low <- panel$ling_distance_nonzero_mean
  panel$ling_distance_nonzero_mean_sensitivity_high <- panel$ling_distance_nonzero_mean
  panel$ling_sensitivity_mapped_speaker_share <- 100
  panel$ling_dyen_unmapped_speaker_share <- 0
  panel$native_english_share <- 0
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)
  out <- diagnose_alternative_distance_first_stages(panel)
  dir <- tempfile("alternative-distance-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  manifest <- save_alternative_distance_first_stages(out, dir)

  expect_setequal(basename(manifest$path), c(
    "alternative_distance_first_stage_summary.csv",
    "alternative_distance_first_stage_coefficients.csv",
    "alternative_distance_first_stage_registry.csv",
    "alternative_distance_first_stage_common_support.csv",
    "alternative_distance_mapping_coverage_sensitivity.csv",
    "distance4_language_decomposition.csv",
    "unmapped_language_decomposition.csv",
    "distance4_leave_one_language_out.csv",
    "alternative_distance_weak_iv_outcomes.csv",
    "alternative_distance_anderson_rubin_grid.csv",
    "iv_diagnostic_applicability.csv",
    "iv_diagnostic_registry.csv",
    "iv_specification_registry.csv",
    "iv_overidentification.csv",
    "iv_monotonicity_summary.csv",
    "iv_monotonicity_bins.csv",
    "iv_monotonicity_state_slopes.csv",
    "linguistic_distance_basis_comparison.csv"
  ))
  expect_true(all(file.exists(manifest$path)))
})

test_that("canonical IV registry drives alternative-distance specifications", {
  registry <- iv_specification_registry()

  expect_identical(alternative_distance_registry(), registry)
  expect_equal(nrow(registry), length(iv_adjustment_sets()) * length(iv_instrument_constructions()))
  expect_true(all(c(
    "outcome", "treatment", "fixed_effect", "controls", "excluded_instruments",
    "n_endogenous", "n_excluded_instruments", "panel_variant", "sample_rule",
    "cluster", "tier", "mapping_coverage_variable"
  ) %in% names(registry)))
  expect_true(all(registry$n_endogenous == 1L))
  expect_true(all(registry$cluster == "state_code_2001"))
})

test_that("canonical IV diagnostic registry includes MOP effective F", {
  registry <- iv_diagnostic_registry()
  row <- registry[registry$diagnostic_id == "effective_f", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_equal(row$family, "relevance")
  expect_true(row$implemented)
  expect_true(row$requires_outcome)
  expect_false(row$requires_overidentified)
})

test_that("canonical IV registries preserve vector-valued specification fields", {
  registry <- iv_specification_registry()
  absorption <- iv_absorption_specification_registry()
  combined <- iv_diagnostic_specification_registry()

  expect_true(is.list(registry$controls))
  expect_true(is.list(registry$included_language_controls))
  expect_true(is.list(registry$excluded_instruments))
  expect_true(is.list(absorption$controls))
  expect_true(is.list(combined$controls))

  main <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  expanded <- absorption[
    absorption$adjustment_id == "state_fe_expanded_controls",
    , drop = FALSE
  ]
  joint <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "distance_shares_all",
    , drop = FALSE
  ]

  expect_identical(main$controls[[1]], census_2001_main_controls())
  expect_identical(expanded$controls[[1]], census_2001_absorption_controls())
  expect_identical(
    joint$excluded_instruments[[1]],
    linguistic_distance_excluded_instruments("all")
  )
})

test_that("diagnostic applicability follows identification structure", {
  registry <- iv_specification_registry()
  applicability <- iv_diagnostic_applicability(registry)
  scalar_id <- registry$specification_id[registry$construction_id == "nonzero_mean" & registry$adjustment_id == "state_main"][[1]]
  multi_id <- registry$specification_id[registry$construction_id == "distance_shares_all" & registry$adjustment_id == "state_main"][[1]]

  scalar_overid <- applicability[
    applicability$specification_id == scalar_id & applicability$diagnostic_id == "overidentification", , drop = FALSE
  ]
  multi_overid <- applicability[
    applicability$specification_id == multi_id & applicability$diagnostic_id == "overidentification", , drop = FALSE
  ]

  expect_false(scalar_overid$applicable)
  expect_identical(scalar_overid$reason, "exactly_identified")
  expect_true(multi_overid$applicable)
  expect_true(multi_overid$implemented)
  expect_true(multi_overid$will_run)
  balance_joint <- applicability[applicability$diagnostic_id == "balance_joint", , drop = FALSE]
  n_inst <- registry$n_excluded_instruments[match(balance_joint$specification_id, registry$specification_id)]
  expect_true(all(balance_joint$applicable == (n_inst == 1L)))
  expect_true(all(balance_joint$will_run == (n_inst == 1L)))
  expect_true(all(applicability$applicable[applicability$diagnostic_id == "anderson_rubin"]))
  expect_true(all(applicability$will_run[applicability$diagnostic_id == "anderson_rubin"]))

  monotonicity <- applicability[applicability$diagnostic_id == "monotonicity_shape", , drop = FALSE]
  scalar_specs <- registry$n_excluded_instruments == 1L
  expect_true(all(monotonicity$applicable == scalar_specs))
  expect_true(all(monotonicity$will_run == scalar_specs))
})

test_that("IV specification variables resolve transformed fixed-effect terms", {
  registry <- iv_specification_registry()

  region_spec <- registry[
    registry$adjustment_id == "region_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  state_spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]

  region_vars <- iv_specification_variables(region_spec)
  state_vars <- iv_specification_variables(state_spec)

  expect_true("region" %in% region_vars)
  expect_false("factor(region)" %in% region_vars)
  expect_true("state_code_2001" %in% state_vars)
  expect_false("factor(state_code_2001)" %in% state_vars)
  expect_true(region_spec$outcome[[1]] %in% region_vars)
  expect_true(region_spec$treatment[[1]] %in% region_vars)
  expect_true(all(unlist(region_spec$excluded_instruments[[1]]) %in% region_vars))
  expect_true(region_spec$cluster[[1]] %in% region_vars)
})

test_that("conditional balance removes tested and accounting-linked controls", {
  registry <- iv_specification_registry()

  main_spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  ordinary <- census_2001_main_controls()[[1]]
  expect_false(ordinary %in% balance_nuisance_controls(main_spec, ordinary))
  expect_setequal(
    balance_nuisance_controls(main_spec, ordinary),
    setdiff(census_2001_main_controls(), ordinary)
  )

  expanded_spec <- registry[
    registry$adjustment_id == "state_expanded" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  aggregate <- "agricultural_worker_share_2001"
  nuisance <- balance_nuisance_controls(expanded_spec, aggregate)

  expect_false(any(c(
    aggregate,
    "cultivator_share_workers_2001",
    "agricultural_labourer_share_workers_2001"
  ) %in% nuisance))
})

test_that("conditional balance does not fit an accounting identity as an outcome", {
  set.seed(901)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  for (variable in census_2001_diagnostic_controls()) {
    panel[[variable]] <- stats::rnorm(n)
  }
  panel$cultivator_share_workers_2001 <- stats::runif(n, 0, 60)
  panel$agricultural_labourer_share_workers_2001 <- stats::runif(n, 0, 40)
  panel$agricultural_worker_share_2001 <-
    panel$cultivator_share_workers_2001 +
    panel$agricultural_labourer_share_workers_2001

  spec <- iv_specification_registry()
  spec <- spec[
    spec$adjustment_id == "state_expanded" &
      spec$construction_id == "nonzero_mean",
    , drop = FALSE
  ]

  expect_warning(
    out <- estimate_iv_balance_spec(
      panel,
      spec,
      "agricultural_worker_share_2001"
    ),
    NA
  )
  expect_equal(out$status, "estimated")
  expect_true(is.finite(out$joint_f))
})

test_that("registry-driven clustered diagnostics honor the declared cluster variable", {
  set.seed(905)
  n <- 120L
  panel <- data.frame(
    cluster_id = rep(sprintf("c%02d", 1:12), each = 10),
    real_log_consumption_change = stats::rnorm(n),
    emi_exposure_all_children_0708 = stats::rnorm(n),
    ling_distance_nonzero_mean = stats::rnorm(n),
    log_population_2001 = stats::rnorm(n),
    stringsAsFactors = FALSE
  )

  registry <- iv_specification_registry()
  spec <- registry[
    registry$adjustment_id == "unadjusted" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  spec$cluster <- "cluster_id"

  expect_identical(iv_specification_cluster_variable(spec), "cluster_id")
  expect_identical(iv_specification_cluster(panel, spec), panel$cluster_id)
  expect_true("cluster_id" %in% iv_specification_variables(spec))
  expect_false("state_code_2001" %in% iv_specification_variables(spec))

  balance <- estimate_iv_balance_spec(panel, spec, "log_population_2001")
  expect_identical(balance$status[[1]], "estimated")
  expect_true(is.finite(balance$joint_f[[1]]))

  ar <- estimate_anderson_rubin_spec(panel, spec, points = 41L)
  expect_identical(ar$summary$status[[1]], "estimated")
  expect_true(is.finite(ar$summary$anderson_rubin_p_beta0[[1]]))
})

test_that("conditional balance uses specification fixed effects and clustered joint tests", {
  set.seed(902)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$emi_exposure_all_children_0708 <- 2 * panel$ling_distance_nonzero_mean + stats::rnorm(n)
  for (v in census_2001_diagnostic_controls()) panel[[v]] <- stats::rnorm(n)
  tested <- census_2001_main_controls()[[1]]
  panel[[tested]] <- 0.5 * panel$ling_distance_nonzero_mean + stats::rnorm(n)
  spec <- iv_specification_registry()
  spec <- spec[spec$adjustment_id == "state_main" & spec$construction_id == "nonzero_mean", , drop = FALSE]

  out <- estimate_iv_balance_spec(panel, spec, tested)

  expect_equal(out$status, "estimated")
  expect_equal(out$fixed_effect, "state")
  expect_equal(out$n_excluded_instruments, 1L)
  expect_true(is.finite(out$joint_f))
  expect_true(is.finite(out$standardized_effect))
})

test_that("diagnostic specification registry absorbs the control-block ladder without duplicates", {
  registry <- iv_diagnostic_specification_registry()
  signatures <- vapply(seq_len(nrow(registry)), function(i) {
    iv_specification_signature(registry[i, , drop = FALSE])
  }, character(1))

  expect_false(anyDuplicated(signatures) > 0L)
  expect_true(any(grepl("^absorption__", registry$specification_id)))
  expect_true(all(iv_specification_registry()$specification_id %in% registry$specification_id))
  expect_true(all(registry$cluster == "state_code_2001"))
})

test_that("joint balance tests only predetermined covariates not already conditioned on", {
  registry <- iv_specification_registry()
  spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  tested <- joint_balance_test_variables(spec)

  expect_setequal(tested, setdiff(
    census_2001_diagnostic_controls(),
    census_2001_main_controls()
  ))
})

test_that("joint balance is one omnibus test per scalar specification", {
  set.seed(904)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$emi_exposure_all_children_0708 <- stats::rnorm(n)
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)

  registry <- iv_specification_registry()
  scalar <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  multi <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "distance_shares_all",
    , drop = FALSE
  ]
  for (instrument in unlist(multi$excluded_instruments[[1]], use.names = FALSE)) {
    panel[[instrument]] <- stats::rnorm(n)
  }

  out <- run_iv_joint_balance_diagnostics(panel, rbind(scalar, multi))

  expect_equal(nrow(out), 1L)
  expect_identical(out$specification_id[[1]], scalar$specification_id[[1]])
  expect_identical(out$status[[1]], "estimated")
  expect_true(is.finite(out$joint_f[[1]]))
})

test_that("monotonicity shape diagnostic recognizes an increasing residual first stage", {
  set.seed(903)
  n <- 180L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:18), each = 10),
    region = rep(panel_region_levels(), each = 30),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  panel$emi_exposure_all_children_0708 <-
    2 * panel$ling_distance_nonzero_mean + stats::rnorm(n, sd = 0.2)
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)

  registry <- iv_specification_registry()
  spec <- registry[
    registry$adjustment_id == "state_main" &
      registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  out <- estimate_iv_monotonicity_shape(panel, spec, bins = 8L)

  expect_equal(out$summary$status, "estimated")
  expect_gt(out$summary$linear_slope, 0)
  expect_gt(out$summary$spearman_rho, 0)
  expect_gt(out$summary$share_nondecreasing_bin_steps, 0.5)
  expect_true(all(c("bin", "instrument", "treatment", "n") %in% names(out$bins)))
  expect_true(all(c("state_code_2001", "slope", "status") %in% names(out$state_slopes)))
})

test_that("Anderson-Rubin inference accepts scalar and multi-instrument registry specifications", {
  set.seed(904)
  n <- 120L
  panel <- data.frame(
    state_code_2001 = rep(sprintf("%02d", 1:12), each = 10),
    region = rep(panel_region_levels(), each = 20),
    real_log_consumption_change = stats::rnorm(n),
    emi_exposure_all_children_0708 = stats::rnorm(n),
    ling_distance_nonzero_mean = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
  shares <- matrix(stats::runif(n * 6), ncol = 6)
  shares <- 100 * shares / rowSums(shares)
  for (degree in 0:5) {
    panel[[paste0("ling_share_distance_", degree)]] <- shares[, degree + 1]
    panel[[paste0("ling_mapped_share_distance_", degree)]] <- shares[, degree + 1]
  }
  panel$ling_share_distance_ge3 <- rowSums(shares[, 4:6, drop = FALSE])
  panel$ling_distance_top3_legacy <- stats::runif(n, 0, 5)
  panel$hindi_share <- stats::runif(n, 0, 60)
  panel$urdu_share <- stats::runif(n, 0, 20)
  panel$hindi_urdu_share <- panel$hindi_share + panel$urdu_share
  panel$native_english_share <- 0
  panel$ling_unmapped_speaker_share <- 0
  panel$ling_mapped_speaker_share <- 100
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)

  registry <- iv_specification_registry()
  scalar <- registry[
    registry$adjustment_id == "state_main" & registry$construction_id == "nonzero_mean",
    , drop = FALSE
  ]
  multi <- registry[
    registry$adjustment_id == "state_main" & registry$construction_id == "distance_shares_all",
    , drop = FALSE
  ]

  scalar_ar <- estimate_anderson_rubin_spec(panel, scalar, points = 21L)
  multi_ar <- estimate_anderson_rubin_spec(panel, multi, points = 21L)

  expect_equal(scalar_ar$summary$status, "estimated")
  expect_equal(multi_ar$summary$status, "estimated")
  expect_equal(nrow(scalar_ar$grid), 21L)
  expect_equal(nrow(multi_ar$grid), 21L)
  expect_true(all(c("anderson_rubin_f_beta0", "anderson_rubin_p_beta0") %in% names(multi_ar$summary)))
})


test_that("linguistic basis comparison remains pairwise under partial lexical coverage", {
  panel <- data.frame(
    ling_distance_nonzero_mean = c(1, 2, 3, 4),
    ling_distance_glottolog_nonhindi_mean = c(2, 4, 6, 8),
    ling_distance_dyen_noncognate_pct = c(20, 30, NA, 50)
  )

  out <- compare_linguistic_distance_bases(panel)

  expect_setequal(
    paste(out$basis_a, out$basis_b, sep = "__"),
    c("shastry__glottolog", "shastry__dyen", "glottolog__dyen")
  )
  expect_equal(out$n[out$basis_b == "dyen"], c(3, 3))
  expect_equal(
    out$pearson_correlation[out$basis_a == "shastry" & out$basis_b == "glottolog"],
    1
  )
})


test_that("language decomposition uses the same adjudicated Shastry resolver as production", {
  census <- data.frame(
    state_std = "01",
    district_std = "001",
    mother_tongue_code = c("000001", "006045"),
    mother_tongue = c("Hindi", "Bhojpuri"),
    canonical_language = c("Hindi", "Hindi"),
    spkr_tot = c(50, 50),
    stringsAsFactors = FALSE
  )
  panel <- data.frame(district_panel_id = make_district_key("01", "001", 2001L))

  rows <- prepare_language_rows_for_decomposition(census, panel)

  expect_equal(rows$ling_degrees[rows$language_identity == "Bhojpuri"], 3)
})


test_that("language decomposition can use the same non-Indo-European resolution as production", {
  census <- data.frame(
    state_std = "01",
    district_std = "001",
    mother_tongue_code = "000001",
    mother_tongue = "Korku",
    canonical_language = "Korku",
    spkr_tot = 100,
    stringsAsFactors = FALSE
  )
  panel <- data.frame(district_panel_id = make_district_key("01", "001", 2001L))
  g <- data.frame(
    id = c("aust1307", "korku"),
    family_id = c("", "aust1307"),
    parent_id = c("", "aust1307"),
    name = c("Austroasiatic", "Korku"),
    bookkeeping = FALSE,
    level = c("family", "language"),
    iso639P3code = c("", "kfq"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    mother_tongue_code = "000001",
    mother_tongue = "Korku",
    canonical_language = "Korku",
    language_glottocode = "korku",
    family_id = "aust1307",
    iso639P3code = "kfq",
    match_basis = "manual",
    review_status = "accepted_manual",
    stringsAsFactors = FALSE
  )

  prepared <- prepare_language_rows_for_decomposition(
    census, panel, list(languoids = g), crosswalk
  )

  expect_equal(prepared$ling_degrees, 5)
  expect_equal(nrow(unmapped_language_decomposition(
    census, panel, list(languoids = g), crosswalk
  )), 0)
})


test_that("Shastry adjudication sensitivities reuse the canonical IV registry machinery", {
  registry <- iv_specification_registry()
  rows <- registry[
    registry$construction_id %in%
      c("nonzero_mean_sensitivity_low", "nonzero_mean_sensitivity_high"),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 2L * length(iv_adjustment_sets()))
  expect_true(all(rows$mapping_coverage_variable == "ling_sensitivity_mapped_speaker_share"))
  expect_true(all(vapply(
    rows$included_language_controls,
    function(x) setequal(x, c("hindi_urdu_share", "native_english_share")),
    logical(1)
  )))
})

test_that("clustered first-stage diagnostics reuse a cached covariance without changing inference", {
  set.seed(42)
  data <- data.frame(
    y = rnorm(80),
    z1 = rnorm(80),
    z2 = rnorm(80),
    cluster = rep(seq_len(20), each = 4)
  )
  fit <- stats::lm(y ~ z1 + z2, data = data)
  cached <- iv_clustered_inference(fit, data$cluster)

  direct_joint <- clustered_joint_wald_test(fit, c("z1", "z2"), data$cluster)
  cached_joint <- clustered_joint_wald_test(
    fit, c("z1", "z2"), data$cluster, inference = cached
  )
  direct_coef <- clustered_first_stage_inference(fit, "z1", data$cluster)
  cached_coef <- clustered_first_stage_inference(
    fit, "z1", data$cluster, inference = cached
  )

  expect_equal(cached_joint, direct_joint, tolerance = 1e-12)
  expect_equal(cached_coef, direct_coef, tolerance = 1e-12)
})

test_that("IV control ordering preserves explicitly declared noncanonical controls", {
  canonical <- c("a", "b", "c")
  expect_identical(
    order_iv_controls(c("custom_baseline", "c", "a"), canonical),
    c("a", "c", "custom_baseline")
  )
  expect_identical(
    order_iv_controls(c("c", "a"), canonical),
    c("a", "c")
  )
})

test_that("consumption IV registry compiles ANCOVA into the canonical IV specification contract", {
  registry <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    treatment = preferred_iv_variables()$treatment,
    instrument = preferred_iv_variables()$instrument,
    adjustment_id = "state_main",
    construction_id = "nonzero_mean",
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    tier = "A",
    stringsAsFactors = FALSE
  )

  out <- compile_consumption_iv_specifications(registry)
  baseline <- consumption_iv_variable_name("long_2023__ancova", "baseline")
  expect_equal(nrow(out), 1L)
  expect_identical(out$fixed_effect[[1]], "state")
  expect_identical(out$outcome_round[[1]], "hces_2023_24")
  expect_identical(out$baseline_round[[1]], "nss_2004_05")
  expect_identical(out$estimand[[1]], "ancova")
  expect_true(baseline %in% unlist(out$controls[[1]], use.names = FALSE))
  expect_true(all(census_2001_main_controls() %in% unlist(out$controls[[1]], use.names = FALSE)))
  expect_identical(
    unlist(out$excluded_instruments[[1]], use.names = FALSE),
    preferred_iv_variables()$instrument
  )
})

test_that("Tier-A consumption IV specifications cannot select on outcome precision", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  registry <- data.frame(
    welfare_specification_id = "headline",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    treatment = preferred_iv_variables()$treatment,
    instrument = preferred_iv_variables()$instrument,
    adjustment_id = "state_main",
    construction_id = "nonzero_mean",
    panel_variant = "primary",
    sample_rule = "preferred_welfare_support",
    tier = "A",
    stringsAsFactors = FALSE
  )
  utils::write.csv(registry, path, row.names = FALSE)

  expect_error(
    read_consumption_iv_outcome_registry(path),
    "Tier-A consumption IV specifications must use ex-ante"
  )

  registry$tier <- "B"
  utils::write.csv(registry, path, row.names = FALSE)
  expect_identical(
    read_consumption_iv_outcome_registry(path)$sample_rule,
    "preferred_welfare_support"
  )
})

test_that("consumption IV outcome data separate analysis support from precision sensitivity", {
  welfare <- data.frame(
    district_2001 = rep(c("pc2001__01__01", "pc2001__01__02", "pc2001__01__03"), 2),
    round_id = rep(c("nss_2004_05", "hces_2023_24"), each = 3),
    outcome_id = "real_mean_mpce",
    estimate = c(100, 200, 300, 200, 400, 600),
    analysis_eligible = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    preferred_eligible = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )

  out <- build_consumption_iv_specification_data(welfare, spec)
  expect_equal(out$outcome_value[[1]], log(200))
  expect_equal(out$baseline_value[[1]], log(100))
  expect_true(out$welfare_support[[1]])
  # District 2 fails the stricter RSE/reporting flag but remains in the primary
  # causal sample because both rounds pass predeclared survey-support rules.
  expect_equal(out$outcome_value[[2]], log(400))
  expect_equal(out$baseline_value[[2]], log(200))
  expect_true(out$welfare_support[[2]])
  expect_true(is.na(out$outcome_value[[3]]))
  expect_false(out$welfare_support[[3]])

  spec$sample_rule <- "preferred_welfare_support"
  strict <- build_consumption_iv_specification_data(welfare, spec)
  expect_true(is.na(strict$outcome_value[[2]]))
  expect_true(is.na(strict$baseline_value[[2]]))
  expect_false(strict$welfare_support[[2]])
})

test_that("consumption IV change outcomes use the same transformed baseline and endpoint", {
  welfare <- data.frame(
    district_2001 = rep(c("pc2001__01__01", "pc2001__01__02"), 2),
    round_id = rep(c("nss_2004_05", "nss_2011_12_type2"), each = 2),
    outcome_id = "real_mean_mpce",
    estimate = c(100, 200, 150, 100),
    analysis_eligible = TRUE,
    preferred_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    welfare_specification_id = "medium_2011__change",
    outcome_id = "real_mean_mpce",
    outcome_round = "nss_2011_12_type2",
    baseline_round = "nss_2004_05",
    estimand = "change",
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )

  out <- build_consumption_iv_specification_data(welfare, spec)
  expect_equal(
    out$outcome_value,
    c(log(150) - log(100), log(100) - log(200)),
    tolerance = 1e-12
  )
  expect_equal(out$baseline_value, log(c(100, 200)), tolerance = 1e-12)
})

test_that("consumption IV panel augmentation preserves canonical panel rows", {
  panel <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    keep = c("a", "b"),
    stringsAsFactors = FALSE
  )
  welfare <- data.frame(
    district_2001 = rep(panel$target_unit_2001, 2),
    round_id = rep(c("nss_2004_05", "hces_2023_24"), each = 2),
    outcome_id = "real_mean_mpce",
    estimate = c(100, 200, 150, 300),
    analysis_eligible = TRUE,
    preferred_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  registry <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    sample_rule = "analysis_welfare_support",
    stringsAsFactors = FALSE
  )

  out <- attach_consumption_iv_outcomes(panel, welfare, registry)
  expect_identical(out$target_unit_2001, panel$target_unit_2001)
  expect_identical(out$keep, panel$keep)
  expect_equal(
    out[[consumption_iv_variable_name("long_2023__ancova", "outcome")]],
    log(c(150, 300))
  )
  expect_equal(
    out[[consumption_iv_variable_name("long_2023__ancova", "baseline")]],
    log(c(100, 200))
  )
})

test_that("IV specification row binding preserves list-column contracts", {
  row_a <- iv_specification_row(
    specification_id = "a",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y_a",
    treatment = "d",
    fixed_effect = "state",
    controls = c("literacy_rate_2001", "custom_baseline"),
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support"
  )
  row_b <- row_a
  row_b$specification_id <- "b"
  row_b$outcome <- "y_b"

  out <- bind_iv_specification_rows(list(row_a, row_b))
  expect_true(is.list(out$controls))
  expect_true(is.list(out$excluded_instruments))
  expect_true(is.list(out$included_language_controls))
  expect_true("custom_baseline" %in% unlist(out$controls[[1]], use.names = FALSE))
  expect_identical(unlist(out$excluded_instruments[[2]], use.names = FALSE), "z")
})

test_that("IV analysis frames drop sf geometry without flattening analysis columns", {
  skip_if_not_installed("sf")
  panel <- data.frame(
    y = c(1, 2),
    d = c(0.2, 0.4),
    z = c(0.1, 0.3),
    state_code_2001 = c("01", "02"),
    stringsAsFactors = FALSE
  )
  panel$x_coord <- c(0, 1)
  panel$y_coord <- c(0, 1)
  panel <- sf::st_as_sf(
    panel,
    coords = c("x_coord", "y_coord"),
    crs = 4326
  )

  out <- iv_analysis_frame(panel, c("y", "d", "z", "state_code_2001"))

  expect_false(inherits(out, "sf"))
  expect_false("geometry" %in% names(out))
  expect_identical(out$y, c(1, 2))
  expect_identical(out$state_code_2001, c("01", "02"))
})

test_that("IV analysis frames reject non-atomic registered variables", {
  panel <- data.frame(y = 1:2)
  panel$z <- I(list(1, 2))

  expect_error(
    iv_analysis_frame(panel, c("y", "z")),
    "must be atomic"
  )
})

test_that("consumption IV coverage resolves fixed-effect formula terms to panel variables", {
  registry <- data.frame(
    welfare_specification_id = "long_2023__ancova",
    outcome_id = "real_mean_mpce",
    outcome_round = "hces_2023_24",
    baseline_round = "nss_2004_05",
    estimand = "ancova",
    analysis_transform = "log",
    treatment = preferred_iv_variables()$treatment,
    instrument = preferred_iv_variables()$instrument,
    adjustment_id = "state_main",
    construction_id = "nonzero_mean",
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    tier = "A",
    stringsAsFactors = FALSE
  )
  spec <- compile_consumption_iv_specifications(registry)
  required <- iv_specification_variables(spec, include_outcome = TRUE)
  panel <- as.data.frame(
    setNames(
      replicate(length(required), rep(1, 5), simplify = FALSE),
      required
    ),
    stringsAsFactors = FALSE
  )
  panel$state_code_2001 <- c("01", "01", "02", "02", "03")

  out <- summarize_consumption_iv_outcome_coverage(panel, spec)
  expect_equal(out$status, "ready")
  expect_equal(out$n_analysis_complete, 5L)
  expect_true(is.na(out$missing_columns))
  expect_false(grepl("factor\\(", paste(required, collapse = ";")))
})

test_that("consumption IV coverage validation blocks non-ready registered specifications", {
  good <- data.frame(
    specification_id = "ready",
    n_analysis_complete = 10L,
    analysis_share = 0.5,
    status = "ready",
    missing_columns = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_equal(
    validate_consumption_iv_outcome_coverage(good),
    good,
    ignore_attr = TRUE
  )

  bad <- good
  bad$specification_id <- "broken"
  bad$status <- "missing_columns"
  bad$missing_columns <- "state_code_2001"
  expect_error(
    validate_consumption_iv_outcome_coverage(bad),
    "broken=missing_columns\\[state_code_2001\\]"
  )
})

test_that("canonical IV specification coercion preserves list columns", {
  row <- iv_specification_row(
    specification_id = "one",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = c("literacy_rate_2001", "custom_baseline"),
    included_language_controls = "hindi_urdu_share",
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support",
    cluster = "state_code_2001"
  )
  out <- as_iv_specifications(row)
  expect_true(is.list(out$controls))
  expect_true(is.list(out$included_language_controls))
  expect_true(is.list(out$excluded_instruments))
  expect_identical(
    unlist(out$controls[[1]], use.names = FALSE),
    unlist(row$controls[[1]], use.names = FALSE)
  )
})

test_that("canonical IV specification coercion rejects flattened list columns", {
  row <- iv_specification_row(
    specification_id = "one",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "literacy_rate_2001",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support"
  )
  expect_error(
    as_iv_specifications(safe_df(row)),
    "list-column contract was lost"
  )
})

test_that("IV variable extraction preserves formula and cluster contracts after binding", {
  rows <- lapply(c("y1", "y2"), function(outcome) {
    iv_specification_row(
      specification_id = outcome,
      adjustment_id = "state_main",
      adjustment = "State FE + main controls",
      construction_id = "nonzero_mean",
      construction = "Nonzero mean",
      outcome = outcome,
      treatment = "d",
      fixed_effect = "state",
      controls = c("literacy_rate_2001", "custom_baseline"),
      included_language_controls = character(),
      excluded_instruments = "z",
      mapping_coverage_variable = "coverage",
      panel_variant = "primary",
      sample_rule = "support",
      cluster = "state_code_2001"
    )
  })
  specs <- bind_iv_specification_rows(rows)
  vars <- iv_specification_variables(specs[1, , drop = FALSE])
  expect_true(all(c(
    "y1", "d", "z", "literacy_rate_2001", "custom_baseline", "state_code_2001"
  ) %in% vars))
  expect_false(any(grepl("factor\\(", vars)))
  expect_identical(
    iv_specification_cluster_variable(specs[1, , drop = FALSE]),
    "state_code_2001"
  )
})

test_that("IV formula helpers reject multi-row specifications explicitly", {
  row <- iv_specification_row(
    specification_id = "one",
    adjustment_id = "state_main",
    adjustment = "State FE + main controls",
    construction_id = "nonzero_mean",
    construction = "Nonzero mean",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = character(),
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = "coverage",
    panel_variant = "primary",
    sample_rule = "support"
  )
  row2 <- row
  row2$specification_id <- "two"
  specs <- bind_iv_specification_rows(list(row, row2))
  expect_error(
    iv_specification_variables(specs),
    "single canonical IV specification"
  )
})

test_that("registered consumption IV dynamics share one specification sample across estimators", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  set.seed(503)
  n <- 96
  panel <- data.frame(
    y = rnorm(n), d = rnorm(n), z = rnorm(n), control = rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:8), each = 12),
    stringsAsFactors = FALSE
  )
  panel$d <- 0.7 * panel$z + 0.3 * panel$control + rnorm(n)
  panel$y <- 0.8 * panel$d + 0.2 * panel$control + rnorm(n)
  panel$y[c(4, 13, 51)] <- NA_real_
  rownames(panel) <- paste0("d_", seq_len(n))

  spec <- iv_specification_row(
    specification_id = "consumption__toy",
    adjustment_id = "state_main",
    adjustment = "State FE",
    construction_id = "nonzero_mean",
    construction = "Toy",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "control",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = NA_character_,
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    cluster = "state_code_2001"
  )
  spec$welfare_specification_id <- "toy"
  spec$welfare_outcome_id <- "real_mean_mpce"
  spec$outcome_round <- "hces_2023_24"
  spec$baseline_round <- "nss_2004_05"
  spec$estimand <- "ancova"
  spec$analysis_transform <- "log"

  out <- estimate_consumption_iv_dynamics(panel, spec, list(), ar_points = 31L)
  row <- out$summary

  expect_equal(nrow(row), 1L)
  expect_equal(row$first_stage_n, row$reduced_form_n)
  expect_equal(row$first_stage_n, row$second_stage_n)
  expect_equal(row$first_stage_n, row$n)
  expect_true(is.finite(row$reduced_form_estimate))
  expect_true(is.finite(row$second_stage_estimate))
  expect_true(is.finite(row$partial_f))
  expect_true(is.finite(row$anderson_rubin_p_beta0))
  expect_true(nrow(out$anderson_rubin_grid) > 0L)
})

test_that("consumption reduced forms preserve canonical controls and fixed effects", {
  skip_if_not_installed("sandwich")
  set.seed(504)
  n <- 72
  panel <- data.frame(
    y = rnorm(n), d = rnorm(n), z = rnorm(n), baseline = rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:6), each = 12),
    stringsAsFactors = FALSE
  )
  spec <- iv_specification_row(
    specification_id = "rf",
    adjustment_id = "state_main",
    adjustment = "State FE",
    construction_id = "nonzero_mean",
    construction = "Toy",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "baseline",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = NA_character_,
    panel_variant = "primary",
    sample_rule = "support",
    cluster = "state_code_2001"
  )

  out <- estimate_iv_reduced_form_spec(panel, spec, list())
  expect_equal(out$status, "estimated")
  expect_equal(out$n, n)
  expect_identical(out$term, "z")
  expect_true(all(is.finite(unlist(
    out[c("estimate", "std.error", "p.value")],
    use.names = FALSE
  ))))
})

test_that("dynamic consumption IV validation enforces the common-sample inference contract", {
  spec <- iv_specification_row(
    specification_id = "consumption__toy",
    adjustment_id = "state_main",
    adjustment = "State FE",
    construction_id = "nonzero_mean",
    construction = "Toy",
    outcome = "y",
    treatment = "d",
    fixed_effect = "state",
    controls = "control",
    included_language_controls = character(),
    excluded_instruments = "z",
    mapping_coverage_variable = NA_character_,
    panel_variant = "primary",
    sample_rule = "analysis_welfare_support",
    cluster = "state_code_2001"
  )
  spec$welfare_specification_id <- "toy"
  spec$welfare_outcome_id <- "real_mean_mpce"
  spec$outcome_round <- "hces_2023_24"
  spec$baseline_round <- "nss_2004_05"
  spec$estimand <- "ancova"
  spec$analysis_transform <- "log"

  summary <- data.frame(
    specification_id = "consumption__toy",
    first_stage_n = 50L,
    first_stage_status = "estimated",
    reduced_form_n = 50L,
    reduced_form_status = "estimated",
    second_stage_n = 50L,
    second_stage_status = "estimated",
    n = 50L,
    status = "estimated",
    partial_f = 4,
    effective_f = 3.5,
    effective_f_critical_value = 10,
    effective_f_p_value = 0.2,
    effective_f_df = 1,
    effective_f_status = "estimated",
    reduced_form_estimate = 0.1,
    reduced_form_std.error = 0.04,
    reduced_form_p.value = 0.02,
    second_stage_estimate = 0.2,
    second_stage_std.error = 0.1,
    second_stage_p.value = 0.05,
    anderson_rubin_p_beta0 = 0.03,
    stringsAsFactors = FALSE
  )
  dynamics <- list(
    summary = summary,
    anderson_rubin_grid = data.frame(
      specification_id = "consumption__toy",
      beta = c(-1, 0, 1),
      accepted = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  )

  out <- validate_consumption_iv_dynamics(dynamics, spec)
  expect_equal(out$summary$specification_id, "consumption__toy")

  dynamics$summary$reduced_form_n <- 49L
  expect_error(
    validate_consumption_iv_dynamics(dynamics, spec),
    "not analysis-ready"
  )
})

test_that("dynamic consumption IV validation rejects missing registered AR grids", {
  specs <- bind_iv_specification_rows(lapply(c("one", "two"), function(id) {
    row <- iv_specification_row(
      specification_id = id,
      adjustment_id = "state_main",
      adjustment = "State FE",
      construction_id = "nonzero_mean",
      construction = "Toy",
      outcome = "y",
      treatment = "d",
      fixed_effect = "state",
      controls = "control",
      included_language_controls = character(),
      excluded_instruments = "z",
      mapping_coverage_variable = NA_character_,
      panel_variant = "primary",
      sample_rule = "support",
      cluster = "state_code_2001"
    )
    row$welfare_specification_id <- id
    row$welfare_outcome_id <- "real_mean_mpce"
    row$outcome_round <- "hces_2023_24"
    row$baseline_round <- "nss_2004_05"
    row$estimand <- "ancova"
    row$analysis_transform <- "log"
    row
  }))

  summary <- data.frame(
    specification_id = c("one", "two"),
    first_stage_n = 50L,
    first_stage_status = "estimated",
    reduced_form_n = 50L,
    reduced_form_status = "estimated",
    second_stage_n = 50L,
    second_stage_status = "estimated",
    n = 50L,
    status = "estimated",
    partial_f = 4,
    effective_f = 3.5,
    effective_f_critical_value = 10,
    effective_f_p_value = 0.2,
    effective_f_df = 1,
    effective_f_status = "estimated",
    reduced_form_estimate = 0.1,
    reduced_form_std.error = 0.04,
    reduced_form_p.value = 0.02,
    second_stage_estimate = 0.2,
    second_stage_std.error = 0.1,
    second_stage_p.value = 0.05,
    anderson_rubin_p_beta0 = 0.03,
    stringsAsFactors = FALSE
  )
  dynamics <- list(
    summary = summary,
    anderson_rubin_grid = data.frame(
      specification_id = "one",
      beta = c(-1, 0, 1),
      accepted = c(TRUE, FALSE, TRUE),
      stringsAsFactors = FALSE
    )
  )

  expect_error(
    validate_consumption_iv_dynamics(dynamics, specs),
    "lack Anderson-Rubin grids"
  )
})

test_that("Anderson-Rubin acceptance components normalize list-column-like inputs", {
  grid <- data.frame(id = 1:7)
  grid$beta <- I(as.list(-3:3))
  grid$accepted <- I(as.list(c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)))

  components <- anderson_rubin_acceptance_components(grid)

  expect_equal(nrow(components), 2L)
  expect_type(components$lower, "double")
  expect_type(components$upper, "double")
  expect_equal(components$lower, c(-3, 2))
  expect_equal(components$upper, c(-2, 3))
  expect_false(any(components$contains_zero))
})

test_that("Anderson-Rubin acceptance grid rejects malformed public inputs", {
  expect_error(
    anderson_rubin_acceptance_components(
      data.frame(beta = c(-1, NA, 1), accepted = TRUE)
    ),
    "non-finite beta"
  )
  expect_error(
    anderson_rubin_acceptance_components(
      data.frame(beta = -1:1, accepted = c("TRUE", "maybe", "FALSE"))
    ),
    "invalid accepted flags"
  )
})

test_that("consumption distribution benchmark preserves serial/configured estimates", {
  set.seed(702)
  n <- 40
  x <- data.frame(
    survey_id = "wave", household_id = paste0("h", seq_len(n)),
    source_state_code = "01", sector = "Rural", subround = "1",
    fsu = as.character(seq_len(n)), stratum = "1", sub_stratum = "1",
    household_size = 1, target_unit_2001 = rep(c("a", "b"), each = n / 2),
    lineage_status = "resolved_exact_2001", lineage_weight = 1,
    lineage_person_weight = 1, real_mpce = exp(rnorm(n, log(200), .3)),
    stringsAsFactors = FALSE
  )
  registry <- data.frame(
    outcome_id = "bottom40", estimand = "survey_bottom_mean", transform = "identity",
    quantile = 0.4, quantile_interval = "", quantile_rule = "",
    role = "robustness", min_households = 1,
    min_fsu = 1, min_kish_effective_n = 1, max_relative_se = 10,
    survey_ids = "*", stringsAsFactors = FALSE
  )
  old <- Sys.getenv("EMI_CONSUMPTION_DOMAIN_CORES", unset = NA_character_)
  Sys.setenv(EMI_CONSUMPTION_DOMAIN_CORES = "1")
  on.exit({
    if (is.na(old)) Sys.unsetenv("EMI_CONSUMPTION_DOMAIN_CORES") else
      Sys.setenv(EMI_CONSUMPTION_DOMAIN_CORES = old)
  }, add = TRUE)
  out <- benchmark_consumption_distribution_domains(x, registry, max_districts = 2L)
  expect_equal(out$mode, c("serial", "configured"))
  expect_true(all(is.finite(out$elapsed_seconds)))
  expect_lte(out$max_abs_estimate_diff[[2L]], 1e-10)
  expect_lte(out$max_abs_se_diff[[2L]], 1e-10)
})

test_that("NSS-64 detailed consumption is gated by official reconstruction before welfare", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  targets_file <- paste(
    readLines(file.path(root, "_targets.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(targets_file, "consumption_mpce_validation_2007_08", fixed = TRUE)
  expect_match(targets_file, "consumption_households_real_2007_08", fixed = TRUE)
  expect_match(targets_file, "consumption_households_lineaged_2007_08", fixed = TRUE)
  expect_match(targets_file, "consumption_district_welfare_2007_08", fixed = TRUE)

  validation <- regexpr(
    "consumption_mpce_validation_2007_08",
    targets_file, fixed = TRUE
  )[[1L]]
  deflation <- regexpr(
    "consumption_households_real_2007_08",
    targets_file, fixed = TRUE
  )[[1L]]
  expect_gt(deflation, validation)
})

test_that("pretrend consumption rounds are validation-gated core welfare inputs", {
  targets_file <- paste(readLines(file.path(
    Sys.getenv("EMI_PROJECT_ROOT", unset = "."), "_targets.R"
  ), warn = FALSE), collapse = "\n")

  for (id in c("2000_01", "2001_02")) {
    expect_match(
      targets_file,
      paste0("consumption_mpce_validation_", id),
      fixed = TRUE
    )
    expect_match(
      targets_file,
      paste0("consumption_households_real_", id),
      fixed = TRUE
    )
    expect_match(
      targets_file,
      paste0("consumption_households_lineaged_", id),
      fixed = TRUE
    )
    expect_match(
      targets_file,
      paste0("consumption_district_welfare_core_", id),
      fixed = TRUE
    )
    expect_false(grepl(
      paste0("consumption_district_welfare_distributional_", id),
      targets_file,
      fixed = TRUE
    ))
  }
})

test_that("production CPI-IW history follows the registered consumption window", {
  targets_file <- paste(readLines(file.path(
    Sys.getenv("EMI_PROJECT_ROOT", unset = "."), "_targets.R"
  ), warn = FALSE), collapse = "\n")

  expect_match(
    targets_file,
    "cpi_iw_estimation_start = consumption_price_window$start_period[[1L]]",
    fixed = TRUE
  )
})

test_that("dynamic consumption IV estimation is stable across multiple registered specifications", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("car")

  set.seed(507)
  n <- 96L
  panel <- data.frame(
    z = rnorm(n),
    control = rnorm(n),
    state_code_2001 = rep(sprintf("%02d", 1:8), each = 12),
    stringsAsFactors = FALSE
  )
  panel$d <- 0.8 * panel$z + 0.2 * panel$control + rnorm(n)
  panel$baseline_a <- rnorm(n)
  panel$baseline_b <- rnorm(n)
  panel$y_a <- 0.5 * panel$d + 0.3 * panel$baseline_a + rnorm(n)
  panel$y_b <- 0.7 * panel$d + 0.3 * panel$baseline_b + rnorm(n)

  make_spec <- function(id, outcome, baseline) {
    spec <- iv_specification_row(
      specification_id = id,
      adjustment_id = "state_main",
      adjustment = "State FE",
      construction_id = "nonzero_mean",
      construction = "Toy",
      outcome = outcome,
      treatment = "d",
      fixed_effect = "state",
      controls = c("control", baseline),
      included_language_controls = character(),
      excluded_instruments = "z",
      mapping_coverage_variable = NA_character_,
      panel_variant = "primary",
      sample_rule = "analysis_welfare_support",
      cluster = "state_code_2001"
    )
    spec$welfare_specification_id <- sub("^consumption__", "", id)
    spec$welfare_outcome_id <- "real_mean_mpce"
    spec$outcome_round <- "toy_endpoint"
    spec$baseline_round <- "toy_baseline"
    spec$estimand <- "ancova"
    spec$analysis_transform <- "log"
    spec
  }

  specs <- bind_iv_specification_rows(list(
    make_spec("consumption__toy_a", "y_a", "baseline_a"),
    make_spec("consumption__toy_b", "y_b", "baseline_b")
  ))

  if (requireNamespace("sf", quietly = TRUE)) {
    panel$x_coord <- seq_len(nrow(panel))
    panel$y_coord <- 0
    panel <- sf::st_as_sf(
      panel,
      coords = c("x_coord", "y_coord"),
      crs = 4326
    )
  }

  out <- estimate_consumption_iv_dynamics(
    panel, specs, list(), ar_points = 31L
  )

  expect_identical(
    out$summary$specification_id,
    c("consumption__toy_a", "consumption__toy_b")
  )
  expect_equal(nrow(out$summary), 2L)
  expect_true(all(out$summary$first_stage_status == "estimated"))
  expect_true(all(out$summary$reduced_form_status == "estimated"))
  expect_true(all(out$summary$second_stage_status == "estimated"))
  expect_true(all(out$summary$status == "estimated"))
  expect_setequal(
    unique(out$anderson_rubin_grid$specification_id),
    out$summary$specification_id
  )
})

test_that("optional pretrend welfare rounds remain outside the causal IV registry", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  survey_registry <- read_consumption_survey_registry(build_paths(root))
  iv_registry <- read_consumption_iv_outcome_registry(
    file.path(root, "data", "metadata", "consumption_iv_outcomes.csv")
  )

  pretrend <- survey_registry$survey_id[
    survey_registry$analysis_role == "optional_pretrend"
  ]
  registered_rounds <- unique(c(
    plain_chr(iv_registry$outcome_round),
    plain_chr(iv_registry$baseline_round)
  ))

  expect_setequal(pretrend, c("nss_2000_01", "nss_2001_02"))
  expect_length(intersect(pretrend, registered_rounds), 0L)
})
