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
  out <- diagnose_ame_benchmark(list(), list(run_diagnostics = list(ame_benchmark = FALSE)))

  expect_equal(out$status, "skipped")
})

test_that("rendered PDF text checks use the system extractor contract", {
  skipped <- c("paper/paper.pdf", "paper/paper-new.pdf")

  expect_false(pdf_text_extractor_available(""))
  expect_true(is.na(extract_pdf_text(tempfile(fileext = ".pdf"), command = "")))
  expect_false(should_fail_pdf_text_skip(skipped, extractor_available = FALSE))
  expect_true(should_fail_pdf_text_skip(skipped, extractor_available = TRUE))
  expect_match(pdf_text_skip_message(skipped), "Poppler/pdftotext", fixed = TRUE)
  expect_match(pdf_text_failure_message(skipped), "pdftotext is available", fixed = TRUE)
})


test_that("rendered PDF layout checks recognize landscape pages", {
  info <- c(
    "Pages:           3",
    "Page    1 size:  612 x 792 pts (letter)",
    "Page    1 rot:   0",
    "Page    2 size:  612 x 792 pts (letter)",
    "Page    2 rot:   90",
    "Page    3 size:  792 x 612 pts (letter)",
    "Page    3 rot:   0"
  )
  layout <- parse_pdf_page_layout(info)

  expect_equal(layout$page, 1:3)
  expect_true(pdf_has_landscape_page(layout))
  expect_false(pdf_has_landscape_page(layout[1, , drop = FALSE]))
})

test_that("landscape requests are detected structurally", {
  path <- tempfile(fileext = ".qmd")
  writeLines(c("portrait", "::: {.landscape}", "content", ":::"), path)
  on.exit(unlink(path), add = TRUE)

  expect_true(source_requests_landscape(path))
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

test_that("report values read Moran estimates by contiguity definition", {
  diag <- data.frame(
    legacy_name = c("m_cons_resid", "m_cons_resid", "m_cons"),
    estimand = c("consumption_iv_residual", "consumption_iv_residual", "consumption_growth"),
    status = "estimated",
    contiguity = c("rook", "queen", "rook"),
    estimate = c(0.0026, 0.0031, 0.3071),
    p.value = c(0.4349, 0.4212, 5.736e-31),
    stringsAsFactors = FALSE
  )

  values <- build_report_values(data.frame(), data.frame(), list(), data.frame(), data.frame(), diag, list())

  expect_equal(values[["moran_iv_residual_i"]], signif(0.0026, 3))
  expect_equal(values[["moran_iv_residual_p"]], signif(0.4349, 3))
  expect_equal(values[["moran_iv_residual_p_queen"]], signif(0.4212, 3))
  expect_equal(values[["moran_consumption_growth_i"]], signif(0.3071, 3))
  expect_equal(values[["moran_consumption_growth_p"]], signif(5.736e-31, 3))
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

test_that("district matching saver preserves empty table schemas", {
  join_map <- data.frame(
    state_20 = character(), district_20 = character(),
    match_status = character(), stringsAsFactors = FALSE
  )
  out <- diagnose_district_matching(
    data.frame(state_20 = character(), district_20 = character()),
    join_map, list()
  )
  dir <- tempfile("district-matching-empty-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  save_district_matching_diagnostics(out, dir)

  for (file in c(
    "district_matching_unmatched_rows.csv",
    "district_matching_manual_matches.csv",
    "district_matching_many_to_many_cases.csv"
  )) {
    header <- names(utils::read.csv(file.path(dir, file), nrows = 0L, check.names = FALSE))
    expect_setequal(header, names(join_map))
  }
  inventory <- names(utils::read.csv(
    file.path(dir, "district_matching_source_key_inventory.csv"),
    nrows = 0L, check.names = FALSE
  ))
  expect_setequal(inventory, names(empty_source_key_inventory()))
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



test_that("fuzzy benchmark persists only benchmark-specific artifacts", {
  td_diag <- tempfile("fuzzy-diagnostics-")
  td_bench <- tempfile("fuzzy-benchmark-")
  tracker <- data.frame(
    district_01 = "Old Name",
    district_07 = "New Name",
    district_17 = "New Name",
    district_20 = "Newest Name"
  )
  diagnostics <- diagnose_fuzzy_matching(tracker, data.frame(), list())
  diagnostic_manifest <- save_fuzzy_matching_diagnostics(diagnostics, td_diag)
  benchmark_manifest <- save_fuzzy_matching_benchmark(
    data.frame(method = "jw", threshold = 0.15), td_bench
  )

  expect_setequal(
    basename(diagnostic_manifest$path),
    c(
      "fuzzy_matching_summary.csv", "fuzzy_matching_legacy_methods.csv",
      "fuzzy_matching_troublesome_pairs.csv", "fuzzy_matching_candidate_pairs.csv",
      "fuzzy_matching_join_status_counts.csv",
      "fuzzy_matching_candidate_pair_coverage.csv",
      "fuzzy_matching_legacy_tuning_reference.csv"
    )
  )
  expect_identical(
    basename(benchmark_manifest$path),
    "fuzzy_matching_threshold_sensitivity.csv"
  )
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
  expect_true(summary$ar_95_information[[1]] %in% c(
    "empty_acceptance_set", "zero_included", "positive_sign_only",
    "negative_sign_only", "zero_excluded_both_signs", "zero_excluded_unclassified"
  ))
  expect_identical(
    summary$ar_95_sign_identified[[1]],
    summary$ar_95_information[[1]] %in% c("positive_sign_only", "negative_sign_only")
  )
})

test_that("public Anderson-Rubin diagnostics cover both candidate main designs", {
  specs <- candidate_iv_diagnostic_specifications()
  expect_equal(nrow(specs), 2L)
  expect_identical(
    specs$adjustment_id,
    iv_candidate_design_adjustments()
  )
  expect_true(all(specs$construction_id == "nonzero_mean"))
  expect_true(all(specs$outcome == "real_log_consumption_change"))
  expect_true(all(specs$treatment == preferred_iv_variables()$treatment))
  expect_true(all(vapply(
    specs$excluded_instruments,
    identical,
    logical(1),
    preferred_iv_variables()$instrument
  )))
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


test_that("spatial connectivity recognizes expected offshore island components", {
  ledger <- data.frame(
    n_neighbors = c(4L, 0L, 0L, 0L),
    district_panel_id = c(
      "2001__01__01", "2001__31__01", "2001__35__01", "2001__35__02"
    ),
    stringsAsFactors = FALSE
  )

  expect_true(spatial_expected_offshore_islands(ledger, n_subgraphs = 4L))
  ledger$district_panel_id[[2L]] <- "2001__09__01"
  expect_false(spatial_expected_offshore_islands(ledger, n_subgraphs = 4L))
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

test_that("Shastry child-population diagnostic registers two paired comparison cells", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  controls <- read_census_2001_control_registry(
    file.path(root, "data", "metadata", "census_2001_control_registry.csv")
  )
  specs <- iv_child_population_first_stage_specifications(control_registry = controls)
  expect_equal(nrow(specs), 2L)
  expect_setequal(specs$fixed_effect, c("none", "region"))
  expect_true(all(vapply(
    specs$controls, function(x) shastry_child_population_variable() %in% x, logical(1)
  )))
  expect_true(all(specs$sample_rule == "child_population_first_stage_common_support"))
})


test_that("Anderson-Rubin topology classifies the information that survives weak identification", {
  component <- function(lower, upper, zero = FALSE) data.frame(
    component = seq_along(lower), lower = lower, upper = upper,
    touches_left_grid_edge = FALSE, touches_right_grid_edge = FALSE,
    contains_zero = zero, stringsAsFactors = FALSE
  )
  expect_identical(classify_anderson_rubin_information(data.frame()), "empty_acceptance_set")
  expect_identical(
    classify_anderson_rubin_information(component(-1, 1, TRUE)), "zero_included"
  )
  expect_identical(
    classify_anderson_rubin_information(component(.1, 2)), "positive_sign_only"
  )
  expect_identical(
    classify_anderson_rubin_information(component(-2, -.1)), "negative_sign_only"
  )
  expect_identical(
    classify_anderson_rubin_information(component(c(-2, .1), c(-.1, 2))),
    "zero_excluded_both_signs"
  )
})

test_that("named spatial residual diagnostics do not relabel another IV model", {
  consumption <- structure(list(), class = "ivreg")
  models <- list(consumption = consumption)

  expect_identical(spatial_iv_model(models, "consumption"), consumption)
  expect_null(spatial_iv_model(models, "gini"))
})
