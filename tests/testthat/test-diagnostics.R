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

test_that("rendered PDF text checks skip only when no extractor is available", {
  skipped <- c("paper/report.pdf", "docs/district-matching.pdf")

  expect_false(should_fail_pdf_text_skip(skipped, extractor_available = FALSE))
  expect_true(should_fail_pdf_text_skip(skipped, extractor_available = TRUE))
  expect_match(pdf_text_skip_message(skipped), "PDF text extractor unavailable")
  expect_match(pdf_text_failure_message(skipped), "PDF text extraction failed")
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
    neighbor_counts = lengths(nb),
    n = 4L,
    n_islands = 0L,
    mean_neighbors = mean(lengths(nb)),
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

test_that("tracker diagnostics include legacy source QA tables", {
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

test_that("fuzzy diagnostics use legacy methods and troublesome pairs", {
  testthat::skip_if_not_installed("stringdist")
  out <- diagnose_fuzzy_matching(data.frame(id = 1), data.frame(match_status = "harmonization_crosswalk_row"), list())

  expect_s3_class(out, "emi_fuzzy_matching_diagnostics")
  expect_equal(attr(out, "legacy_methods")$method, c("soundex", "qgram", "jw", "dl", "osa"))
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

test_that("tracker diagnostics preserve legacy comment benchmarks", {
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
  expect_true(nrow(attr(out, "legacy_reference")) >= 3L)
  expect_equal(nrow(attr(out, "legacy_expected_state_changes")), 2L)
  expect_equal(attr(out, "legacy_expected_inperiod_district_changes")$legacy_expected_rows[[1]], 16L)
  expect_equal(attr(out, "legacy_expected_same_name_districts")$legacy_expected_min_districts[[1]], 6L)
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

test_that("spatial weights diagnostics include legacy neighbor-count reference", {
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
    EMIE = c(0, 10, 80),
    wavg_ling_degrees = c(0, 1, 5),
    region = c("North", "North", "South")
  )

  out <- diagnose_instrument_exploration(panel, list())

  expect_true(is.list(out))
  expect_equal(nrow(out$dotplot_data), 3L)
  expect_true(all(c("district_order", "district_code", "EMIE", "state_prefix") %in% names(out$dotplot_data)))
  expect_true(nrow(out$legacy_notes) >= 3L)
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

  expect_true(all(c("year", "n_same_name_districts", "n_same_name_district_names", "within_legacy_range") %in% names(out)))
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
    EMIE = c(1, 2),
    wavg_ling_degrees = c(3, 4),
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
