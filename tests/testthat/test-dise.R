test_that("DISE metadata registries are unique and baseline reports decode ordered slots", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  registry <- read.csv(file.path(root, "data", "metadata", "dise_archive_registry.csv"), stringsAsFactors = FALSE)
  crosswalk <- read.csv(file.path(root, "data", "metadata", "dise_medium_slot_crosswalk.csv"), stringsAsFactors = FALSE)

  expect_equal(anyDuplicated(registry$academic_year), 0L)
  expect_setequal(
    registry$academic_year[registry$analytic_role == "baseline_treatment"],
    c("2005-06", "2006-07", "2007-08")
  )
  expect_true(all(crosswalk$medium_slot %in% 1:5))
  expect_true(all(c("English", "Hindi") %in% crosswalk$language_label))
  key <- with(crosswalk, paste(academic_year, state_report, district_report, medium_slot, sep = "|"))
  expect_equal(anyDuplicated(key), 0L)

  kupwara <- crosswalk[
    crosswalk$academic_year == "2005-06" &
      grepl("KUPWARA", crosswalk$district_report, fixed = TRUE),
    , drop = FALSE
  ]
  expect_identical(kupwara$language_label[match(1:3, kupwara$medium_slot)], c("English", "Others", "Urdu"))
})

test_that("DISE machine-name repair recovers ordered medium blocks from duplicated archival headers", {
  names <- c(
    "statecd", "distcd",
    paste0("enr_med1_", 1:5),
    paste0("enr_med2_", 1:5),
    paste0("enr_med2_", 1:5),
    paste0("enr_med4_", 1:5),
    paste0("enr_med5_", 1:5)
  )

  repaired <- repair_dise_machine_names(names)

  expect_identical(
    repaired[13:17],
    paste0("enr_med3_", 1:5)
  )
  expect_equal(anyDuplicated(repaired), 0L)
})

test_that("DISE count extraction preserves denominator and medium-slot identities", {
  data <- data.frame(
    statecd = "01", statename = "JAMMU & KASHMIR",
    distcd = "0101", distname = "KUPWARA",
    enr_govt1 = 70, enr_pvt1 = 30, enr_govt9 = 0, enr_pvt9 = 0,
    enr_cy_c1 = 55, enr_cy_c2 = 45,
    enr_med1_1 = 60, enr_med1_2 = 20, enr_med2_1 = 10, enr_med2_2 = 5,
    enr_med3_1 = 3, enr_med3_2 = 2,
    stringsAsFactors = FALSE
  )
  extracted <- extract_dise_enrollment_measures(data, "2005-06")
  expect_equal(extracted$dise_total_enrollment, 100)
  expect_equal(extracted$dise_management_enrollment, 100)
  expect_equal(extracted$dise_management_enrollment_difference, 0)
  expect_equal(extracted$dise_private_enrollment_share, 30)
  expect_equal(extracted$dise_medium_slot_1_enrollment, 80)
  expect_equal(extracted$dise_medium_slot_3_enrollment, 5)
  expect_equal(extracted$dise_medium_classified_enrollment, 100)
  expect_equal(extracted$dise_medium_classification_ratio, 100)

  crosswalk <- data.frame(
    academic_year = rep("2005-06", 3),
    state_report = rep("JAMMU & KASHMIR", 3),
    district_report = rep("KUPWARA", 3),
    medium_slot = 1:3,
    language_label = c("English", "Others", "Urdu"),
    stringsAsFactors = FALSE
  )
  labeled <- attach_dise_medium_identities(extracted, crosswalk)
  expect_true(labeled$dise_medium_identity_complete)
  expect_equal(labeled$dise_english_enrollment, 80)
  expect_equal(labeled$dise_hindi_enrollment, 0)
  expect_equal(labeled$dise_emi_enrollment_share_total, 80)
})

test_that("DISE percentage constructs use the same 0-100 scale as NSS EMI exposure", {
  data <- data.frame(
    statecd = "01", statename = "State", distcd = "0101", distname = "District",
    enr_govt1 = 75, enr_pvt1 = 25,
    enr_cy_c1 = 60, enr_cy_c2 = 40,
    enr_med1_1 = 40, enr_med2_1 = 60,
    stringsAsFactors = FALSE
  )
  extracted <- extract_dise_enrollment_measures(data, "2007-08")
  crosswalk <- data.frame(
    academic_year = "2007-08", state_report = "State", district_report = "District",
    medium_slot = 1:2, language_label = c("English", "Hindi"),
    stringsAsFactors = FALSE
  )
  labeled <- attach_dise_medium_identities(extracted, crosswalk)

  expect_equal(extracted$dise_private_enrollment_share, 25)
  expect_equal(extracted$dise_medium_classification_ratio, 100)
  expect_equal(labeled$dise_emi_enrollment_share_total, 40)
  expect_equal(labeled$dise_hindi_enrollment_share_total, 60)
  expect_equal(labeled$dise_english_share_english_hindi, 40)
})

test_that("medium-classification totals are diagnostics, never alternative denominators", {
  data <- data.frame(
    statecd = "01", statename = "State", distcd = "0101", distname = "District",
    enr_govt1 = 75, enr_pvt1 = 25,
    enr_cy_c1 = 60, enr_cy_c2 = 40,
    enr_med1_1 = 40, enr_med2_1 = 80,
    stringsAsFactors = FALSE
  )
  extracted <- extract_dise_enrollment_measures(data, "2007-08")
  crosswalk <- data.frame(
    academic_year = "2007-08", state_report = "State", district_report = "District",
    medium_slot = 1:2, language_label = c("English", "Hindi"),
    stringsAsFactors = FALSE
  )
  labeled <- attach_dise_medium_identities(extracted, crosswalk)

  expect_equal(extracted$dise_total_enrollment, 100)
  expect_equal(extracted$dise_medium_classified_enrollment, 120)
  expect_equal(extracted$dise_medium_classification_ratio, 120)
  expect_equal(labeled$dise_emi_enrollment_share_total, 40)
  expect_false("dise_emi_enrollment_share_reported" %in% names(labeled))
})

test_that("known English remains usable when an unrelated positive medium is unresolved", {
  data <- data.frame(
    academic_year = "2007-08",
    state_name_dise = "Example State", district_name_dise = "Example District",
    state_code_dise = "99", district_code_dise = "9901",
    dise_total_enrollment = 100, dise_private_enrollment_share = 20,
    dise_medium_slot_1_enrollment = 80, dise_medium_slot_2_enrollment = 20,
    dise_medium_slot_3_enrollment = 0, dise_medium_slot_4_enrollment = 0,
    dise_medium_slot_5_enrollment = 0, stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    academic_year = "2007-08", state_report = "Example State",
    district_report = "Example District", medium_slot = 1,
    language_label = "English", stringsAsFactors = FALSE
  )

  out <- attach_dise_medium_identities(data, crosswalk)

  expect_false(out$dise_medium_identity_complete)
  expect_true(out$dise_english_identity_resolved)
  expect_false(out$dise_hindi_identity_resolved)
  expect_equal(out$dise_english_enrollment, 80)
  expect_equal(out$dise_emi_enrollment_share_total, 80)
  expect_true(is.na(out$dise_hindi_enrollment_share_total))
})

test_that("unknown positive DISE medium slots never become zero English enrollment", {
  data <- data.frame(
    academic_year = "2007-08",
    state_name_dise = "Example State", district_name_dise = "Example District",
    state_code_dise = "99", district_code_dise = "9901",
    dise_total_enrollment = 100, dise_private_enrollment_share = 20,
    dise_medium_slot_1_enrollment = 80, dise_medium_slot_2_enrollment = 20,
    dise_medium_slot_3_enrollment = 0, dise_medium_slot_4_enrollment = 0,
    dise_medium_slot_5_enrollment = 0, stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    academic_year = "2007-08", state_report = "Example State",
    district_report = "Example District", medium_slot = 1,
    language_label = "Hindi", stringsAsFactors = FALSE
  )
  out <- attach_dise_medium_identities(data, crosswalk)
  expect_false(out$dise_medium_identity_complete)
  expect_false(out$dise_english_identity_resolved)
  expect_true(is.na(out$dise_english_enrollment))
  expect_true(is.na(out$dise_emi_enrollment_share_total))
})

test_that("DISE treatment construction does not require parser-only QA columns", {
  years <- c("2005-06", "2006-07", "2007-08")
  data <- data.frame(
    academic_year = years,
    state_name_dise = "State", district_name_dise = "District",
    state_code_dise = "01", district_code_dise = "0101",
    dise_total_enrollment = c(100, 100, 100),
    dise_private_enrollment_share = c(10, 10, 10),
    dise_medium_slot_1_enrollment = c(20, 30, 40),
    dise_medium_slot_2_enrollment = c(80, 70, 60),
    dise_medium_slot_3_enrollment = 0,
    dise_medium_slot_4_enrollment = 0,
    dise_medium_slot_5_enrollment = 0,
    dise_government_schools = 9,
    dise_private_schools = 1,
    dise_total_schools = 10,
    dise_private_school_share = 10,
    stringsAsFactors = FALSE
  )
  crosswalk <- do.call(rbind, lapply(years, function(year) data.frame(
    academic_year = year,
    state_report = "State",
    district_report = "District",
    medium_slot = 1:2,
    language_label = c("English", "Hindi"),
    stringsAsFactors = FALSE
  )))

  out <- build_dise_baseline_treatments(data, crosswalk)

  expect_equal(out$dise_emi_enrollment_share_total_0708, 40)
  expect_equal(out$dise_emi_enrollment_share_total_0508_pooled, 30)
  expect_false("dise_medium_classification_ratio_0708" %in% names(out))
})

test_that("pooled DISE EMI is a ratio of pooled counts and requires all three baseline years", {
  years <- c("2005-06", "2006-07", "2007-08")
  data <- data.frame(
    academic_year = years,
    state_name_dise = "State", district_name_dise = "District",
    state_code_dise = "01", district_code_dise = "0101",
    dise_government_enrollment = c(90, 180, 270),
    dise_private_enrollment = c(10, 20, 30),
    dise_total_enrollment = c(100, 200, 300),
    dise_private_enrollment_share = c(10, 10, 10),
    dise_medium_slot_1_enrollment = c(20, 80, 180),
    dise_medium_slot_2_enrollment = c(80, 120, 120),
    dise_medium_slot_3_enrollment = 0, dise_medium_slot_4_enrollment = 0,
    dise_medium_slot_5_enrollment = 0,
    dise_government_schools = 9, dise_private_schools = 1,
    dise_total_schools = 10, dise_private_school_share = 10,
    stringsAsFactors = FALSE
  )
  crosswalk <- do.call(rbind, lapply(years, function(year) data.frame(
    academic_year = year, state_report = "State", district_report = "District",
    medium_slot = 1:2, language_label = c("English", "Hindi"), stringsAsFactors = FALSE
  )))
  out <- build_dise_baseline_treatments(data, crosswalk)
  expect_equal(out$dise_baseline_years_observed, 3L)
  expect_equal(out$dise_emi_enrollment_share_total_0508_pooled, 100 * 280 / 600)
  expect_false("dise_medium_classification_ratio_0708" %in% names(out))
  expect_false(isTRUE(all.equal(out$dise_emi_enrollment_share_total_0508_pooled, mean(c(20, 40, 60)))))
})

test_that("DISE construct registry separates treatments from mechanism outcomes", {
  registry <- dise_construct_registry()
  structural <- registry[registry$analysis_scope == "structural_iv", , drop = FALSE]
  relevance <- registry[registry$analysis_scope == "relevance_only", , drop = FALSE]
  expect_equal(nrow(structural), 4L)
  expect_true(all(grepl("dise_emi_", structural$variable, fixed = TRUE)))
  expect_setequal(
    structural$construct_id,
    c(
      "emi_total_0708", "emi_total_0508_pooled",
      "emi_age6_13_gross_0708", "emi_age6_13_gross_0508_pooled"
    )
  )
  expect_setequal(
    relevance$construct_id,
    c("hindi_share_0708", "english_hindi_share_0708", "private_enrollment_share_0708", "private_school_share_0708")
  )
  expect_false(any(grepl("reported", registry$construct_id, fixed = TRUE)))
})



test_that("DISE IV diagnostic projection is insensitive to unrelated future outcomes", {
  constructs <- dise_construct_registry()
  validation <- dise_nss_validation_registry()
  needed_numeric <- unique(c(
    "real_log_consumption_change",
    constructs$variable,
    validation$dise_variable,
    validation$nss_variable,
    census_2001_diagnostic_controls(),
    alternative_distance_variables()
  ))
  panel <- data.frame(
    state_code_2001 = c("01", "02"),
    district_code_2001 = c("001", "002"),
    region = c("north", "south"),
    stringsAsFactors = FALSE
  )
  for (variable in setdiff(
    needed_numeric,
    c("state_code_2001", "district_code_2001", "region")
  )) {
    panel[[variable]] <- c(1, 2)
  }
  panel$future_hces_outcome <- c(100, 200)

  projected <- prepare_dise_iv_diagnostic_panel(panel, constructs)

  expect_false("future_hces_outcome" %in% names(projected))
  expect_true(all(constructs$variable %in% names(projected)))
  expect_true("real_log_consumption_change" %in% names(projected))
})

test_that("DISE-NSS validation compares like-scaled enrolled measures and residualizes states", {
  panel <- data.frame(
    state_code_2001 = rep(c("01", "02"), each = 3),
    dise_emi_enrollment_share_total_0708 = c(10, 20, 30, 40, 50, 60),
    emi_share_enrolled_0708 = c(12, 18, 31, 39, 52, 58),
    emi_exposure_all_children_0708 = c(8, 15, 25, 30, 42, 48),
    stringsAsFactors = FALSE
  )
  out <- diagnose_dise_nss_validation(panel)
  enrolled <- out[out$comparison == "enrolled_total_denominator", , drop = FALSE]

  expect_equal(nrow(out), 2L)
  expect_identical(enrolled$status[[1]], "estimated")
  expect_equal(enrolled$n[[1]], 6L)
  expect_gt(enrolled$pearson[[1]], 0.9)
  expect_true(is.finite(enrolled$state_residual_pearson[[1]]))
  expect_equal(enrolled$mean_difference[[1]], mean(
    panel$dise_emi_enrollment_share_total_0708 - panel$emi_share_enrolled_0708
  ))
})

test_that("alternative-distance registry carries the requested treatment through every permutation", {
  registry <- alternative_distance_registry(treatment = "dise_emi_enrollment_share_total_0708")
  expect_true(all(registry$treatment == "dise_emi_enrollment_share_total_0708"))
  expect_setequal(unique(registry$fixed_effect), c("none", "region", "state"))
  expect_setequal(unique(registry$construction_id), names(iv_instrument_constructions()))
})


test_that("DISE publication-check metadata references parsed DISE metric names", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  checks <- read.csv(
    file.path(root, "data", "metadata", "dise_publication_checks.csv"),
    stringsAsFactors = FALSE
  )
  expect_true(all(startsWith(checks$metric, "dise_")))
  expect_setequal(
    checks$metric,
    paste0("dise_medium_slot_", 1:3, "_enrollment")
  )
})

test_that("DISE publication checks compare parsed raw counts rather than replacing them", {
  district_year <- data.frame(
    academic_year = "2005-06", state_name_dise = "JAMMU & KASHMIR",
    district_name_dise = "KUPWARA", dise_medium_slot_1_enrollment = 92642,
    stringsAsFactors = FALSE
  )
  checks <- data.frame(
    academic_year = "2005-06", state = "JAMMU & KASHMIR", district = "KUPWARA",
    metric = "dise_medium_slot_1_enrollment", expected_value = 92642,
    source_pdf = "report.pdf", source_page = 1, note = "anchor",
    stringsAsFactors = FALSE
  )
  out <- dise_publication_check_values(district_year, checks)
  expect_true(out$matches)
  expect_equal(out$difference, 0)
  expect_equal(district_year$dise_medium_slot_1_enrollment, 92642)
})

test_that("DISE diagnostic saver returns the repository-standard output manifest", {
  empty <- data.frame()
  archive <- list(
    year_summary = empty,
    treatment_summary = empty,
    publication_checks = empty
  )
  permutations <- list(
    construct_registry = empty,
    nss_validation = empty,
    first_stage = empty,
    first_stage_coefficients = empty,
    weak_iv_outcomes = empty,
    anderson_rubin_grid = empty,
    overidentification = empty,
    monotonicity_summary = empty,
    monotonicity_bins = empty,
    monotonicity_state_slopes = empty,
    balance = empty,
    joint_balance = empty
  )
  dir <- tempfile("dise-diagnostics-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  manifest <- save_dise_diagnostics(archive, permutations, empty, empty, dir = dir)

  expect_s3_class(manifest, "data.frame")
  expect_setequal(names(manifest), c("path", "description"))
  expect_equal(nrow(manifest), 32L)
  expect_true(all(file.exists(manifest$path)))
  expect_setequal(
    basename(manifest$path)[grepl("age_6_13|elementary_age_exposure", basename(manifest$path))],
    c(
      "census_age_6_13_anchors_2001_2011.csv",
      "census_age_6_13_population_by_academic_year.csv",
      "dise_elementary_age_exposure_dynamic_summary.csv",
      "dise_elementary_age_exposure_dynamic_event_study.csv"
    )
  )
  expect_setequal(
    basename(manifest$path)[grepl("dise_school_quality_", basename(manifest$path))],
    c(
      "dise_school_quality_registry.csv",
      "dise_school_quality_baseline_association.csv",
      "dise_school_quality_report_2001.csv",
      "dise_school_quality_dynamic_summary.csv",
      "dise_school_quality_dynamic_event_study.csv"
    )
  )
})

test_that("DISE lineage candidate constructors preserve empty schemas", {
  admin <- data.frame(
    unit_id = character(),
    state_std = character(),
    district_std = character(),
    stringsAsFactors = FALSE
  )
  roster <- data.frame(
    source_row_id = character(),
    state_std = character(),
    district_std = character(),
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_row_id = character(),
    target_unit_2001 = character(),
    weight = numeric(),
    panel_variant = character(),
    stringsAsFactors = FALSE
  )

  admin_candidates <- dise_admin_2001_candidates(admin)
  reviewed_candidates <- dise_reviewed_lineage_candidates(roster, reviewed)

  expect_equal(nrow(admin_candidates), 0L)
  expect_equal(nrow(reviewed_candidates), 0L)
  expect_identical(
    names(admin_candidates),
    c("state_key", "district_key", "target_unit_2001", "bridge_source")
  )
  expect_identical(names(reviewed_candidates), names(admin_candidates))
})

test_that("DISE lineage bridge permits absent candidate sources", {
  dise <- data.frame(
    academic_year = "2007-08",
    state_name_dise = "State",
    district_name_dise = "Unmatched",
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    unit_id = character(),
    state_std = character(),
    district_std = character(),
    stringsAsFactors = FALSE
  )
  roster <- data.frame(
    source_row_id = character(),
    state_std = character(),
    district_std = character(),
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_row_id = character(),
    target_unit_2001 = character(),
    weight = numeric(),
    panel_variant = character(),
    stringsAsFactors = FALSE
  )

  bridge <- build_dise_deterministic_lineage_bridge(dise, roster, reviewed, admin)

  expect_equal(nrow(bridge), 1L)
  expect_true(is.na(bridge$target_unit_2001[[1]]))
  expect_equal(bridge$n_candidate_targets[[1]], 0L)
  expect_identical(
    bridge$bridge_status[[1]],
    "unresolved_no_deterministic_lineage"
  )
})

test_that("DISE deterministic lineage bridge accepts only one weight-one Census-2001 target", {
  dise <- data.frame(
    academic_year = c("2007-08", "2007-08", "2007-08"),
    state_name_dise = "State",
    district_name_dise = c("Parent", "Child", "Ambiguous"),
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    unit_id = "pc2001__01__01",
    state_std = "State",
    district_std = "Parent",
    stringsAsFactors = FALSE
  )
  roster <- data.frame(
    source_row_id = c("r1", "r2", "r3"),
    state_std = "State",
    district_std = c("Child", "Ambiguous", "Ambiguous"),
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_row_id = c("r1", "r2", "r3"),
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__01", "pc2001__01__02"),
    weight = 1,
    panel_variant = "deterministic",
    stringsAsFactors = FALSE
  )

  bridge <- build_dise_deterministic_lineage_bridge(dise, roster, reviewed, admin)
  expect_identical(
    bridge$target_unit_2001[bridge$district_key == "parent"][[1]],
    "pc2001__01__01"
  )
  expect_identical(
    bridge$target_unit_2001[bridge$district_key == "child"][[1]],
    "pc2001__01__01"
  )
  ambiguous <- bridge[bridge$district_key == "ambiguous", , drop = FALSE]
  expect_true(is.na(ambiguous$target_unit_2001[[1]]))
  expect_identical(ambiguous$bridge_status[[1]], "ambiguous_reviewed_lineage")
})

test_that("DISE harmonization sums child counts before recomputing EMI", {
  district_year <- data.frame(
    academic_year = c("2007-08", "2007-08"),
    state_name_dise = "State",
    district_name_dise = c("Child A", "Child B"),
    dise_english_enrollment = c(20, 80),
    dise_hindi_enrollment = c(80, 20),
    dise_total_enrollment = c(100, 300),
    dise_government_enrollment = c(80, 210),
    dise_private_enrollment = c(20, 90),
    dise_government_schools = c(8, 21),
    dise_private_schools = c(2, 9),
    dise_total_schools = c(10, 30),
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state",
    district_key = c("child a", "child b"),
    target_unit_2001 = "pc2001__01__01",
    n_candidate_targets = 1L,
    bridge_status = "deterministic_to_2001",
    bridge_sources = "reviewed",
    stringsAsFactors = FALSE
  )

  out <- harmonize_dise_counts_to_2001(district_year, bridge)
  expect_equal(nrow(out), 1L)
  expect_equal(out$dise_source_district_count, 2L)
  expect_equal(out$dise_english_enrollment, 100)
  expect_equal(out$dise_total_enrollment, 400)
  expect_equal(out$dise_emi_enrollment_share_total, 25)
  expect_false(isTRUE(all.equal(out$dise_emi_enrollment_share_total, mean(c(20, 100 * 80 / 300)))))
})

test_that("DISE harmonization reapplies language-count validity after aggregation", {
  district_year <- data.frame(
    academic_year = "2010-11",
    state_name_dise = "State",
    district_name_dise = "District",
    dise_english_enrollment = 120,
    dise_hindi_enrollment = 10,
    dise_total_enrollment = 100,
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state",
    district_key = "district",
    target_unit_2001 = "pc2001__01__01",
    n_candidate_targets = 1L,
    bridge_status = "deterministic_to_2001",
    bridge_sources = "reviewed",
    stringsAsFactors = FALSE
  )

  out <- harmonize_dise_counts_to_2001(district_year, bridge)

  expect_equal(out$dise_english_enrollment, 120)
  expect_false(out$dise_english_count_valid)
  expect_false(out$dise_english_identity_resolved)
  expect_true(is.na(out$dise_emi_enrollment_share_total))
})

test_that("DISE harmonization never uses fractional reviewed lineage weights", {
  dise <- data.frame(
    academic_year = "2007-08",
    state_name_dise = "State",
    district_name_dise = "Split",
    stringsAsFactors = FALSE
  )
  roster <- data.frame(
    source_row_id = "r1",
    state_std = "State",
    district_std = "Split",
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_row_id = c("r1", "r1"),
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    weight = c(0.6, 0.4),
    panel_variant = "population_allocation",
    stringsAsFactors = FALSE
  )
  admin <- data.frame(
    unit_id = character(), state_std = character(), district_std = character(),
    stringsAsFactors = FALSE
  )

  bridge <- build_dise_deterministic_lineage_bridge(dise, roster, reviewed, admin)
  expect_true(is.na(bridge$target_unit_2001[[1]]))
  expect_identical(bridge$bridge_status[[1]], "unresolved_no_deterministic_lineage")
})

test_that("Census-2001 DISE baseline pooling preserves annual count aggregation", {
  years <- c("2005-06", "2006-07", "2007-08")
  x <- data.frame(
    academic_year = years,
    target_unit_2001 = "pc2001__01__01",
    dise_source_district_count = c(1L, 2L, 2L),
    dise_english_enrollment = c(20, 80, 180),
    dise_hindi_enrollment = c(80, 120, 120),
    dise_total_enrollment = c(100, 200, 300),
    dise_english_identity_resolved = TRUE,
    dise_hindi_identity_resolved = TRUE,
    dise_emi_enrollment_share_total = c(20, 40, 60),
    dise_hindi_enrollment_share_total = c(80, 60, 40),
    dise_english_share_english_hindi = c(20, 40, 60),
    dise_private_enrollment_share = 10,
    dise_private_school_share = 10,
    stringsAsFactors = FALSE
  )

  out <- build_dise_baseline_treatments_2001(x)
  expect_identical(out$target_unit_2001[[1]], "pc2001__01__01")
  expect_equal(out$dise_source_district_count_0708, 2L)
  expect_equal(out$dise_emi_enrollment_share_total_0708, 60)
  expect_equal(out$dise_baseline_years_observed, 3L)
  expect_equal(out$dise_emi_enrollment_share_total_0508_pooled, 100 * 280 / 600)
})

test_that("Census-2001 DISE attachment is one-to-one and preserves panel order", {
  panel <- data.frame(
    target_unit_2001 = c("pc2001__01__02", "pc2001__01__01"),
    y = c(2, 1),
    stringsAsFactors = FALSE
  )
  treatments <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    dise_emi_enrollment_share_total_0708 = c(10, 20),
    stringsAsFactors = FALSE
  )
  out <- attach_dise_treatments_to_panel_2001(panel, treatments)
  expect_identical(out$y, c(2, 1))
  expect_identical(out$dise_emi_enrollment_share_total_0708, c(20, 10))
})

test_that("DISE metadata inputs are explicit file dependencies in the targets graph", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  targets_text <- paste(
    readLines(file.path(root, "_targets.R"), warn = FALSE),
    collapse = "\n"
  )
  metadata_targets <- c(
    "dise_archive_registry_file",
    "dise_medium_slot_crosswalk_file",
    "dise_publication_checks_file",
    "dise_report_language_enrollment_file"
  )

  for (target in metadata_targets) {
    expect_match(targets_text, target, fixed = TRUE)
  }
  expect_gte(
    lengths(regmatches(targets_text, gregexpr('format = "file"', targets_text, fixed = TRUE))),
    4L
  )
})

test_that("DISE baseline treatment reader does not depend on Teacher sheets", {
  body_text <- paste(deparse(body(read_dise_baseline_year)), collapse = "\n")
  expect_false(grepl("teacher_sheet", body_text, fixed = TRUE))
  expect_false(grepl("extract_dise_teacher_measures", body_text, fixed = TRUE))
})

test_that("DISE school-quality extraction preserves additive counts", {
  school <- data.frame(
    statecd = "01", distcd = "0101",
    schgovt1 = 8, schpvt1 = 2,
    tch1_school = 2, gtoilet_sch = 7,
    stringsAsFactors = FALSE
  )
  teacher <- data.frame(
    distcd = "0101", tch_govt1 = 9, tch_pvt1 = 1,
    stringsAsFactors = FALSE
  )
  quality <- finalize_dise_school_quality_measures(cbind(
    data.frame(dise_total_enrollment = 200),
    extract_dise_school_measures(school),
    extract_dise_teacher_measures(teacher)["dise_total_teachers"]
  ))

  expect_equal(quality$dise_total_schools, 10)
  expect_equal(quality$dise_total_teachers, 10)
  expect_equal(quality$dise_pupils_per_teacher, 20)
  expect_equal(quality$dise_single_teacher_school_share, 20)
  expect_equal(quality$dise_girls_toilet_school_share, 70)
})

test_that("DISE school-quality ratios are recomputed after lineage aggregation", {
  district_year <- data.frame(
    academic_year = c("2007-08", "2007-08"),
    state_name_dise = "State",
    district_name_dise = c("Child A", "Child B"),
    dise_total_enrollment = c(100, 300),
    dise_total_teachers = c(10, 20),
    dise_total_schools = c(10, 30),
    dise_single_teacher_schools = c(2, 3),
    dise_girls_toilet_schools = c(8, 18),
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state",
    district_key = c("child a", "child b"),
    target_unit_2001 = "pc2001__01__01",
    n_candidate_targets = 1L,
    bridge_status = "deterministic_to_2001",
    bridge_sources = "reviewed",
    stringsAsFactors = FALSE
  )

  out <- harmonize_dise_counts_to_2001(district_year, bridge)

  expect_equal(out$dise_pupils_per_teacher, 400 / 30)
  expect_equal(out$dise_single_teacher_school_share, 12.5)
  expect_equal(out$dise_girls_toilet_school_share, 65)
})

test_that("DISE school-quality registry is concise and stable", {
  registry <- dise_school_quality_registry()
  expect_identical(
    registry$outcome,
    c(
      "dise_pupils_per_teacher",
      "dise_single_teacher_school_share",
      "dise_girls_toilet_school_share"
    )
  )
  expect_identical(anyDuplicated(registry$outcome), 0L)
})

test_that("DISE state aliases used by archived reports canonicalize to lineage states", {
  expect_identical(
    canonicalize_state_name(c("A & N Islands", "D & N Haveli")),
    c("andaman and nicobar islands", "dadra and nagar haveli")
  )
})

test_that("DISE workbook materialization follows file signature over a wrong extension", {
  need_pkg("readxl", "DISE workbook-format test")
  source <- tempfile(fileext = ".xls")
  writeBin(
    as.raw(c(0x50, 0x4B, 0x03, 0x04, rep(0x00, 12))),
    source
  )

  normalized <- normalize_dise_workbook_extension(source)

  expect_identical(readxl::format_from_signature(normalized), "xlsx")
  expect_identical(tolower(tools::file_ext(normalized)), "xlsx")
  expect_identical(readBin(normalized, "raw", n = 16L), readBin(source, "raw", n = 16L))
})

test_that("DISE workbook materialization leaves extension-correct files alone", {
  source <- tempfile(fileext = ".xlsx")
  writeBin(
    as.raw(c(0x50, 0x4B, 0x03, 0x04, rep(0x00, 12))),
    source
  )

  expect_identical(normalize_dise_workbook_extension(source), source)
})

test_that("DISE machine-sheet names come from the selected machine header", {
  preview <- as.data.frame(
    rbind(
      c("District Code", "State Name", "District Name", "Medium 1", "Enrollment"),
      c("DISTCD", "STATNAME", "DISTNAME", "M1", "ENRE11")
    ),
    stringsAsFactors = FALSE
  )

  expect_identical(
    dise_machine_names_from_preview(preview, 2L),
    c("distcd", "statename", "distname", "m1", "enre11")
  )
})

test_that("DISE machine-header selection prefers machine fields over human labels", {
  preview <- data.frame(
    V1 = c("State Code", "statecd"),
    V2 = c("State Name", "statename"),
    V3 = c("District Code", "distcd"),
    V4 = c("District Name", "distname"),
    V5 = c("Primary", "enr_govt1"),
    V6 = c("Upper Primary", "enr_govt2"),
    V7 = c("Girls", "enr_cy_c1"),
    stringsAsFactors = FALSE
  )

  expect_identical(find_dise_machine_header_row(preview), 2L)
})

test_that("DISE key repair uses the local header block when machine key labels are lost", {
  preview <- data.frame(
    V1 = c("State Code", "statecd"),
    V2 = c("State Name", "Jammu & Kashmir"),
    V3 = c("District Code", "101"),
    V4 = c("District Name", "Kupwara"),
    V5 = c("Primary", "enr_govt1"),
    stringsAsFactors = FALSE
  )
  names <- repair_dise_machine_names(unlist(preview[2, ], use.names = FALSE))

  repaired <- repair_dise_key_names_from_preview(names, preview, 2L)
  positions <- dise_key_positions_from_preview(preview, 2L)

  expect_identical(
    unname(positions[c("statecd", "statename", "distcd", "distname")]),
    1:4
  )
  expect_identical(
    repaired[1:4],
    c("statecd", "statename", "distcd", "distname")
  )
  expect_identical(repaired[[5]], "enr_govt1")
})

test_that("DISE key-position inference supports later sheets without state codes", {
  preview <- data.frame(
    V1 = c("Year", "2014_15"),
    V2 = c("District Code", "DISTCD"),
    V3 = c("State Name", "State Name"),
    V4 = c("District Name", "DISTNAME"),
    V5 = c("Schools", "SCH1"),
    stringsAsFactors = FALSE
  )

  positions <- dise_key_positions_from_preview(preview, 2L)
  repaired <- repair_dise_key_names_from_preview(
    repair_dise_machine_names(unlist(preview[2, ], use.names = FALSE)),
    preview,
    2L
  )

  expect_true(is.na(positions[["statecd"]]))
  expect_identical(
    unname(positions[c("statename", "distcd", "distname")]),
    c(3L, 2L, 4L)
  )
  expect_identical(repaired[c(2, 3, 4)], c("distcd", "statename", "distname"))
})

test_that("DISE key repair handles the documented 2010-11 header-data collision", {
  preview <- data.frame(
    V1 = c("State Code", "statecd"),
    V2 = c("State Name", "State Name"),
    V3 = c("District Code", "101"),
    V4 = c("District Name", "Kupwara"),
    V5 = c("Primary", "enr_govt1"),
    V6 = c("Upper Primary", "enr_govt2"),
    stringsAsFactors = FALSE
  )
  repaired <- repair_dise_key_names_from_preview(
    repair_dise_machine_names(unlist(preview[2, ], use.names = FALSE)),
    preview,
    2L
  )

  expect_identical(
    repaired[1:4],
    c("statecd", "statename", "distcd", "distname")
  )
  expect_identical(repaired[5:6], c("enr_govt1", "enr_govt2"))
})

test_that("DISE machine-name normalization handles later workbook labels", {
  repaired <- repair_dise_machine_names(c(
    "State Code", "State Name", "District Code", "District Name",
    "enr cy c1", "enr govt6", "M1", "ENRE11"
  ))
  expect_identical(
    repaired,
    c("statecd", "statename", "distcd", "distname",
      "enr_cy_c1", "enr_govt6", "m1", "enre11")
  )
})

test_that("DISE report-language maintainer parses both documented table orientations", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  python <- Sys.which("python3")
  skip_if(!nzchar(python), "python3 is required for the maintainer self-test")

  script <- file.path(root, "scripts", "build_dise_report_language_enrollment.py")
  expect_true(file.exists(script))
  status <- system2(
    python,
    c(shQuote(script), "--self-test"),
    stdout = TRUE,
    stderr = TRUE
  )
  status_code <- attr(status, "status")
  if (is.null(status_code)) status_code <- 0L
  expect_equal(status_code, 0L)
})

test_that("DISE report-language maintainer stays outside the targets runtime graph", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  targets_text <- paste(
    readLines(file.path(root, "_targets.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_false(grepl("build_dise_report_language_enrollment.py", targets_text, fixed = TRUE))
  expect_false(grepl("build_dise_report_total_enrollment_2010.py", targets_text, fixed = TRUE))
  expect_false(grepl("build_dise_report_school_quality.py", targets_text, fixed = TRUE))
})

test_that("report-derived DISE language metadata is unique and spans dynamic report years", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  path <- file.path(root, "data", "metadata", "dise_report_language_enrollment.csv")
  x <- read.csv(path, stringsAsFactors = FALSE)
  key <- paste(x$academic_year, canonicalize_state_name(x$state_report),
               canonicalize_district_name(x$district_report), sep = "|")
  expect_equal(anyDuplicated(key), 0L)
  expect_setequal(
    unique(x$academic_year),
    c("2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15")
  )
  expect_true(all(c("source_pdf", "source_page", "report_priority") %in% names(x)))
})

test_that("DISE 2010-11 published enrollment totals are complete and reproducible", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  path <- file.path(
    root, "data", "metadata", "dise_report_total_enrollment_2010_11.csv"
  )
  x <- read.csv(path, stringsAsFactors = FALSE)
  expect_true(nrow(x) > 600L)
  expect_true(all(x$academic_year == "2010-11"))
  expect_true(all(is.finite(x$report_total_enrollment)))
  expect_true(all(x$report_total_enrollment >= 0))
  key <- paste(
    x$academic_year,
    canonicalize_state_name(x$state_report),
    canonicalize_district_name(x$district_report),
    sep = "|"
  )
  expect_equal(anyDuplicated(key), 0L)

  python <- Sys.which("python3")
  skip_if(!nzchar(python), "python3 is required for the maintainer self-test")
  script <- file.path(root, "scripts", "build_dise_report_total_enrollment_2010.py")
  expect_true(file.exists(script))
  status <- system2(
    python,
    c(shQuote(script), "--self-test"),
    stdout = TRUE,
    stderr = TRUE
  )
  status_code <- attr(status, "status")
  if (is.null(status_code)) status_code <- 0L
  expect_equal(status_code, 0L)
})

test_that("later DISE language counts cannot exceed the elementary denominator", {
  data <- data.frame(
    academic_year = "2010-11",
    state_name_dise = "State",
    district_name_dise = "District",
    dise_total_enrollment = 100,
    stringsAsFactors = FALSE
  )
  report <- data.frame(
    academic_year = "2010-11",
    state_report = "State",
    district_report = "District",
    english_enrollment = 120,
    hindi_enrollment = 10,
    source_pdf = "report.pdf",
    source_page = 1L,
    report_priority = 1L,
    stringsAsFactors = FALSE
  )

  out <- attach_dise_report_language_counts(data, report)

  expect_false(out$dise_english_count_valid)
  expect_false(out$dise_english_identity_resolved)
  expect_true(is.na(out$dise_emi_enrollment_share_total))
  expect_true(out$dise_hindi_count_valid)
})

test_that("2010-11 published totals override corrupt raw enrollment but preserve QA", {
  data <- data.frame(
    academic_year = "2010-11",
    state_name_dise = "Puducherry",
    district_name_dise = "Mahe",
    dise_total_enrollment = 138158,
    dise_total_enrollment_source = "grade_i_viii_sum",
    stringsAsFactors = FALSE
  )
  report <- data.frame(
    academic_year = "2010-11",
    state_report = "Puducherry",
    district_report = "Mahe",
    report_total_enrollment = 7482,
    source_pdf = "report.pdf",
    source_page = 94L,
    report_priority = 2L,
    stringsAsFactors = FALSE
  )

  out <- attach_dise_report_total_enrollment(data, report)

  expect_equal(out$dise_total_enrollment, 7482)
  expect_identical(out$dise_total_enrollment_source, "report_card_current_year_total")
  expect_equal(out$dise_total_enrollment_raw, 138158)
  expect_identical(out$dise_total_enrollment_source_raw, "grade_i_viii_sum")
  expect_equal(out$dise_report_to_raw_total_ratio, 7482 / 138158)
})

test_that("2010-11 report totals match historical state spelling variants", {
  data <- data.frame(
    academic_year = "2010-11",
    state_name_dise = "Chhattisgarh",
    district_name_dise = "Mahasamund",
    dise_total_enrollment = 731540,
    dise_total_enrollment_source = "grade_i_viii_sum",
    stringsAsFactors = FALSE
  )
  report <- data.frame(
    academic_year = "2010-11",
    state_report = "Chhatisgarh",
    district_report = "MAHASAMUND",
    report_total_enrollment = 189286,
    source_pdf = "report.pdf",
    source_page = 1L,
    report_priority = 1L,
    stringsAsFactors = FALSE
  )

  out <- attach_dise_report_total_enrollment(data, report)

  expect_true(out$dise_report_total_matched)
  expect_equal(out$dise_total_enrollment, 189286)
  expect_identical(out$dise_total_enrollment_source, "report_card_current_year_total")
  expect_equal(out$dise_total_enrollment_raw, 731540)
})

test_that("2010-11 corrupt raw totals are never fallback production denominators", {
  data <- data.frame(
    academic_year = "2010-11",
    state_name_dise = "Haryana",
    district_name_dise = "Ambala",
    dise_total_enrollment = 76827,
    dise_total_enrollment_source = "grade_i_viii_sum",
    stringsAsFactors = FALSE
  )
  report <- data.frame(
    academic_year = character(),
    state_report = character(),
    district_report = character(),
    report_total_enrollment = numeric(),
    source_pdf = character(),
    source_page = integer(),
    report_priority = integer(),
    stringsAsFactors = FALSE
  )

  out <- attach_dise_report_total_enrollment(data, report)

  expect_false(out$dise_report_total_matched)
  expect_true(is.na(out$dise_total_enrollment))
  expect_identical(
    out$dise_total_enrollment_source,
    "unavailable_without_report_total"
  )
  expect_equal(out$dise_total_enrollment_raw, 76827)
  expect_identical(out$dise_total_enrollment_source_raw, "grade_i_viii_sum")
})

test_that("later report attachment never interprets absent English as zero", {
  data <- data.frame(
    academic_year = "2012-13",
    state_name_dise = "State",
    district_name_dise = "District",
    dise_total_enrollment = 100,
    stringsAsFactors = FALSE
  )
  report <- data.frame(
    academic_year = "2012-13",
    state_report = "State",
    district_report = "District",
    english_enrollment = NA_real_,
    hindi_enrollment = 80,
    source_pdf = "report.pdf",
    source_page = 1L,
    report_priority = 1L,
    stringsAsFactors = FALSE
  )
  out <- attach_dise_report_language_counts(data, report)
  expect_false(out$dise_english_identity_resolved)
  expect_true(is.na(out$dise_emi_enrollment_share_total))
  expect_equal(out$dise_hindi_enrollment, 80)
})

test_that("2015 positive enrollment under an unidentified medium remains unresolved", {
  data <- data.frame(
    distcd = "0101",
    m1 = 19, m2 = 0, m3 = 0, m4 = 0, m5 = 0,
    enre11 = 40, enre21 = 10,
    enre31 = 0, enre41 = 0, enre51 = 0,
    stringsAsFactors = FALSE
  )

  out <- extract_dise_2015_medium_counts(data)

  expect_true(is.na(out$dise_english_enrollment))
  expect_true(is.na(out$dise_hindi_enrollment))
})

test_that("2015 missing enrollment blocks are harmless only for absent medium codes", {
  data <- data.frame(
    distcd = c("0101", "0102"),
    m1 = c(19, 19), m2 = c(4, 4),
    m3 = c(NA, 7), m4 = NA, m5 = NA,
    enre11 = c(40, 40), enre21 = c(30, 30),
    stringsAsFactors = FALSE
  )

  out <- extract_dise_2015_medium_counts(data)

  expect_equal(out$dise_english_enrollment[[1]], 40)
  expect_equal(out$dise_hindi_enrollment[[1]], 30)
  expect_false(is.na(out$dise_english_enrollment[[1]]))
  expect_false(is.na(out$dise_hindi_enrollment[[1]]))

  expect_true(is.na(out$dise_english_enrollment[[2]]))
  expect_true(is.na(out$dise_hindi_enrollment[[2]]))
})

test_that("2015 medium decoder requires canonical code columns", {
  data <- data.frame(
    distcd = "0101",
    m1 = 19, m2 = 4, m3 = 0, m4 = 0,
    enre11 = 1, enre21 = 1, enre31 = 0, enre41 = 0, enre51 = 0,
    stringsAsFactors = FALSE
  )

  expect_error(
    extract_dise_2015_medium_counts(data),
    "missing canonical columns: m5"
  )
})

test_that("2015 coded media use official Hindi and English codes", {
  data <- data.frame(
    distcd = "0101",
    m1 = 19, m2 = 4, m3 = NA, m4 = NA, m5 = NA,
    enre11 = 40, enre12 = 10,
    enre21 = 30, enre22 = 20,
    stringsAsFactors = FALSE
  )
  out <- extract_dise_2015_medium_counts(data)
  expect_equal(out$dise_english_enrollment, 50)
  expect_equal(out$dise_hindi_enrollment, 50)
})

test_that("dynamic instrument registry removes district-FE algebraic duplicates", {
  registry <- dise_dynamic_instrument_registry()
  expect_equal(anyDuplicated(registry$excluded_instrument), 0L)
  preferred <- registry[registry$excluded_instrument == "ling_distance_nonzero_mean", , drop = FALSE]
  expect_equal(nrow(preferred), 1L)
  expect_match(preferred$equivalent_construction_ids, "nonzero_mean")
  expect_match(preferred$equivalent_construction_ids, "nonzero_mean_shastry")
  expect_false(any(grepl("distance_shares", registry$construction_id)))
})

test_that("dynamic event study recovers changes relative to the reference-year gradient", {
  set.seed(1)
  districts <- paste0("d", 1:40)
  years <- c("2006-07", "2007-08", "2008-09")
  base <- expand.grid(
    target_unit_2001 = districts,
    academic_year = years,
    stringsAsFactors = FALSE
  )
  z <- setNames(seq(-1, 1, length.out = length(districts)), districts)
  state <- setNames(rep(c("01", "02"), each = 20), districts)
  base$state_code_2001 <- state[base$target_unit_2001]
  base$ling_distance_nonzero_mean <- z[base$target_unit_2001]
  gradient <- c("2006-07" = 1, "2007-08" = 2, "2008-09" = 4)
  district_fe <- setNames(seq(-2, 2, length.out = length(districts)), districts)
  base$dise_emi_enrollment_share_total <-
    district_fe[base$target_unit_2001] +
    gradient[base$academic_year] * base$ling_distance_nonzero_mean +
    stats::rnorm(nrow(base), sd = 0.01)
  rownames(base) <- seq(101L, by = 2L, length.out = nrow(base))

  fit <- estimate_dise_dynamic_spec(
    base, "ling_distance_nonzero_mean", "district_year", "2007-08"
  )
  b <- setNames(fit$coefficients$estimate, fit$coefficients$academic_year)
  expect_equal(unname(b["2006-07"]), -1, tolerance = 0.02)
  expect_equal(unname(b["2008-09"]), 2, tolerance = 0.02)
  expect_equal(fit$summary$n_years, 3L)
  expect_identical(fit$summary$cluster_status[[1]], "estimated")
  expect_true(all(is.finite(fit$coefficients$std.error)))
  expect_true(is.finite(fit$summary$joint_distance_year_f[[1]]))
  expect_true(is.finite(fit$summary$joint_distance_year_p[[1]]))
  expect_true(is.finite(fit$summary$pre_distance_year_f[[1]]))
  expect_true(is.finite(fit$summary$post_distance_year_f[[1]]))
  expect_true(is.finite(fit$summary$pre_distance_year_p[[1]]))
  expect_true(is.finite(fit$summary$post_distance_year_p[[1]]))

  state_year_fit <- estimate_dise_dynamic_spec(
    base, "ling_distance_nonzero_mean", "district_state_year", "2007-08"
  )
  expect_identical(state_year_fit$summary$cluster_status[[1]], "estimated")
  expect_true(all(is.finite(state_year_fit$coefficients$std.error)))
  expect_true(is.finite(state_year_fit$summary$joint_distance_year_f[[1]]))
  expect_true(is.finite(state_year_fit$summary$joint_distance_year_p[[1]]))
})

test_that("DISE diagnostic saver includes longitudinal outputs", {
  empty <- data.frame()
  archive <- list(year_summary = empty, treatment_summary = empty, publication_checks = empty)
  permutations <- list(
    construct_registry = empty, nss_validation = empty,
    first_stage = empty, first_stage_coefficients = empty,
    weak_iv_outcomes = empty, anderson_rubin_grid = empty,
    overidentification = empty, monotonicity_summary = empty,
    monotonicity_bins = empty, monotonicity_state_slopes = empty,
    balance = empty, joint_balance = empty
  )
  dynamic <- list(registry = empty, summary = empty, coefficients = empty)
  dir <- tempfile("dise-longitudinal-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  manifest <- save_dise_diagnostics(
    archive, permutations, empty, empty,
    dynamic_panel = empty, dynamic_relevance = dynamic, dir = dir
  )
  expect_equal(nrow(manifest), 32L)
  expect_true(all(file.exists(manifest$path)))
  expect_setequal(
    basename(manifest$path)[grepl("dise_dynamic_", basename(manifest$path))],
    c(
      "dise_dynamic_district_year_2001.csv",
      "dise_dynamic_specification_registry.csv",
      "dise_dynamic_first_stage_summary.csv",
      "dise_dynamic_first_stage_event_study.csv"
    )
  )
})

test_that("pooled baseline age exposure sums person-year counts before forming the ratio", {
  x <- data.frame(
    academic_year = c("2005-06", "2006-07", "2007-08"),
    target_unit_2001 = "pc2001__09__01",
    dise_source_district_count = 1L,
    dise_english_enrollment = c(20, 80, 180),
    dise_hindi_enrollment = c(80, 120, 120),
    dise_total_enrollment = c(100, 200, 300),
    dise_english_identity_resolved = TRUE,
    dise_hindi_identity_resolved = TRUE,
    dise_emi_enrollment_share_total = c(20, 40, 60),
    dise_hindi_enrollment_share_total = c(80, 60, 40),
    dise_english_share_english_hindi = c(20, 40, 60),
    dise_private_enrollment_share = 10,
    dise_private_school_share = 10,
    census_age_6_13_population = c(100, 200, 400),
    dise_emi_gross_enrollment_ratio_age_6_13 = c(20, 40, 45),
    dise_elementary_gross_enrollment_ratio_age_6_13 = c(100, 100, 75),
    stringsAsFactors = FALSE
  )

  out <- build_dise_baseline_treatments_2001(x)
  expect_equal(out$dise_emi_gross_enrollment_ratio_age_6_13_0708, 45)
  expect_equal(out$dise_age_6_13_baseline_years_observed, 3L)
  expect_equal(
    out$dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled,
    100 * 280 / 700
  )
})

test_that("DISE school-quality diagnostics combine predetermined raw and later report measures", {
  set.seed(42)
  states <- sprintf("%02d", 1:5)
  districts <- paste0(
    "pc2001__", rep(states, each = 8), "__", sprintf("%02d", rep(1:8, 5))
  )
  distance <- rep(seq(0.1, 0.8, length.out = 8), 5)
  baseline <- do.call(rbind, lapply(c("2005-06", "2006-07"), function(year) {
    data.frame(
      target_unit_2001 = districts,
      state_code_2001 = rep(states, each = 8),
      academic_year = year,
      ling_distance_nonzero_mean = distance,
      dise_pupils_per_teacher = stats::rnorm(40, 30, 2),
      dise_single_teacher_school_share = stats::runif(40, 0, 20),
      dise_girls_toilet_school_share = stats::runif(40, 20, 80),
      stringsAsFactors = FALSE
    )
  }))
  dynamic_design <- do.call(rbind, lapply(
    c("2011-12", "2012-13", "2013-14", "2014-15"),
    function(year) {
      data.frame(
        target_unit_2001 = districts,
        state_code_2001 = rep(states, each = 8),
        academic_year = year,
        ling_distance_nonzero_mean = distance,
        dise_pupils_per_teacher = NA_real_,
        dise_single_teacher_school_share = NA_real_,
        dise_girls_toilet_school_share = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  ))
  report <- do.call(rbind, lapply(seq_along(c("2011-12", "2012-13", "2013-14", "2014-15")), function(i) {
    year <- c("2011-12", "2012-13", "2013-14", "2014-15")[[i]]
    residual <- 0.2 * sin(seq_along(districts) + 1.7 * i)
    data.frame(
      target_unit_2001 = districts,
      academic_year = year,
      dise_report_school_quality_status = "one_to_one_report_ratio",
      dise_report_pupils_per_teacher = 25 + i * distance + residual,
      dise_report_single_teacher_school_share = 10 + 2 * i * distance + residual,
      dise_report_girls_toilet_school_share = 60 + 3 * i * distance + residual,
      girls_toilet_definition = ifelse(
        year == "2011-12", "all_schools", "girls_and_coeducational_schools"
      ),
      stringsAsFactors = FALSE
    )
  }))
  out <- diagnose_dise_school_quality_mechanisms(
    safe_bind_rows(list(baseline, dynamic_design)),
    report
  )

  expect_setequal(unique(out$baseline_association$academic_year), c("2005-06", "2006-07"))
  expect_true(all(out$baseline_association$cluster_variable == "state_code_2001"))
  expect_true(all(out$registry$dynamic_status == "estimated_report_cards"))
  expect_true(nrow(out$summary) > 0L)
  expect_true(nrow(out$coefficients) > 0L)
  ptr_summary <- out$summary[
    out$summary$outcome == "dise_report_pupils_per_teacher",
    , drop = FALSE
  ]
  toilet_summary <- out$summary[
    out$summary$outcome == "dise_report_girls_toilet_school_share",
    , drop = FALSE
  ]
  expect_true(all(ptr_summary$reference_year == "2011-12"))
  expect_true(all(toilet_summary$reference_year == "2012-13"))
})

test_that("report school-quality ratios are not averaged across later child districts", {
  report <- data.frame(
    academic_year = c("2013-14", "2013-14", "2013-14"),
    state_report = "State",
    district_report = c("Single", "Child A", "Child B"),
    report_pupils_per_teacher = c(20, 25, 35),
    report_single_teacher_school_share = c(5, 10, 20),
    report_girls_toilet_school_share = c(80, 70, 60),
    girls_toilet_definition = "girls_and_coeducational_schools",
    source_pdf = "report.pdf",
    source_page = 1:3,
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state",
    district_key = c("single", "child a", "child b"),
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02", "pc2001__01__02"),
    bridge_status = "deterministic_to_2001",
    stringsAsFactors = FALSE
  )

  out <- harmonize_dise_report_school_quality_to_2001(report, bridge)
  single <- out[out$target_unit_2001 == "pc2001__01__01", , drop = FALSE]
  split <- out[out$target_unit_2001 == "pc2001__01__02", , drop = FALSE]

  expect_equal(single$dise_report_pupils_per_teacher, 20)
  expect_identical(single$dise_report_school_quality_status, "one_to_one_report_ratio")
  expect_true(is.na(split$dise_report_pupils_per_teacher))
  expect_true(is.na(split$dise_report_single_teacher_school_share))
  expect_identical(
    split$dise_report_school_quality_status,
    "multiple_source_districts_not_aggregated"
  )
})

test_that("tracked DISE report school-quality metadata has valid ranges and definitions", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", ".")
  path <- file.path(root, "data", "metadata", "dise_report_school_quality_2011_15.csv")
  x <- read.csv(path, stringsAsFactors = FALSE)
  expect_setequal(unique(x$academic_year), c("2011-12", "2012-13", "2013-14", "2014-15"))
  expect_true(all(x$report_pupils_per_teacher >= 0))
  expect_true(all(x$report_single_teacher_school_share >= 0 & x$report_single_teacher_school_share <= 100))
  expect_true(all(x$report_girls_toilet_school_share >= 0 & x$report_girls_toilet_school_share <= 100))
  expect_true(all(
    x$girls_toilet_definition[x$academic_year == "2011-12"] == "all_schools"
  ))
  expect_true(all(
    x$girls_toilet_definition[x$academic_year != "2011-12"] ==
      "girls_and_coeducational_schools"
  ))
  key <- paste(
    x$academic_year,
    canonicalize_state_name(x$state_report),
    canonicalize_district_name(x$district_report),
    sep = "|"
  )
  expect_equal(anyDuplicated(key), 0L)

  python <- Sys.which("python3")
  skip_if(!nzchar(python), "python3 is required for the maintainer self-test")
  script <- file.path(root, "scripts", "build_dise_report_school_quality.py")
  status <- system2(python, c(shQuote(script), "--self-test"), stdout = TRUE, stderr = TRUE)
  code <- attr(status, "status")
  if (is.null(code)) code <- 0L
  expect_equal(code, 0L)
})


test_that("DISE total enrollment uses one grade-first hierarchy in all workbook generations", {
  data <- data.frame(
    enr_cy_c1 = c(60, NA, NA),
    enr_cy_c2 = c(40, NA, NA),
    enrtot = c(1000, 120, NA),
    enr_govt1 = c(70, 80, 75),
    enr_pvt1 = c(30, 20, 25),
    stringsAsFactors = FALSE
  )
  totals <- dise_enrollment_total_candidates(data)
  expect_equal(totals$dise_total_enrollment, c(100, 120, 100))
  expect_identical(
    totals$dise_total_enrollment_source,
    c("grade_i_viii_sum", "direct_enrtot", "government_private_sum")
  )
  expect_equal(totals$dise_direct_to_grade_enrollment_ratio[[1]], 10)
})

test_that("later DISE ENRTOT cannot override complete grade-I-VIII counts", {
  data <- data.frame(
    statecd = "01", statename = "State", distcd = "0101", distname = "District",
    enr_cy_c1 = 6000, enr_cy_c2 = 4000, enrtot = 100000,
    stringsAsFactors = FALSE
  )
  out <- extract_dise_total_enrollment(data, "2010-11")
  expect_equal(out$dise_grade_enrollment, 10000)
  expect_equal(out$dise_direct_enrollment, 100000)
  expect_equal(out$dise_total_enrollment, 10000)
  expect_identical(out$dise_total_enrollment_source, "grade_i_viii_sum")
})

test_that("DISE count harmonization does not require diagnostic bridge provenance", {
  district_year <- data.frame(
    academic_year = "2007-08",
    state_name_dise = "State",
    district_name_dise = "District",
    dise_english_enrollment = 25,
    dise_hindi_enrollment = 25,
    dise_total_enrollment = 100,
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state",
    district_key = "district",
    target_unit_2001 = "pc2001__01__01",
    bridge_status = "deterministic_to_2001",
    stringsAsFactors = FALSE
  )

  out <- harmonize_dise_counts_to_2001(district_year, bridge)

  expect_equal(nrow(out), 1L)
  expect_identical(out$target_unit_2001, "pc2001__01__01")
  expect_equal(out$dise_emi_enrollment_share_total, 25)
})

test_that("published 2010-11 report totals remain authoritative after lineage pooling", {
  district_year <- data.frame(
    academic_year = c("2010-11", "2010-11"),
    state_name_dise = "State",
    district_name_dise = c("Child A", "Child B"),
    dise_english_enrollment = c(10, 20),
    dise_hindi_enrollment = c(20, 30),
    report_total_enrollment = c(100, 200),
    dise_grade_enrollment = c(1000, 2000),
    dise_direct_enrollment = c(900, 1900),
    dise_management_enrollment = c(950, 1950),
    dise_total_enrollment = c(100, 200),
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state",
    district_key = c("child a", "child b"),
    target_unit_2001 = "pc2001__01__01",
    bridge_status = "deterministic_to_2001",
    stringsAsFactors = FALSE
  )

  out <- harmonize_dise_counts_to_2001(district_year, bridge)

  expect_equal(out$report_total_enrollment, 300)
  expect_equal(out$dise_grade_enrollment, 3000)
  expect_equal(out$dise_total_enrollment, 300)
  expect_identical(
    out$dise_total_enrollment_source,
    "report_card_current_year_total"
  )
  expect_equal(out$dise_emi_enrollment_share_total, 10)
})

test_that("DISE lineage aggregation reapplies denominator precedence after pooling counts", {
  district_year <- data.frame(
    academic_year = c("2010-11", "2010-11"),
    state_name_dise = "State", district_name_dise = c("Child A", "Child B"),
    dise_english_enrollment = c(10, 20), dise_hindi_enrollment = c(50, 70),
    dise_grade_enrollment = c(100, 200),
    dise_direct_enrollment = c(1000, 2000),
    dise_management_enrollment = c(100, 200),
    dise_total_enrollment = c(100, 200),
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state", district_key = c("child a", "child b"),
    target_unit_2001 = "pc2001__01__01", n_candidate_targets = 1L,
    bridge_status = "deterministic_to_2001", bridge_sources = "reviewed",
    stringsAsFactors = FALSE
  )
  out <- harmonize_dise_counts_to_2001(district_year, bridge)
  expect_equal(out$dise_grade_enrollment, 300)
  expect_equal(out$dise_direct_enrollment, 3000)
  expect_equal(out$dise_total_enrollment, 300)
  expect_identical(out$dise_total_enrollment_source, "grade_i_viii_sum")
  expect_equal(out$dise_emi_enrollment_share_total, 10)
})

test_that("2010-11 unavailable report totals remain unavailable after lineage pooling", {
  district_year <- data.frame(
    academic_year = c("2010-11", "2010-11"),
    state_name_dise = "State",
    district_name_dise = c("Child A", "Child B"),
    dise_english_enrollment = c(10, 20),
    dise_hindi_enrollment = c(20, 30),
    report_total_enrollment = c(100, NA),
    dise_grade_enrollment = c(1000, 2000),
    dise_direct_enrollment = c(900, 1900),
    dise_management_enrollment = c(950, 1950),
    dise_total_enrollment = c(100, NA),
    dise_total_enrollment_source = c(
      "report_card_current_year_total",
      "unavailable_without_report_total"
    ),
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    state_key = "state",
    district_key = c("child a", "child b"),
    target_unit_2001 = "pc2001__01__01",
    bridge_status = "deterministic_to_2001",
    stringsAsFactors = FALSE
  )

  out <- harmonize_dise_counts_to_2001(district_year, bridge)

  expect_true(is.na(out$report_total_enrollment))
  expect_true(is.na(out$dise_grade_enrollment))
  expect_true(is.na(out$dise_total_enrollment))
  expect_true(is.na(out$dise_emi_enrollment_share_total))
})
