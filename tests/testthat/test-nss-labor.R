test_that("NSS64 source normalizer preserves survey design and person keys", {
  raw <- data.frame(
    key_memb = c("a", "b"),
    Sector = haven::labelled(c("1", "2"), c(rural = "1", urban = "2")),
    Sub_Round = c("1", "2"), Sub_sample = c("1", "2"),
    State_Region = c("101", "102"), state = c("01", "01"),
    District = c("02", "03"), Stratum = c("11", "12"),
    Sub_Stratum = c("1", "1"), FSU = c("10001", "10002"),
    Ss_stratum = c("1", "2"), Sample_hhold_No = c("1", "1"),
    wgt_combined = c("100.5", "200.5"), B4_c1 = c("1", "2")
  )
  out <- normalize_nss64_design(raw, "B4_c1", "test")
  expect_equal(out$state_code, c("01", "01"))
  expect_equal(out$district_code, c("02", "03"))
  expect_equal(out$nss_region, c("101", "102"))
  expect_equal(out$survey_weight, c(100.5, 200.5))
  expect_equal(out$person_key, c("a", "b"))
})

test_that("NSS64 source normalizer fails closed on duplicate people or invalid weights", {
  raw <- data.frame(
    key_memb = c("a", "a"), Sector = 1, Sub_Round = 1, Sub_sample = 1,
    State_Region = 101, state = 1, District = 2, Stratum = 11,
    Sub_Stratum = 1, FSU = c(10001, 10002), Ss_stratum = 1,
    Sample_hhold_No = 1, wgt_combined = 100, B4_c1 = c(1, 2)
  )
  expect_error(normalize_nss64_design(raw, "B4_c1", "test"), "complete and unique")
  raw$key_memb <- c("a", "b")
  raw$wgt_combined[[2]] <- 0
  expect_error(normalize_nss64_design(raw, "B4_c1", "test"), "finite and positive")
})

test_that("NSS64 Block 4 and Block 6 validation requires one common person universe", {
  block4 <- data.frame(person_key = c("a", "b"), survey_weight = c(1, 2))
  block6 <- data.frame(person_key = c("b", "a"), survey_weight = c(2, 1))
  ddi <- data.frame(file_id = c("F4", "F6"), case_count = c(2, 2))
  out <- validate_nss64_source_pair(block4, block6, ddi)
  expect_equal(out$rows, c(2, 2))
  expect_true(all(out$positive_weight_share == 1))

  block6$person_key[[2]] <- "c"
  expect_error(validate_nss64_source_pair(block4, block6, ddi), "same household members")
})

test_that("NSS64 cross-block validation requires identical survey design by person", {
  design <- data.frame(
    person_key = c("a", "b"), state_code = c("01", "01"),
    district_code = c("02", "03"), sector = c(1, 2), sub_round = c(1, 2),
    sub_sample = c(1, 2), nss_region = c("011", "011"),
    stratum = c(11, 12), sub_stratum = c(1, 1), fsu = c(1001, 1002),
    second_stage_stratum = c(1, 2), household_no = c(1, 1),
    person_no = c(1, 2), survey_weight = c(100, 200),
    stringsAsFactors = FALSE
  )
  migration <- design[c(2, 1), , drop = FALSE]
  expect_invisible(validate_nss64_cross_block_design(design, migration))

  migration$district_code[migration$person_key == "a"] <- "04"
  expect_error(
    validate_nss64_cross_block_design(design, migration),
    "disagree on shared field district_code"
  )
})

test_that("NSS64 reviewed lineage uses the documented SSRDD source identity", {
  persons <- data.frame(
    person_key = c("a", "b", "c"),
    state_code = c("22", "22", "22"),
    district_code = c("09", "10", "99"),
    nss_region = c("222", "222", "222"),
    sector = c(1, 2, 1), fsu = c(1001, 1002, 1003),
    survey_weight = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    wave = c("nss_2007_08", "nss_2007_08", "nss_2017_18"),
    source_code = c("22209", "22210", "22101"),
    target_unit_2001 = c("pc2001__22__09", "pc2001__22__10", "pc2001__22__01"),
    weight = c(1, 1, 1),
    panel_variant = c("deterministic", "deterministic", "deterministic"),
    stringsAsFactors = FALSE
  )

  out <- attach_nss64_reviewed_lineage(persons, crosswalk)
  expect_equal(out$source_district_code, c("22209", "22210", "22299"))
  expect_equal(out$target_unit_2001[1:2], c("pc2001__22__09", "pc2001__22__10"))
  expect_true(is.na(out$target_unit_2001[[3L]]))
  expect_equal(out$lineage_status[[3L]], "unresolved_source_district")

  support <- summarize_nss64_lineage_support(out)
  expect_equal(support$n_sample_people, c(1L, 1L, 1L))
  expect_equal(support$n_fsu, c(1L, 1L, 1L))
  expect_equal(support$sum_person_weight, c(10, 20, 30))
  expect_equal(support$kish_effective_n, c(1, 1, 1))
})

test_that("NSS64 target support pools reviewed source districts before support diagnostics", {
  persons <- data.frame(
    source_district_code = c("22209", "22209", "22210", "22299"),
    target_unit_2001 = c("pc2001__22__09", "pc2001__22__09", "pc2001__22__09", NA),
    lineage_status = c(
      "resolved_reviewed_deterministic", "resolved_reviewed_deterministic",
      "resolved_reviewed_deterministic", "unresolved_source_district"
    ),
    state_code = "22", sector = c(1, 1, 2, 1), fsu = c(1001, 1001, 1002, 1003),
    survey_weight = c(1, 2, 3, 100), stringsAsFactors = FALSE
  )

  out <- summarize_nss64_target_support(persons)
  expect_equal(nrow(out), 1L)
  expect_equal(out$n_source_districts, 2L)
  expect_equal(out$n_sample_people, 3L)
  expect_equal(out$n_fsu, 2L)
  expect_equal(out$sum_person_weight, 6)
  expect_equal(out$kish_effective_n, 36 / 14, tolerance = 1e-8)
  expect_equal(out$n_rural_people, 2L)
  expect_equal(out$n_urban_people, 1L)
})

test_that("NSS64 lineage fails closed on inconsistent or non-deterministic geography", {
  persons <- data.frame(
    person_key = "a", state_code = "22", district_code = "09",
    nss_region = "231", survey_weight = 1, stringsAsFactors = FALSE
  )
  expect_error(nss64_source_district_code(persons), "internally inconsistent")

  crosswalk <- data.frame(
    wave = "nss_2007_08", source_code = "22209",
    target_unit_2001 = "pc2001__22__09", weight = 0.5,
    panel_variant = "population_allocation", stringsAsFactors = FALSE
  )
  expect_error(nss64_reviewed_lineage_map(crosswalk), "no deterministic reviewed")
})

test_that("NSS64 design identifiers remain nested within source districts", {
  rows <- data.frame(
    source_district_code = c("01101", "01102"),
    state_code = "01", sector = 1, stratum = 1, sub_stratum = 1, fsu = 1001,
    stringsAsFactors = FALSE
  )
  expect_equal(length(unique(nss64_design_psu_key(rows))), 2L)
  expect_equal(length(unique(nss64_design_stratum_key(rows))), 2L)
})

test_that("NSS64 support rule excludes only genuinely thin target designs", {
  support <- data.frame(
    target_unit_2001 = c("a", "b", "c"),
    n_fsu = c(4, 5, 8),
    kish_effective_n = c(150, 99, 100),
    stringsAsFactors = FALSE
  )
  out <- nss64_target_support_classification(support)
  expect_equal(out$preferred_eligible, c(FALSE, FALSE, TRUE))
  expect_equal(out$support_reason[[1L]], "too_few_psus")
  expect_equal(out$support_reason[[2L]], "low_kish_effective_n")
  expect_true(is.na(out$support_reason[[3L]]))
})

test_that("NSS64 employment follows usual principal-plus-subsidiary status", {
  flags <- nss64_labor_status_flags(
    c(91, 81, 31, 51),
    c(31, NA, 51, NA)
  )
  expect_equal(flags$employed, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(flags$unemployed, c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(flags$regular_salaried, c(TRUE, FALSE, TRUE, FALSE))
})

test_that("NSS64 labor registry is compact and source-defined", {
  registry <- nss64_outcome_registry()
  expect_equal(nrow(registry), 5L)
  expect_identical(anyDuplicated(registry$outcome_id), 0L)
  expect_setequal(
    registry$outcome_id,
    c(
      "labor_force_participation_age15plus",
      "employment_rate_age15plus",
      "unemployment_rate_age15plus",
      "regular_salaried_share_employed_age15plus",
      "migrant_from_last_upr_share_age15plus"
    )
  )
})

test_that("NSS64 usual-status outcomes use documented denominator populations", {
  rows <- data.frame(
    age15plus = rep(TRUE, 5),
    employed = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    unemployed = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    labor_force = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    regular_salaried = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    migrant_from_last_upr = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_equal(nss64_outcome_domain(rows, "labor_force_participation_age15plus")$value, rows$labor_force)
  expect_equal(
    nss64_outcome_domain(rows, "unemployment_rate_age15plus")$rows,
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_equal(
    nss64_outcome_domain(rows, "regular_salaried_share_employed_age15plus")$rows,
    c(TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("NSS64 design-based district estimates preserve thin districts but flag support", {
  people <- data.frame(
    person_key = paste0("p", 1:12),
    source_district_code = rep(c("01101", "01102"), each = 6),
    state_code = "01", district_code = rep(c("01", "02"), each = 6),
    sector = 1, sub_round = 1, sub_sample = 1, nss_region = "011",
    stratum = rep(c(1, 2), each = 6), sub_stratum = 1,
    fsu = c(1:6, 7, 7, 8, 8, 9, 10), second_stage_stratum = 1, household_no = 1:12,
    person_no = 1, survey_weight = 1,
    age = 20,
    usual_principal_status = c(31, 31, 81, 91, 91, 91, 11, 31, 51, 81, 91, 91),
    usual_subsidiary_status = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    target_unit_2001 = rep(c("pc2001__01__01", "pc2001__01__02"), each = 6),
    lineage_status = "resolved_reviewed_deterministic",
    stringsAsFactors = FALSE
  )
  migration <- people[c(
    "person_key", "state_code", "district_code", "sector", "sub_round", "sub_sample",
    "nss_region", "stratum", "sub_stratum", "fsu", "second_stage_stratum",
    "household_no", "person_no", "survey_weight"
  )]
  migration$enumeration_differs_last_upr <- c(1, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2)
  support <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    n_source_districts = 1, n_sample_people = 6, n_fsu = c(6, 4),
    sum_person_weight = 6, kish_effective_n = c(6, 6),
    n_rural_people = 6, n_urban_people = 0,
    stringsAsFactors = FALSE
  )
  registry <- nss64_outcome_registry()[1:2, , drop = FALSE]
  out <- estimate_nss64_district_outcomes(
    people, migration, support, registry,
    rule = data.frame(min_fsu = 5L, min_kish_effective_n = 1)
  )
  expect_equal(nrow(out$estimates), 4L)
  lf <- out$estimates[out$estimates$outcome_id == "labor_force_participation_age15plus", ]
  expect_equal(lf$estimate[lf$target_unit_2001 == "pc2001__01__01"], 0.5, tolerance = 1e-8)
  expect_true(lf$analysis_eligible[lf$target_unit_2001 == "pc2001__01__01"])
  expect_false(lf$analysis_eligible[lf$target_unit_2001 == "pc2001__01__02"])
  expect_true(is.finite(lf$estimate[lf$target_unit_2001 == "pc2001__01__02"]))
})


test_that("NSS labor DDI validation is reusable across survey waves", {
  expect_true(is.function(read_nss_labor_ddi_contract))
  req <- nss66_eus_ddi_requirements()
  expect_setequal(names(req), c("F4", "F5", "F6"))
  expect_true(all(c("WEIGHT", "PID", "District", "State_Region") %in% req$F5))
  expect_true("Usual_Principal_Activity_Status" %in% req$F5)
  expect_true("Usual_Subsidiary_Activity_Status" %in% req$F6)
})

test_that("NSS64 outcomes are frozen as near-treatment reference measures", {
  registry <- nss64_outcome_registry()
  expect_true("temporal_role" %in% names(registry))
  expect_identical(unique(registry$temporal_role), "near_treatment_reference")
})

test_that("NSS66 conversion contract pins one standard converter and three blocks", {
  contract <- read_nss66_conversion_contract("data/metadata/nss66_conversion_contract.csv")
  expect_identical(contract$file_id, c("F4", "F5", "F6"))
  expect_equal(contract$expected_rows, c(459784, 459784, 34689))
  expect_identical(unique(contract$converter_package), "nesstar-converter")
  expect_identical(unique(contract$converter_version), "1.0.4")
  expect_true(all(grepl("^data/interim/nss66_eus/F[456]\\.csv$", contract$relative_path)))
})

test_that("NSS66 canonical adapter joins F4/F5 one-to-one and F6 conditionally", {
  common <- data.frame(
    FSU_Serial_No = c("1001", "1002", "1003"), Sector = c("1", "2", "2"),
    State_Region = "011", District = c("01", "01", "02"), Stratum = "1",
    Sub_Stratum_No = c("1", "", ""), Sub_Round = "1", Sub_Sample = "1",
    Second_Stage_Stratum_No = "1", Sample_Hhld_No = c("1", "2", "3"),
    Person_Serial_No = "1", STATE = "01", DISTRICT_CODE = c("01", "01", "02"),
    HHID = c("h1", "h2", "h3"), PID = c("p1", "p2", "p3"), WEIGHT = c("10", "20", "30"),
    stringsAsFactors = FALSE
  )
  f4 <- cbind(common, Sex = c("1", "2", "1"), Age = c("20", "30", "40"),
              General_Education = "5", Technical_Education = "1")
  f5 <- cbind(common, Age = c("20", "30", "40"),
              Usual_Principal_Activity_Status = c("81", "31", "91"),
              Usual_Principal_Activity_NIC2004 = c("0", "100", "0"),
              Usual_Principal_Activity_NCO2004 = c("0", "200", "0"),
              Whether_in_Subsidiary_Activity = c("1", "2", "2"))
  f6 <- cbind(common[1, , drop = FALSE], Age = "20",
              Usual_Subsidiary_Activity_Status = "31",
              Usual_SubsidiaryActivity_NIC2004 = "100",
              Usual_SubsidiaryActivity_NCO2004 = "200")
  ddi <- data.frame(file_id = c("F4", "F5", "F6"), case_count = c(3, 3, 1))

  out <- build_nss66_usual_activity(f4, f5, f6, ddi)
  expect_equal(nrow(out), 3L)
  expect_identical(out$person_key, c("p1", "p2", "p3"))
  expect_equal(out$usual_subsidiary_status, c(31, NA, NA))
  expect_equal(out$sub_stratum, c("1", "__none__", "__none__"))
  expect_equal(out$survey_weight, c(10, 20, 30))

  bad_f6 <- f6
  bad_f6$PID <- "p2"
  expect_error(
    build_nss66_usual_activity(f4, f5, bad_f6),
    "F6 must equal the F5 persons coded as having subsidiary activity"
  )
})

test_that("NSS66 lineage reuses the reviewed same-round consumption bridge", {
  persons <- data.frame(
    person_key = c("p1", "p2", "p3"), state_code = c("01", "01", "02"),
    district_code = c("01", "02", "01"), stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    survey_id = rep("nss_2009_10_type2", 2),
    source_state_code = c("01", "01"), source_district_code = c("01", "02"),
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    lineage_weight = 1, lineage_status = c("resolved_exact_2001", "resolved_reviewed_identity_alias"),
    stringsAsFactors = FALSE
  )
  out <- attach_nss66_reviewed_lineage(persons, bridge)
  expect_equal(out$source_district_code, c("0101", "0102", "0201"))
  expect_equal(out$target_unit_2001[1:2], c("pc2001__01__01", "pc2001__01__02"))
  expect_equal(out$lineage_status[1:2], rep("resolved_reviewed_deterministic", 2))
  expect_equal(out$lineage_status[[3L]], "unresolved_source_district")
})

test_that("shared NSS labor registry separates near-treatment and early-post waves", {
  nss64 <- nss64_outcome_registry()
  nss66 <- nss66_outcome_registry()
  expect_equal(nrow(nss64), 5L)
  expect_equal(nrow(nss66), 4L)
  expect_identical(unique(nss64$temporal_role), "near_treatment_reference")
  expect_identical(unique(nss66$temporal_role), "early_post")
  expect_false("migrant_from_last_upr_share_age15plus" %in% nss66$outcome_id)
  expect_setequal(
    nss66$outcome_id,
    setdiff(nss64$outcome_id, "migrant_from_last_upr_share_age15plus")
  )
})
