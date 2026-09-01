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
