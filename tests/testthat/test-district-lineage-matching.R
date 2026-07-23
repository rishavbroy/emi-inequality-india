test_that("exact names remain review candidates rather than accepted matches", {
  source <- data.frame(
    source_row_id = "s1", wave = "nss_2017_18", source_code = "17",
    state_std = "manipur", district_std = "imphal east",
    stringsAsFactors = FALSE
  )
  reference <- data.frame(
    unit_id = "u1", level = "district", state_code = "14", district_code = "277",
    state_std = "manipur", district_std = "imphal east",
    valid_from = NA_character_, valid_to = NA_character_, source_id = "fixture",
    reference_vintage = "2011", stringsAsFactors = FALSE
  )

  out <- exact_source_candidates_v2(source, reference)

  expect_equal(out$candidate_unit, "u1")
  expect_equal(out$candidate_method, "exact_normalized_name")
  expect_equal(out$candidate_source_id, "fixture")
  expect_false("status" %in% names(out))
  expect_false(out$high_precision_candidate)
})

test_that("fuzzy candidate scoring ranks close names but never adjudicates them", {
  skip_if_not_installed("stringdist")
  source <- data.frame(
    source_row_id = c("s1", "s2"),
    wave = "nss_2017_18",
    source_code = c("1", "2"),
    state_std = c("uttar pradesh", "manipur"),
    district_std = c("farukhabad", "imphal e"),
    stringsAsFactors = FALSE
  )
  reference <- data.frame(
    unit_id = c("u1", "u2", "u3", "u4"),
    level = "district", state_code = NA_character_, district_code = NA_character_,
    state_std = c("uttar pradesh", "uttar pradesh", "manipur", "manipur"),
    district_std = c("farrukhabad", "firozabad", "imphal east", "imphal west"),
    valid_from = NA_character_, valid_to = NA_character_, source_id = "fixture",
    reference_vintage = "2011", stringsAsFactors = FALSE
  )

  out <- score_match_candidates_v2(source, reference)
  best <- out[out$rank == 1L, , drop = FALSE]

  expect_equal(best$candidate_name[best$source_row_id == "s1"], "farrukhabad")
  expect_equal(best$candidate_name[best$source_row_id == "s2"], "imphal east")
  expect_false(best$high_precision_candidate[best$source_row_id == "s2"])
  expect_false("status" %in% names(out))
})

test_that("tracked adjudications are unique and reference known units", {
  reference <- data.frame(unit_id = "u1", reference_vintage = "2001")
  accepted <- data.frame(
    source_row_id = "s1", wave = "nss_2007_08", raw_state = "state",
    raw_district = "district", unit_id = "u1", method = "official_code",
    source_id = "official", status = "accepted", note = NA_character_
  )

  out <- build_adjudicated_source_matches_v2(accepted, reference)

  expect_equal(out$reference_vintage, "2001")
  expect_error(
    read_adjudicated_source_matches_v2(rbind(accepted, accepted)),
    "at most one row"
  )
  accepted$unit_id <- "unknown"
  expect_error(
    build_adjudicated_source_matches_v2(accepted, reference),
    "unknown units"
  )
})

test_that("primary eligibility accepts only adjudicated deterministic mappings", {
  roster <- data.frame(
    source_row_id = c("a", "b", "c"),
    wave = c("nss_2007_08", "nss_2017_18", "nss_2017_18"),
    source_code = c("1", "2", "3"),
    state_std = "state",
    district_std = c("old", "child", "ambiguous"),
    stringsAsFactors = FALSE
  )
  matches <- data.frame(
    source_row_id = c("a", "b", "c"), wave = roster$wave,
    raw_state = "state", raw_district = roster$district_std,
    unit_id = c("pc2001__01__01", "pc2011__01__010", "pc2011__01__011"),
    reference_vintage = c("2001", "2011", "2011"),
    method = "manual", source_id = "official", status = "accepted", note = NA_character_,
    stringsAsFactors = FALSE
  )
  a01 <- data.frame(unit_id = "pc2001__01__01", state_code = "01", district_code = "01")
  a11 <- data.frame(
    unit_id = c("pc2011__01__010", "pc2011__01__011"),
    state_code = "01", district_code = c("010", "011")
  )
  transition <- data.frame(
    state_code_2011 = "01", district_code_2011 = "010",
    state_code_2001 = "01", district_code_2001 = "01",
    population_share_to_2001 = 1,
    mapping_class = "deterministic_containment"
  )

  out <- build_primary_mapping_eligibility(roster, matches, transition, a01, a11)

  expect_identical(out$eligible_primary[match(c("a", "b", "c"), out$source_row_id)], c(TRUE, TRUE, FALSE))
  expect_equal(
    out$target_unit_2001[match(c("a", "b"), out$source_row_id)],
    c("pc2001__01__01", "pc2001__01__01")
  )
  expect_equal(out$exclusion_reason[out$source_row_id == "c"], "geographic_transition_non_nested_or_incomplete")
})

test_that("duplicate diagnostics distinguish identical from conflicting rows", {
  x <- data.frame(id = c("a", "a", "b", "b"), value = c("x", "x", "y", "z"))
  out <- duplicate_key_diagnostics_v2(x, "id", "fixture")

  expect_equal(out$duplicate_type[out$key == "a"], "identical")
  expect_equal(out$duplicate_type[out$key == "b"], "conflicting")
})


test_that("NSS roster IDs distinguish conflicting labels on one source key", {
  raw <- data.frame(
    district_code_1718 = c("01101", "01101"),
    state_std = c("state", "state"),
    district_std = c("alpha", "beta"),
    stringsAsFactors = FALSE
  )

  out <- build_nss_district_roster_v2(data.frame(), raw)
  duplicate <- duplicate_key_diagnostics_v2(out, "source_key", "fixture")

  expect_equal(length(unique(out$source_row_id)), 2L)
  expect_equal(length(unique(out$source_key)), 1L)
  expect_equal(duplicate$duplicate_type, "conflicting")
})

test_that("unadjudicated sources are explicitly ineligible", {
  roster <- data.frame(
    source_row_id = "s1", wave = "nss_2017_18", source_code = "1",
    state_std = "state", district_std = "district", stringsAsFactors = FALSE
  )
  a01 <- data.frame(unit_id = "u1", state_code = "01", district_code = "01")
  a11 <- data.frame(unit_id = "u2", state_code = "01", district_code = "001")

  out <- build_primary_mapping_eligibility(
    roster, empty_source_matches_v2(), data.frame(), a01, a11
  )

  expect_false(out$eligible_primary)
  expect_equal(out$exclusion_reason, "source_identity_unadjudicated")
})


test_that("accepted administrative edges resolve later units backward", {
  a01 <- data.frame(unit_id = "pc2001__01__01", state_code = "01", district_code = "01")
  a11 <- data.frame(unit_id = "pc2011__01__010", state_code = "01", district_code = "010")
  events <- data.frame(
    from_unit = "pc2011__01__010", to_unit = "lgd_district__900",
    status = "accepted", stringsAsFactors = FALSE
  )

  out <- resolve_lineage_terminals_v2("lgd_district__900", events, a01, a11)

  expect_equal(out$terminal_unit, "pc2011__01__010")
  expect_equal(out$terminal_vintage, "2011")
  expect_equal(out$resolution_status, "resolved")
})

test_that("multiple accepted parents block deterministic backward mapping", {
  a01 <- data.frame(unit_id = c("a", "b"), state_code = "01", district_code = c("01", "02"))
  a11 <- data.frame(unit_id = character(), state_code = character(), district_code = character())
  events <- data.frame(
    from_unit = c("a", "b"), to_unit = c("child", "child"),
    status = c("accepted", "accepted"), stringsAsFactors = FALSE
  )

  out <- resolve_lineage_terminals_v2("child", events, a01, a11)

  expect_equal(out$resolution_status, "multiple_parent_non_nested")
  expect_true(is.na(out$terminal_unit))
})

test_that("needs-review identities remain ineligible even when a candidate unit is known", {
  roster <- data.frame(
    source_row_id = "s1", wave = "nss_2017_18", source_code = "1",
    state_std = "state", district_std = "district", stringsAsFactors = FALSE
  )
  matches <- data.frame(
    source_row_id = "s1", wave = "nss_2017_18", raw_state = "state",
    raw_district = "district", unit_id = "pc2001__01__01",
    reference_vintage = "2001", method = "candidate_review",
    source_id = "official", status = "needs_review", note = NA_character_,
    stringsAsFactors = FALSE
  )
  admin <- data.frame(unit_id = "pc2001__01__01", state_code = "01", district_code = "01")

  out <- build_primary_mapping_eligibility(
    roster, matches, data.frame(), admin, data.frame()
  )

  expect_false(out$eligible_primary)
  expect_equal(out$exclusion_reason, "source_identity_unadjudicated")
})


test_that("primary crosswalk exports only complete deterministic mappings", {
  eligibility <- data.frame(
    source_row_id = c("accepted", "excluded"),
    wave = c("nss_2007_08", "nss_2017_18"),
    source_code = c("1", "2"),
    raw_state = c("State", "State"),
    raw_district = c("Old", "Ambiguous"),
    state_std = c("state", "state"),
    district_std = c("old", "ambiguous"),
    target_unit_2001 = c("pc2001__01__01", NA),
    target_state_code_2001 = c("01", NA),
    target_district_code_2001 = c("01", NA),
    mapping_class = c("identity_or_documented_rename_to_2001", "unresolved_or_non_nested"),
    eligible_primary = c(TRUE, FALSE),
    exclusion_reason = c(NA, "source_identity_unadjudicated"),
    stringsAsFactors = FALSE
  )

  crosswalk <- build_primary_source_crosswalk_v2(eligibility)
  excluded <- build_excluded_source_rows_v2(eligibility)

  expect_equal(crosswalk$source_row_id, "accepted")
  expect_equal(crosswalk$target_unit_2001, "pc2001__01__01")
  expect_equal(excluded$source_row_id, "excluded")
  expect_equal(excluded$exclusion_reason, "source_identity_unadjudicated")
})

test_that("NSS roster preserves equivalent raw aliases without duplicating source rows", {
  raw <- data.frame(
    district_code_1718 = c("01101", "01101"),
    state_1718 = c("State", "State"),
    district_1718 = c("Lahul & Spiti", "Lahaul and Spiti"),
    state_std = c("state", "state"),
    district_std = c("lahaul and spiti", "lahaul and spiti"),
    stringsAsFactors = FALSE
  )

  out <- build_nss_district_roster_v2(data.frame(), raw)

  expect_equal(nrow(out), 1L)
  expect_match(out$raw_district, "Lahul & Spiti", fixed = TRUE)
  expect_match(out$raw_district, "Lahaul and Spiti", fixed = TRUE)
})


test_that("blank source adjudications remain empty and typed", {
  out <- read_adjudicated_source_matches_v2(data.frame())

  expect_equal(nrow(out), 0L)
  expect_named(
    out,
    c("source_row_id", "wave", "raw_state", "raw_district", "unit_id",
      "method", "source_id", "status", "note")
  )
})
