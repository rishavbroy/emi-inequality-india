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


test_that("directional district qualifiers cannot pass as interchangeable names", {
  expect_true(directional_tokens_compatible("Imphal East", "Imphal East"))
  expect_true(directional_tokens_compatible("Raigarh", "Raigad"))
  expect_false(directional_tokens_compatible("Imphal East", "Imphal West"))
  expect_false(directional_tokens_compatible("North District", "District"))

  skip_if_not_installed("stringdist")
  gold <- data.frame(
    source_name = c("Imphal East", "Imphal East"),
    reference_name = c("Imphal East", "Imphal West"),
    label = c("match", "nonmatch"),
    stringsAsFactors = FALSE
  )
  scored <- score_gold_set_v2(gold)

  expect_identical(scored$passes_name_rule, c(TRUE, FALSE))
  expect_equal(
    summarize_gold_set_v2(scored)$value[
      summarize_gold_set_v2(scored)$metric == "observed_nonmatch_acceptances"
    ],
    0
  )
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
    shrid_coverage = 1,
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


test_that("fuzzy ranking compares distinct names and prefers the survey-relevant vintage", {
  skip_if_not_installed("stringdist")
  roster <- data.frame(
    source_row_id = "source",
    wave = "nss_2007_08",
    source_code = "1",
    raw_state = "State",
    raw_district = "Anantpur",
    state_std = "state",
    district_std = "anantpur",
    stringsAsFactors = FALSE
  )
  references <- data.frame(
    unit_id = c("lgd__1", "pc2011__1", "pc2001__1", "pc2001__2"),
    reference_vintage = c("current_lgd", "2011", "2001", "2001"),
    state_std = "state",
    district_std = c("anantapur", "anantapur", "anantapur", "another"),
    source_id = c("lgd", "pc2011", "pc2001", "pc2001"),
    stringsAsFactors = FALSE
  )

  out <- score_match_candidates_v2(roster, references)
  same_name <- out[out$candidate_name == "anantapur", , drop = FALSE]

  expect_true(all(same_name$rank == 1L))
  expect_equal(unique(same_name$margin), same_name$score[[1]] - out$score[out$rank == 2L][[1]])
  expect_equal(
    same_name$candidate_unit[same_name$high_precision_candidate],
    "pc2001__1"
  )
})

test_that("adjudication recommendations use wave-specific vintage preferences", {
  roster <- data.frame(
    source_row_id = c("early", "late"),
    wave = c("nss_2007_08", "nss_2017_18"),
    source_code = c("1", "2"),
    raw_state = "State",
    raw_district = "District",
    state_std = "state",
    district_std = "district",
    stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    source_row_id = rep(c("early", "late"), each = 3),
    wave = rep(c("nss_2007_08", "nss_2017_18"), each = 3),
    candidate_unit = rep(c("lgd", "pc2011", "pc2001"), 2),
    candidate_name = "district",
    reference_vintage = rep(c("current_lgd", "2011", "2001"), 2),
    candidate_method = "exact_normalized_name",
    rank = 1L,
    score = 1,
    high_precision_candidate = FALSE,
    stringsAsFactors = FALSE
  )

  out <- build_source_adjudication_queue_v2(roster, candidates)
  recommended <- stats::setNames(out$recommended_unit, out$source_row_id)

  expect_equal(recommended[["early"]], "pc2001")
  expect_equal(recommended[["late"]], "pc2011")
})

test_that("source adjudication queue prioritizes deterministic review work", {
  roster <- data.frame(
    source_row_id = c("single", "multiple", "fuzzy", "none"),
    wave = "nss_2007_08", source_code = c("1", "2", "3", "4"),
    raw_state = "State", raw_district = c("A", "B", "C", "D"),
    state_std = "state", district_std = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    source_row_id = c("single", "multiple", "multiple", "fuzzy"),
    candidate_unit = c("u1", "u2", "u3", "u4"),
    candidate_name = c("a", "b", "b", "cee"),
    reference_vintage = c("2001", "2001", "2011", "2001"),
    candidate_method = c(
      "exact_normalized_name", "exact_normalized_name",
      "exact_normalized_name", "fuzzy_name_candidate"
    ),
    rank = c(1L, 1L, 2L, 1L), score = c(1, 1, 1, 0.9),
    high_precision_candidate = c(FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  out <- build_source_adjudication_queue_v2(roster, candidates)
  classes <- stats::setNames(out$review_class, out$source_row_id)

  expect_equal(classes[["single"]], "single_vintage_exact_candidate")
  expect_equal(classes[["multiple"]], "cross_vintage_exact_candidate")
  expect_equal(classes[["fuzzy"]], "high_precision_fuzzy_candidate")
  expect_equal(classes[["none"]], "no_candidate")
  expect_equal(out$recommended_unit[out$source_row_id == "multiple"], "u2")
  expect_equal(out$candidate_name_count[out$source_row_id == "multiple"], 1L)
  expect_equal(out$exact_vintage_count[out$source_row_id == "multiple"], 2L)
  expect_true(all(diff(out$review_priority) >= 0))
})

test_that("NSS-64 compact codes separate region from district", {
  expect_identical(
    nss64_census2001_unit_id_v2(
      c("07101", "28216", "35302", "56611")
    ),
    c(
      "pc2001__07__01",
      "pc2001__28__16",
      "pc2001__35__02",
      "pc2001__56__11"
    )
  )
  expect_true(all(is.na(
    nss64_census2001_unit_id_v2(
      c("0711", "071001", "07A01", NA)
    )
  )))
})

test_that("NSS-64 code resolution requires a known Census-2001 unit", {
  admin <- data.frame(
    unit_id = c("pc2001__07__01", "pc2001__35__02"),
    stringsAsFactors = FALSE
  )

  expect_identical(
    resolve_nss64_census2001_unit_id_v2(
      c("07101", "35302", "28211", "invalid"),
      admin
    ),
    c(
      "pc2001__07__01",
      "pc2001__35__02",
      NA_character_,
      NA_character_
    )
  )
})

test_that("NSS-64 code resolution declares its registry schema", {
  expect_error(
    resolve_nss64_census2001_unit_id_v2(
      "07101",
      data.frame(state_code = "07", district_code = "01")
    ),
    "missing required columns: unit_id"
  )
})

test_that("deterministic transitions require complete one-to-one coverage", {
  transition <- data.frame(
    state_code_2011 = c("01", "01", "01"),
    district_code_2011 = c("001", "002", "003"),
    state_code_2001 = c("01", "01", "01"),
    district_code_2001 = c("01", "02", "03"),
    population_share_to_2001 = c(1, 0.99, 1),
    shrid_coverage = c(1, 1, 0.99),
    mapping_class = rep("deterministic_containment", 3),
    stringsAsFactors = FALSE
  )

  out <- deterministic_transition_2011_to_2001_v2(transition)

  expect_equal(nrow(out), 1L)
  expect_identical(out$district_code_2011, "001")
  expect_identical(out$district_code_2001, "01")
})

test_that("deterministic transitions reject duplicate source targets", {
  transition <- data.frame(
    state_code_2011 = c("01", "01"),
    district_code_2011 = c("001", "001"),
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("01", "02"),
    population_share_to_2001 = c(1, 1),
    shrid_coverage = c(1, 1),
    mapping_class = rep("deterministic_containment", 2),
    stringsAsFactors = FALSE
  )

  expect_error(
    deterministic_transition_2011_to_2001_v2(transition),
    "one target per source"
  )
})

test_that("empty transition inputs yield no deterministic bridges", {
  out <- deterministic_transition_2011_to_2001_v2(data.frame())

  expect_equal(nrow(out), 0L)
  expect_identical(
    names(out),
    c(
      "state_code_2011", "district_code_2011",
      "state_code_2001", "district_code_2001",
      "population_share_to_2001", "shrid_coverage", "mapping_class"
    )
  )
})
