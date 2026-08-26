test_that("consumption lineage accepts exact identities and stable reviewed mappings only", {
  households <- data.frame(
    survey_id = "wave", household_id = c("h1", "h2", "h3", "h4"),
    source_state_code = "01", source_district_code = c("01", "02", "03", "98"),
    state_std = "state", district_std = c("old", "renamed", "conflict", "aggregate"),
    source_unit_kind = c("district", "district", "district", "aggregate"),
    source_lineage_eligible = c(TRUE, TRUE, TRUE, FALSE),
    survey_weight = 10, household_size = 2, stringsAsFactors = FALSE
  )
  admin <- data.frame(
    unit_id = "pc2001__01__01", state_std = "state", district_std = "old",
    stringsAsFactors = FALSE
  )
  roster <- data.frame(
    source_row_id = paste0("r", 1:4),
    wave = c("2007", "2017", "2007", "2017"),
    state_std = "state",
    district_std = c("renamed", "renamed", "conflict", "conflict"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_row_id = c("r1", "r1", "r2", "r2", "r3", "r4"),
    target_unit_2001 = c("pc2001__01__02", "pc2001__01__03", "pc2001__01__02", "pc2001__01__03", "pc2001__01__04", "pc2001__01__05"),
    weight = c(.4, .6, .4, .6, 1, 1),
    basis = "reviewed", panel_variant = "population_allocation",
    stringsAsFactors = FALSE
  )

  reference <- build_consumption_lineage_reference(admin, roster, crosswalk)
  bridge <- build_consumption_lineage_bridge(households, reference)

  exact <- bridge[bridge$source_district_code == "01", ]
  renamed <- bridge[bridge$source_district_code == "02", ]
  conflict <- bridge[bridge$source_district_code == "03", ]
  aggregate <- bridge[bridge$source_district_code == "98", ]
  expect_equal(exact$target_unit_2001, "pc2001__01__01")
  expect_equal(exact$lineage_status, "resolved_exact_2001")
  expect_equal(sort(renamed$lineage_weight), c(.4, .6))
  expect_true(all(renamed$lineage_status == "resolved_reviewed_consensus"))
  expect_equal(conflict$lineage_status, "reviewed_lineage_conflict")
  expect_true(is.na(conflict$target_unit_2001))
  expect_equal(aggregate$lineage_status, "source_not_lineage_eligible")
})

test_that("consumption lineage expansion conserves survey and person weight", {
  households <- data.frame(
    survey_id = "wave", household_id = "h1",
    source_state_code = "01", source_district_code = "02",
    state_std = "state", district_std = "renamed",
    source_unit_kind = "district", source_lineage_eligible = TRUE,
    survey_weight = 10, household_size = 3, real_mpce = 50,
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    survey_id = "wave", source_state_code = "01", source_district_code = "02",
    target_unit_2001 = c("a", "b"), lineage_weight = c(.25, .75),
    lineage_basis = "reviewed", lineage_status = "resolved_reviewed_consensus",
    stringsAsFactors = FALSE
  )

  out <- attach_consumption_lineage(households, bridge)

  expect_equal(sum(out$lineage_survey_weight), 10)
  expect_equal(sum(out$lineage_person_weight), 30)
  expect_equal(out$real_mpce, c(50, 50))
})

test_that("consumption lineage attachment never silently drops uncovered geography", {
  households <- data.frame(
    survey_id = "wave", household_id = "h1",
    source_state_code = "01", source_district_code = "01",
    survey_weight = 1, household_size = 1, stringsAsFactors = FALSE
  )
  bridge <- empty_consumption_lineage_bridge()
  expect_error(
    attach_consumption_lineage(households, bridge),
    "does not cover every source household geography"
  )
})

test_that("consumption lineage handles fully resolved rosters without zero-row assignment errors", {
  households <- data.frame(
    survey_id = "wave", household_id = "h1",
    source_state_code = "01", source_district_code = "01",
    state_std = "state", district_std = "old",
    source_unit_kind = "district", source_lineage_eligible = TRUE,
    survey_weight = 1, household_size = 1, stringsAsFactors = FALSE
  )
  reference <- build_consumption_lineage_reference(
    data.frame(
      unit_id = "pc2001__01__01", state_std = "state", district_std = "old",
      stringsAsFactors = FALSE
    ),
    data.frame(
      source_row_id = character(), wave = character(),
      state_std = character(), district_std = character(),
      stringsAsFactors = FALSE
    ),
    data.frame(
      source_row_id = character(), target_unit_2001 = character(),
      weight = numeric(), basis = character(), panel_variant = character(),
      stringsAsFactors = FALSE
    )
  )

  bridge <- build_consumption_lineage_bridge(households, reference)

  expect_equal(nrow(bridge), 1L)
  expect_equal(bridge$lineage_status, "resolved_exact_2001")
  expect_equal(bridge$lineage_weight, 1)
})

test_that("consumption lineage review queue saver uses the pipeline API", {
  path <- tempfile(fileext = ".csv")
  queue <- data.frame(
    survey_id = "wave", source_state_code = "01", source_district_code = "02",
    state_std = "state", district_std = "unresolved", source_unit_kind = "district",
    lineage_status = "unresolved_no_stable_lineage", stringsAsFactors = FALSE
  )

  out <- save_consumption_lineage_review_queue(queue, path)

  expect_true(file.exists(path))
  expect_identical(normalizePath(out), normalizePath(path))
})

test_that("reviewed consumption identity aliases resolve only to known Census-2001 districts", {
  admin <- data.frame(
    unit_id = "pc2001__24__07", state_std = "gujarat", district_std = "ahmadabad",
    stringsAsFactors = FALSE
  )
  exact <- exact_census_2001_identity_lineage(admin)
  aliases <- data.frame(
    state_std = "gujarat", source_district_std = "ahmedabad",
    target_district_std = "ahmadabad", basis = "orthographic_variant",
    stringsAsFactors = FALSE
  )
  out <- consumption_identity_alias_lineage(aliases, exact)
  expect_equal(out$district_std, "ahmedabad")
  expect_equal(out$target_unit_2001, "pc2001__24__07")
  expect_equal(out$lineage_weight, 1)
  expect_match(out$lineage_basis, "reviewed_identity_alias")

  aliases$target_district_std <- "not a census district"
  expect_error(consumption_identity_alias_lineage(aliases, exact), "unknown Census-2001")
})

test_that("identity aliases resolve before cross-wave lineage and never override exact identities", {
  households <- data.frame(
    survey_id = "nss_test", source_state_code = c("24", "24"),
    source_district_code = c("07", "08"), state_std = "gujarat",
    district_std = c("ahmedabad", "ahmadabad"), source_unit_kind = "district",
    source_lineage_eligible = TRUE, stringsAsFactors = FALSE
  )
  admin <- data.frame(
    unit_id = "pc2001__24__07", state_std = "gujarat", district_std = "ahmadabad",
    stringsAsFactors = FALSE
  )
  aliases <- data.frame(
    state_std = "gujarat", source_district_std = "ahmedabad",
    target_district_std = "ahmadabad", basis = "orthographic_variant",
    stringsAsFactors = FALSE
  )
  ref <- list(
    exact = exact_census_2001_identity_lineage(admin),
    aliases = consumption_identity_alias_lineage(aliases, exact_census_2001_identity_lineage(admin)),
    administrative = data.frame(),
    reviewed = list(mapping = data.frame(), conflicts = data.frame())
  )
  out <- build_consumption_lineage_bridge(households, ref)
  expect_equal(out$lineage_status[out$source_district_code == "07"], "resolved_reviewed_identity_alias")
  expect_equal(out$lineage_status[out$source_district_code == "08"], "resolved_exact_2001")
  expect_true(all(out$target_unit_2001 == "pc2001__24__07"))
})

test_that("consumption lineage reuses deterministic current-LGD Census-code ancestry", {
  admin_2001 <- data.frame(
    unit_id = "pc2001__20__04", state_code = "20", district_code = "04",
    state_std = "jharkhand", district_std = "hazaribagh",
    stringsAsFactors = FALSE
  )
  admin_2011 <- data.frame(
    unit_id = "pc2011__20__361", state_code = "20", district_code = "361",
    district_std = "ramgarh", stringsAsFactors = FALSE
  )
  reference <- data.frame(
    unit_id = "lgd_district__607", level = "district",
    state_code = "20", district_code = "361",
    state_std = "jharkhand", district_std = "ramgarh",
    reference_vintage = "current_lgd", stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = "20", district_code_2011 = "361",
    state_code_2001 = "20", district_code_2001 = "04",
    population_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    stringsAsFactors = FALSE
  )

  out <- consumption_admin_transition_lineage(
    reference, data.frame(), admin_2001, admin_2011, transition
  )

  expect_equal(out$district_std, "ramgarh")
  expect_equal(out$target_unit_2001, "pc2001__20__04")
  expect_equal(out$lineage_weight, 1)
  expect_match(out$lineage_basis, "current_lgd_census2011_code")
})

test_that("consumption lineage reuses accepted single-parent administrative events", {
  admin_2001 <- data.frame(
    unit_id = "pc2001__22__11", state_code = "22", district_code = "11",
    state_std = "chhattisgarh", district_std = "raipur",
    stringsAsFactors = FALSE
  )
  admin_2011 <- data.frame(
    unit_id = "pc2011__22__410", state_code = "22", district_code = "410",
    district_std = "raipur", stringsAsFactors = FALSE
  )
  reference <- data.frame(
    unit_id = "lgd_district__645", level = "district",
    state_code = NA_character_, district_code = NA_character_,
    state_std = "chhattisgarh", district_std = "gariyaband",
    reference_vintage = "current_lgd", stringsAsFactors = FALSE
  )
  events <- data.frame(
    from_unit = "pc2011__22__410", to_unit = "lgd_district__645",
    status = "accepted", stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = "22", district_code_2011 = "410",
    state_code_2001 = "22", district_code_2001 = "11",
    population_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    stringsAsFactors = FALSE
  )

  out <- consumption_admin_transition_lineage(
    reference, events, admin_2001, admin_2011, transition
  )

  expect_equal(out$target_unit_2001, "pc2001__22__11")
  expect_match(out$lineage_basis, "accepted_admin_event_parentage")
})

test_that("consumption administrative lineage keeps ambiguous and non-deterministic ancestry unresolved", {
  admin_2001 <- data.frame(
    unit_id = c("pc2001__28__06", "pc2001__28__07"),
    state_code = "28", district_code = c("06", "07"),
    state_std = "andhra pradesh", district_std = c("parent a", "parent b"),
    stringsAsFactors = FALSE
  )
  admin_2011 <- data.frame(
    unit_id = c("pc2011__28__537", "pc2011__28__538"),
    state_code = "28", district_code = c("537", "538"),
    district_std = c("parent a", "parent b"), stringsAsFactors = FALSE
  )
  reference <- data.frame(
    unit_id = "lgd_district__698", level = "district",
    state_code = NA_character_, district_code = NA_character_,
    state_std = "telangana", district_std = "vikarabad",
    reference_vintage = "current_lgd", stringsAsFactors = FALSE
  )
  events <- data.frame(
    from_unit = c("pc2011__28__537", "pc2011__28__538"),
    to_unit = "lgd_district__698", status = "accepted",
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("28", "28"),
    district_code_2011 = c("537", "538"),
    state_code_2001 = c("28", "28"),
    district_code_2001 = c("06", "07"),
    population_share_to_2001 = c(.999, .999),
    shrid_coverage = 1,
    mapping_class = "non_nested_or_incomplete",
    stringsAsFactors = FALSE
  )

  out <- consumption_admin_transition_lineage(
    reference, events, admin_2001, admin_2011, transition
  )

  expect_equal(nrow(out), 0L)
})

test_that("administrative lineage resolves before cross-wave consensus", {
  households <- data.frame(
    survey_id = "hces_test", source_state_code = "20",
    source_district_code = "01", state_std = "jharkhand",
    district_std = "ramgarh", source_unit_kind = "district",
    source_lineage_eligible = TRUE, stringsAsFactors = FALSE
  )
  ref <- list(
    exact = data.frame(
      state_std = "jharkhand", district_std = "hazaribagh",
      target_unit_2001 = "pc2001__20__04", lineage_weight = 1,
      lineage_basis = "exact_census_2001_identity", stringsAsFactors = FALSE
    ),
    aliases = data.frame(),
    administrative = data.frame(
      state_std = "jharkhand", district_std = "ramgarh",
      target_unit_2001 = "pc2001__20__04", lineage_weight = 1,
      lineage_basis = "reviewed_admin_ancestry:test", stringsAsFactors = FALSE
    ),
    reviewed = list(
      mapping = data.frame(
        state_std = "jharkhand", district_std = "ramgarh",
        target_unit_2001 = "pc2001__20__99", lineage_weight = 1,
        lineage_basis = "reviewed_crosswave_consensus:test",
        stringsAsFactors = FALSE
      ),
      conflicts = data.frame()
    )
  )

  out <- build_consumption_lineage_bridge(households, ref)

  expect_equal(out$target_unit_2001, "pc2001__20__04")
  expect_equal(out$lineage_status, "resolved_reviewed_admin_ancestry")
})
