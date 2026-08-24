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
    reviewed = list(mapping = data.frame(), conflicts = data.frame())
  )
  out <- build_consumption_lineage_bridge(households, ref)
  expect_equal(out$lineage_status[out$source_district_code == "07"], "resolved_reviewed_identity_alias")
  expect_equal(out$lineage_status[out$source_district_code == "08"], "resolved_exact_2001")
  expect_true(all(out$target_unit_2001 == "pc2001__24__07"))
})
