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
