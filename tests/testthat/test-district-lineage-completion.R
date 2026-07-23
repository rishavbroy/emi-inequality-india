test_that("adjudication drafts never auto-accept candidates", {
  roster <- data.frame(
    source_row_id = "s1", wave = "nss_2007_08", raw_state = "State",
    raw_district = "District", stringsAsFactors = FALSE
  )
  queue <- data.frame(
    source_row_id = "s1", recommended_unit = "pc2001__01__01",
    recommended_method = "exact_normalized_name",
    review_class = "cross_vintage_exact_candidate",
    recommended_vintage = "2001", stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    source_row_id = "s1", candidate_unit = "pc2001__01__01",
    candidate_source_id = "census_2001_c16", stringsAsFactors = FALSE
  )

  draft <- build_adjudication_draft_v2(roster, queue, candidates)

  expect_identical(draft$status, "needs_review")
  expect_identical(draft$unit_id, "pc2001__01__01")
  expect_match(draft$note, "Confirm administrative continuity")
})

test_that("sensitivity crosswalk preserves deterministic and reviewed weights", {
  primary <- data.frame(
    source_row_id = "s1", wave = "nss_2007_08", source_code = "101",
    target_unit_2001 = "pc2001__01__01", mapping_class = "one_to_one",
    stringsAsFactors = FALSE
  )
  weights <- data.frame(
    source_unit = c("s2", "s2"),
    target_2001 = c("pc2001__01__01", "pc2001__01__02"),
    weight = c(0.6, 0.4), basis = "population",
    source_id = "official_source", status = "accepted",
    stringsAsFactors = FALSE
  )

  out <- build_sensitivity_crosswalk_v2(primary, weights)

  expect_equal(sum(out$weight[out$source_row_id == "s1"]), 1)
  expect_equal(sum(out$weight[out$source_row_id == "s2"]), 1)
  expect_setequal(out$panel_variant, c("deterministic", "population_allocation"))
})

test_that("production comparison reports same, changed, and missing targets", {
  primary <- data.frame(
    source_row_id = c("s1", "s2", "s3"),
    wave = c("nss_2007_08", "nss_2007_08", "nss_2017_18"),
    source_code = c("101", "102", "999"),
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02", "pc2001__01__03"),
    stringsAsFactors = FALSE
  )
  panel <- data.frame(
    district_code_0708 = c("101", "102"),
    district_code_1718 = c("201", "202"),
    district_panel_id = c("2001__01__01", "2001__01__09"),
    stringsAsFactors = FALSE
  )

  out <- build_production_crosswalk_comparison_v2(primary, panel)

  expect_identical(
    out$comparison_status[match("s1", out$source_row_id)],
    "same_target"
  )
  expect_identical(
    out$comparison_status[match("s2", out$source_row_id)],
    "changed_target"
  )
  expect_identical(
    out$comparison_status[match("s3", out$source_row_id)],
    "missing_from_production_panel"
  )
})

test_that("completion status remains blocked without reviewed evidence", {
  status <- lineage_completion_steps_v2(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(
      source_row_id = character(), status = character()
    ),
    adjudication_queue = data.frame(
      review_class = "cross_vintage_exact_candidate",
      adjudication_status = NA_character_
    ),
    evidence_requests = data.frame(event_id = "e1"),
    allocation_validation = data.frame(
      source_key = "pc2011__01__001",
      coverage_complete = FALSE
    ),
    allocation_weights = data.frame(status = character()),
    primary_crosswalk = data.frame(),
    sensitivity_crosswalk = data.frame(),
    production_comparison = data.frame(comparison_status = character()),
    geometry_qa = data.frame(metric = "geometry_available", value = FALSE),
    readiness = data.frame(
      gate = "production_crosswalk_migration_ready", passed = FALSE
    )
  )

  expect_identical(status$step, seq_len(9L))
  expect_true(status$complete[status$step == 2L])
  expect_false(any(status$complete[status$step != 2L]))
  expect_true(all(nzchar(status$next_action)))
})

test_that("one accepted allocation cannot clear unrelated coverage gaps", {
  status <- lineage_completion_steps_v2(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(
      source_row_id = character(), status = character()
    ),
    adjudication_queue = data.frame(
      review_class = "cross_vintage_exact_candidate",
      adjudication_status = NA_character_
    ),
    evidence_requests = data.frame(),
    allocation_validation = data.frame(
      source_key = c("pc2011__01__001", "pc2011__01__002"),
      coverage_complete = c(FALSE, FALSE)
    ),
    allocation_weights = data.frame(
      source_unit = "pc2011__01__001",
      status = "accepted"
    ),
    primary_crosswalk = data.frame(),
    sensitivity_crosswalk = data.frame(),
    production_comparison = data.frame(comparison_status = character()),
    geometry_qa = data.frame(metric = "geometry_available", value = FALSE),
    readiness = data.frame(
      gate = "production_crosswalk_migration_ready", passed = FALSE
    )
  )

  expect_false(status$complete[status$step == 4L])
})

test_that("production review requires every accepted mapping to match", {
  common <- list(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(
      source_row_id = "s1", status = "accepted"
    ),
    adjudication_queue = data.frame(
      review_class = "cross_vintage_exact_candidate",
      adjudication_status = "accepted"
    ),
    evidence_requests = data.frame(),
    allocation_validation = data.frame(
      source_key = "pc2011__01__001",
      coverage_complete = TRUE
    ),
    allocation_weights = data.frame(
      source_unit = character(), status = character()
    ),
    primary_crosswalk = data.frame(source_row_id = "s1"),
    sensitivity_crosswalk = data.frame(source_row_id = "s1"),
    geometry_qa = data.frame(metric = "geometry_available", value = FALSE),
    readiness = data.frame(
      gate = "production_crosswalk_migration_ready", passed = FALSE
    )
  )

  missing <- do.call(
    lineage_completion_steps_v2,
    c(common, list(
      production_comparison = data.frame(
        comparison_status = "missing_from_production_panel"
      )
    ))
  )
  changed <- do.call(
    lineage_completion_steps_v2,
    c(common, list(
      production_comparison = data.frame(
        comparison_status = "changed_target"
      )
    ))
  )
  same <- do.call(
    lineage_completion_steps_v2,
    c(common, list(
      production_comparison = data.frame(
        comparison_status = "same_target"
      )
    ))
  )

  expect_false(missing$complete[missing$step == 8L])
  expect_false(changed$complete[changed$step == 8L])
  expect_true(same$complete[same$step == 8L])
})
