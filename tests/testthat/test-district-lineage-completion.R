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


test_that("adjudication drafts pair evidence with the recommended source row", {
  roster <- data.frame(
    source_row_id = c("s1", "s2"),
    wave = "nss_2007_08",
    raw_state = "State",
    raw_district = c("One", "Two"),
    stringsAsFactors = FALSE
  )
  queue <- data.frame(
    source_row_id = c("s1", "s2"),
    recommended_unit = c("u1", "u2"),
    recommended_method = "exact_normalized_name",
    review_class = "cross_vintage_exact_candidate",
    recommended_vintage = "2001",
    adjudication_status = NA_character_,
    stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    source_row_id = c("s1", "s1", "s2"),
    candidate_unit = c("u1", "u2", "u2"),
    candidate_source_id = c("source-for-s1", "wrong-pair", "source-for-s2"),
    stringsAsFactors = FALSE
  )

  draft <- build_adjudication_draft_v2(roster, queue, candidates)

  expect_identical(
    draft$source_id[match(c("s1", "s2"), draft$source_row_id)],
    c("source-for-s1", "source-for-s2")
  )
})

test_that("resolved identities disappear from the generated review draft", {
  roster <- data.frame(
    source_row_id = c("s1", "s2"),
    wave = "nss_2007_08",
    raw_state = "State",
    raw_district = c("One", "Two"),
    stringsAsFactors = FALSE
  )
  queue <- data.frame(
    source_row_id = c("s1", "s2"),
    recommended_unit = c("u1", "u2"),
    recommended_method = "exact_normalized_name",
    review_class = "cross_vintage_exact_candidate",
    recommended_vintage = "2001",
    adjudication_status = c("accepted", NA_character_),
    stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    source_row_id = c("s1", "s2"),
    candidate_unit = c("u1", "u2"),
    candidate_source_id = c("source-1", "source-2"),
    stringsAsFactors = FALSE
  )

  draft <- build_adjudication_draft_v2(roster, queue, candidates)

  expect_identical(draft$source_row_id, "s2")
})

test_that("production comparison flags ambiguous legacy mappings without row expansion", {
  primary <- data.frame(
    source_row_id = "s1", wave = "nss_2007_08", source_code = "101",
    target_unit_2001 = "pc2001__01__01", stringsAsFactors = FALSE
  )
  panel <- data.frame(
    district_code_0708 = c("101", "101"),
    district_code_1718 = c("201", "202"),
    district_panel_id = c("2001__01__01", "2001__01__02"),
    stringsAsFactors = FALSE
  )

  out <- build_production_crosswalk_comparison_v2(primary, panel)

  expect_equal(nrow(out), 1L)
  expect_identical(out$comparison_status, "ambiguous_production_mapping")
})

test_that("accepted allocations may include resolved complete sources", {
  status <- lineage_completion_steps_v2(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = character(), status = character()),
    adjudication_queue = data.frame(
      review_class = "cross_vintage_exact_candidate",
      adjudication_status = NA_character_
    ),
    evidence_requests = data.frame(),
    allocation_validation = data.frame(
      source_key = c("gap", "complete"),
      coverage_complete = c(FALSE, TRUE)
    ),
    allocation_weights = data.frame(
      source_unit = c("gap", "complete"),
      status = c("accepted", "accepted")
    ),
    primary_crosswalk = data.frame(),
    sensitivity_crosswalk = data.frame(),
    production_comparison = data.frame(comparison_status = character()),
    geometry_qa = data.frame(metric = "geometry_available", value = FALSE),
    readiness = data.frame(
      gate = "production_crosswalk_migration_ready", passed = FALSE
    )
  )

  expect_true(status$complete[status$step == 4L])
})

test_that("geometry dissolve is independent of the sf geometry-column name", {
  skip_if_not_installed("sf")

  square <- function(xmin, ymin) {
    sf::st_polygon(list(rbind(
      c(xmin, ymin), c(xmin + 1, ymin),
      c(xmin + 1, ymin + 1), c(xmin, ymin + 1),
      c(xmin, ymin)
    )))
  }
  polygons <- sf::st_sfc(square(0, 0), square(1, 0), crs = 4326)
  geometry <- sf::st_sf(
    shrid2 = c("a", "b"),
    geom = polygons,
    sf_column_name = "geom"
  )
  bridge <- data.frame(
    shrid2 = c("a", "b"),
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("001", "001"),
    deterministic_2001 = TRUE,
    stringsAsFactors = FALSE
  )

  out <- dissolve_shrid_geometry_2001_v2(geometry, bridge)

  expect_s3_class(out, "sf")
  expect_identical(out$unit_id, "pc2001__01__001")
  expect_equal(nrow(out), 1L)
  expect_true(sf::st_is_valid(out)[[1]])
})

test_that("geometry validity repair fixes an invalid polygon", {
  skip_if_not_installed("sf")

  bowtie <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 1), c(1, 0), c(0, 1), c(0, 0)
  )))
  x <- sf::st_sf(
    unit_id = "u1",
    geom = sf::st_sfc(bowtie),
    sf_column_name = "geom"
  )

  expect_false(sf::st_is_valid(x)[[1]])
  repaired <- make_valid_sf_v2(x)
  expect_true(sf::st_is_valid(repaired)[[1]])
})

test_that("geometry completion reports constructed but incomplete QA", {
  status <- lineage_completion_steps_v2(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = character(), status = character()),
    adjudication_queue = data.frame(
      review_class = "cross_vintage_exact_candidate",
      adjudication_status = NA_character_
    ),
    evidence_requests = data.frame(),
    allocation_validation = data.frame(
      source_key = "source",
      coverage_complete = TRUE
    ),
    allocation_weights = data.frame(
      source_unit = character(), status = character()
    ),
    primary_crosswalk = data.frame(),
    sensitivity_crosswalk = data.frame(),
    production_comparison = data.frame(comparison_status = character()),
    geometry_qa = data.frame(
      metric = c(
        "geometry_available", "geometry_rows", "expected_admin_units",
        "missing_admin_units", "unexpected_geometry_units",
        "invalid_geometries"
      ),
      value = c(TRUE, 582, 593, 11, 0, 0)
    ),
    readiness = data.frame(
      gate = "production_crosswalk_migration_ready", passed = FALSE
    )
  )

  expect_false(status$complete[status$step == 5L])
  expect_match(status$observed[status$step == 5L], "582/593")
  expect_match(status$observed[status$step == 5L], "11 missing")
})

test_that("geometry coverage identifies missing and unexpected district IDs", {
  skip_if_not_installed("sf")

  geometry <- sf::st_sf(
    unit_id = c("pc2001__01__01", "pc2001__99__99"),
    geom = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    ),
    sf_column_name = "geom"
  )
  admin <- data.frame(
    unit_id = c("pc2001__01__01", "pc2001__01__02"),
    state_code = c("01", "01"),
    district_code = c("01", "02"),
    state_std = c("state", "state"),
    district_std = c("one", "two"),
    stringsAsFactors = FALSE
  )

  coverage <- geometry_unit_coverage_v2(geometry, admin)

  expect_identical(
    coverage$coverage_status[coverage$unit_id == "pc2001__01__02"],
    "missing_geometry"
  )
  expect_identical(
    coverage$coverage_status[coverage$unit_id == "pc2001__99__99"],
    "unexpected_geometry"
  )
})

test_that("constructed incomplete geometry points to the coverage table", {
  status <- lineage_completion_steps_v2(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = character(), status = character()),
    adjudication_queue = data.frame(
      review_class = "cross_vintage_exact_candidate",
      adjudication_status = NA_character_
    ),
    evidence_requests = data.frame(),
    allocation_validation = data.frame(
      source_key = "source",
      coverage_complete = TRUE
    ),
    allocation_weights = data.frame(
      source_unit = character(), status = character()
    ),
    primary_crosswalk = data.frame(),
    sensitivity_crosswalk = data.frame(),
    production_comparison = data.frame(comparison_status = character()),
    geometry_qa = data.frame(
      metric = c(
        "geometry_available", "geometry_rows", "expected_admin_units",
        "missing_admin_units", "unexpected_geometry_units",
        "invalid_geometries"
      ),
      value = c(TRUE, 582, 593, 11, 0, 0)
    ),
    readiness = data.frame(
      gate = "production_crosswalk_migration_ready", passed = FALSE
    )
  )

  expect_match(
    status$next_action[status$step == 5L],
    "geometry_2001_unit_coverage.csv",
    fixed = TRUE
  )
})

test_that("accepted geometry carry-backs fill only missing 2001 units", {
  skip_if_not_installed("sf")

  geometry_2001 <- sf::st_sf(
    unit_id = "pc2001__01__01",
    legacy_attribute = "base",
    geometry_2001 = sf::st_sfc(
      sf::st_point(c(0, 0)),
      crs = 4326
    ),
    sf_column_name = "geometry_2001"
  )
  geometry_2011 <- sf::st_sf(
    pc11_state_id = c("07", "27"),
    pc11_district_id = c("090", "518"),
    later_attribute = c("delhi", "mumbai"),
    geometry_2011 = sf::st_sfc(
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 4326
    ),
    sf_column_name = "geometry_2011"
  )
  carrybacks <- data.frame(
    target_unit_2001 = c("pc2001__07__01", "pc2001__27__22"),
    source_unit_2011 = c("pc2011__07__090", "pc2011__27__518"),
    source_id = c("delhi_atlas", "maharashtra_atlas"),
    status = "accepted",
    note = "official unchanged-boundary decision",
    stringsAsFactors = FALSE
  )

  out <- apply_geometry_carrybacks_v2(
    geometry_2001, geometry_2011, carrybacks
  )

  expect_setequal(
    out$unit_id,
    c("pc2001__01__01", "pc2001__07__01", "pc2001__27__22")
  )
  expect_equal(anyDuplicated(out$unit_id), 0L)
  expect_identical(names(sf::st_drop_geometry(out)), "unit_id")
  expect_identical(attr(out, "sf_column"), "geometry")
})

test_that("unit geometry normalization rejects mismatched identifiers", {
  skip_if_not_installed("sf")

  x <- sf::st_sf(
    id = c("a", "b"),
    geom = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1))
    ),
    sf_column_name = "geom"
  )

  expect_error(
    as_unit_geometry_v2(x, unit_id = "only-one"),
    "one value per feature"
  )
})
