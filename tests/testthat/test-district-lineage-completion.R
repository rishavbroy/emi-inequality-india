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

  draft <- build_adjudication_draft(roster, queue, candidates)

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
  eligibility <- data.frame(
    source_row_id = "s2",
    wave = "nss_2017_18",
    source_code = "202",
    terminal_unit = "pc2011__01__002",
    status = "accepted",
    eligible_conservative = FALSE,
    stringsAsFactors = FALSE
  )
  weights <- data.frame(
    source_unit = c("pc2011__01__002", "pc2011__01__002"),
    target_2001 = c("pc2001__01__01", "pc2001__01__02"),
    weight = c(0.6, 0.4), basis = "population",
    source_id = "official_source", status = "accepted",
    stringsAsFactors = FALSE
  )

  out <- build_sensitivity_crosswalk(
    primary, weights, eligibility
  )

  expect_equal(sum(out$weight[out$source_row_id == "s1"]), 1)
  expect_equal(sum(out$weight[out$source_row_id == "s2"]), 1)
  expect_setequal(out$panel_variant, c("deterministic", "population_allocation"))
})

test_that("legacy comparison reports same, changed, and missing targets", {
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

  out <- build_legacy_crosswalk_comparison(primary, panel)

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
    "missing_from_legacy_panel"
  )
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

  draft <- build_adjudication_draft(roster, queue, candidates)

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

  draft <- build_adjudication_draft(roster, queue, candidates)

  expect_identical(draft$source_row_id, "s2")
})

test_that("legacy comparison flags ambiguous legacy mappings without row expansion", {
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

  out <- build_legacy_crosswalk_comparison(primary, panel)

  expect_equal(nrow(out), 1L)
  expect_identical(out$comparison_status, "ambiguous_legacy_mapping")
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

  out <- dissolve_shrid_geometry_2001(geometry, bridge)

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
  repaired <- make_valid_sf(x)
  expect_true(sf::st_is_valid(repaired)[[1]])
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

  coverage <- geometry_unit_coverage(geometry, admin)

  expect_identical(
    coverage$coverage_status[coverage$unit_id == "pc2001__01__02"],
    "missing_geometry"
  )
  expect_identical(
    coverage$coverage_status[coverage$unit_id == "pc2001__99__99"],
    "unexpected_geometry"
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

  out <- apply_geometry_carrybacks(
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
    as_unit_geometry(x, unit_id = "only-one"),
    "one value per feature"
  )
})

test_that("district-lineage measure mapping preserves one-to-one values", {
  measures <- data.frame(
    district_code_0708 = c("10101", "10102"),
    consumption_0708 = c(100, 200),
    npeople_0708 = c(1000, 2000),
    nhouses_0708 = c(100, 200),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_row_id = c("a", "b"),
    wave = "nss_2007_08",
    source_code = c("10101", "10102"),
    target_unit_2001 = c("pc2001__10__01", "pc2001__10__02"),
    mapping_class = "identity",
    stringsAsFactors = FALSE
  )

  out <- map_lineage_measures(
    measures, crosswalk, "nss_2007_08"
  )

  expect_equal(nrow(out), 2L)
  expect_setequal(out$consumption_0708, c(100, 200))
  expect_true(all(out$lineage_source_count == 1L))
  expect_true(all(out$lineage_aggregation_status == "one_to_one"))
})

test_that("district-lineage multi-source collapse is explicit and weighted", {
  mapped <- data.frame(
    target_unit_2001 = c("pc2001__20__16", "pc2001__20__16"),
    consumption_1718 = c(100, 200),
    gini_cons_1718 = c(0.2, 0.4),
    npeople_1718 = c(1000, 3000),
    nhouses_1718 = c(100, 300),
    stringsAsFactors = FALSE
  )

  out <- collapse_lineage_measure_rows(
    mapped,
    lineage_wave_measure_spec("nss_2017_18")
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$consumption_1718, 175)
  expect_equal(out$npeople_1718, 4000)
  expect_equal(out$nhouses_1718, 400)
  expect_equal(out$lineage_source_count, 2L)
  expect_identical(
    out$lineage_aggregation_status,
    "district_aggregate_weighted"
  )
})

test_that("panel membership comparison separates additions and removals", {
  legacy <- data.frame(
    district_panel_id = c("2001__01__01", "2001__01__02"),
    stringsAsFactors = FALSE
  )
  candidate <- data.frame(
    target_unit_2001 = c("pc2001__01__02", "pc2001__01__03"),
    stringsAsFactors = FALSE
  )

  out <- compare_lineage_panels(legacy, candidate)

  expect_setequal(
    paste(out$target_unit_2001, out$comparison_status, sep = "->"),
    c(
      "pc2001__01__01->legacy_only",
      "pc2001__01__02->shared",
      "pc2001__01__03->lineage_only"
    )
  )
})

test_that("downstream coverage exposes incomplete 2017 lineage", {
  crosswalk <- data.frame(
    source_row_id = c("a", "b", "c"),
    wave = c("nss_2007_08", "nss_2007_08", "nss_2017_18"),
    target_unit_2001 = c(
      "pc2001__01__01", "pc2001__01__02", "pc2001__01__01"
    ),
    stringsAsFactors = FALSE
  )
  eligibility <- data.frame(
    source_row_id = c("a", "b", "c", "d"),
    wave = c(
      "nss_2007_08", "nss_2007_08",
      "nss_2017_18", "nss_2017_18"
    ),
    status = "accepted",
    stringsAsFactors = FALSE
  )
  legacy <- data.frame(
    district_panel_id = c("2001__01__01", "2001__01__02"),
    stringsAsFactors = FALSE
  )
  candidate <- data.frame(
    target_unit_2001 = "pc2001__01__01",
    stringsAsFactors = FALSE
  )

  coverage <- summarize_lineage_downstream_coverage(
    crosswalk, eligibility, legacy, candidate
  )
  row_17 <- coverage[coverage$wave == "nss_2017_18", , drop = FALSE]
  panel <- coverage[coverage$scope == "panel", , drop = FALSE]

  expect_equal(row_17$accepted_identities, 2L)
  expect_equal(row_17$mapped_identities, 1L)
  expect_equal(row_17$crosswalk_rows, 1L)
  expect_equal(row_17$unmapped_identities, 1L)
  expect_equal(row_17$identity_coverage_share, 0.5)
  expect_equal(panel$mapped_targets, 1L)
  expect_equal(panel$shared_unique_units, 1L)
  expect_equal(panel$legacy_only_units, 1L)
  expect_equal(panel$candidate_only_units, 0L)
})

test_that("downstream gates allow additions but forbid silent support loss", {
  coverage <- data.frame(
    scope = "panel",
    wave = "two_wave_panel",
    legacy_only_units = 0L,
    candidate_only_units = 1L,
    shared_unique_units = 1L,
    stringsAsFactors = FALSE
  )
  legacy <- data.frame(
    district_panel_id = c("2001__09__17", "2001__09__17"),
    stringsAsFactors = FALSE
  )
  candidate <- data.frame(
    target_unit_2001 = c("pc2001__09__17", "pc2001__09__18"),
    stringsAsFactors = FALSE
  )
  adjudication <- data.frame(
    target_unit_2001 = c("pc2001__09__17", "pc2001__09__18"),
    decision = c("exclude_inherited_duplicate", "accept_lineage_coverage_addition"),
    status = c("excluded", "accepted"),
    stringsAsFactors = FALSE
  )

  gates <- lineage_downstream_review_gates(
    coverage, legacy, candidate, adjudication,
    identity_coverage_complete = TRUE
  )

  expect_true(gates$passed[
    gates$gate == "inherited_legacy_duplicates_identified"
  ])
  expect_true(gates$passed[
    gates$gate == "panel_membership_adjudicated"
  ])
  expect_true(gates$passed[
    gates$gate == "preferred_panel_constructed_from_reviewed_sources"
  ])
  expect_true(gates$passed[
    gates$gate == "shared_support_comparison_available"
  ])
  expect_true(gates$passed[
    gates$gate == "legacy_comparison_reviewable"
  ])

  coverage$legacy_only_units <- 1L
  not_blocked_by_v1 <- lineage_downstream_review_gates(
    coverage, legacy, candidate, adjudication,
    identity_coverage_complete = TRUE
  )
  expect_true(not_blocked_by_v1$passed[
    not_blocked_by_v1$gate ==
      "preferred_panel_constructed_from_reviewed_sources"
  ])
  expect_true(not_blocked_by_v1$passed[
    not_blocked_by_v1$gate == "legacy_comparison_reviewable"
  ])

  no_candidate <- lineage_downstream_review_gates(
    coverage, legacy, candidate[0, , drop = FALSE], adjudication,
    identity_coverage_complete = TRUE
  )
  expect_false(no_candidate$passed[
    no_candidate$gate ==
      "preferred_panel_constructed_from_reviewed_sources"
  ])
})

test_that("multi-source Ginis require pooled household reconstruction", {
  panel <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    lineage_source_count = c(1L, 2L),
    lineage_aggregation_status = c(
      "one_to_one", "district_aggregate_weighted"
    ),
    gini_cons_0708 = c(0.25, 0.30),
    gini_cons_1718 = c(0.27, 0.34),
    stringsAsFactors = FALSE
  )

  queue <- build_conservative_gini_reconstruction_queue(panel)

  expect_identical(queue$target_unit_2001, "pc2001__01__02")
  expect_identical(queue$status, "needs_reconstruction")
  expect_match(queue$next_action, "Pool the contributing household records")
})

test_that("an empty Gini queue retains a stable output schema", {
  queue <- build_conservative_gini_reconstruction_queue(data.frame(
    target_unit_2001 = "pc2001__01__01",
    lineage_source_count = 1L,
    lineage_aggregation_status = "one_to_one",
    gini_cons_0708_reconstruction_status = "not_required",
    gini_cons_1718_reconstruction_status = "not_required",
    gini_cons_0708 = 0.2,
    gini_cons_1718 = 0.3,
    stringsAsFactors = FALSE
  ))

  expect_equal(nrow(queue), 0L)
  expect_true(all(c(
    "target_unit_2001", "status", "next_action"
  ) %in% names(queue)))
})

test_that("an empty Gini queue writes a header-only diagnostic", {
  queue <- empty_conservative_gini_reconstruction_queue()
  dir <- tempfile("district-lineage-downstream-")

  save_lineage_downstream_review(
    list(gini_reconstruction_queue = queue),
    dir = dir
  )

  path <- file.path(dir, "downstream_gini_reconstruction_queue.csv")
  expect_true(file.exists(path))
  expect_match(readLines(path, n = 1L), "target_unit_2001")
  expect_equal(length(readLines(path)), 1L)
})

test_that("sensitivity allocations remain linked to NSS source identities", {
  primary <- data.frame(
    source_row_id = "nss_2007_08__a",
    wave = "nss_2007_08",
    source_code = "10101",
    target_unit_2001 = "pc2001__10__01",
    mapping_class = "identity",
    stringsAsFactors = FALSE
  )
  eligibility <- data.frame(
    source_row_id = "nss_2017_18__b",
    wave = "nss_2017_18",
    source_code = "10202",
    terminal_unit = "pc2011__10__202",
    status = "accepted",
    eligible_conservative = FALSE,
    stringsAsFactors = FALSE
  )
  weights <- data.frame(
    source_unit = c("pc2011__10__202", "pc2011__10__202"),
    target_2001 = c("pc2001__10__02", "pc2001__10__03"),
    weight = c(0.75, 0.25),
    basis = "population_allocation",
    source_id = "shrid",
    status = "accepted",
    stringsAsFactors = FALSE
  )

  out <- build_sensitivity_crosswalk(
    primary, weights, eligibility
  )
  allocated <- out[out$panel_variant == "population_allocation", ]

  expect_equal(nrow(allocated), 2L)
  expect_true(all(allocated$source_row_id == "nss_2017_18__b"))
  expect_true(all(allocated$wave == "nss_2017_18"))
  expect_true(all(allocated$source_code == "10202"))
  expect_equal(sum(allocated$weight), 1)
})

test_that("population allocation preserves counts and intensive values", {
  measures <- data.frame(
    district_code_1718 = "10202",
    consumption_1718 = 200,
    gini_cons_1718 = 0.4,
    npeople_1718 = 1000,
    nhouses_1718 = 200,
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_row_id = c("source", "source"),
    wave = "nss_2017_18",
    source_code = "10202",
    target_unit_2001 = c("pc2001__10__02", "pc2001__10__03"),
    weight = c(0.75, 0.25),
    basis = "population_allocation",
    source_id = "shrid",
    panel_variant = "population_allocation",
    stringsAsFactors = FALSE
  )

  out <- map_lineage_measures(
    measures, crosswalk, "nss_2017_18"
  )
  out <- out[order(out$target_unit_2001), ]

  expect_equal(out$npeople_1718, c(750, 250))
  expect_equal(out$nhouses_1718, c(150, 50))
  expect_equal(out$consumption_1718, c(200, 200))
  expect_equal(out$gini_cons_1718, c(0.4, 0.4))
  expect_true(all(
    out$lineage_aggregation_status ==
      "source_split_population_allocated"
  ))
})

test_that("district-lineage measure mapping returns empty for an absent wave", {
  measures <- data.frame(
    district_code_1718 = "10202",
    consumption_1718 = 200,
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_row_id = "source",
    wave = "nss_2007_08",
    source_code = "10101",
    target_unit_2001 = "pc2001__10__01",
    stringsAsFactors = FALSE
  )

  out <- map_lineage_measures(
    measures, crosswalk, "nss_2017_18"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("allocation coverage counts identities separately from crosswalk rows", {
  crosswalk <- data.frame(
    source_row_id = c("a", "b", "b"),
    wave = "nss_2017_18",
    target_unit_2001 = c(
      "pc2001__01__01", "pc2001__01__02", "pc2001__01__03"
    ),
    stringsAsFactors = FALSE
  )
  eligibility <- data.frame(
    source_row_id = c("a", "b", "c"),
    wave = "nss_2017_18",
    status = "accepted",
    stringsAsFactors = FALSE
  )
  legacy <- data.frame(
    district_panel_id = c("2001__01__01", "2001__01__02"),
    stringsAsFactors = FALSE
  )
  candidate <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__03"),
    stringsAsFactors = FALSE
  )

  out <- summarize_lineage_downstream_coverage(
    crosswalk, eligibility, legacy, candidate
  )
  wave <- out[out$scope == "wave", , drop = FALSE]
  panel <- out[out$scope == "panel", , drop = FALSE]

  expect_equal(wave$accepted_identities, 3L)
  expect_equal(wave$mapped_identities, 2L)
  expect_equal(wave$crosswalk_rows, 3L)
  expect_equal(wave$unmapped_identities, 1L)
  expect_equal(wave$identity_coverage_share, 2 / 3)
  expect_equal(panel$shared_unique_units, 1L)
  expect_equal(panel$legacy_only_units, 1L)
  expect_equal(panel$candidate_only_units, 1L)
})

test_that("shared support excludes duplicate and non-overlapping units", {
  legacy <- data.frame(
    district_panel_id = c(
      "2001__01__01", "2001__01__02",
      "2001__01__02", "2001__01__03"
    ),
    value = 1:4,
    stringsAsFactors = FALSE
  )
  candidate <- data.frame(
    target_unit_2001 = c(
      "pc2001__01__01", "pc2001__01__02", "pc2001__01__04"
    ),
    value = 5:7,
    stringsAsFactors = FALSE
  )

  out <- build_lineage_shared_support(legacy, candidate)

  expect_identical(out$units$target_unit_2001, "pc2001__01__01")
  expect_identical(out$units$state_2001_cluster, "01")
  expect_equal(nrow(out$legacy), 1L)
  expect_equal(nrow(out$lineage), 1L)
  expect_identical(out$legacy$state_2001_cluster, "01")
  expect_identical(out$lineage$state_2001_cluster, "01")
  expect_false(any(
    out$legacy$target_unit_2001 == "pc2001__01__02"
  ))
})

test_that("unmapped identity queue excludes already mapped accepted rows", {
  eligibility <- data.frame(
    source_row_id = c("mapped", "unmapped", "excluded"),
    wave = "nss_2017_18",
    source_code = c("101", "102", "103"),
    raw_state = "State",
    raw_district = c("Mapped", "Unmapped", "Excluded"),
    state_std = "state",
    district_std = c("mapped", "unmapped", "excluded"),
    terminal_unit = c(
      "pc2011__01__001", "pc2011__01__002", "pc2011__01__003"
    ),
    status = c("accepted", "accepted", "excluded"),
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_row_id = "mapped",
    target_unit_2001 = "pc2001__01__01",
    stringsAsFactors = FALSE
  )

  out <- build_lineage_unmapped_identity_queue(
    eligibility, crosswalk
  )

  expect_identical(out$source_row_id, "unmapped")
  expect_identical(
    out$review_scope,
    "accepted_identity_without_sensitivity_mapping"
  )
})

test_that("non-overlap queue retains both panel-only directions", {
  membership <- data.frame(
    target_unit_2001 = c(
      "pc2001__01__01", "pc2001__01__02", "pc2001__01__03"
    ),
    in_legacy = c(TRUE, TRUE, FALSE),
    in = c(TRUE, FALSE, TRUE),
    comparison_status = c("shared", "legacy_only", "lineage_only"),
    stringsAsFactors = FALSE
  )
  admin_2001 <- data.frame(
    unit_id = c(
      "pc2001__01__01", "pc2001__01__02", "pc2001__01__03"
    ),
    level = "district",
    state_std = "state",
    district_std = c("shared", "production only", "lineage only"),
    source_id = "census2001",
    stringsAsFactors = FALSE
  )

  out <- build_lineage_nonoverlap_queue(
    membership, admin_2001
  )

  expect_setequal(
    out$comparison_status,
    c("legacy_only", "lineage_only")
  )
  expect_setequal(
    out$district_label_2001,
    c("production only", "lineage only")
  )
  expect_true(all(out$canonical_label_available))
  expect_true(all(nzchar(out$next_action)))
})

test_that("non-overlap queue uses canonical 2001 labels", {
  membership <- data.frame(
    target_unit_2001 = "pc2001__17__01",
    in_legacy = TRUE,
    in = FALSE,
    comparison_status = "legacy_only",
    stringsAsFactors = FALSE
  )
  admin_2001 <- data.frame(
    unit_id = "pc2001__17__01",
    level = "district",
    state_std = "meghalaya",
    district_std = "west garo hills",
    source_id = "census2001_c16",
    stringsAsFactors = FALSE
  )

  out <- build_lineage_nonoverlap_queue(
    membership,
    admin_2001
  )

  expect_identical(out$state_label_2001, "meghalaya")
  expect_identical(out$district_label_2001, "west garo hills")
  expect_identical(out$label_source_id, "census2001_c16")
  expect_true(out$canonical_label_available)
})

test_that("accepted sensitivity coverage counts identities, not allocation rows", {
  eligibility <- data.frame(
    source_row_id = c("a", "b", "c"),
    status = "accepted",
    stringsAsFactors = FALSE
  )
  sensitivity <- data.frame(
    source_row_id = c("a", "b", "b"),
    target_unit_2001 = c(
      "pc2001__01__01", "pc2001__01__02", "pc2001__01__03"
    ),
    stringsAsFactors = FALSE
  )

  status <- accepted_sensitivity_mapping_status(
    eligibility, sensitivity
  )

  expect_equal(status$n_accepted, 3L)
  expect_equal(status$n_mapped, 2L)
  expect_equal(status$n_unmapped, 1L)
  expect_false(status$coverage_complete)
})

test_that("accepted sensitivity coverage completes only after every identity maps", {
  eligibility <- data.frame(
    source_row_id = c("a", "b"),
    status = "accepted",
    stringsAsFactors = FALSE
  )
  sensitivity <- data.frame(
    source_row_id = c("a", "b", "b"),
    target_unit_2001 = c(
      "pc2001__01__01", "pc2001__01__02", "pc2001__01__03"
    ),
    stringsAsFactors = FALSE
  )

  status <- accepted_sensitivity_mapping_status(
    eligibility, sensitivity
  )

  expect_true(status$coverage_complete)
  expect_equal(status$n_mapped, status$n_accepted)
})

test_that("canonical 2001 labels reject duplicate registry units", {
  admin_2001 <- data.frame(
    unit_id = c("pc2001__01__01", "pc2001__01__01"),
    level = "district",
    state_std = "state",
    district_std = c("district a", "district b"),
    stringsAsFactors = FALSE
  )

  expect_error(
    lineage_admin_2001_labels(admin_2001),
    "must be unique"
  )
})

test_that("terminal review queue deduplicates identities and reuses evidence", {
  identities <- data.frame(
    source_row_id = c("row-1", "row-2", "row-3"),
    source_code = c("101", "102", "103"),
    wave = "nss_2017_18",
    terminal_unit = c(
      "pc2011__01__001",
      "pc2011__01__001",
      "pc2011__01__002"
    ),
    state_std = "state",
    district_std = c("district a", "district a", "district b"),
    stringsAsFactors = FALSE
  )
  allocations <- data.frame(
    source_unit = "pc2011__01__001",
    target_2001 = NA_character_,
    weight = 0,
    basis = "unresolved",
    source_id = "source-a",
    status = "rejected",
    note = "Unsupported proposal",
    stringsAsFactors = FALSE
  )

  out <- build_lineage_unmapped_terminal_queue(
    identities, allocations
  )

  expect_equal(nrow(out), 2L)
  expect_equal(
    out$identity_count[out$terminal_unit == "pc2011__01__001"],
    2L
  )
  expect_identical(
    out$evidence_class[
      out$terminal_unit == "pc2011__01__001"
    ],
    "rejected_allocation_record"
  )
  expect_identical(
    out$evidence_class[
      out$terminal_unit == "pc2011__01__002"
    ],
    "no_allocation_record"
  )
  expect_lt(
    out$review_priority[
      out$terminal_unit == "pc2011__01__002"
    ],
    out$review_priority[
      out$terminal_unit == "pc2011__01__001"
    ]
  )
})

test_that("accepted disconnected allocations receive highest priority", {
  identities <- data.frame(
    source_row_id = "row-1",
    source_code = "101",
    wave = "nss_2017_18",
    terminal_unit = "pc2011__01__001",
    state_std = "state",
    district_std = "district",
    stringsAsFactors = FALSE
  )
  allocations <- data.frame(
    source_unit = "pc2011__01__001",
    target_2001 = "pc2001__01__01",
    weight = 1,
    basis = "reviewed",
    source_id = "source-a",
    status = "accepted",
    note = "Accepted allocation",
    stringsAsFactors = FALSE
  )

  out <- build_lineage_unmapped_terminal_queue(
    identities, allocations
  )

  expect_identical(
    out$evidence_class,
    "accepted_allocation_not_connected"
  )
  expect_identical(out$review_priority, 1L)
  expect_match(out$next_action, "Repair the connected sensitivity crosswalk")
})

test_that("panel membership adjudication waits for identity coverage", {
  membership <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    in_legacy = c(TRUE, FALSE),
    in = c(FALSE, TRUE),
    comparison_status = c("legacy_only", "lineage_only"),
    stringsAsFactors = FALSE
  )

  out <- build_lineage_panel_membership_adjudication(
    membership,
    identity_coverage_complete = FALSE
  )

  expect_true(all(out$status == "needs_review"))
  expect_true(all(
    out$decision == "defer_until_identity_coverage_complete"
  ))
})

test_that("panel membership adjudication follows support invariants", {
  membership <- data.frame(
    target_unit_2001 = c(
      "pc2001__01__01", "pc2001__01__02",
      "pc2001__01__03", "pc2001__01__04"
    ),
    in_legacy = c(TRUE, FALSE, TRUE, TRUE),
    in = c(TRUE, TRUE, FALSE, FALSE),
    comparison_status = c(
      "shared", "lineage_only", "legacy_only", "legacy_only"
    ),
    stringsAsFactors = FALSE
  )
  duplicates <- data.frame(
    target_unit_2001 = "pc2001__01__04",
    panel_variant = "legacy",
    stringsAsFactors = FALSE
  )

  out <- build_lineage_panel_membership_adjudication(
    membership,
    duplicates,
    identity_coverage_complete = TRUE
  )

  decision <- setNames(out$decision, out$target_unit_2001)
  status <- setNames(out$status, out$target_unit_2001)
  expect_identical(
    decision[["pc2001__01__01"]],
    "retain_shared_support"
  )
  expect_identical(
    decision[["pc2001__01__02"]],
    "accept_lineage_coverage_addition"
  )
  expect_identical(
    decision[["pc2001__01__03"]],
    "exclude_inherited_only_support"
  )
  expect_identical(
    decision[["pc2001__01__04"]],
    "exclude_inherited_duplicate"
  )
  expect_identical(status[["pc2001__01__02"]], "accepted")
  expect_identical(status[["pc2001__01__03"]], "excluded")
})

test_that("multi-source district Ginis are reconstructed from pooled households", {
  panel <- data.frame(
    target_unit_2001 = "pc2001__35__01",
    gini_cons_0708 = 0.2,
    gini_cons_1718 = 0.9,
    lineage_source_count = 2L,
    lineage_aggregation_status = "district_aggregate_weighted",
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_row_id = c("a", "b"),
    wave = "nss_2017_18",
    source_code = c("35102", "35103"),
    target_unit_2001 = "pc2001__35__01",
    weight = 1,
    stringsAsFactors = FALSE
  )
  block3 <- data.frame(
    NSS_Region = c(35, 35, 35, 35),
    District = c(102, 102, 103, 103),
    HHID = c("a1", "a2", "b1", "b2"),
    HH_Con_exp_rs = c(100, 200, 400, 800),
    Household_size = 1,
    MULT_Combined = c(1, 1, 1, 1),
    stringsAsFactors = FALSE
  )

  out <- reconstruct_lineage_pooled_ginis(
    panel, crosswalk, list(), list(nss1718edu_block3 = block3)
  )

  expect_equal(out$panel$gini_cons_1718, wgini(c(100, 200, 400, 800), rep(1, 4)))
  expect_identical(
    out$panel$gini_cons_1718_reconstruction_status,
    "reconstructed"
  )
  expect_identical(out$audit$status, "reconstructed")
  expect_equal(out$audit$source_count, 2L)
  expect_equal(out$audit$household_count, 4L)
  expect_equal(nrow(build_conservative_gini_reconstruction_queue(out$panel)), 0L)
})

test_that("pooled Gini reconstruction preserves sf geometry", {
  testthat::skip_if_not_installed("sf")

  panel <- sf::st_sf(
    target_unit_2001 = "pc2001__35__01",
    gini_cons_0708 = 0.2,
    gini_cons_1718 = 0.9,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
      ))),
      crs = 4326
    )
  )
  crosswalk <- data.frame(
    source_row_id = c("a", "b"),
    wave = "nss_2017_18",
    source_code = c("35102", "35103"),
    target_unit_2001 = "pc2001__35__01",
    weight = 1,
    stringsAsFactors = FALSE
  )
  block3 <- data.frame(
    NSS_Region = c(35, 35),
    District = c(102, 103),
    HHID = c("a1", "b1"),
    HH_Con_exp_rs = c(100, 400),
    Household_size = 1,
    MULT_Combined = 1,
    stringsAsFactors = FALSE
  )

  out <- reconstruct_lineage_pooled_ginis(
    panel, crosswalk, list(), list(nss1718edu_block3 = block3)
  )

  expect_s3_class(out$panel, "sf")
  expect_identical(sf::st_crs(out$panel), sf::st_crs(panel))
  expect_true(sf::st_equals(out$panel, panel, sparse = FALSE)[1, 1])
})


test_that("district recovery audit accounts for the full 2001 universe", {
  admin <- data.frame(
    unit_id = c("a", "b"), state_code = "01", district_code = c("01", "02")
  )
  roster <- data.frame(source_row_id = character())
  eligibility <- data.frame(source_row_id = character())
  preferred <- data.frame(
    source_row_id = c("p07", "p17"),
    wave = c("nss_2007_08", "nss_2017_18"),
    target_unit_2001 = c("a", "a")
  )
  sensitivity <- rbind(
    preferred,
    data.frame(source_row_id = c("s07", "s17"), wave = c("nss_2007_08", "nss_2017_18"), target_unit_2001 = c("b", "b"))
  )

  out <- build_lineage_district_loss_audit(
    admin, roster, eligibility, preferred, sensitivity
  )

  expect_equal(nrow(out), 2L)
  expect_true(out$preferred_two_wave[out$target_unit_2001 == "a"])
  expect_equal(
    out$loss_stage[out$target_unit_2001 == "b"],
    "available_only_under_sensitivity_rule"
  )
})

test_that("recovery gates reject generic exclusions and preferred multi-parent mappings", {
  loss <- data.frame(target_unit_2001 = sprintf("d%03d", 1:593))
  rec <- data.frame(
    exclusion_reason = c("primary_near_complete_requires_review", "multi_parent_allocation_sensitivity_only"),
    recovery_class = c("primary_near_complete", "multi_parent_fractional_mapping"),
    eligible_conservative = c(FALSE, FALSE)
  )

  gates <- build_lineage_recovery_gates(loss, rec)

  expect_true(all(gates$passed))
})


test_that("dominant-parent reviews create a separate one-parent crosswalk", {
  primary <- data.frame(
    source_row_id = "p07", wave = "nss_2007_08", source_code = "1",
    target_unit_2001 = "pc2001__01__01", weight = 1,
    basis = "preferred", source_id = "official", panel_variant = "preferred",
    stringsAsFactors = FALSE
  )
  sensitivity <- rbind(
    primary,
    data.frame(
      source_row_id = "d17", wave = "nss_2017_18", source_code = "2",
      target_unit_2001 = "pc2001__01__01", weight = 1,
      basis = "population_renormalized_min_99pct_mapped",
      source_id = "shrug_pc_keys", panel_variant = "allocated",
      stringsAsFactors = FALSE
    )
  )
  reviews <- data.frame(
    source_row_id = "d17", wave = "nss_2017_18", source_code = "2",
    raw_state = "State", raw_district = "District",
    terminal_unit = "pc2011__01__001", target_unit_2001 = "pc2001__01__01",
    review_status = "accepted_primary", reviewed_panel = "primary",
    evidence_basis = "iss_2001_2011_continuity_and_shrug_min_99pct_single_parent",
    evidence_source_ids = "alluvial|shrug_pc_keys", notes = "reviewed",
    stringsAsFactors = FALSE
  )

  out <- build_conservative_source_crosswalk(primary, sensitivity, reviews)

  expect_setequal(out$source_row_id, c("p07", "d17"))
  expect_identical(anyDuplicated(out$source_row_id), 0L)
  expect_named(out, names(sensitivity))
  expect_true(all(out$panel_variant == "primary"))
})

test_that("dominant-parent reviews cannot admit multi-parent allocations", {
  primary <- data.frame(
    source_row_id = character(), wave = character(), source_code = character(),
    target_unit_2001 = character(), weight = numeric(), basis = character(),
    source_id = character(), panel_variant = character(), stringsAsFactors = FALSE
  )
  sensitivity <- data.frame(
    source_row_id = c("d17", "d17"), wave = "nss_2017_18", source_code = "2",
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    weight = c(.9, .1), basis = "population_share", source_id = "shrug_pc_keys",
    panel_variant = "allocated", stringsAsFactors = FALSE
  )
  reviews <- data.frame(
    source_row_id = "d17", wave = "nss_2017_18", source_code = "2",
    raw_state = "State", raw_district = "District",
    terminal_unit = "pc2011__01__001", target_unit_2001 = "pc2001__01__01",
    review_status = "accepted_primary", reviewed_panel = "primary",
    evidence_basis = "reviewed", evidence_source_ids = "alluvial|shrug_pc_keys",
    notes = "reviewed", stringsAsFactors = FALSE
  )

  expect_error(
    build_conservative_source_crosswalk(primary, sensitivity, reviews),
    "do not match an eligible single-target allocation"
  )
})

test_that("panel-variant review stacks the same model contract across panels", {
  panel <- data.frame(
    target_unit_2001 = c("a", "b"),
    consumption_growth = c(1, 2),
    EMIE_d = c(3, 4),
    wavg_ling_degrees = c(5, 6),
    stringsAsFactors = FALSE
  )
  empty_models <- list()
  first_stage <- data.frame()

  out <- build_lineage_panel_variant_review(
    panels = list(conservative_preferred = panel, primary = panel),
    models = list(conservative_preferred = empty_models, primary = empty_models),
    first_stage_tests = list(
      conservative_preferred = first_stage,
      primary = first_stage
    ),
    gini_audits = list(
      conservative_preferred = data.frame(status = "not_required"),
      primary = data.frame(status = "reconstructed")
    )
  )

  expect_setequal(
    out$panel_summary$panel_variant,
    c("conservative_preferred", "primary")
  )
  expect_true(all(out$panel_summary$unique_districts == 2L))
  expect_setequal(out$gini_reconstruction$panel_variant, out$panel_summary$panel_variant)
})

test_that("multi-parent review queue preserves every fractional target", {
  rec <- data.frame(
    source_row_id = "d17", wave = "nss_2017_18", source_code = "2",
    raw_state = "State", raw_district = "District",
    recovery_class = "multi_parent_fractional_mapping",
    allocation_target_count = 2, allocation_weight_sum = 1,
    allocation_basis = "population_renormalized_min_99pct_mapped",
    allocation_source_id = "shrug_pc_keys",
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_row_id = c("d17", "d17"), wave = "nss_2017_18",
    source_code = "2", target_unit_2001 = c("a", "b"),
    weight = c(.8, .2), basis = "population_renormalized_min_99pct_mapped",
    source_id = "shrug_pc_keys", panel_variant = "population_allocation",
    stringsAsFactors = FALSE
  )

  out <- build_lineage_multi_parent_review_queue(rec, crosswalk)

  expect_equal(nrow(out), 2L)
  expect_equal(sum(out$weight), 1)
  expect_setequal(out$allocation_rank, c(1, 2))
  expect_true(all(out$review_status == "needs_fractional_validation"))
})

test_that("lineage completion tracks current six-step invariants", {
  geometry <- data.frame(
    metric = c("geometry_available", "geometry_rows", "expected_admin_units",
               "missing_admin_units", "unexpected_geometry_units", "invalid_geometries"),
    value = c(TRUE, 593, 593, 0, 0, 0)
  )
  status <- lineage_completion_steps(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = "s1", status = "accepted"),
    adjudication_queue = data.frame(review_class = character(), adjudication_status = character()),
    evidence_requests = data.frame(),
    allocation_validation = data.frame(source_key = "u1", weights_well_formed = TRUE, coverage_complete = TRUE),
    allocation_weights = data.frame(source_unit = character(), status = character()),
    conservative_crosswalk = data.frame(source_row_id = "s1"),
    primary_crosswalk = data.frame(source_row_id = "s1"),
    full_reviewed_crosswalk = data.frame(source_row_id = "s1"),
    geometry_qa = geometry,
    conservative_eligibility = data.frame(source_row_id = "s1", status = "accepted", eligible_conservative = TRUE)
  )
  expect_identical(status$step, seq_len(6L))
  expect_true(all(status$complete))
  expect_true(all(nzchar(status$next_action)))
})

test_that("panel roles remain monotone", {
  geometry <- data.frame(
    metric = c("geometry_available", "missing_admin_units", "unexpected_geometry_units", "invalid_geometries"),
    value = c(TRUE, 0, 0, 0)
  )
  status <- lineage_completion_steps(
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = "s1", status = "accepted"),
    adjudication_queue = data.frame(review_class = character(), adjudication_status = character()),
    evidence_requests = data.frame(),
    allocation_validation = data.frame(source_key = "u1", weights_well_formed = TRUE, coverage_complete = TRUE),
    allocation_weights = data.frame(source_unit = character(), status = character()),
    conservative_crosswalk = data.frame(source_row_id = c("s1", "s2")),
    primary_crosswalk = data.frame(source_row_id = "s1"),
    full_reviewed_crosswalk = data.frame(source_row_id = "s1"),
    geometry_qa = geometry,
    conservative_eligibility = data.frame(source_row_id = "s1", status = "accepted", eligible_conservative = TRUE)
  )
  expect_false(status$complete[status$work_item == "Build the conservative, primary, and full reviewed crosswalks"])
})
