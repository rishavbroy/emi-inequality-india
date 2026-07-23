test_that("wide Alluvial sources become adjacent-year lineage candidates", {
  raw <- data.frame(
    `2001-State` = "A",
    `2001-District` = "Old",
    `2011-State` = "A",
    `2011-District` = "Middle",
    `2024-State` = "A",
    `2024-District` = "New",
    check.names = FALSE
  )

  out <- parse_alluvial_district_changes(raw)

  expect_equal(nrow(out), 2L)
  expect_equal(out$source_year_raw, c(2001L, 2011L))
  expect_equal(out$target_year_raw, c(2011L, 2024L))
  expect_equal(out$source_district_raw, c("Old", "Middle"))
  expect_equal(out$target_district_raw, c("Middle", "New"))
  expect_true(all(out$change_type == "lineage_candidate"))
})

test_that("LGD SpreadsheetML readers locate the actual table header", {
  skip_if_not_installed("XML")
  path <- tempfile(fileext = ".xls")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet" xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet">',
    '<Worksheet><Table>',
    '<Row><Cell><Data>Local Government Directory</Data></Cell></Row>',
    '<Row><Cell><Data>District Code</Data></Cell><Cell><Data>District Name(In English)</Data></Cell><Cell ss:Index="4"><Data>State Code</Data></Cell></Row>',
    '<Row><Cell><Data>666</Data></Cell><Cell><Data>Longding</Data></Cell><Cell ss:Index="4"><Data>12</Data></Cell></Row>',
    '<Row><Cell><Data>Total 1</Data></Cell></Row>',
    '<Row><Cell><Data>Report Generated</Data></Cell></Row>',
    '</Table></Worksheet></Workbook>'
  ), path)

  out <- read_lgd_spreadsheetml(path)

  expect_equal(nrow(out), 1L)
  expect_equal(out[[first_col(out, "District Code")]], "666")
  expect_equal(out[[first_col(out, "District Name(In English)")]], "Longding")
  expect_equal(out[[first_col(out, "State Code")]], "12")
})

test_that("empty LGD inputs remain empty after standardization", {
  out <- standardize_lgd_registry(data.frame(), "district")
  modified <- standardize_lgd_modification_roster(data.frame(), "district")

  expect_equal(nrow(out), 0L)
  expect_equal(nrow(modified), 0L)
  expect_true(all(c("district_lgd_code", "census2011_district_code") %in% names(out)))
})

test_that("changed-unit rosters remain complete while geometry stays inventory-only", {
  paths <- build_paths(tempdir())
  specs <- district_lineage_v2_input_specs(paths)

  expect_true(specs$load_for_diagnostic[specs$source_id == "lgd_mod_villages"])
  expect_false(specs$load_for_diagnostic[specs$source_id == "shrug_shrid_geometry_zip"])
  expect_true(specs$load_for_diagnostic[specs$source_id == "lgd_mod_districts"])
  expect_true(specs$load_for_diagnostic[specs$source_id == "lgd_mod_urban_local_bodies"])
  expect_true(specs$load_for_diagnostic[specs$source_id == "lgd_urban_coverage"])
})

test_that("annual Jaacks triplets become adjacent-year candidate edges", {
  raw <- data.frame(
    `2001` = c("STATENAME", "A"),
    `...2` = c("DISTNAME", "Old"),
    `...3` = c("DISTCODE", "001"),
    `2002` = c("STATENAME", "A"),
    `...5` = c("DISTNAME", "New"),
    `...6` = c("DISTCODE", "002"),
    check.names = FALSE
  )

  out <- parse_india_district_tracker(raw)

  expect_equal(nrow(out), 1L)
  expect_equal(out$source_district_raw, "Old")
  expect_equal(out$target_district_raw, "New")
  expect_equal(out$source_code_raw, "001")
  expect_equal(out$target_code_raw, "002")
  expect_equal(out$change_type, "annual_lineage_candidate")
})

test_that("LGD urban coverage retains its district and locality bridge", {
  raw <- data.frame(
    `State Name (In English)` = "A",
    `Local Body Code` = 10,
    `Local Body Name (In English)` = "Town",
    `Census 2011 Code` = 100,
    `District Code` = 20,
    `District Name (In English)` = "District",
    `Subdistrict Code` = 30,
    `Subdistrict Name (In English)` = "Subdistrict",
    `Village Code` = 40,
    `Village Name (In English)` = "Village",
    check.names = FALSE
  )

  out <- standardize_lgd_urban_coverage(raw)

  expect_equal(out$urban_local_body_code, "10")
  expect_equal(out$district_lgd_code, "20")
  expect_equal(out$subdistrict_lgd_code, "30")
  expect_equal(out$village_lgd_code, "40")
})

test_that("Kumar-Somanathan transfer rows retain both population shares", {
  raw <- data.frame(
    district_1991 = "Parent",
    pop_1991 = 1000,
    district_2001 = "Child",
    pct_01in91 = 25,
    pct_91in01 = 80,
    stringsAsFactors = FALSE
  )

  out <- parse_carveouts_renamings(raw)

  expect_equal(out$source_year_raw, 1991L)
  expect_equal(out$target_year_raw, 2001L)
  expect_equal(out$source_share_to_target, 0.25)
  expect_equal(out$target_share_from_source, 0.8)
})

test_that("ISDED anchor links are deduplicated and remain candidate evidence", {
  raw <- list(isded_1951_2024 = data.frame(
    source_district = c("Old", "Old", "Same"),
    dest_district = c("New", "New", "Same"),
    source_year = c(2001, 2001, 2001),
    dest_year = c(2011, 2011, 2011),
    filter_state = c("State", "State", "State"),
    stringsAsFactors = FALSE
  ))

  out <- build_isded_candidate_events_v2(raw)

  expect_equal(nrow(out), 1L)
  expect_equal(out$from_district, "Old")
  expect_equal(out$to_district, "New")
  expect_equal(out$status, "candidate_unadjudicated")
  expect_true(grepl("does not by itself establish", out$note, fixed = TRUE))
})

test_that("manifest file IDs route through their explicit district-source parsers", {
  raw <- list(district_changes_alluvial = data.frame(
    `2001-State` = "A", `2001-District` = "Old",
    `2011-State` = "A", `2011-District` = "New",
    check.names = FALSE
  ))

  out <- build_district_tracker(raw)

  expect_equal(out$source_file_id, "alluvial")
  expect_equal(out$source_year, 2001L)
  expect_equal(out$target_year, 2011L)
  expect_equal(out$source_district_raw, "Old")
  expect_equal(out$target_district_raw, "New")
})

test_that("lineage evidence registry requires stable unique source IDs", {
  registry <- data.frame(
    source_id = c("a", "a"), citation = c("one", "two"),
    path_or_url = c("x", "y"), accessed = c("2026-01-01", "2026-01-02")
  )

  expect_error(read_lineage_source_registry_v2(registry), "unique source_id")
})

test_that("candidate event years are not promoted to fabricated exact dates", {
  tracker <- data.frame(
    source_file_id = "district_splits", .row_in_source = 1L,
    source_state_raw = "State", source_district_raw = "Parent",
    target_state_raw = "State", target_district_raw = "Child",
    source_year = 2014L, target_year = 2015L, event_year = 2015L,
    change_type = "split_or_carveout", stringsAsFactors = FALSE
  )

  out <- build_candidate_admin_events_v2(tracker, list())

  expect_true(is.na(out$effective_date))
  expect_equal(out$reported_year, 2015L)
  expect_equal(out$source_year, 2014L)
  expect_equal(out$target_year, 2015L)
  expect_equal(out$date_precision, "year_only")
})

test_that("accepted lineage decisions cite registered evidence", {
  registry <- data.frame(
    source_id = "official", citation = "Official source",
    path_or_url = "path", accessed = "2026-07-22"
  )
  matches <- data.frame(
    source_row_id = c("a", "b"), source_id = c("official", "unknown"),
    status = c("accepted", "accepted"), stringsAsFactors = FALSE
  )

  issues <- validate_lineage_source_references_v2(registry, matches)

  expect_equal(issues$object_id, "b")
  expect_equal(issues$issue, "unregistered_source_id")

  matches$source_id[[2]] <- NA_character_
  issues <- validate_lineage_source_references_v2(registry, matches)
  expect_equal(issues$issue, "missing_source_id")
})


test_that("source branches assemble by source ID rather than branch order", {
  branches <- list(
    list(source_id = "b", value = data.frame(x = 2)),
    list(source_id = "a", value = data.frame(x = 1))
  )

  out <- assemble_district_lineage_v2_sources(branches)

  expect_named(out, c("b", "a"))
  expect_equal(out$a$x, 1)
  expect_error(
    assemble_district_lineage_v2_sources(c(branches, branches[1])),
    "duplicate source IDs"
  )
  expect_error(
    assemble_district_lineage_v2_sources(list(list(value = data.frame()))),
    "must have a source_id"
  )
})

test_that("source specifications split loaded inputs from the complete inventory", {
  specs <- data.frame(
    source_id = c("loaded", "missing", "inventory_only"),
    relative_path = c("a", "b", "c"),
    reader = c("csv", "csv", "inventory_only"),
    role = c("candidate", "candidate", "geometry"),
    load_for_diagnostic = c(TRUE, TRUE, FALSE),
    exists = c(TRUE, FALSE, TRUE),
    size_bytes = c(1, NA, 2),
    absolute_path = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  inventory <- district_lineage_v2_source_inventory(specs)
  branches <- split_district_lineage_v2_source_specs(specs)

  expect_equal(inventory$source_id, specs$source_id)
  expect_length(branches, 1L)
  expect_equal(branches[[1]]$source_id, "loaded")
})


test_that("changed-component roster retains every loaded administrative level", {
  canonical <- function(level, code) data.frame(
    level = level,
    entity_code = code,
    entity_name = paste(level, code),
    state_lgd_code = "1",
    state_name = "State",
    district_lgd_code = if (level == "district") code else "10",
    district_name = "District",
    subdistrict_lgd_code = if (level == "subdistrict") code else "100",
    subdistrict_name = "Subdistrict",
    period_start = "2011-01-01",
    period_end = "2018-06-30",
    event_type = "unknown_modification",
    evidence_status = "changed_unit_roster_only",
    stringsAsFactors = FALSE
  )
  sources <- list(
    lgd_mod_districts = canonical("district", "10"),
    lgd_mod_subdistricts = canonical("subdistrict", "100"),
    lgd_mod_villages = canonical("village", "1000"),
    lgd_mod_urban_local_bodies = canonical("urban_local_body", "10000")
  )

  out <- build_changed_component_roster_v2(sources)

  expect_setequal(
    out$level,
    c("district", "subdistrict", "village", "urban_local_body")
  )
  expect_equal(nrow(out), 4L)
})


test_that("blank tracked lineage ledgers retain zero-row schemas", {
  events <- read_admin_events_v2(data.frame())
  registry <- read_lineage_source_registry_v2(data.frame())

  expect_equal(nrow(events), 0L)
  expect_named(
    events,
    c("event_id", "effective_date", "event_type", "from_unit", "to_unit",
      "share", "source_id", "status", "note")
  )
  expect_equal(nrow(registry), 0L)
  expect_named(registry, c("source_id", "citation", "path_or_url", "accessed"))
})


test_that("lineage diagnostic writer preserves typed zero-row schemas", {
  dir <- tempfile("lineage-v2-")
  diagnostics <- list(
    source_matches = empty_source_matches_v2(),
    missing_core_inputs = data.frame(source_id = character(), stringsAsFactors = FALSE)
  )

  save_district_lineage_v2(diagnostics, dir)

  matches <- utils::read.csv(file.path(dir, "source_matches.csv"), check.names = FALSE)
  missing <- utils::read.csv(file.path(dir, "missing_core_inputs.csv"), check.names = FALSE)
  expect_named(matches, names(empty_source_matches_v2()))
  expect_named(missing, "source_id")
  expect_equal(nrow(matches), 0L)
  expect_equal(nrow(missing), 0L)
})


test_that("empty duplicate diagnostics preserve their schema", {
  out <- duplicate_key_diagnostics_v2(data.frame(id = "a"), "id", "fixture")

  expect_equal(nrow(out), 0L)
  expect_named(out, names(empty_duplicate_key_diagnostics_v2()))
})


test_that("evidence requests follow deterministic adjudication work", {
  sources <- data.frame(
    source_row_id = c("exact", "fuzzy"), wave = "nss_2007_08",
    state_std = "state", district_std = c("alpha", "beta"),
    stringsAsFactors = FALSE
  )
  queue <- data.frame(
    source_row_id = c("exact", "fuzzy"), wave = "nss_2007_08",
    state_std = "state", district_std = c("alpha", "beta"),
    review_class = c("cross_vintage_exact_candidate", "fuzzy_candidates"),
    recommended_method = c("exact_normalized_name", "fuzzy_name_candidate"),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    event_id = c("alpha_event", "beta_event"),
    from_state = "state", to_state = "state",
    from_district = c("alpha", "beta"), to_district = c("alpha child", "beta child"),
    status = "candidate_unadjudicated", source_id = "fixture",
    reported_year = 2007L, source_year = NA_integer_, target_year = NA_integer_,
    effective_date = NA_character_, stringsAsFactors = FALSE
  )

  out <- build_evidence_requests_v2(events, sources, queue)

  expect_false(any(grepl("exact", out$request_id, fixed = TRUE)))
  expect_true(any(out$request_id == "source__fuzzy"))
  expect_true(any(out$request_id == "event__beta_event"))
})


test_that("SHRID bridge summaries aggregate each status without dropping mass", {
  bridge <- data.frame(
    bridge_status = c("stable", "stable", "changed"),
    shrid2 = c("a", "b", "c"),
    population = c(10, 20, 5),
    area = c(1, 2, 0.5),
    stringsAsFactors = FALSE
  )

  out <- summarize_shrid_bridge_v2(bridge)

  expect_named(out, c("bridge_status", "n_shrid", "population", "area"))
  expect_setequal(out$bridge_status, c("stable", "changed"))
  expect_equal(sum(out$n_shrid), 3L)
  expect_equal(sum(out$population), 35)
  expect_equal(sum(out$area), 3.5)
})

test_that("lineage summary preserves the complete diagnostic metric contract", {
  summary <- lineage_v2_summary(
    inventory = data.frame(exists = c(TRUE, FALSE)),
    admin_2001 = data.frame(unit_id = "a"),
    admin_2011 = data.frame(unit_id = c("b", "c")),
    bridge = data.frame(deterministic = c(TRUE, FALSE)),
    transition = data.frame(row = 1),
    source_roster = data.frame(source_row_id = c("s1", "s2")),
    source_matches = data.frame(source_row_id = "s1", status = "accepted"),
    candidates = data.frame(row = 1:2),
    adjudication_queue = data.frame(
      review_class = c("cross_vintage_exact_candidate", "fuzzy_candidates")
    ),
    eligibility = data.frame(eligible_primary = c(TRUE, FALSE)),
    events = data.frame(row = 1),
    current_components = data.frame(row = 1:3),
    urban_coverage = data.frame(row = 1:4),
    changed_components = data.frame(row = 1:5),
    evidence_requests = data.frame(row = 1:6),
    adjudicated_weights = data.frame(
      source_unit = c("pc2011__01__002", "pc2011__01__002"),
      status = c("accepted", "accepted")
    ),
    adjudicated_weight_validation = data.frame(
      source_key = "pc2011__01__002",
      coverage_complete = TRUE
    ),
    allocation_validation = data.frame(
      source_key = c("pc2011__01__001", "pc2011__01__002", "pc2011__01__003"),
      coverage_complete = c(TRUE, FALSE, FALSE)
    )
  )

  expected_metrics <- c(
    "available_inputs", "missing_inputs", "admin_units_2001", "admin_units_2011",
    "shrid_bridge_rows", "deterministic_shrid_rows", "district_transition_rows",
    "nss_source_rows", "accepted_source_matches", "unadjudicated_source_rows",
    "candidate_rows", "cross_vintage_exact_review_rows",
    "single_vintage_exact_review_rows", "fuzzy_review_rows",
    "no_candidate_rows", "primary_eligible_source_rows", "candidate_event_rows",
    "current_component_rows", "urban_coverage_rows", "changed_component_rows",
    "targeted_evidence_requests", "accepted_allocation_sources",
    "rejected_allocation_sources", "remaining_incomplete_allocations"
  )

  expect_named(summary, c("metric", "value"))
  expect_setequal(summary$metric, expected_metrics)
  expect_equal(anyDuplicated(summary$metric), 0L)
  expect_equal(
    summary$value[summary$metric == "unadjudicated_source_rows"],
    1
  )
  expect_equal(
    summary$value[summary$metric == "targeted_evidence_requests"],
    6
  )
  expect_equal(
    summary$value[summary$metric == "accepted_allocation_sources"],
    1
  )
  expect_equal(
    summary$value[summary$metric == "rejected_allocation_sources"],
    0
  )
  expect_equal(
    summary$value[summary$metric == "remaining_incomplete_allocations"],
    1
  )
})

test_that("migration readiness is derived from prerequisite gates", {
  args <- list(
    missing_core = character(),
    admin_2001 = data.frame(unit_id = "pc2001__01__01"),
    admin_2011 = data.frame(unit_id = "pc2011__01__001"),
    allocation_validation = data.frame(
      source_key = "pc2011__01__001",
      weights_well_formed = TRUE,
      coverage_complete = TRUE
    ),
    source_roster = data.frame(source_row_id = "source-1"),
    source_matches = data.frame(source_row_id = "source-1", status = "accepted"),
    primary_eligibility = data.frame(status = "accepted", eligible_primary = TRUE),
    duplicate_keys = empty_duplicate_key_diagnostics_v2(),
    adjudicated_allocation_validation = data.frame(
      source_key = character(),
      weights_well_formed = logical(),
      coverage_complete = logical()
    ),
    source_reference_issues = data.frame()
  )

  ready <- do.call(build_migration_readiness_v2, args)
  expect_true(ready$passed[ready$gate == "production_crosswalk_migration_ready"])

  args$allocation_validation$coverage_complete <- FALSE
  blocked <- do.call(build_migration_readiness_v2, args)
  expect_false(blocked$passed[blocked$gate == "production_crosswalk_migration_ready"])
  expect_identical(
    build_migration_blockers_v2(blocked)$gate,
    "shrid_allocation_coverage_complete"
  )
})

test_that("migration blockers contain one actionable row per failed prerequisite", {
  readiness <- data.frame(
    gate = c(
      "core_inputs_available",
      "all_source_rows_adjudicated",
      "production_crosswalk_migration_ready"
    ),
    passed = c(TRUE, FALSE, FALSE),
    note = c("complete", "incomplete", "summary"),
    stringsAsFactors = FALSE
  )
  blockers <- build_migration_blockers_v2(readiness)

  expect_named(blockers, c("gate", "note", "next_action"))
  expect_identical(blockers$gate, "all_source_rows_adjudicated")
  expect_true(nzchar(blockers$next_action))
})


test_that("migration gates distinguish absent acceptances from ineligible acceptances", {
  args <- list(
    missing_core = character(),
    admin_2001 = data.frame(unit_id = "pc2001__01__01"),
    admin_2011 = data.frame(unit_id = "pc2011__01__001"),
    allocation_validation = data.frame(
      source_key = "pc2011__01__001",
      weights_well_formed = TRUE,
      coverage_complete = TRUE
    ),
    source_roster = data.frame(source_row_id = "source-1"),
    source_matches = data.frame(
      source_row_id = "source-1",
      status = "excluded"
    ),
    primary_eligibility = data.frame(
      status = "excluded",
      eligible_primary = FALSE
    ),
    duplicate_keys = empty_duplicate_key_diagnostics_v2(),
    adjudicated_allocation_validation = data.frame(
      source_key = character(),
      weights_well_formed = logical(),
      coverage_complete = logical()
    ),
    source_reference_issues = data.frame()
  )

  readiness <- do.call(build_migration_readiness_v2, args)
  expect_false(readiness$passed[readiness$gate == "accepted_source_rows_present"])
  expect_true(
    readiness$passed[readiness$gate == "all_accepted_rows_primary_eligible"]
  )

  args$source_matches$status <- "accepted"
  args$primary_eligibility$status <- "accepted"
  readiness <- do.call(build_migration_readiness_v2, args)
  expect_true(readiness$passed[readiness$gate == "accepted_source_rows_present"])
  expect_false(
    readiness$passed[readiness$gate == "all_accepted_rows_primary_eligible"]
  )
})


test_that("derived Census 2001 geometry is an optional loaded source", {
  specs <- district_lineage_v2_input_specs(build_paths())
  row <- specs[specs$source_id == "lineage_geometry_2001", , drop = FALSE]

  expect_equal(nrow(row), 1L)
  expect_true(row$load_for_diagnostic)
  expect_identical(row$reader, "gpkg")
  expect_identical(row$role, "derived_2001_geometry")
})

test_that("reviewed geometry and source decisions satisfy evidence contracts", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  metadata_path <- function(name) {
    file.path(root, "data", "metadata", name)
  }

  carrybacks <- read_geometry_carrybacks_v2(
    read.csv(
      metadata_path("district_geometry_carrybacks_v2.csv"),
      stringsAsFactors = FALSE
    )
  )
  adjudications <- read_adjudicated_source_matches_v2(
    read.csv(
      metadata_path("district_adjudications_v2.csv"),
      stringsAsFactors = FALSE
    )
  )
  registry <- read_lineage_source_registry_v2(
    read.csv(
      metadata_path("district_sources_v2.csv"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(carrybacks), 11L)
  expect_true(all(carrybacks$status == "accepted"))
  expect_equal(nrow(adjudications), 597L)
  expect_true(all(adjudications$status == "accepted"))
  expect_setequal(
    unique(adjudications$method),
    c(
      "official_unchanged_boundary_carryback",
      "official_nss64_census2001_code_name_identity",
      "official_nss64_census2001_code_identity"
    )
  )

  geometry_matches <- adjudications[
    adjudications$method %in% "official_unchanged_boundary_carryback",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(geometry_matches), 18L)
  expect_true(all(
    geometry_matches$unit_id %in% carrybacks$target_unit_2001
  ))

  nss64_matches <- adjudications[
    adjudications$method %in%
      "official_nss64_census2001_code_name_identity",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(nss64_matches), 178L)
  expect_true(all(nss64_matches$wave == "nss_2007_08"))
  expect_true(all(grepl("^pc2001__", nss64_matches$unit_id)))
  expect_true(all(
    nss64_matches$source_id == "nss64_education_district_codes"
  ))

  nss64_code_matches <- adjudications[
    adjudications$method %in%
      "official_nss64_census2001_code_identity",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(nss64_code_matches), 401L)
  expect_true(all(nss64_code_matches$wave == "nss_2007_08"))
  expect_true(all(grepl("^pc2001__", nss64_code_matches$unit_id)))
  expect_true(all(
    nss64_code_matches$source_id ==
      "nss64_education_district_codes"
  ))

  issues <- validate_lineage_source_references_v2(
    registry,
    source_matches = adjudications,
    geometry_carrybacks = carrybacks
  )
  expect_equal(nrow(issues), 0L)
})

test_that("accepted primary eligibility ignores unrelated NA statuses", {
  readiness <- build_migration_readiness_v2(
    missing_core = character(),
    admin_2001 = data.frame(unit_id = "pc2001__01__01"),
    admin_2011 = data.frame(unit_id = "pc2011__01__001"),
    allocation_validation = data.frame(
      source_key = "pc2011__01__001",
      weights_well_formed = TRUE,
      coverage_complete = TRUE
    ),
    source_roster = data.frame(source_row_id = c("accepted", "unreviewed")),
    source_matches = data.frame(source_row_id = "accepted", status = "accepted"),
    primary_eligibility = data.frame(
      status = c("accepted", NA_character_),
      eligible_primary = c(TRUE, FALSE)
    ),
    duplicate_keys = empty_duplicate_key_diagnostics_v2(),
    adjudicated_allocation_validation = data.frame(
      source_key = character(),
      weights_well_formed = logical(),
      coverage_complete = logical()
    ),
    source_reference_issues = data.frame()
  )

  gate <- readiness$passed[
    readiness$gate == "all_accepted_rows_primary_eligible"
  ]
  expect_identical(gate, TRUE)
})

test_that("migration readiness counts valid reviewed sensitivity coverage", {
  generated <- data.frame(
    source_key = c("pc2011__01__001", "pc2011__01__002"),
    weights_well_formed = TRUE,
    coverage_complete = FALSE,
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_key = "pc2011__01__002",
    coverage_complete = TRUE,
    stringsAsFactors = FALSE
  )

  readiness <- build_migration_readiness_v2(
    missing_core = character(),
    admin_2001 = data.frame(unit_id = "pc2001__01__01"),
    admin_2011 = data.frame(unit_id = "pc2011__01__001"),
    allocation_validation = generated,
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = "s1", status = "accepted"),
    primary_eligibility = data.frame(
      status = "accepted", eligible_primary = TRUE
    ),
    duplicate_keys = empty_duplicate_key_diagnostics_v2(),
    adjudicated_allocation_validation = reviewed,
    source_reference_issues = data.frame()
  )

  gate <- readiness$passed[
    readiness$gate == "shrid_allocation_coverage_complete"
  ]
  expect_false(gate)

  reviewed <- rbind(
    reviewed,
    data.frame(
      source_key = "pc2011__01__001",
      coverage_complete = TRUE,
      stringsAsFactors = FALSE
    )
  )
  readiness <- build_migration_readiness_v2(
    missing_core = character(),
    admin_2001 = data.frame(unit_id = "pc2001__01__01"),
    admin_2011 = data.frame(unit_id = "pc2011__01__001"),
    allocation_validation = generated,
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = "s1", status = "accepted"),
    primary_eligibility = data.frame(
      status = "accepted", eligible_primary = TRUE
    ),
    duplicate_keys = empty_duplicate_key_diagnostics_v2(),
    adjudicated_allocation_validation = reviewed,
    source_reference_issues = data.frame()
  )
  gate <- readiness$passed[
    readiness$gate == "shrid_allocation_coverage_complete"
  ]
  expect_true(gate)
})

test_that("tracked high-coverage decisions leave only lower-coverage gaps", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  transition_path <- file.path(
    root, "outputs", "diagnostics", "extended",
    "district_lineage_v2", "district_transition_2001_2011.csv"
  )
  skip_if_not(file.exists(transition_path))
  transition <- read.csv(transition_path, stringsAsFactors = FALSE)

  generated <- validate_allocation_weights(transition)
  weights <- read_adjudicated_allocation_weights_v2(
    read_lineage_source(
      file.path(
        root, "data", "metadata", "district_allocation_weights_v2.csv"
      ),
      reader = "allocation_csv",
      source_id = "lineage_allocation_weights"
    )
  )
  reviewed <- validate_adjudicated_allocation_weights_v2(weights)
  decisions <- allocation_decision_status_v2(weights)
  status <- allocation_coverage_status_v2(
    generated, reviewed, decisions
  )

  expect_equal(status$n_reviewed_accepted, 457L)
  expect_equal(status$n_reviewed_rejected, 79L)
  expect_equal(status$n_unresolved, 0L)
  expect_true(status$coverage_resolved)
})

test_that("allocation summary counts source decisions rather than ledger rows", {
  weights <- data.frame(
    source_unit = c(
      "pc2011__01__001",
      "pc2011__01__001",
      "pc2011__01__002"
    ),
    status = c("accepted", "accepted", "rejected"),
    stringsAsFactors = FALSE
  )
  summary <- lineage_v2_summary(
    inventory = data.frame(exists = logical()),
    admin_2001 = data.frame(),
    admin_2011 = data.frame(),
    bridge = data.frame(deterministic = logical()),
    transition = data.frame(),
    source_roster = data.frame(source_row_id = character()),
    source_matches = data.frame(
      source_row_id = character(),
      status = character()
    ),
    candidates = data.frame(),
    adjudication_queue = data.frame(review_class = character()),
    eligibility = data.frame(eligible_primary = logical()),
    events = data.frame(),
    current_components = data.frame(),
    urban_coverage = data.frame(),
    changed_components = data.frame(),
    evidence_requests = data.frame(),
    adjudicated_weights = weights,
    adjudicated_weight_validation = data.frame(
      source_key = "pc2011__01__001",
      coverage_complete = TRUE
    ),
    allocation_validation = data.frame(
      source_key = c("pc2011__01__001", "pc2011__01__002"),
      coverage_complete = FALSE
    )
  )

  values <- stats::setNames(summary$value, summary$metric)
  expect_equal(values[["accepted_allocation_sources"]], 1)
  expect_equal(values[["rejected_allocation_sources"]], 1)
  expect_equal(values[["remaining_incomplete_allocations"]], 0)
})

test_that("tracked NSS-64 identities require code and name agreement", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  adjudications <- read.csv(
    file.path(root, "data", "metadata", "district_adjudications_v2.csv"),
    stringsAsFactors = FALSE
  )
  rows <- adjudications[
    adjudications$method %in%
      "official_nss64_census2001_code_name_identity",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 178L)
  expect_true(all(rows$wave == "nss_2007_08"))
  expect_true(all(rows$status == "accepted"))
  expect_true(all(rows$source_id == "nss64_education_district_codes"))
  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_true(all(grepl("^pc2001__", rows$unit_id)))
})

test_that("source reference validation is method-agnostic", {
  registry <- read_lineage_source_registry_v2(data.frame(
    source_id = c("geometry_source", "survey_source"),
    citation = c("Geometry evidence", "Survey evidence"),
    path_or_url = c("geometry", "survey"),
    accessed = c("2026-07-23", "2026-07-23"),
    stringsAsFactors = FALSE
  ))
  matches <- read_adjudicated_source_matches_v2(data.frame(
    source_row_id = c("geometry_row", "survey_row"),
    wave = c("nss_2007_08", "nss_2007_08"),
    raw_state = c("State", "State"),
    raw_district = c("District A", "District B"),
    unit_id = c("pc2001__01__01", "pc2001__02__02"),
    method = c(
      "official_unchanged_boundary_carryback",
      "official_nss64_census2001_code_name_identity"
    ),
    source_id = c("geometry_source", "survey_source"),
    status = c("accepted", "accepted"),
    note = c("Geometry decision", "Survey decision"),
    stringsAsFactors = FALSE
  ))
  carrybacks <- read_geometry_carrybacks_v2(data.frame(
    target_unit_2001 = "pc2001__01__01",
    source_unit_2011 = "pc2011__01__001",
    source_id = "geometry_source",
    status = "accepted",
    note = "Geometry decision",
    stringsAsFactors = FALSE
  ))

  issues <- validate_lineage_source_references_v2(
    registry,
    source_matches = matches,
    geometry_carrybacks = carrybacks
  )

  expect_equal(nrow(issues), 0L)
  expect_false(
    matches$unit_id[[2]] %in% carrybacks$target_unit_2001
  )
})

test_that("tracked NSS-64 code identities are unique registry matches", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  adjudications <- read.csv(
    file.path(root, "data", "metadata", "district_adjudications_v2.csv"),
    stringsAsFactors = FALSE
  )
  rows <- adjudications[
    adjudications$method %in%
      "official_nss64_census2001_code_identity",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 30L)
  expect_true(all(rows$wave == "nss_2007_08"))
  expect_true(all(rows$status == "accepted"))
  expect_true(all(rows$source_id == "nss64_education_district_codes"))
  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_true(all(grepl("^pc2001__", rows$unit_id)))
})

test_that("tracked NSS-64 code decisions cover every known Census unit", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  adjudications <- read.csv(
    file.path(
      root, "data", "metadata", "district_adjudications_v2.csv"
    ),
    stringsAsFactors = FALSE
  )
  code_rows <- adjudications[
    adjudications$method %in%
      "official_nss64_census2001_code_identity",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(code_rows), 401L)
  expect_true(all(code_rows$wave == "nss_2007_08"))
  expect_equal(anyDuplicated(code_rows$source_row_id), 0L)
  expect_true(all(grepl("^pc2001__", code_rows$unit_id)))
})
