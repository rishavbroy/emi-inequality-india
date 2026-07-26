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
  specs <- district_lineage_input_specs(paths)

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

  out <- build_isded_candidate_events(raw)

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

  expect_error(read_lineage_source_registry(registry), "unique source_id")
})

test_that("candidate event years are not promoted to fabricated exact dates", {
  tracker <- data.frame(
    source_file_id = "district_splits", .row_in_source = 1L,
    source_state_raw = "State", source_district_raw = "Parent",
    target_state_raw = "State", target_district_raw = "Child",
    source_year = 2014L, target_year = 2015L, event_year = 2015L,
    change_type = "split_or_carveout", stringsAsFactors = FALSE
  )

  out <- build_candidate_admin_events(tracker, list())

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

  issues <- validate_lineage_source_references(registry, matches)

  expect_equal(issues$object_id, "b")
  expect_equal(issues$issue, "unregistered_source_id")

  matches$source_id[[2]] <- NA_character_
  issues <- validate_lineage_source_references(registry, matches)
  expect_equal(issues$issue, "missing_source_id")
})


test_that("source branches assemble by source ID rather than branch order", {
  branches <- list(
    list(source_id = "b", value = data.frame(x = 2)),
    list(source_id = "a", value = data.frame(x = 1))
  )

  out <- assemble_district_lineage_sources(branches)

  expect_named(out, c("b", "a"))
  expect_equal(out$a$x, 1)
  expect_error(
    assemble_district_lineage_sources(c(branches, branches[1])),
    "duplicate source IDs"
  )
  expect_error(
    assemble_district_lineage_sources(list(list(value = data.frame()))),
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

  inventory <- district_lineage_source_inventory(specs)
  branches <- split_district_lineage_source_specs(specs)

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
    census2001_code = "01",
    census2011_code = "001",
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

  out <- build_changed_component_roster(sources)

  expect_setequal(
    out$level,
    c("district", "subdistrict", "village", "urban_local_body")
  )
  expect_equal(nrow(out), 4L)
})


test_that("blank tracked lineage ledgers retain zero-row schemas", {
  events <- read_admin_events(data.frame())
  registry <- read_lineage_source_registry(data.frame())

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
  dir <- tempfile("district-lineage-")
  diagnostics <- list(
    source_matches = empty_source_matches(),
    missing_core_inputs = data.frame(source_id = character(), stringsAsFactors = FALSE)
  )

  save_district_lineage(diagnostics, dir)

  matches <- utils::read.csv(file.path(dir, "source_matches.csv"), check.names = FALSE)
  missing <- utils::read.csv(file.path(dir, "missing_core_inputs.csv"), check.names = FALSE)
  expect_named(matches, names(empty_source_matches()))
  expect_named(missing, "source_id")
  expect_equal(nrow(matches), 0L)
  expect_equal(nrow(missing), 0L)
})


test_that("empty duplicate diagnostics preserve their schema", {
  out <- duplicate_key_diagnostics(data.frame(id = "a"), "id", "fixture")

  expect_equal(nrow(out), 0L)
  expect_named(out, names(empty_duplicate_key_diagnostics()))
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

  eligibility <- data.frame(
    source_row_id = c("exact", "fuzzy"),
    status = "accepted",
    exclusion_reason = c(
      "geographic_lineage_no_accepted_parent_edge",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  out <- build_evidence_requests(
    events,
    sources,
    queue,
    eligibility
  )

  expect_true(any(out$request_id == "lineage__exact"))
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

  out <- summarize_shrid_bridge(bridge)

  expect_named(out, c("bridge_status", "n_shrid", "population", "area"))
  expect_setequal(out$bridge_status, c("stable", "changed"))
  expect_equal(sum(out$n_shrid), 3L)
  expect_equal(sum(out$population), 35)
  expect_equal(sum(out$area), 3.5)
})

test_that("lineage summary preserves the complete diagnostic metric contract", {
  summary <- lineage_summary(
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
    eligibility = data.frame(eligible_conservative = c(TRUE, FALSE)),
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

test_that("production Census 2001 geometry has an explicit tracked path", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  paths <- build_paths(root)

  expect_identical(
    normalizePath(lineage_geometry_2001_path(paths), mustWork = FALSE),
    normalizePath(
      file.path(
        root, "outputs", "derived", "district_lineage",
        "district_2001.gpkg"
      ),
      mustWork = FALSE
    )
  )
  specs <- district_lineage_input_specs(paths)
  expect_false("lineage_geometry_2001" %in% specs$source_id)
})

test_that("production geometry is attached without losing sf semantics", {
  skip_if_not_installed("sf")
  polygon <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  geometry <- sf::st_sf(
    unit_id = "pc2001__01__01",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  out <- attach_lineage_geometry_source(list(example = data.frame(x = 1)), geometry)

  expect_named(out, c("example", "lineage_geometry_2001"))
  expect_s3_class(out$lineage_geometry_2001, "sf")
  expect_identical(out$lineage_geometry_2001$unit_id, geometry$unit_id)
  expect_true(sf::st_crs(out$lineage_geometry_2001) == sf::st_crs(geometry))
})

test_that("production geometry reader enforces unit identity invariants", {
  skip_if_not_installed("sf")
  path <- tempfile(fileext = ".gpkg")
  on.exit(unlink(path), add = TRUE)
  polygon <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  geometry <- sf::st_sf(
    unit_id = "pc2001__01__01",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )
  sf::st_write(geometry, path, quiet = TRUE)

  out <- read_lineage_geometry_2001(path)

  expect_s3_class(out, "sf")
  expect_identical(out$unit_id, geometry$unit_id)
  expect_true(sf::st_crs(out) == sf::st_crs(geometry))
})

test_that("reviewed geometry and source decisions satisfy evidence contracts", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  metadata_path <- function(name) {
    file.path(root, "data", "metadata", name)
  }

  carrybacks <- read_geometry_carrybacks(
    read.csv(
      metadata_path("district_geometry_carrybacks.csv"),
      stringsAsFactors = FALSE
    )
  )
  adjudications <- read_adjudicated_source_matches(
    read.csv(
      metadata_path("district_adjudications.csv"),
      stringsAsFactors = FALSE
    )
  )
  registry <- read_lineage_source_registry(
    read.csv(
      metadata_path("district_lineage_sources.csv"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(carrybacks), 11L)
  expect_true(all(carrybacks$status == "accepted"))
  expect_equal(anyDuplicated(adjudications$source_row_id), 0L)
  expect_true(all(adjudications$status %in% c("accepted", "excluded")))
  expect_true(all(
    (adjudications$status == "excluded") ==
      (adjudications$method == "explicit_multi_parent_sensitivity_exclusion")
  ))
  expect_true(all(grepl(
    "^(official|reviewed|explicit)_",
    adjudications$method
  )))
  expect_contains(
    unique(adjudications$method),
    c(
      "official_nss64_census2001_code_identity",
      "official_nss75_exact_census2011_identity",
      "explicit_multi_parent_sensitivity_exclusion"
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

  nss75_matches <- adjudications[
    adjudications$method %in%
      "official_nss75_exact_name_deterministic_2011_to_2001",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(nss75_matches), 85L)
  expect_true(all(nss75_matches$wave == "nss_2017_18"))
  expect_true(all(grepl("^pc2011__", nss75_matches$unit_id)))
  expect_true(all(
    nss75_matches$source_id == "nss75_shrug_exact_deterministic"
  ))

  issues <- validate_lineage_source_references(
    registry,
    source_matches = adjudications,
    geometry_carrybacks = carrybacks
  )
  expect_equal(nrow(issues), 0L)
})

test_that("tracked high-coverage decisions leave only lower-coverage gaps", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  transition_path <- file.path(
    root, "outputs", "diagnostics", "extended",
    "district_lineage", "district_transition_2001_2011.csv"
  )
  skip_if_not(file.exists(transition_path))
  transition <- read.csv(transition_path, stringsAsFactors = FALSE)

  generated <- validate_allocation_weights(transition)
  weights <- read_adjudicated_allocation_weights(
    read_lineage_source(
      file.path(
        root, "data", "metadata", "district_allocation_weights.csv"
      ),
      reader = "allocation_csv",
      source_id = "lineage_allocation_weights"
    )
  )
  reviewed <- validate_adjudicated_allocation_weights(weights)
  decisions <- allocation_decision_status(weights)
  status <- allocation_coverage_status(
    generated, reviewed, decisions
  )

  expected <- allocation_decision_status(weights)
  incomplete_sources <- unique(generated$source_key[
    !(generated$coverage_complete %in% TRUE)
  ])
  expect_equal(
    status$n_reviewed_accepted,
    sum(
      expected$source_key %in% incomplete_sources &
        expected$decision_status %in% "accepted"
    )
  )
  expect_equal(
    status$n_reviewed_rejected,
    sum(
      expected$source_key %in% incomplete_sources &
        expected$decision_status %in% "rejected"
    )
  )
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
  summary <- lineage_summary(
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
    eligibility = data.frame(eligible_conservative = logical()),
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
    file.path(root, "data", "metadata", "district_adjudications.csv"),
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
  registry <- read_lineage_source_registry(data.frame(
    source_id = c("geometry_source", "survey_source"),
    citation = c("Geometry evidence", "Survey evidence"),
    path_or_url = c("geometry", "survey"),
    accessed = c("2026-07-23", "2026-07-23"),
    stringsAsFactors = FALSE
  ))
  matches <- read_adjudicated_source_matches(data.frame(
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
  carrybacks <- read_geometry_carrybacks(data.frame(
    target_unit_2001 = "pc2001__01__01",
    source_unit_2011 = "pc2011__01__001",
    source_id = "geometry_source",
    status = "accepted",
    note = "Geometry decision",
    stringsAsFactors = FALSE
  ))

  issues <- validate_lineage_source_references(
    registry,
    source_matches = matches,
    geometry_carrybacks = carrybacks
  )

  expect_equal(nrow(issues), 0L)
  expect_false(
    matches$unit_id[[2]] %in% carrybacks$target_unit_2001
  )
})

test_that("tracked NSS-64 code decisions cover every known Census unit", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  adjudications <- read.csv(
    file.path(
      root, "data", "metadata", "district_adjudications.csv"
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

test_that("tracked NSS-75 identities use exact names and deterministic bridges", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  metadata <- read.csv(
    file.path(
      root, "data", "metadata", "district_adjudications.csv"
    ),
    stringsAsFactors = FALSE
  )
  rows <- metadata[
    metadata$method %in%
      "official_nss75_exact_name_deterministic_2011_to_2001",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 85L)
  expect_true(all(rows$wave == "nss_2017_18"))
  expect_true(all(rows$status == "accepted"))
  expect_true(all(rows$source_id == "nss75_shrug_exact_deterministic"))
  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_true(all(grepl("^pc2011__", rows$unit_id)))
})

test_that("tracked Andaman decisions use reviewed official lineage", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  metadata_path <- function(name) {
    file.path(root, "data", "metadata", name)
  }
  adjudications <- read_adjudicated_source_matches(
    read.csv(
      metadata_path("district_adjudications.csv"),
      stringsAsFactors = FALSE
    )
  )
  events <- read_admin_events(
    read.csv(
      metadata_path("district_admin_events.csv"),
      stringsAsFactors = FALSE
    )
  )
  rows <- adjudications[
    adjudications$source_id %in% "census2011_andaman_reorganization",
    ,
    drop = FALSE
  ]
  edges <- events[
    events$source_id %in% "census2011_andaman_reorganization",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 4L)
  expect_setequal(
    rows$unit_id,
    c("pc2011__35__638", "pc2011__35__639", "pc2011__35__640")
  )
  expect_true(all(rows$status == "accepted"))
  expect_equal(nrow(edges), 3L)
  expect_true(all(edges$status == "accepted"))
  expect_setequal(
    paste(edges$from_unit, edges$to_unit, sep = "->"),
    c(
      "pc2001__35__02->pc2011__35__638",
      "pc2001__35__01->pc2011__35__639",
      "pc2001__35__01->pc2011__35__640"
    )
  )
})

test_that("official Census aliases identify only reviewed source rows", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  metadata <- read_adjudicated_source_matches(
    read.csv(
      file.path(
        root, "data", "metadata", "district_adjudications.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  rows <- metadata[
    metadata$method %in% "official_census2011_alias_identity",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 5L)
  expect_true(all(rows$wave == "nss_2017_18"))
  expect_true(all(rows$status == "accepted"))
  expect_true(all(rows$source_id == "census2011_official_district_aliases"))
  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_setequal(
    paste(rows$raw_district, rows$unit_id, sep = "->"),
    c(
      "Leh->pc2011__01__003",
      "Sri Ganganagar->pc2011__08__099",
      "Y.S.R. (Cuddapah)->pc2011__28__551",
      "Aizwal->pc2011__15__283",
      "Bhatinda->pc2011__03__046"
    )
  )
})

test_that("LGD modification rosters retain Census linkage codes", {
  raw <- data.frame(
    `District Code` = "233",
    `District Name(In English)` = "Kurung Kumey",
    `State Code` = "12",
    `State Name (In English)` = "Arunachal Pradesh",
    `Census 2001 Code` = "14",
    `Census 2011 Code` = "256",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  out <- standardize_lgd_modification_roster(raw, "district")

  expect_equal(nrow(out), 1L)
  expect_identical(out$census2001_code, "14")
  expect_identical(out$census2011_code, "256")
})

test_that("tracked LGD modification identities use official Census codes", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  metadata <- read_adjudicated_source_matches(
    read.csv(
      file.path(root, "data", "metadata", "district_adjudications.csv"),
      stringsAsFactors = FALSE
    )
  )
  rows <- metadata[
    metadata$method %in%
      "official_lgd_modification_census2011_identity",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 29L)
  expect_true(all(rows$wave == "nss_2017_18"))
  expect_true(all(rows$status == "accepted"))
  expect_true(all(rows$source_id == "lgd_mod_districts_census_codes"))
  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_true(all(grepl("^pc2011__", rows$unit_id)))

  telangana <- rows[rows$raw_state == "Telangana", , drop = FALSE]
  expect_setequal(
    telangana$unit_id,
    c(
      "pc2011__28__532",
      "pc2011__28__533",
      "pc2011__28__536",
      "pc2011__28__539",
      "pc2011__28__541"
    )
  )
  expect_false(any(grepl("^pc2011__36__", rows$unit_id)))
})

test_that("official NSS-75 exact identities remain separate from bridge eligibility", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  adjudications <- read_adjudicated_source_matches(
    read.csv(
      file.path(
        root, "data", "metadata", "district_adjudications.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  rows <- adjudications[
    adjudications$method %in%
      "official_nss75_exact_census2011_identity",
    ,
    drop = FALSE
  ]

  expect_gt(nrow(rows), 0L)
  expect_true(all(rows$wave == "nss_2017_18"))
  expect_true(all(rows$status == "accepted"))
  expect_true(all(
    rows$source_id ==
      "nss75_official_district_list_census2011_exact"
  ))
  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_true(all(grepl("^pc2011__", rows$unit_id)))
})

test_that("official NSS-75 exact current identities do not imply bridge eligibility", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  adjudications <- read_adjudicated_source_matches(
    read.csv(
      file.path(
        root, "data", "metadata", "district_adjudications.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  rows <- adjudications[
    adjudications$method %in%
      "official_nss75_exact_contemporaneous_identity",
    ,
    drop = FALSE
  ]
  exclusions <- adjudications[
    adjudications$method %in%
      "explicit_multi_parent_sensitivity_exclusion",
    ,
    drop = FALSE
  ]

  expect_gt(nrow(rows), 0L)
  expect_true(all(rows$wave == "nss_2017_18"))
  expect_true(all(rows$status == "accepted"))
  expect_true(all(
    rows$source_id == "nss75_official_exact_current_identity"
  ))
  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_false(any(rows$source_row_id %in% exclusions$source_row_id))
  expect_true(all(grepl("^(pc2011|lgd_district)__", rows$unit_id)))
  expect_setequal(
    rows$unit_id[rows$raw_state %in% c(
      "Daman & Diu", "Dadra & Nagar Haveli"
    )],
    c("pc2011__25__494", "pc2011__25__495", "pc2011__26__496")
  )
})

test_that("reviewed NSS-75 aliases complete source identity without granting ancestry", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  rows <- read_adjudicated_source_matches(
    read.csv(
      file.path(
        root, "data", "metadata", "district_adjudications.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  aliases <- rows[
    rows$method %in% "reviewed_nss75_official_alias_identity",
    ,
    drop = FALSE
  ]

  expect_equal(anyDuplicated(rows$source_row_id), 0L)
  expect_true(all(rows$status %in% c("accepted", "excluded")))
  expect_true(all(aliases$status == "accepted"))

  expect_equal(nrow(aliases), 16L)
  expect_true(all(aliases$wave == "nss_2017_18"))
  expect_true(all(
    aliases$source_id == "nss75_reviewed_district_aliases"
  ))
  expect_setequal(
    aliases$unit_id[aliases$raw_district %in% c(
      "Rangareddy", "Warangal Rural", "Warangal Urban"
    )],
    c(
      "lgd_district__518",
      "lgd_district__522",
      "lgd_district__686"
    )
  )
  expect_identical(
    aliases$unit_id[aliases$raw_district == "Maharajganj"],
    "pc2011__09__187"
  )
})

test_that("tracked Telangana parentage records only single-parent ancestry", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  events <- read_admin_events(
    read.csv(
      file.path(
        root, "data", "metadata", "district_admin_events.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  rows <- events[
    events$source_id %in% "telangana_2016_parent_district_review",
    ,
    drop = FALSE
  ]
  parent_counts <- table(rows$to_unit)
  single_parent_units <- names(parent_counts[parent_counts == 1L])
  single_parent <- rows[
    rows$to_unit %in% single_parent_units,
    ,
    drop = FALSE
  ]

  expect_equal(nrow(single_parent), 24L)
  expect_true(all(single_parent$status == "accepted"))
  expect_true(all(single_parent$effective_date == "2016-10-11"))
  expect_true(all(grepl("^pc2011__28__", single_parent$from_unit)))
  expect_true(all(grepl("^lgd_district__", single_parent$to_unit)))
  expect_true(all(is.na(single_parent$share)))

  expect_setequal(
    single_parent$to_unit[
      single_parent$from_unit == "pc2011__28__532"
    ],
    c(
      "lgd_district__680",
      "lgd_district__684",
      "lgd_district__699"
    )
  )
  expect_false(any(single_parent$to_unit %in% c(
    "lgd_district__688",
    "lgd_district__698"
  )))
})

test_that("tracked Chhattisgarh and Punjab events complete single-parent ancestry", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  events <- read_admin_events(
    read.csv(
      file.path(
        root, "data", "metadata", "district_admin_events.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  rows <- events[
    events$source_id %in% c(
      "chhattisgarh_2012_parent_district_review",
      "punjab_2011_parent_district_review"
    ),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 11L)
  expect_true(all(rows$status == "accepted"))
  expect_true(all(is.na(rows$share)))
  expect_true(all(grepl("^pc2011__", rows$from_unit)))
  expect_true(all(grepl("^lgd_district__", rows$to_unit)))

  expect_setequal(
    rows$to_unit[rows$from_unit == "pc2011__22__401"],
    c("lgd_district__648", "lgd_district__649")
  )
  expect_setequal(
    rows$to_unit[rows$from_unit == "pc2011__22__410"],
    c("lgd_district__644", "lgd_district__645")
  )
  expect_setequal(
    rows$to_unit[rows$from_unit == "pc2011__22__409"],
    c("lgd_district__646", "lgd_district__650")
  )
  expect_setequal(
    paste(rows$from_unit, rows$to_unit, sep = "->"),
    c(
      "pc2011__22__401->lgd_district__648",
      "pc2011__22__401->lgd_district__649",
      "pc2011__22__410->lgd_district__644",
      "pc2011__22__410->lgd_district__645",
      "pc2011__22__409->lgd_district__650",
      "pc2011__22__409->lgd_district__646",
      "pc2011__22__406->lgd_district__647",
      "pc2011__22__414->lgd_district__643",
      "pc2011__22__416->lgd_district__642",
      "pc2011__03__035->lgd_district__662",
      "pc2011__03__043->lgd_district__651"
    )
  )
})

test_that("tracked mixed-parent Telangana events remain non-deterministic", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  events <- read_admin_events(
    read.csv(
      file.path(
        root, "data", "metadata", "district_admin_events.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  rows <- events[
    events$event_id %in% c(
      "telangana_2016_537_698",
      "telangana_2016_538_698",
      "telangana_2016_540_688",
      "telangana_2016_541_688"
    ),
    ,
    drop = FALSE
  ]

  expect_equal(nrow(rows), 4L)
  expect_true(all(rows$status == "accepted"))
  expect_true(all(rows$effective_date == "2016-10-11"))
  expect_true(all(is.na(rows$share)))
  expect_setequal(
    rows$from_unit[rows$to_unit == "lgd_district__698"],
    c("pc2011__28__537", "pc2011__28__538")
  )
  expect_setequal(
    rows$from_unit[rows$to_unit == "lgd_district__688"],
    c("pc2011__28__540", "pc2011__28__541")
  )

  resolved <- resolve_lineage_terminals(
    c("lgd_district__698", "lgd_district__688"),
    events,
    data.frame(
      unit_id = c(
        "pc2001__28__06",
        "pc2001__28__07",
        "pc2001__28__09",
        "pc2001__28__10"
      ),
      stringsAsFactors = FALSE
    ),
    admin_2011 = data.frame(
      unit_id = c(
        "pc2011__28__537",
        "pc2011__28__538",
        "pc2011__28__540",
        "pc2011__28__541"
      ),
      stringsAsFactors = FALSE
    )
  )

  expect_true(all(
    resolved$resolution_status == "multiple_parent_non_nested"
  ))
  expect_true(all(is.na(resolved$terminal_unit)))
})

test_that("archived legacy mapping reviews cover every historical difference", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  reviews <- read_legacy_mapping_reviews(
    read.csv(
      file.path(
        root, "data", "metadata",
        "district_legacy_mapping_reviews.csv"
      ),
      stringsAsFactors = FALSE
    )
  )
  mapping <- reviews[
    reviews$review_scope %in% "mapping_difference",
    ,
    drop = FALSE
  ]
  downstream <- reviews[
    reviews$review_scope %in% "downstream_results",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(mapping), 7L)
  expect_true(all(mapping$status == "accepted"))
  expect_true(all(mapping$decision == "accept"))
  expect_equal(nrow(downstream), 1L)
  expect_identical(downstream$status, "accepted")
  expect_identical(
    downstream$decision,
    "accept_complete_source_panel"
  )
  expect_match(downstream$note, "not treated as an authoritative support universe")
  expect_match(downstream$note, "every NSS source identity is adjudicated")
  expect_match(downstream$note, "pooled multi-source Ginis are reconstructed")
})

test_that("terminal allocation decisions are complete and conservative", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  specs <- district_lineage_input_specs(build_paths(root))
  spec <- specs[
    specs$source_id == "lineage_allocation_weights",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(spec), 1L)
  allocations <- read_adjudicated_allocation_weights(
    read_lineage_source(
      spec$absolute_path[[1]],
      reader = spec$reader[[1]],
      source_id = spec$source_id[[1]]
    )
  )
  resolved_units <- c(
    "pc2011__24__483",
    "pc2011__21__371",
    "pc2011__01__011",
    "pc2011__18__315",
    "pc2011__24__493",
    "pc2011__20__361"
  )
  excluded_units <- c(
    "pc2011__18__324",
    "pc2011__18__326",
    "pc2011__01__022"
  )

  expect_true(all(
    resolved_units %in%
      allocations$source_unit[allocations$status == "accepted"]
  ))
  expect_true(all(
    excluded_units %in%
      allocations$source_unit[allocations$status == "rejected"]
  ))
  expect_false(any(
    allocations$status == "rejected" &
      (!is.na(allocations$target_2001) &
        nzchar(allocations$target_2001))
  ))
})


test_that("primary review ledger is registered", {
  specs <- district_lineage_input_specs(build_paths())
  expect_true("lineage_primary_reviews" %in% specs$source_id)
})

test_that("production lineage specs exclude archived legacy reviews", {
  specs <- district_lineage_input_specs(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  expect_false("lineage_legacy_reviews" %in% specs$source_id)
  expect_false(any(grepl("district_legacy_mapping_reviews.csv", specs$relative_path, fixed = TRUE)))
})

test_that("lineage readiness contains only current invariants", {
  readiness <- build_lineage_readiness(
    missing_core = character(),
    admin_2001 = data.frame(unit_id = "pc2001__01__01"),
    admin_2011 = data.frame(unit_id = "pc2011__01__001"),
    allocation_validation = data.frame(source_key = "pc2011__01__001", weights_well_formed = TRUE, coverage_complete = TRUE),
    source_roster = data.frame(source_row_id = "s1"),
    source_matches = data.frame(source_row_id = "s1", status = "accepted"),
    conservative_eligibility = data.frame(source_row_id = "s1", status = "accepted", eligible_conservative = TRUE),
    duplicate_keys = data.frame(),
    adjudicated_allocation_validation = data.frame(),
    source_reference_issues = data.frame(),
    full_reviewed_crosswalk = data.frame(source_row_id = "s1")
  )
  expect_true(readiness$passed[readiness$gate == "lineage_ready"])
  expect_false(any(grepl("migration|production|legacy", readiness$gate)))
  expect_equal(nrow(build_lineage_blockers(readiness)), 0L)
})
