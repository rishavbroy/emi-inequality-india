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
