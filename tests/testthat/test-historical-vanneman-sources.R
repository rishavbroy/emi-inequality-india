vanneman_test_sas <- function(source_id, record_ids = character()) {
  paste(
    sprintf("filename source pipe 'gunzip -c %s.data.gz';", source_id),
    "input stateid 1-2 distid 3-4 record 5-7 year 8-9 version 10",
    paste(sprintf("/* %s */", record_ids), collapse = " "),
    sep = "\n"
  )
}

vanneman_test_write_gz <- function(path, lines) {
  con <- gzfile(path, open = "wt")
  on.exit(close(con), add = TRUE)
  writeLines(lines, con)
}


vanneman_test_write_archive_checksums <- function(project_root, source_root) {
  rel <- c(
    "data_archived/panel4.data.gz",
    "data_archived/dist81.data.gz",
    "data_archived/dist91.data.gz",
    "sas_commands_archived/panel4.sas",
    "sas_commands_archived/dist81.sas",
    "sas_commands_archived/dist91.sas"
  )
  paths <- file.path(source_root, rel)
  registry <- data.frame(
    relative_path = rel,
    size_bytes = as.numeric(file.info(paths)$size),
    md5 = unname(tools::md5sum(paths)),
    sha256 = rep("fixture", length(rel)),
    archive_snapshot = rep("fixture", length(rel)),
    stringsAsFactors = FALSE
  )
  metadata_dir <- file.path(project_root, "data", "metadata")
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(registry, file.path(metadata_dir, "vanneman_archive_2013_checksums.csv"), row.names = FALSE)
}

test_that("Vanneman source QA reads documented fixed-width identifiers and SAS contracts", {
  td <- tempfile(); dir.create(td)
  codebook <- file.path(td, "codebook.html")
  writeLines(
    "Version number (2 = cross-sectional data; 6 = panel 1961-91 data)",
    codebook
  )
  education <- file.path(td, "education.html")
  writeLines(c(
    '<div align="CENTER"> 151 </div>',
    '<div align="CENTER"> 156 </div>'
  ), education)
  panel <- file.path(td, "panel4.data.gz")
  vanneman_test_write_gz(panel, c(
    "0201100615Srikakulam", "0201110615        1",
    "0201100715Srikakulam", "0201110715        1",
    "0201100815Srikakulam", "0201110815        1",
    "0201100915Srikakulam", "0201110915        1"
  ))

  expect_equal(vanneman_documented_panel_version(codebook), 6L)
  expect_equal(vanneman_documented_record_ids(education), c("151", "156"))
  ids <- vanneman_identifier_rows(panel)
  expect_equal(sort(unique(ids$year)), c(1961L, 1971L, 1981L, 1991L))
  expect_equal(unique(ids$version), 5L)

  sas <- file.path(td, "panel4.sas")
  writeLines(vanneman_test_sas("panel4", c("100", "151")), sas)
  contract <- vanneman_sas_reader_contract(sas, "panel4")
  expect_true(contract$source_specific_reader_targets_file)
  expect_true(contract$source_specific_identifier_layout_verified)
  expect_true(contract$parser_contract_verified)
})

test_that("file-specific SAS readers supersede generic version labels for parser eligibility", {
  td <- tempfile(); dir.create(td)
  root <- file.path(td, "data/raw/census_1961-91/vanneman_1961-91")
  dir.create(file.path(root, "codebook"), recursive = TRUE)
  dir.create(file.path(root, "data_archived"), recursive = TRUE)
  dir.create(file.path(root, "sas_commands_archived"), recursive = TRUE)
  writeLines(
    "Version number (2 = cross-sectional data; 6 = panel 1961-91 data)",
    file.path(root, "codebook/Codebook_ Indian district database.html")
  )
  writeLines(
    c('<div align="CENTER"> 100 </div>', '<div align="CENTER"> 151 </div>'),
    file.path(root, "codebook/Variables_ Indian district codebook.html")
  )
  writeLines(
    '<div align="CENTER"> 151 </div>',
    file.path(root, "codebook/Education and literacy_ Indian district codebook.html")
  )
  vanneman_test_write_gz(file.path(root, "data_archived/panel4.data.gz"), c(
    "0201100615Srikakulam", "0201100715Srikakulam",
    "0201100815Srikakulam", "0201100915Srikakulam"
  ))
  vanneman_test_write_gz(file.path(root, "data_archived/dist81.data.gz"), c(
    "0201100812Srikakulam",
    "0201151813        1"
  ))
  vanneman_test_write_gz(file.path(root, "data_archived/dist91.data.gz"), "0201100912Srikakulam")
  writeLines(vanneman_test_sas("panel4", "100"), file.path(root, "sas_commands_archived/panel4.sas"))
  writeLines(vanneman_test_sas("dist81", c("100", "151")), file.path(root, "sas_commands_archived/dist81.sas"))
  writeLines(vanneman_test_sas("dist91", "100"), file.path(root, "sas_commands_archived/dist91.sas"))
  vanneman_test_write_archive_checksums(td, root)

  paths <- list(root = td)
  class(paths) <- "emi_paths"
  out <- summarize_vanneman_historical_sources(paths)
  panel <- out[out$source_id == "panel4", ]
  dist81 <- out[out$source_id == "dist81", ]
  dist91 <- out[out$source_id == "dist91", ]

  expect_equal(panel$observed_versions, "5")
  expect_equal(panel$generic_codebook_version, 6L)
  expect_false(panel$generic_codebook_version_match)
  expect_true(panel$parser_contract_verified)
  expect_true(panel$archive_distribution_pair_verified)
  expect_true(panel$eligible_for_baseline_values)
  expect_equal(panel$status, "source_reader_verified_generic_version_differs")

  expect_equal(dist81$observed_versions, "2;3")
  expect_equal(dist81$version_exception_record_ids, "151")
  expect_true(dist81$version_exception_definitions_present)
  expect_true(dist81$source_specific_reader_covers_version_exceptions)
  expect_true(dist81$parser_contract_verified)
  expect_true(dist81$archive_distribution_pair_verified)
  expect_true(dist81$eligible_for_baseline_values)
  expect_equal(dist81$status, "source_reader_verified_generic_version_differs")

  expect_true(dist91$generic_codebook_version_match)
  expect_true(dist91$parser_contract_verified)
  expect_true(dist91$archive_distribution_pair_verified)
  expect_equal(dist91$status, "source_contract_verified")
  expect_true(dist91$eligible_for_baseline_values)
})

test_that("Vanneman parser eligibility fails closed when SAS does not cover exceptional records", {
  td <- tempfile(); dir.create(td)
  sas <- file.path(td, "dist81.sas")
  writeLines(vanneman_test_sas("dist81", "100"), sas)

  contract <- vanneman_sas_reader_contract(sas, "dist81", "151")
  expect_true(contract$source_specific_reader_targets_file)
  expect_true(contract$source_specific_identifier_layout_verified)
  expect_false(contract$source_specific_reader_covers_version_exceptions)
  expect_false(contract$parser_contract_verified)
})


test_that("Vanneman panel4 geography inventory exposes stable IDs without pretending they are Census IDs", {
  td <- tempfile(); dir.create(td)
  path <- file.path(td, "panel4.data.gz")
  lines <- c(
    "0201000615Hyderabad", "0201000715Hyderabad",
    "0201000815Hyderabad+Rangareddi", "0201000915Hyderabad+Rangareddi",
    "0201100615      100", "0201100715      120",
    "0201100815      140", "0201100915      160",
    "0202000615Srikakulam", "0202000715Srikakulam",
    "0202000815Srikakulam", "0202000915Srikakulam",
    "0202100615      200", "0202100715      220",
    "0202100815      240", "0202100915      260"
  )
  vanneman_test_write_gz(path, lines)

  out <- vanneman_panel4_geography_inventory(path)
  expect_equal(nrow(out), 2L)
  expect_identical(names(out)[1:2], c("vanneman_state_id", "vanneman_district_id"))
  aggregate <- out[out$vanneman_district_id == "01", , drop = FALSE]
  expect_identical(aggregate$district_label_1991, "Hyderabad+Rangareddi")
  expect_true(aggregate$explicit_aggregate_label_1991)
  expect_true(aggregate$label_changed_1981_1991 == FALSE)
  expect_equal(aggregate$population_1991, 160)
  expect_false(out$explicit_aggregate_label_1991[out$vanneman_district_id == "02"])
  expect_true(all(out$n_distinct_labels >= 1L))
})


test_that("Vanneman source QA fails eligibility when archived data-reader pairing changes", {
  td <- tempfile(); dir.create(td)
  root <- file.path(td, "data/raw/census_1961-91/vanneman_1961-91")
  dir.create(file.path(root, "codebook"), recursive = TRUE)
  dir.create(file.path(root, "data_archived"), recursive = TRUE)
  dir.create(file.path(root, "sas_commands_archived"), recursive = TRUE)
  writeLines(
    "Version number (2 = cross-sectional data; 6 = panel 1961-91 data)",
    file.path(root, "codebook/Codebook_ Indian district database.html")
  )
  writeLines('<div align="CENTER"> 100 </div>', file.path(root, "codebook/Variables_ Indian district codebook.html"))
  writeLines('<div align="CENTER"> 151 </div>', file.path(root, "codebook/Education and literacy_ Indian district codebook.html"))
  vanneman_test_write_gz(file.path(root, "data_archived/panel4.data.gz"), c(
    "0201100615Srikakulam", "0201100715Srikakulam",
    "0201100815Srikakulam", "0201100915Srikakulam"
  ))
  vanneman_test_write_gz(file.path(root, "data_archived/dist81.data.gz"), "0201100812Srikakulam")
  vanneman_test_write_gz(file.path(root, "data_archived/dist91.data.gz"), "0201100912Srikakulam")
  writeLines(vanneman_test_sas("panel4", "100"), file.path(root, "sas_commands_archived/panel4.sas"))
  writeLines(vanneman_test_sas("dist81", "100"), file.path(root, "sas_commands_archived/dist81.sas"))
  writeLines(vanneman_test_sas("dist91", "100"), file.path(root, "sas_commands_archived/dist91.sas"))
  vanneman_test_write_archive_checksums(td, root)
  cat("\nchanged", file = file.path(root, "sas_commands_archived/panel4.sas"), append = TRUE)

  paths <- list(root = td); class(paths) <- "emi_paths"
  out <- summarize_vanneman_historical_sources(paths)
  panel <- out[out$source_id == "panel4", , drop = FALSE]
  expect_false(panel$archive_sas_checksum_verified)
  expect_false(panel$archive_distribution_pair_verified)
  expect_false(panel$eligible_for_baseline_values)
  expect_equal(panel$status, "archive_distribution_checksum_mismatch")
})

test_that("Vanneman panel geography preserves documented 1991 missing-census sentinels", {
  td <- tempfile(); dir.create(td)
  path <- file.path(td, "panel4.data.gz")
  lines <- c(
    "1309000615Doda", "1309000715Doda", "1309000815Doda", "1309000915Doda",
    "1309100615   262471", "1309100715   342220", "1309100815   425262", "1309100915       -1",
    "0201000615Srikakulam", "0201000715Srikakulam", "0201000815Srikakulam", "0201000915Srikakulam",
    "0201100615      100", "0201100715      120", "0201100815      140", "0201100915      160"
  )
  vanneman_test_write_gz(path, lines)

  out <- vanneman_panel4_geography_inventory(path)
  doda <- out[out$vanneman_state_id == "13", , drop = FALSE]
  expect_false(doda$population_1991_available)
  expect_true(is.na(doda$population_1991))
  expect_equal(doda$population_1991_status, "documented_missing_sentinel")
  expect_true(out$population_1991_available[out$vanneman_state_id == "02"])
})

test_that("Vanneman panel to 1991 crosswalk automates only deterministic exact labels", {
  panel <- data.frame(
    vanneman_state_id = c("02", "02", "13", "03"),
    vanneman_district_id = c("01", "16", "09", "00"),
    district_label_1991 = c("Srikakulam", "Hyderabad+Rangareddi", "Doda", "Arunachal Pradesh"),
    explicit_aggregate_label_1991 = c(FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  dist91 <- data.frame(
    dist91_state_id = c("02", "02", "02", "03"),
    dist91_district_id = c("01", "15", "16", "00"),
    dist91_district_label = c("Srikakulam", "Rangareddi", "Hyderabad", "Arunachal Pradesh"),
    stringsAsFactors = FALSE
  )
  dist91$dist91_label_key <- canonicalize_district_name(dist91$dist91_district_label)
  states <- data.frame(
    panel_state_id = c("02", "13", "03"),
    dist81_state_id = c("02", "13", "03"),
    dist91_state_id = c("02", NA, "03"),
    state_name = c("Andhra Pradesh", "Jammu & Kashmir", "Arunachal Pradesh"),
    panel_to_1991_state_status = c("mapped_one_to_one", "no_1991_census", "mapped_one_to_one"),
    source_basis = "fixture",
    stringsAsFactors = FALSE
  )

  out <- vanneman_panel4_dist91_crosswalk(panel, dist91, states, documented_combined_units = "0216")
  one <- out[out$panel_unit_id == "0201", , drop = FALSE]
  combined <- out[out$panel_unit_id == "0216", , drop = FALSE]
  jammu <- out[out$panel_unit_id == "1309", , drop = FALSE]
  small <- out[out$panel_unit_id == "0300", , drop = FALSE]

  expect_true(one$preferred_pretrend_eligible)
  expect_equal(one$mapping_class, "deterministic_one_to_one")
  expect_equal(one$dist91_district_id, "01")
  expect_false(combined$preferred_pretrend_eligible)
  expect_equal(combined$mapping_class, "aggregate_requires_review")
  expect_equal(jammu$mapping_class, "no_1991_census")
  expect_equal(small$mapping_class, "small_state_aggregate")
})

test_that("documented Vanneman combined units are parsed from the author codebook", {
  path <- tempfile(fileext = ".html")
  writeLines(c("Andhra Pradesh: 0216 = Rangareddi + Hyderabad", "Bihar: 0501 = Patna + Nalanda"), path)
  expect_setequal(vanneman_documented_combined_panel_units(path), c("0216", "0501"))
})
