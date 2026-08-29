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
  dir.create(file.path(root, "sas_commands"), recursive = TRUE)
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
  vanneman_test_write_gz(file.path(root, "panel4.data.gz"), c(
    "0201100615Srikakulam", "0201100715Srikakulam",
    "0201100815Srikakulam", "0201100915Srikakulam"
  ))
  vanneman_test_write_gz(file.path(root, "dist81.data.gz"), c(
    "0201100812Srikakulam",
    "0201151813        1"
  ))
  vanneman_test_write_gz(file.path(root, "dist91.data.gz"), "0201100912Srikakulam")
  writeLines(vanneman_test_sas("panel4", "100"), file.path(root, "sas_commands/panel4.sas"))
  writeLines(vanneman_test_sas("dist81", c("100", "151")), file.path(root, "sas_commands/dist81.sas"))
  writeLines(vanneman_test_sas("dist91", "100"), file.path(root, "sas_commands/dist91.sas"))

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
  expect_true(panel$eligible_for_baseline_values)
  expect_equal(panel$status, "source_reader_verified_generic_version_differs")

  expect_equal(dist81$observed_versions, "2;3")
  expect_equal(dist81$version_exception_record_ids, "151")
  expect_true(dist81$version_exception_definitions_present)
  expect_true(dist81$source_specific_reader_covers_version_exceptions)
  expect_true(dist81$parser_contract_verified)
  expect_true(dist81$eligible_for_baseline_values)
  expect_equal(dist81$status, "source_reader_verified_generic_version_differs")

  expect_true(dist91$generic_codebook_version_match)
  expect_true(dist91$parser_contract_verified)
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
