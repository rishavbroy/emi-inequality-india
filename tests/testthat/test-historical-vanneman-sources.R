test_that("Vanneman source QA reads documented fixed-width identifiers", {
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
  write_gz <- function(path, lines) {
    con <- gzfile(path, open = "wt")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con)
  }
  panel <- file.path(td, "panel4.data.gz")
  dist81 <- file.path(td, "dist81.data.gz")
  dist91 <- file.path(td, "dist91.data.gz")
  write_gz(panel, c(
    "0201100615Srikakulam", "0201110615        1",
    "0201100715Srikakulam", "0201110715        1",
    "0201100815Srikakulam", "0201110815        1",
    "0201100915Srikakulam", "0201110915        1"
  ))
  write_gz(dist81, c("0201100812Srikakulam", "0201110812        1"))
  write_gz(dist91, c("0201100912Srikakulam", "0201110912        1"))

  expect_equal(vanneman_documented_panel_version(codebook), 6L)
  expect_equal(vanneman_documented_record_ids(education), c("151", "156"))
  ids <- vanneman_identifier_rows(panel)
  expect_equal(sort(unique(ids$year)), c(1961L, 1971L, 1981L, 1991L))
  expect_equal(unique(ids$version), 5L)
})

test_that("Vanneman source QA refuses to promote a panel vintage that disagrees with its codebook", {
  td <- tempfile(); dir.create(td)
  dir.create(file.path(td, "data/raw/census_1961-91/vanneman_1961-91/codebook"), recursive = TRUE)
  root <- file.path(td, "data/raw/census_1961-91/vanneman_1961-91")
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
  write_gz <- function(name, lines) {
    con <- gzfile(file.path(root, name), open = "wt")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con)
  }
  write_gz("panel4.data.gz", c(
    "0201100615Srikakulam", "0201100715Srikakulam",
    "0201100815Srikakulam", "0201100915Srikakulam"
  ))
  write_gz("dist81.data.gz", c(
    "0201100812Srikakulam",
    "0201151813        1"
  ))
  write_gz("dist91.data.gz", "0201100912Srikakulam")

  paths <- list(root = td)
  class(paths) <- "emi_paths"
  out <- summarize_vanneman_historical_sources(paths)
  panel <- out[out$source_id == "panel4", ]
  dist81 <- out[out$source_id == "dist81", ]
  dist91 <- out[out$source_id == "dist91", ]

  expect_equal(panel$observed_versions, "5")
  expect_equal(panel$documented_or_expected_version, 6L)
  expect_equal(panel$status, "version_mismatch")
  expect_equal(panel$provenance_gap, "panel_version_provenance_missing")
  expect_false(panel$version_provenance_resolved)
  expect_false(panel$eligible_for_baseline_values)

  expect_equal(dist81$observed_versions, "2;3")
  expect_equal(dist81$noncontract_record_ids, "151")
  expect_true(dist81$noncontract_record_definitions_present)
  expect_equal(
    dist81$provenance_gap,
    "record_definition_present_but_version_provenance_missing"
  )
  expect_equal(dist81$status, "mixed_record_versions")
  expect_false(dist81$version_provenance_resolved)
  expect_false(dist81$eligible_for_baseline_values)

  expect_equal(dist91$status, "source_contract_verified")
  expect_true(dist91$version_provenance_resolved)
  expect_identical(dist91$provenance_gap, "")
  expect_true(dist91$eligible_for_baseline_values)
})
