liu_benchmark_write_construction_contract <- function(raw_root) {
  dm <- file.path(raw_root, "dm-Stata")
  dir.create(dm, recursive = TRUE, showWarnings = FALSE)
  writeLines("dictionary using Data/Source/PCA/panel4.data {", file.path(dm, "lst-dm-01a-Vanneman_dictionary.dct"))
  writeLines(
    "merge 1:1 state_id dist_id using Data/Source/PCA/Vanneman_district_crosswalk.dta",
    file.path(dm, "lst-dm-01a-clean_Vanneman_data.do")
  )
  writeLines(
    c("rename state_id_ st_code", "rename dist_id_ dist_code"),
    file.path(dm, "lst-dm-01b-make_pca_1961_1991.do")
  )
  writeLines(
    c(
      "append using Data/Derived/PCA/pca_1961_1991.dta",
      "egen state_id = group(statename_temp)",
      "egen district_id = group(state_id dtname_temp)"
    ),
    file.path(dm, "lst-dm-01d-make_pca_1961_2011.do")
  )
}

test_that("Liu Vanneman benchmark preserves stable IDs without importing PCA geography IDs", {
  td <- tempfile(); dir.create(td)
  raw_root <- file.path(td, "data/raw/maggieliuDataCodeClimate2023")
  dir.create(raw_root, recursive = TRUE)
  liu_benchmark_write_construction_contract(raw_root)

  haven::write_dta(
    data.frame(
      state_id = c(2, 2),
      dist_id = c(1, 2),
      dist_name_Vanneman = c("Srikakulam", "Visakhapatnam(pt)+Srikak.(pt"),
      state_name_Vanneman = c("Andhra Pradesh", "Andhra Pradesh"),
      state_name_david = c("Andhra Pradesh", "Andhra Pradesh"),
      dist_name_david = c("Srikakulam", "Vizianagaram"),
      state_dist = c(201, 202),
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "Vanneman_district_crosswalk.dta")
  )
  writeLines(
    c(
      "0201000915Srikakulam",
      "0202000915Vizianagaram"
    ),
    file.path(raw_root, "panel4_lst.data")
  )
  haven::write_dta(
    data.frame(
      state = c("andhra pradesh", "andhra pradesh"),
      district = c("srikakulam and visakhapatnam", "srikakulam and visakhapatnam"),
      state_id = c(2, 2),
      district_id = c(16, 16),
      st_code = c(2, 2),
      statename = c("andhra pradesh", "andhra pradesh"),
      dist_code = c(1, 2),
      dtname = c("srikakulam", "vizianagaram"),
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "PCA_census1991_dist_match.dta")
  )
  haven::write_dta(
    data.frame(year = 2011),
    file.path(raw_root, "PCA_census2011_dist_match.dta")
  )

  panel <- data.frame(
    vanneman_state_id = c("02", "02"),
    vanneman_district_id = c("01", "02"),
    district_label_1961 = c("Srikakulam", "Visakhapatnam(pt)+Srikak.(pt"),
    district_label_1991 = c("Srikakulam", "Vizianagaram"),
    stringsAsFactors = FALSE
  )
  paths <- list(root = td); class(paths) <- "emi_paths"
  out <- build_vanneman_liu_geography_benchmark(panel, paths)

  expect_equal(nrow(out$panel_comparison), 2L)
  expect_true(all(out$panel_comparison$panel_1991_name_agrees_liu_harmonized))
  expect_true(all(out$panel_comparison$liu_panel4_copy_present))
  expect_true(all(out$panel_comparison$liu_panel4_1991_name_agrees_current))
  expect_true(all(out$panel_comparison$benchmark_status == "external_identity_and_1991_label_agree"))

  pca_groups <- out$source_summary$value[out$source_summary$metric == "pca1991_harmonized_groups"]
  expect_equal(pca_groups, 1)
  expect_false(pca_groups == nrow(panel))
})

test_that("Liu benchmark retains missing external panel copies as benchmark QA rather than exclusions", {
  td <- tempfile(); dir.create(td)
  raw_root <- file.path(td, "data/raw/maggieliuDataCodeClimate2023")
  dir.create(raw_root, recursive = TRUE)
  liu_benchmark_write_construction_contract(raw_root)

  haven::write_dta(
    data.frame(
      state_id = c(29, 29), dist_id = c(9, 10),
      dist_name_Vanneman = c("Calcutta", "Howrah"),
      state_name_Vanneman = c("West Bengal", "West Bengal"),
      state_name_david = c("West Bengal", "West Bengal"),
      dist_name_david = c("Kolkata", "Howrah"),
      state_dist = c(2909, 2910),
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "Vanneman_district_crosswalk.dta")
  )
  writeLines("2910000915Howrah", file.path(raw_root, "panel4_lst.data"))
  haven::write_dta(
    data.frame(
      state = "west bengal", district = "kolkata and howrah", state_id = 29,
      district_id = 1, st_code = 29, statename = "west bengal", dist_code = 1, dtname = "kolkata",
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "PCA_census1991_dist_match.dta")
  )
  haven::write_dta(data.frame(year = 2011), file.path(raw_root, "PCA_census2011_dist_match.dta"))

  panel <- data.frame(
    vanneman_state_id = c("29", "29"),
    vanneman_district_id = c("09", "10"),
    district_label_1961 = c("Calcutta", "Howrah"),
    district_label_1991 = c("Kolkata", "Howrah"),
    stringsAsFactors = FALSE
  )
  paths <- list(root = td); class(paths) <- "emi_paths"
  out <- build_vanneman_liu_geography_benchmark(panel, paths)$panel_comparison

  calcutta <- out[out$panel_unit_id == "2909", , drop = FALSE]
  expect_false(calcutta$liu_panel4_copy_present)
  expect_equal(calcutta$benchmark_status, "external_panel_copy_missing")
  expect_true(calcutta$panel_1991_name_agrees_liu_harmonized)
})

test_that("Liu benchmark requires exactly the canonical Vanneman stable-ID universe", {
  td <- tempfile(); dir.create(td)
  raw_root <- file.path(td, "data/raw/maggieliuDataCodeClimate2023")
  dir.create(raw_root, recursive = TRUE)
  liu_benchmark_write_construction_contract(raw_root)
  haven::write_dta(
    data.frame(
      state_id = 2, dist_id = 1, dist_name_Vanneman = "Srikakulam",
      state_name_Vanneman = "Andhra Pradesh", state_name_david = "Andhra Pradesh",
      dist_name_david = "Srikakulam", state_dist = 201,
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "Vanneman_district_crosswalk.dta")
  )
  writeLines("0201000915Srikakulam", file.path(raw_root, "panel4_lst.data"))
  haven::write_dta(
    data.frame(
      state = "andhra pradesh", district = "srikakulam", state_id = 2, district_id = 1,
      st_code = 2, statename = "andhra pradesh", dist_code = 1, dtname = "srikakulam",
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "PCA_census1991_dist_match.dta")
  )
  haven::write_dta(data.frame(year = 2011), file.path(raw_root, "PCA_census2011_dist_match.dta"))

  panel <- data.frame(
    vanneman_state_id = c("02", "02"), vanneman_district_id = c("01", "02"),
    district_label_1961 = c("Srikakulam", "Vizianagaram"),
    district_label_1991 = c("Srikakulam", "Vizianagaram"),
    stringsAsFactors = FALSE
  )
  paths <- list(root = td); class(paths) <- "emi_paths"
  expect_error(
    build_vanneman_liu_geography_benchmark(panel, paths),
    "does not cover exactly the current stable panel IDs"
  )
})


test_that("Liu construction scripts document stable Vanneman IDs before separate six-census harmonization", {
  td <- tempfile(); dir.create(td)
  raw_root <- file.path(td, "data/raw/maggieliuDataCodeClimate2023")
  liu_benchmark_write_construction_contract(raw_root)
  paths <- list(root = td); class(paths) <- "emi_paths"

  contract <- liu_vanneman_construction_contract(paths)
  expect_true(all(contract$passed))
  expect_match(
    contract$interpretation[contract$check == "stable_id_merge"],
    "1:1"
  )
  expect_match(
    contract$interpretation[contract$check == "six_census_rebuilds_harmonized_ids"],
    "separate harmonized geography"
  )
})

test_that("Liu district aliases are parsed from published construction code", {
  path <- tempfile(fileext = ".do")
  writeLines(c(
    "gen dtname_temp = lower(dtname)",
    "replace dtname_temp=\"vishakhapatnam\" if dtname_temp==\"visakhapatanam\"",
    "replace dtname_temp=\"amristar\" if dtname_temp==\"amritsar\" | dtname_temp==\"amritsar \"",
    "* Generate consistent state and district identifiers across six Census rounds"
  ), path)

  out <- liu_direct_district_aliases(path)
  expect_true(any(out$source_key == "visakhapatanam" & out$target_key == "vishakhapatnam"))
  expect_equal(sum(out$target_key == "amristar"), 2L)
})

test_that("reviewed Vanneman aliases require stable IDs, raw Census codes, and a direct published rule", {
  td <- tempfile(); dir.create(td)
  raw_root <- file.path(td, "data/raw/maggieliuDataCodeClimate2023")
  dir.create(file.path(raw_root, "dm-Stata"), recursive = TRUE)
  writeLines(c(
    "gen dtname_temp = lower(dtname)",
    "replace dtname_temp=\"vishakhapatnam\" if dtname_temp==\"visakhapatanam\"",
    "* Generate consistent state and district identifiers across six Census rounds"
  ), file.path(raw_root, "dm-Stata/lst-dm-01d-make_pca_1961_2011.do"))
  haven::write_dta(
    data.frame(
      state_id = 2, dist_id = 3, dist_name_Vanneman = "Visakhapatnam",
      state_name_Vanneman = "Andhra Pradesh", state_name_david = "Andhra Pradesh",
      dist_name_david = "Vishakhapatnam", state_dist = 203,
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "Vanneman_district_crosswalk.dta")
  )
  haven::write_dta(
    data.frame(
      state = c("andhra pradesh", "andhra pradesh"),
      district = c("vishakhapatnam", "east godavari"),
      state_id = c(2, 2), district_id = c(1, 2),
      st_code = c(2, 2), statename = c("andhra pradesh", "andhra pradesh"),
      dist_code = c(3, 4), dtname = c("visakhapatanam", "east godavari"),
      stringsAsFactors = FALSE
    ),
    file.path(raw_root, "PCA_census1991_dist_match.dta")
  )
  vanneman_root <- file.path(td, "data/raw/census_1961-91/vanneman_1961-91/data_archived")
  dir.create(vanneman_root, recursive = TRUE)
  con <- gzfile(file.path(vanneman_root, "dist91.data.gz"), "wt")
  writeLines(c("0203000912Visakhapatanam", "0204000912East Godavari"), con); close(con)

  panel <- data.frame(
    panel_unit_id = "0203", dist91_state_id = "02",
    mapping_class = "label_review_required", stringsAsFactors = FALSE
  )
  ledger <- data.frame(
    panel_unit_id = "0203", dist91_state_id = "02", dist91_district_id = "03",
    decision = "accepted_one_to_one", source_id = "maggieliuDataCodeClimate2023",
    evidence = "published_direct_alias_and_raw_1991_code", stringsAsFactors = FALSE
  )
  paths <- list(root = td); class(paths) <- "emi_paths"
  out <- validate_vanneman_panel4_dist91_adjudications(ledger, panel, paths)

  expect_equal(out$dist91_district_label, "Visakhapatanam")
  expect_equal(out$evidence_status, "verified_direct_alias")
  expect_true(is.finite(out$liu_alias_source_line))

  bad <- ledger
  bad$dist91_district_id <- "04"
  expect_error(
    validate_vanneman_panel4_dist91_adjudications(bad, panel, paths),
    "lacks a direct published Liu district-name alias rule"
  )
  aggregate <- ledger
  aggregate$dist91_district_id <- "00"
  expect_error(
    validate_vanneman_panel4_dist91_adjudications(aggregate, panel, paths),
    "may not promote district-00 aggregates"
  )
})
