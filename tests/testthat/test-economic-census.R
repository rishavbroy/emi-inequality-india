test_that("SHRUG EC05 reader uses the documented district member and count contract", {
  td <- tempfile("ec05-")
  dir.create(td)
  csv <- file.path(td, "ec05_pc01dist.csv")
  utils::write.csv(
    data.frame(
      pc01_state_id = c(9, 9), pc01_district_id = c(1, 2),
      ec05_emp_all = c(100, 200), ec05_emp_f = c(20, 50),
      ec05_emp_hired = c(60, 100), ec05_emp_priv = c(80, 150),
      ec05_emp_inf = c(30, 100), ec05_emp_manuf = c(25, 50),
      ec05_emp_services = c(65, 120), ec05_count_all = c(20, 25)
    ),
    csv,
    row.names = FALSE
  )
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  zip <- tempfile(fileext = ".zip")
  utils::zip(zipfile = zip, files = basename(csv), flags = "-q")

  out <- read_shrug_ec05_district(zip)

  expect_identical(out$state_code, c("09", "09"))
  expect_identical(out$district_code, c("01", "02"))
  expect_equal(out$nonfarm_employment, c(100, 200))
  expect_equal(out$firms_total, c(20, 25))
})

test_that("SHRUG EC05 reader rejects malformed core counts and duplicate districts", {
  make_zip <- function(x) {
    td <- tempfile("ec05-")
    dir.create(td)
    csv <- file.path(td, "ec05_pc01dist.csv")
    utils::write.csv(x, csv, row.names = FALSE)
    old <- setwd(td)
    on.exit(setwd(old), add = TRUE)
    zip <- tempfile(fileext = ".zip")
    utils::zip(zipfile = zip, files = basename(csv), flags = "-q")
    zip
  }
  base <- data.frame(
    pc01_state_id = 9, pc01_district_id = 1,
    ec05_emp_all = 100, ec05_emp_f = 20, ec05_emp_hired = 60,
    ec05_emp_priv = 80, ec05_emp_inf = 30, ec05_emp_manuf = 25,
    ec05_emp_services = 65, ec05_count_all = 20
  )
  negative <- base
  negative$ec05_emp_all <- -1
  expect_error(read_shrug_ec05_district(make_zip(negative)), "negative core counts")

  duplicate <- rbind(base, base)
  expect_error(read_shrug_ec05_district(make_zip(duplicate)), "unique by complete district keys")
})

test_that("EC05 measures retain documented source gaps rather than imputing them", {
  source <- data.frame(
    state_code = c("09", "09"), district_code = c("01", "02"),
    nonfarm_employment = c(100, 200), female_employment = c(20, 50),
    hired_employment = c(60, 100), private_employment = c(80, 150),
    informal_employment = c(30, 100), manufacturing_employment = c(25, 50),
    services_employment = c(65, 120), firms_total = c(20, 25)
  )
  admin <- data.frame(
    level = "district", state_code = c("09", "09", "09"),
    district_code = c("01", "02", "03"), state_std = "state",
    district_std = c("a", "b", "c"), stringsAsFactors = FALSE
  )

  out <- build_economic_census_2005_measures(source, admin, minimum_source_coverage = 0.5)

  expect_equal(nrow(out), 3L)
  expect_identical(out$source_available, c(TRUE, TRUE, FALSE))
  expect_equal(out$female_employment_share[1:2], c(.2, .25))
  expect_equal(out$mean_employment_per_firm[1:2], c(5, 8))
  expect_true(is.na(out$nonfarm_employment[[3L]]))
  expect_true(is.na(out$services_employment_share[[3L]]))
})

test_that("EC05 measures fail on noncanonical districts or implausibly low coverage", {
  source <- data.frame(
    state_code = "09", district_code = "99", nonfarm_employment = 100,
    female_employment = 20, hired_employment = 60, private_employment = 80,
    informal_employment = 30, manufacturing_employment = 25,
    services_employment = 65, firms_total = 20
  )
  admin <- data.frame(
    level = "district", state_code = "09", district_code = "01",
    state_std = "state", district_std = "a", stringsAsFactors = FALSE
  )
  expect_error(
    build_economic_census_2005_measures(source, admin, minimum_source_coverage = 0),
    "outside the canonical Census-2001 registry"
  )

  source$district_code <- "01"
  admin <- rbind(admin, transform(admin, district_code = "02", district_std = "b"))
  expect_error(
    build_economic_census_2005_measures(source, admin),
    "coverage"
  )

  source <- rbind(source, transform(source, district_code = "02"))
  source$female_employment[[1L]] <- 101
  expect_error(
    build_economic_census_2005_measures(source, admin, minimum_source_coverage = 0),
    "bounded subsets"
  )
})

test_that("Economic Census DDI validates the shared Sixth-EC establishment schema", {
  required <- c(
    "ST", "DT", "BACT", "NIC3", "OWN_SHIP_C",
    "M_H", "F_H", "M_NH", "F_NH", "TOTAL_WORKER", "SECTOR"
  )
  make_ddi <- function(drop = character()) {
    vars <- setdiff(required, drop)
    var_xml <- paste0(
      '<var files="F1 F2" name="', vars, '"><labl>', vars, '</labl></var>',
      collapse = ""
    )
    xml <- paste0(
      '<?xml version="1.0"?><codeBook xmlns="http://www.icpsr.umich.edu/DDI">',
      '<fileDscr ID="F1"><fileTxt><fileName>EC6A_ST01_ALPHA.NSDstat</fileName>',
      '<dimensns><caseQnty>10</caseQnty></dimensns></fileTxt></fileDscr>',
      '<fileDscr ID="F2"><fileTxt><fileName>EC6A_ST37_BETA.NSDstat</fileName>',
      '<dimensns><caseQnty>20</caseQnty></dimensns></fileTxt></fileDscr>',
      '<dataDscr>', var_xml, '</dataDscr></codeBook>'
    )
    path <- tempfile(fileext = ".xml")
    writeLines(xml, path)
    path
  }

  out <- read_economic_census_ddi_contract(make_ddi())
  expect_identical(out$state_code, c("01", "37"))
  expect_equal(out$case_count, c(10, 20))
  expect_true(all(out$required_variables_complete))

  expect_error(
    read_economic_census_ddi_contract(make_ddi("NIC3")),
    "required establishment schema"
  )
})

test_that("shared Economic Census count validation rejects malformed canonical sources", {
  source <- data.frame(
    state_code = "09", district_code = "01",
    nonfarm_employment = 100, female_employment = 20,
    hired_employment = 60, private_employment = 80,
    informal_employment = 30, manufacturing_employment = 25,
    services_employment = 65, firms_total = 20
  )
  expect_equal(
    validate_economic_census_source_counts(source, "test source"),
    source
  )

  duplicate <- rbind(source, source)
  expect_error(
    validate_economic_census_source_counts(duplicate, "test source"),
    "unique by complete district keys"
  )
})


test_that("SHRUG EC13 reader uses the complete Census-2011 district product", {
  td <- tempfile("ec13-")
  dir.create(td)
  csv <- file.path(td, "ec13_pc11dist.csv")
  utils::write.csv(
    data.frame(
      pc11_state_id = c(9, 9), pc11_district_id = c(1, 2),
      ec13_emp_all = c(120, 240), ec13_emp_f = c(30, 72),
      ec13_emp_hired = c(72, 144), ec13_emp_priv = c(90, 180),
      ec13_emp_manuf = c(24, 60), ec13_emp_services = c(84, 144),
      ec13_count_all = c(24, 30)
    ),
    csv,
    row.names = FALSE
  )
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  zip <- tempfile(fileext = ".zip")
  utils::zip(zipfile = zip, files = basename(csv), flags = "-q")

  out <- read_shrug_ec13_district(zip)

  expect_identical(out$state_code, c("09", "09"))
  expect_identical(out$district_code, c("001", "002"))
  expect_equal(out$nonfarm_employment, c(120, 240))
  expect_false("informal_employment" %in% names(out))
})

test_that("EC13 counts are pooled before shares and changes are constructed", {
  source <- data.frame(
    state_code = c("09", "09"), district_code = c("001", "002"),
    nonfarm_employment = c(100, 300), female_employment = c(10, 90),
    hired_employment = c(40, 180), private_employment = c(80, 240),
    manufacturing_employment = c(20, 60), services_employment = c(70, 210),
    firms_total = c(20, 30), stringsAsFactors = FALSE
  )
  admin11 <- data.frame(
    level = "district", state_code = c("09", "09"), district_code = c("001", "002"),
    district_std = c("child a", "child b"), stringsAsFactors = FALSE
  )
  admin01 <- data.frame(
    level = "district", state_code = "09", district_code = "01",
    state_std = "state", district_std = "parent", stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("09", "09"), district_code_2011 = c("001", "002"),
    state_code_2001 = c("09", "09"), district_code_2001 = c("01", "01"),
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "deterministic_containment", stringsAsFactors = FALSE
  )

  followup <- build_economic_census_2013_measures(source, admin11, admin01, transition, expected_source_districts = 2L)
  expect_equal(nrow(followup), 1L)
  expect_true(followup$harmonized_available)
  expect_equal(followup$nonfarm_employment, 400)
  expect_equal(followup$female_employment_share, 0.25)
  expect_equal(followup$mean_employment_per_firm, 8)

  baseline <- data.frame(
    target_unit_2001 = "pc2001__09__01",
    log_nonfarm_employment = log(200), log_firms_total = log(25),
    mean_employment_per_firm = 8,
    female_employment_share = .20, hired_employment_share = .50,
    private_employment_share = .70, manufacturing_employment_share = .25,
    services_employment_share = .60,
    stringsAsFactors = FALSE
  )
  changes <- build_economic_census_change_measures(baseline, followup)
  expect_equal(changes$log_nonfarm_employment_change_2013_2005, log(2))
  expect_equal(changes$female_employment_share_change_2013_2005, .05)
})

test_that("Economic Census longitudinal family excludes EC05-only informal employment", {
  expect_false("informal_employment_share" %in% economic_census_longitudinal_measure_columns())
  expect_true("informal_employment_share" %in% names(economic_census_share_specs(TRUE)))
})

test_that("Economic Census mechanism registry is compact and predeclared", {
  registry <- economic_census_mechanism_registry()

  expect_equal(nrow(registry), 6L)
  expect_equal(anyDuplicated(registry$outcome_id), 0L)
  expect_equal(anyDuplicated(registry$variable), 0L)
  expect_setequal(
    registry$variable,
    c(
      "log_nonfarm_employment_change_2013_2005",
      "log_firms_total_change_2013_2005",
      "hired_employment_share_change_2013_2005",
      "private_employment_share_change_2013_2005",
      "services_employment_share_change_2013_2005",
      "manufacturing_employment_share_change_2013_2005"
    )
  )
  expect_false(any(grepl("informal|female|mean_employment", registry$variable)))
  expect_equal(sum(registry$tier == "core"), 5L)
  expect_equal(sum(registry$tier == "secondary"), 1L)
})

test_that("Economic Census mechanism registry is supported by longitudinal measures", {
  registry <- economic_census_mechanism_registry()
  expected <- paste0(economic_census_longitudinal_measure_columns(), "_change_2013_2005")

  expect_true(all(registry$variable %in% expected))
  expect_false("informal_employment_share_change_2013_2005" %in% expected)
})

economic_census_2005_test_raw <- function() {
  data.frame(
    schedule = rep("53", 5L),
    state_code = rep("09", 5L), district_code = c("01", "01", "01", "01", "02"),
    activity = c("1", "1", "2", "1", "1"),
    nic_2004 = c("7210", "5211", "7220", "7210", "7290"),
    agri_class = c("2", "2", "2", "1", "2"), workers = c(10, 20, 50, 30, 5),
    stringsAsFactors = FALSE
  )
}

test_that("EC05 IT parser does not depend on the redundant rural/urban sector byte", {
  positions <- economic_census_2005_it_fwf_positions()
  expect_false("sector" %in% positions$col_names)

  raw <- economic_census_2005_test_raw()
  expect_silent(summarise_economic_census_2005_it_rows(raw))
  raw$schedule[[1L]] <- "99"
  expect_error(
    summarise_economic_census_2005_it_rows(raw),
    "malformed schedule/geography/worker fields"
  )
})

test_that("EC05 IT baseline uses major nonfarm NIC-2004 Division 72 establishments", {
  out <- summarise_economic_census_2005_it_rows(economic_census_2005_test_raw())
  expect_equal(nrow(out), 2L)
  one <- out[out$district_code == "01", , drop = FALSE]
  expect_equal(one$nonfarm_firms_raw, 2)
  expect_equal(one$nonfarm_employment_raw, 30)
  expect_equal(one$it_firms, 1)
  expect_equal(one$it_employment, 10)

  # Subsidiary activity and agricultural rows never enter either numerator or denominator.
  expect_false(any(out$nonfarm_employment_raw == 80))
})

test_that("official EC05 directory separates fixed-width state and district names", {
  td <- tempfile("ec05-directory-")
  dir.create(td)
  directory <- file.path(td, "Directory.txt")
  writeLines(
    c(
      sprintf("%02d%02d%-30s%s %02d", 10, 38, "BIHAR", "Arval *", 38),
      sprintf("%02d%02d%-30s%s %02d", 23, 46, "MADHYA PRADESH", "Ashok Nagar", 46)
    ),
    directory
  )
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  zip <- tempfile(fileext = ".zip")
  utils::zip(zipfile = zip, files = basename(directory), flags = "-q")

  out <- economic_census_2005_directory(zip)

  expect_identical(out$state_code, c("10", "23"))
  expect_identical(out$district_code, c("38", "46"))
  expect_identical(out$district_name, c("Arval *", "Ashok Nagar"))
  expect_false(any(grepl("BIHAR|MADHYA PRADESH", out$district_name)))
})

test_that("EC05 IT baseline pools only deterministic post-2001 splits", {
  source <- data.frame(
    state_code = rep("09", 4L), district_code = c("01", "02", "03", "04"),
    district_name = c("parent a", "parent b", "child a", "child b"),
    nonfarm_firms_raw = c(8, 4, 2, 1), nonfarm_employment_raw = c(80, 40, 20, 10),
    it_firms = c(1, 1, 1, 0), it_employment = c(8, 4, 2, 0), stringsAsFactors = FALSE
  )
  admin01 <- data.frame(
    level = "district", state_code = c("09", "09"), district_code = c("01", "02"),
    state_std = "state", district_std = c("parent a", "parent b"), stringsAsFactors = FALSE
  )
  admin11 <- data.frame(
    level = "district", state_code = c("09", "09"), district_code = c("101", "102"),
    district_std = c("child a", "child b"), stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("09", "09"), district_code_2011 = c("101", "102"),
    state_code_2001 = c("09", "09"), district_code_2001 = c("01", "02"),
    population_share_to_2001 = c(1, .5), area_share_to_2001 = c(1, .5), shrid_coverage = 1,
    mapping_class = c("reviewed_single_parent_ancestry", "non_nested_or_incomplete"),
    stringsAsFactors = FALSE
  )

  out <- build_economic_census_2005_it_baseline(source, admin01, admin11, transition)
  expect_true(out$source_available[out$district_code == "01"])
  expect_equal(out$nonfarm_employment_raw[out$district_code == "01"], 100)
  expect_equal(out$it_employment_share_nonfarm[out$district_code == "01"], .10)
  expect_false(out$source_available[out$district_code == "02"])
  expect_true(is.na(out$it_employment[out$district_code == "02"]))

  bad <- source
  bad$it_firms[[1L]] <- 9
  expect_error(
    build_economic_census_2005_it_baseline(bad, admin01, admin11, transition),
    "subset accounting"
  )
})

test_that("EC05 IT baseline does not fabricate a split for merged district codes", {
  source <- data.frame(
    state_code = "27", district_code = "22", district_name = "Mumbai",
    nonfarm_firms_raw = 10, nonfarm_employment_raw = 100,
    it_firms = 2, it_employment = 20, stringsAsFactors = FALSE
  )
  admin01 <- data.frame(
    level = "district", state_code = c("27", "27"), district_code = c("22", "23"),
    state_std = "maharashtra", district_std = c("mumbai suburban", "mumbai"), stringsAsFactors = FALSE
  )
  admin11 <- data.frame(
    level = "district", state_code = "27", district_code = "519", district_std = "mumbai",
    stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = "27", district_code_2011 = "519",
    state_code_2001 = "27", district_code_2001 = "23",
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge", stringsAsFactors = FALSE
  )

  out <- build_economic_census_2005_it_baseline(source, admin01, admin11, transition)
  expect_equal(nrow(out), 2L)
  expect_false(any(out$source_available))
  expect_true(all(is.na(out$it_firm_share_nonfarm)))
})

test_that("EC05 IT opportunity baseline stays outside the weak-IV outcome registry", {
  expect_false(any(grepl("it_|computer", economic_census_mechanism_registry()$variable)))
})
