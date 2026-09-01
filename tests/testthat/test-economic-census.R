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
  expect_error(read_shrug_ec05_district(make_zip(duplicate)), "unique by complete Census-2001 keys")
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
