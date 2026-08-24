test_that("detailed consumption adapters emit one canonical household schema", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))

  type2 <- consumption_survey_spec(registry, "nss_2009_10_type2")
  raw <- data.frame(
    HH_ID = c("001", "002"), HH_Size = c(4, 2), Multiplier = c(10, 20),
    State = c("14", "14"), District = c("1406", "1406"), Sector = c("2", "1"),
    Sub_Round = c("4", "4"), FSU_Serial_number = c("00001", "00002"),
    Stratum = c("1", "1"), Sub_Stratum = c("01", "01"),
    MPCE = c(1777.59, 900), stringsAsFactors = FALSE
  )
  out <- canonicalize_detailed_consumption_households(raw, type2)

  expect_named(out, c(
    "survey_id", "household_id", "state_code_source", "district_code_source",
    "sector", "subround", "fsu", "stratum", "sub_stratum", "household_size",
    "survey_weight", "nominal_mpce",
    "nominal_household_consumption", "mpce_contract"
  ))
  expect_equal(out$nominal_mpce, c(1777.59, 900))
  expect_equal(out$nominal_household_consumption, c(7110.36, 1800))
  expect_equal(out$survey_weight, c(10, 20))
})

test_that("2011-12 adapter applies the documented hundredths-of-rupees MPCE scale", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "nss_2011_12_type2")
  households <- data.frame(
    HHID = c("715581201", "715581202"), hh_size = c(5, 2), Combined_Multiplier = c(324.08, 150),
    State_code = c("17", "17"), District_Code = c("1701", "1701"), Sector = c("1", "1"),
    Sub_Round = c("1", "1"), FSU_Serial_No = c("15581", "15581"),
    Stratum = c("1", "1"), Sub_Stratum_No = c("1", "1"), stringsAsFactors = FALSE
  )
  mpce <- data.frame(HHID = c("715581201", "715581202"), MPCE = c(123088, 50000))

  out <- canonicalize_detailed_consumption_households(households, spec, mpce)

  expect_equal(out$nominal_mpce, c(1230.88, 500))
  expect_equal(out$nominal_household_consumption, c(6154.4, 1000))
})

test_that("2004-05 split household adapter can join official MPCE to household size", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "nss_2004_05")
  households <- data.frame(
    HHID = c("220001101", "220001102"), B3_q1 = c(5, 3), Wgt_Combined = c(22.93335, 11),
    State = c("19", "19"), District = c("27", "27"), Sector = c("Urban", "Rural"),
    SubRound = c("Sub round 1", "Sub round 1"), Vill_Blk_Slno = c("00001", "00002"),
    Stratum = c("1", "1"), SubStratum = c("1", "1"), stringsAsFactors = FALSE
  )
  mpce <- data.frame(HHID = c("220001101", "220001102"), B3_q29 = c(1881.65, 750))

  out <- canonicalize_detailed_consumption_households(households, spec, mpce)

  expect_equal(out$nominal_mpce, c(1881.65, 750))
  expect_equal(out$nominal_household_consumption, c(9408.25, 2250))
})

test_that("adapter contracts reject unsupported surveys and incomplete joins", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  modern <- consumption_survey_spec(registry, "hces_2022_23")
  expect_error(
    canonicalize_detailed_consumption_households(data.frame(), modern),
    "does not use a direct detailed-MPCE adapter"
  )

  spec <- consumption_survey_spec(registry, "nss_2011_12_type2")
  households <- data.frame(
    HHID = "1", hh_size = 2, Combined_Multiplier = 1, State_code = "17",
    District_Code = "1701", Sector = "1", Sub_Round = "1", FSU_Serial_No = "15581",
    Stratum = "1", Sub_Stratum_No = "1"
  )
  expect_error(
    canonicalize_detailed_consumption_households(households, spec, data.frame(HHID = "2", MPCE = 10000)),
    "do not have one-to-one household coverage"
  )
})


test_that("registered detailed-consumption reader discovers source members by column contract", {
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "nss_2009_10_type2")
  td <- tempfile("consumption-archive-")
  dir.create(td)
  household <- data.frame(
    HH_ID = "000001101", HH_Size = "3", Multiplier = "186.375", State = "14",
    District = "1406", Sector = "2", Sub_Round = "4", FSU_Serial_number = "00001",
    Stratum = "1", Sub_Stratum = "01", MPCE = "1777.59", stringsAsFactors = FALSE
  )
  noise <- data.frame(HH_ID = "000001101", Item = "1", Value = "5")
  utils::write.csv(household, file.path(td, "household.csv"), row.names = FALSE)
  utils::write.csv(noise, file.path(td, "items.csv"), row.names = FALSE)
  old <- setwd(td)
  on.exit(setwd(old), add = TRUE)
  archive <- file.path(td, "survey.zip")
  utils::zip(archive, c("household.csv", "items.csv"), flags = "-q")

  out <- read_registered_detailed_consumption(archive, spec)
  expect_equal(out$nominal_mpce, 1777.59)
  expect_equal(out$fsu, "00001")
  expect_equal(out$stratum, "1")
  expect_equal(out$sub_stratum, "01")
})

test_that("consumption source geography normalizes two- and four-digit district codes", {
  codebook <- data.frame(
    source_id = "round",
    state_code_source = c("14", "14"),
    district_code_source = c("06", "07"),
    state_name_source = c("Manipur", "Manipur"),
    district_name_source = c("Thoubal", "Bishnupur"),
    state_std = c("manipur", "manipur"),
    district_std = c("thoubal", "bishnupur"),
    stringsAsFactors = FALSE
  )
  households <- data.frame(
    state_code_source = c("14", "14"),
    district_code_source = c("1406", "07"),
    stringsAsFactors = FALSE
  )
  out <- attach_consumption_source_district_identity(households, codebook)
  expect_equal(out$source_district_code, c("06", "07"))
  expect_equal(out$source_district_name, c("Thoubal", "Bishnupur"))
})

test_that("consumption source geography resolves state-name households without guessing code order", {
  codebook <- data.frame(
    source_id = "round",
    state_code_source = c("24", "24"), district_code_source = c("01", "02"),
    state_name_source = c("Gujarat", "Gujarat"), district_name_source = c("Kachchh", "Banas Kantha"),
    state_std = c("gujarat", "gujarat"), district_std = c("kachchh", "banas kantha"),
    stringsAsFactors = FALSE
  )
  households <- data.frame(state_code_source = "Gujrat", district_code_source = "2", stringsAsFactors = FALSE)
  out <- attach_consumption_source_district_identity(households, codebook)
  expect_equal(out$source_state_code, "24")
  expect_equal(out$source_district_code, "02")
  expect_equal(out$district_std, "banas kantha")
})

test_that("consumption source geography fails unresolved survey codes", {
  codebook <- data.frame(
    source_id = "round", state_code_source = "14", district_code_source = "06",
    state_name_source = "Manipur", district_name_source = "Thoubal",
    state_std = "manipur", district_std = "thoubal", stringsAsFactors = FALSE
  )
  households <- data.frame(state_code_source = "14", district_code_source = "1499", stringsAsFactors = FALSE)
  expect_error(attach_consumption_source_district_identity(households, codebook), "unresolved")
})

test_that("district-codebook normalization rejects duplicate code identities", {
  raw <- data.frame(
    state = c("Assam", ""), state_code = c("18", "18"),
    district = c("One", "Two"), district_code = c("01", "01"), stringsAsFactors = FALSE
  )
  expect_error(
    normalize_consumption_codebook(raw, "state", "state_code", "district", "district_code", "test"),
    "duplicate state/district codes"
  )
})


test_that("district-codebook anomaly review surfaces foreign-state text without rewriting it", {
  codebook <- data.frame(
    source_id = c("round", "round"),
    state_code_source = c("18", "31"), district_code_source = c("12", "01"),
    state_name_source = c("Assam", "Lakshadweep"),
    district_name_source = c("Lakshadweephimpur", "Lakshadweep"),
    state_std = c("assam", "lakshadweep"), district_std = c("lakshadweephimpur", "lakshadweep"),
    stringsAsFactors = FALSE
  )
  anomalies <- consumption_codebook_name_anomalies(codebook)
  expect_equal(anomalies$district_name_source, "Lakshadweephimpur")
})


test_that("NSS 68 district DDI parser collapses repeated consistent District_Code definitions", {
  ddi <- tempfile(fileext = ".xml")
  writeLines(c(
    "<codeBook xmlns='urn:ddi:test'><dataDscr>",
    "<var ID='V1' name='District_Code'>",
    "<catgry><catValu>1406</catValu><labl>Thoubal</labl></catgry>",
    "<catgry><catValu>1407</catValu><labl>Bishnupur</labl></catgry>",
    "</var>",
    "<var ID='V2' name='District_Code'>",
    "<catgry><catValu>1406</catValu><labl>Thoubal</labl></catgry>",
    "<catgry><catValu>1407</catValu><labl>Bishnupur</labl></catgry>",
    "</var></dataDscr></codeBook>"
  ), ddi)
  out <- read_consumption_district_codebook_ddi(ddi, "nss_2011_12")
  expect_equal(out$state_code_source, c("14", "14"))
  expect_equal(out$district_code_source, c("06", "07"))
  expect_equal(out$district_name_source, c("Thoubal", "Bishnupur"))
  expect_equal(out$state_std, c("manipur", "manipur"))
})

test_that("NSS 68 district DDI parser rejects conflicting repeated code labels", {
  ddi <- tempfile(fileext = ".xml")
  writeLines(c(
    "<codeBook><dataDscr>",
    "<var name='District_Code'><catgry><catValu>1406</catValu><labl>Thoubal</labl></catgry></var>",
    "<var name='District_Code'><catgry><catValu>1406</catValu><labl>Wrong name</labl></catgry></var>",
    "</dataDscr></codeBook>"
  ), ddi)
  expect_error(
    read_consumption_district_codebook_ddi(ddi, "nss_2011_12"),
    "conflicting labels for district codes: 1406"
  )
})

test_that("district-codebook anomaly review avoids short-state substring false positives", {
  codebook <- data.frame(
    source_id = c("round", "round"),
    state_code_source = c("18", "30"), district_code_source = c("03", "01"),
    state_name_source = c("Assam", "Goa"), district_name_source = c("Goalpara", "North Goa"),
    state_std = c("assam", "goa"), district_std = c("goalpara", "north goa"),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(consumption_codebook_name_anomalies(codebook)), 0L)
})
