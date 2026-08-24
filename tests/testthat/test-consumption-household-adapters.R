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
