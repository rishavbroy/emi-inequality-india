test_that("modern HCES official district codebook has unique complete geography", {
  path <- file.path(
    Sys.getenv("EMI_PROJECT_ROOT", "."),
    "data", "metadata", "hces_2022_24_district_codebook.csv"
  )
  codebook <- read_consumption_district_codebook_csv(path, "hces_2022_24")

  expect_equal(nrow(codebook), 695L)
  expect_equal(length(unique(codebook$state_code_source)), 36L)
  expect_equal(anyDuplicated(codebook[c("state_code_source", "district_code_source")]), 0L)
  expect_true(all(codebook$source_unit_kind == "district"))
  expect_true(all(codebook$source_lineage_eligible))

  ap <- codebook[
    codebook$state_code_source == "28" & codebook$district_code_source == "10",
    , drop = FALSE
  ]
  expect_equal(ap$district_name_source, "Y.S.R. (Cuddapah)")

  telangana <- codebook[
    codebook$state_code_source == "36" & codebook$district_code_source == "22",
    , drop = FALSE
  ]
  expect_equal(telangana$district_name_source, "Hyderabad")
})

test_that("modern HCES source geography reuses the canonical attachment contract", {
  path <- file.path(
    Sys.getenv("EMI_PROJECT_ROOT", "."),
    "data", "metadata", "hces_2022_24_district_codebook.csv"
  )
  codebook <- read_consumption_district_codebook_csv(path, "hces_2022_24")
  households <- data.frame(
    state_code_source = c("28", "36"),
    district_code_source = c("10", "22"),
    stratum = c("1", "1"),
    stringsAsFactors = FALSE
  )

  out <- attach_consumption_source_district_identity(households, codebook)
  expect_equal(out$source_district_name, c("Y.S.R. (Cuddapah)", "Hyderabad"))
  expect_equal(out$state_std, c("andhra pradesh", "telangana"))
})

test_that("modern HCES codebook preserves historical price geography after UT reorganization", {
  path <- file.path(
    Sys.getenv("EMI_PROJECT_ROOT", "."),
    "data", "metadata", "hces_2022_24_district_codebook.csv"
  )
  codebook <- read_consumption_district_codebook_csv(path, "hces_2022_24")

  modern <- data.frame(
    state_code_source = c("25", "25", "25", "37", "37", "28"),
    district_code_source = c("01", "02", "03", "01", "02", "10"),
    stratum = "1",
    stringsAsFactors = FALSE
  )
  out <- attach_consumption_source_district_identity(modern, codebook)

  expect_equal(
    out$price_state_code,
    c("DADI", "DADI", "DNHA", "JNK", "JNK", "28")
  )
  expect_equal(
    out$source_state_code,
    c("25", "25", "25", "37", "37", "28")
  )
})

test_that("NSS compact SSRDD district codes preserve the Census district component", {
  expect_equal(
    consumption_household_district_code(
      c("01113", "28216", "13"),
      c("01", "28", "28")
    ),
    c("13", "16", "13")
  )
})

test_that("labelled NSS geography builds a unique state-district codebook", {
  state <- haven::labelled(
    c(1, 1, 28),
    labels = c("Jammu & Kashmir" = 1, "Andhra Pradesh" = 28)
  )
  district <- haven::labelled(
    c(1113, 1114, 28216),
    labels = c("Jammu" = 1113, "Kathua" = 1114, "Krishna" = 28216)
  )
  raw <- list(household = data.frame(State = state, District = district))
  spec <- consumption_survey_spec(
    read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", "."))),
    "nss_2007_08_consumption"
  )

  out <- build_consumption_district_codebook_from_labels(raw, spec)
  expect_equal(out$state_code_source, c("01", "01", "28"))
  expect_equal(out$district_code_source, c("13", "14", "16"))
  expect_equal(out$district_std, c("jammu", "kathua", "krishna"))
  expect_equal(anyDuplicated(out[c("state_code_source", "district_code_source")]), 0L)
})

test_that("Census-2001 codebooks keep unmatched legacy district codes unresolved", {
  admin <- data.frame(
    unit_id = c("pc2001__20__01", "pc2001__20__02"),
    state_code = c("20", "20"),
    district_code = c("01", "02"),
    state_std = c("jharkhand", "jharkhand"),
    district_std = c("garhwa", "palamu"),
    stringsAsFactors = FALSE
  )
  households <- data.frame(
    state_code_source = c("34", "34"),
    district_code_source = c("01", "28"),
    stringsAsFactors = FALSE
  )
  spec <- data.frame(
    survey_id = "nss_2000_01", survey_family = "nss", survey_label = "Wave",
    survey_start = as.Date("2000-07-01"), survey_end = as.Date("2001-06-30"),
    schedule_variant = "schedule_1_0", analysis_role = "optional_pretrend",
    raw_path = "unused", price_timing = "quarterly_subround", price_group_months = 3,
    district_identity_source = "codes", mpce_contract = "schedule_1_0_summary",
    legacy_wave = NA, household_adapter = "direct_mpce",
    household_id_field = "HHID", household_id_suffix_field = "",
    mpce_field = "B3_q17", mpce_scale = 1, household_size_field = "B3_q1",
    household_size_encoding = "value", weight_field = "Wgt_Combined",
    state_field = "State", district_field = "District", sector_field = "Sector",
    subround_field = "SubRound", fsu_field = "Vill_Blk_Slno",
    stratum_field = "Stratum", sub_stratum_field = "SubStratum",
    stringsAsFactors = FALSE
  )
  crosswalk <- data.frame(
    source_id = "nss_2000_01", source_state_code = "34",
    state_code_2001 = "20", source_state_name = "Jharkhand",
    mapping_basis = "official", stringsAsFactors = FALSE
  )

  out <- build_consumption_census2001_codebook(
    households, admin, spec, crosswalk
  )

  expect_true(out$source_lineage_eligible[out$district_code_source == "01"])
  expect_false(out$source_lineage_eligible[out$district_code_source == "28"])
  expect_equal(
    out$source_unit_kind[out$district_code_source == "28"],
    "unresolved_legacy_district_code"
  )
})

test_that("name-coded NSS-57 states map directly to Census-2001 state codes", {
  admin <- data.frame(
    unit_id = c("pc2001__01__13", "pc2001__01__14"),
    state_code = c("01", "01"),
    district_code = c("13", "14"),
    state_std = c("jammu and kashmir", "jammu and kashmir"),
    district_std = c("jammu", "kathua"),
    stringsAsFactors = FALSE
  )
  households <- data.frame(
    state_code_source = c("Jammu & Kashmir", "Jammu & Kashmir"),
    district_code_source = c("13", "14"),
    stringsAsFactors = FALSE
  )
  registry <- read_consumption_survey_registry(build_paths(Sys.getenv("EMI_PROJECT_ROOT", ".")))
  spec <- consumption_survey_spec(registry, "nss_2001_02")

  out <- build_consumption_census2001_codebook(
    households, admin, spec, data.frame()
  )
  expect_true(all(out$source_lineage_eligible))
  expect_equal(out$district_std, c("jammu", "kathua"))
})
