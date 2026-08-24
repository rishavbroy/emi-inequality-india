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
