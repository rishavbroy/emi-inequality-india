test_that("Census 1991 validation manifest uses the historically observed file scope", {
  expect_identical(census_1991_state_file_codes(), sprintf("%02d", c(2:9, 11:33)))
  expect_false("10" %in% census_1991_state_file_codes())
  expect_setequal(
    census_1991_validation_tables(),
    c("B01S", "C02T", "C02U", "C06T", "C09T")
  )
})

test_that("Census manifest helper requires explicit 1991 file scope", {
  root <- tempfile("census1991-manifest-")
  dir.create(root)
  dir.create(file.path(root, "data/raw/census_1991"), recursive = TRUE)
  manifest <- file.path(root, "census_1991_download_manifest.tsv")
  rows <- data.frame(
    table = "TEST", state_code = c("02", "03"),
    relative_path = c(
      "data/raw/census_1991/test-02.xlsx",
      "data/raw/census_1991/test-03.xlsx"
    ),
    url = c("https://example.invalid/02", "https://example.invalid/03"),
    stringsAsFactors = FALSE
  )
  utils::write.table(rows, manifest, sep = "\t", row.names = FALSE, quote = FALSE)
  for (path in file.path(root, rows$relative_path)) writeLines("fixture", path)
  paths <- list(root = root)

  expect_error(
    census_manifest_files(paths, 1991L, "TEST", manifest_file = manifest),
    "declare their expected geographic file scope"
  )
  files <- census_manifest_files(
    paths, 1991L, "TEST", manifest_file = manifest, expected_states = c("02", "03")
  )
  expect_equal(basename(files), c("test-02.xlsx", "test-03.xlsx"))
  expect_error(
    census_manifest_files(
      paths, 1991L, "TEST", manifest_file = manifest, expected_states = c("02", "04")
    ),
    "one row for each declared geographic file code"
  )
})


test_that("Census 1991 B-01(S) parser enforces the worker-status partition", {
  raw <- as.data.frame(matrix(NA_character_, nrow = 2, ncol = 21), stringsAsFactors = FALSE)
  raw[1, 1:6] <- c("B01T", "02", "00", "State", "Total", "TOTAL")
  raw[2, 1:6] <- c("B01T", "02", "01", "District-A", "Total", "TOTAL")
  raw[2, c(7, 10, 13, 16)] <- c("100", "40", "10", "50")
  out <- parse_census_1991_b01s_sheet(raw)
  expect_equal(nrow(out), 1L)
  expect_identical(out$state_code_1991, "02")
  expect_identical(out$district_code_1991, "01")
  expect_equal(out$main_workers_b01s_1991_count, 40)

  raw[2, 16] <- "49"
  expect_error(parse_census_1991_b01s_sheet(raw), "do not exhaust")
})

test_that("Census 1991 C-02 parser constructs the primary secondary-plus count", {
  raw <- as.data.frame(matrix("0", nrow = 2, ncol = 29), stringsAsFactors = FALSE)
  raw[, 1] <- "C02T"
  raw[, 2] <- "02"
  raw[, 3] <- "01"
  raw[, 4] <- "District-A"
  raw[, 5] <- "Total"
  raw[, 6] <- c("All ages", "0-6")
  raw[, 7] <- c("100", "20")
  raw[1, 20:29] <- c("5", "4", "3", "2", "1", "1", "1", "1", "1", "1")
  out <- parse_census_1991_c02t_sheet(raw)
  expect_equal(out$population_7plus_c02t_1991_count, 80)
  expect_equal(out$secondary_plus_c02t_1991_count, 20)
})

test_that("Census 1991 C-06 parser preserves the dependency accounting contract", {
  ages <- c("All ages", census_1991_c06_age_groups())
  raw <- as.data.frame(matrix("0", nrow = length(ages), ncol = 13), stringsAsFactors = FALSE)
  raw[, 1] <- "C06T"
  raw[, 2] <- "02"
  raw[, 3] <- "01"
  raw[, 4] <- "District-A"
  raw[, 5] <- "Total"
  raw[, 6] <- ages
  raw[1, 7:8] <- c("105", "105")
  raw[-1, 7:8] <- "5"
  out <- parse_census_1991_c06t_sheet(raw)
  expect_equal(out$population_c06t_1991_count, 210)
  expect_equal(out$dependent_population_c06t_1991_count, 110)
  expect_equal(out$working_age_population_c06t_1991_count, 100)
})

test_that("Census 1991 C-09 parser preserves published population anomalies", {
  raw <- as.data.frame(matrix("0", nrow = 1, ncol = 32), stringsAsFactors = FALSE)
  raw[1, 1:5] <- c("C09T", "02", "01", "District-A", "Total")
  raw[1, 6] <- "81"
  raw[1, c(9, 12, 15, 18, 21, 24, 27, 30)] <- rep("10", 8)
  out <- parse_census_1991_c09t_sheet(raw)
  expect_equal(out$muslim_population_c09t_1991_count, 10)
  expect_equal(out$religion_population_sum_c09t_1991_count, 80)
  expect_equal(out$population_c09t_1991_count, 81)
})

test_that("primary Census 1991 validation preserves exact contracts and explicit anomalies", {
  keys <- data.frame(
    state_code_1991 = c("02", "02"), district_code_1991 = c("01", "02"),
    stringsAsFactors = FALSE
  )
  reference <- cbind(keys, data.frame(
    population_1991_count = c(100, 80), rural_population_1991_count = c(60, 80),
    matriculate_plus_1991_count = c(20, 10), main_workers_1991_count = c(40, 30),
    dependent_population_1991_count = c(45, 35), working_age_population_1991_count = c(50, 40),
    muslim_population_1991_count = c(10, 8), stringsAsFactors = FALSE
  ))
  b01 <- cbind(keys, data.frame(
    population_b01s_1991_count = c(100, 80), main_workers_b01s_1991_count = c(40, 30),
    marginal_workers_b01s_1991_count = c(5, 4), nonworkers_b01s_1991_count = c(55, 46)
  ))
  c02t <- cbind(keys, data.frame(
    population_c02t_1991_count = c(100, 80), population_0_6_c02t_1991_count = c(15, 12),
    population_7plus_c02t_1991_count = c(85, 68), secondary_plus_c02t_1991_count = c(20, 10)
  ))
  c02u <- cbind(keys[1, , drop = FALSE], data.frame(
    urban_population_c02u_1991_count = 40, urban_secondary_plus_c02u_1991_count = 12
  ))
  c06 <- cbind(keys, data.frame(
    population_c06t_1991_count = c(100, 80), dependent_population_c06t_1991_count = c(45, 35),
    working_age_population_c06t_1991_count = c(50, 40)
  ))
  c09 <- cbind(keys, data.frame(
    population_c09t_1991_count = c(101, 80),
    muslim_population_c09t_1991_count = c(10, 8),
    religion_population_sum_c09t_1991_count = c(100, 80)
  ))

  out <- build_census_1991_primary_validation(b01, c02t, c02u, c06, c09, reference)
  strict <- out$comparison_summary[out$comparison_summary$exact_required, , drop = FALSE]
  expect_true(all(strict$exact_match_share == 1))
  expect_equal(
    out$comparison_summary$exact_match_share[
      out$comparison_summary$measure_id == "population_c09t"
    ],
    .5
  )
  expect_true(any(out$discrepancies$measure_id == "population_c09t"))
  expect_equal(
    out$comparison_summary$compared_districts[
      out$comparison_summary$measure_id == "urban_population_c02u"
    ],
    2L
  )
})
