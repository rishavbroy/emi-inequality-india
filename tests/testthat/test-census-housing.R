test_that("Census 2001 H09 lighting categories exhaust households", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 13), stringsAsFactors = FALSE)
  raw[1, 1:6] <- c("H4009", "09", "01", "0000", "District - Alpha 01", "Total")
  raw[1, 7:13] <- c(100, 60, 30, 2, 1, 3, 4)
  out <- parse_census_h09_2001_sheet(raw)
  expect_equal(out$households_total, 100)
  expect_equal(out$lighting_electricity, 60)
  raw[1, 13] <- 3
  expect_error(parse_census_h09_2001_sheet(raw), "do not sum exactly")
})

test_that("Census 2001 H12 independently partitions electricity and latrine access", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 13), stringsAsFactors = FALSE)
  raw[1, 1:8] <- c("H4912", "09", "01", "0000", "District - Alpha 01", "Total", "All Sources", "Total")
  raw[1, 9:13] <- c(100, 60, 40, 25, 75)
  out <- parse_census_h12_2001_sheet(raw)
  expect_equal(out$electricity_available, 60)
  expect_equal(out$latrine_available, 25)
  raw[1, 13] <- 74
  expect_error(parse_census_h12_2001_sheet(raw), "latrine categories do not sum exactly")
})

test_that("Census 2011 HL11 derives marginal electricity and latrine counts from joint cells", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 14), stringsAsFactors = FALSE)
  raw[1, 1:9] <- c(
    "HH3711", "09", "132", "00000", "000000", "District - Alpha", "Total",
    "All Sources", "Total number of households"
  )
  raw[1, 10:14] <- c(100, 20, 40, 10, 30)
  out <- parse_census_hl11_2011_sheet(raw)
  expect_equal(out$electricity_available, 60)
  expect_equal(out$latrine_available, 30)
  raw[1, 14] <- 29
  expect_error(parse_census_hl11_2011_sheet(raw), "do not sum exactly")
})

test_that("Census asset readers retain common and 2011-only technologies without additive asset assumptions", {
  h13 <- data.frame(matrix("", nrow = 1, ncol = 15), stringsAsFactors = FALSE)
  h13[1, 1:6] <- c("H5813", "09", "01", "0000", "District - Alpha 01", "Total")
  h13[1, 7:15] <- c(100, 40, 20, 50, 10, 60, 15, 5, 25)
  out01 <- parse_census_h13_2001_sheet(h13)
  expect_equal(out01$banking, 40)
  expect_equal(out01$telephone, 10)

  hl12 <- data.frame(matrix("", nrow = 1, ncol = 20), stringsAsFactors = FALSE)
  hl12[1, 1:7] <- c("HH4012", "09", "132", "00000", "000000", "District - Alpha", "Total")
  hl12[1, 8:20] <- c(100, 50, 20, 60, 10, 15, 5, 30, 10, 40, 20, 5, 8)
  out11 <- parse_census_hl12_2011_sheet(hl12)
  expect_equal(out11$telephone, 45)
  expect_equal(out11$computer, 25)
  expect_equal(out11$computer_internet, 10)
})

test_that("Housing cross-table validation enforces common household and electricity universes", {
  light <- data.frame(
    state_code = "09", district_code = "01", households_total = 100,
    lighting_electricity = 60, stringsAsFactors = FALSE
  )
  utility <- data.frame(
    state_code = "09", district_code = "01", households_total = 100,
    electricity_available = 60, stringsAsFactors = FALSE
  )
  assets <- data.frame(
    state_code = "09", district_code = "01", households_total = 100,
    stringsAsFactors = FALSE
  )
  out <- validate_census_housing_sources(light, utility, assets, 2001L)
  expect_equal(nrow(out), 3L)
  expect_true(all(out$max_abs_difference == 0))

  utility$electricity_available <- 59
  expect_error(validate_census_housing_sources(light, utility, assets, 2001L), "counts disagree")
})

test_that("Census housing counts are pooled before living-standard shares", {
  hl07 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"), district_name = c("A", "B"),
    households_total = c(100, 300), lighting_electricity = c(80, 120),
    lighting_kerosene = c(10, 150), lighting_solar = c(0, 0), lighting_other_oil = c(0, 0),
    lighting_other = c(0, 0), lighting_none = c(10, 30), stringsAsFactors = FALSE
  )
  hl11 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"), district_name = c("A", "B"),
    households_total = c(100, 300), electricity_available = c(80, 120),
    latrine_available = c(30, 90), stringsAsFactors = FALSE
  )
  hl12 <- data.frame(
    state_code = c("09", "09"), district_code = c("132", "133"), district_name = c("A", "B"),
    households_total = c(100, 300), banking = c(20, 180), radio = c(20, 60), television = c(20, 120),
    telephone = c(10, 150), bicycle = c(20, 120), motorcycle = c(10, 90), car = c(5, 30),
    computer = c(5, 60), computer_internet = c(2, 30), stringsAsFactors = FALSE
  )
  transition <- data.frame(
    state_code_2011 = c("09", "09"), district_code_2011 = c("132", "133"),
    state_code_2001 = c("09", "09"), district_code_2001 = c("01", "01"),
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "deterministic_containment", stringsAsFactors = FALSE
  )
  out <- build_census_2011_housing_measures(hl07, hl11, hl12, transition)
  expect_equal(nrow(out), 1L)
  expect_equal(out$households_total, 400)
  expect_equal(out$electricity_share_households, 0.5)
  expect_equal(out$banking_share_households, 0.5)
  expect_equal(out$census_2011_source_district_count, 2L)
})

test_that("Census housing changes compare harmonized 2011 shares with the matching 2001 parent", {
  common <- census_housing_common_share_columns()
  h01 <- data.frame(target_unit_2001 = "pc2001__09__01", stringsAsFactors = FALSE)
  h11 <- data.frame(
    target_unit_2001 = "pc2001__09__01", census_2011_source_district_count = 2L,
    census_2011_parent_reconstruction_complete = TRUE, households_total = 200,
    computer_share_households = 0.2, internet_computer_share_households = 0.1,
    stringsAsFactors = FALSE
  )
  for (variable in common) {
    h01[[variable]] <- 0.25
    h11[[variable]] <- 0.40
  }
  out <- build_census_housing_change_measures(h01, h11)
  expect_equal(out$electricity_share_households_change_2011_2001, 0.15)
  expect_equal(out$banking_share_households_change_2011_2001, 0.15)
})

test_that("Census 2001 H12 may be a validated district subset without truncating housing measures", {
  h09 <- data.frame(
    state_code = c("09", "09"), district_code = c("01", "02"), district_name = c("A", "B"),
    households_total = c(100, 200), lighting_electricity = c(60, 100),
    lighting_kerosene = c(30, 80), lighting_solar = c(2, 4), lighting_other_oil = c(1, 2),
    lighting_other = c(3, 6), lighting_none = c(4, 8), stringsAsFactors = FALSE
  )
  h12 <- data.frame(
    state_code = "09", district_code = "01", households_total = 100,
    electricity_available = 60, latrine_available = 25, stringsAsFactors = FALSE
  )
  h13 <- data.frame(
    state_code = c("09", "09"), district_code = c("01", "02"), households_total = c(100, 200),
    banking = c(40, 80), radio = c(20, 40), television = c(50, 100), telephone = c(10, 20),
    bicycle = c(60, 120), motorcycle = c(15, 30), car = c(5, 10), stringsAsFactors = FALSE
  )
  validation <- validate_census_housing_sources(h09, h12, h13, 2001L)
  expect_equal(validation$n_source_districts[validation$check == "household_total_lighting_vs_utility"], 1L)
  out <- build_census_2001_housing_measures(h09, h12, h13)
  expect_equal(nrow(out), 2L)
  expect_equal(out$latrine_share_households[out$district_code == "01"], 0.25)
  expect_true(is.na(out$latrine_share_households[out$district_code == "02"]))
})

test_that("Census housing mechanism registry uses full-support longitudinal changes", {
  registry <- census_housing_mechanism_registry()
  expect_equal(nrow(registry), 8L)
  expect_identical(anyDuplicated(registry$outcome_id), 0L)
  expect_identical(anyDuplicated(registry$variable), 0L)
  expect_true(all(registry$source_id == "change"))
  expect_true(all(registry$denominator == "households"))
  expect_true(all(grepl("_change_2011_2001$", registry$variable)))
  expect_false("latrine_share_households_change_2011_2001" %in% registry$variable)
  expect_false("kerosene_lighting_share_households_change_2011_2001" %in% registry$variable)
  expect_false("solar_lighting_share_households_change_2011_2001" %in% registry$variable)
})

test_that("Census housing change mechanisms reuse the common IV inference engine", {
  skip_if_not_installed("ivreg")
  skip_if_not_installed("lmtest")
  skip_if_not_installed("momentfit")
  skip_if_not_installed("sandwich")
  set.seed(733)
  n <- 120L
  state <- sprintf("%02d", rep(1:12, each = 10))
  district <- rep(sprintf("%02d", 1:10), 12)
  target <- paste0("pc2001__", state, "__", district)
  panel <- data.frame(
    state_code_2001 = state,
    district_code_2001 = district,
    region = rep(panel_region_levels(), length.out = n),
    stringsAsFactors = FALSE
  )
  for (variable in census_2001_diagnostic_controls()) panel[[variable]] <- stats::rnorm(n)
  for (variable in alternative_distance_variables()) panel[[variable]] <- stats::rnorm(n)
  panel$ling_mapped_speaker_share <- stats::runif(n, 0.8, 1)
  panel$ling_glottolog_mapped_speaker_share <- stats::runif(n, 0.8, 1)
  panel$ling_dyen_mapped_speaker_share <- stats::runif(n, 0.8, 1)
  treatment <- preferred_iv_variables()$treatment
  panel[[treatment]] <- 0.5 * panel$ling_distance_nonzero_mean + stats::rnorm(n)

  registry <- census_housing_mechanism_registry()
  housing_change <- data.frame(target_unit_2001 = target, stringsAsFactors = FALSE)
  for (variable in registry$variable) {
    housing_change[[variable]] <- stats::rnorm(n, sd = 0.08)
  }

  mechanism_panel <- prepare_census_housing_mechanism_panel(
    panel, housing_change, registry
  )
  expect_equal(nrow(mechanism_panel), n)
  expect_equal(attr(mechanism_panel, "n_harmonized_mechanism_districts"), n)

  diagnostics <- estimate_census_housing_mechanism_models(
    mechanism_panel, registry, cfg = list(), ar_points = 21L
  )
  expect_equal(nrow(diagnostics$first_stage), 6L)
  expect_equal(nrow(diagnostics$reduced_form), 6L * nrow(registry))
  expect_equal(nrow(diagnostics$weak_iv), 6L * nrow(registry))
  expect_true(all(diagnostics$first_stage$n == n))
  expect_true(all(diagnostics$reduced_form$n == n))
  expect_true(all(diagnostics$weak_iv$n == n))
  expect_true(all(diagnostics$reduced_form$status == "estimated"))
  expect_true(all(diagnostics$weak_iv$status == "estimated"))
  expect_true(all(is.finite(diagnostics$weak_iv$anderson_rubin_p_beta0)))
  expect_true(all(is.finite(
    diagnostics$weak_iv$anderson_rubin_p_beta0_holm_within_spec
  )))
  expect_true(nrow(diagnostics$anderson_rubin_grid) > 0L)
  expect_setequal(unique(diagnostics$anderson_rubin_grid$outcome_id), registry$outcome_id)
})
