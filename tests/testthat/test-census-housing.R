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


test_that("HL13 structure categories exhaust households and temporary structure", {
  raw <- data.frame(matrix("", nrow = 1, ncol = 14), stringsAsFactors = FALSE)
  raw[1, 1:7] <- c("HH4313", "09", "132", "00000", "000000", "District - Alpha", "Total")
  raw[1, 8:14] <- c(100, 60, 20, 15, 10, 5, 5)
  out <- parse_census_hl13_2011_sheet(raw)
  expect_equal(out$structure_permanent, 60)
  expect_equal(out$structure_temporary, 15)
  expect_equal(out$structure_temporary_nonserviceable, 5)

  bad_total <- raw
  bad_total[1, 14] <- 4
  expect_error(parse_census_hl13_2011_sheet(bad_total), "structure")

  bad_temporary <- raw
  bad_temporary[1, 13] <- 4
  expect_error(parse_census_hl13_2011_sheet(bad_temporary), "temporary structure")
})


test_that("Census 2001 H04 Appendix remains acquisition-only until source inspection", {
  expect_error(
    census_housing_manifest_files(character(), "H04A", census_year = 2001L),
    "Census 2001 housing reader supports"
  )
})

test_that("HL13 shares a strict household universe with the active 2011 housing sources", {
  hl07 <- data.frame(
    state_code = "09", district_code = "132", district_name = "Alpha",
    households_total = 100, lighting_electricity = 70,
    stringsAsFactors = FALSE
  )
  hl11 <- data.frame(
    state_code = "09", district_code = "132", households_total = 100,
    electricity_available = 70, latrine_available = 60,
    stringsAsFactors = FALSE
  )
  hl12 <- data.frame(
    state_code = "09", district_code = "132", households_total = 100,
    stringsAsFactors = FALSE
  )
  hl13 <- data.frame(
    state_code = "09", district_code = "132", households_total = 100,
    stringsAsFactors = FALSE
  )
  validation <- validate_census_housing_sources(
    hl07, hl11, hl12, 2011L, hl13 = hl13
  )
  structural <- validation[validation$check == "household_total_lighting_vs_structure", , drop = FALSE]
  expect_equal(structural$n_overlap_districts, 1L)
  expect_equal(structural$max_abs_difference, 0)

  bad <- hl13
  bad$households_total <- 99
  expect_error(
    validate_census_housing_sources(hl07, hl11, hl12, 2011L, hl13 = bad),
    "household-universe counts disagree"
  )
})

test_that("follow-up housing shares use the common household denominator", {
  x <- data.frame(
    households_total = 200,
    households_owned = 120,
    households_rented = 60,
    computer = 40,
    computer_internet = 10,
    structure_permanent = 130,
    structure_semi_permanent = 40,
    structure_temporary = 20,
    structure_temporary_nonserviceable = 8,
    stringsAsFactors = FALSE
  )
  out <- add_census_housing_followup_shares(x)
  expect_equal(out$permanent_structure_share_households, 130 / 200)
  expect_equal(out$semi_permanent_structure_share_households, 40 / 200)
  expect_equal(out$temporary_structure_share_households, 20 / 200)
  expect_equal(out$nonserviceable_temporary_structure_share_households, 8 / 200)
  expect_equal(out$owned_share_households, 120 / 200)
})

test_that("Census housing mechanism registry remains a fixed predeclared longitudinal family", {
  registry <- census_housing_mechanism_registry()
  expect_equal(nrow(registry), 8L)
  expect_identical(anyDuplicated(registry$outcome_id), 0L)
  expect_identical(anyDuplicated(registry$variable), 0L)
  expect_true(all(registry$source_id == "change"))
  expect_true(all(registry$denominator == "households"))
  expect_true(all(grepl("_change_2011_2001$", registry$variable)))
  expect_false("kerosene_lighting_share_households_change_2011_2001" %in% registry$variable)
  expect_false("solar_lighting_share_households_change_2011_2001" %in% registry$variable)
  expect_false("latrine_share_households_change_2011_2001" %in% registry$variable)
  expect_false("bathroom_share_households_change_2011_2001" %in% registry$variable)
  expect_false("clean_cooking_fuel_share_households_change_2011_2001" %in% registry$variable)
  expect_false("permanent_structure_share_households" %in% registry$variable)
  expect_false("temporary_structure_share_households" %in% registry$variable)
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


test_that("H05 room-size accounting yields a conservative greater-than-two-person crowding lower bound", {
  sizes <- c("All Households", "1", "2", "3", "4", "5", "6-8", "9+")
  rows <- data.frame(
    state_code = "09", district_code = "01", district_name = "Alpha",
    household_size = sizes,
    households_total = c(70, rep(10, 7)),
    rooms_none = 0,
    rooms_one = c(25, 0, 0, 5, 5, 5, 5, 5),
    rooms_two = c(20, 10, 0, 0, 0, 5, 5, 0),
    rooms_three = c(10, 0, 10, 0, 0, 0, 0, 0),
    rooms_four = c(5, 0, 0, 5, 0, 0, 0, 0),
    rooms_five = c(5, 0, 0, 0, 5, 0, 0, 0),
    rooms_six_plus = c(5, 0, 0, 0, 0, 0, 0, 5),
    stringsAsFactors = FALSE
  )
  out <- summarise_census_room_rows(rows, "toy H05")
  expected <- sum(rows$rooms_one[rows$household_size %in% c("3", "4", "5", "6-8", "9+")]) +
    sum(rows$rooms_two[rows$household_size %in% c("5", "6-8", "9+")]) +
    sum(rows$rooms_three[rows$household_size == "9+"]) +
    sum(rows$rooms_four[rows$household_size == "9+"])
  expect_equal(out$households_total, 70)
  expect_equal(out$rooms_one, 25)
  expect_equal(out$overcrowding_gt2_ppr_lower_bound, expected)
})

test_that("HL04 ownership is follow-up-only and must exhaust the all-household room distribution", {
  sizes <- c("All Households", "1", "2", "3", "4", "5", "6-8", "9+")
  total_rows <- data.frame(
    state_code = "09", district_code = "132", district_name = "Alpha",
    ownership = "Total", household_size = sizes,
    households_total = c(70, rep(10, 7)),
    rooms_none = 0, rooms_one = c(70, rep(10, 7)), rooms_two = 0, rooms_three = 0,
    rooms_four = 0, rooms_five = 0, rooms_six_plus = 0, stringsAsFactors = FALSE
  )
  owner_rows <- data.frame(
    state_code = "09", district_code = "132", district_name = "Alpha",
    ownership = c("Owned", "Rented", "Any Other"), household_size = "All Households",
    households_total = c(50, 15, 5), rooms_none = 0, rooms_one = c(50, 15, 5),
    rooms_two = 0, rooms_three = 0, rooms_four = 0, rooms_five = 0, rooms_six_plus = 0,
    stringsAsFactors = FALSE
  )
  out <- summarise_census_room_rows(rbind(total_rows, owner_rows), "toy HL04", "ownership")
  expect_equal(out$households_owned, 50)
  expect_equal(out$households_rented, 15)

  bad <- rbind(total_rows, owner_rows)
  bad$households_total[bad$ownership == "Rented"] <- 14
  expect_error(summarise_census_room_rows(bad, "toy HL04", "ownership"), "ownership categories")
})

test_that("H08 and HL06 water contracts preserve only genuinely comparable broad source groups", {
  rows <- data.frame(
    state_code = "09", district_code = "01", district_name = "Alpha",
    water_location_group = c("Total", "Within", "Near", "Away"),
    households_total = c(100, 40, 35, 25),
    water_tap = c(40, 20, 15, 5),
    water_well = c(10, 5, 3, 2),
    water_handpump = c(20, 5, 10, 5),
    water_tubewell = c(10, 4, 3, 3),
    water_spring = c(5, 2, 2, 1),
    water_surface = c(10, 3, 1, 6),
    water_other = c(5, 1, 1, 3),
    stringsAsFactors = FALSE
  )
  out <- summarise_census_water_rows(rows, "toy water")
  expect_equal(out$water_handpump_tubewell, 30)
  expect_equal(out$water_within_premises, 40)
  expect_equal(out$water_away, 25)

  bad <- rows
  bad$water_tap[bad$water_location_group == "Away"] <- 4
  expect_error(summarise_census_water_rows(bad, "toy water"), "do not exhaust")
})

test_that("H05 may be a validated 2001 subset without truncating water or existing housing support", {
  reference <- data.frame(
    state_code = c("04", "09"), district_code = c("01", "01"),
    households_total = c(20, 100), stringsAsFactors = FALSE
  )
  rooms <- data.frame(
    state_code = "09", district_code = "01", households_total = 100,
    stringsAsFactors = FALSE
  )
  validation <- census_housing_validation_row(
    reference, rooms, "households_total", "households_total",
    "toy H05 subset", "household_total_lighting_vs_rooms", TRUE
  )
  expect_equal(validation$n_reference_districts, 2L)
  expect_equal(validation$n_source_districts, 1L)
  expect_equal(validation$n_overlap_districts, 1L)
})

test_that("new housing space and water shares are ratios of pooled counts", {
  x <- data.frame(
    households_total = 400,
    rooms_no_exclusive = 20, rooms_one = 160, rooms_two = 100,
    overcrowding_gt2_ppr_lower_bound = 80,
    water_tap = 200, water_well = 40, water_handpump_tubewell = 80,
    water_surface = 40, water_within_premises = 240, water_away = 60,
    lighting_electricity = 200, lighting_kerosene = 100, lighting_solar = 0,
    lighting_other_oil = 0, lighting_other = 0, lighting_none = 100,
    latrine_available = 120, banking = 200, radio = 80, television = 120,
    telephone = 160, bicycle = 140, motorcycle = 100, car = 40,
    stringsAsFactors = FALSE
  )
  out <- add_census_housing_shares(x)
  expect_equal(out$one_room_share_households, 0.4)
  expect_equal(out$overcrowding_gt2_ppr_lower_bound_share_households, 0.2)
  expect_equal(out$tap_water_share_households, 0.5)
  expect_equal(out$water_within_premises_share_households, 0.6)
  expect_equal(out$water_away_share_households, 0.15)
})

test_that("H10 and HL08 enforce exhaustive latrine accounting", {
  h10 <- data.frame(matrix("", nrow = 1, ncol = 15), stringsAsFactors = FALSE)
  h10[1, 1:6] <- c("H4310", "09", "01", "0000", "District - Alpha 01", "Total")
  h10[1, 7:15] <- c(100, 40, 20, 30, 10, 40, 25, 35, 40)
  out01 <- parse_census_h10_2001_sheet(h10)
  expect_equal(out01$latrine_available, 60)
  expect_equal(out01$latrine_flush_or_water_closet, 30)
  expect_equal(out01$drainage_none, 40)

  hl08 <- data.frame(matrix("", nrow = 1, ncol = 20), stringsAsFactors = FALSE)
  hl08[1, 1:7] <- c("HH2808", "09", "132", "00000", "000000", "District - Alpha", "Total")
  hl08[1, 8:20] <- c(100, 60, 10, 20, 5, 10, 5, 4, 3, 3, 40, 8, 32)
  out11 <- parse_census_hl08_2011_sheet(hl08)
  expect_equal(out11$latrine_available, 60)
  expect_equal(out11$latrine_flush_or_water_closet, 35)
  expect_equal(out11$latrine_pit, 15)

  bad <- hl08
  bad[1, 20] <- 31
  expect_error(parse_census_hl08_2011_sheet(bad), "no-latrine alternatives")
})

test_that("H10 and HL09 drainage partitions are independently exhaustive", {
  hl09 <- data.frame(matrix("", nrow = 1, ncol = 14), stringsAsFactors = FALSE)
  hl09[1, 1:7] <- c("HH3109", "09", "132", "00000", "000000", "District - Alpha", "Total")
  hl09[1, 8:14] <- c(100, 50, 20, 30, 25, 35, 40)
  out <- parse_census_hl09_2011_sheet(hl09)
  expect_equal(out$bathroom_available, 50)
  expect_equal(out$drainage_closed, 25)
  expect_equal(out$drainage_none, 40)

  bad <- hl09
  bad[1, 14] <- 39
  expect_error(parse_census_hl09_2011_sheet(bad), "drainage categories")
})

test_that("H11 and HL10 kitchen hierarchies exhaust household and fuel totals", {
  fuels <- census_cooking_fuel_columns()
  mk <- function(status, households, firewood, kerosene, lpg, no_cooking) {
    out <- data.frame(
      row_order = seq_along(status),
      state_code = "09", district_code = "01", district_name = "Alpha",
      kitchen_status = status, households_total = households,
      stringsAsFactors = FALSE
    )
    for (fuel in fuels) out[[fuel]] <- 0
    out$fuel_firewood <- firewood
    out$fuel_kerosene <- kerosene
    out$fuel_lpg_png <- lpg
    out$fuel_no_cooking <- no_cooking
    out
  }

  rows01 <- mk(
    c("Total", "Available", "Not available", "Cooking in Open", "No Cooking"),
    c(100, 68, 20, 10, 2),
    c(75, 50, 15, 10, 0),
    c(5, 0, 5, 0, 0),
    c(18, 18, 0, 0, 0),
    c(2, 0, 0, 0, 2)
  )
  out01 <- summarise_census_kitchen_fuel_rows(rows01, "toy H11", 2001L)
  expect_equal(out01$separate_kitchen_within_house, 68)
  expect_equal(out01$cooking_solid_fuel, 75)
  expect_equal(out01$cooking_clean_fuel, 18)

  rows11 <- mk(
    c(
      "Total", "Cooking inside house:", "Has Kitchen", "Does not have kitchen",
      "Cooking outside house:", "Has Kitchen", "Does not have kitchen", "No Cooking"
    ),
    c(100, 70, 45, 25, 28, 18, 10, 2),
    c(70, 47, 27, 20, 23, 15, 8, 0),
    c(5, 5, 3, 2, 0, 0, 0, 0),
    c(23, 18, 15, 3, 5, 3, 2, 0),
    c(2, 0, 0, 0, 0, 0, 0, 2)
  )
  out11 <- summarise_census_kitchen_fuel_rows(rows11, "toy HL10", 2011L)
  expect_equal(out11$separate_kitchen_within_house, 45)
  expect_equal(out11$cooking_outside_house, 28)

  bad <- rows11
  bad$fuel_firewood[which(bad$kitchen_status == "Does not have kitchen")[[1L]]] <- 19
  expect_error(summarise_census_kitchen_fuel_rows(bad, "toy HL10", 2011L))
})

test_that("dedicated sanitation tables replace incomplete utility latrine counts without weakening validation", {
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
  h10 <- data.frame(
    state_code = c("09", "09"), district_code = c("01", "02"), households_total = c(100, 200),
    bathroom_available = c(40, 80), latrine_available = c(25, 80),
    latrine_flush_or_water_closet = c(15, 40), latrine_pit = c(5, 20),
    drainage_closed = c(20, 40), drainage_none = c(50, 100), stringsAsFactors = FALSE
  )
  validation <- validate_census_housing_sources(
    h09, h12, h13, 2001L, h10_or_hl08 = h10
  )
  expect_equal(
    validation$n_source_districts[validation$check == "latrine_dedicated_vs_utility"], 1L
  )
  bad_h10 <- h10
  bad_h10$latrine_available[[1L]] <- 26
  expect_error(
    validate_census_housing_sources(h09, h12, h13, 2001L, h10_or_hl08 = bad_h10),
    "latrine counts disagree"
  )
  out <- build_census_2001_housing_measures(h09, h12, h13, h10 = h10)
  expect_equal(out$latrine_share_households[out$district_code == "02"], 0.4)
})

test_that("sanitation drainage and cooking shares use the common household denominator", {
  x <- data.frame(
    households_total = 400,
    bathroom_available = 240, latrine_flush_or_water_closet = 160, latrine_pit = 80,
    drainage_closed = 120, drainage_none = 200, separate_kitchen_within_house = 280,
    cooking_solid_fuel = 240, cooking_clean_fuel = 120,
    lighting_electricity = 200, lighting_kerosene = 100, lighting_solar = 0,
    lighting_other_oil = 0, lighting_other = 0, lighting_none = 100,
    latrine_available = 280, banking = 200, radio = 80, television = 120,
    telephone = 160, bicycle = 140, motorcycle = 100, car = 40,
    stringsAsFactors = FALSE
  )
  out <- add_census_housing_shares(x)
  expect_equal(out$bathroom_share_households, 0.6)
  expect_equal(out$flush_or_water_closet_latrine_share_households, 0.4)
  expect_equal(out$closed_drainage_share_households, 0.3)
  expect_equal(out$separate_kitchen_within_house_share_households, 0.7)
  expect_equal(out$clean_cooking_fuel_share_households, 0.3)
})
