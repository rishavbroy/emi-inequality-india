test_that("SHRUG locality keys recognize published population and area fields", {
  raw <- data.frame(
    shrid2 = "x",
    pc11_state_id = 1,
    pc11_district_id = 2,
    pc11_subdistrict_id = 3,
    pc11_village_id = 4,
    pc11_land_area = 2.5,
    pc11_pca_tot_p = 100
  )

  out <- standardize_shrug_locality_key(raw, 2011L, "rural")

  expect_equal(out$population, 100)
  expect_equal(out$area, 2.5)
  expect_equal(out$district_code, "002")
})

test_that("administrative code padding handles spreadsheet numeric text", {
  expect_equal(pad_admin_code(c("1", "01", "1.0", NA), 2L), c("01", "01", "01", NA))
})

test_that("SHRUG code widths follow their Census vintages", {
  pc01 <- data.frame(
    shrid2 = "x", pc01_state_id = 3, pc01_district_id = 1,
    pc01_subdistrict_id = 2, pc01_village_id = 82,
    pc01_land_area = 1, pc01_pca_tot_p = 10
  )

  locality <- standardize_shrug_locality_key(pc01, 2001L, "rural")
  district <- standardize_shrug_district_key(pc01, 2001L)

  expect_equal(locality$district_code, "01")
  expect_equal(locality$subdistrict_code, "0002")
  expect_equal(district$district_code, "01")
})

test_that("SHRUG bridge exposes incomplete coverage instead of renormalizing it", {
  locality <- function(shrid, district, population) {
    data.frame(
      shrid2 = shrid,
      pc11_state_id = "01",
      pc11_district_id = district,
      pc11_subdistrict_id = "00001",
      pc11_village_id = paste0("v", shrid),
      pc11_pca_tot_p = population,
      pc11_land_area = population / 10,
      stringsAsFactors = FALSE
    )
  }
  locality01 <- function(shrid, district) {
    data.frame(
      shrid2 = shrid,
      pc01_state_id = "01",
      pc01_district_id = district,
      pc01_subdistrict_id = "0001",
      pc01_village_id = paste0("v", shrid),
      pc01_pca_tot_p = NA_real_,
      pc01_land_area = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  pc11r <- safe_bind_rows(list(
    locality("a", "010", 60),
    locality("b", "010", 40),
    locality("c", "011", 20)
  ))
  pc01r <- safe_bind_rows(list(
    locality01("a", "001"),
    locality01("b", "001"),
    locality01("c", "002")
  ))
  empty01 <- pc01r[0, , drop = FALSE]
  empty11 <- pc11r[0, , drop = FALSE]
  d01 <- data.frame(
    shrid2 = c("a", "b", "b", "c"),
    pc01_state_id = "01",
    pc01_district_id = c("001", "001", "002", "002")
  )
  d11 <- data.frame(
    shrid2 = c("a", "b", "c"),
    pc11_state_id = "01",
    pc11_district_id = c("010", "010", "011")
  )

  bridge <- build_shrug_district_bridge(pc01r, empty01, pc11r, empty11, d01, d11)
  transition <- build_district_transition_2001_2011(bridge)
  incomplete <- transition[transition$district_code_2011 == "010", , drop = FALSE]
  complete <- transition[transition$district_code_2011 == "011", , drop = FALSE]

  expect_equal(bridge$bridge_status[bridge$shrid2 == "b"], "crosses_district_boundary")
  expect_equal(incomplete$population_share_to_2001, 0.6)
  expect_equal(incomplete$shrid_coverage, 0.5)
  expect_equal(incomplete$mapping_class, "non_nested_or_incomplete")
  expect_equal(complete$population_share_to_2001, 1)
  expect_equal(complete$mapping_class, "deterministic_containment")
})

test_that("allocation validation rejects incomplete or negative source weights", {
  weights <- data.frame(
    state_code_2011 = c("01", "01"),
    district_code_2011 = c("010", "010"),
    population_share_to_2001 = c(0.4, 0.5)
  )
  incomplete <- validate_allocation_weights(weights)
  weights$population_share_to_2001 <- c(1.1, -0.1)
  negative <- validate_allocation_weights(weights)

  expect_true(incomplete$weights_well_formed)
  expect_false(incomplete$coverage_complete)
  expect_equal(incomplete$weight_sum, 0.9)
  expect_equal(incomplete$unmapped_share, 0.1)
  expect_false(negative$weights_well_formed)
  expect_false(negative$coverage_complete)
  expect_equal(negative$n_negative_weights, 1L)
})

test_that("tracked allocation weights require known targets and sum to one", {
  admin <- data.frame(unit_id = c("pc2001__01__01", "pc2001__01__02"))
  raw <- data.frame(
    source_unit = c("later", "later"),
    target_2001 = admin$unit_id,
    weight = c(0.25, 0.75),
    basis = "population",
    reference_year = 2011,
    source_id = "shrug_pc_keys",
    status = "accepted",
    note = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- read_adjudicated_allocation_weights_v2(raw, admin)
  validation <- validate_adjudicated_allocation_weights_v2(out)

  expect_equal(validation$weight_sum, 1)
  expect_true(validation$weights_well_formed)
  expect_true(validation$coverage_complete)

  raw$weight <- c(0.25, 0.70)
  incomplete <- validate_adjudicated_allocation_weights_v2(
    read_adjudicated_allocation_weights_v2(raw, admin)
  )
  expect_true(incomplete$weights_well_formed)
  expect_false(incomplete$coverage_complete)

  raw$target_2001[[1]] <- "unknown"
  expect_error(
    read_adjudicated_allocation_weights_v2(raw, admin),
    "unknown 2001 units"
  )
})

test_that("accepted tracked allocations reject negative or missing weights", {
  admin <- data.frame(unit_id = "pc2001__01__01")
  raw <- data.frame(
    source_unit = "later", target_2001 = admin$unit_id,
    weight = -0.1, basis = "population", reference_year = 2011,
    source_id = "shrug_pc_keys", status = "accepted", note = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_error(
    read_adjudicated_allocation_weights_v2(raw, admin),
    "nonnegative finite weight"
  )
})

test_that("SHRUG bridge requires locality keys in both Census vintages", {
  pc01 <- data.frame(
    shrid2 = "a", pc01_state_id = "01", pc01_district_id = "01",
    pc01_subdistrict_id = "0001", pc01_village_id = "000001",
    pc01_pca_tot_p = 10, pc01_land_area = 1
  )
  pc11 <- data.frame(
    shrid2 = c("a", "b"), pc11_state_id = "01", pc11_district_id = "010",
    pc11_subdistrict_id = "00001", pc11_village_id = c("000001", "000002"),
    pc11_pca_tot_p = c(10, 20), pc11_land_area = c(1, 2)
  )
  d01 <- data.frame(
    shrid2 = c("a", "b"), pc01_state_id = "01", pc01_district_id = "01"
  )
  d11 <- data.frame(
    shrid2 = c("a", "b"), pc11_state_id = "01", pc11_district_id = "010"
  )

  bridge <- build_shrug_district_bridge(
    pc01, pc01[0, ], pc11, pc11[0, ], d01, d11
  )

  expect_equal(
    bridge$bridge_status[bridge$shrid2 == "b"],
    "missing_census_locality_key"
  )
  expect_false(bridge$deterministic[bridge$shrid2 == "b"])
})


test_that("SHRUG district membership requires a unique state as well as district", {
  key <- data.frame(
    shrid2 = c("missing_state", "ambiguous_state", "ambiguous_state"),
    census_year = 2011L,
    state_code = c(NA, "01", "02"),
    district_code = c("010", "010", "010"),
    stringsAsFactors = FALSE
  )

  out <- unique_shrid_district_membership(key, "2011")

  expect_false(out$deterministic[out$shrid2 == "missing_state"])
  expect_false(out$deterministic[out$shrid2 == "ambiguous_state"])
  expect_equal(out$n_state_memberships[out$shrid2 == "ambiguous_state"], 2L)
})

test_that("SHRUG bridge distinguishes ambiguous from absent district membership", {
  locality <- data.frame(
    shrid2 = c("ambiguous", "missing"),
    pc01_state_id = "01", pc01_district_id = "01",
    pc01_subdistrict_id = "0001", pc01_village_id = c("1", "2"),
    pc01_pca_tot_p = c(10, 20), pc01_land_area = c(1, 2)
  )
  locality11 <- data.frame(
    shrid2 = c("ambiguous", "missing"),
    pc11_state_id = "01", pc11_district_id = "001",
    pc11_subdistrict_id = "00001", pc11_village_id = c("1", "2"),
    pc11_pca_tot_p = c(10, 20), pc11_land_area = c(1, 2)
  )
  d01 <- data.frame(
    shrid2 = c("ambiguous", "ambiguous", "missing"),
    pc01_state_id = "01",
    pc01_district_id = c("01", "02", NA_character_)
  )
  d11 <- data.frame(
    shrid2 = c("ambiguous", "missing"),
    pc11_state_id = "01", pc11_district_id = "001"
  )

  bridge <- build_shrug_district_bridge(
    locality, locality[0, ], locality11, locality11[0, ], d01, d11
  )

  expect_equal(
    bridge$bridge_status[bridge$shrid2 == "ambiguous"],
    "crosses_district_boundary"
  )
  expect_equal(
    bridge$bridge_status[bridge$shrid2 == "missing"],
    "missing_census_membership"
  )
  expect_equal(
    bridge$n_district_memberships_2001[bridge$shrid2 == "ambiguous"],
    2L
  )
})


test_that("Census 2001 registry uses vintage state names and district labels", {
  census <- data.frame(
    state_code = c("06", "25", "26"),
    district_code = c("01", "01", "01"),
    district_name = c("Panchkula", "Daman", "Dadra & Nagar Haveli"),
    state_std = c("6", "25", "26"),
    district_std = c("1", "1", "1"),
    stringsAsFactors = FALSE
  )

  out <- build_admin_registry_2001(census)

  expect_equal(
    out$state_std,
    c("haryana", "daman and diu", "dadra and nagar haveli")
  )
  expect_equal(
    out$district_std,
    c("panchkula", "daman", "dadra and nagar haveli")
  )
  expect_equal(
    out$unit_id,
    c("pc2001__06__01", "pc2001__25__01", "pc2001__26__01")
  )
})

test_that("Census 2001 registry rejects unknown state codes", {
  census <- data.frame(
    state_code = "99", district_code = "01", district_name = "Example",
    stringsAsFactors = FALSE
  )

  expect_error(
    build_admin_registry_2001(census),
    "Unknown Census 2001 state codes"
  )
})

