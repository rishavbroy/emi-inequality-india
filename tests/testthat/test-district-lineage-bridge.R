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
    source_unit = c("pc2011__01__001", "pc2011__01__001"),
    target_2001 = admin$unit_id,
    weight = c(0.25, 0.75),
    basis = "population",
    reference_year = 2011,
    source_id = "shrug_pc_keys",
    status = "accepted",
    note = NA_character_,
    stringsAsFactors = FALSE
  )

  out <- read_adjudicated_allocation_weights(raw, admin)
  validation <- validate_adjudicated_allocation_weights(out)

  expect_equal(validation$weight_sum, 1)
  expect_true(validation$weights_well_formed)
  expect_true(validation$coverage_complete)

  raw$weight <- c(0.25, 0.70)
  incomplete <- validate_adjudicated_allocation_weights(
    read_adjudicated_allocation_weights(raw, admin)
  )
  expect_true(incomplete$weights_well_formed)
  expect_false(incomplete$coverage_complete)

  raw$target_2001[[1]] <- "unknown"
  expect_error(
    read_adjudicated_allocation_weights(raw, admin),
    "unknown 2001 units"
  )
})

test_that("accepted tracked allocations reject negative or missing weights", {
  admin <- data.frame(unit_id = "pc2001__01__01")
  raw <- data.frame(
    source_unit = "pc2011__01__001", target_2001 = admin$unit_id,
    weight = -0.1, basis = "population", reference_year = 2011,
    source_id = "shrug_pc_keys", status = "accepted", note = NA_character_,
    stringsAsFactors = FALSE
  )

  expect_error(
    read_adjudicated_allocation_weights(raw, admin),
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

test_that("allocation coverage rejects malformed nonempty validation tables", {
  expect_error(
    allocation_coverage_status(
      data.frame(coverage_complete = TRUE),
      data.frame()
    ),
    class = "lineage_allocation_validation_error"
  )
  expect_error(
    allocation_coverage_status(
      data.frame(source_key = "pc2011__01__001", coverage_complete = TRUE),
      data.frame(source_key = "pc2011__01__002")
    ),
    class = "lineage_allocation_validation_error"
  )
})

test_that("reviewed allocations resolve only their corresponding coverage gaps", {
  generated <- data.frame(
    source_key = c(
      "pc2011__01__001", "pc2011__01__002", "pc2011__01__003"
    ),
    coverage_complete = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_key = "pc2011__01__002",
    coverage_complete = TRUE,
    stringsAsFactors = FALSE
  )

  status <- allocation_coverage_status(generated, reviewed)

  expect_equal(status$n_generated_sources, 3L)
  expect_equal(status$n_reviewed_complete, 1L)
  expect_equal(status$n_unresolved, 1L)
  expect_false(status$coverage_resolved)
})

test_that("tracked allocation decisions preserve weights and rejections", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  path <- file.path(
    root, "data", "metadata", "district_allocation_weights.csv"
  )
  decisions <- read_adjudicated_allocation_weights(
    read_lineage_source(
      path,
      reader = "allocation_csv",
      source_id = "lineage_allocation_weights"
    )
  )
  accepted <- decisions[
    decisions$status %in% "accepted",
    ,
    drop = FALSE
  ]
  rejected <- decisions[
    decisions$status %in% "rejected",
    ,
    drop = FALSE
  ]
  validation <- validate_adjudicated_allocation_weights(decisions)
  decision_status <- allocation_decision_status(decisions)

  expect_setequal(
    unique(decisions$status),
    c("accepted", "rejected")
  )
  expect_equal(
    nrow(decision_status),
    length(unique(decisions$source_unit))
  )
  expect_true(all(decision_status$decision_complete))

  accepted_bases <- unique(accepted$basis)
  expect_true(all(nzchar(accepted_bases)))
  expect_true(all(
    accepted_bases %in% c(
      "population_renormalized_min_99pct_mapped",
      "canonical_registry_name_continuity",
      "official_single_parent_or_alias_continuity",
      "official_single_parent_pre_2001_parentage"
    )
  ))
  expect_true(
    "official_single_parent_pre_2001_parentage" %in% accepted_bases
  )
  deterministic_parentage <- accepted[
    accepted$basis %in% "official_single_parent_pre_2001_parentage",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(deterministic_parentage), 0L)
  expect_true(all(deterministic_parentage$weight == 1))
  expect_equal(
    length(unique(deterministic_parentage$source_unit)),
    nrow(deterministic_parentage)
  )
  expect_equal(
    nrow(validation),
    length(unique(accepted$source_unit))
  )
  expect_true(all(validation$coverage_complete))
  expect_true(all(validation$weights_well_formed))

  expect_gt(nrow(rejected), 0L)
  expect_true(all(is.na(rejected$target_2001) | !nzchar(rejected$target_2001)))
  expect_true(all(is.na(rejected$weight)))
  expect_identical(
    unique(rejected$basis),
    "multi_parent_allocation_unavailable"
  )
})

test_that("allocation source keys use canonical Census unit IDs", {
  expect_identical(
    canonical_allocation_source_key(
      c("01.001", "1.001", "pc2011__01__001")
    ),
    rep("pc2011__01__001", 3)
  )
  expect_error(
    canonical_allocation_source_key(c(1.001, 1.01)),
    "must be read as character"
  )
  expect_identical(
    allocation_source_key(c(1, 27), c(1, 518)),
    c("pc2011__01__001", "pc2011__27__518")
  )
})

test_that("reviewed allocation coverage matches canonicalized source keys", {
  generated <- data.frame(
    source_key = c("pc2011__01__001", "pc2011__01__010"),
    coverage_complete = FALSE,
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_key = c("01.001", "01.010"),
    coverage_complete = TRUE,
    stringsAsFactors = FALSE
  )

  status <- allocation_coverage_status(generated, reviewed)

  expect_equal(status$n_reviewed_complete, 2L)
  expect_equal(status$n_unresolved, 0L)
  expect_true(status$coverage_resolved)
})

test_that("duplicate reviewed rows cannot resolve another source gap", {
  generated <- data.frame(
    source_key = c("pc2011__01__001", "pc2011__01__002"),
    coverage_complete = FALSE,
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    source_key = rep("pc2011__01__002", 2),
    coverage_complete = TRUE,
    stringsAsFactors = FALSE
  )

  status <- allocation_coverage_status(generated, reviewed)

  expect_equal(status$n_reviewed_complete, 1L)
  expect_equal(status$n_unresolved, 1L)
  expect_false(status$coverage_resolved)
})

test_that("tracked accepted allocations resolve every reviewed source gap", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  weights <- read_lineage_source(
    file.path(
      root, "data", "metadata", "district_allocation_weights.csv"
    ),
    reader = "allocation_csv",
    source_id = "lineage_allocation_weights"
  )
  reviewed <- validate_adjudicated_allocation_weights(
    read_adjudicated_allocation_weights(weights)
  )

  decisions <- allocation_decision_status(
    read_adjudicated_allocation_weights(weights)
  )
  accepted_sources <- decisions$source_key[
    decisions$decision_status %in% "accepted"
  ]

  expect_setequal(reviewed$source_key, accepted_sources)
  expect_true(all(grepl(
    "^pc2011__[0-9]{2}__[0-9]{3}$",
    reviewed$source_key
  )))
})

test_that("allocation CSV reader preserves identifier columns as character", {
  skip_if_not_installed("data.table")

  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "source_unit,target_2001,weight,basis,reference_year,source_id,status,note",
    "01.010,pc2001__01__01,1,population,2011,shrug_pc_keys,accepted,"
  ), path)

  raw <- read_lineage_source(
    path,
    reader = "allocation_csv",
    source_id = "lineage_allocation_weights"
  )
  parsed <- read_adjudicated_allocation_weights(raw)

  expect_type(raw$source_unit, "character")
  expect_identical(raw$source_unit, "01.010")
  expect_identical(parsed$source_unit, "pc2011__01__010")
})

test_that("allocation decisions distinguish accepted weights from rejections", {
  decisions <- allocation_decision_status(data.frame(
    source_unit = c(
      "pc2011__01__001",
      "pc2011__01__001",
      "pc2011__01__002"
    ),
    status = c("accepted", "accepted", "rejected"),
    stringsAsFactors = FALSE
  ))

  expect_equal(nrow(decisions), 2L)
  expect_setequal(
    decisions$decision_status,
    c("accepted", "rejected")
  )
  expect_true(all(decisions$decision_complete))
})

test_that("rejected allocations cannot fabricate targets or weights", {
  raw <- data.frame(
    source_unit = "pc2011__01__001",
    target_2001 = "pc2001__01__01",
    weight = 1,
    status = "rejected",
    stringsAsFactors = FALSE
  )

  expect_error(
    read_adjudicated_allocation_weights(raw),
    "must not carry targets or weights"
  )
})

test_that("tracked allocation ledger completes every generated decision", {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  weights <- read_adjudicated_allocation_weights(
    read_lineage_source(
      file.path(
        root, "data", "metadata",
        "district_allocation_weights.csv"
      ),
      reader = "allocation_csv",
      source_id = "lineage_allocation_weights"
    )
  )
  decisions <- allocation_decision_status(weights)

  expect_setequal(
    decisions$decision_status,
    c("accepted", "rejected")
  )
  expect_equal(
    nrow(decisions),
    length(unique(weights$source_unit))
  )
  expect_true(all(decisions$decision_complete))
  expect_equal(
    sum(decisions$decision_status == "rejected"),
    length(unique(weights$source_unit[weights$status %in% "rejected"]))
  )
})

test_that("LGD Census-code bridge supplies official deterministic transitions", {
  lgd <- data.frame(
    state_lgd_code = c("35", "35", "12"),
    census2001_code = c("01", "", "09"),
    census2011_code = c("640", "639", "252"),
    stringsAsFactors = FALSE
  )

  out <- build_lgd_district_transition_2001_2011(lgd)

  expect_equal(nrow(out), 2L)
  expect_setequal(
    paste(out$state_code_2011, out$district_code_2011, sep = "__"),
    c("35__640", "12__252")
  )
  expect_true(all(out$mapping_class == "official_lgd_census_code_bridge"))
  expect_true(all(out$population_share_to_2001 == 1))
})

test_that("official LGD transitions override incomplete SHRUG transitions", {
  shrug <- data.frame(
    state_code_2011 = c("35", "35"),
    district_code_2011 = c("640", "641"),
    state_code_2001 = c("35", "35"),
    district_code_2001 = c("01", "02"),
    population_share_to_2001 = c(0.99, 1),
    area_share_to_2001 = c(0.99, 1),
    shrid_coverage = c(0.99, 1),
    mapping_class = c("non_nested_or_incomplete", "deterministic_containment"),
    stringsAsFactors = FALSE
  )
  lgd <- data.frame(
    state_code_2011 = "35",
    district_code_2011 = "640",
    state_code_2001 = "35",
    district_code_2001 = "01",
    population_share_to_2001 = 1,
    area_share_to_2001 = 1,
    shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    source_id = "lgd_mod_districts_2001_2011",
    stringsAsFactors = FALSE
  )

  out <- combine_district_transitions_2001_2011(shrug, lgd)
  key <- paste(out$state_code_2011, out$district_code_2011, sep = "__")

  expect_equal(sum(key == "35__640"), 1L)
  expect_equal(out$mapping_class[key == "35__640"], "official_lgd_census_code_bridge")
  expect_equal(sum(key == "35__641"), 1L)
})

test_that("reviewed single-parent ancestry can enter the preferred panel", {
  source_roster <- data.frame(
    source_row_id = "nss_2017_18__jharkhand__20122__simdega",
    source_key = "nss_2017_18__jharkhand__20122",
    wave = "nss_2017_18",
    source_code = "20122",
    raw_state = "Jharkhand",
    raw_district = "Simdega",
    state_std = "jharkhand",
    district_std = "simdega",
    stringsAsFactors = FALSE
  )
  source_matches <- data.frame(
    source_row_id = source_roster$source_row_id,
    unit_id = "pc2011__20__367",
    reference_vintage = "2011",
    method = "official_nss75_exact_name_deterministic_2011_to_2001",
    status = "accepted",
    stringsAsFactors = FALSE
  )
  admin_2001 <- data.frame(
    unit_id = "pc2001__20__16", state_code = "20",
    district_code = "16", stringsAsFactors = FALSE
  )
  admin_2011 <- data.frame(
    unit_id = "pc2011__20__367", state_code = "20",
    district_code = "367", stringsAsFactors = FALSE
  )
  weights <- data.frame(
    source_unit = "pc2011__20__367",
    target_2001 = "pc2001__20__16",
    weight = 1,
    basis = "official_single_parent_pre_2001_parentage",
    status = "accepted",
    stringsAsFactors = FALSE
  )

  out <- build_conservative_mapping_eligibility(
    source_roster, source_matches, data.frame(), admin_2001, admin_2011,
    allocation_weights = weights
  )

  expect_true(out$eligible_conservative)
  expect_identical(out$target_unit_2001, "pc2001__20__16")
  expect_identical(out$mapping_class, "deterministic_2011_to_2001")
})

test_that("reviewed ancestry does not replace a valid LGD Census-code transition", {
  admin_2001 <- data.frame(
    unit_id = c("pc2001__35__01", "pc2001__35__02"),
    stringsAsFactors = FALSE
  )
  lgd <- data.frame(
    state_code_2011 = "35", district_code_2011 = "640",
    state_code_2001 = "35", district_code_2001 = "01",
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    source_id = "lgd",
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    state_code_2011 = "35", district_code_2011 = "640",
    state_code_2001 = "35", district_code_2001 = "02",
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "reviewed_single_parent_ancestry",
    source_id = "reviewed",
    stringsAsFactors = FALSE
  )

  out <- combine_district_transitions_2001_2011(
    data.frame(), lgd, reviewed, admin_2001 = admin_2001
  )

  expect_equal(nrow(out), 1L)
  expect_identical(transition_target_unit_2001(out), "pc2001__35__01")
  expect_identical(out$mapping_class, "official_lgd_census_code_bridge")
})

test_that("reviewed one-parent ancestry overrides phantom LGD Census-2001 targets", {
  admin_2001 <- data.frame(
    unit_id = c("pc2001__12__10", "pc2001__20__16"),
    state_code = c("12", "20"), district_code = c("10", "16"),
    stringsAsFactors = FALSE
  )
  admin_2011 <- data.frame(
    unit_id = c("pc2011__12__258", "pc2011__20__367"),
    state_code = c("12", "20"), district_code = c("258", "367"),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    from_unit = c("pc2001__12__10", "pc2001__20__16"),
    to_unit = c("pc2011__12__258", "pc2011__20__367"),
    source_id = c("lower_dibang_history", "simdega_history"),
    status = "accepted",
    stringsAsFactors = FALSE
  )
  reviewed <- build_reviewed_ancestry_transition_2001_2011(
    events, admin_2001, admin_2011
  )
  lgd <- data.frame(
    state_code_2011 = c("12", "20"),
    district_code_2011 = c("258", "367"),
    state_code_2001 = c("12", "20"),
    district_code_2001 = c("15", "21"),
    population_share_to_2001 = 1,
    area_share_to_2001 = 1,
    shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    source_id = "lgd",
    stringsAsFactors = FALSE
  )

  out <- combine_district_transitions_2001_2011(
    data.frame(), lgd, reviewed, admin_2001 = admin_2001
  )

  expect_equal(nrow(out), 2L)
  expect_setequal(
    transition_target_unit_2001(out),
    c("pc2001__12__10", "pc2001__20__16")
  )
  expect_true(all(out$mapping_class == "reviewed_single_parent_ancestry"))
  expect_silent(validate_district_transition_targets(out, admin_2001))
})

test_that("district transition rejects targets absent from the Census-2001 registry", {
  transition <- data.frame(
    state_code_2011 = "12", district_code_2011 = "258",
    state_code_2001 = "12", district_code_2001 = "15",
    population_share_to_2001 = 1, area_share_to_2001 = 1, shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    stringsAsFactors = FALSE
  )
  admin_2001 <- data.frame(
    unit_id = "pc2001__12__10",
    stringsAsFactors = FALSE
  )

  expect_error(
    validate_district_transition_targets(transition, admin_2001),
    "unknown Census-2001 target units"
  )
})
