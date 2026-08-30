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

test_that("reviewed ancestry fills sources without an LGD bridge before SHRUG", {
  admin_2001 <- data.frame(
    unit_id = "pc2001__28__01",
    stringsAsFactors = FALSE
  )
  shrug <- data.frame(
    state_code_2011 = "28", district_code_2011 = "532",
    state_code_2001 = "28", district_code_2001 = "01",
    population_share_to_2001 = 0.9967,
    area_share_to_2001 = 0.9874,
    shrid_coverage = 0.9863,
    mapping_class = "non_nested_or_incomplete",
    source_id = NA_character_,
    stringsAsFactors = FALSE
  )
  reviewed <- data.frame(
    state_code_2011 = "28", district_code_2011 = "532",
    state_code_2001 = "28", district_code_2001 = "01",
    population_share_to_2001 = 1,
    area_share_to_2001 = 1,
    shrid_coverage = 1,
    mapping_class = "reviewed_single_parent_ancestry",
    source_id = "census2011_andhra_admin_atlas",
    stringsAsFactors = FALSE
  )

  out <- combine_district_transitions_2001_2011(
    shrug, data.frame(), reviewed, admin_2001 = admin_2001
  )

  expect_equal(nrow(out), 1L)
  expect_identical(out$mapping_class, "reviewed_single_parent_ancestry")
  expect_identical(transition_target_unit_2001(out), "pc2001__28__01")
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

test_that("SHRUG Census code widths include the published 1991 identifiers", {
  pc91 <- data.frame(
    shrid2 = "x", pc91_state_id = 2, pc91_district_id = 1,
    pc91_subdistrict_id = 10, pc91_village_id = 335,
    pc91_land_area = 0.53, pc91_pca_tot_p = 368
  )

  locality <- standardize_shrug_locality_key(pc91, 1991L, "rural")
  district <- standardize_shrug_district_key(pc91, 1991L)

  expect_equal(locality$district_code, "01")
  expect_equal(locality$subdistrict_code, "0010")
  expect_equal(district$district_code, "01")
  expect_error(shrug_census_code_widths(1981L), "Unsupported SHRUG Population Census year")
})

test_that("historical SHRUG transition weights use source-year population", {
  pc91 <- data.frame(
    shrid2 = c("a", "b"), pc91_state_id = "02", pc91_district_id = "01",
    pc91_subdistrict_id = "0010", pc91_village_id = c("1", "2"),
    pc91_pca_tot_p = c(90, 10), pc91_land_area = c(9, 1),
    stringsAsFactors = FALSE
  )
  pc01 <- data.frame(
    shrid2 = c("a", "b"), pc01_state_id = "02", pc01_district_id = c("01", "02"),
    pc01_subdistrict_id = "0001", pc01_village_id = c("1", "2"),
    pc01_pca_tot_p = c(1, 999), pc01_land_area = c(1, 999),
    stringsAsFactors = FALSE
  )
  d91 <- pc91[c("shrid2", "pc91_state_id", "pc91_district_id")]
  d01 <- pc01[c("shrid2", "pc01_state_id", "pc01_district_id")]

  bridge <- build_shrug_district_bridge_1991_2001(
    pc91, pc91[0, ], pc01, pc01[0, ], d91, d01
  )
  transition <- build_district_transition_1991_2001(bridge)
  transition <- transition[order(transition$district_code_2001), ]

  expect_equal(transition$population_share_to_2001, c(0.9, 0.1))
  expect_equal(unique(transition$population_1991_total), 100)
  expect_equal(unique(transition$area_1991_total), 10)
  expect_true(all(transition$mapping_class == "non_nested_or_incomplete"))
  expect_equal(attr(bridge, "source_year"), 1991L)
  expect_equal(attr(bridge, "target_year"), 2001L)
})

test_that("source-district mapping summary centralizes coverage semantics", {
  bridge <- data.frame(
    shrid2 = c("a", "b", "c", "d"),
    state_code_1991 = "02",
    district_code_1991 = c("01", "01", "02", "02"),
    state_code_2001 = "02",
    district_code_2001 = c("01", "02", "03", NA),
    deterministic = c(TRUE, TRUE, TRUE, FALSE),
    population = c(90, 10, 99, 1),
    stringsAsFactors = FALSE
  )

  out <- summarize_shrug_source_district_mapping(
    bridge, 1991L, 2001L, min_population_coverage = 0.99
  )
  split <- out[out$district_code_1991 == "01", ]
  high <- out[out$district_code_1991 == "02", ]

  expect_equal(split$mapping_class, "splits_across_target_districts")
  expect_false(split$preferred_single_target)
  expect_equal(high$mapping_class, "high_population_coverage_single_target")
  expect_true(high$preferred_single_target)
  expect_false(high$exact_one_to_one)
  expect_equal(high$population_coverage, 0.99)
  expect_error(
    summarize_shrug_source_district_mapping(bridge, 1991L, 2001L, 0),
    "coverage must be in"
  )
})

test_that("historical language geography separates exact, high-coverage, and split mappings", {
  bridge <- data.frame(
    shrid2 = c("a", "b", "c", "d", "e", "f"),
    state_code_1991 = "02", district_code_1991 = c("01", "01", "02", "03", "03", "03"),
    state_code_2001 = "02", district_code_2001 = c("01", "02", "03", "04", "04", NA),
    deterministic = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    population = c(90, 10, 50, 495, 495, 10),
    stringsAsFactors = FALSE
  )

  out <- historical_1991_district_geography_summary(bridge, min_population_coverage = 0.99)
  split <- out[out$district_code_1991 == "01", ]
  exact <- out[out$district_code_1991 == "02", ]
  high_coverage <- out[out$district_code_1991 == "03", ]

  expect_equal(split$mapping_class, "splits_across_2001_districts")
  expect_false(split$preferred_language_persistence)
  expect_equal(split$n_target_2001_districts, 2L)
  expect_equal(exact$mapping_class, "deterministic_one_to_one")
  expect_true(exact$exact_language_persistence)
  expect_true(exact$preferred_language_persistence)
  expect_equal(high_coverage$mapping_class, "high_population_coverage_single_target")
  expect_false(high_coverage$exact_language_persistence)
  expect_true(high_coverage$preferred_language_persistence)
  expect_equal(high_coverage$population_coverage, 0.99)

  sensitivity <- historical_linguistic_geography_sensitivity(out, thresholds = c(0.99, 1))
  expect_equal(sensitivity$eligible_districts, c(2L, 1L))
})


test_that("canonical geography transitions preserve weights and source provenance", {
  transition <- data.frame(
    state_code_2011 = c("01", "01"),
    district_code_2011 = c("001", "001"),
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("01", "02"),
    population_share_to_2001 = c(.7, .3),
    area_share_to_2001 = c(.6, .4),
    shrid_coverage = c(1, 1),
    mapping_class = "deterministic_containment",
    source_id = "shrug",
    stringsAsFactors = FALSE
  )

  out <- as_geography_transition(transition, 2011, 2001)

  expect_identical(out$topology, c("split", "split"))
  expect_equal(out$population_weight, c(.7, .3))
  expect_equal(out$area_weight, c(.6, .4))
  expect_identical(out$evidence_source, c("shrug", "shrug"))
  expect_identical(out$source_unit_id, rep("census2011__01__001", 2))
  expect_setequal(
    out$target_unit_id,
    c("census2001__01__01", "census2001__01__02")
  )
})

test_that("canonical geography topology distinguishes mergers and many-to-many changes", {
  merger <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01"),
    source_district_code = c("01", "02"),
    source_unit_id = c("census1991__01__01", "census1991__01__02"),
    target_state_code = c("01", "01"),
    target_district_code = c("01", "01"),
    target_unit_id = c("census2001__01__01", "census2001__01__01"),
    population_weight = c(1, 1),
    area_weight = c(1, 1),
    source_coverage = c(1, 1),
    target_coverage = c(1, 1),
    mapping_class = "reviewed",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  expect_identical(
    annotate_geography_transition_topology(merger)$topology,
    c("merger", "merger")
  )

  many <- rbind(
    merger,
    transform(
      merger,
      target_district_code = "02",
      target_unit_id = "census2001__01__02"
    )
  )
  expect_true(all(
    annotate_geography_transition_topology(many)$topology ==
      "many_to_many"
  ))
})

test_that("canonical geography transitions reject invalid allocation weights", {
  transition <- data.frame(
    state_code_2011 = "01",
    district_code_2011 = "001",
    state_code_2001 = "01",
    district_code_2001 = "01",
    population_share_to_2001 = 1.2,
    area_share_to_2001 = 1,
    mapping_class = "test",
    stringsAsFactors = FALSE
  )
  expect_error(
    as_geography_transition(transition, 2011, 2001),
    "population_weight"
  )
})


test_that("geography components use standard undirected connected components", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01", "01", "01"),
    source_district_code = c("01", "02", "03", "03"),
    source_unit_id = c(
      "census1991__01__01", "census1991__01__02",
      "census1991__01__03", "census1991__01__03"
    ),
    target_state_code = c("01", "01", "01", "01"),
    target_district_code = c("01", "01", "02", "03"),
    target_unit_id = c(
      "census2001__01__01", "census2001__01__01",
      "census2001__01__02", "census2001__01__03"
    ),
    population_weight = c(1, 1, .6, .4),
    area_weight = c(1, 1, .5, .5),
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "deterministic_containment",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )

  membership <- build_geography_components(transition)
  summary <- summarize_geography_components(
    transition, membership
  )

  expect_equal(nrow(summary), 2L)
  expect_setequal(summary$component_class, c("merger", "split"))
  expect_true(all(summary$deterministic_amalgamation_eligible))
  expect_equal(
    length(unique(membership$harmonized_component_id)),
    2L
  )
})

test_that("deterministic amalgamation requires complete coverage on both vintages", {
  transition <- data.frame(
    source_vintage = c(1991L, 1991L),
    target_vintage = c(2001L, 2001L),
    source_state_code = c("01", "01"),
    source_district_code = c("01", "02"),
    source_unit_id = c("census1991__01__01", "census1991__01__02"),
    target_state_code = c("01", "01"),
    target_district_code = c("01", "01"),
    target_unit_id = c("census2001__01__01", "census2001__01__01"),
    population_weight = c(1, 1),
    area_weight = c(1, 1),
    source_coverage = c(1, .98),
    target_coverage = c(1, 1),
    mapping_class = "deterministic_containment",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  summary <- summarize_geography_components(transition)

  expect_identical(summary$component_class, "merger")
  expect_false(summary$source_coverage_complete)
  expect_true(summary$target_coverage_complete)
  expect_false(summary$deterministic_amalgamation_eligible)
})


test_that("geography component IDs are stable across edge ordering", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01", "01"),
    source_district_code = c("01", "02", "03"),
    source_unit_id = c(
      "census1991__01__01",
      "census1991__01__02",
      "census1991__01__03"
    ),
    target_state_code = c("01", "01", "01"),
    target_district_code = c("01", "01", "02"),
    target_unit_id = c(
      "census2001__01__01",
      "census2001__01__01",
      "census2001__01__02"
    ),
    population_weight = 1,
    area_weight = 1,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "deterministic_containment",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )

  forward <- build_geography_components(transition)
  reverse <- build_geography_components(
    transition[rev(seq_len(nrow(transition))), , drop = FALSE]
  )

  forward <- forward[order(forward$unit_id), ]
  reverse <- reverse[order(reverse$unit_id), ]
  expect_identical(
    forward$harmonized_component_id,
    reverse$harmonized_component_id
  )
  expect_false(anyNA(forward$harmonized_component_id))
})

test_that("deterministic harmonized crosswalk includes complete components only", {
  transition <- data.frame(
    source_vintage = c(1991L, 1991L, 1991L),
    target_vintage = c(2001L, 2001L, 2001L),
    source_state_code = c("01", "01", "01"),
    source_district_code = c("01", "02", "03"),
    source_unit_id = c(
      "census1991__01__01",
      "census1991__01__02",
      "census1991__01__03"
    ),
    target_state_code = c("01", "01", "01"),
    target_district_code = c("01", "01", "02"),
    target_unit_id = c(
      "census2001__01__01",
      "census2001__01__01",
      "census2001__01__02"
    ),
    population_weight = 1,
    area_weight = 1,
    source_coverage = c(1, 1, .98),
    target_coverage = 1,
    mapping_class = "deterministic_containment",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )

  components <- build_geography_components(transition)
  summary <- summarize_geography_components(transition, components)
  crosswalk <- build_harmonized_region_crosswalk(
    components, summary
  )

  expect_equal(length(unique(crosswalk$harmonized_region_id)), 1L)
  expect_identical(unique(crosswalk$component_class), "merger")
  expect_true(all(crosswalk$deterministic_amalgamation_eligible))
  expect_setequal(
    crosswalk$vintage,
    c(1991L, 2001L)
  )
  expect_setequal(
    crosswalk$district_code[crosswalk$vintage == 1991L],
    c("01", "02")
  )
  expect_identical(
    crosswalk$district_code[crosswalk$vintage == 2001L],
    "01"
  )
})

test_that("harmonized crosswalk never duplicates a vintage geography unit", {
  membership <- data.frame(
    harmonized_component_id = c("a", "b"),
    vintage = c(1991L, 1991L),
    state_code = c("01", "01"),
    district_code = c("01", "01"),
    unit_id = c("same", "same"),
    side = "source",
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    harmonized_component_id = c("a", "b"),
    component_class = c("one_to_one", "one_to_one"),
    source_coverage_complete = TRUE,
    target_coverage_complete = TRUE,
    deterministic_amalgamation_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  expect_error(
    build_harmonized_region_crosswalk(membership, summary),
    "cannot belong to multiple"
  )
})


test_that("geography transition comparison separates agreement from conflict", {
  base <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01", "01"),
    source_district_code = c("01", "01", "02"),
    source_unit_id = c("s1", "s1", "s2"),
    target_state_code = c("01", "01", "01"),
    target_district_code = c("01", "02", "03"),
    target_unit_id = c("t1", "t2", "t3"),
    population_weight = c(.6, .4, 1),
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "reference",
    stringsAsFactors = FALSE
  )
  candidate <- base
  candidate$evidence_source <- "candidate"
  candidate$population_weight <- c(.61, .39, 1)
  candidate <- candidate[c(1, 3), , drop = FALSE]
  candidate <- rbind(
    candidate,
    transform(
      candidate[1, , drop = FALSE],
      source_district_code = "01",
      source_unit_id = "s1",
      target_district_code = "04",
      target_unit_id = "t4",
      population_weight = .39
    )
  )

  out <- compare_geography_transitions(base, candidate)

  s1 <- out$sources[out$sources$source_unit_id == "s1", , drop = FALSE]
  s2 <- out$sources[out$sources$source_unit_id == "s2", , drop = FALSE]
  expect_identical(s1$source_status, "target_set_conflict")
  expect_identical(s2$source_status, "exact_target_set_agreement")
  expect_equal(out$summary$n_shared_sources, 2L)
  expect_equal(out$summary$n_exact_target_set_agreements, 1L)
  expect_equal(out$summary$n_target_set_conflicts, 1L)
  expect_true(any(
    out$edges$edge_status == "both" &
      is.finite(out$edges$population_weight_abs_diff)
  ))
})

test_that("empty geography transition uses the canonical schema", {
  out <- empty_geography_transition(annotated = TRUE)
  expect_identical(
    names(out)[seq_along(geography_transition_columns())],
    geography_transition_columns()
  )
  expect_true(all(
    c("source_degree", "target_degree", "topology") %in% names(out)
  ))
  expect_equal(nrow(out), 0L)
})


test_that("geography coverage normalization preserves keyed unit IDs", {
  input <- c(
    census1991__01__01 = 1.0003,
    census1991__01__02 = .9997,
    census1991__01__03 = 1.01,
    census1991__01__04 = .75
  )
  out <- normalize_geography_coverage(
    input,
    rounding_tolerance = .0005
  )

  expect_identical(names(out), names(input))
  expect_equal(unname(out[1:2]), c(1, 1))
  expect_equal(unname(out[3:4]), c(1.01, .75))
  expect_equal(
    unname(out[c("census1991__01__01", "census1991__01__04")]),
    c(1, .75)
  )
  expect_error(
    normalize_geography_coverage(1, rounding_tolerance = -1),
    "nonnegative"
  )
})


test_that("geography concordance compares both descendants and parents", {
  make_transition <- function(edges) {
    out <- data.frame(
      source_vintage = 1991L,
      target_vintage = 2001L,
      source_state_code = "01",
      source_district_code = sub("s", "", edges$source_unit_id),
      source_unit_id = edges$source_unit_id,
      target_state_code = "01",
      target_district_code = sub("t", "", edges$target_unit_id),
      target_unit_id = edges$target_unit_id,
      population_weight = 1,
      area_weight = NA_real_,
      source_coverage = 1,
      target_coverage = 1,
      mapping_class = "test",
      evidence_source = "test",
      stringsAsFactors = FALSE
    )
    annotate_geography_transition_topology(out)
  }
  reference <- make_transition(data.frame(
    source_unit_id = c("s1", "s2"),
    target_unit_id = c("t1", "t1")
  ))
  candidate <- make_transition(data.frame(
    source_unit_id = c("s1", "s2"),
    target_unit_id = c("t1", "t2")
  ))

  out <- compare_geography_transitions(reference, candidate)

  expect_identical(
    out$sources$source_status[
      out$sources$source_unit_id == "s1"
    ],
    "exact_target_set_agreement"
  )
  expect_identical(
    out$targets$target_status[
      out$targets$target_unit_id == "t1"
    ],
    "source_set_conflict"
  )
  expect_equal(out$summary$n_source_set_conflicts, 1L)
})

test_that("consensus geography requires bilateral set agreement and complete coverage", {
  make_transition <- function(source, target, source_cov = 1, target_cov = 1) {
    out <- data.frame(
      source_vintage = 1991L,
      target_vintage = 2001L,
      source_state_code = "01",
      source_district_code = sub("s", "", source),
      source_unit_id = source,
      target_state_code = "01",
      target_district_code = sub("t", "", target),
      target_unit_id = target,
      population_weight = 1,
      area_weight = NA_real_,
      source_coverage = source_cov,
      target_coverage = target_cov,
      mapping_class = "test",
      evidence_source = "test",
      stringsAsFactors = FALSE
    )
    annotate_geography_transition_topology(out)
  }
  reference <- make_transition(c("s1", "s2"), c("t1", "t2"))
  candidate <- make_transition(c("s1", "s2"), c("t1", "t2"))
  comparison <- compare_geography_transitions(reference, candidate)

  consensus <- build_consensus_geography_transition(
    reference, candidate, comparison
  )

  expect_equal(nrow(consensus), 2L)
  expect_true(all(consensus$source_coverage == 1))
  expect_true(all(consensus$target_coverage == 1))
  expect_true(all(is.na(consensus$population_weight)))
  expect_true(all(
    consensus$mapping_class == "bilateral_exact_consensus"
  ))

  candidate$target_coverage[candidate$target_unit_id == "t2"] <- .9
  incomplete <- build_consensus_geography_transition(
    reference,
    candidate,
    compare_geography_transitions(reference, candidate)
  )
  expect_setequal(incomplete$source_unit_id, "s1")
})

test_that("consensus geography excludes unilateral source agreement", {
  reference <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = "01",
    source_district_code = c("01", "02"),
    source_unit_id = c("s1", "s2"),
    target_state_code = "01",
    target_district_code = "01",
    target_unit_id = "t1",
    population_weight = 1,
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "reference",
    stringsAsFactors = FALSE
  )
  candidate <- reference[1, , drop = FALSE]
  candidate$evidence_source <- "candidate"

  reference <- annotate_geography_transition_topology(reference)
  candidate <- annotate_geography_transition_topology(candidate)
  comparison <- compare_geography_transitions(reference, candidate)
  consensus <- build_consensus_geography_transition(
    reference, candidate, comparison
  )

  expect_equal(nrow(consensus), 0L)
})


test_that("geography allocation semantics keep human and area weights distinct", {
  semantics <- geography_allocation_semantics_registry()
  expect_invisible(validate_geography_allocation_semantics(semantics))

  human <- semantics[
    semantics$semantic_id == "extensive_human",
    ,
    drop = FALSE
  ]
  land <- semantics[
    semantics$semantic_id == "land_area",
    ,
    drop = FALSE
  ]
  survey <- semantics[
    semantics$semantic_id == "survey_microdata",
    ,
    drop = FALSE
  ]

  expect_true(human$population_fractional_allowed)
  expect_false(human$area_fractional_allowed)
  expect_true(land$area_fractional_allowed)
  expect_false(land$population_fractional_allowed)
  expect_identical(
    survey$aggregation_operation,
    "reweight_records"
  )

  invalid <- semantics
  invalid$area_fractional_allowed[
    invalid$semantic_id == "extensive_human"
  ] <- TRUE
  expect_error(
    validate_geography_allocation_semantics(invalid),
    "interchangeable"
  )
})

test_that("geography measure families require sufficient-statistic semantics", {
  families <- geography_measure_family_registry()
  semantics <- geography_allocation_semantics_registry()

  expect_invisible(
    validate_geography_measure_families(families, semantics)
  )
  expect_identical(
    families$sufficient_statistics[
      families$measure_family == "eventual_emie"
    ],
    "enrolled_weight+eligible_weight"
  )
  expect_identical(
    families$sufficient_statistics[
      families$measure_family == "linguistic_distance"
    ],
    "speaker_count+distance_components"
  )
  expect_identical(
    families$semantic_id[
      families$measure_family == "consumption_welfare"
    ],
    "survey_microdata"
  )

  invalid <- families
  invalid$semantic_id[[1L]] <- "unknown"
  expect_error(
    validate_geography_measure_families(invalid, semantics),
    "unknown semantics"
  )
})

test_that("geography specifications expose assumptions instead of calendar rules", {
  specs <- geography_specification_registry()
  expect_invisible(validate_geography_specifications(specs))
  expect_setequal(
    specs$geography_spec_id,
    c(
      "G0_exact_only",
      "G1_deterministic_amalgamation",
      "G2_population_interpolated",
      "G3_area_interpolated",
      "G4_reviewed_fractional"
    )
  )
  expect_false(
    specs$allows_fractional_allocation[
      specs$geography_spec_id == "G1_deterministic_amalgamation"
    ]
  )
  expect_identical(
    specs$fractional_basis[
      specs$geography_spec_id == "G2_population_interpolated"
    ],
    "population"
  )
  expect_identical(
    specs$fractional_basis[
      specs$geography_spec_id == "G3_area_interpolated"
    ],
    "area"
  )
})

test_that("multi-vintage geography connects adjacent transition graphs", {
  transition <- function(
      source_vintage, target_vintage,
      source_unit, target_unit) {
    data.frame(
      source_vintage = source_vintage,
      target_vintage = target_vintage,
      source_state_code = "01",
      source_district_code = sub(".*__", "", source_unit),
      source_unit_id = source_unit,
      target_state_code = "01",
      target_district_code = sub(".*__", "", target_unit),
      target_unit_id = target_unit,
      population_weight = NA_real_,
      area_weight = NA_real_,
      source_coverage = 1,
      target_coverage = 1,
      mapping_class = "test",
      evidence_source = "test",
      stringsAsFactors = FALSE
    )
  }

  t91 <- transition(
    1991L, 2001L,
    "census1991__01__01",
    "census2001__01__01"
  )
  t11 <- transition(
    2011L, 2001L,
    "census2011__01__001",
    "census2001__01__01"
  )

  out <- build_multivintage_geography_inventory(
    list(old = t91, recent = t11),
    required_vintages = c(1991L, 2001L, 2011L)
  )

  expect_equal(nrow(out$component_summary), 1L)
  expect_true(out$component_summary$spans_all_required_vintages)
  expect_equal(out$component_summary$n_units_1991, 1L)
  expect_equal(out$component_summary$n_units_2001, 1L)
  expect_equal(out$component_summary$n_units_2011, 1L)
  expect_setequal(
    out$transitions$transition_id,
    c("old", "recent")
  )
})

test_that("multi-vintage membership collapses bridge units without duplication", {
  first <- data.frame(
    harmonized_component_id = "c1",
    vintage = 2001L,
    state_code = "01",
    district_code = "01",
    unit_id = "census2001__01__01",
    side = "target",
    stringsAsFactors = FALSE
  )
  second <- first
  second$side <- "source"

  out <- collapse_multivintage_component_membership(
    rbind(first, second)
  )

  expect_equal(nrow(out), 1L)
  expect_identical(out$side, "bridge")
})

test_that("geography harmonization saver exposes policy and graph diagnostics", {
  semantics <- geography_allocation_semantics_registry()
  families <- geography_measure_family_registry()
  specs <- geography_specification_registry()
  empty <- list(
    transitions = data.frame(status = "test"),
    components = data.frame(status = "test"),
    component_summary = data.frame(status = "test"),
    transition_summary = data.frame(status = "test")
  )
  dir <- tempfile()

  paths <- save_geography_harmonization_foundation(
    empty, semantics, families, specs, directory = dir
  )

  expect_setequal(
    basename(paths),
    c(
      "allocation_semantics.csv",
      "measure_allocation_families.csv",
      "geography_specifications.csv",
      "multivintage_transitions.csv",
      "multivintage_components.csv",
      "multivintage_component_summary.csv",
      "multivintage_transition_summary.csv"
    )
  )
})


test_that("production canonical lineage coverage is derived bidirectionally from SHRUG", {
  transition <- data.frame(
    state_code_2011 = "01",
    district_code_2011 = "001",
    state_code_2001 = "01",
    district_code_2001 = "01",
    population_share_to_2001 = 1,
    area_share_to_2001 = 1,
    shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    source_id = "official",
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    shrid2 = c("a", "b"),
    deterministic = TRUE,
    population = c(60, 40),
    area = c(6, 4),
    state_code_2011 = "01",
    district_code_2011 = "001",
    state_code_2001 = "01",
    district_code_2001 = "01",
    stringsAsFactors = FALSE
  )

  out <- attach_shrug_transition_coverage(
    as_geography_transition(transition, 2011L, 2001L),
    bridge, 2011L, 2001L
  )

  expect_equal(out$source_coverage, 1)
  expect_equal(out$target_coverage, 1)
  expect_identical(out$evidence_source, "official")
})

test_that("SHRUG coverage overrides overloaded raw coverage flags in canonical geography", {
  transition <- data.frame(
    state_code_2011 = "01",
    district_code_2011 = "001",
    state_code_2001 = "01",
    district_code_2001 = "01",
    population_share_to_2001 = 1,
    area_share_to_2001 = 1,
    shrid_coverage = 1,
    mapping_class = "official_lgd_census_code_bridge",
    source_id = "official",
    stringsAsFactors = FALSE
  )
  bridge <- data.frame(
    shrid2 = c("a", "b"),
    deterministic = c(TRUE, FALSE),
    population = c(60, 40),
    area = c(6, 4),
    state_code_2011 = "01",
    district_code_2011 = "001",
    state_code_2001 = "01",
    district_code_2001 = "01",
    stringsAsFactors = FALSE
  )

  out <- attach_shrug_transition_coverage(
    as_geography_transition(transition, 2011L, 2001L),
    bridge, 2011L, 2001L
  )

  expect_equal(out$source_coverage, .5)
  expect_equal(out$target_coverage, .5)
  expect_false(
    summarize_geography_components(out)$deterministic_amalgamation_eligible
  )
})

test_that("exact transition extraction retains whole closed components only", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01", "01"),
    source_district_code = c("01", "02", "03"),
    source_unit_id = c("s1", "s2", "s3"),
    target_state_code = c("01", "01", "01"),
    target_district_code = c("01", "01", "02"),
    target_unit_id = c("t1", "t1", "t2"),
    population_weight = NA_real_,
    area_weight = NA_real_,
    source_coverage = c(1, 1, .9),
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )

  out <- extract_exact_geography_transition(transition)

  expect_setequal(out$source_unit_id, c("s1", "s2"))
  expect_setequal(out$target_unit_id, "t1")
})

test_that("exact multi-vintage geography requires pairwise closure and all vintages", {
  transition <- function(
      source_vintage, target_vintage,
      source_unit, target_unit,
      source_coverage = 1, target_coverage = 1) {
    data.frame(
      source_vintage = source_vintage,
      target_vintage = target_vintage,
      source_state_code = "01",
      source_district_code = sub(".*__", "", source_unit),
      source_unit_id = source_unit,
      target_state_code = "01",
      target_district_code = sub(".*__", "", target_unit),
      target_unit_id = target_unit,
      population_weight = NA_real_,
      area_weight = NA_real_,
      source_coverage = source_coverage,
      target_coverage = target_coverage,
      mapping_class = "test",
      evidence_source = "test",
      stringsAsFactors = FALSE
    )
  }

  t91 <- transition(
    1991L, 2001L,
    "census1991__01__01",
    "census2001__01__01"
  )
  t11 <- transition(
    2011L, 2001L,
    "census2011__01__001",
    "census2001__01__01"
  )

  out <- build_exact_multivintage_geography(
    list(old = t91, recent = t11),
    required_vintages = c(1991L, 2001L, 2011L)
  )

  expect_equal(nrow(out$component_summary), 1L)
  expect_true(out$component_summary$spans_all_required_vintages)
  expect_equal(length(unique(out$crosswalk$harmonized_region_id)), 1L)
  expect_setequal(out$crosswalk$vintage, c(1991L, 2001L, 2011L))
  expect_true(all(
    out$crosswalk$geography_spec_id ==
      "G1_deterministic_amalgamation"
  ))

  t11$target_coverage <- .9
  blocked <- build_exact_multivintage_geography(
    list(old = t91, recent = t11),
    required_vintages = c(1991L, 2001L, 2011L)
  )
  expect_equal(nrow(blocked$crosswalk), 0L)
})

test_that("exact multi-vintage saver exposes pairwise certification and crosswalk", {
  x <- list(
    transitions = data.frame(status = "test"),
    components = data.frame(status = "test"),
    component_summary = data.frame(status = "test"),
    crosswalk = data.frame(status = "test"),
    pairwise_summary = data.frame(status = "test")
  )
  dir <- tempfile()
  paths <- save_exact_multivintage_geography(
    x, directory = dir
  )

  expect_setequal(
    basename(paths),
    c(
      "exact_multivintage_transitions.csv",
      "exact_multivintage_components.csv",
      "exact_multivintage_component_summary.csv",
      "exact_multivintage_crosswalk.csv",
      "exact_multivintage_pairwise_summary.csv"
    )
  )
})


test_that("survey microdata cannot use generic population interpolation", {
  semantics <- geography_allocation_semantics_registry()
  survey <- semantics[
    semantics$semantic_id == "survey_microdata",
    ,
    drop = FALSE
  ]
  expect_false(survey$population_fractional_allowed)
  expect_identical(
    survey$preferred_fractional_basis,
    "reviewed_record_allocation"
  )

  compatible <- population_interpolation_compatible_measure_families()
  expect_false(
    "consumption_welfare" %in% compatible$measure_family
  )
  expect_false(
    "survey_person_outcomes" %in% compatible$measure_family
  )
  expect_true(
    "eventual_emie" %in% compatible$measure_family
  )
  expect_true(
    "linguistic_distance" %in% compatible$measure_family
  )
})

test_that("canonical SHRUG support replaces placeholder transition weights", {
  transition <- data.frame(
    state_code_2011 = "01",
    district_code_2011 = "001",
    state_code_2001 = c("01", "01"),
    district_code_2001 = c("01", "02"),
    population_share_to_2001 = c(1, 1),
    area_share_to_2001 = c(1, 1),
    shrid_coverage = 1,
    mapping_class = "reviewed_single_parent_ancestry",
    source_id = "reviewed",
    stringsAsFactors = FALSE
  )
  canonical <- as_geography_transition(
    transition, 2011L, 2001L
  )
  bridge <- data.frame(
    shrid2 = c("a", "b"),
    deterministic = TRUE,
    population = c(70, 30),
    area = c(8, 2),
    state_code_2011 = "01",
    district_code_2011 = "001",
    state_code_2001 = "01",
    district_code_2001 = c("01", "02"),
    stringsAsFactors = FALSE
  )

  out <- attach_shrug_transition_weights(
    canonical, bridge, 2011L, 2001L
  )

  expect_equal(out$population_weight, c(.7, .3))
  expect_equal(out$area_weight, c(.8, .2))
  expect_identical(out$evidence_source, c("reviewed", "reviewed"))
})

test_that("population interpolation preserves incomplete mass without renormalizing", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01"),
    source_district_code = c("01", "01"),
    source_unit_id = c("s1", "s1"),
    target_state_code = c("01", "01"),
    target_district_code = c("01", "02"),
    target_unit_id = c("t1", "t2"),
    population_weight = c(.6, .3),
    area_weight = NA_real_,
    source_coverage = .9,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )

  out <- build_population_interpolation_crosswalk(
    list(old = transition), target_vintage = 2001L
  )

  source <- out$source_coverage[
    out$source_coverage$source_unit_id == "s1",
    ,
    drop = FALSE
  ]
  expect_equal(source$source_population_coverage, .9)
  expect_equal(source$unallocated_population_share, .1)
  expect_identical(
    source$allocation_status,
    "partial_population_partition"
  )
  allocated <- out$crosswalk[
    out$crosswalk$source_unit_id == "s1",
    ,
    drop = FALSE
  ]
  expect_equal(sum(allocated$allocation_weight), .9)
})

test_that("population interpolation rejects materially overfull source partitions", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01"),
    source_district_code = c("01", "01"),
    source_unit_id = c("s1", "s1"),
    target_state_code = c("01", "01"),
    target_district_code = c("01", "02"),
    target_unit_id = c("t1", "t2"),
    population_weight = c(.6, .5),
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  expect_error(
    build_population_interpolation_crosswalk(
      list(old = transition), target_vintage = 2001L
    ),
    "exceed one"
  )
})

test_that("population allocation operates on sufficient statistics, not final ratios", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("01", "01"),
    source_district_code = c("01", "01"),
    source_unit_id = c("s1", "s1"),
    target_state_code = c("01", "01"),
    target_district_code = c("01", "02"),
    target_unit_id = c(
      "census2001__01__01",
      "census2001__01__02"
    ),
    population_weight = c(.75, .25),
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  map <- build_population_interpolation_crosswalk(
    list(old = transition), 2001L
  )$crosswalk
  data <- data.frame(
    unit = "s1",
    enrolled_weight = 40,
    eligible_weight = 100,
    stringsAsFactors = FALSE
  )

  out <- allocate_population_sufficient_statistics(
    data, map,
    source_vintage = 1991L,
    unit_field = "unit",
    statistic_fields = c("enrolled_weight", "eligible_weight"),
    measure_family = "eventual_emie"
  )

  expect_equal(
    sort(out$enrolled_weight),
    c(10, 30)
  )
  expect_equal(
    sort(out$eligible_weight),
    c(25, 75)
  )
  expect_equal(
    out$enrolled_weight / out$eligible_weight,
    c(.4, .4)
  )
  expect_setequal(
    names(out)[names(out) %in% c(
      "target_vintage", "target_state_code",
      "target_district_code", "target_unit_id"
    )],
    c(
      "target_vintage", "target_state_code",
      "target_district_code", "target_unit_id"
    )
  )
  expect_setequal(out$target_state_code, "01")
  expect_setequal(out$target_district_code, c("01", "02"))
  expect_setequal(
    out$target_unit_id,
    c("census2001__01__01", "census2001__01__02")
  )
})

test_that("generic population allocation fails closed for survey microdata", {
  transition <- data.frame(
    source_vintage = 2011L,
    target_vintage = 2001L,
    source_state_code = "01",
    source_district_code = "001",
    source_unit_id = "s1",
    target_state_code = "01",
    target_district_code = "01",
    target_unit_id = "t1",
    population_weight = 1,
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  map <- build_population_interpolation_crosswalk(
    list(recent = transition), 2001L
  )$crosswalk
  data <- data.frame(
    unit = "s1",
    record_weight = 2,
    stringsAsFactors = FALSE
  )

  expect_error(
    allocate_population_sufficient_statistics(
      data, map,
      source_vintage = 2011L,
      unit_field = "unit",
      statistic_fields = "record_weight",
      measure_family = "consumption_welfare"
    ),
    "not eligible"
  )
})

test_that("population interpolation saver exposes crosswalk and coverage diagnostics", {
  x <- list(
    crosswalk = data.frame(status = "test"),
    source_coverage = data.frame(status = "test"),
    summary = data.frame(status = "test")
  )
  dir <- tempfile()
  paths <- save_population_interpolation_geography(
    x, directory = dir
  )

  expect_setequal(
    basename(paths),
    c(
      "population_interpolation_crosswalk.csv",
      "population_interpolation_source_coverage.csv",
      "population_interpolation_summary.csv"
    )
  )
})


test_that("population allocation rejects inconsistent target identity metadata", {
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = "01",
    source_district_code = "01",
    source_unit_id = "s1",
    target_state_code = "01",
    target_district_code = "01",
    target_unit_id = "census2001__01__01",
    population_weight = 1,
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  map <- build_population_interpolation_crosswalk(
    list(old = transition), 2001L
  )$crosswalk
  map$target_district_code[
    map$transition_id != "target_identity"
  ] <- "99"

  expect_error(
    allocate_population_sufficient_statistics(
      data.frame(unit = "s1", count = 10),
      map,
      source_vintage = 1991L,
      unit_field = "unit",
      statistic_fields = "count",
      measure_family = "census_extensive_counts"
    ),
    "disagree with target administrative codes"
  )
})
