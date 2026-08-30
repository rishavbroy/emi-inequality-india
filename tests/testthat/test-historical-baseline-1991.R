historical_baseline_source_fixture <- function() {
  state <- rep(c("02", "03"), each = 2)
  district <- rep(sprintf("%02d", 1:2), 2)
  pca <- data.frame(
    pc91_state_id = state, pc91_district_id = district,
    pc91_pca_tot_p = c(1000, 1200, 900, 1100),
    pc91_pca_tot_f = c(480, 590, 430, 540),
    pc91_pca_p_06 = c(150, 180, 120, 165),
    pc91_pca_p_sc = c(100, 120, 90, 110),
    pc91_pca_p_st = c(50, 0, 180, 55),
    pc91_pca_p_lit = c(510, 650, 520, 610),
    pc91_pca_mainwork_p = c(400, 500, 360, 440),
    pc91_pca_margwork_p = c(40, 30, 45, 35),
    pc91_pca_main_cl_p = c(160, 200, 100, 176),
    pc91_pca_main_al_p = c(120, 150, 140, 132),
    stringsAsFactors = FALSE
  )
  vd <- data.frame(
    pc91_state_id = state, pc91_district_id = district,
    pc91_vd_p_sch = c(10, 12, 9, 11), pc91_vd_s_sch = c(2, 3, 1, 2),
    pc91_vd_hosp = c(1, 2, 1, 1), pc91_vd_ph_cntr = c(2, NA, 1, 2),
    stringsAsFactors = FALSE
  )
  td <- data.frame(
    pc91_state_id = state, pc91_district_id = district,
    pc91_td_p_7andup = c(300, 400, 250, 350),
    pc91_td_p_lit_7andup = c(180, 260, 175, 245),
    pc91_td_primary = c(4, 5, 3, 4), pc91_td_hospitals = c(1, 1, 0, 1),
    pc91_td_banks = c(2, 3, 1, 2), stringsAsFactors = FALSE
  )
  list(pca = pca, vd = vd, td = td)
}

test_that("SHRUG 1991 baseline constructs source-appropriate scale-free covariates", {
  out <- build_shrug_1991_baseline_controls(historical_baseline_source_fixture())

  expect_equal(nrow(out), 4L)
  expect_equal(out$female_share_1991[[1]], 48)
  expect_equal(out$child_share_0_6_1991[[1]], 15)
  expect_equal(out$literacy_share_7plus_1991[[1]], 60)
  expect_equal(out$cultivator_share_main_workers_1991[[1]], 40)
  expect_equal(out$rural_primary_schools_per_100k_1991[[1]], 1000)
  expect_equal(out$urban_literacy_share_7plus_1991[[1]], 60)
  expect_equal(out$urban_primary_schools_per_100k_7plus_1991[[1]], 4 / 300 * 1e5)
  expect_true(is.na(out$rural_phc_per_100k_1991[[2]]))

  coverage <- summarize_historical_baseline_1991_coverage(out)
  phc <- coverage[coverage$variable == "rural_phc_per_100k_1991", , drop = FALSE]
  expect_equal(phc$n_nonmissing, 3L)
  expect_equal(phc$source, "VD91")
})

test_that("SHRUG 1991 baseline rejects impossible shares and duplicate district keys", {
  source <- historical_baseline_source_fixture()
  source$pca$pc91_pca_p_lit[[1]] <- 1000
  expect_error(
    build_shrug_1991_baseline_controls(source),
    "bounded shares outside [0, 100]",
    fixed = TRUE
  )

  source <- historical_baseline_source_fixture()
  source$pca <- rbind(source$pca, source$pca[1, , drop = FALSE])
  expect_error(
    build_shrug_1991_baseline_controls(source),
    "duplicate 1991 district keys",
    fixed = TRUE
  )

  source <- historical_baseline_source_fixture()
  source$td$pc91_district_id[[1]] <- "99"
  expect_error(
    build_shrug_1991_baseline_controls(source),
    "absent from PCA91",
    fixed = TRUE
  )
})

test_that("SHRUG 1991 archive reader selects the documented district member", {
  root <- tempfile("shrug91-archive-")
  dir.create(root)
  csv <- file.path(root, "pc91_pca_clean_pc91dist.csv")
  utils::write.csv(data.frame(pc91_state_id = "02", pc91_district_id = "01"), csv, row.names = FALSE)
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  utils::zip("toy.zip", basename(csv), flags = "-q")
  out <- read_shrug_district_archive(file.path(root, "toy.zip"), basename(csv))
  expect_equal(nrow(out), 1L)
  expect_identical(names(out), c("pc91_state_id", "pc91_district_id"))
})

historical_baseline_balance_fixture <- function() {
  n <- 48L
  state <- sprintf("%02d", rep(2:9, each = 6))
  district <- sprintf("%02d", seq_len(n))
  treatment <- seq(-1, 1, length.out = n) + rep(c(-0.03, 0.02, 0.01), length.out = n)
  metadata <- historical_baseline_1991_metadata()
  baseline <- data.frame(
    state_code_1991 = state,
    district_code_1991 = district,
    population_1991 = 1000 + seq_len(n) * 10,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(metadata))) {
    baseline[[metadata$variable[[i]]]] <- sin(seq_len(n) * (i + 1) / 19) + i
  }
  baseline$log_population_1991 <- 2 * treatment + rep(seq_along(unique(state)), each = 6)

  source_districts <- data.frame(
    state_code_1991 = state, district_code_1991 = district,
    exact_language_persistence = c(FALSE, rep(TRUE, n - 2L), FALSE),
    preferred_language_persistence = c(rep(TRUE, n - 1L), FALSE),
    population_coverage = c(0.995, rep(1, n - 2L), 0.8),
    n_target_2001_districts = c(rep(1L, n - 1L), 2L),
    stringsAsFactors = FALSE
  )
  transition <- rbind(
    data.frame(
      state_code_1991 = state[1:(n - 1L)], district_code_1991 = district[1:(n - 1L)],
      state_code_2001 = state[1:(n - 1L)], district_code_2001 = district[1:(n - 1L)],
      stringsAsFactors = FALSE
    ),
    data.frame(
      state_code_1991 = rep(state[[n]], 2), district_code_1991 = rep(district[[n]], 2),
      state_code_2001 = rep(state[[n]], 2), district_code_2001 = c(district[[n]], "99"),
      stringsAsFactors = FALSE
    )
  )
  panel <- data.frame(
    state_code_2001 = state, district_code_2001 = district,
    emi_exposure_all_children_0708 = treatment,
    stringsAsFactors = FALSE
  )
  list(
    baseline = baseline,
    geography = list(source_districts = source_districts, transition = transition),
    panel = panel,
    treatment = treatment
  )
}

test_that("historical baseline balance keeps preferred and exact samples distinct", {
  fixture <- historical_baseline_balance_fixture()
  out <- build_historical_baseline_balance_1991(
    fixture$baseline, fixture$geography, fixture$panel
  )

  expect_setequal(out$estimates$predictor_id, "eventual_emie")
  expect_setequal(out$estimates$sample, c("preferred_geography", "exact_one_to_one"))
  expect_setequal(out$estimates$fixed_effect, c("none", "state"))
  expect_equal(unique(out$joint_balance$n_tested_covariates[out$joint_balance$domain == "demography"]), 5L)

  preferred <- out$estimates[
    out$estimates$sample == "preferred_geography" &
      out$estimates$fixed_effect == "none" &
      out$estimates$variable == "log_population_1991",
    , drop = FALSE
  ]
  exact <- out$estimates[
    out$estimates$sample == "exact_one_to_one" &
      out$estimates$fixed_effect == "none" &
      out$estimates$variable == "log_population_1991",
    , drop = FALSE
  ]
  expect_equal(preferred$n, 47L)
  expect_equal(exact$n, 46L)
  expect_true(is.finite(preferred$estimate))
  expect_true(is.finite(preferred$standardized_effect))
})

test_that("historical baseline balance keeps EMI and reviewed LD support separate", {
  fixture <- historical_baseline_balance_fixture()
  distance <- data.frame(
    state_code_1991 = fixture$baseline$state_code_1991,
    district_code_1991 = fixture$baseline$district_code_1991,
    min_accepted_coverage = 0.99,
    max_distance_bound_width = 0.5,
    historical_language_status = "eligible",
    ling_distance_nonzero_mean_1991 = seq(0.5, 4, length.out = nrow(fixture$baseline)),
    stringsAsFactors = FALSE
  )
  distance$historical_language_status[[2]] <- "below_coverage_threshold"

  complete <- build_historical_baseline_balance_1991(
    fixture$baseline, fixture$geography, fixture$panel, distance
  )
  fixture$panel$emi_exposure_all_children_0708[[3]] <- NA_real_
  missing_emie <- build_historical_baseline_balance_1991(
    fixture$baseline, fixture$geography, fixture$panel, distance
  )

  expect_setequal(complete$estimates$predictor_id, c("eventual_emie", "historical_ld_1991"))
  select_n <- function(x, predictor) {
    unique(x$estimates$n[
      x$estimates$predictor_id == predictor &
        x$estimates$sample == "preferred_geography" &
        x$estimates$fixed_effect == "none"
    ])
  }
  expect_equal(select_n(missing_emie, "eventual_emie"), select_n(complete, "eventual_emie") - 1L)
  expect_equal(select_n(missing_emie, "historical_ld_1991"), select_n(complete, "historical_ld_1991"))
})


test_that("historical joint balance does not label unavailable Wald inference as estimated", {
  fixture <- historical_baseline_balance_fixture()
  panel <- historical_baseline_1991_panel(
    fixture$baseline, fixture$geography, fixture$panel
  )
  rural <- historical_baseline_1991_metadata()
  rural <- rural$variable[rural$domain == "rural_development"]
  keep <- c(1:3, 7:9)
  for (variable in rural) panel[[variable]][-keep] <- NA_real_

  predictor <- unname(historical_baseline_predictors(panel)[["eventual_emie"]])
  expect_identical(predictor, "emie_exposure")
  out <- expect_warning(
    estimate_historical_baseline_joint_balance(
      panel, predictor, "rural_development", exact_only = FALSE
    ),
    NA
  )
  expect_identical(out$status, "not_estimable")
  expect_true(is.na(out$joint_f))
  expect_true(is.na(out$joint_p))
  expect_true(out$reason %in% c(
    "no_residual_degrees_of_freedom",
    "tested_terms_aliased",
    "clustered_joint_inference_unavailable"
  ))
})


test_that("historical balance sampling fails clearly on noncanonical predictor columns", {
  fixture <- historical_baseline_balance_fixture()
  panel <- historical_baseline_1991_panel(
    fixture$baseline, fixture$geography, fixture$panel
  )

  expect_error(
    historical_baseline_balance_sample(
      panel, "emi_exposure_all_children_0708", "log_population_1991"
    ),
    "Historical baseline balance sample lacks columns: emi_exposure_all_children_0708",
    fixed = TRUE
  )
})


test_that("historical weighted inference helper preserves clustered standardized-effect contract", {
  set.seed(591)
  n <- 80L
  x <- data.frame(
    predictor = stats::rnorm(n),
    outcome = stats::rnorm(n),
    population = sample(100:1000, n, replace = TRUE),
    state = rep(sprintf("%02d", 1:8), each = 10),
    stringsAsFactors = FALSE
  )
  out <- historical_weighted_term_inference(
    x, "predictor", "outcome", "population", "state", "state"
  )
  expect_true(all(is.finite(unlist(out[c(
    "estimate", "std.error", "p.value", "standardized_effect"
  )]))))
  expect_equal(out$n, n)
  expect_equal(out$n_states, 8L)
  expect_equal(out$population_weight, sum(x$population))
})


test_that("historical baseline reuses one balance engine for broad and strict distance sources", {
  fixture <- historical_baseline_balance_fixture()
  fixture$panel$ling_distance_nonzero_mean <- seq(
    1, 4, length.out = nrow(fixture$panel)
  )
  atlas <- data.frame(
    state_code_1991 = fixture$baseline$state_code_1991,
    district_code_1991 = fixture$baseline$district_code_1991,
    min_accepted_coverage = 0.99,
    max_distance_bound_width = 0.5,
    historical_language_status = "eligible",
    ling_distance_nonzero_mean_1991 = seq(
      .5, 4, length.out = nrow(fixture$baseline)
    ),
    stringsAsFactors = FALSE
  )
  atlas$historical_language_status[[2]] <- "below_coverage_threshold"
  helms <- data.frame(
    state_code_1991 = fixture$baseline$state_code_1991,
    district_code_1991 = fixture$baseline$district_code_1991,
    linguistic_distance_1991_helms_lim = seq(
      .6, 4.1, length.out = nrow(fixture$baseline)
    ),
    stringsAsFactors = FALSE
  )
  helms$linguistic_distance_1991_helms_lim[[4]] <- NA_real_

  out <- build_historical_baseline_balance_1991(
    fixture$baseline, fixture$geography, fixture$panel,
    historical_distance = atlas,
    external_historical_distance = helms
  )

  expect_setequal(
    out$estimates$predictor_id,
    c(
      "eventual_emie", "census_2001_ld",
      "helms_lim_ld_1991", "historical_ld_1991"
    )
  )
  preferred <- out$predictor_coverage[
    out$predictor_coverage$sample == "preferred_geography",
    , drop = FALSE
  ]
  n_by_id <- setNames(preferred$n, preferred$predictor_id)
  expect_equal(n_by_id[["eventual_emie"]], 47L)
  expect_equal(n_by_id[["census_2001_ld"]], 47L)
  expect_equal(n_by_id[["helms_lim_ld_1991"]], 46L)
  expect_equal(n_by_id[["historical_ld_1991"]], 46L)
})

test_that("historical baseline optional external source does not change Atlas eligibility", {
  fixture <- historical_baseline_balance_fixture()
  atlas <- data.frame(
    state_code_1991 = fixture$baseline$state_code_1991,
    district_code_1991 = fixture$baseline$district_code_1991,
    min_accepted_coverage = 0.99,
    max_distance_bound_width = 0.5,
    historical_language_status = "eligible",
    ling_distance_nonzero_mean_1991 = seq(
      .5, 4, length.out = nrow(fixture$baseline)
    ),
    stringsAsFactors = FALSE
  )
  atlas$historical_language_status[[2]] <- "below_coverage_threshold"

  without_external <- build_historical_baseline_balance_1991(
    fixture$baseline, fixture$geography, fixture$panel,
    historical_distance = atlas
  )
  helms <- data.frame(
    state_code_1991 = fixture$baseline$state_code_1991,
    district_code_1991 = fixture$baseline$district_code_1991,
    linguistic_distance_1991_helms_lim = NA_real_,
    stringsAsFactors = FALSE
  )
  with_external <- build_historical_baseline_balance_1991(
    fixture$baseline, fixture$geography, fixture$panel,
    historical_distance = atlas,
    external_historical_distance = helms
  )

  atlas_n <- function(x) {
    x$predictor_coverage$n[
      x$predictor_coverage$predictor_id == "historical_ld_1991" &
        x$predictor_coverage$sample == "preferred_geography"
    ]
  }
  expect_equal(atlas_n(without_external), atlas_n(with_external))
})


test_that("PCA91 sufficient statistics reconstruct the existing PCA baseline", {
  source <- historical_baseline_source_fixture()
  counts <- build_shrug_1991_pca_sufficient_statistics(source$pca)
  measures <- historical_baseline_1991_pca_measures_from_counts(counts)
  existing <- clean_shrug_pca_1991_district(source$pca)

  expect_equal(measures$population_1991, existing$population_1991)
  for (variable in historical_baseline_1991_pca_variables()) {
    expect_equal(measures[[variable]], existing[[variable]])
  }
})

test_that("G2 PCA baseline allocates counts before reconstructing shares", {
  source <- historical_baseline_source_fixture()
  pca <- source$pca[1, , drop = FALSE]
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("02", "02"),
    source_district_code = c("01", "01"),
    source_unit_id = c(
      "census1991__02__01", "census1991__02__01"
    ),
    target_state_code = c("02", "02"),
    target_district_code = c("01", "02"),
    target_unit_id = c(
      "census2001__02__01", "census2001__02__02"
    ),
    population_weight = c(.75, .25),
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  crosswalk <- build_population_interpolation_crosswalk(
    list(old = transition), 2001L
  )$crosswalk

  out <- build_population_interpolated_pca_baseline_1991(
    pca, crosswalk, coverage_threshold = .99
  )

  expect_equal(sort(out$controls$population_1991), c(250, 750))
  expect_setequal(
    out$controls$district_code_2001,
    c("01", "02")
  )
  expect_setequal(
    out$controls$target_unit_id,
    c("census2001__02__01", "census2001__02__02")
  )
  expect_equal(
    out$controls$female_share_1991[
      out$controls$district_code_2001 == "01"
    ],
    48
  )
  expect_equal(
    out$controls$literacy_share_7plus_1991[
      out$controls$district_code_2001 == "02"
    ],
    60
  )
  expect_equal(out$coverage$allocated_population_share, 1)
})

test_that("G2 PCA baseline excludes source districts below the declared threshold", {
  source <- historical_baseline_source_fixture()
  pca <- source$pca[1:2, , drop = FALSE]
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = c("02", "02"),
    source_district_code = c("01", "02"),
    source_unit_id = c(
      "census1991__02__01", "census1991__02__02"
    ),
    target_state_code = c("02", "02"),
    target_district_code = c("01", "02"),
    target_unit_id = c(
      "census2001__02__01", "census2001__02__02"
    ),
    population_weight = c(.995, .94),
    area_weight = NA_real_,
    source_coverage = c(.995, .94),
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  crosswalk <- build_population_interpolation_crosswalk(
    list(old = transition), 2001L
  )$crosswalk

  out <- build_population_interpolated_pca_baseline_1991(
    pca, crosswalk, coverage_threshold = .99
  )

  expect_equal(out$coverage$n_source_districts, 1L)
  expect_equal(out$coverage$n_target_districts, 1L)
  expect_equal(
    out$coverage$allocated_population_share,
    .995
  )
})

test_that("G2 historical baseline sensitivity estimates PCA human domains only", {
  source <- historical_baseline_source_fixture()
  pca <- source$pca
  transition <- data.frame(
    source_vintage = 1991L,
    target_vintage = 2001L,
    source_state_code = pad_admin_code(pca$pc91_state_id, 2L),
    source_district_code = pad_admin_code(pca$pc91_district_id, 2L),
    source_unit_id = geography_transition_unit_id(
      1991L, pca$pc91_state_id, pca$pc91_district_id
    ),
    target_state_code = pad_admin_code(pca$pc91_state_id, 2L),
    target_district_code = pad_admin_code(pca$pc91_district_id, 2L),
    target_unit_id = geography_transition_unit_id(
      2001L, pca$pc91_state_id, pca$pc91_district_id
    ),
    population_weight = 1,
    area_weight = NA_real_,
    source_coverage = 1,
    target_coverage = 1,
    mapping_class = "test",
    evidence_source = "test",
    stringsAsFactors = FALSE
  )
  crosswalk <- build_population_interpolation_crosswalk(
    list(old = transition), 2001L
  )$crosswalk
  panel <- data.frame(
    state_code_2001 = pad_admin_code(pca$pc91_state_id, 2L),
    district_code_2001 = pad_admin_code(pca$pc91_district_id, 2L),
    emi_exposure_all_children_0708 = c(.1, .2, .3, .4),
    ling_distance_nonzero_mean = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  out <- build_historical_baseline_g2_sensitivity(
    pca, crosswalk, panel,
    coverage_thresholds = c(.90, .99)
  )

  expect_setequal(
    unique(out$estimates$source),
    "PCA91"
  )
  expect_setequal(
    unique(out$estimates$source_coverage_threshold),
    c(.90, .99)
  )
  expect_false(any(
    out$estimates$domain %in%
      c("rural_development", "urban_development")
  ))
  expect_setequal(
    out$estimates$predictor_id,
    c("eventual_emie", "census_2001_ld")
  )
})

test_that("G2 historical baseline saver exposes coverage and inference", {
  x <- list(
    controls = data.frame(status = "test"),
    coverage = data.frame(status = "test"),
    estimates = data.frame(status = "test"),
    joint_balance = data.frame(status = "test")
  )
  dir <- tempfile()
  paths <- save_historical_baseline_g2_sensitivity(
    x, directory = dir
  )
  expect_setequal(
    basename(paths),
    c(
      "historical_baseline_1991_g2_controls.csv",
      "historical_baseline_1991_g2_coverage.csv",
      "historical_baseline_1991_g2_balance.csv",
      "historical_baseline_1991_g2_balance_joint.csv"
    )
  )
})
