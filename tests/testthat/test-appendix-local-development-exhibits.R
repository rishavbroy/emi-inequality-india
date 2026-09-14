mechanism_fixture <- function(n_outcomes) {
  outcomes <- paste0("outcome_", seq_len(n_outcomes))
  grid <- expand.grid(
    outcome_id = outcomes,
    adjustment_id = c("region_main", "state_main"),
    construction_id = c("nonzero_mean", "glottolog_mean", "dyen_noncognate"),
    stringsAsFactors = FALSE
  )
  reduced <- transform(
    grid,
    outcome_variable = outcome_id, mechanism_family = "family", tier = "core",
    denominator = "population", specification_id = paste(adjustment_id, construction_id, sep = "__"),
    fixed_effect = ifelse(adjustment_id == "state_main", "state", "region"),
    term = "ling_distance", estimate = seq_len(nrow(grid)) / 100,
    std.error = 0.01, p.value = 0.20, p_holm_within_spec = 0.40,
    n = 500L, status = "estimated"
  )
  empty <- data.frame()
  list(
    registry = empty, sample_coverage = empty, sample_support = empty,
    first_stage = empty, reduced_form = reduced, weak_iv = empty
  )
}

test_that("migration appendix keeps preferred mobility nulls and skilled sorting evidence", {
  migration <- mechanism_fixture(8L)
  ids <- c(
    "interstate_migrant_composition", "work_migration_reason",
    "education_migration_reason", "skilled_migrant_composition",
    "technical_migrant_composition", "outside_state_recent_work_migration",
    "skilled_recent_work_migration", "technical_recent_work_migration"
  )
  migration$reduced_form$outcome_id <- ids[match(
    migration$reduced_form$outcome_id, paste0("outcome_", seq_along(ids))
  )]
  migration$reduced_form$p.value <- 0.20
  migration$reduced_form$p_holm_within_spec <- 0.40
  skilled <- migration$reduced_form$outcome_id == "skilled_recent_work_migration" &
    migration$reduced_form$adjustment_id == "state_main" &
    migration$reduced_form$construction_id == "nonzero_mean"
  migration$reduced_form$p.value[skilled] <- 0.003
  migration$reduced_form$p_holm_within_spec[skilled] <- 0.03
  migration$hindi_belt_skilled_migration <- data.frame(
    outcome_id = "skilled_recent_work_migration", estimate = 0.008, std.error = 0.006,
    p.value = 0.17, n = 178L, n_states = 11L, adjustment_id = "state_main",
    construction_id = "nonzero_mean", fixed_effect = "state", status = "estimated",
    stringsAsFactors = FALSE
  )

  migration_table <- appendix_migration_summary(migration)
  csv <- attr(migration_table, "csv_data")
  national <- csv[csv$sample == "All states", , drop = FALSE]
  hindi <- csv[csv$sample == "Hindi-belt states", , drop = FALSE]

  expect_true(all(national$adjustment_id == "state_main"))
  expect_true(all(national$construction_id == "nonzero_mean"))
  expect_true(all(national$p.value_for_stars == 0.40 | national$outcome_id == "skilled_recent_work_migration"))
  expect_equal(
    national$p.value_for_stars[national$outcome_id == "skilled_recent_work_migration"],
    0.03
  )
  expect_true(all(c(
    "outside_state_recent_work_migration", "skilled_recent_work_migration"
  ) %in% national$outcome_id))
  expect_equal(nrow(hindi), 1L)
  expect_equal(hindi$p.value_for_stars, hindi$p.value)

  expect_equal(nrow(appendix_d3_housing_assets(mechanism_fixture(8L))), 48L)
  expect_equal(nrow(appendix_d4_economic_census(mechanism_fixture(6L))), 36L)
  d5 <- appendix_d5_labor(mechanism_fixture(2L), mechanism_fixture(2L), mechanism_fixture(2L))
  expect_equal(nrow(attr(d5, "csv_data")), 36L)
})

test_that("Appendix D household and heterogeneity tables preserve bounded registered families", {
  household <- list(estimates = data.frame(
    outcome_id = rep(c("literacy_depth", "matriculate_access", "graduate_access", "female_graduate_access"), each = 2),
    predictor_id = rep(c("linguistic_opportunity", "schooling_exposure"), 4),
    predictor_role = rep(c("instrument", "treatment"), 4), estimand = "descriptive",
    estimate = 0.01, std_error_state_clustered = 0.005, p_value_state_clustered = 0.1,
    p_value_holm_predictor_family = 0.2, n = 355L, n_states = 34L, status = "estimated",
    stringsAsFactors = FALSE
  ))
  d6 <- appendix_d6_household_capacity(household)
  expect_equal(nrow(attr(d6, "csv_data")), 8L)

  social <- list(estimates = data.frame(
    sample = "all_states", social_group = c("Scheduled Tribe", "Scheduled Caste"),
    outcome = "enrollment_rate_0708", estimate = c(-1, 1), std_error_state_clustered = 1,
    p_value_state_clustered = 0.5, p_value_holm_family = 1, n_districts = 300L,
    stringsAsFactors = FALSE
  ))
  st <- list(estimates = data.frame(
    sample = "all_states", outcome_id = "emi_all_children", heterogeneity = "continuous_interaction",
    term = "ling_distance_nonzero_mean:st_share_10pp", estimate = 0.1,
    std_error_state_clustered = 0.1, p_value_state_clustered = 0.5,
    p_value_holm_family = 1, n_districts = 573L, status = "estimated", stringsAsFactors = FALSE
  ))
  d7 <- appendix_d7_social_heterogeneity(social, st)
  expect_equal(nrow(attr(d7, "csv_data")), 3L)
  expect_setequal(unique(attr(d7, "csv_data")$family), c("Social-group gap x distance", "ST concentration"))
})

test_that("Appendix D residual spatial table uses only preferred residual diagnostics", {
  spatial <- data.frame(
    estimand = c("linguistic_distance", "consumption_iv_residual", "consumption_first_stage_residual"),
    estimate = c(0.8, 0.01, -0.01), p.value = c(0.001, 0.4, 0.6), n = 500L,
    contiguity = "rook", weights_style = "W", status = "estimated", stringsAsFactors = FALSE
  )
  d9 <- appendix_d9_residual_spatial_diagnostics(spatial)
  csv <- attr(d9, "csv_data")
  expect_equal(nrow(csv), 2L)
  expect_setequal(csv$estimand, c("consumption_iv_residual", "consumption_first_stage_residual"))
})

test_that("Appendix D raw spatial panel uses the three canonical map diagnostics", {
  spatial <- data.frame(
    estimand = c(
      "linguistic_distance", "emie", "real_consumption_growth",
      "consumption_iv_residual", "consumption_first_stage_residual"
    ),
    estimate = c(0.886, 0.661, 0.307, 0.003, -0.010),
    p.value = c(1e-10, 1e-10, 1e-8, 0.435, 0.625),
    n = 573L, contiguity = "rook", weights_style = "W",
    status = "estimated", stringsAsFactors = FALSE
  )

  d8 <- appendix_d8_raw_spatial_geography(spatial)

  expect_identical(d8$panel, c("A", "B", "C"))
  expect_identical(
    d8$estimand,
    c("linguistic_distance", "emie", "real_consumption_growth")
  )
  expect_identical(
    d8$map_name,
    c("map_linguistic_distance", "map_emi_exposure", "map_consumption_growth")
  )
  expect_equal(d8$moran_i, c(0.886, 0.661, 0.307))
  expect_true(length(unique(d8$n)) == 1L)
  expect_true(length(unique(d8$contiguity)) == 1L)
  expect_true(length(unique(d8$weights_style)) == 1L)
})

test_that("Appendix D raw spatial panel reuses rendered maps rather than rebuilding geometry", {
  paths <- file.path(tempdir(), c(
    "map_linguistic_distance.png", "map_emi_exposure.png", "map_consumption_growth.png"
  ))
  file.create(paths)

  resolved <- appendix_d8_map_paths(
    paths,
    c("map_linguistic_distance", "map_emi_exposure", "map_consumption_growth")
  )

  expect_identical(normalizePath(resolved), normalizePath(paths))
  expect_error(
    appendix_d8_map_paths(paths[-1L], "map_linguistic_distance"),
    "exactly one rendered source map"
  )
})

test_that("Appendix D map labels add Moran context without changing the source map", {
  skip_if_not_installed("magick")
  path <- tempfile(fileext = ".png")
  magick::image_write(magick::image_blank(width = 120, height = 80, color = "white"), path)

  labeled <- appendix_d8_labeled_map_image(path, "A", "Linguistic distance", 0.886)
  info <- magick::image_info(labeled)

  expect_s3_class(labeled, "magick-image")
  expect_equal(info$width, 800L)
  expect_gt(info$height, round(80 * 800 / 120))
})
