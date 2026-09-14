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

test_that("migration summary uses registered inference for significance markers", {
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

  expect_true(all(national$p.value_for_stars == 0.40 | national$outcome_id == "skilled_recent_work_migration"))
  expect_equal(
    national$p.value_for_stars[national$outcome_id == "skilled_recent_work_migration"],
    0.03
  )
  expect_equal(hindi$p.value_for_stars, hindi$p.value)

})
