test_that("processed replication readers preserve district-level analysis inputs", {
  panel_path <- tempfile(fileext = ".csv")
  controls <- census_2001_main_controls()
  panel <- data.frame(
    target_unit_2001 = c("pc2001__01__01", "pc2001__01__02"),
    state_code_2001 = c("01", "01"),
    region = c("North", "North"),
    emi_exposure_all_children_0708 = c(0.1, 0.2),
    ling_distance_nonzero_mean = c(2, 3),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (nm in controls) panel[[nm]] <- c(0.1, 0.2)
  utils::write.csv(panel, panel_path, row.names = FALSE, na = "")

  welfare_path <- tempfile(fileext = ".csv")
  welfare <- data.frame(
    district_2001 = c("pc2001__01__01", "pc2001__01__02"),
    round_id = c("hces_2022_23", "hces_2022_23"),
    outcome_id = c("real_mean_mpce", "real_mean_mpce"),
    estimate = c(100, 110),
    preferred_eligible = TRUE,
    analysis_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  utils::write.csv(welfare, welfare_path, row.names = FALSE, na = "")

  panel_read <- read_processed_replication_panel(panel_path)
  welfare_read <- read_processed_replication_welfare(welfare_path)

  expect_identical(nrow(panel_read), 2L)
  expect_identical(nrow(welfare_read), 2L)
  expect_setequal(controls, intersect(controls, names(panel_read)))
})

test_that("processed replication rejects duplicate empirical keys", {
  panel_path <- tempfile(fileext = ".csv")
  controls <- census_2001_main_controls()
  panel <- data.frame(
    target_unit_2001 = rep("pc2001__01__01", 2),
    state_code_2001 = "01",
    region = "North",
    emi_exposure_all_children_0708 = 0.1,
    ling_distance_nonzero_mean = 2,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (nm in controls) panel[[nm]] <- 0.1
  utils::write.csv(panel, panel_path, row.names = FALSE, na = "")
  expect_error(read_processed_replication_panel(panel_path), "one row per Census-2001 district")

  welfare_path <- tempfile(fileext = ".csv")
  welfare <- data.frame(
    district_2001 = rep("pc2001__01__01", 2),
    round_id = rep("hces_2022_23", 2),
    outcome_id = rep("real_mean_mpce", 2),
    estimate = c(100, 101),
    preferred_eligible = TRUE,
    analysis_eligible = TRUE,
    stringsAsFactors = FALSE
  )
  utils::write.csv(welfare, welfare_path, row.names = FALSE, na = "")
  expect_error(read_processed_replication_welfare(welfare_path), "duplicate district-round-outcome")
})

test_that("processed replication graph is independent of raw-data and selection targets", {
  skip_if_not_installed("targets")
  script <- repo_file("_targets_processed.R")
  root <- dirname(script)
  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  manifest <- targets::tar_manifest(
    script = basename(script),
    fields = tidyselect::any_of(c("name", "command", "format")),
    callr_function = NULL,
    envir = new.env(parent = globalenv())
  )
  names <- plain_chr(manifest$name)

  expect_true(all(c(
    "processed_district_panel_file",
    "processed_consumption_welfare_file",
    "consumption_iv_dynamics",
    "schooling_consumption_bridge",
    "schooling_consumption_conversion",
    "alternative_distance_first_stage_base",
    "first_stage_absorption_diagnostics",
    "processed_replication_files"
  ) %in% names))
  expect_false(any(c("raw_data_preflight", "selection_data", "selection_model", "ame_results") %in% names))
  expect_identical(manifest$format[match("processed_district_panel_file", names)], "file")
  expect_identical(manifest$format[match("processed_consumption_welfare_file", names)], "file")
})


test_that("processed replication file target returns only existing paths", {
  dir <- tempfile("processed-replication-")
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  frame <- data.frame(id = "x", estimate = 1, stringsAsFactors = FALSE)
  dynamics <- list(summary = frame, anderson_rubin_grid = frame)
  bridge <- list(
    treatments = frame, welfare = frame,
    specifications = frame, estimates = frame
  )
  conversion <- list(specifications = frame, estimates = frame)
  alternative <- structure(
    list(summary = frame, coefficients = frame),
    class = "emi_alternative_distance_first_stages"
  )
  absorption <- structure(
    list(
      summary = frame, semantic_summary = frame, registry = frame,
      aliases = frame, common_support = frame, state_residual_ranges = frame,
      state_deletion = frame, district_influence = frame, vif = frame
    ),
    class = "emi_first_stage_absorption"
  )

  files <- save_processed_replication_results(
    dynamics, bridge, conversion, alternative, absorption, directory = dir
  )

  expect_type(files, "character")
  expect_false(anyDuplicated(files))
  expect_true(all(file.exists(files)))
})

test_that("processed replication verification compares reported empirical components", {
  targets <- processed_replication_shared_targets()
  template <- list(
    summary = data.frame(specification_id = "main", estimate = 1),
    estimates = data.frame(specification_id = "main", estimate = 2)
  )
  full <- setNames(rep(list(template), length(targets)), targets)
  processed <- full
  processed[[targets[[1L]]]]$summary$estimate <- 1L

  matched <- compare_processed_replication_values(full, processed, targets)
  expect_true(all(matched$status == "match"))

  processed[[targets[[2L]]]]$estimates$estimate <- 2.1
  mismatched <- compare_processed_replication_values(full, processed, targets)
  expect_identical(mismatched$status[[2L]], "value_mismatch")
  expect_match(mismatched$detail[[2L]], "difference|Mean relative difference")

  missing <- compare_processed_replication_values(
    full,
    processed[setdiff(names(processed), targets[[3L]])],
    targets
  )
  expect_identical(missing$status[[3L]], "missing_processed")
})
