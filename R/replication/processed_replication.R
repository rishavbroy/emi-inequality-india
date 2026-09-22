# Aggregate-data replication helpers.
#
# This tier deliberately excludes individual-level NSS selection data. It
# reproduces district-level welfare, first-stage, and IV evidence from tracked
# processed inputs whose rows are district-level aggregates.

processed_replication_panel_path <- function(paths = build_paths()) {
  path_project(paths, "data", "processed", "district_panel_emi_consumption_2001_2007_2017_2020.csv")
}

processed_replication_welfare_path <- function(paths = build_paths()) {
  path_project(paths, "data", "processed", "consumption_district_welfare.csv")
}

read_processed_replication_csv <- function(path) {
  if (!file.exists(path)) stop("Processed replication input is missing: ", path, call. = FALSE)
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

read_processed_replication_panel <- function(path, control_registry = NULL) {
  x <- read_processed_replication_csv(path)
  required <- c(
    "target_unit_2001", "state_code_2001", "region",
    "emi_exposure_all_children_0708", "ling_distance_nonzero_mean",
    census_2001_main_controls(control_registry)
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Processed district panel is missing replication fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(plain_chr(x$target_unit_2001))) {
    stop("Processed district panel must contain one row per Census-2001 district.", call. = FALSE)
  }
  x
}

read_processed_replication_welfare <- function(path) {
  x <- read_processed_replication_csv(path)
  required <- c(
    "district_2001", "round_id", "outcome_id", "estimate",
    "preferred_eligible", "analysis_eligible"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Processed district welfare file is missing replication fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  key <- paste(x$district_2001, x$round_id, x$outcome_id, sep = "|")
  if (anyDuplicated(key)) {
    stop("Processed district welfare file contains duplicate district-round-outcome rows.", call. = FALSE)
  }
  x
}

save_processed_replication_results <- function(
    dynamics,
    bridge,
    conversion,
    alternative_first_stages,
    first_stage_absorption,
    directory = "outputs/replication/processed") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!inherits(alternative_first_stages, "emi_alternative_distance_first_stages")) {
    stop("Processed replication requires alternative-distance first-stage results.", call. = FALSE)
  }
  files <- c(
    save_consumption_iv_dynamics(dynamics, directory = directory),
    save_schooling_consumption_bridge(bridge, dir = directory),
    save_schooling_consumption_conversion(conversion, dir = directory),
    write_diagnostic_csv(
      safe_df(alternative_first_stages$summary),
      file.path(directory, "alternative_distance_first_stage_summary.csv")
    ),
    write_diagnostic_csv(
      safe_df(alternative_first_stages$coefficients),
      file.path(directory, "alternative_distance_first_stage_coefficients.csv")
    ),
    unname(save_first_stage_absorption_diagnostics(first_stage_absorption, dir = directory))
  )
  unique(unname(files))
}
