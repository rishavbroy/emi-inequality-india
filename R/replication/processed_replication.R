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


processed_replication_shared_targets <- function() {
  c(
    "consumption_iv_dynamics",
    "schooling_consumption_bridge",
    "schooling_consumption_conversion",
    "alternative_distance_first_stage_base",
    "first_stage_absorption_diagnostics"
  )
}

compare_processed_replication_metadata <- function(
    full_meta,
    processed_meta,
    target_names = processed_replication_shared_targets()) {
  full_meta <- as.data.frame(full_meta, stringsAsFactors = FALSE)
  processed_meta <- as.data.frame(processed_meta, stringsAsFactors = FALSE)
  required <- c("name", "data")
  for (x in list(full = full_meta, processed = processed_meta)) {
    missing <- setdiff(required, names(x))
    if (length(missing)) {
      stop(
        "Target metadata is missing required fields: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  }

  select_hash <- function(meta, target) {
    rows <- which(as.character(meta$name) == target)
    if (!length(rows)) return(NA_character_)
    if (length(rows) != 1L) {
      stop("Expected exactly one metadata row for target: ", target, call. = FALSE)
    }
    as.character(meta$data[[rows]])
  }

  full_hash <- vapply(target_names, function(x) select_hash(full_meta, x), character(1))
  processed_hash <- vapply(target_names, function(x) select_hash(processed_meta, x), character(1))
  status <- ifelse(
    is.na(full_hash), "missing_full",
    ifelse(
      is.na(processed_hash), "missing_processed",
      ifelse(full_hash == processed_hash, "match", "hash_mismatch")
    )
  )
  data.frame(
    target = target_names,
    full_data_hash = unname(full_hash),
    processed_data_hash = unname(processed_hash),
    status = unname(status),
    stringsAsFactors = FALSE
  )
}

verify_processed_replication <- function(
    full_store = "_targets",
    processed_store = "_targets_processed",
    output_path = "outputs/replication/processed/verification.csv") {
  if (!targets::tar_exist_meta(store = full_store)) {
    stop("Full-source targets metadata is missing: ", full_store, call. = FALSE)
  }
  if (!targets::tar_exist_meta(store = processed_store)) {
    stop("Processed targets metadata is missing: ", processed_store, call. = FALSE)
  }

  report <- compare_processed_replication_metadata(
    targets::tar_meta(store = full_store, targets_only = TRUE),
    targets::tar_meta(store = processed_store, targets_only = TRUE)
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(report, output_path, row.names = FALSE, na = "")

  failures <- report$target[report$status != "match"]
  if (length(failures)) {
    stop(
      "Processed replication differs from the full-source build for: ",
      paste(failures, collapse = ", "),
      ". See ", output_path, ".",
      call. = FALSE
    )
  }
  normalizePath(output_path, mustWork = TRUE)
}
