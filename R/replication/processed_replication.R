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
    save_first_stage_absorption_diagnostics(first_stage_absorption, dir = directory)$path
  )
  unique(unname(as.character(files)))
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

canonicalize_processed_replication_frame <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  rownames(x) <- NULL
  for (name in names(x)) {
    if (is.factor(x[[name]])) x[[name]] <- as.character(x[[name]])
  }
  x
}

processed_replication_components <- function(value, target) {
  if (!is.list(value)) {
    stop("Processed replication target is not a list: ", target, call. = FALSE)
  }
  keep <- vapply(value, is.data.frame, logical(1))
  components <- value[keep]
  if (!length(components)) {
    stop("Processed replication target has no tabular result components: ", target, call. = FALSE)
  }
  lapply(components, canonicalize_processed_replication_frame)
}

compare_processed_replication_values <- function(
    full_values,
    processed_values,
    target_names = processed_replication_shared_targets(),
    tolerance = sqrt(.Machine$double.eps)) {
  compare_one <- function(target) {
    if (!target %in% names(full_values)) {
      return(c(status = "missing_full", detail = "target is absent from the full store"))
    }
    if (!target %in% names(processed_values)) {
      return(c(status = "missing_processed", detail = "target is absent from the processed store"))
    }

    full <- processed_replication_components(full_values[[target]], target)
    processed <- processed_replication_components(processed_values[[target]], target)
    if (!identical(names(full), names(processed))) {
      return(c(status = "component_mismatch", detail = "reported component names differ"))
    }

    for (component in names(full)) {
      full_frame <- full[[component]]
      processed_frame <- processed[[component]]
      if (!identical(names(full_frame), names(processed_frame))) {
        return(c(
          status = "value_mismatch",
          detail = paste0(component, ": column names differ")
        ))
      }
      if (!identical(dim(full_frame), dim(processed_frame))) {
        return(c(
          status = "value_mismatch",
          detail = paste0(component, ": dimensions differ")
        ))
      }
      for (column in names(full_frame)) {
        comparison <- all.equal(
          full_frame[[column]], processed_frame[[column]],
          tolerance = tolerance, check.attributes = FALSE
        )
        if (!isTRUE(comparison)) {
          detail <- paste(as.character(comparison), collapse = "; ")
          return(c(
            status = "value_mismatch",
            detail = substr(
              paste0(component, "$", column, ": ", detail), 1L, 1000L
            )
          ))
        }
      }
    }
    c(status = "match", detail = "")
  }

  compared <- lapply(target_names, compare_one)
  data.frame(
    target = target_names,
    status = vapply(compared, `[[`, character(1), "status"),
    detail = vapply(compared, `[[`, character(1), "detail"),
    stringsAsFactors = FALSE
  )
}

read_processed_replication_targets <- function(
    store, target_names = processed_replication_shared_targets()) {
  meta <- targets::tar_meta(store = store, fields = "name", targets_only = TRUE)
  available <- intersect(target_names, as.character(meta$name))
  values <- lapply(available, targets::tar_read_raw, store = store)
  names(values) <- available
  values
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

  report <- compare_processed_replication_values(
    read_processed_replication_targets(full_store),
    read_processed_replication_targets(processed_store)
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
