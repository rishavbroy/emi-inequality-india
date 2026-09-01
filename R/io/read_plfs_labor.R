# Source contracts for PLFS labor waves.
# Canonical person ingestion is activated only after local raw files are inspected;
# this module freezes the official catalog structure and analytical role first.

read_plfs_labor_contracts <- function(path = "data/metadata/plfs_labor_contracts.csv") {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  resolved <- if (grepl("^(/|[A-Za-z]:[/\\])", path)) path else file.path(root, path)
  x <- utils::read.csv(resolved, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "wave_id", "reference_id", "field_period", "temporal_role",
    "first_visit_person_file", "first_visit_person_rows",
    "first_visit_household_file", "first_visit_household_rows",
    "revisit_person_file", "revisit_person_rows",
    "revisit_household_file", "revisit_household_rows",
    "annual_usual_status_source", "multiplier_field", "state_field",
    "district_field", "nss_region_field", "stratum_field", "sub_stratum_field",
    "sub_sample_field", "fsu_field", "second_stage_stratum_field",
    "household_field", "person_field", "age_field", "principal_status_field",
    "subsidiary_status_field"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("PLFS labor contract is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x) || anyDuplicated(x$wave_id)) {
    stop("PLFS labor contract must contain unique wave identifiers.", call. = FALSE)
  }
  count_fields <- c(
    "first_visit_person_rows", "first_visit_household_rows",
    "revisit_person_rows", "revisit_household_rows"
  )
  for (nm in count_fields) x[[nm]] <- as.integer(x[[nm]])
  if (any(!is.finite(as.matrix(x[count_fields])) | as.matrix(x[count_fields]) <= 0)) {
    stop("PLFS labor contract contains invalid official case counts.", call. = FALSE)
  }
  if (any(x$annual_usual_status_source != "first_visit_person")) {
    stop("PLFS annual usual-status contracts must use the first-visit person universe.", call. = FALSE)
  }
  if (any(!nzchar(trimws(x$multiplier_field))) ||
      any(!nzchar(trimws(x$principal_status_field))) ||
      any(!nzchar(trimws(x$subsidiary_status_field)))) {
    stop("PLFS labor contract contains blank analytical field declarations.", call. = FALSE)
  }
  x
}

plfs_2017_18_contract <- function(path = "data/metadata/plfs_labor_contracts.csv") {
  x <- read_plfs_labor_contracts(path)
  out <- x[x$wave_id == "plfs_2017_18", , drop = FALSE]
  if (nrow(out) != 1L) stop("PLFS 2017-18 contract must contain exactly one row.", call. = FALSE)
  out
}
