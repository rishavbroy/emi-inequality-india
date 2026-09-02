# Source contracts for PLFS labor waves.
# Annual usual-status ingestion uses the official first-visit person universe.

read_plfs_labor_contracts <- function(path = "data/metadata/plfs_labor_contracts.csv") {
  root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
  resolved <- if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) path else file.path(root, path)
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

plfs_2017_18_ddi_requirements <- function(contract = plfs_2017_18_contract()) {
  fields <- c(
    "multiplier_field", "state_field", "district_field", "nss_region_field",
    "stratum_field", "sub_stratum_field", "sub_sample_field", "fsu_field",
    "second_stage_stratum_field", "household_field", "person_field", "age_field",
    "principal_status_field", "subsidiary_status_field"
  )
  list(F1 = unique(unname(unlist(contract[fields], use.names = FALSE))))
}

read_plfs_2017_18_ddi_contract <- function(path, contract = plfs_2017_18_contract()) {
  out <- read_labor_ddi_contract(
    path, plfs_2017_18_ddi_requirements(contract), "PLFS 2017-18"
  )
  if (out$file_name[[1L]] != contract$first_visit_person_file[[1L]] ||
      out$case_count[[1L]] != contract$first_visit_person_rows[[1L]]) {
    stop(
      "PLFS 2017-18 DDI first-visit person file differs from the registered source contract.",
      call. = FALSE
    )
  }
  out
}

plfs_2017_18_layout_expectations <- function() {
  data.frame(
    full_name = c(
      "District Code", "NSS-Region", "FSU", "Second Stage Stratum No.",
      "Sample Household Number", "Person Serial No.", "Age", "Status Code",
      "Status Code", "Sub-sample wise Multiplier"
    ),
    start = c(15L, 17L, 29L, 35L, 36L, 38L, 42L, 61L, 80L, 309L),
    end = c(16L, 19L, 33L, 35L, 37L, 39L, 44L, 62L, 81L, 318L),
    stringsAsFactors = FALSE
  )
}

validate_plfs_2017_18_layout <- function(layout_path) {
  raw <- readxl::read_excel(
    layout_path, sheet = "Data Layout", col_names = FALSE, .name_repair = "minimal"
  )
  if (ncol(raw) < 7L) stop("PLFS 2017-18 layout has fewer than seven columns.", call. = FALSE)
  x <- data.frame(
    full_name = trimws(plain_chr(raw[[2L]])),
    start = suppressWarnings(as.integer(raw[[6L]])),
    end = suppressWarnings(as.integer(raw[[7L]])),
    stringsAsFactors = FALSE
  )
  expected <- plfs_2017_18_layout_expectations()
  matches <- vapply(seq_len(nrow(expected)), function(i) {
    row <- expected[i, , drop = FALSE]
    any(
      x$full_name == row$full_name[[1L]] &
        x$start == row$start[[1L]] & x$end == row$end[[1L]],
      na.rm = TRUE
    )
  }, logical(1))
  if (!all(matches)) {
    missing <- paste0(
      expected$full_name[!matches], " [bytes ", expected$start[!matches], "-", expected$end[!matches], "]"
    )
    stop(
      "PLFS 2017-18 layout does not match reviewed person/design fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

inspect_plfs_2017_18_source_package <- function(
    paths = build_paths(), source_rows = NULL,
    layout_validator = validate_plfs_2017_18_layout,
    ddi_validator = read_plfs_2017_18_ddi_contract) {
  rows <- if (is.null(source_rows)) {
    require_manifest_files(paths, source_id = "plfs_labor_market", required_only = FALSE)
  } else {
    safe_df(source_rows)
  }
  required <- c("file_id", "expected_size_bytes", "absolute_path")
  if (!all(required %in% names(rows))) {
    stop("PLFS 2017-18 source manifest lacks file, size, or path fields.", call. = FALSE)
  }
  ids <- c("plfs1718_nesstar", "plfs1718_layout", "plfs1718_ddi")
  rows <- rows[rows$file_id %in% ids, , drop = FALSE]
  if (!setequal(rows$file_id, ids) || nrow(rows) != length(ids)) {
    stop("PLFS 2017-18 source manifest must declare one Nesstar, layout, and DDI file.", call. = FALSE)
  }
  rows$expected_size_bytes <- num(rows$expected_size_bytes)
  rows$size_bytes <- as.numeric(file.info(rows$absolute_path)$size)
  if (any(!is.finite(rows$expected_size_bytes)) ||
      any(rows$size_bytes != rows$expected_size_bytes)) {
    stop("PLFS 2017-18 source package byte sizes do not match the reviewed local archive.", call. = FALSE)
  }
  layout_path <- rows$absolute_path[rows$file_id == "plfs1718_layout"][[1L]]
  ddi_path <- rows$absolute_path[rows$file_id == "plfs1718_ddi"][[1L]]
  layout_validator(layout_path)
  ddi <- ddi_validator(ddi_path)
  data.frame(
    source_id = "plfs_2017_18",
    status = "ready_for_materialization",
    nesstar_file_id = "plfs1718_nesstar",
    nesstar_bytes = rows$size_bytes[rows$file_id == "plfs1718_nesstar"],
    layout_file_id = "plfs1718_layout",
    layout_bytes = rows$size_bytes[rows$file_id == "plfs1718_layout"],
    ddi_file_id = "plfs1718_ddi",
    ddi_bytes = rows$size_bytes[rows$file_id == "plfs1718_ddi"],
    ddi_file_name = ddi$file_name[[1L]],
    annual_usual_status_rows = ddi$case_count[[1L]],
    stringsAsFactors = FALSE
  )
}
