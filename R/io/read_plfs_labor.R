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

plfs_2017_18_ddi_path <- function(nesstar_path) {
  file.path(dirname(nesstar_path), "DDI-IND-CSO-PLFS-2017-18.xml")
}

plfs_2017_18_layout_expectations <- function() {
  data.frame(
    full_name = c(
      "District Code", "NSS-Region", "FSU", "Second Stage Stratum No.",
      "Sample Household Number", "Person Serial No.", "Age", "Status Code",
      "Status Code", "Sub-sample wise Multiplier"
    ),
    block = c("1", "1", "1", "1", "1", "4", "4", "5.1", "5.2", "Generated"),
    item = c("4", "4", "1", "14", "15", "1", "6", "3", "3", NA_character_),
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
    block = trimws(plain_chr(raw[[3L]])),
    item = trimws(plain_chr(raw[[4L]])),
    start = suppressWarnings(as.integer(raw[[6L]])),
    end = suppressWarnings(as.integer(raw[[7L]])),
    stringsAsFactors = FALSE
  )
  expected <- plfs_2017_18_layout_expectations()
  matches <- vapply(seq_len(nrow(expected)), function(i) {
    row <- expected[i, , drop = FALSE]
    same_item <- if (is.na(row$item[[1L]])) {
      is.na(x$item) | !nzchar(x$item)
    } else {
      x$item == row$item[[1L]]
    }
    any(
      x$full_name == row$full_name[[1L]] &
        x$block == row$block[[1L]] & same_item &
        x$start == row$start[[1L]] & x$end == row$end[[1L]],
      na.rm = TRUE
    )
  }, logical(1))
  if (!all(matches)) {
    missing <- paste0(
      expected$full_name[!matches], " [block ", expected$block[!matches], "]"
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
    layout_validator = validate_plfs_2017_18_layout) {
  rows <- if (is.null(source_rows)) {
    require_manifest_files(paths, source_id = "plfs_labor_market", required_only = FALSE)
  } else {
    safe_df(source_rows)
  }
  required <- c("file_id", "expected_size_bytes", "absolute_path")
  if (!all(required %in% names(rows))) {
    stop("PLFS 2017-18 source manifest lacks file, size, or path fields.", call. = FALSE)
  }
  rows <- rows[rows$file_id %in% c("plfs1718_nesstar", "plfs1718_layout"), , drop = FALSE]
  if (!setequal(rows$file_id, c("plfs1718_nesstar", "plfs1718_layout")) || nrow(rows) != 2L) {
    stop("PLFS 2017-18 source manifest must declare one Nesstar and one layout file.", call. = FALSE)
  }
  rows$expected_size_bytes <- num(rows$expected_size_bytes)
  rows$size_bytes <- as.numeric(file.info(rows$absolute_path)$size)
  if (any(!is.finite(rows$expected_size_bytes)) ||
      any(rows$size_bytes != rows$expected_size_bytes)) {
    stop("PLFS 2017-18 source package byte sizes do not match the reviewed local archive.", call. = FALSE)
  }
  nesstar_path <- rows$absolute_path[rows$file_id == "plfs1718_nesstar"][[1L]]
  layout_path <- rows$absolute_path[rows$file_id == "plfs1718_layout"][[1L]]
  layout_validator(layout_path)
  ddi <- plfs_2017_18_ddi_path(nesstar_path)
  data.frame(
    source_id = "plfs_2017_18",
    status = if (file.exists(ddi)) "ddi_present_unregistered" else "blocked_missing_ddi",
    nesstar_file_id = "plfs1718_nesstar",
    nesstar_bytes = rows$size_bytes[rows$file_id == "plfs1718_nesstar"],
    layout_file_id = "plfs1718_layout",
    layout_bytes = rows$size_bytes[rows$file_id == "plfs1718_layout"],
    ddi_expected_name = basename(ddi),
    ddi_exists = file.exists(ddi),
    annual_usual_status_rows = plfs_2017_18_contract()$first_visit_person_rows[[1L]],
    stringsAsFactors = FALSE
  )
}
