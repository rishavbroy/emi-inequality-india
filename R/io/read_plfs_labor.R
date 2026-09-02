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
    "annual_usual_status_source", "multiplier_field", "quarter_field", "visit_field",
    "sector_field", "segment_field", "nss_count_field", "nsc_count_field",
    "annual_quarters_field", "state_field", "district_field", "nss_region_field",
    "stratum_field", "sub_stratum_field",
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
    "multiplier_field", "quarter_field", "visit_field", "sector_field",
    "segment_field", "nss_count_field", "nsc_count_field", "annual_quarters_field",
    "state_field", "district_field", "nss_region_field", "stratum_field",
    "sub_stratum_field", "sub_sample_field", "fsu_field",
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
  ids <- c("plfs1718_nesstar", "plfs1718_layout", "plfs1718_ddi", "plfs1718_readme")
  rows <- rows[rows$file_id %in% ids, , drop = FALSE]
  if (!setequal(rows$file_id, ids) || nrow(rows) != length(ids)) {
    stop("PLFS 2017-18 source manifest must declare one Nesstar, layout, DDI, and README file.", call. = FALSE)
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
    readme_file_id = "plfs1718_readme",
    readme_bytes = rows$size_bytes[rows$file_id == "plfs1718_readme"],
    ddi_file_name = ddi$file_name[[1L]],
    annual_usual_status_rows = ddi$case_count[[1L]],
    stringsAsFactors = FALSE
  )
}


plfs_2017_18_materialized_columns <- function(contract = plfs_2017_18_contract()) {
  fields <- c(
    "multiplier_field", "quarter_field", "visit_field", "sector_field",
    "segment_field", "nss_count_field", "nsc_count_field", "annual_quarters_field",
    "state_field", "district_field", "nss_region_field", "stratum_field",
    "sub_stratum_field", "sub_sample_field", "fsu_field",
    "second_stage_stratum_field", "household_field", "person_field", "age_field",
    "principal_status_field", "subsidiary_status_field"
  )
  unique(unname(unlist(contract[fields], use.names = FALSE)))
}

plfs_2017_18_annual_weight <- function(multiplier, nss, nsc, n_quarters) {
  multiplier <- num(multiplier)
  nss <- num(nss)
  nsc <- num(nsc)
  n_quarters <- num(n_quarters)
  valid <- is.finite(multiplier) & multiplier > 0 &
    is.finite(nss) & nss > 0 & is.finite(nsc) & nsc > 0 &
    is.finite(n_quarters) & n_quarters > 0
  if (!all(valid)) stop("PLFS annual-weight inputs must be finite and positive.", call. = FALSE)
  combined_divisor <- ifelse(nss == nsc, 100, 200)
  multiplier / combined_divisor / n_quarters
}

read_plfs_2017_18_materialized_persons <- function(
    materialization, contract = plfs_2017_18_contract(),
    root = Sys.getenv("EMI_PROJECT_ROOT", unset = ".")) {
  if (!isTRUE(materialization$ready) || !identical(materialization$source_id, "plfs_2017_18")) {
    stop("PLFS 2017-18 F1 must be fully materialized before canonical ingestion.", call. = FALSE)
  }
  blocks <- safe_df(materialization$blocks)
  row <- blocks[blocks$block_id == "F1", , drop = FALSE]
  if (nrow(row) != 1L) stop("PLFS 2017-18 materialization must contain exactly one F1 block.", call. = FALSE)
  path <- file.path(root, plain_chr(row$relative_path[[1L]]))
  columns <- plfs_2017_18_materialized_columns(contract)
  raw <- safe_df(data.table::fread(
    path, select = columns, colClasses = "character",
    na.strings = c("", "NA"), showProgress = FALSE
  ))
  missing <- setdiff(columns, names(raw))
  if (length(missing)) stop("Materialized PLFS F1 is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (nrow(raw) != contract$first_visit_person_rows[[1L]]) {
    stop("Materialized PLFS F1 row count differs from the registered source contract.", call. = FALSE)
  }

  value <- function(field) raw[[plain_chr(contract[[field]][[1L]])]]
  quarter <- plain_chr(value("quarter_field"))
  visit <- plain_chr(value("visit_field"))
  state <- normalize_census_code(num(value("state_field")), 2L)
  district <- normalize_census_code(num(value("district_field")), 2L)
  region <- normalize_census_code(num(value("nss_region_field")), 3L)
  if (!all(quarter %in% paste0("Q", 1:4)) || !all(visit %in% "V1")) {
    stop("PLFS annual usual-status F1 must contain first visits from quarters Q1-Q4 only.", call. = FALSE)
  }
  if (anyNA(state) || anyNA(district) || anyNA(region) || any(substr(region, 1L, 2L) != state)) {
    stop("PLFS F1 has internally inconsistent state, NSS-region, or district geography.", call. = FALSE)
  }

  segment <- num(value("segment_field"))
  second_stage <- num(value("second_stage_stratum_field"))
  household <- num(value("household_field"))
  person <- num(value("person_field"))
  fsu <- num(value("fsu_field"))
  person_key <- paste(
    quarter, visit, state, num(value("sector_field")), fsu, segment,
    second_stage, household, person, sep = "__"
  )
  survey_weight <- plfs_2017_18_annual_weight(
    value("multiplier_field"), value("nss_count_field"), value("nsc_count_field"),
    value("annual_quarters_field")
  )
  out <- data.frame(
    person_key = person_key,
    state_code = state,
    district_code = district,
    sector = num(value("sector_field")),
    sub_round = match(quarter, paste0("Q", 1:4)),
    sub_sample = num(value("sub_sample_field")),
    nss_region = region,
    stratum = num(value("stratum_field")),
    sub_stratum = normalize_census_code(num(value("sub_stratum_field")), 2L),
    fsu = fsu,
    second_stage_stratum = second_stage,
    household_no = household,
    person_no = person,
    survey_weight = survey_weight,
    age = num(value("age_field")),
    usual_principal_status = num(value("principal_status_field")),
    usual_subsidiary_status = num(value("subsidiary_status_field")),
    stringsAsFactors = FALSE
  )
  design <- c(
    "person_key", "state_code", "district_code", "sector", "sub_round", "sub_sample",
    "nss_region", "stratum", "sub_stratum", "fsu", "second_stage_stratum",
    "household_no", "person_no", "survey_weight", "age", "usual_principal_status"
  )
  if (any(!stats::complete.cases(out[design])) || anyDuplicated(out$person_key)) {
    stop("PLFS F1 canonical person/design fields must be complete and unique.", call. = FALSE)
  }
  if (any(!is.finite(out$survey_weight) | out$survey_weight <= 0)) {
    stop("PLFS annual survey weights must be finite and positive.", call. = FALSE)
  }
  employed <- nss_labor_employed_status_codes()
  subsidiary <- out$usual_subsidiary_status
  if (any(!is.na(subsidiary) & !(subsidiary %in% employed))) {
    stop("PLFS F1 contains an unexpected non-employment subsidiary status code.", call. = FALSE)
  }
  out
}
