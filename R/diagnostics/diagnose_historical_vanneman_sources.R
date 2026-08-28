# Source QA for the Vanneman-Barnes Indian District Database.

vanneman_historical_paths <- function(paths = build_paths()) {
  root <- path_project(paths, "data/raw/census_1961-91/vanneman_1961-91")
  c(
    panel4 = file.path(root, "panel4.data.gz"),
    dist81 = file.path(root, "dist81.data.gz"),
    dist91 = file.path(root, "dist91.data.gz"),
    codebook = file.path(root, "codebook/Codebook_ Indian district database.html")
  )
}

vanneman_identifier_rows <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman data file: ", path, call. = FALSE)
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  if (!length(lines) || any(nchar(lines) < 10L)) {
    stop("Vanneman fixed-width data contain malformed records: ", path, call. = FALSE)
  }
  data.frame(
    state_id = substr(lines, 1L, 2L),
    district_id = substr(lines, 3L, 4L),
    record_id = substr(lines, 5L, 7L),
    year = 1900L + suppressWarnings(as.integer(substr(lines, 8L, 9L))),
    version = suppressWarnings(as.integer(substr(lines, 10L, 10L))),
    stringsAsFactors = FALSE
  )
}

vanneman_documented_panel_version <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman codebook page: ", path, call. = FALSE)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  hit <- regmatches(
    text,
    regexec("Version number[^0-9]+2[^0-9]+cross-sectional data[^0-9]+([0-9]+)[^0-9]+panel 1961-91 data", text, ignore.case = TRUE)
  )[[1L]]
  if (length(hit) != 2L) {
    stop("Could not resolve the documented Vanneman panel version from the codebook.", call. = FALSE)
  }
  as.integer(hit[[2L]])
}

summarize_vanneman_historical_sources <- function(paths = build_paths()) {
  files <- vanneman_historical_paths(paths)
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop("Historical Vanneman source QA is missing files: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  documented_panel_version <- vanneman_documented_panel_version(files[["codebook"]])
  specs <- data.frame(
    source_id = c("panel4", "dist81", "dist91"),
    expected_years = c("1961;1971;1981;1991", "1981", "1991"),
    expected_version = c(documented_panel_version, 2L, 2L),
    stringsAsFactors = FALSE
  )
  rows <- lapply(specs$source_id, function(id) vanneman_identifier_rows(files[[id]]))
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    x <- rows[[i]]
    versions <- sort(unique(x$version[is.finite(x$version)]))
    years <- sort(unique(x$year[is.finite(x$year)]))
    expected_years <- sort(as.integer(strsplit(specs$expected_years[[i]], ";", fixed = TRUE)[[1L]]))
    expected_version <- as.integer(specs$expected_version[[i]])
    version_ok <- length(versions) == 1L && identical(versions, expected_version)
    years_ok <- identical(years, expected_years)
    noncontract_records <- sort(unique(x$record_id[is.finite(x$version) & x$version != expected_version]))
    status <- if (years_ok && version_ok) {
      "source_contract_verified"
    } else if (!years_ok) {
      "year_mismatch"
    } else if (length(versions) > 1L) {
      "mixed_record_versions"
    } else {
      "version_mismatch"
    }
    data.frame(
      source_id = specs$source_id[[i]],
      n_records = nrow(x),
      n_state_district_ids = length(unique(paste(x$state_id, x$district_id, sep = "__"))),
      n_record_types = length(unique(x$record_id)),
      observed_years = paste(years, collapse = ";"),
      observed_versions = paste(versions, collapse = ";"),
      documented_or_expected_version = expected_version,
      noncontract_record_ids = paste(noncontract_records, collapse = ";"),
      years_match_contract = years_ok,
      version_matches_contract = version_ok,
      eligible_for_baseline_values = years_ok && version_ok,
      status = status,
      stringsAsFactors = FALSE
    )
  }))
}

save_vanneman_historical_source_qa <- function(
    x, path = "outputs/diagnostics/extended/instrument_relevance/vanneman_historical_source_qa.csv") {
  write_diagnostic_csv(x, path)
}
