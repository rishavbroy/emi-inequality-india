# Source QA for the Vanneman-Barnes Indian District Database.

vanneman_historical_paths <- function(paths = build_paths()) {
  root <- path_project(paths, "data/raw/census_1961-91/vanneman_1961-91")
  c(
    panel4 = file.path(root, "data_archived/panel4.data.gz"),
    dist81 = file.path(root, "data_archived/dist81.data.gz"),
    dist91 = file.path(root, "data_archived/dist91.data.gz"),
    codebook = file.path(root, "codebook/Codebook_ Indian district database.html"),
    variables_codebook = file.path(root, "codebook/Variables_ Indian district codebook.html"),
    education_codebook = file.path(root, "codebook/Education and literacy_ Indian district codebook.html"),
    panel4_sas = file.path(root, "sas_commands_archived/panel4.sas"),
    dist81_sas = file.path(root, "sas_commands_archived/dist81.sas"),
    dist91_sas = file.path(root, "sas_commands_archived/dist91.sas"),
    archive_checksums = path_project(paths, "data/metadata/vanneman_archive_2013_checksums.csv")
  )
}


read_vanneman_archive_checksums <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman archive checksum registry: ", path, call. = FALSE)
  x <- read.csv(path, stringsAsFactors = FALSE)
  required <- c("relative_path", "size_bytes", "md5", "sha256", "archive_snapshot")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Vanneman archive checksum registry lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(x$relative_path)) stop("Vanneman archive checksum registry has duplicate paths.", call. = FALSE)
  x
}

vanneman_archive_file_verified <- function(path, relative_path, checksums) {
  row <- checksums[checksums$relative_path == relative_path, , drop = FALSE]
  if (nrow(row) != 1L) return(FALSE)
  size_ok <- isTRUE(as.numeric(file.info(path)$size) == as.numeric(row$size_bytes[[1L]]))
  md5_ok <- identical(unname(tools::md5sum(path)), as.character(row$md5[[1L]]))
  size_ok && md5_ok
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


vanneman_documented_record_ids <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman codebook page: ", path, call. = FALSE)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  hits <- regmatches(
    text,
    gregexpr(
      "<div[^>]*align=[\"']?center[\"']?[^>]*>[[:space:]]*[0-9]{3}[[:space:]]*</div>",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  )[[1L]]
  if (!length(hits) || identical(hits, "-1")) return(character())
  sort(unique(sub(".*?([0-9]{3}).*", "\\1", hits, perl = TRUE)))
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

vanneman_sas_record_ids <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman SAS reader: ", path, call. = FALSE)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  hits <- regmatches(
    text,
    gregexpr(
      "/\\*[[:space:]]*(?:Record[[:space:]]+ID[[:space:]]*#[[:space:]]*)?[0-9]{3}",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  )[[1L]]
  if (!length(hits) || identical(hits, "-1")) return(character())
  sort(unique(sub(".*?([0-9]{3})$", "\\1", hits, perl = TRUE)))
}

vanneman_sas_reader_contract <- function(path, source_id, exception_record_ids = character()) {
  if (!file.exists(path)) stop("Missing Vanneman SAS reader: ", path, call. = FALSE)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  squashed <- gsub("[[:space:]]+", " ", text)
  source_file <- paste0(source_id, ".data.gz")
  input_fields <- c(
    "stateid[^0-9]+1-2",
    "distid[^0-9]+3-4",
    "(?:record|rec000)[^0-9]+5-7",
    "year[^0-9]+8-9",
    "version[^0-9]+10"
  )
  fields_declared <- all(vapply(
    input_fields,
    function(pattern) grepl(pattern, squashed, ignore.case = TRUE, perl = TRUE),
    logical(1)
  ))
  record_ids <- vanneman_sas_record_ids(path)
  exceptions_covered <- !length(exception_record_ids) || all(exception_record_ids %in% record_ids)
  data.frame(
    source_specific_reader_targets_file = grepl(source_file, text, fixed = TRUE),
    source_specific_identifier_layout_verified = fields_declared,
    source_specific_reader_covers_version_exceptions = exceptions_covered,
    parser_contract_verified =
      grepl(source_file, text, fixed = TRUE) && fields_declared && exceptions_covered,
    stringsAsFactors = FALSE
  )
}

summarize_vanneman_historical_sources <- function(paths = build_paths()) {
  files <- vanneman_historical_paths(paths)
  missing <- files[!file.exists(files)]
  if (length(missing)) {
    stop("Historical Vanneman source QA is missing files: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  archive_checksums <- read_vanneman_archive_checksums(files[["archive_checksums"]])
  documented_panel_version <- vanneman_documented_panel_version(files[["codebook"]])
  documented_records <- unique(c(
    vanneman_documented_record_ids(files[["variables_codebook"]]),
    vanneman_documented_record_ids(files[["education_codebook"]])
  ))
  specs <- data.frame(
    source_id = c("panel4", "dist81", "dist91"),
    expected_years = c("1961;1971;1981;1991", "1981", "1991"),
    generic_codebook_version = c(documented_panel_version, 2L, 2L),
    data_relative_path = paste0("data_archived/", c("panel4", "dist81", "dist91"), ".data.gz"),
    sas_relative_path = paste0("sas_commands_archived/", c("panel4", "dist81", "dist91"), ".sas"),
    stringsAsFactors = FALSE
  )
  rows <- lapply(specs$source_id, function(id) vanneman_identifier_rows(files[[id]]))
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    x <- rows[[i]]
    versions <- sort(unique(x$version[is.finite(x$version)]))
    years <- sort(unique(x$year[is.finite(x$year)]))
    expected_years <- sort(as.integer(strsplit(specs$expected_years[[i]], ";", fixed = TRUE)[[1L]]))
    generic_version <- as.integer(specs$generic_codebook_version[[i]])
    generic_version_match <- length(versions) == 1L && identical(versions, generic_version)
    years_ok <- identical(years, expected_years)
    exception_records <- if (length(versions) > 1L) {
      sort(unique(x$record_id[is.finite(x$version) & x$version != generic_version]))
    } else {
      character()
    }
    exception_definitions_present <- if (length(exception_records)) {
      all(exception_records %in% documented_records)
    } else {
      NA
    }
    reader <- vanneman_sas_reader_contract(
      files[[paste0(specs$source_id[[i]], "_sas")]],
      specs$source_id[[i]],
      exception_records
    )
    parser_ok <- isTRUE(reader$parser_contract_verified[[1L]])
    data_checksum_ok <- vanneman_archive_file_verified(
      files[[specs$source_id[[i]]]], specs$data_relative_path[[i]], archive_checksums
    )
    sas_checksum_ok <- vanneman_archive_file_verified(
      files[[paste0(specs$source_id[[i]], "_sas")]], specs$sas_relative_path[[i]], archive_checksums
    )
    archive_pair_verified <- data_checksum_ok && sas_checksum_ok
    data.frame(
      source_id = specs$source_id[[i]],
      n_records = nrow(x),
      n_state_district_ids = length(unique(paste(x$state_id, x$district_id, sep = "__"))),
      n_record_types = length(unique(x$record_id)),
      observed_years = paste(years, collapse = ";"),
      observed_versions = paste(versions, collapse = ";"),
      generic_codebook_version = generic_version,
      generic_codebook_version_match = generic_version_match,
      version_exception_record_ids = paste(exception_records, collapse = ";"),
      version_exception_definitions_present = exception_definitions_present,
      source_specific_reader_targets_file = reader$source_specific_reader_targets_file,
      source_specific_identifier_layout_verified = reader$source_specific_identifier_layout_verified,
      source_specific_reader_covers_version_exceptions =
        reader$source_specific_reader_covers_version_exceptions,
      parser_contract_verified = parser_ok,
      archive_data_checksum_verified = data_checksum_ok,
      archive_sas_checksum_verified = sas_checksum_ok,
      archive_distribution_pair_verified = archive_pair_verified,
      years_match_contract = years_ok,
      eligible_for_baseline_values = years_ok && parser_ok && archive_pair_verified,
      status = if (!years_ok) {
        "year_mismatch"
      } else if (!parser_ok) {
        "source_reader_contract_unverified"
      } else if (!archive_pair_verified) {
        "archive_distribution_checksum_mismatch"
      } else if (!generic_version_match) {
        "source_reader_verified_generic_version_differs"
      } else {
        "source_contract_verified"
      },
      stringsAsFactors = FALSE
    )
  }))
}

save_vanneman_historical_source_qa <- function(
    x, path = "outputs/diagnostics/extended/instrument_relevance/vanneman_historical_source_qa.csv") {
  write_diagnostic_csv(x, path)
}


vanneman_panel4_geography_inventory <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman panel4 file: ", path, call. = FALSE)
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  if (!length(lines) || any(nchar(lines) < 10L)) {
    stop("Vanneman panel4 contains malformed fixed-width records.", call. = FALSE)
  }

  state_id <- substr(lines, 1L, 2L)
  district_id <- substr(lines, 3L, 4L)
  record_id <- substr(lines, 5L, 7L)
  year <- 1900L + suppressWarnings(as.integer(substr(lines, 8L, 9L)))
  label_rows <- record_id == "000"
  labels <- data.frame(
    vanneman_state_id = state_id[label_rows],
    vanneman_district_id = district_id[label_rows],
    year = year[label_rows],
    label = trimws(substr(lines[label_rows], 11L, nchar(lines[label_rows]))),
    stringsAsFactors = FALSE
  )
  expected_years <- c(1961L, 1971L, 1981L, 1991L)
  keys <- unique(labels[c("vanneman_state_id", "vanneman_district_id")])
  if (anyDuplicated(labels[c("vanneman_state_id", "vanneman_district_id", "year")])) {
    stop("Vanneman panel4 contains duplicate district-year label records.", call. = FALSE)
  }
  counts <- table(paste(labels$vanneman_state_id, labels$vanneman_district_id, sep = "__"))
  if (length(counts) != nrow(keys) || any(counts != length(expected_years)) ||
      !setequal(unique(labels$year), expected_years)) {
    stop("Vanneman panel4 does not contain one label record per panel district and census year.", call. = FALSE)
  }

  out <- keys
  for (yr in expected_years) {
    part <- labels[labels$year == yr, , drop = FALSE]
    key <- paste(part$vanneman_state_id, part$vanneman_district_id, sep = "__")
    out_key <- paste(out$vanneman_state_id, out$vanneman_district_id, sep = "__")
    out[[paste0("district_label_", yr)]] <- part$label[match(out_key, key)]
  }

  pop_rows <- record_id == "100" & year == 1991L
  populations <- data.frame(
    vanneman_state_id = state_id[pop_rows],
    vanneman_district_id = district_id[pop_rows],
    population_1991 = suppressWarnings(as.numeric(trimws(substr(lines[pop_rows], 11L, 19L)))),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(populations[c("vanneman_state_id", "vanneman_district_id")])) {
    stop("Vanneman panel4 contains duplicate 1991 population records.", call. = FALSE)
  }
  pop_key <- paste(populations$vanneman_state_id, populations$vanneman_district_id, sep = "__")
  out_key <- paste(out$vanneman_state_id, out$vanneman_district_id, sep = "__")
  out$population_1991 <- populations$population_1991[match(out_key, pop_key)]
  if (any(!is.finite(out$population_1991) | out$population_1991 <= 0)) {
    stop("Vanneman panel4 geography inventory lacks positive 1991 population.", call. = FALSE)
  }

  label_cols <- paste0("district_label_", expected_years)
  out$n_distinct_labels <- apply(out[label_cols], 1L, function(x) length(unique(x)))
  out$label_changed_1981_1991 <- out$district_label_1981 != out$district_label_1991
  out$explicit_aggregate_label_1991 <- grepl("+", out$district_label_1991, fixed = TRUE)
  out[order(out$vanneman_state_id, out$vanneman_district_id), , drop = FALSE]
}

build_vanneman_panel4_geography_inventory <- function(source_qa, paths = build_paths()) {
  qa <- safe_df(source_qa)
  panel <- qa[qa$source_id == "panel4", , drop = FALSE]
  if (nrow(panel) != 1L || !isTRUE(panel$eligible_for_baseline_values[[1L]])) {
    stop("Vanneman panel4 parser contract must be verified before building geography inventory.", call. = FALSE)
  }
  vanneman_panel4_geography_inventory(vanneman_historical_paths(paths)[["panel4"]])
}

save_vanneman_panel4_geography_inventory <- function(
    x, path = "outputs/diagnostics/extended/instrument_relevance/vanneman_panel4_geography_inventory.csv") {
  write_diagnostic_csv(x, path)
}
