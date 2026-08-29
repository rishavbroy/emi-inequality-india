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
    combining_codebook = file.path(root, "codebook/Combining divided district to recreate 1961 district boundary.html"),
    panel_state_crosswalk = path_project(paths, "data/metadata/vanneman_panel_state_crosswalk.csv"),
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
  qa_file_ids <- c(
    "panel4", "dist81", "dist91", "codebook", "variables_codebook", "education_codebook",
    "panel4_sas", "dist81_sas", "dist91_sas", "archive_checksums"
  )
  missing <- files[qa_file_ids][!file.exists(files[qa_file_ids])]
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


read_vanneman_panel_state_crosswalk <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman panel-state crosswalk: ", path, call. = FALSE)
  x <- read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  required <- c(
    "panel_state_id", "dist81_state_id", "dist91_state_id", "state_name",
    "panel_to_1991_state_status", "source_basis"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Vanneman panel-state crosswalk lacks columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$panel_state_id <- normalize_census_code(x$panel_state_id, 2L)
  x$dist81_state_id <- ifelse(
    nzchar(trimws(x$dist81_state_id)), normalize_census_code(x$dist81_state_id, 2L), NA_character_
  )
  x$dist91_state_id <- ifelse(
    nzchar(trimws(x$dist91_state_id)), normalize_census_code(x$dist91_state_id, 2L), NA_character_
  )
  if (anyDuplicated(x$panel_state_id)) stop("Vanneman panel-state crosswalk has duplicate panel state IDs.", call. = FALSE)
  allowed <- c("mapped_one_to_one", "split_across_1991_states", "no_1991_census")
  if (any(!x$panel_to_1991_state_status %in% allowed)) {
    stop("Vanneman panel-state crosswalk contains unsupported mapping statuses.", call. = FALSE)
  }
  mapped <- x$panel_to_1991_state_status == "mapped_one_to_one"
  if (any(mapped & is.na(x$dist91_state_id))) {
    stop("Mapped Vanneman panel states require a 1991 state ID.", call. = FALSE)
  }
  x
}

vanneman_documented_combined_panel_units <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman combining-district documentation: ", path, call. = FALSE)
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  hits <- regmatches(text, gregexpr("[0-9]{4}[[:space:]]*=", text, perl = TRUE))[[1L]]
  if (!length(hits) || identical(hits, "-1")) return(character())
  sort(unique(sub("[[:space:]]*=.*$", "", hits)))
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
    population_1991_raw = suppressWarnings(as.numeric(trimws(substr(lines[pop_rows], 11L, 19L)))),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(populations[c("vanneman_state_id", "vanneman_district_id")])) {
    stop("Vanneman panel4 contains duplicate 1991 population records.", call. = FALSE)
  }
  pop_key <- paste(populations$vanneman_state_id, populations$vanneman_district_id, sep = "__")
  out_key <- paste(out$vanneman_state_id, out$vanneman_district_id, sep = "__")
  raw_population <- populations$population_1991_raw[match(out_key, pop_key)]
  if (any(!is.finite(raw_population))) {
    stop("Vanneman panel4 geography inventory contains malformed 1991 population values.", call. = FALSE)
  }
  unsupported_nonpositive <- raw_population <= 0 & raw_population != -1
  if (any(unsupported_nonpositive)) {
    stop("Vanneman panel4 contains nonpositive 1991 population values other than the documented -1 missing sentinel.", call. = FALSE)
  }
  out$population_1991 <- ifelse(raw_population == -1, NA_real_, raw_population)
  out$population_1991_available <- is.finite(out$population_1991) & out$population_1991 > 0
  out$population_1991_status <- ifelse(out$population_1991_available, "observed", "documented_missing_sentinel")

  label_cols <- paste0("district_label_", expected_years)
  out$n_distinct_labels <- apply(out[label_cols], 1L, function(x) length(unique(x)))
  out$label_changed_1981_1991 <- out$district_label_1981 != out$district_label_1991
  out$explicit_aggregate_label_1991 <- grepl("+", out$district_label_1991, fixed = TRUE)
  out[order(out$vanneman_state_id, out$vanneman_district_id), , drop = FALSE]
}

vanneman_dist91_geography_inventory <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman dist91 file: ", path, call. = FALSE)
  con <- gzfile(path, open = "rt")
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  if (!length(lines) || any(nchar(lines) < 10L)) {
    stop("Vanneman dist91 contains malformed fixed-width records.", call. = FALSE)
  }
  keep <- substr(lines, 5L, 7L) == "000" & substr(lines, 8L, 9L) == "91"
  out <- data.frame(
    dist91_state_id = substr(lines[keep], 1L, 2L),
    dist91_district_id = substr(lines[keep], 3L, 4L),
    dist91_district_label = trimws(substr(lines[keep], 11L, nchar(lines[keep]))),
    stringsAsFactors = FALSE
  )
  if (!nrow(out) || anyDuplicated(out[c("dist91_state_id", "dist91_district_id")])) {
    stop("Vanneman dist91 label inventory must contain unique 1991 state-district IDs.", call. = FALSE)
  }
  out$dist91_label_key <- canonicalize_district_name(out$dist91_district_label)
  out
}

vanneman_panel4_dist91_crosswalk <- function(
    panel_geography, dist91_geography, state_crosswalk, documented_combined_units = character()) {
  panel <- safe_df(panel_geography)
  dist91 <- safe_df(dist91_geography)
  states <- safe_df(state_crosswalk)
  required_panel <- c("vanneman_state_id", "vanneman_district_id", "district_label_1991", "explicit_aggregate_label_1991")
  missing <- setdiff(required_panel, names(panel))
  if (length(missing)) stop("Vanneman panel geography lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)

  panel$panel_unit_id <- paste0(panel$vanneman_state_id, panel$vanneman_district_id)
  panel$panel_label_key <- canonicalize_district_name(panel$district_label_1991)
  state_idx <- match(panel$vanneman_state_id, states$panel_state_id)
  if (anyNA(state_idx)) stop("Vanneman panel geography contains state IDs absent from the state crosswalk.", call. = FALSE)
  panel$dist91_state_id <- states$dist91_state_id[state_idx]
  panel$panel_to_1991_state_status <- states$panel_to_1991_state_status[state_idx]
  panel$documented_combined_panel_unit <- panel$panel_unit_id %in% documented_combined_units

  match_one <- function(i) {
    if (panel$panel_to_1991_state_status[[i]] == "no_1991_census") {
      return(c(NA_character_, NA_character_, "no_1991_census", "no_1991_census", "FALSE"))
    }
    if (panel$panel_to_1991_state_status[[i]] != "mapped_one_to_one") {
      return(c(NA_character_, NA_character_, "state_mapping_requires_review", "state_mapping_requires_review", "FALSE"))
    }
    if (isTRUE(panel$explicit_aggregate_label_1991[[i]]) || isTRUE(panel$documented_combined_panel_unit[[i]])) {
      return(c(NA_character_, NA_character_, "aggregate_requires_review", "documented_or_explicit_aggregate", "FALSE"))
    }
    cand <- dist91[
      dist91$dist91_state_id == panel$dist91_state_id[[i]] &
        dist91$dist91_label_key == panel$panel_label_key[[i]],
      , drop = FALSE
    ]
    if (nrow(cand) != 1L) {
      return(c(NA_character_, NA_character_, "label_review_required", "no_unique_exact_normalized_label", "FALSE"))
    }
    if (identical(cand$dist91_district_id[[1L]], "00")) {
      return(c(cand$dist91_district_id[[1L]], cand$dist91_district_label[[1L]], "small_state_aggregate", "exact_label_but_aggregated_state", "FALSE"))
    }
    c(cand$dist91_district_id[[1L]], cand$dist91_district_label[[1L]], "deterministic_one_to_one", "exact_normalized_label_within_state", "TRUE")
  }
  matched <- t(vapply(seq_len(nrow(panel)), match_one, character(5)))
  colnames(matched) <- c(
    "dist91_district_id", "dist91_district_label", "mapping_class", "mapping_basis", "preferred_pretrend_eligible"
  )
  out <- cbind(panel, as.data.frame(matched, stringsAsFactors = FALSE))
  out$preferred_pretrend_eligible <- out$preferred_pretrend_eligible == "TRUE"
  out[order(out$vanneman_state_id, out$vanneman_district_id), , drop = FALSE]
}

read_vanneman_panel4_dist91_adjudications <- function(path) {
  if (!file.exists(path)) stop("Missing Vanneman panel-to-dist91 adjudication ledger: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  required <- c(
    "panel_unit_id", "dist91_state_id", "dist91_district_id",
    "decision", "source_id", "evidence"
  )
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop("Vanneman panel-to-dist91 adjudication ledger lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(out$panel_unit_id)) {
    stop("Vanneman panel-to-dist91 adjudication ledger has duplicate panel IDs.", call. = FALSE)
  }
  if (any(out$decision != "accepted_one_to_one")) {
    stop("Vanneman panel-to-dist91 adjudications currently support accepted_one_to_one decisions only.", call. = FALSE)
  }
  out
}

apply_vanneman_panel4_dist91_adjudications <- function(panel_crosswalk, adjudications) {
  out <- safe_df(panel_crosswalk)
  reviewed <- safe_df(adjudications)
  if (!nrow(reviewed)) return(out)
  required <- c(
    "panel_unit_id", "dist91_state_id", "dist91_district_id", "dist91_district_label",
    "decision", "source_id", "evidence_status"
  )
  missing <- setdiff(required, names(reviewed))
  if (length(missing)) {
    stop("Validated Vanneman adjudications lack fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (any(reviewed$evidence_status != "verified_direct_alias")) {
    stop("Only verified direct-alias adjudications may enter the preferred Vanneman geography.", call. = FALSE)
  }
  idx <- match(reviewed$panel_unit_id, out$panel_unit_id)
  if (anyNA(idx)) stop("Validated Vanneman adjudication references an unknown stable panel ID.", call. = FALSE)
  if (any(out$mapping_class[idx] != "label_review_required")) {
    stop("Vanneman adjudications may promote only unresolved label-review cases.", call. = FALSE)
  }

  out$review_source_id <- NA_character_
  out$review_evidence <- NA_character_
  out$dist91_state_id[idx] <- reviewed$dist91_state_id
  out$dist91_district_id[idx] <- reviewed$dist91_district_id
  out$dist91_district_label[idx] <- reviewed$dist91_district_label
  out$mapping_class[idx] <- "reviewed_one_to_one"
  out$mapping_basis[idx] <- "reviewed_liu_direct_alias"
  out$preferred_pretrend_eligible[idx] <- TRUE
  out$review_source_id[idx] <- reviewed$source_id
  out$review_evidence[idx] <- reviewed$evidence_status
  out[order(out$vanneman_state_id, out$vanneman_district_id), , drop = FALSE]
}

build_vanneman_pretrend_geography <- function(panel_crosswalk, source_geography_1991, transition_1991_2001) {
  panel <- safe_df(panel_crosswalk)
  geography <- safe_df(source_geography_1991)
  transition <- safe_df(transition_1991_2001)
  required_panel <- c(
    "panel_unit_id", "dist91_state_id", "dist91_district_id",
    "dist91_district_label", "preferred_pretrend_eligible"
  )
  required_geography <- c(
    "state_code_1991", "district_code_1991", "mapping_class",
    "population_coverage", "n_target_2001_districts", "preferred_language_persistence"
  )
  required_transition <- c(
    "state_code_1991", "district_code_1991", "state_code_2001", "district_code_2001"
  )
  missing_panel <- setdiff(required_panel, names(panel))
  missing_geography <- setdiff(required_geography, names(geography))
  missing_transition <- setdiff(required_transition, names(transition))
  if (length(missing_panel)) stop("Vanneman pretrend panel geography lacks fields: ", paste(missing_panel, collapse = ", "), call. = FALSE)
  if (length(missing_geography)) stop("Historical 1991 geography lacks fields: ", paste(missing_geography, collapse = ", "), call. = FALSE)
  if (length(missing_transition)) stop("Historical 1991-2001 transition lacks fields: ", paste(missing_transition, collapse = ", "), call. = FALSE)
  if (anyDuplicated(panel$panel_unit_id)) stop("Vanneman pretrend geography requires unique stable panel IDs.", call. = FALSE)
  if (anyDuplicated(geography[c("state_code_1991", "district_code_1991")])) {
    stop("Historical 1991 geography must contain unique source district codes.", call. = FALSE)
  }

  key <- paste(panel$dist91_state_id, panel$dist91_district_id, sep = "__")
  geography_key <- paste(geography$state_code_1991, geography$district_code_1991, sep = "__")
  gidx <- match(key, geography_key)
  out <- panel
  out$project_1991_mapping_class <- geography$mapping_class[gidx]
  out$project_1991_population_coverage <- suppressWarnings(as.numeric(geography$population_coverage[gidx]))
  out$project_1991_preferred_single_target <- geography$preferred_language_persistence[gidx]
  out$project_1991_n_target_2001_districts <- suppressWarnings(as.integer(geography$n_target_2001_districts[gidx]))

  transition_key <- paste(transition$state_code_1991, transition$district_code_1991, sep = "__")
  target_count <- table(transition_key)
  out$n_transition_targets <- as.integer(target_count[key])
  out$n_transition_targets[is.na(out$n_transition_targets)] <- 0L
  out$state_code_2001 <- NA_character_
  out$district_code_2001 <- NA_character_

  out$pretrend_geography_status <- "panel_to_1991_not_preferred"
  panel_ok <- out$preferred_pretrend_eligible
  missing_geo <- panel_ok & is.na(gidx)
  out$pretrend_geography_status[missing_geo] <- "missing_project_1991_geography"
  split <- panel_ok & !missing_geo & (
    is.na(out$project_1991_n_target_2001_districts) |
      out$project_1991_n_target_2001_districts != 1L |
      out$n_transition_targets != 1L
  )
  out$pretrend_geography_status[split] <- "splits_across_2001_districts"
  project_preferred <- as.logical(out$project_1991_preferred_single_target)
  project_preferred[is.na(project_preferred)] <- FALSE
  not_preferred <- panel_ok & !missing_geo & !split & !project_preferred
  out$pretrend_geography_status[not_preferred] <- "project_1991_geography_not_preferred"
  preferred <- panel_ok & !missing_geo & !split & project_preferred
  out$pretrend_geography_status[preferred] <- "preferred_single_target"

  if (any(preferred)) {
    tidx <- match(key[preferred], transition_key)
    out$state_code_2001[preferred] <- transition$state_code_2001[tidx]
    out$district_code_2001[preferred] <- transition$district_code_2001[tidx]
  }
  out$preferred_vanneman_pretrend_eligible <- preferred
  out[order(out$panel_unit_id), , drop = FALSE]
}

save_vanneman_pretrend_geography <- function(
    x, path = "outputs/diagnostics/extended/instrument_relevance/vanneman_pretrend_geography.csv") {
  write_diagnostic_csv(x, path)
}

build_vanneman_panel4_geography_inventory <- function(source_qa, paths = build_paths()) {
  qa <- safe_df(source_qa)
  panel <- qa[qa$source_id == "panel4", , drop = FALSE]
  if (nrow(panel) != 1L || !isTRUE(panel$eligible_for_baseline_values[[1L]])) {
    stop("Vanneman panel4 parser contract must be verified before building geography inventory.", call. = FALSE)
  }
  files <- vanneman_historical_paths(paths)
  out <- vanneman_panel4_geography_inventory(files[["panel4"]])
  states <- read_vanneman_panel_state_crosswalk(files[["panel_state_crosswalk"]])
  state_idx <- match(out$vanneman_state_id, states$panel_state_id)
  if (anyNA(state_idx)) stop("Vanneman panel4 geography contains states absent from the documented state crosswalk.", call. = FALSE)
  out$panel_to_1991_state_status <- states$panel_to_1991_state_status[state_idx]
  out$dist91_state_id <- states$dist91_state_id[state_idx]
  missing_population <- !out$population_1991_available
  if (any(missing_population & out$panel_to_1991_state_status != "no_1991_census")) {
    stop("Vanneman panel4 has undocumented missing 1991 population outside a no-census state.", call. = FALSE)
  }
  out$population_1991_status[missing_population] <- "no_1991_census"
  out
}

build_vanneman_panel4_dist91_crosswalk <- function(source_qa, panel_geography, paths = build_paths()) {
  qa <- safe_df(source_qa)
  dist91_qa <- qa[qa$source_id == "dist91", , drop = FALSE]
  if (nrow(dist91_qa) != 1L || !isTRUE(dist91_qa$eligible_for_baseline_values[[1L]])) {
    stop("Vanneman dist91 parser contract must be verified before building the panel geography crosswalk.", call. = FALSE)
  }
  files <- vanneman_historical_paths(paths)
  vanneman_panel4_dist91_crosswalk(
    panel_geography = panel_geography,
    dist91_geography = vanneman_dist91_geography_inventory(files[["dist91"]]),
    state_crosswalk = read_vanneman_panel_state_crosswalk(files[["panel_state_crosswalk"]]),
    documented_combined_units = vanneman_documented_combined_panel_units(files[["combining_codebook"]])
  )
}

save_vanneman_panel4_geography_inventory <- function(
    x, path = "outputs/diagnostics/extended/instrument_relevance/vanneman_panel4_geography_inventory.csv") {
  write_diagnostic_csv(x, path)
}

save_vanneman_panel4_dist91_crosswalk <- function(
    x, path = "outputs/diagnostics/extended/instrument_relevance/vanneman_panel4_dist91_crosswalk.csv") {
  write_diagnostic_csv(x, path)
}
