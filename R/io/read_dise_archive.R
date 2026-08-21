# Archived DISE/UDISE district-report-card readers.

read_dise_archive_registry <- function(paths = build_paths()) {
  path <- path_metadata(paths, "dise_archive_registry.csv")
  if (!file.exists(path)) stop("Missing DISE archive registry: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

read_dise_publication_checks <- function(paths = build_paths()) {
  path <- path_metadata(paths, "dise_publication_checks.csv")
  if (!file.exists(path)) stop("Missing DISE publication checks: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

read_dise_medium_slot_crosswalk <- function(paths = build_paths()) {
  path <- path_metadata(paths, "dise_medium_slot_crosswalk.csv")
  if (!file.exists(path)) stop("Missing DISE medium-slot crosswalk: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE)
  key <- paste(out$academic_year, out$state_report, out$district_report, out$medium_slot, sep = "|")
  if (anyDuplicated(key)) stop("DISE medium-slot crosswalk contains duplicate year/district/slot keys.", call. = FALSE)
  out
}

materialize_dise_workbook <- function(paths, registry_row) {
  root <- path_project(paths, "data", "raw", "dise_internet_archive")
  source <- file.path(root, registry_row$raw_file[[1]])
  if (!file.exists(source)) stop("Missing DISE archive file: ", source, call. = FALSE)
  member <- plain_chr(registry_row$zip_member[[1]] %||% "")
  if (!nzchar(member)) return(source)

  listing <- utils::unzip(source, list = TRUE)$Name
  if (!member %in% listing) {
    stop("DISE ZIP member is missing: ", member, " in ", source, call. = FALSE)
  }
  exdir <- tempfile("dise-workbook-")
  dir.create(exdir, recursive = TRUE)
  utils::unzip(source, files = member, exdir = exdir)
  file.path(exdir, member)
}

find_dise_machine_header_row <- function(preview) {
  hits <- which(vapply(seq_len(nrow(preview)), function(i) {
    row <- tolower(trimws(plain_chr(unlist(preview[i, , drop = FALSE], use.names = FALSE))))
    all(c("statecd", "distcd") %in% row)
  }, logical(1)))
  if (length(hits) != 1L) {
    stop("Expected exactly one DISE machine-name header row; found ", length(hits), ".", call. = FALSE)
  }
  hits[[1]]
}

read_dise_machine_sheet <- function(path, sheet) {
  need_pkg("readxl", "archived DISE district-report-card workbooks")
  if (!sheet %in% readxl::excel_sheets(path)) {
    stop("DISE workbook is missing sheet '", sheet, "': ", path, call. = FALSE)
  }
  preview <- readxl::read_excel(
    path, sheet = sheet, col_names = FALSE, n_max = 40L, .name_repair = "minimal"
  )
  header_row <- find_dise_machine_header_row(preview)
  out <- safe_df(readxl::read_excel(
    path, sheet = sheet, skip = header_row - 1L, .name_repair = "minimal"
  ))
  names(out) <- make.unique(tolower(trimws(names(out))))
  required <- c("statecd", "statename", "distcd", "distname")
  missing <- setdiff(required, names(out))
  if (length(missing)) stop("DISE sheet is missing key columns: ", paste(missing, collapse = ", "), call. = FALSE)
  keep <- nzchar(trimws(plain_chr(out$statename))) & nzchar(trimws(plain_chr(out$distname)))
  out[keep, , drop = FALSE]
}

row_sum_available <- function(data, columns) {
  columns <- intersect(columns, names(data))
  if (!length(columns)) return(rep(NA_real_, nrow(data)))
  values <- as.data.frame(lapply(data[columns], num), stringsAsFactors = FALSE)
  present <- rowSums(!is.na(values))
  total <- rowSums(values, na.rm = TRUE)
  total[present == 0L] <- NA_real_
  total
}

dise_slot_columns <- function(data, slot) {
  grep(paste0("^enr_med", slot, "_[1-5]$"), names(data), value = TRUE)
}

dise_management_columns <- function(data, prefix) {
  intersect(paste0(prefix, c(1:5, 9)), names(data))
}

extract_dise_enrollment_measures <- function(data, academic_year) {
  out <- data.frame(
    academic_year = academic_year,
    state_code_dise = plain_chr(data$statecd),
    state_name_dise = trimws(plain_chr(data$statename)),
    district_code_dise = plain_chr(data$distcd),
    district_name_dise = trimws(plain_chr(data$distname)),
    stringsAsFactors = FALSE
  )
  out$dise_government_enrollment <- row_sum_available(data, dise_management_columns(data, "enr_govt"))
  out$dise_private_enrollment <- row_sum_available(data, dise_management_columns(data, "enr_pvt"))
  out$dise_total_enrollment <- out$dise_government_enrollment + out$dise_private_enrollment
  out$dise_private_enrollment_share <- ifelse(
    is.finite(out$dise_total_enrollment) & out$dise_total_enrollment > 0,
    100 * out$dise_private_enrollment / out$dise_total_enrollment,
    NA_real_
  )
  for (slot in 1:5) {
    out[[paste0("dise_medium_slot_", slot, "_enrollment")]] <-
      row_sum_available(data, dise_slot_columns(data, slot))
  }
  slot_cols <- paste0("dise_medium_slot_", 1:5, "_enrollment")
  out$dise_medium_reported_enrollment <- row_sum_available(out, slot_cols)
  out$dise_medium_reporting_share <- ifelse(
    is.finite(out$dise_total_enrollment) & out$dise_total_enrollment > 0,
    100 * out$dise_medium_reported_enrollment / out$dise_total_enrollment,
    NA_real_
  )
  out
}

extract_dise_school_measures <- function(data) {
  out <- data.frame(
    state_code_dise = plain_chr(data$statecd),
    district_code_dise = plain_chr(data$distcd),
    stringsAsFactors = FALSE
  )
  out$dise_government_schools <- row_sum_available(data, dise_management_columns(data, "schgovt"))
  out$dise_private_schools <- row_sum_available(data, dise_management_columns(data, "schpvt"))
  out$dise_total_schools <- out$dise_government_schools + out$dise_private_schools
  out$dise_private_school_share <- ifelse(
    is.finite(out$dise_total_schools) & out$dise_total_schools > 0,
    100 * out$dise_private_schools / out$dise_total_schools,
    NA_real_
  )
  out
}

read_dise_baseline_year <- function(paths, registry_row) {
  workbook <- materialize_dise_workbook(paths, registry_row)
  enrolment <- extract_dise_enrollment_measures(
    read_dise_machine_sheet(workbook, registry_row$enrollment_sheet[[1]]),
    registry_row$academic_year[[1]]
  )
  schools <- extract_dise_school_measures(
    read_dise_machine_sheet(workbook, registry_row$school_sheet[[1]])
  )
  key <- c("state_code_dise", "district_code_dise")
  if (anyDuplicated(enrolment[key])) stop("DISE enrollment sheet contains duplicate district codes.", call. = FALSE)
  if (anyDuplicated(schools[key])) stop("DISE school sheet contains duplicate district codes.", call. = FALSE)
  merge(enrolment, schools, by = key, all.x = TRUE, sort = FALSE)
}

read_dise_baseline_archive <- function(paths = build_paths(), registry = read_dise_archive_registry(paths)) {
  require_manifest_files(
    paths, source_id = "dise_district_report_cards", required_only = FALSE
  )
  rows <- registry[registry$analytic_role == "baseline_treatment", , drop = FALSE]
  if (!nrow(rows)) stop("DISE archive registry has no baseline treatment years.", call. = FALSE)
  safe_bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    read_dise_baseline_year(paths, rows[i, , drop = FALSE])
  }))
}
