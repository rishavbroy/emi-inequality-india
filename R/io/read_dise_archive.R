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
    row <- repair_dise_machine_names(
      plain_chr(unlist(preview[i, , drop = FALSE], use.names = FALSE))
    )
    "distcd" %in% row && "distname" %in% row &&
      any(c("statecd", "statename") %in% row)
  }, logical(1)))
  if (length(hits) != 1L) {
    stop("Expected exactly one DISE machine-name header row; found ", length(hits), ".", call. = FALSE)
  }
  hits[[1]]
}

repair_dise_machine_names <- function(names) {
  names <- tolower(trimws(plain_chr(names)))
  names <- gsub("[^a-z0-9]+", "_", names)
  names <- gsub("^_|_$", "", names)
  aliases <- c(
    "state_code" = "statecd", "statcd" = "statecd",
    "state_name" = "statename", "statname" = "statename",
    "district_code" = "distcd", "district_name" = "distname"
  )
  hit <- match(names, names(aliases), nomatch = 0L)
  names[hit > 0L] <- unname(aliases[hit[hit > 0L]])
  positions <- which(grepl("^enr_med[0-9]+_[0-9]+$", names))
  if (length(positions)) {
    categories <- suppressWarnings(as.integer(sub("^.*_", "", names[positions])))
    if (any(!is.finite(categories))) {
      stop("Could not decode DISE medium-enrollment category suffixes.", call. = FALSE)
    }
    new_block <- c(
      TRUE,
      diff(positions) != 1L |
        categories[-1L] <= categories[-length(categories)]
    )
    slots <- cumsum(new_block)
    if (max(slots) > 5L) {
      stop("DISE enrollment sheet contains more than five ordered medium blocks.", call. = FALSE)
    }
    names[positions] <- paste0("enr_med", slots, "_", categories)
  }
  make.unique(names)
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
  names(out) <- repair_dise_machine_names(names(out))
  required <- c("statename", "distcd", "distname")
  missing <- setdiff(required, names(out))
  if (length(missing)) stop("DISE sheet is missing key columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!"statecd" %in% names(out)) out$statecd <- NA_character_
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

dise_grade_columns <- function(data) {
  intersect(paste0("enr_cy_c", 1:8), names(data))
}

dise_management_columns <- function(data, prefix) {
  intersect(paste0(prefix, c(1:7, 9)), names(data))
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
  out$dise_management_enrollment <- out$dise_government_enrollment + out$dise_private_enrollment
  out$dise_grade_enrollment <- row_sum_available(data, dise_grade_columns(data))
  out$dise_total_enrollment <- ifelse(
    is.finite(out$dise_grade_enrollment),
    out$dise_grade_enrollment,
    out$dise_management_enrollment
  )
  out$dise_management_enrollment_difference <-
    out$dise_management_enrollment - out$dise_total_enrollment
  out$dise_private_enrollment_share <- ifelse(
    is.finite(out$dise_management_enrollment) & out$dise_management_enrollment > 0,
    100 * out$dise_private_enrollment / out$dise_management_enrollment,
    NA_real_
  )
  for (slot in 1:5) {
    out[[paste0("dise_medium_slot_", slot, "_enrollment")]] <-
      row_sum_available(data, dise_slot_columns(data, slot))
  }
  slot_cols <- paste0("dise_medium_slot_", 1:5, "_enrollment")
  out$dise_medium_classified_enrollment <- row_sum_available(out, slot_cols)
  out$dise_medium_classification_ratio <- ifelse(
    is.finite(out$dise_total_enrollment) & out$dise_total_enrollment > 0,
    100 * out$dise_medium_classified_enrollment / out$dise_total_enrollment,
    NA_real_
  )
  out$dise_medium_classification_difference <-
    out$dise_medium_classified_enrollment - out$dise_total_enrollment
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

read_dise_report_language_enrollment <- function(paths = build_paths()) {
  path <- path_metadata(paths, "dise_report_language_enrollment.csv")
  if (!file.exists(path)) stop("Missing DISE report-language metadata: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  key <- paste(out$academic_year, canonicalize_state_name(out$state_report),
               canonicalize_district_name(out$district_report), sep = "|")
  if (anyDuplicated(key)) stop("DISE report-language metadata contains duplicate district-year keys.", call. = FALSE)
  out
}

extract_dise_direct_total <- function(data, academic_year) {
  out <- data.frame(
    academic_year = academic_year,
    state_code_dise = plain_chr(data$statecd),
    state_name_dise = trimws(plain_chr(data$statename)),
    district_code_dise = plain_chr(data$distcd),
    district_name_dise = trimws(plain_chr(data$distname)),
    stringsAsFactors = FALSE
  )
  if ("enrtot" %in% names(data)) {
    out$dise_total_enrollment <- num(data$enrtot)
  } else {
    grade <- row_sum_available(data, dise_grade_columns(data))
    management <- row_sum_available(data, dise_management_columns(data, "enr_govt")) +
      row_sum_available(data, dise_management_columns(data, "enr_pvt"))
    out$dise_total_enrollment <- ifelse(is.finite(grade), grade, management)
  }
  out
}

attach_dise_report_language_counts <- function(data, report) {
  x <- safe_df(data)
  r <- safe_df(report)
  x$state_key <- canonicalize_state_name(x$state_name_dise)
  x$district_key <- canonicalize_district_name(x$district_name_dise)
  r$state_key <- canonicalize_state_name(r$state_report)
  r$district_key <- canonicalize_district_name(r$district_report)
  keep <- c(
    "academic_year", "state_key", "district_key",
    "english_enrollment", "hindi_enrollment",
    "source_pdf", "source_page", "report_priority"
  )
  r <- r[keep]
  out <- merge(
    x, r,
    by = c("academic_year", "state_key", "district_key"),
    all.x = TRUE, sort = FALSE
  )
  out$dise_english_enrollment <- num(out$english_enrollment)
  out$dise_hindi_enrollment <- num(out$hindi_enrollment)
  out$dise_english_identity_resolved <- is.finite(out$dise_english_enrollment)
  out$dise_hindi_identity_resolved <- is.finite(out$dise_hindi_enrollment)
  out$dise_emi_enrollment_share_total <- ifelse(
    out$dise_english_identity_resolved &
      is.finite(num(out$dise_total_enrollment)) &
      num(out$dise_total_enrollment) > 0,
    100 * out$dise_english_enrollment / num(out$dise_total_enrollment),
    NA_real_
  )
  out
}

extract_dise_2015_medium_counts <- function(data) {
  out <- data.frame(
    district_code_dise = plain_chr(data$distcd),
    stringsAsFactors = FALSE
  )
  slot_count <- function(slot) {
    cols <- grep(paste0("^enre", slot, "[1-7]$"), names(data), value = TRUE)
    row_sum_available(data, cols)
  }
  for (slot in 1:5) {
    out[[paste0("medium_code_", slot)]] <- num(data[[paste0("m", slot)]])
    out[[paste0("medium_enrollment_", slot)]] <- slot_count(slot)
  }
  language_count <- function(code) {
    vapply(seq_len(nrow(out)), function(i) {
      codes <- vapply(1:5, function(slot) out[[paste0("medium_code_", slot)]][[i]], numeric(1))
      counts <- vapply(1:5, function(slot) out[[paste0("medium_enrollment_", slot)]][[i]], numeric(1))
      known <- is.finite(codes)
      hit <- known & codes == code
      if (any(hit)) return(sum(counts[hit], na.rm = TRUE))
      unresolved_positive <- !known & is.finite(counts) & counts > 0
      if (any(unresolved_positive)) return(NA_real_)
      0
    }, numeric(1))
  }
  out$dise_english_enrollment <- language_count(19)
  out$dise_hindi_enrollment <- language_count(4)
  out
}

read_dise_dynamic_year <- function(paths, registry_row, report_languages) {
  workbook <- materialize_dise_workbook(paths, registry_row)
  year <- registry_row$academic_year[[1]]
  if (identical(year, "2015-16")) {
    base <- read_dise_machine_sheet(workbook, "2015-16_1")
    medium <- read_dise_machine_sheet(workbook, "2015-16_2")
    totals <- extract_dise_direct_total(base, year)
    medium <- extract_dise_2015_medium_counts(medium)
    out <- merge(totals, medium, by = "district_code_dise", all.x = TRUE, sort = FALSE)
    out$dise_english_identity_resolved <- is.finite(out$dise_english_enrollment)
    out$dise_hindi_identity_resolved <- is.finite(out$dise_hindi_enrollment)
    out$dise_emi_enrollment_share_total <- ifelse(
      out$dise_english_identity_resolved &
        is.finite(out$dise_total_enrollment) &
        out$dise_total_enrollment > 0,
      100 * out$dise_english_enrollment / out$dise_total_enrollment,
      NA_real_
    )
    return(out)
  }
  sheet <- registry_row$enrollment_sheet[[1]]
  if (identical(year, "2014-15")) sheet <- "2014-15_PY"
  totals <- extract_dise_direct_total(read_dise_machine_sheet(workbook, sheet), year)
  attach_dise_report_language_counts(
    totals,
    report_languages[report_languages$academic_year == year, , drop = FALSE]
  )
}

read_dise_dynamic_archive <- function(
  paths = build_paths(),
  registry = read_dise_archive_registry(paths),
  report_languages = read_dise_report_language_enrollment(paths)
) {
  require_manifest_files(paths, source_id = "dise_district_report_cards", required_only = FALSE)
  rows <- registry[registry$analytic_role == "dynamic_future", , drop = FALSE]
  if (!nrow(rows)) stop("DISE archive registry has no dynamic years.", call. = FALSE)
  safe_bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    read_dise_dynamic_year(paths, rows[i, , drop = FALSE], report_languages)
  }))
}
