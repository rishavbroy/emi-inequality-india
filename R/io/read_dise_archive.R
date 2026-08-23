# Archived DISE/UDISE district-report-card readers.

read_dise_archive_registry <- function(
  paths = build_paths(),
  path = path_metadata(paths, "dise_archive_registry.csv")
) {
  if (!file.exists(path)) stop("Missing DISE archive registry: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

read_dise_publication_checks <- function(
  paths = build_paths(),
  path = path_metadata(paths, "dise_publication_checks.csv")
) {
  if (!file.exists(path)) stop("Missing DISE publication checks: ", path, call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

read_dise_medium_slot_crosswalk <- function(
  paths = build_paths(),
  path = path_metadata(paths, "dise_medium_slot_crosswalk.csv")
) {
  if (!file.exists(path)) stop("Missing DISE medium-slot crosswalk: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE)
  key <- paste(out$academic_year, out$state_report, out$district_report, out$medium_slot, sep = "|")
  if (anyDuplicated(key)) stop("DISE medium-slot crosswalk contains duplicate year/district/slot keys.", call. = FALSE)
  out
}

normalize_dise_workbook_extension <- function(path) {
  need_pkg("readxl", "archived DISE district-report-card workbooks")
  format <- readxl::format_from_signature(path)
  if (is.na(format) || !format %in% c("xls", "xlsx")) return(path)

  extension <- tolower(tools::file_ext(path))
  if (identical(extension, format)) return(path)

  normalized <- tempfile("dise-workbook-", fileext = paste0(".", format))
  if (!file.copy(path, normalized, overwrite = TRUE)) {
    stop(
      "Could not create extension-correct DISE workbook view for ",
      path,
      ".",
      call. = FALSE
    )
  }
  normalized
}

materialize_dise_workbook <- function(paths, registry_row) {
  root <- path_project(paths, "data", "raw", "dise_internet_archive")
  source <- file.path(root, registry_row$raw_file[[1]])
  if (!file.exists(source)) stop("Missing DISE archive file: ", source, call. = FALSE)
  member <- plain_chr(registry_row$zip_member[[1]] %||% "")
  if (!nzchar(member)) return(normalize_dise_workbook_extension(source))

  listing <- utils::unzip(source, list = TRUE)$Name
  if (!member %in% listing) {
    stop("DISE ZIP member is missing: ", member, " in ", source, call. = FALSE)
  }
  exdir <- tempfile("dise-workbook-")
  dir.create(exdir, recursive = TRUE)
  utils::unzip(source, files = member, exdir = exdir)
  normalize_dise_workbook_extension(file.path(exdir, member))
}

dise_machine_header_score <- function(values) {
  row <- repair_dise_machine_names(values)
  key_score <- sum(row %in% c("statecd", "statename", "distcd", "distname"))
  machine_score <- sum(grepl(
    "^(enr_|sch|tch|stch|gtoilet|sgtoil|m[1-5]$|enre[0-9]|c[0-9]+_[bg]$)",
    row
  ))
  key_score + machine_score
}

find_dise_machine_header_row <- function(preview) {
  scores <- vapply(seq_len(nrow(preview)), function(i) {
    dise_machine_header_score(
      plain_chr(unlist(preview[i, , drop = FALSE], use.names = FALSE))
    )
  }, numeric(1))
  best <- max(scores, na.rm = TRUE)
  hits <- which(scores == best & scores > 4)
  if (length(hits) != 1L) {
    stop(
      "Expected one highest-scoring DISE machine-name header row; found ",
      length(hits),
      " (best score = ",
      best,
      ").",
      call. = FALSE
    )
  }
  hits[[1]]
}

dise_key_positions_from_preview <- function(preview, header_row) {
  key_names <- c("statecd", "statename", "distcd", "distname")
  rows <- rev(seq_len(header_row))
  positions <- setNames(rep(NA_integer_, length(key_names)), key_names)

  for (i in rows) {
    row <- repair_dise_machine_names(
      plain_chr(unlist(preview[i, , drop = FALSE], use.names = FALSE))
    )
    for (key in key_names) {
      if (is.finite(positions[[key]])) next
      hits <- which(row == key)
      if (length(hits) == 1L) positions[[key]] <- hits[[1]]
    }
    if (all(is.finite(positions[c("statename", "distcd", "distname")]))) break
  }
  positions
}

repair_dise_key_names_from_preview <- function(names, preview, header_row) {
  positions <- dise_key_positions_from_preview(preview, header_row)
  for (key in names(positions)) {
    position <- positions[[key]]
    if (is.finite(position) && position <= length(names)) {
      names[[position]] <- key
    }
  }
  make.unique(names)
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

dise_machine_names_from_preview <- function(preview, header_row) {
  raw_names <- plain_chr(unlist(preview[header_row, , drop = FALSE], use.names = FALSE))
  repair_dise_key_names_from_preview(
    repair_dise_machine_names(raw_names),
    preview,
    header_row
  )
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
  machine_names <- dise_machine_names_from_preview(preview, header_row)
  out <- safe_df(readxl::read_excel(
    path,
    sheet = sheet,
    skip = header_row,
    col_names = FALSE,
    .name_repair = "minimal"
  ))
  if (ncol(out) > length(machine_names)) {
    stop(
      "DISE data rows contain more columns than the selected machine header ",
      "[sheet=", sheet, ", header_row=", header_row, ", path=", path, "].",
      call. = FALSE
    )
  }
  names(out) <- machine_names[seq_len(ncol(out))]
  required <- c("statename", "distcd", "distname")
  missing <- setdiff(required, names(out))
  if (length(missing)) {
    stop(
      "DISE sheet is missing key columns after header-block repair: ",
      paste(missing, collapse = ", "),
      " [sheet=", sheet, ", header_row=", header_row, ", path=", path, "].",
      call. = FALSE
    )
  }
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

dise_total_from_columns <- function(data, total_column, fallback_columns = character()) {
  if (total_column %in% names(data)) return(num(data[[total_column]]))
  row_sum_available(data, intersect(fallback_columns, names(data)))
}

extract_dise_school_measures <- function(data) {
  out <- data.frame(
    state_code_dise = if ("statecd" %in% names(data)) plain_chr(data$statecd) else NA_character_,
    district_code_dise = plain_chr(data$distcd),
    stringsAsFactors = FALSE
  )
  out$dise_government_schools <- row_sum_available(data, dise_management_columns(data, "schgovt"))
  out$dise_private_schools <- row_sum_available(data, dise_management_columns(data, "schpvt"))
  out$dise_total_schools <- if ("schtot" %in% names(data)) {
    num(data$schtot)
  } else {
    out$dise_government_schools + out$dise_private_schools
  }
  out$dise_single_teacher_schools <- dise_total_from_columns(
    data, "stchtot", "tch1_school"
  )
  out$dise_girls_toilet_schools <- dise_total_from_columns(
    data, "sgtoiltot", "gtoilet_sch"
  )
  out$dise_private_school_share <- ifelse(
    is.finite(out$dise_total_schools) & out$dise_total_schools > 0,
    100 * out$dise_private_schools / out$dise_total_schools,
    NA_real_
  )
  out
}

extract_dise_teacher_measures <- function(data) {
  out <- data.frame(
    district_code_dise = plain_chr(data$distcd),
    stringsAsFactors = FALSE
  )
  out$dise_total_teachers <- if ("tchtot" %in% names(data)) {
    num(data$tchtot)
  } else {
    row_sum_available(data, dise_management_columns(data, "tch_govt")) +
      row_sum_available(data, dise_management_columns(data, "tch_pvt"))
  }
  out
}

finalize_dise_school_quality_measures <- function(data) {
  out <- safe_df(data)
  if ("dise_total_schools" %in% names(out)) {
    total <- num(out$dise_total_schools)
    valid_share <- function(count) {
      count <- num(count)
      ifelse(
        is.finite(count) & is.finite(total) & total > 0 &
          count >= 0 & count <= total,
        100 * count / total,
        NA_real_
      )
    }
    if ("dise_single_teacher_schools" %in% names(out)) {
      out$dise_single_teacher_school_share <- valid_share(out$dise_single_teacher_schools)
    }
    if ("dise_girls_toilet_schools" %in% names(out)) {
      out$dise_girls_toilet_school_share <- valid_share(out$dise_girls_toilet_schools)
    }
  }
  if (all(c("dise_total_enrollment", "dise_total_teachers") %in% names(out))) {
    enrollment <- num(out$dise_total_enrollment)
    teachers <- num(out$dise_total_teachers)
    out$dise_pupils_per_teacher <- ifelse(
      is.finite(enrollment) & enrollment >= 0 & is.finite(teachers) & teachers > 0,
      enrollment / teachers,
      NA_real_
    )
  }
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
  finalize_dise_school_quality_measures(
    merge(enrolment, schools, by = key, all.x = TRUE, sort = FALSE)
  )
}

read_dise_baseline_teacher_year <- function(paths, registry_row) {
  year <- registry_row$academic_year[[1]]
  sheet <- registry_row$teacher_sheet[[1]]
  if (!is.character(sheet) || length(sheet) != 1L || is.na(sheet) || !nzchar(sheet)) {
    stop("DISE baseline teacher sheet is not registered for ", year, ".", call. = FALSE)
  }
  tryCatch(
    {
      workbook <- materialize_dise_workbook(paths, registry_row)
      out <- extract_dise_teacher_measures(read_dise_machine_sheet(workbook, sheet))
      out$academic_year <- year
      if (anyDuplicated(out[c("academic_year", "district_code_dise")])) {
        stop("DISE teacher sheet contains duplicate district codes.", call. = FALSE)
      }
      out
    },
    error = function(e) {
      stop(
        "DISE baseline teacher year ", year, " [sheet=", sheet, "] failed: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

read_dise_baseline_teacher_archive <- function(
  paths = build_paths(),
  registry = read_dise_archive_registry(paths)
) {
  rows <- registry[registry$analytic_role == "baseline_treatment", , drop = FALSE]
  safe_bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    read_dise_baseline_teacher_year(paths, rows[i, , drop = FALSE])
  }))
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

read_dise_report_language_enrollment <- function(
  paths = build_paths(),
  path = path_metadata(paths, "dise_report_language_enrollment.csv")
) {
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

finalize_dise_language_measure <- function(data) {
  out <- safe_df(data)
  total <- num(out$dise_total_enrollment)
  english <- num(out$dise_english_enrollment)
  hindi <- num(out$dise_hindi_enrollment)

  within_total <- function(count) {
    is.finite(count) &
      is.finite(total) &
      total > 0 &
      count >= 0 &
      count <= total + 1e-8
  }

  english_previously_resolved <- if ("dise_english_identity_resolved" %in% names(out)) {
    out$dise_english_identity_resolved %in% TRUE
  } else {
    is.finite(english)
  }
  hindi_previously_resolved <- if ("dise_hindi_identity_resolved" %in% names(out)) {
    out$dise_hindi_identity_resolved %in% TRUE
  } else {
    is.finite(hindi)
  }

  out$dise_english_count_valid <- within_total(english)
  out$dise_hindi_count_valid <- within_total(hindi)
  out$dise_english_identity_resolved <-
    english_previously_resolved & out$dise_english_count_valid
  out$dise_hindi_identity_resolved <-
    hindi_previously_resolved & out$dise_hindi_count_valid

  out$dise_emi_enrollment_share_total <- ifelse(
    out$dise_english_identity_resolved,
    100 * english / total,
    NA_real_
  )
  out$dise_hindi_enrollment_share_total <- ifelse(
    out$dise_hindi_identity_resolved,
    100 * hindi / total,
    NA_real_
  )
  both <- out$dise_english_identity_resolved & out$dise_hindi_identity_resolved
  english_hindi <- english + hindi
  out$dise_english_share_english_hindi <- ifelse(
    both & is.finite(english_hindi) & english_hindi > 0,
    100 * english / english_hindi,
    NA_real_
  )
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
  finalize_dise_language_measure(out)
}

extract_dise_2015_medium_counts <- function(data) {
  data <- safe_df(data)
  code_columns <- paste0("m", 1:5)
  missing <- setdiff(c("distcd", code_columns), names(data))
  if (length(missing)) {
    stop(
      "DISE 2015-16 medium sheet is missing canonical columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  out <- data.frame(
    district_code_dise = plain_chr(data$distcd),
    stringsAsFactors = FALSE
  )
  for (slot in 1:5) {
    count_columns <- intersect(paste0("enre", slot, 1:7), names(data))
    out[[paste0("medium_code_", slot)]] <- num(data[[code_columns[[slot]]]])
    out[[paste0("medium_enrollment_", slot)]] <-
      row_sum_available(data, count_columns)
  }
  language_count <- function(code) {
    vapply(seq_len(nrow(out)), function(i) {
      codes <- vapply(
        1:5,
        function(slot) out[[paste0("medium_code_", slot)]][[i]],
        numeric(1)
      )
      counts <- vapply(
        1:5,
        function(slot) out[[paste0("medium_enrollment_", slot)]][[i]],
        numeric(1)
      )
      identified <- is.finite(codes) & codes > 0
      unresolved_positive <- !identified & is.finite(counts) & counts > 0
      unresolved_identified <- identified & !is.finite(counts)
      if (any(unresolved_positive) || any(unresolved_identified)) {
        return(NA_real_)
      }

      hit <- identified & codes == code
      if (!any(hit)) return(0)

      matched <- counts[hit]
      if (any(!is.finite(matched))) return(NA_real_)
      sum(matched)
    }, numeric(1))
  }
  out$dise_english_enrollment <- language_count(19)
  out$dise_hindi_enrollment <- language_count(4)
  out
}

read_dise_dynamic_year <- function(paths, registry_row, report_languages) {
  workbook <- materialize_dise_workbook(paths, registry_row)
  year <- registry_row$academic_year[[1]]

  if (year %in% c("2014-15", "2015-16")) {
    sheet <- if (identical(year, "2015-16")) "2015-16_1" else "2014-15_PY"
    base <- read_dise_machine_sheet(workbook, sheet)
    out <- merge(
      extract_dise_direct_total(base, year),
      extract_dise_school_measures(base),
      by = "district_code_dise", all.x = TRUE, sort = FALSE
    )
    out <- merge(
      out, extract_dise_teacher_measures(base),
      by = "district_code_dise", all.x = TRUE, sort = FALSE
    )
    if (identical(year, "2015-16")) {
      out <- merge(
        out,
        extract_dise_2015_medium_counts(read_dise_machine_sheet(workbook, "2015-16_2")),
        by = "district_code_dise", all.x = TRUE, sort = FALSE
      )
      out$dise_english_identity_resolved <- is.finite(out$dise_english_enrollment)
      out$dise_hindi_identity_resolved <- is.finite(out$dise_hindi_enrollment)
      return(finalize_dise_school_quality_measures(finalize_dise_language_measure(out)))
    }
    out <- attach_dise_report_language_counts(
      out,
      report_languages[report_languages$academic_year == year, , drop = FALSE]
    )
    return(finalize_dise_school_quality_measures(out))
  }

  out <- merge(
    extract_dise_direct_total(
      read_dise_machine_sheet(workbook, registry_row$enrollment_sheet[[1]]),
      year
    ),
    extract_dise_school_measures(
      read_dise_machine_sheet(workbook, registry_row$school_sheet[[1]])
    ),
    by = "district_code_dise", all.x = TRUE, sort = FALSE
  )
  out <- merge(
    out,
    extract_dise_teacher_measures(
      read_dise_machine_sheet(workbook, registry_row$teacher_sheet[[1]])
    ),
    by = "district_code_dise", all.x = TRUE, sort = FALSE
  )
  out <- attach_dise_report_language_counts(
    out,
    report_languages[report_languages$academic_year == year, , drop = FALSE]
  )
  finalize_dise_school_quality_measures(out)
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
    row <- rows[i, , drop = FALSE]
    tryCatch(
      read_dise_dynamic_year(paths, row, report_languages),
      error = function(e) {
        stop(
          "DISE dynamic year ",
          row$academic_year[[1]],
          " failed: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }))
}
