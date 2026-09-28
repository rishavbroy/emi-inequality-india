# This file is part of the EMI inequality research pipeline.
# Manual corrections are restricted to explicit source/target identity fields.

manual_correction_field_map <- function(side) {
  side <- tolower(trimws(plain_chr(side)[[1L]]))
  fields <- list(
    source = c(
      state = "source_state_raw",
      district = "source_district_raw",
      year = "source_year_raw"
    ),
    target = c(
      state = "target_state_raw",
      district = "target_district_raw",
      year = "target_year_raw"
    )
  )
  fields[[side]] %||% NULL
}

manual_name_correction_types <- function() {
  c("typo", "spelling", "standardization", "rename", "name_change")
}

#' Apply manual district corrections
#'
apply_manual_district_corrections <- function(
    tracker,
    corrections_path = "data/metadata/manual_district_corrections.csv") {
  tracker <- safe_df(tracker)
  if (!file.exists(corrections_path)) return(tracker)
  corrections <- utils::read.csv(corrections_path, stringsAsFactors = FALSE)
  validate_manual_corrections(corrections, tracker)
  corrections <- active_manual_corrections(corrections)
  audit <- build_manual_correction_audit(tracker, corrections)

  for (i in seq_len(nrow(corrections))) {
    tracker <- apply_single_name_correction(
      tracker, corrections[i, , drop = FALSE]
    )
  }
  tracker <- standardize_tracker_names(tracker)

  attr(tracker, "manual_corrections") <- corrections
  attr(tracker, "manual_correction_audit") <- audit
  tracker
}

#' Validate manual corrections
#'
validate_manual_corrections <- function(corrections, tracker) {
  corrections <- safe_df(corrections)
  required <- c(
    "correction_id", "source_dataset", "match_year", "side",
    "state_raw", "district_raw", "state_corrected", "district_corrected",
    "correction_type", "reason"
  )
  missing <- setdiff(required, names(corrections))
  if (length(missing)) {
    stop("Manual corrections missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(corrections)) return(invisible(TRUE))
  if (any(!nzchar(trimws(plain_chr(corrections$correction_id))))) {
    stop("Manual corrections require non-empty correction_id values.", call. = FALSE)
  }
  if (any(!nzchar(trimws(plain_chr(corrections$reason))))) {
    stop("Manual corrections require documented reasons.", call. = FALSE)
  }
  if (anyDuplicated(plain_chr(corrections$correction_id))) {
    stop("Manual corrections require unique correction_id values.", call. = FALSE)
  }
  if (any(!nzchar(trimws(plain_chr(corrections$state_raw)))) ||
      any(!nzchar(trimws(plain_chr(corrections$district_raw))))) {
    stop("Manual corrections require explicit state_raw and district_raw match values.", call. = FALSE)
  }

  sides <- tolower(trimws(plain_chr(corrections$side)))
  if (any(!sides %in% c("source", "target"))) {
    stop("Manual corrections require side = 'source' or 'target'.", call. = FALSE)
  }
  types <- manual_correction_type(corrections)
  invalid_types <- setdiff(unique(types), manual_name_correction_types())
  if (length(invalid_types)) {
    stop(
      "Manual district corrections are identity/name corrections only; lineage events belong in reviewed lineage sources. Unsupported correction_type: ",
      paste(invalid_types, collapse = ", "),
      call. = FALSE
    )
  }

  required_tracker_fields <- unique(unlist(lapply(sides, function(side) {
    fields <- manual_correction_field_map(side)
    unname(fields[c("state", "district", "year")])
  }), use.names = FALSE))
  missing_tracker <- setdiff(required_tracker_fields, names(tracker))
  if (length(missing_tracker)) {
    stop(
      "Manual correction tracker is missing explicit fields: ",
      paste(missing_tracker, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

active_manual_corrections <- function(corrections) {
  corrections <- safe_df(corrections)
  if (!nrow(corrections)) return(corrections)
  if (!"applied" %in% names(corrections)) corrections$applied <- TRUE
  keep <- is.na(corrections$applied) |
    tolower(as.character(corrections$applied)) %in% c("", "true", "t", "1", "yes", "y")
  corrections[keep, , drop = FALSE]
}

manual_correction_row_match <- function(tracker, correction) {
  tracker <- safe_df(tracker)
  fields <- manual_correction_field_map(correction$side)
  if (is.null(fields)) return(rep(FALSE, nrow(tracker)))

  state_key <- canonicalize_state_name(correction$state_raw)
  district_key <- canon(correction$district_raw)
  row_match <- canonicalize_state_name(tracker[[fields[["state"]]]]) == state_key &
    canon(tracker[[fields[["district"]]]]) == district_key

  if (manual_scalar_has_value(correction$source_dataset) &&
      any(c("source_file_id", "source_type") %in% names(tracker))) {
    dataset_key <- canon(correction$source_dataset)
    dataset_match <- rep(FALSE, nrow(tracker))
    if ("source_file_id" %in% names(tracker)) {
      dataset_match <- dataset_match | canon(tracker$source_file_id) == dataset_key
    }
    if ("source_type" %in% names(tracker)) {
      dataset_match <- dataset_match | canon(tracker$source_type) == dataset_key
    }
    row_match <- row_match & dataset_match
  }
  if (manual_scalar_has_value(correction$match_year)) {
    row_match <- row_match &
      suppressWarnings(as.integer(tracker[[fields[["year"]]]])) ==
      suppressWarnings(as.integer(correction$match_year))
  }
  row_match[is.na(row_match)] <- FALSE
  row_match
}

apply_single_name_correction <- function(tracker, correction) {
  tracker <- safe_df(tracker)
  correction <- safe_df(correction)
  if (!nrow(tracker) || nrow(correction) != 1L) return(tracker)

  fields <- manual_correction_field_map(correction$side)
  row_match <- manual_correction_row_match(tracker, correction)
  if (!any(row_match)) return(tracker)

  state_corrected <- if (manual_scalar_has_value(correction$state_corrected)) {
    plain_chr(correction$state_corrected)[[1L]]
  } else {
    plain_chr(correction$state_raw)[[1L]]
  }
  district_corrected <- if (manual_scalar_has_value(correction$district_corrected)) {
    plain_chr(correction$district_corrected)[[1L]]
  } else {
    plain_chr(correction$district_raw)[[1L]]
  }
  tracker[[fields[["state"]]]][row_match] <- state_corrected
  tracker[[fields[["district"]]]][row_match] <- district_corrected
  tracker
}

manual_correction_type <- function(corrections) {
  tolower(gsub("[^a-z0-9]+", "_", as.character(corrections$correction_type %||% "")))
}

build_manual_correction_audit <- function(tracker, corrections) {
  tracker <- safe_df(tracker)
  corrections <- safe_df(corrections)
  if (!nrow(corrections)) return(data.frame())
  safe_bind_rows(lapply(seq_len(nrow(corrections)), function(i) {
    corr <- corrections[i, , drop = FALSE]
    data.frame(
      correction_id = corr$correction_id,
      correction_type = corr$correction_type,
      side = corr$side,
      n_matching_rows_before = sum(manual_correction_row_match(tracker, corr)),
      reason = corr$reason,
      stringsAsFactors = FALSE
    )
  }))
}

manual_scalar_has_value <- function(x) {
  length(x) > 0L && !is.na(x[[1]]) && nzchar(trimws(as.character(x[[1]])))
}
