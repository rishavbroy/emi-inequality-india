# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' apply manual district corrections
#'
apply_manual_district_corrections <- function(tracker, corrections_path = "data/metadata/manual_district_corrections.csv") {
  tracker <- safe_df(tracker)
  if (!file.exists(corrections_path)) return(tracker)
  corrections <- utils::read.csv(corrections_path, stringsAsFactors = FALSE)
  validate_manual_corrections(corrections, tracker)
  corrections <- active_manual_corrections(corrections)
  audit <- build_manual_correction_audit(tracker, corrections)
  tracker <- apply_typo_corrections(tracker, corrections)
  tracker <- apply_rename_corrections(tracker, corrections)
  tracker <- apply_split_merge_corrections(tracker, corrections)
  attr(tracker, "manual_corrections") <- corrections
  attr(tracker, "manual_correction_audit") <- audit
  tracker
}

#' validate manual corrections
#'
validate_manual_corrections <- function(corrections, tracker) {
  required <- c("correction_id", "source_year", "source_dataset", "state_raw", "district_raw", "correction_type", "reason")
  missing <- setdiff(required, names(corrections))
  if (length(missing)) stop("Manual corrections missing columns: ", paste(missing, collapse = ", "))
  if (any(!nzchar(as.character(corrections$correction_id)) & nrow(corrections))) stop("Manual corrections require non-empty correction_id values.", call. = FALSE)
  if (any(!nzchar(as.character(corrections$reason)) & nrow(corrections))) stop("Manual corrections require documented reasons.", call. = FALSE)
  invisible(TRUE)
}

active_manual_corrections <- function(corrections) {
  corrections <- safe_df(corrections)
  if (!nrow(corrections)) return(corrections)
  if (!"applied" %in% names(corrections)) corrections$applied <- TRUE
  keep <- is.na(corrections$applied) | tolower(as.character(corrections$applied)) %in% c("", "true", "t", "1", "yes", "y")
  corrections[keep, , drop = FALSE]
}

#' apply rename corrections
#'
apply_rename_corrections <- function(tracker, corrections) {
  apply_name_corrections_by_type(tracker, corrections, c("rename", "name_change"))
}

#' apply split merge corrections
#'
apply_split_merge_corrections <- function(tracker, corrections) {
  out <- apply_name_corrections_by_type(tracker, corrections, c("split", "merge", "split_merge", "carveout", "border_shift"))
  if (nrow(out) && nrow(corrections)) attr(out, "manual_split_merge_corrections") <- corrections[manual_correction_type(corrections) %in% c("split", "merge", "split_merge", "carveout", "border_shift"), , drop = FALSE]
  out
}

#' apply typo corrections
#'
apply_typo_corrections <- function(tracker, corrections) {
  apply_name_corrections_by_type(tracker, corrections, c("typo", "spelling", "standardization"))
}

apply_name_corrections_by_type <- function(tracker, corrections, types) {
  tracker <- safe_df(tracker)
  corrections <- safe_df(corrections)
  if (!nrow(tracker) || !nrow(corrections)) return(tracker)
  corrections <- corrections[manual_correction_type(corrections) %in% types, , drop = FALSE]
  if (!nrow(corrections)) return(tracker)

  for (i in seq_len(nrow(corrections))) {
    corr <- corrections[i, , drop = FALSE]
    state_raw <- corr$state_raw %||% NA_character_
    district_raw <- corr$district_raw %||% NA_character_
    state_corrected <- corr$state_corrected %||% state_raw
    district_corrected <- corr$district_corrected %||% district_raw
    if (!manual_scalar_has_value(state_corrected)) state_corrected <- state_raw
    if (!manual_scalar_has_value(district_corrected)) district_corrected <- district_raw
    tracker <- apply_single_name_correction(tracker, state_raw, district_raw, state_corrected, district_corrected, corr)
  }
  tracker
}

apply_single_name_correction <- function(tracker, state_raw, district_raw, state_corrected, district_corrected, correction) {
  state_cols <- grep("(^state(_|$)|_state$|state_raw|source_state_raw|target_state_raw)", names(tracker), value = TRUE, ignore.case = TRUE)
  district_cols <- grep("(^district(_|$)|_district$|district_raw|source_district_raw|target_district_raw|district_name)", names(tracker), value = TRUE, ignore.case = TRUE)
  state_key <- canonicalize_state_name(state_raw)
  district_key <- canon(district_raw)
  if (!manual_scalar_has_value(state_key) && !manual_scalar_has_value(district_key)) return(tracker)

  row_match <- rep(TRUE, nrow(tracker))
  if ("source_file_id" %in% names(tracker) && manual_scalar_has_value(correction$source_dataset)) {
    dataset_key <- canon(correction$source_dataset)
    row_match <- row_match & (canon(tracker$source_file_id) == dataset_key | canon(tracker$source_type %||% NA_character_) == dataset_key)
  }
  if ("source_year" %in% names(tracker) && manual_scalar_has_value(correction$source_year)) {
    row_match <- row_match & suppressWarnings(as.integer(tracker$source_year)) == suppressWarnings(as.integer(correction$source_year))
  }
  state_match <- if (length(state_cols)) Reduce(`|`, lapply(state_cols, function(col) canonicalize_state_name(tracker[[col]]) == state_key)) else rep(TRUE, nrow(tracker))
  district_match <- if (length(district_cols)) Reduce(`|`, lapply(district_cols, function(col) canon(tracker[[col]]) == district_key)) else rep(TRUE, nrow(tracker))
  row_match <- row_match & state_match & district_match

  for (col in state_cols) {
    hit <- row_match & canonicalize_state_name(tracker[[col]]) == state_key
    if (any(hit, na.rm = TRUE)) tracker[[col]][hit] <- state_corrected
  }
  for (col in district_cols) {
    hit <- row_match & canon(tracker[[col]]) == district_key
    if (any(hit, na.rm = TRUE)) tracker[[col]][hit] <- district_corrected
  }
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
    state_key <- canonicalize_state_name(corr$state_raw)
    district_key <- canon(corr$district_raw)
    state_cols <- grep("state", names(tracker), value = TRUE, ignore.case = TRUE)
    district_cols <- grep("district", names(tracker), value = TRUE, ignore.case = TRUE)
    state_hit <- if (length(state_cols)) Reduce(`|`, lapply(state_cols, function(col) canonicalize_state_name(tracker[[col]]) == state_key)) else rep(FALSE, nrow(tracker))
    district_hit <- if (length(district_cols)) Reduce(`|`, lapply(district_cols, function(col) canon(tracker[[col]]) == district_key)) else rep(FALSE, nrow(tracker))
    data.frame(
      correction_id = corr$correction_id,
      correction_type = corr$correction_type,
      n_matching_rows_before = sum(state_hit & district_hit, na.rm = TRUE),
      reason = corr$reason,
      stringsAsFactors = FALSE
    )
  }))
}

manual_scalar_has_value <- function(x) {
  length(x) > 0L && !is.na(x[[1]]) && nzchar(as.character(x[[1]]))
}
