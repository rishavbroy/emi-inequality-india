# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' diagnose district tracker sources
#'
#' Preserve the legacy tracker-source QA from Chunk 6: source row coverage,
#' state/UT changes recorded in the tracker, unrecorded historical state-name
#' changes documented in comments, districts changing names within sample
#' periods, and same-name districts occurring across multiple states.  The
#' diagnostic records both row-level current results and the legacy comment
#' benchmarks so a changed tracker can be distinguished from a missing port.
diagnose_district_tracker_sources <- function(raw_district_changes, district_tracker, cfg) {
  source_counts <- compare_tracker_source_coverage(raw_district_changes, district_tracker)
  tracker <- as.data.frame(district_tracker, stringsAsFactors = FALSE)

  state_changes <- safe_bind_rows(list(
    detect_tracker_state_changes(tracker),
    detect_raw_tracker_state_changes(raw_district_changes)
  ))
  inperiod <- safe_bind_rows(list(
    detect_inperiod_district_changes(tracker),
    detect_raw_inperiod_district_changes(raw_district_changes)
  ))
  same_names <- find_same_name_districts(tracker)
  legacy_expected_state_changes <- legacy_recorded_state_changes()
  legacy_expected_inperiod <- legacy_inperiod_district_changes_reference()
  legacy_expected_same_names <- legacy_same_name_districts_reference()
  legacy_reference <- legacy_tracker_comment_reference(
    detected_state_change_rows = nrow(state_changes),
    detected_inperiod_rows = nrow(inperiod),
    detected_same_name_rows = nrow(same_names)
  )

  out <- source_counts
  attr(out, "state_changes") <- state_changes
  attr(out, "state_change_events") <- summarize_tracker_state_change_events(state_changes)
  attr(out, "unrecorded_state_changes") <- legacy_unrecorded_state_changes()
  attr(out, "legacy_expected_state_changes") <- legacy_expected_state_changes
  attr(out, "inperiod_district_changes") <- inperiod
  attr(out, "legacy_expected_inperiod_district_changes") <- legacy_expected_inperiod
  attr(out, "same_name_districts") <- same_names
  attr(out, "legacy_expected_same_name_districts") <- legacy_expected_same_names
  attr(out, "source_disagreements") <- find_source_disagreements(raw_district_changes, tracker)
  attr(out, "legacy_reference") <- legacy_reference
  class(out) <- c("emi_tracker_source_diagnostics", class(out))
  out
}

compare_tracker_source_coverage <- function(raw_district_changes, district_tracker = NULL) {
  data.frame(
    source_file_id = names(raw_district_changes),
    n_rows = vapply(raw_district_changes, function(x) nrow(as.data.frame(x)), integer(1)),
    n_columns = vapply(raw_district_changes, function(x) ncol(as.data.frame(x)), integer(1)),
    stringsAsFactors = FALSE
  )
}

tracker_year_suffixes <- function(tracker, prefix = "state") {
  cols <- grep(paste0("^", prefix, "_([0-9]{2}|[0-9]{4})$"), names(tracker), value = TRUE)
  suffixes <- sub(paste0("^", prefix, "_"), "", cols)
  suffixes[order(suppressWarnings(as.integer(ifelse(nchar(suffixes) == 2L, paste0("20", suffixes), suffixes))))]
}

tracker_suffix_year <- function(sfx) {
  yr <- suppressWarnings(as.integer(sfx))
  ifelse(nchar(sfx) == 2L, ifelse(yr <= 30L, 2000L + yr, 1900L + yr), yr)
}

tracker_value <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

detect_tracker_state_changes <- function(tracker) {
  tracker <- as.data.frame(tracker, stringsAsFactors = FALSE)
  suffixes <- tracker_year_suffixes(tracker, "state")
  state_cols <- paste0("state_", suffixes)
  state_cols <- state_cols[state_cols %in% names(tracker)]
  if (!length(state_cols) || !nrow(tracker)) return(data.frame())

  changed <- apply(tracker[state_cols], 1, function(x) {
    vals <- unique(tracker_value(x))
    vals <- vals[nzchar(vals)]
    length(vals) > 1L
  })
  if (!any(changed)) return(data.frame())

  rows <- tracker[changed, , drop = FALSE]
  safe_bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    vals <- tracker_value(rows[i, state_cols, drop = TRUE])
    vals <- vals[nzchar(vals)]
    data.frame(
      tracker_row = as.integer(rownames(rows)[[i]]),
      years = paste(tracker_suffix_year(suffixes[seq_along(tracker_value(rows[i, state_cols, drop = TRUE]))]), collapse = ";"),
      states = paste(unique(vals), collapse = " -> "),
      first_state = vals[[1]],
      last_state = vals[[length(vals)]],
      stringsAsFactors = FALSE
    )
  }))
}

summarize_tracker_state_change_events <- function(state_changes) {
  state_changes <- as.data.frame(state_changes, stringsAsFactors = FALSE)
  if (!nrow(state_changes)) return(data.frame())
  pairs <- paste(state_changes$first_state, state_changes$last_state, sep = " -> ")
  tab <- as.data.frame(table(state_transition = pairs), stringsAsFactors = FALSE)
  names(tab) <- c("state_transition", "n_rows")
  tab$n_rows <- as.integer(tab$n_rows)
  tab[order(-tab$n_rows, tab$state_transition), , drop = FALSE]
}

legacy_recorded_state_changes <- function() {
  data.frame(
    legacy_event = c(
      "Ladakh split from Jammu and Kashmir",
      "Dadra and Nagar Haveli and Daman and Diu merger"
    ),
    first_reflected = "2019 data",
    legacy_chunk = "Chunk 6 district tracker source QA",
    current_detection_status = "must be detected from raw/pre-correction tracker columns or carried as this reference row",
    stringsAsFactors = FALSE
  )
}

legacy_inperiod_district_changes_reference <- function() {
  data.frame(
    diagnostic = "in_period_district_name_changes",
    legacy_expected_rows = 16L,
    legacy_chunk = "Chunk 6 district tracker source QA",
    legacy_note = "Legacy comments counted rows where district_05 != district_06, district_07 != district_08, district_17 != district_18, or district_19 != district_20 before downstream corrections.",
    current_detection_status = "rendered analysis should compare this benchmark with current tracker_inperiod_district_changes.csv",
    stringsAsFactors = FALSE
  )
}


legacy_same_name_districts_reference <- function() {
  data.frame(
    diagnostic = "same_name_districts_across_states",
    legacy_expected_min_districts = 6L,
    legacy_expected_max_districts = 10L,
    legacy_chunk = "Chunk 6 district tracker source QA",
    legacy_note = "Legacy comments counted between min(n_same_name_districts$n) = 6 and max(n_same_name_districts$n) = 10 districts with shared names in each year of interest.",
    current_detection_status = "rendered analysis should compare this benchmark with tracker_same_name_districts.csv; a zero current count means the active tracker no longer exposes the raw same-name ambiguity, not that the legacy QA was irrelevant.",
    stringsAsFactors = FALSE
  )
}

legacy_unrecorded_state_changes <- function() {
  data.frame(
    change = c(
      "Pondicherry/Puducherry district and UT rename",
      "Uttaranchal/Uttarakhand state rename",
      "Orissa/Odisha state rename",
      "Telangana split from Andhra Pradesh"
    ),
    legacy_note = c(
      "Legacy comment: 2007-08 NSS still uses Pondicherry despite 2006 Puducherry rename.",
      "Legacy comment: 2007-08 NSS uses Uttaranchal rather than Uttarakhand.",
      "Legacy comment: apply pre-2011 Orissa naming when matching earlier samples.",
      "Legacy comment: apply Andhra Pradesh name before Telangana split when matching pre-2014 data."
    ),
    stringsAsFactors = FALSE
  )
}

detect_raw_tracker_state_changes <- function(raw_district_changes) {
  safe_bind_rows(lapply(names(raw_district_changes), function(source_id) {
    df <- as.data.frame(raw_district_changes[[source_id]], stringsAsFactors = FALSE)
    if (!nrow(df)) return(data.frame())
    out <- detect_tracker_state_changes(df)
    if (!nrow(out)) return(data.frame())
    out$source_file_id <- source_id
    out$detection_source <- "raw_district_change_source"
    out
  }))
}

detect_raw_inperiod_district_changes <- function(raw_district_changes) {
  safe_bind_rows(lapply(names(raw_district_changes), function(source_id) {
    df <- as.data.frame(raw_district_changes[[source_id]], stringsAsFactors = FALSE)
    if (!nrow(df)) return(data.frame())
    out <- detect_inperiod_district_changes(df)
    if (!nrow(out)) return(data.frame())
    out$source_file_id <- source_id
    out$detection_source <- "raw_district_change_source"
    out
  }))
}

detect_inperiod_district_changes <- function(tracker) {
  tracker <- as.data.frame(tracker, stringsAsFactors = FALSE)
  pairs <- list(c("05", "06"), c("07", "08"), c("17", "18"), c("19", "20"))
  rows <- lapply(pairs, function(pair) {
    a <- paste0("district_", pair[[1]])
    b <- paste0("district_", pair[[2]])
    if (!all(c(a, b) %in% names(tracker))) return(data.frame())
    av <- tracker_value(tracker[[a]])
    bv <- tracker_value(tracker[[b]])
    changed <- nzchar(av) & nzchar(bv) & av != bv
    if (!any(changed)) return(data.frame())
    state_a <- paste0("state_", pair[[1]])
    state_b <- paste0("state_", pair[[2]])
    data.frame(
      tracker_row = which(changed),
      period = paste(pair, collapse = "_to_"),
      state_start = if (state_a %in% names(tracker)) tracker[[state_a]][changed] else NA_character_,
      state_end = if (state_b %in% names(tracker)) tracker[[state_b]][changed] else NA_character_,
      district_start = tracker[[a]][changed],
      district_end = tracker[[b]][changed],
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(rows)
}

find_same_name_districts <- function(tracker) {
  tracker <- as.data.frame(tracker, stringsAsFactors = FALSE)
  suffixes <- tracker_year_suffixes(tracker, "district")
  out <- lapply(suffixes, function(sfx) {
    state_col <- paste0("state_", sfx)
    district_col <- paste0("district_", sfx)
    if (!all(c(state_col, district_col) %in% names(tracker))) return(data.frame())
    temp <- tracker[c(state_col, district_col)]
    names(temp) <- c("state", "district")
    temp$state <- tracker_value(temp$state)
    temp$district <- tracker_value(temp$district)
    temp <- temp[nzchar(temp$district), , drop = FALSE]
    if (!nrow(temp)) return(data.frame())
    split_d <- split(temp, temp$district)
    safe_bind_rows(lapply(split_d, function(x) {
      states <- sort(unique(x$state[nzchar(x$state)]))
      if (length(states) <= 1L) return(data.frame())
      data.frame(year_suffix = sfx, district_name = x$district[[1]], n_districts = nrow(x), n_states = length(states), states = paste(states, collapse = "; "), stringsAsFactors = FALSE)
    }))
  })
  safe_bind_rows(out)
}

find_source_disagreements <- function(raw_district_changes, district_tracker = NULL) {
  coverage <- compare_tracker_source_coverage(raw_district_changes, district_tracker)
  if (!nrow(coverage)) return(data.frame())
  coverage$diagnostic <- "source_coverage"
  coverage
}

legacy_tracker_comment_reference <- function(detected_state_change_rows, detected_inperiod_rows, detected_same_name_rows = 0L) {
  data.frame(
    diagnostic = c(
      "recorded_state_ut_changes",
      "unrecorded_state_ut_changes",
      "in_period_district_name_changes",
      "same_name_districts_across_states"
    ),
    legacy_comment_expected = c(
      "Legacy Chunk 6 comments identify two recorded state/UT change events in the tracker sources.",
      "Legacy Chunk 6 comments identify four unrecorded state/UT naming/split changes requiring manual attention.",
      "Legacy Chunk 6 comments record 16 districts changing names within the sampling periods.",
      "Legacy Chunk 6 comments record between 6 and 10 same-name districts in each year of interest."
    ),
    current_detected_rows = c(detected_state_change_rows, nrow(legacy_unrecorded_state_changes()), detected_inperiod_rows, detected_same_name_rows),
    interpretation = c(
      "Compare row-level current detections with tracker_state_change_events.csv because row counts can exceed event counts.",
      "These are preserved as documented legacy correction notes, not inferred from active tracker rows.",
      "A current count different from 16 reflects active tracker/correction changes and should be reviewed before describing it as an improvement.",
      "A current count of zero means the active cleaned tracker no longer exposes this raw ambiguity; it should be reported as resolved-by-current-cleaning, not as proof that the legacy QA was unnecessary."
    ),
    stringsAsFactors = FALSE
  )
}

summarize_tracker_source_errors <- function(raw_district_changes, district_tracker = NULL) {
  diag <- diagnose_district_tracker_sources(raw_district_changes, district_tracker %||% data.frame(), list())
  data.frame(
    diagnostic = c("state_changes", "state_change_events", "inperiod_district_changes", "same_name_districts"),
    n = c(nrow(attr(diag, "state_changes")), nrow(attr(diag, "state_change_events")), nrow(attr(diag, "inperiod_district_changes")), nrow(attr(diag, "same_name_districts"))),
    stringsAsFactors = FALSE
  )
}

save_tracker_source_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/district_tracker_sources") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    source_counts = write_diagnostic_csv(as.data.frame(diagnostics), file.path(dir, "tracker_source_counts.csv")),
    state_changes = write_diagnostic_csv(attr(diagnostics, "state_changes") %||% data.frame(), file.path(dir, "tracker_state_changes.csv")),
    state_change_events = write_diagnostic_csv(attr(diagnostics, "state_change_events") %||% data.frame(), file.path(dir, "tracker_state_change_events.csv")),
    unrecorded_state_changes = write_diagnostic_csv(attr(diagnostics, "unrecorded_state_changes") %||% data.frame(), file.path(dir, "tracker_unrecorded_state_changes.csv")),
    legacy_expected_state_changes = write_diagnostic_csv(attr(diagnostics, "legacy_expected_state_changes") %||% data.frame(), file.path(dir, "tracker_legacy_expected_state_changes.csv")),
    inperiod_district_changes = write_diagnostic_csv(attr(diagnostics, "inperiod_district_changes") %||% data.frame(), file.path(dir, "tracker_inperiod_district_changes.csv")),
    legacy_expected_inperiod_district_changes = write_diagnostic_csv(attr(diagnostics, "legacy_expected_inperiod_district_changes") %||% data.frame(), file.path(dir, "tracker_legacy_expected_inperiod_district_changes.csv")),
    same_name_districts = write_diagnostic_csv(attr(diagnostics, "same_name_districts") %||% data.frame(), file.path(dir, "tracker_same_name_districts.csv")),
    legacy_expected_same_name_districts = write_diagnostic_csv(attr(diagnostics, "legacy_expected_same_name_districts") %||% data.frame(), file.path(dir, "tracker_legacy_expected_same_name_districts.csv")),
    source_disagreements = write_diagnostic_csv(attr(diagnostics, "source_disagreements") %||% data.frame(), file.path(dir, "tracker_source_disagreements.csv")),
    legacy_reference = write_diagnostic_csv(attr(diagnostics, "legacy_reference") %||% data.frame(), file.path(dir, "tracker_legacy_comment_reference.csv"))
  )
  legacy_output_manifest(paths)
}
