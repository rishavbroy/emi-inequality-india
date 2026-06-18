# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' diagnose district tracker sources
#'
#' Preserve the legacy tracker-source QA from Chunk 6: source row coverage,
#' state/UT changes recorded in the tracker, unrecorded historical state-name
#' changes documented in comments, districts changing names within sample periods,
#' and same-name districts occurring across multiple states.
diagnose_district_tracker_sources <- function(raw_district_changes, district_tracker, cfg) {
  source_counts <- data.frame(
    source_file_id = names(raw_district_changes),
    n_rows = vapply(raw_district_changes, function(x) nrow(as.data.frame(x)), integer(1)),
    stringsAsFactors = FALSE
  )
  tracker <- as.data.frame(district_tracker, stringsAsFactors = FALSE)
  out <- source_counts
  attr(out, "state_changes") <- detect_tracker_state_changes(tracker)
  attr(out, "unrecorded_state_changes") <- legacy_unrecorded_state_changes()
  attr(out, "inperiod_district_changes") <- detect_inperiod_district_changes(tracker)
  attr(out, "same_name_districts") <- find_same_name_districts(tracker)
  attr(out, "source_disagreements") <- find_source_disagreements(raw_district_changes, tracker)
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

detect_tracker_state_changes <- function(tracker) {
  tracker <- as.data.frame(tracker, stringsAsFactors = FALSE)
  state_cols <- grep("^state_[0-9]{2}$", names(tracker), value = TRUE)
  if (!length(state_cols) || !nrow(tracker)) return(data.frame())
  changed <- apply(tracker[state_cols], 1, function(x) length(unique(stats::na.omit(as.character(x)))) > 1L)
  out <- tracker[changed, state_cols, drop = FALSE]
  if (!nrow(out)) return(data.frame())
  out$.tracker_row <- which(changed)
  out
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

detect_inperiod_district_changes <- function(tracker) {
  tracker <- as.data.frame(tracker, stringsAsFactors = FALSE)
  pairs <- list(c("05", "06"), c("07", "08"), c("17", "18"), c("19", "20"))
  rows <- lapply(pairs, function(pair) {
    a <- paste0("district_", pair[[1]])
    b <- paste0("district_", pair[[2]])
    if (!all(c(a, b) %in% names(tracker))) return(data.frame())
    changed <- !is.na(tracker[[a]]) & !is.na(tracker[[b]]) & tracker[[a]] != tracker[[b]]
    if (!any(changed)) return(data.frame())
    data.frame(
      .tracker_row = which(changed),
      period = paste(pair, collapse = "_to_"),
      district_start = tracker[[a]][changed],
      district_end = tracker[[b]][changed],
      stringsAsFactors = FALSE
    )
  })
  safe_bind_rows(rows)
}

find_same_name_districts <- function(tracker) {
  tracker <- as.data.frame(tracker, stringsAsFactors = FALSE)
  suffixes <- sub("^district_", "", grep("^district_[0-9]{2}$", names(tracker), value = TRUE))
  out <- lapply(suffixes, function(sfx) {
    state_col <- paste0("state_", sfx)
    district_col <- paste0("district_", sfx)
    if (!all(c(state_col, district_col) %in% names(tracker))) return(data.frame())
    temp <- tracker[c(state_col, district_col)]
    names(temp) <- c("state", "district")
    temp <- temp[!is.na(temp$district) & nzchar(as.character(temp$district)), , drop = FALSE]
    if (!nrow(temp)) return(data.frame())
    split_d <- split(temp, temp$district)
    safe_bind_rows(lapply(split_d, function(x) {
      n_states <- length(unique(stats::na.omit(as.character(x$state))))
      if (n_states <= 1L) return(data.frame())
      data.frame(year_suffix = sfx, district_name = x$district[[1]], n_districts = nrow(x), n_states = n_states, states = paste(sort(unique(x$state)), collapse = "; "), stringsAsFactors = FALSE)
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

summarize_tracker_source_errors <- function(raw_district_changes, district_tracker = NULL) {
  diag <- diagnose_district_tracker_sources(raw_district_changes, district_tracker %||% data.frame(), list())
  data.frame(
    diagnostic = c("state_changes", "inperiod_district_changes", "same_name_districts"),
    n = c(nrow(attr(diag, "state_changes")), nrow(attr(diag, "inperiod_district_changes")), nrow(attr(diag, "same_name_districts"))),
    stringsAsFactors = FALSE
  )
}

save_tracker_source_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/district_tracker_sources") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    source_counts = write_diagnostic_csv(as.data.frame(diagnostics), file.path(dir, "tracker_source_counts.csv")),
    state_changes = write_diagnostic_csv(attr(diagnostics, "state_changes") %||% data.frame(), file.path(dir, "tracker_state_changes.csv")),
    unrecorded_state_changes = write_diagnostic_csv(attr(diagnostics, "unrecorded_state_changes") %||% data.frame(), file.path(dir, "tracker_unrecorded_state_changes.csv")),
    inperiod_district_changes = write_diagnostic_csv(attr(diagnostics, "inperiod_district_changes") %||% data.frame(), file.path(dir, "tracker_inperiod_district_changes.csv")),
    same_name_districts = write_diagnostic_csv(attr(diagnostics, "same_name_districts") %||% data.frame(), file.path(dir, "tracker_same_name_districts.csv")),
    source_disagreements = write_diagnostic_csv(attr(diagnostics, "source_disagreements") %||% data.frame(), file.path(dir, "tracker_source_disagreements.csv"))
  )
  legacy_output_manifest(paths)
}
