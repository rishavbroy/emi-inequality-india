# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' diagnose district matching
#'
#' Port the legacy Chunk 20 district-matching diagnostics: unmatched rows,
#' many-to-many / flagged cases, panel-vs-join row counts, and a searchable table
#' of state/district/source names for close-match inspection.  Attributes on the
#' join map are read before coercion because as.data.frame() can strip the
#' legacy-compatible unmatched/flagged diagnostics created by the matcher.
diagnose_district_matching <- function(district_panel, district_join_map, cfg) {
  unmatched <- extract_unmatched_districts(district_join_map)
  manual <- extract_manual_matches(district_join_map)
  many <- extract_many_to_many_cases(district_join_map)

  panel <- as.data.frame(if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else district_panel, stringsAsFactors = FALSE)
  join_map <- as.data.frame(district_join_map, stringsAsFactors = FALSE)
  key_comparison <- compare_join_keys_to_panel(panel, join_map)

  base <- data.frame(
    n_panel_rows = nrow(panel),
    n_join_rows = nrow(join_map),
    n_unmatched_rows = nrow(unmatched),
    n_many_to_many_cases = nrow(many),
    n_panel_unmatched_by_key = sum(key_comparison$panel_key_status == "not_in_join_map", na.rm = TRUE),
    n_join_unmatched_by_key = sum(key_comparison$join_key_status == "not_in_panel", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  attr(base, "unmatched_rows") <- unmatched
  attr(base, "manual_matches") <- manual
  attr(base, "many_to_many_cases") <- many
  attr(base, "tracker_panel_comparison") <- compare_tracker_to_matched_panel(panel, join_map, unmatched, many)
  attr(base, "all_rows_search") <- build_district_matching_search_table(panel, join_map, unmatched)
  attr(base, "key_comparison") <- key_comparison
  attr(base, "legacy_reference") <- legacy_district_matching_reference(base)
  class(base) <- c("emi_district_matching_diagnostics", class(base))
  base
}

extract_unmatched_districts <- function(join_map, ...) {
  out <- attr(join_map, "unmatched_rows", exact = TRUE)
  if (!is.null(out)) return(as.data.frame(out, stringsAsFactors = FALSE))

  join_map <- as.data.frame(join_map, stringsAsFactors = FALSE)
  if (!nrow(join_map)) return(data.frame())
  if ("match_status" %in% names(join_map)) {
    status <- as.character(join_map$match_status)
    keep <- grepl("unmatched|failed", status, ignore.case = TRUE) &
      !grepl("legacy_tracker_row|matched|exact_name|fuzzy_name", status, ignore.case = TRUE)
    return(join_map[keep, , drop = FALSE])
  }
  data.frame()
}

extract_manual_matches <- function(join_map, ...) {
  join_map <- as.data.frame(join_map, stringsAsFactors = FALSE)
  if (!nrow(join_map)) return(data.frame())
  cols <- names(join_map)
  keep <- rep(FALSE, nrow(join_map))
  if ("match_status" %in% cols) keep <- keep | grepl("manual|correct", join_map$match_status, ignore.case = TRUE)
  if ("source" %in% cols) keep <- keep | grepl("manual", join_map$source, ignore.case = TRUE)
  join_map[keep, , drop = FALSE]
}

extract_many_to_many_cases <- function(join_map, ...) {
  out <- attr(join_map, "many_to_many_cases", exact = TRUE)
  if (!is.null(out)) return(as.data.frame(out, stringsAsFactors = FALSE))

  join_map <- as.data.frame(join_map, stringsAsFactors = FALSE)
  if (!nrow(join_map)) return(data.frame())
  if ("many_to_many" %in% names(join_map)) {
    return(join_map[!is.na(join_map$many_to_many) & join_map$many_to_many %in% TRUE, , drop = FALSE])
  }
  data.frame()
}

compare_tracker_to_matched_panel <- function(district_panel, district_join_map, unmatched_rows = NULL, many_to_many_cases = NULL, ...) {
  panel <- as.data.frame(district_panel, stringsAsFactors = FALSE)
  join_map <- as.data.frame(district_join_map, stringsAsFactors = FALSE)
  unmatched_rows <- unmatched_rows %||% extract_unmatched_districts(district_join_map)
  many_to_many_cases <- many_to_many_cases %||% extract_many_to_many_cases(district_join_map)
  data.frame(
    object = c("district_panel", "district_join_map", "unmatched_rows", "many_to_many_cases"),
    n_rows = c(nrow(panel), nrow(join_map), nrow(unmatched_rows), nrow(many_to_many_cases)),
    n_complete_rows = c(sum(stats::complete.cases(panel)), sum(stats::complete.cases(join_map)), NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
}

canonical_match_key <- function(df, state_candidates, district_candidates) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  state_col <- first_col(df, state_candidates)
  district_col <- first_col(df, district_candidates)
  if (is.null(state_col) || is.null(district_col)) return(rep(NA_character_, nrow(df)))
  paste(canon(df[[state_col]]), canon(df[[district_col]]), sep = "__")
}

compare_join_keys_to_panel <- function(panel, join_map) {
  panel <- as.data.frame(panel, stringsAsFactors = FALSE)
  join_map <- as.data.frame(join_map, stringsAsFactors = FALSE)
  if (!nrow(panel) || !nrow(join_map)) return(data.frame())

  panel_key <- canonical_match_key(panel, c("state_20", "state_18", "state_17", "state_07", "state_std", "state_0708"), c("district_20", "district_18", "district_17", "district_07", "district_std", "district_0708"))
  join_key <- canonical_match_key(join_map, c("state_20", "state_18", "state_17", "state_07", "state_std", "state_0708", ".tracker_state_key"), c("district_20", "district_18", "district_17", "district_07", "district_std", "district_0708", ".tracker_district_key"))
  panel_key <- panel_key[!is.na(panel_key) & nzchar(panel_key)]
  join_key <- join_key[!is.na(join_key) & nzchar(join_key)]
  if (!length(panel_key) || !length(join_key)) return(data.frame())

  data.frame(
    key = sort(unique(c(panel_key, join_key))),
    panel_key_status = ifelse(sort(unique(c(panel_key, join_key))) %in% panel_key, "in_panel", "not_in_panel"),
    join_key_status = ifelse(sort(unique(c(panel_key, join_key))) %in% join_key, "in_join_map", "not_in_join_map"),
    stringsAsFactors = FALSE
  )
}

build_district_matching_search_table <- function(district_panel, district_join_map, unmatched_rows = NULL) {
  dfs <- list(panel = district_panel, join_map = district_join_map, unmatched = unmatched_rows %||% extract_unmatched_districts(district_join_map))
  safe_bind_rows(lapply(names(dfs), function(source) {
    df <- as.data.frame(dfs[[source]], stringsAsFactors = FALSE)
    if (!nrow(df)) return(data.frame())
    state_cols <- grep("^state|state_", names(df), value = TRUE, ignore.case = TRUE)
    district_cols <- grep("^district|district_", names(df), value = TRUE, ignore.case = TRUE)
    state <- if (length(state_cols)) do.call(dplyr::coalesce, lapply(df[state_cols], as.character)) else rep(NA_character_, nrow(df))
    district <- if (length(district_cols)) do.call(dplyr::coalesce, lapply(df[district_cols], as.character)) else rep(NA_character_, nrow(df))
    data.frame(state = state, district = district, source = source, stringsAsFactors = FALSE)
  }))
}

legacy_district_matching_reference <- function(summary) {
  data.frame(
    diagnostic = c("unmatched_rows", "many_to_many_cases", "all_rows_search"),
    legacy_chunk = "Chunk 20 Match districts: Diagnose errors",
    current_value = c(summary$n_unmatched_rows, summary$n_many_to_many_cases, NA_integer_),
    interpretation = c(
      "Uses legacy matcher attributes when available; only falls back to match_status when attributes are absent.",
      "Uses explicit many_to_many attributes/flags rather than treating every source-key row as a many-to-many case.",
      "Search table preserves the legacy View()-style close-match inspection in a CSV artifact."
    ),
    stringsAsFactors = FALSE
  )
}

save_district_matching_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/district_matching") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    summary = write_diagnostic_csv(as.data.frame(diagnostics), file.path(dir, "district_matching_summary.csv")),
    unmatched_rows = write_diagnostic_csv(attr(diagnostics, "unmatched_rows") %||% data.frame(), file.path(dir, "district_matching_unmatched_rows.csv")),
    manual_matches = write_diagnostic_csv(attr(diagnostics, "manual_matches") %||% data.frame(), file.path(dir, "district_matching_manual_matches.csv")),
    many_to_many = write_diagnostic_csv(attr(diagnostics, "many_to_many_cases") %||% data.frame(), file.path(dir, "district_matching_many_to_many_cases.csv")),
    tracker_panel_comparison = write_diagnostic_csv(attr(diagnostics, "tracker_panel_comparison") %||% data.frame(), file.path(dir, "district_matching_tracker_panel_comparison.csv")),
    key_comparison = write_diagnostic_csv(attr(diagnostics, "key_comparison") %||% data.frame(), file.path(dir, "district_matching_key_comparison.csv")),
    all_rows_search = write_diagnostic_csv(attr(diagnostics, "all_rows_search") %||% data.frame(), file.path(dir, "district_matching_all_rows_search.csv")),
    legacy_reference = write_diagnostic_csv(attr(diagnostics, "legacy_reference") %||% data.frame(), file.path(dir, "district_matching_legacy_reference.csv"))
  )
  legacy_output_manifest(paths)
}
