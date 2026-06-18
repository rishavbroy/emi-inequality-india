# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' diagnose district matching
#'
#' Port the legacy Chunk 20 district-matching diagnostics: unmatched rows,
#' many-to-many / flagged cases, panel-vs-join row counts, and a searchable table
#' of state/district/source names for close-match inspection.
diagnose_district_matching <- function(district_panel, district_join_map, cfg) {
  panel <- as.data.frame(if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else district_panel, stringsAsFactors = FALSE)
  join_map <- as.data.frame(district_join_map, stringsAsFactors = FALSE)
  base <- data.frame(
    n_panel_rows = nrow(panel),
    n_join_rows = nrow(join_map),
    n_unmatched_rows = nrow(extract_unmatched_districts(join_map)),
    n_many_to_many_cases = nrow(extract_many_to_many_cases(join_map)),
    stringsAsFactors = FALSE
  )
  attr(base, "unmatched_rows") <- extract_unmatched_districts(join_map)
  attr(base, "manual_matches") <- extract_manual_matches(join_map)
  attr(base, "many_to_many_cases") <- extract_many_to_many_cases(join_map)
  attr(base, "tracker_panel_comparison") <- compare_tracker_to_matched_panel(panel, join_map)
  attr(base, "all_rows_search") <- build_district_matching_search_table(panel, join_map)
  class(base) <- c("emi_district_matching_diagnostics", class(base))
  base
}

extract_unmatched_districts <- function(join_map, ...) {
  out <- attr(join_map, "unmatched_rows")
  if (is.null(out)) {
    join_map <- as.data.frame(join_map, stringsAsFactors = FALSE)
    if ("match_status" %in% names(join_map)) out <- join_map[grepl("unmatched|failed", join_map$match_status, ignore.case = TRUE), , drop = FALSE]
  }
  as.data.frame(out %||% data.frame(), stringsAsFactors = FALSE)
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
  out <- attr(join_map, "many_to_many_cases")
  if (is.null(out)) {
    join_map <- as.data.frame(join_map, stringsAsFactors = FALSE)
    if ("many_to_many" %in% names(join_map)) out <- join_map[!is.na(join_map$many_to_many) & join_map$many_to_many %in% TRUE, , drop = FALSE]
  }
  as.data.frame(out %||% data.frame(), stringsAsFactors = FALSE)
}

compare_tracker_to_matched_panel <- function(district_panel, district_join_map, ...) {
  panel <- as.data.frame(district_panel, stringsAsFactors = FALSE)
  join_map <- as.data.frame(district_join_map, stringsAsFactors = FALSE)
  data.frame(
    object = c("district_panel", "district_join_map", "unmatched_rows", "many_to_many_cases"),
    n_rows = c(nrow(panel), nrow(join_map), nrow(extract_unmatched_districts(join_map)), nrow(extract_many_to_many_cases(join_map))),
    n_complete_rows = c(sum(stats::complete.cases(panel)), sum(stats::complete.cases(join_map)), NA_integer_, NA_integer_),
    stringsAsFactors = FALSE
  )
}

build_district_matching_search_table <- function(district_panel, district_join_map) {
  dfs <- list(panel = district_panel, join_map = district_join_map, unmatched = extract_unmatched_districts(district_join_map))
  safe_bind_rows(lapply(names(dfs), function(source) {
    df <- as.data.frame(dfs[[source]], stringsAsFactors = FALSE)
    if (!nrow(df)) return(data.frame())
    state_cols <- grep("^state|state_", names(df), value = TRUE, ignore.case = TRUE)
    district_cols <- grep("^district|district_", names(df), value = TRUE, ignore.case = TRUE)
    state <- if (length(state_cols)) do.call(dplyr::coalesce, df[state_cols]) else NA_character_
    district <- if (length(district_cols)) do.call(dplyr::coalesce, df[district_cols]) else NA_character_
    data.frame(state = state, district = district, source = source, stringsAsFactors = FALSE)
  }))
}

save_district_matching_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/district_matching") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    summary = write_diagnostic_csv(as.data.frame(diagnostics), file.path(dir, "district_matching_summary.csv")),
    unmatched_rows = write_diagnostic_csv(attr(diagnostics, "unmatched_rows") %||% data.frame(), file.path(dir, "district_matching_unmatched_rows.csv")),
    manual_matches = write_diagnostic_csv(attr(diagnostics, "manual_matches") %||% data.frame(), file.path(dir, "district_matching_manual_matches.csv")),
    many_to_many = write_diagnostic_csv(attr(diagnostics, "many_to_many_cases") %||% data.frame(), file.path(dir, "district_matching_many_to_many_cases.csv")),
    tracker_panel_comparison = write_diagnostic_csv(attr(diagnostics, "tracker_panel_comparison") %||% data.frame(), file.path(dir, "district_matching_tracker_panel_comparison.csv")),
    all_rows_search = write_diagnostic_csv(attr(diagnostics, "all_rows_search") %||% data.frame(), file.path(dir, "district_matching_all_rows_search.csv"))
  )
  legacy_output_manifest(paths)
}
