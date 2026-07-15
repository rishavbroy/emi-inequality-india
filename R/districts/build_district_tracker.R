# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-district-crosswalk-qa

#' build district tracker
#'
build_district_tracker <- function(raw_district_changes) {
  tracker_key <- if ("india_district_tracker" %in% names(raw_district_changes)) "india_district_tracker" else "tracker"
  parsed <- list(
    alluvial = parse_alluvial_district_changes(raw_district_changes[["alluvial"]] %||% data.frame()),
    carveouts_renamings = parse_carveouts_renamings(raw_district_changes[["carveouts_renamings"]] %||% data.frame()),
    new_districts_created = parse_new_districts_created(raw_district_changes[["new_districts_created"]] %||% data.frame()),
    name_changes = parse_name_changes(raw_district_changes[["name_changes"]] %||% data.frame()),
    district_splits = parse_district_splits(raw_district_changes[["district_splits"]] %||% data.frame())
  )
  parsed[[tracker_key]] <- parse_india_district_tracker(raw_district_changes[[tracker_key]] %||% data.frame())
  extras <- setdiff(names(raw_district_changes), c("alluvial", "india_district_tracker", "tracker", "carveouts_renamings", "new_districts_created", "name_changes", "district_splits"))
  for (nm in extras) parsed[[nm]] <- parse_district_change_source(raw_district_changes[[nm]], source_type = nm)
  standardize_tracker_names(standardize_tracker_years(combine_district_tracker_sources(parsed)))
}

#' parse alluvial district changes
#'
parse_alluvial_district_changes <- function(x) {
  parse_district_change_source(x, source_type = "alluvial")
}

#' parse india district tracker
#'
parse_india_district_tracker <- function(x) {
  parse_district_change_source(x, source_type = "india_district_tracker")
}

#' parse carveouts renamings
#'
parse_carveouts_renamings <- function(x) {
  parse_district_change_source(x, source_type = "carveouts_renamings")
}

#' parse new districts created
#'
parse_new_districts_created <- function(x) {
  parse_district_change_source(x, source_type = "new_districts_created")
}

#' parse name changes
#'
parse_name_changes <- function(x) {
  parse_district_change_source(x, source_type = "name_changes")
}

#' parse district splits
#'
parse_district_splits <- function(x) {
  parse_district_change_source(x, source_type = "district_splits")
}

parse_district_change_source <- function(x, source_type) {
  x <- safe_df(x)
  if (!nrow(x)) return(data.frame(source_type = character(), stringsAsFactors = FALSE))
  x$source_type <- source_type
  x$source_state_raw <- first_present_value(x, c("state", "State", "state_raw", "state_from", "state_01", "state_07", "state_08", "state_17", "state_18", "state_20"))
  x$source_district_raw <- first_present_value(x, c("district", "District", "district_raw", "district_from", "district_01", "district_07", "district_08", "district_17", "district_18", "district_20", "district_name"))
  x$target_state_raw <- first_present_value(x, c("state_to", "new_state", "target_state", "state_20", "state_18", "state_17", "state_08", "state_07", "state"))
  x$target_district_raw <- first_present_value(x, c("district_to", "new_district", "target_district", "district_20", "district_18", "district_17", "district_08", "district_07", "district"))
  x$source_year_raw <- first_present_value(x, c("source_year", "year_from", "from_year", "start_year", "year"))
  x$target_year_raw <- first_present_value(x, c("target_year", "year_to", "to_year", "end_year", "year"))
  x$change_type <- first_present_value(x, c("change_type", "type", "event_type", "status"))
  x
}

first_present_value <- function(df, candidates) {
  hit <- first_col(df, candidates)
  if (is.null(hit)) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit]])
}

#' combine district tracker sources
#'
combine_district_tracker_sources <- function(raw_district_changes) {
  out <- safe_bind_rows(lapply(names(raw_district_changes), function(name) {
    x <- safe_df(raw_district_changes[[name]])
    if (!nrow(x)) return(data.frame())
    x$source_file_id <- if ("source_file_id" %in% names(x)) x$source_file_id else name
    if (!"source_type" %in% names(x)) x$source_type <- name
    x
  }))
  if (nrow(out)) out$.row_in_source <- ave(seq_len(nrow(out)), out$source_file_id, FUN = seq_along)
  out
}

#' standardize tracker years
#'
standardize_tracker_years <- function(tracker) {
  tracker <- safe_df(tracker)
  if (!nrow(tracker)) return(tracker)
  if ("source_year_raw" %in% names(tracker) && !"source_year" %in% names(tracker)) tracker$source_year <- suppressWarnings(as.integer(tracker$source_year_raw))
  if ("target_year_raw" %in% names(tracker) && !"target_year" %in% names(tracker)) tracker$target_year <- suppressWarnings(as.integer(tracker$target_year_raw))
  tracker
}

#' standardize tracker names
#'
standardize_tracker_names <- function(tracker) {
  tracker <- safe_df(tracker)
  if (!nrow(tracker)) return(tracker)
  if ("source_state_raw" %in% names(tracker)) tracker$source_state_key <- canonicalize_state_name(tracker$source_state_raw)
  if ("source_district_raw" %in% names(tracker)) tracker$source_district_key <- canon(tracker$source_district_raw)
  if ("target_state_raw" %in% names(tracker)) tracker$target_state_key <- canonicalize_state_name(tracker$target_state_raw)
  if ("target_district_raw" %in% names(tracker)) tracker$target_district_key <- canon(tracker$target_district_raw)
  tracker
}

# sample-end: code-district-crosswalk-qa
