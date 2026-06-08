# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-district-crosswalk-qa

#' build district tracker
#'
build_district_tracker <- function(raw_district_changes) {
  combine_district_tracker_sources(raw_district_changes)
}

#' parse alluvial district changes
#'
parse_alluvial_district_changes <- function(x) {
  x
}

#' parse india district tracker
#'
parse_india_district_tracker <- function(x) {
  x
}

#' parse carveouts renamings
#'
parse_carveouts_renamings <- function(x) {
  x
}

#' parse new districts created
#'
parse_new_districts_created <- function(x) {
  x
}

#' parse name changes
#'
parse_name_changes <- function(x) {
  x
}

#' parse district splits
#'
parse_district_splits <- function(x) {
  x
}

#' combine district tracker sources
#'
combine_district_tracker_sources <- function(raw_district_changes) {
  out <- safe_bind_rows(lapply(names(raw_district_changes), function(name) {
    x <- safe_df(raw_district_changes[[name]])
    x$source_file_id <- name
    x
  }))
  if (nrow(out)) out$.row_in_source <- ave(seq_len(nrow(out)), out$source_file_id, FUN = seq_along)
  out
}

#' standardize tracker years
#'
standardize_tracker_years <- function(tracker) {
  tracker
}

#' standardize tracker names
#'
standardize_tracker_names <- function(tracker) {
  tracker
}

# sample-end: code-district-crosswalk-qa
