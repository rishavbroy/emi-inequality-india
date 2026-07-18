# This file is part of the EMI inequality research pipeline.
# The reviewed harmonization crosswalk is the sole active district-map authority.

# sample-start: code-district-join-map

#' Required reviewed district-crosswalk columns
#'
#' @return The name columns needed to attach 2001, 2007-08, 2017-18, and 2020
#'   source data to one harmonized district row.
district_join_map_required_columns <- function() {
  c("state_01", "district_01", "state_07", "district_07", "state_17", "district_17", "state_20", "district_20")
}

#' Prepare the reviewed district join map
#'
#' The active pipeline does not infer the harmonization map at build time. It
#' consumes the reviewed metadata crosswalk, gives each row a stable internal
#' identifier, and exposes empty diagnostic attributes until source attachment
#' produces a row-level match ledger in the planned district-matching rewrite.
prepare_district_join_map <- function(crosswalk) {
  out <- safe_df(crosswalk)
  missing <- setdiff(district_join_map_required_columns(), names(out))
  if (length(missing)) {
    stop(
      "District harmonization crosswalk is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(out)) stop("District harmonization crosswalk contains no rows.", call. = FALSE)

  out$.tracker_row <- seq_len(nrow(out))
  out$source <- "harmonization_crosswalk"
  out$match_status <- "reviewed_crosswalk_row"
  out$possible_false_positive <- FALSE
  out$many_to_many <- FALSE
  attr(out, "unmatched_rows") <- out[0, , drop = FALSE]
  attr(out, "possible_false_positives") <- out[0, , drop = FALSE]
  attr(out, "many_to_many_cases") <- out[0, , drop = FALSE]
  out
}

#' Flag many-to-many matches in a row-level match ledger
#'
#' This helper is retained for matching diagnostics. It is not used to infer the
#' reviewed crosswalk itself.
flag_many_to_many_matches <- function(join_map, source_col = NULL, tracker_col = NULL) {
  join_map <- safe_df(join_map)
  if (!nrow(join_map)) {
    join_map$many_to_many <- logical()
    join_map$many_to_many_type <- character()
    return(join_map)
  }
  source_col <- source_col %||% first_col(join_map, c(".source_row", "source_row", "source_key", "source_id"))
  tracker_col <- tracker_col %||% first_col(join_map, c(".tracker_row", "tracker_row", "tracker_key", "district_panel_id"))
  if (is.null(source_col) || is.null(tracker_col)) {
    join_map$many_to_many <- FALSE
    join_map$many_to_many_type <- "not_evaluable_missing_keys"
    return(join_map)
  }
  source_key <- canon(join_map[[source_col]])
  tracker_key <- canon(join_map[[tracker_col]])
  source_n <- ave(rep(1L, nrow(join_map)), source_key, FUN = length)
  tracker_n <- ave(rep(1L, nrow(join_map)), tracker_key, FUN = length)
  source_dup <- source_n > 1L & !is.na(source_key) & nzchar(source_key)
  tracker_dup <- tracker_n > 1L & !is.na(tracker_key) & nzchar(tracker_key)
  join_map$n_source_matches <- as.integer(source_n)
  join_map$n_tracker_matches <- as.integer(tracker_n)
  join_map$many_to_many <- source_dup & tracker_dup
  join_map$many_to_many_type <- ifelse(
    source_dup & tracker_dup, "many_to_many",
    ifelse(source_dup, "one_source_to_many_tracker",
      ifelse(tracker_dup, "many_source_to_one_tracker", "one_to_one"))
  )
  join_map
}

# sample-end: code-district-join-map
