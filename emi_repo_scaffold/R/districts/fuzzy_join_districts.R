# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-fuzzy-record-linkage
# The functions below preserve the cascading fuzzy-match architecture from the legacy Rmd.
# sample-end marker appears near the end of this file after the functions are migrated.

#' evaluate distances
#'
#' @return A tibble, model object, list, or file path depending on context.
evaluate_distances <- function(pairs, methods, thresholds, col1 = "str1", col2 = "str2") {
  if(length(methods) != length(thresholds)) stop("methods and thresholds must have the same length.")
  pairs <- pairs |> dplyr::mutate(dplyr::across(dplyr::all_of(c(col1, col2)), as.character))
  dplyr::bind_rows(lapply(seq_along(methods), function(i) { m <- methods[i]; th <- thresholds[i]; pairs |> dplyr::transmute(str1 = .data[[col1]], str2 = .data[[col2]], method = m, distance = stringdist::stringdist(str1, str2, method = m), threshold = th, match = distance <= th) })) |> dplyr::arrange(str1, str2, method)
}

#' fuzzy join sequence
#'
#' @return A tibble, model object, list, or file path depending on context.
fuzzy_join_sequence <- function(df1, df2, dist1, state1, dist2, state2, methods, thresholds, mode = "full") {
  df1_id <- df1 |> dplyr::mutate(.id1 = dplyr::row_number())
  df2_id <- df2 |> dplyr::mutate(.id2 = dplyr::row_number())
  df1_curr <- df1_id; df2_curr <- df2_id; matched_all <- tibble::tibble()
  for (i in seq_along(methods)) {
    full_j <- fuzzyjoin::stringdist_join(df1_curr, df2_curr, by = stats::setNames(c(dist2, state2), c(dist1, state1)), mode = mode, method = methods[i], max_dist = thresholds[i], distance_col = "dist")
    matched_i <- full_j |> dplyr::filter(!is.na(.id1), !is.na(.id2)) |> dplyr::group_by(.id1) |> dplyr::slice_min(dist, n = 1, with_ties = FALSE) |> dplyr::ungroup() |> dplyr::group_by(.id2) |> dplyr::slice_min(dist, n = 1, with_ties = FALSE) |> dplyr::ungroup()
    matched_all <- dplyr::bind_rows(matched_all, matched_i)
    df1_curr <- df1_curr |> dplyr::filter(!(.id1 %in% matched_i$.id1)); df2_curr <- df2_curr |> dplyr::filter(!(.id2 %in% matched_i$.id2))
  }
  list(joined = matched_all, unmatched_df1 = df1_curr |> dplyr::select(-.id1), unmatched_df2 = df2_curr |> dplyr::select(-.id2))
}

#' unmatched rows
#'
#' @return A tibble, model object, list, or file path depending on context.
unmatched_rows <- function(...) {
  stop("TODO: migrate unmatched_rows() from legacy chunk 17/20")
}

#' merge dfs into tracker
#'
#' @return A tibble, model object, list, or file path depending on context.
merge_dfs_into_tracker <- function(...) {
  stop("TODO: migrate merge_dfs_into_tracker() from legacy chunk 17")
}

#' join year to tracker
#'
#' @return A tibble, model object, list, or file path depending on context.
join_year_to_tracker <- function(...) {
  stop("TODO")
}

#' join all district sources
#'
#' @return A tibble, model object, list, or file path depending on context.
join_all_district_sources <- function(...) {
  stop("TODO")
}

#' flag many to many matches
#'
#' @return A tibble, model object, list, or file path depending on context.
flag_many_to_many_matches <- function(join_map) {
  join_map
}

#' score candidate matches
#'
#' @return A tibble, model object, list, or file path depending on context.
score_candidate_matches <- function(candidates) {
  candidates
}

#' fuzzy join districts
#'
#' @return A tibble, model object, list, or file path depending on context.
fuzzy_join_districts <- function(district_tracker, district_keys_2001, district_keys_2007, district_keys_2017, district_keys_2020, cfg) {
  list(tracker = district_tracker, keys = list(y2001 = district_keys_2001, y2007 = district_keys_2007, y2017 = district_keys_2017, y2020 = district_keys_2020))
}

