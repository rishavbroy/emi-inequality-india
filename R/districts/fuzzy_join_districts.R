# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-fuzzy-record-linkage
# The functions below preserve the cascading fuzzy-match architecture from the legacy Rmd.
# sample-end marker appears near the end of this file after the functions are migrated.

#' evaluate distances
#'
#' @return A tibble, model object, list, or file path depending on context.
evaluate_distances <- function(pairs, methods, thresholds, col1 = "str1", col2 = "str2") {
  if (length(methods) != length(thresholds)) stop("\"methods\" and \"thresholds\" must have the same length.")
  safe_bind_rows(lapply(seq_along(methods), function(i) {
    distance <- utils::adist(canon(pairs[[col1]]), canon(pairs[[col2]]), ignore.case = TRUE)[, 1]
    data.frame(
      str1 = pairs[[col1]],
      str2 = pairs[[col2]],
      method = methods[i],
      distance = distance,
      threshold = thresholds[i],
      match = distance <= thresholds[i],
      stringsAsFactors = FALSE
    )
  }))
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
  out <- safe_bind_rows(Map(function(keys, source) {
    if (!nrow(keys)) return(data.frame())
    keys$source <- source
    keys$match_status <- "key_only"
    keys$possible_false_positive <- FALSE
    keys$many_to_many <- FALSE
    keys
  }, list(district_keys_2001, district_keys_2007, district_keys_2017, district_keys_2020), c("2001", "2007", "2017", "2020")))
  attr(out, "unmatched_rows") <- out[0, , drop = FALSE]
  attr(out, "possible_false_positives") <- out[out$possible_false_positive, , drop = FALSE]
  attr(out, "many_to_many_cases") <- out[out$many_to_many, , drop = FALSE]
  out
}
# sample-end: code-fuzzy-record-linkage
