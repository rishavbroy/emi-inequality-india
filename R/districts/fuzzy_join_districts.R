# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

# sample-start: code-fuzzy-record-linkage
# The functions below implement the current cascading fuzzy-match architecture.
# sample-end marker appears near the end of this file for coding-sample extraction.

#' fuzzy join sequence
#'
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
#' @return A data frame of unmatched rows when the join map carries that attribute.
unmatched_rows <- function(join_map, ...) {
  attr(join_map, "unmatched_rows") %||% join_map[0, , drop = FALSE]
}

#' merge dfs into tracker
#'
#' Cascading district matcher. `df_names` is a named list (or a
#' data frame) whose elements carry source-specific district/state names.  For
#' each source we try every requested tracker year in order, keep one-to-one
#' canonical name matches, and then fuzzy-match remaining rows within state.
#'
#' @return A list with `joined_df`, `unmatched_df`, and `flagged_df`, matching
#' the current matcher object contract.
merge_dfs_into_tracker <- function(df_names, tracker = NULL, years_of_interest = c("2001", "2007", "2008", "2017", "2018", "2020"), flag = TRUE, ...) {
  if (is.null(tracker)) return(list(joined_df = safe_df(df_names), unmatched_df = data.frame(), flagged_df = data.frame()))
  tracker <- safe_df(tracker)
  if (!nrow(tracker)) return(list(joined_df = data.frame(), unmatched_df = safe_df(df_names), flagged_df = data.frame()))
  if (!".tracker_row" %in% names(tracker)) tracker$.tracker_row <- seq_len(nrow(tracker))
  xs <- if (inherits(df_names, "data.frame")) list(source = df_names) else as_input_list(df_names)

  joined_all <- list()
  unmatched_all <- list()
  flagged_all <- list()
  for (source_name in names(xs)) {
    source <- safe_df(xs[[source_name]])
    if (!nrow(source)) next
    source$.source_row <- seq_len(nrow(source))
    source$.source_name <- source_name
    source <- add_source_name_keys(source)
    if (!all(c(".source_state_key", ".source_district_key") %in% names(source))) {
      unmatched_all[[source_name]] <- source
      next
    }

    source$.matched <- FALSE
    for (yr in years_of_interest) {
      sfx <- substr(as.character(yr), 3, 4)
      state_col <- paste0("state_", sfx)
      district_col <- paste0("district_", sfx)
      if (!all(c(state_col, district_col) %in% names(tracker))) next
      candidate <- tracker[c(".tracker_row", state_col, district_col)]
      candidate$.tracker_state_key <- canon(candidate[[state_col]])
      candidate$.tracker_district_key <- canon(candidate[[district_col]])
      exact <- merge(
        source[!source$.matched, , drop = FALSE],
        candidate,
        by.x = c(".source_state_key", ".source_district_key"),
        by.y = c(".tracker_state_key", ".tracker_district_key"),
        all.x = FALSE,
        all.y = FALSE
      )
      if (!nrow(exact)) next
      exact <- exact[!duplicated(exact$.source_row) & !duplicated(exact$.tracker_row), , drop = FALSE]
      if (!nrow(exact)) next
      exact$match_year <- as.character(yr)
      exact$match_status <- "exact_name"
      joined_all[[paste(source_name, yr, sep = "_")]] <- exact
      source$.matched[source$.source_row %in% exact$.source_row] <- TRUE
    }

    if (any(!source$.matched)) {
      fuzzy <- fuzzy_match_remaining_to_tracker(source[!source$.matched, , drop = FALSE], tracker, years_of_interest)
      if (nrow(fuzzy)) {
        joined_all[[paste0(source_name, "_fuzzy")]] <- fuzzy
        source$.matched[source$.source_row %in% fuzzy$.source_row] <- TRUE
        if (flag) flagged_all[[source_name]] <- fuzzy[fuzzy$possible_false_positive %in% TRUE, , drop = FALSE]
      }
    }
    unmatched_all[[source_name]] <- source[!source$.matched, , drop = FALSE]
  }
  list(
    joined_df = safe_bind_rows(joined_all),
    unmatched_df = safe_bind_rows(unmatched_all),
    flagged_df = safe_bind_rows(flagged_all)
  )
}

add_source_name_keys <- function(source) {
  state <- first_col(source, c("state", "state_01", "state_07", "state_08", "state_17", "state_18", "state_20", "state_0708", "state_1718"))
  district <- first_col(source, c("district", "district_01", "district_07", "district_08", "district_17", "district_18", "district_20", "district_0708", "district_1718", "district_name"))
  if (!is.null(state)) source$.source_state_key <- canon(source[[state]])
  if (!is.null(district)) source$.source_district_key <- canon(source[[district]])
  source
}

fuzzy_match_remaining_to_tracker <- function(source, tracker, years_of_interest) {
  out <- list()
  for (yr in years_of_interest) {
    sfx <- substr(as.character(yr), 3, 4)
    state_col <- paste0("state_", sfx)
    district_col <- paste0("district_", sfx)
    if (!all(c(state_col, district_col) %in% names(tracker))) next
    candidate <- tracker[c(".tracker_row", state_col, district_col)]
    candidate$.tracker_state_key <- canon(candidate[[state_col]])
    candidate$.tracker_district_key <- canon(candidate[[district_col]])
    for (i in seq_len(nrow(source))) {
      pool <- candidate[candidate$.tracker_state_key == source$.source_state_key[[i]], , drop = FALSE]
      if (!nrow(pool)) next
      dist <- utils::adist(source$.source_district_key[[i]], pool$.tracker_district_key, ignore.case = TRUE)[1, ]
      best <- which.min(dist)
      if (!length(best) || !is.finite(dist[[best]]) || dist[[best]] > 2) next
      row <- cbind(source[i, , drop = FALSE], pool[best, , drop = FALSE])
      row$match_year <- as.character(yr)
      row$match_status <- "fuzzy_name"
      row$dist <- dist[[best]]
      row$possible_false_positive <- dist[[best]] > 0
      out[[length(out) + 1L]] <- row
    }
  }
  x <- safe_bind_rows(out)
  if (nrow(x)) x <- x[!duplicated(x$.source_row) & !duplicated(x$.tracker_row), , drop = FALSE]
  x
}

#' join year to tracker
#'
join_year_to_tracker <- function(source, tracker, years_of_interest, ...) {
  merge_dfs_into_tracker(source, tracker = tracker, years_of_interest = years_of_interest, ...)
}

#' join all district sources
#'
join_all_district_sources <- function(df_names, tracker, years_of_interest, ...) {
  merge_dfs_into_tracker(df_names, tracker = tracker, years_of_interest = years_of_interest, ...)
}

#' flag many to many matches
#'
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

#' score candidate matches
#'
score_candidate_matches <- function(candidates) {
  if (!"dist" %in% names(candidates)) candidates$dist <- 0
  candidates$score <- -num(candidates$dist)
  candidates
}

#' fuzzy join districts
#'
fuzzy_join_districts <- function(district_tracker, district_keys_2001, district_keys_2007, district_keys_2017, district_keys_2020, cfg) {
  tracker <- safe_df(district_tracker)
  if (nrow(tracker) && all(c("state_01", "district_01", "state_07", "district_07", "state_17", "district_17", "state_20", "district_20") %in% names(tracker))) {
    tracker$.tracker_row <- seq_len(nrow(tracker))
    tracker$source <- "harmonization_crosswalk"
    tracker$match_status <- "harmonization_crosswalk_row"
    tracker$possible_false_positive <- FALSE
    tracker$many_to_many <- FALSE
    attr(tracker, "unmatched_rows") <- tracker[0, , drop = FALSE]
    attr(tracker, "possible_false_positives") <- tracker[0, , drop = FALSE]
    attr(tracker, "many_to_many_cases") <- tracker[0, , drop = FALSE]
    return(tracker)
  }

  out <- safe_bind_rows(Map(function(keys, source) {
    if (!nrow(keys)) return(data.frame())
    keys$source <- source
    keys$match_status <- "source_key_unmatched"
    keys$possible_false_positive <- FALSE
    keys$many_to_many <- FALSE
    keys
  }, list(district_keys_2001, district_keys_2007, district_keys_2017, district_keys_2020), c("2001", "2007", "2017", "2020")))
  attr(out, "unmatched_rows") <- out
  attr(out, "possible_false_positives") <- out[out$possible_false_positive, , drop = FALSE]
  attr(out, "many_to_many_cases") <- out[out$many_to_many, , drop = FALSE]
  out
}
# sample-end: code-fuzzy-record-linkage
