# This file is part of the EMI inequality research pipeline.
# Fuzzy-matching tuning and threshold helpers are sourced by benchmark targets
# and reused by extended fuzzy-matching diagnostics.

fuzzy_tuning_reference <- function() {
  data.frame(
    diagnostic = c(
      "full_join_method_row_counts",
      "lcs_osa_3_3",
      "jw_dl_osa_lcs",
      "final_method_choice"
    ),
    legacy_result = c(
      "osa/lv/dl 859 rows; hamming 872; lcs 825; qgram 829; cosine/jaccard/jw 435262",
      "189/734 rows had any NA",
      "180/734 rows had any NA",
      "soundex=0, qgram=0, jw<=0.15, dl<=2, osa<=1; 166/734 rows had any NA"
    ),
    legacy_chunk = "Chunk 16 Match districts: Test joining methods",
    stringsAsFactors = FALSE
  )
}

troublesome_name_pairs <- function() {
  data.frame(
    str1 = c(
      "Baleshwar",
      "Jammu & Kashmir",
      "East Godavari",
      "Sikim",
      "Mumbai",
      "24-Parganas ( North )",
      "North Twenty Four Pargan*",
      "Sahibzada Ajit Singh Nag*",
      "Sri Potti Sriramulu Nell*"
    ),
    str2 = c(
      "Balasore",
      "Jammu and Kashmir",
      "Godavari East",
      "Sikkim",
      "Mumbai",
      "North Twenty Four Parganas",
      "North Twenty Four Parganas",
      "Sahibzada Ajit Singh Nagar",
      "Sri Potti Sriramulu Nellore"
    ),
    pair_source = "legacy_troublesome_comment",
    stringsAsFactors = FALSE
  )
}

#' evaluate distances
#'
#' This preserves the legacy helper from Chunk 16.  It uses stringdist's named
#' methods when available, including the legacy notes: soundex = 0, qgram = 0,
#' jw <= 0.15, dl <= 2, and osa <= 1.
benchmark_string_distance_methods <- function(pairs, methods = district_fuzzy_match_methods(), thresholds = district_fuzzy_match_thresholds()) {
  evaluate_distances(pairs, methods, thresholds)
}

test_troublesome_name_pairs <- function(pairs = troublesome_name_pairs(), methods = district_fuzzy_match_methods(), thresholds = district_fuzzy_match_thresholds()) {
  evaluate_distances(pairs, methods, thresholds)
}

summarize_fuzzy_candidate_pair_coverage <- function(pairs) {
  pairs <- as.data.frame(pairs, stringsAsFactors = FALSE)
  if (!nrow(pairs)) return(data.frame())
  out <- as.data.frame(table(pair_source = pairs$pair_source), stringsAsFactors = FALSE)
  names(out) <- c("pair_source", "n_pairs")
  out$n_pairs <- as.integer(out$n_pairs)
  out$coverage_note <- ifelse(
    out$pair_source == "legacy_troublesome_comment",
    "legacy hand-picked examples from Chunk 16",
    "active source/tracker candidate pair emitted by the current pipeline"
  )
  out
}

summarize_threshold_sensitivity <- function(pairs = troublesome_name_pairs(), methods = district_fuzzy_match_methods(), threshold_grid = NULL) {
  pairs <- as.data.frame(pairs, stringsAsFactors = FALSE)
  if (!nrow(pairs)) return(data.frame())
  if (!"pair_source" %in% names(pairs)) pairs$pair_source <- "unspecified"
  if (is.null(threshold_grid)) {
    threshold_grid <- list(soundex = c(0), qgram = c(0, 1, 2), jw = c(0.10, 0.15, 0.20), dl = c(1, 2, 3), osa = c(1, 2, 3), lcs = c(3, 5))
  }
  safe_bind_rows(lapply(names(threshold_grid), function(method) {
    safe_bind_rows(lapply(threshold_grid[[method]], function(th) {
      res <- evaluate_distances(pairs, method, th)
      safe_bind_rows(lapply(split(res, res$pair_source), function(x) {
        data.frame(method = method, threshold = th, pair_source = x$pair_source[[1]], n_pairs = nrow(x), n_matches = sum(x$match, na.rm = TRUE), pct_matches = mean(x$match, na.rm = TRUE), stringsAsFactors = FALSE)
      }))
    }))
  }))
}
