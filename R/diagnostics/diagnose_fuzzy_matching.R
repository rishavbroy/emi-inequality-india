# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

legacy_fuzzy_match_methods <- function() c("soundex", "qgram", "jw", "dl", "osa")
legacy_fuzzy_match_thresholds <- function() c(0, 0, 0.15, 2, 1)

legacy_fuzzy_tuning_reference <- function() {
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

legacy_troublesome_name_pairs <- function() {
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

#' diagnose fuzzy matching
#'
#' Port the legacy Chunk 16 fuzzy-matching tuning diagnostics: method/threshold
#' choices, string-distance examples, and current join status counts.  The
#' benchmark target expands this from the hand-picked commented pairs to the
#' active tracker/join candidate pairs.
diagnose_fuzzy_matching <- function(district_tracker, district_join_map, cfg) {
  tracker <- as.data.frame(district_tracker, stringsAsFactors = FALSE)
  join_map <- as.data.frame(district_join_map, stringsAsFactors = FALSE)
  candidate_pairs <- legacy_fuzzy_candidate_pairs(tracker, join_map)
  base <- data.frame(
    n_tracker_rows = nrow(tracker),
    n_join_rows = nrow(join_map),
    n_unmatched_rows = nrow(attr(district_join_map, "unmatched_rows", exact = TRUE) %||% data.frame()),
    n_candidate_pairs = nrow(candidate_pairs),
    n_active_candidate_pairs = sum(candidate_pairs$pair_source != "legacy_troublesome_comment", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  attr(base, "legacy_methods") <- data.frame(method = legacy_fuzzy_match_methods(), threshold = legacy_fuzzy_match_thresholds(), stringsAsFactors = FALSE)
  attr(base, "troublesome_pairs") <- test_troublesome_name_pairs()
  attr(base, "candidate_pairs") <- candidate_pairs
  attr(base, "join_status_counts") <- summarize_fuzzy_join_status(join_map)
  attr(base, "legacy_tuning_reference") <- legacy_fuzzy_tuning_reference()
  attr(base, "candidate_pair_coverage") <- summarize_fuzzy_candidate_pair_coverage(candidate_pairs)
  class(base) <- c("emi_fuzzy_matching_diagnostics", class(base))
  base
}

legacy_fuzzy_candidate_pairs <- function(district_tracker = data.frame(), district_join_map = data.frame()) {
  out <- list(legacy_troublesome_name_pairs())
  tracker <- as.data.frame(district_tracker, stringsAsFactors = FALSE)
  join_map <- as.data.frame(district_join_map, stringsAsFactors = FALSE)

  add_tracker_pair <- function(a, b, source) {
    ca <- paste0("district_", a)
    cb <- paste0("district_", b)
    if (!all(c(ca, cb) %in% names(tracker))) return(data.frame())
    x <- data.frame(str1 = as.character(tracker[[ca]]), str2 = as.character(tracker[[cb]]), pair_source = source, stringsAsFactors = FALSE)
    x <- x[!is.na(x$str1) & !is.na(x$str2) & nzchar(x$str1) & nzchar(x$str2) & x$str1 != x$str2, , drop = FALSE]
    unique(x)
  }
  suffixes <- tracker_year_suffixes(tracker, "district")
  if (length(suffixes) >= 2L) {
    for (i in seq_len(length(suffixes) - 1L)) {
      out[[length(out) + 1L]] <- add_tracker_pair(
        suffixes[[i]],
        suffixes[[i + 1L]],
        paste0("tracker_", tracker_suffix_year(suffixes[[i]]), "_to_", tracker_suffix_year(suffixes[[i + 1L]]))
      )
    }
  }
  out[[length(out) + 1L]] <- add_tracker_pair("01", "07", "tracker_2001_to_2007")
  out[[length(out) + 1L]] <- add_tracker_pair("07", "17", "tracker_2007_to_2017")
  out[[length(out) + 1L]] <- add_tracker_pair("17", "20", "tracker_2017_to_2020")

  # When merge_dfs_into_tracker() has real fuzzy candidate columns, preserve the
  # current source-vs-tracker comparisons.  This keeps the legacy "all rows
  # search" / View()-style inspection reproducible without forcing GUI output.
  src_district <- first_col(join_map, c(".source_district_key", "district", "district_name", "district_0708", "district_1718"))
  trk_district <- first_col(join_map, c(".tracker_district_key", "district_20", "district_17", "district_07", "district_std"))
  if (!is.null(src_district) && !is.null(trk_district)) {
    x <- data.frame(str1 = as.character(join_map[[src_district]]), str2 = as.character(join_map[[trk_district]]), pair_source = "active_join_candidates", stringsAsFactors = FALSE)
    x <- x[!is.na(x$str1) & !is.na(x$str2) & nzchar(x$str1) & nzchar(x$str2) & x$str1 != x$str2, , drop = FALSE]
    out[[length(out) + 1L]] <- unique(x)
  }

  join_district_cols <- grep("district", names(join_map), value = TRUE, ignore.case = TRUE)
  join_district_cols <- setdiff(join_district_cols, grep("key|code", join_district_cols, value = TRUE, ignore.case = TRUE))
  if (length(join_district_cols) >= 2L) {
    for (i in seq_len(length(join_district_cols) - 1L)) {
      a <- join_district_cols[[i]]
      b <- join_district_cols[[i + 1L]]
      x <- data.frame(str1 = as.character(join_map[[a]]), str2 = as.character(join_map[[b]]), pair_source = paste0("join_map_", a, "_to_", b), stringsAsFactors = FALSE)
      x <- x[!is.na(x$str1) & !is.na(x$str2) & nzchar(x$str1) & nzchar(x$str2) & x$str1 != x$str2, , drop = FALSE]
      out[[length(out) + 1L]] <- unique(x)
    }
  }

  pairs <- unique(safe_bind_rows(out))
  pairs[order(pairs$pair_source, pairs$str1, pairs$str2), , drop = FALSE]
}

#' evaluate distances
#'
#' This preserves the legacy helper from Chunk 16.  It uses stringdist's named
#' methods when available, including the legacy notes: soundex = 0, qgram = 0,
#' jw <= 0.15, dl <= 2, and osa <= 1.
evaluate_distances <- function(pairs, methods, thresholds, col1 = "str1", col2 = "str2") {
  if (length(methods) != length(thresholds)) {
    stop('The character vectors "methods" and "thresholds" must have the same length.', call. = FALSE)
  }
  pairs <- as.data.frame(pairs, stringsAsFactors = FALSE)
  if (!"pair_source" %in% names(pairs)) pairs$pair_source <- "unspecified"
  need_pkg("stringdist", "fuzzy matching diagnostics")
  out <- safe_bind_rows(lapply(seq_along(methods), function(i) {
    m <- methods[[i]]
    th <- thresholds[[i]]
    distance <- stringdist::stringdist(as.character(pairs[[col1]]), as.character(pairs[[col2]]), method = m)
    data.frame(
      str1 = pairs[[col1]],
      str2 = pairs[[col2]],
      pair_source = pairs$pair_source,
      method = m,
      distance = distance,
      threshold = th,
      match = distance <= th,
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$pair_source, out$str1, out$str2, out$method), , drop = FALSE]
}

benchmark_string_distance_methods <- function(pairs, methods = legacy_fuzzy_match_methods(), thresholds = legacy_fuzzy_match_thresholds()) {
  evaluate_distances(pairs, methods, thresholds)
}

test_troublesome_name_pairs <- function(pairs = legacy_troublesome_name_pairs(), methods = legacy_fuzzy_match_methods(), thresholds = legacy_fuzzy_match_thresholds()) {
  evaluate_distances(pairs, methods, thresholds)
}

summarize_fuzzy_join_status <- function(join_map) {
  join_map <- as.data.frame(join_map, stringsAsFactors = FALSE)
  if (!nrow(join_map)) return(data.frame())
  status <- if ("match_status" %in% names(join_map)) as.character(join_map$match_status) else rep("unknown", nrow(join_map))
  as.data.frame(table(match_status = status), stringsAsFactors = FALSE)
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

summarize_threshold_sensitivity <- function(pairs = legacy_troublesome_name_pairs(), methods = legacy_fuzzy_match_methods(), threshold_grid = NULL) {
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

save_fuzzy_matching_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/fuzzy_matching") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    summary = write_diagnostic_csv(as.data.frame(diagnostics), file.path(dir, "fuzzy_matching_summary.csv")),
    legacy_methods = write_diagnostic_csv(attr(diagnostics, "legacy_methods") %||% data.frame(), file.path(dir, "fuzzy_matching_legacy_methods.csv")),
    troublesome_pairs = write_diagnostic_csv(attr(diagnostics, "troublesome_pairs") %||% data.frame(), file.path(dir, "fuzzy_matching_troublesome_pairs.csv")),
    candidate_pairs = write_diagnostic_csv(attr(diagnostics, "candidate_pairs") %||% data.frame(), file.path(dir, "fuzzy_matching_candidate_pairs.csv")),
    join_status_counts = write_diagnostic_csv(attr(diagnostics, "join_status_counts") %||% data.frame(), file.path(dir, "fuzzy_matching_join_status_counts.csv")),
    candidate_pair_coverage = write_diagnostic_csv(attr(diagnostics, "candidate_pair_coverage") %||% data.frame(), file.path(dir, "fuzzy_matching_candidate_pair_coverage.csv")),
    legacy_tuning_reference = write_diagnostic_csv(attr(diagnostics, "legacy_tuning_reference") %||% data.frame(), file.path(dir, "fuzzy_matching_legacy_tuning_reference.csv"))
  )
  legacy_output_manifest(paths)
}
