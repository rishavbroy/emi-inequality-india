# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

legacy_fuzzy_match_methods <- function() c("soundex", "qgram", "jw", "dl", "osa")
legacy_fuzzy_match_thresholds <- function() c(0, 0, 0.15, 2, 1)

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
    stringsAsFactors = FALSE
  )
}

#' diagnose fuzzy matching
#'
#' Port the legacy Chunk 16 fuzzy-matching tuning diagnostics: method/threshold
#' choices, string-distance examples, and current join status counts.  Full
#' threshold sweeps live in benchmarking targets.
diagnose_fuzzy_matching <- function(district_tracker, district_join_map, cfg) {
  tracker <- as.data.frame(district_tracker, stringsAsFactors = FALSE)
  join_map <- as.data.frame(district_join_map, stringsAsFactors = FALSE)
  base <- data.frame(
    n_tracker_rows = nrow(tracker),
    n_join_rows = nrow(join_map),
    n_unmatched_rows = nrow(attr(join_map, "unmatched_rows") %||% data.frame()),
    stringsAsFactors = FALSE
  )
  attr(base, "legacy_methods") <- data.frame(method = legacy_fuzzy_match_methods(), threshold = legacy_fuzzy_match_thresholds(), stringsAsFactors = FALSE)
  attr(base, "troublesome_pairs") <- test_troublesome_name_pairs()
  attr(base, "join_status_counts") <- summarize_fuzzy_join_status(join_map)
  class(base) <- c("emi_fuzzy_matching_diagnostics", class(base))
  base
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
  need_pkg("stringdist", "fuzzy matching diagnostics")
  out <- safe_bind_rows(lapply(seq_along(methods), function(i) {
    m <- methods[[i]]
    th <- thresholds[[i]]
    distance <- stringdist::stringdist(as.character(pairs[[col1]]), as.character(pairs[[col2]]), method = m)
    data.frame(
      str1 = pairs[[col1]],
      str2 = pairs[[col2]],
      method = m,
      distance = distance,
      threshold = th,
      match = distance <= th,
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$str1, out$str2, out$method), , drop = FALSE]
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

summarize_threshold_sensitivity <- function(pairs = legacy_troublesome_name_pairs(), methods = legacy_fuzzy_match_methods(), threshold_grid = NULL) {
  if (is.null(threshold_grid)) {
    threshold_grid <- list(soundex = c(0), qgram = c(0, 1, 2), jw = c(0.10, 0.15, 0.20), dl = c(1, 2, 3), osa = c(1, 2, 3), lcs = c(3, 5))
  }
  safe_bind_rows(lapply(names(threshold_grid), function(method) {
    safe_bind_rows(lapply(threshold_grid[[method]], function(th) {
      res <- evaluate_distances(pairs, method, th)
      data.frame(method = method, threshold = th, n_pairs = nrow(res), n_matches = sum(res$match, na.rm = TRUE), pct_matches = mean(res$match, na.rm = TRUE), stringsAsFactors = FALSE)
    }))
  }))
}

save_fuzzy_matching_diagnostics <- function(diagnostics, dir = "outputs/diagnostics/extended/fuzzy_matching") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    summary = write_diagnostic_csv(as.data.frame(diagnostics), file.path(dir, "fuzzy_matching_summary.csv")),
    legacy_methods = write_diagnostic_csv(attr(diagnostics, "legacy_methods") %||% data.frame(), file.path(dir, "fuzzy_matching_legacy_methods.csv")),
    troublesome_pairs = write_diagnostic_csv(attr(diagnostics, "troublesome_pairs") %||% data.frame(), file.path(dir, "fuzzy_matching_troublesome_pairs.csv")),
    join_status_counts = write_diagnostic_csv(attr(diagnostics, "join_status_counts") %||% data.frame(), file.path(dir, "fuzzy_matching_join_status_counts.csv"))
  )
  legacy_output_manifest(paths)
}
