# Canonical district fuzzy-distance helpers.
# Legacy chunks 16--20 used the same method cascade for tuning diagnostics and
# district-source attachment. Keep the methods and thresholds in one place so
# benchmarks, diagnostics, and production matching cannot drift apart.

legacy_fuzzy_match_methods <- function() c("soundex", "qgram", "jw", "dl", "osa")

legacy_fuzzy_match_thresholds <- function() c(0, 0, 0.15, 2, 1)

# Compatibility aliases used by district-source attachment tests and callers.
legacy_source_match_methods <- legacy_fuzzy_match_methods
legacy_source_match_thresholds <- legacy_fuzzy_match_thresholds

#' Evaluate district-name string distances for a method cascade
#'
#' @param pairs Data frame containing two string columns.
#' @param methods Stringdist method names.
#' @param thresholds Per-method match thresholds.
#' @param col1,col2 Names of the string columns in `pairs`.
#' @return Data frame with one row per pair-method combination.
evaluate_distances <- function(pairs, methods, thresholds, col1 = "str1", col2 = "str2") {
  if (length(methods) != length(thresholds)) {
    stop("\"methods\" and \"thresholds\" must have the same length.", call. = FALSE)
  }
  pairs <- safe_df(pairs)
  if (!"pair_source" %in% names(pairs)) pairs$pair_source <- "unspecified"
  if (!all(c(col1, col2) %in% names(pairs))) {
    stop("pairs must contain columns ", col1, " and ", col2, ".", call. = FALSE)
  }
  need_pkg("stringdist", "district fuzzy-distance evaluation")

  out <- safe_bind_rows(lapply(seq_along(methods), function(i) {
    distance <- stringdist::stringdist(
      canon(pairs[[col1]]),
      canon(pairs[[col2]]),
      method = methods[[i]]
    )
    data.frame(
      str1 = pairs[[col1]],
      str2 = pairs[[col2]],
      pair_source = pairs$pair_source,
      method = methods[[i]],
      distance = distance,
      threshold = thresholds[[i]],
      match = is.finite(distance) & distance <= thresholds[[i]],
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$pair_source, out$str1, out$str2, out$method), , drop = FALSE]
}
