# District-source attachment helpers for legacy tracker-based panels.
# These functions keep district matching separate from measure construction.

legacy_suffix_chain <- function(source_suffix, years_of_interest = c("2001", "2005", "2006", "2007", "2008", "2011", "2017", "2018", "2019", "2020")) {
  suffixes <- substr(as.character(years_of_interest), 3, 4)
  source_num <- suppressWarnings(as.integer(source_suffix))
  suffix_nums <- suppressWarnings(as.integer(suffixes))
  if (!is.finite(source_num)) return(suffixes)
  suffixes[order(abs(suffix_nums - source_num), suffix_nums)]
}

legacy_attach_source <- function(tracker, source, suffixes, source_label) {
  if (!nrow(source)) return(tracker)
  source <- add_legacy_join_keys(source, suffixes)
  if (!all(c(".legacy_state_key", ".legacy_district_key") %in% names(source))) return(tracker)
  source <- source[!duplicated(source[c(".legacy_state_key", ".legacy_district_key")]), , drop = FALSE]

  for (suffix in suffixes) {
    s_col <- paste0("state_", suffix)
    d_col <- paste0("district_", suffix)
    if (!all(c(s_col, d_col) %in% names(tracker))) next
    tracker$.legacy_state_key <- canonicalize_state_name(tracker[[s_col]])
    tracker$.legacy_district_key <- canon(tracker[[d_col]])
    already <- paste0(".matched_", source_label)
    if (!already %in% names(tracker)) tracker[[already]] <- FALSE
    to_join <- tracker[!tracker[[already]], c(".tracker_row", ".legacy_state_key", ".legacy_district_key"), drop = FALSE]
    if (!nrow(to_join)) next
    joined <- merge(to_join, source, by = c(".legacy_state_key", ".legacy_district_key"), all.x = TRUE, sort = FALSE)
    source_cols <- setdiff(names(joined), c(".legacy_state_key", ".legacy_district_key", ".tracker_row"))
    hits <- source_hits_with_values(joined, source_cols)
    if (!nrow(hits)) next
    rows <- match(hits$.tracker_row, tracker$.tracker_row)
    tracker <- fill_source_values_by_tracker_row(tracker, hits, source_cols, rows)
    tracker[[already]][rows[!is.na(rows)]] <- TRUE
  }
  tracker$.legacy_state_key <- NULL
  tracker$.legacy_district_key <- NULL
  tracker
}

legacy_attach_source_one_to_one <- function(tracker, source, suffixes, source_label) {
  if (!nrow(source)) return(tracker)
  source <- add_legacy_join_keys(source, suffixes)
  if (!all(c(".legacy_state_key", ".legacy_district_key") %in% names(source))) return(tracker)
  source$.source_row <- seq_len(nrow(source))
  source$.source_used <- FALSE

  matched <- paste0(".matched_", source_label)
  if (!matched %in% names(tracker)) tracker[[matched]] <- FALSE

  for (suffix in suffixes) {
    s_col <- paste0("state_", suffix)
    d_col <- paste0("district_", suffix)
    if (!all(c(s_col, d_col) %in% names(tracker))) next

    tracker$.legacy_state_key <- canonicalize_state_name(tracker[[s_col]])
    tracker$.legacy_district_key <- canon(tracker[[d_col]])
    tracker_open <- tracker[
      !tracker[[matched]] &
        !is.na(tracker$.legacy_state_key) &
        nzchar(tracker$.legacy_state_key) &
        !is.na(tracker$.legacy_district_key) &
        nzchar(tracker$.legacy_district_key),
      c(".tracker_row", ".legacy_state_key", ".legacy_district_key"),
      drop = FALSE
    ]
    source_open <- source[
      !source$.source_used &
        !is.na(source$.legacy_state_key) &
        nzchar(source$.legacy_state_key) &
        !is.na(source$.legacy_district_key) &
        nzchar(source$.legacy_district_key),
      c(".source_row", ".legacy_state_key", ".legacy_district_key"),
      drop = FALSE
    ]
    if (!nrow(tracker_open) || !nrow(source_open)) next

    chosen <- legacy_select_source_tracker_matches(source_open, tracker_open)
    if (!nrow(chosen)) next

    hits <- merge(chosen, source, by = ".source_row", sort = FALSE)
    rows <- match(hits$.tracker_row, tracker$.tracker_row)
    source_cols <- setdiff(
      names(hits),
      c(
        ".source_row", ".source_used", ".tracker_row",
        ".legacy_state_key", ".legacy_district_key",
        ".legacy_match_method", ".legacy_match_distance"
      )
    )
    tracker <- fill_source_values_by_tracker_row(tracker, hits, source_cols, rows)
    tracker[[matched]][rows[!is.na(rows)]] <- TRUE
    source$.source_used[match(hits$.source_row, source$.source_row)] <- TRUE
    if (all(source$.source_used)) break
  }

  tracker$.legacy_state_key <- NULL
  tracker$.legacy_district_key <- NULL
  tracker
}


legacy_select_source_tracker_matches <- function(
  source_open, tracker_open,
  methods = legacy_source_match_methods(),
  thresholds = legacy_source_match_thresholds()
) {
  if (!nrow(source_open) || !nrow(tracker_open)) {
    return(data.frame(
      .source_row = integer(),
      .tracker_row = integer(),
      .legacy_match_method = character(),
      .legacy_match_distance = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  if (length(methods) != length(thresholds)) {
    stop("Legacy district match methods and thresholds must have the same length.", call. = FALSE)
  }
  need_pkg("stringdist", "legacy district-source fuzzy matching")

  remaining_source <- source_open
  remaining_tracker <- tracker_open
  chosen <- data.frame(
    .source_row = integer(),
    .tracker_row = integer(),
    .legacy_match_method = character(),
    .legacy_match_distance = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(methods)) {
    candidates <- merge(
      remaining_source,
      remaining_tracker,
      by = ".legacy_state_key",
      suffixes = c("_source", "_tracker"),
      sort = FALSE
    )
    if (!nrow(candidates)) next

    candidates$.legacy_match_distance <- stringdist::stringdist(
      candidates$.legacy_district_key_source,
      candidates$.legacy_district_key_tracker,
      method = methods[[i]]
    )
    candidates <- candidates[
      is.finite(candidates$.legacy_match_distance) &
        candidates$.legacy_match_distance <= thresholds[[i]],
      ,
      drop = FALSE
    ]
    if (!nrow(candidates)) next
    candidates <- candidates[
      order(candidates$.legacy_match_distance, candidates$.source_row, candidates$.tracker_row),
      ,
      drop = FALSE
    ]

    picked <- data.frame(
      .source_row = integer(),
      .tracker_row = integer(),
      .legacy_match_method = character(),
      .legacy_match_distance = numeric(),
      stringsAsFactors = FALSE
    )
    used_source <- integer()
    used_tracker <- integer()
    for (j in seq_len(nrow(candidates))) {
      source_j <- candidates$.source_row[[j]]
      tracker_j <- candidates$.tracker_row[[j]]
      if (source_j %in% used_source || tracker_j %in% used_tracker) next
      picked <- rbind(
        picked,
        data.frame(
          .source_row = source_j,
          .tracker_row = tracker_j,
          .legacy_match_method = methods[[i]],
          .legacy_match_distance = candidates$.legacy_match_distance[[j]],
          stringsAsFactors = FALSE
        )
      )
      used_source <- c(used_source, source_j)
      used_tracker <- c(used_tracker, tracker_j)
    }
    if (!nrow(picked)) next

    chosen <- rbind(chosen, picked)
    remaining_source <- remaining_source[!remaining_source$.source_row %in% picked$.source_row, , drop = FALSE]
    remaining_tracker <- remaining_tracker[!remaining_tracker$.tracker_row %in% picked$.tracker_row, , drop = FALSE]
    if (!nrow(remaining_source) || !nrow(remaining_tracker)) break
  }

  rownames(chosen) <- NULL
  chosen
}

add_legacy_join_keys <- function(source, suffixes) {
  candidates <- unlist(lapply(suffixes, function(sfx) list(
    c(paste0("state_", sfx), paste0("district_", sfx)),
    c(paste0("state_", sfx), paste0("district_", sfx, "_name"))
  )), recursive = FALSE)
  candidates <- c(candidates, list(c("state_0708", "district_0708"), c("state_1718", "district_1718"), c("state", "district"), c("state", "district_name")))
  for (pair in candidates) {
    if (all(pair %in% names(source))) {
      source$.legacy_state_key <- canonicalize_state_name(source[[pair[[1]]]])
      source$.legacy_district_key <- canon(source[[pair[[2]]]])
      return(source)
    }
  }
  source
}

legacy_attach_source_by_standard_keys <- function(panel, source, source_label = NULL) {
  if (!nrow(panel) || !nrow(source)) return(panel)
  if (!all(c("state_std", "district_std") %in% names(panel))) return(panel)
  if (!all(c("state_std", "district_std") %in% names(source))) return(panel)

  source <- source[!is.na(source$state_std) & !is.na(source$district_std), , drop = FALSE]
  if (!nrow(source)) return(panel)
  source$.std_state_key <- canon(source$state_std)
  source$.std_district_key <- canon(source$district_std)
  source <- source[!duplicated(source[c(".std_state_key", ".std_district_key")]), , drop = FALSE]

  panel$.std_state_key <- canon(panel$state_std)
  panel$.std_district_key <- canon(panel$district_std)
  joined <- merge(
    panel[c(".tracker_row", ".std_state_key", ".std_district_key")],
    source,
    by = c(".std_state_key", ".std_district_key"),
    all.x = TRUE,
    sort = FALSE
  )

  source_cols <- setdiff(
    names(joined),
    c(".std_state_key", ".std_district_key", ".tracker_row", "state_std", "district_std")
  )
  if (!length(source_cols)) {
    panel$.std_state_key <- NULL
    panel$.std_district_key <- NULL
    return(panel)
  }

  has_value <- vapply(seq_len(nrow(joined)), function(i) {
    any(vapply(joined[i, source_cols, drop = FALSE], scalar_has_value, logical(1)))
  }, logical(1))
  hits <- joined[!is.na(joined$.tracker_row) & has_value, c(".tracker_row", source_cols), drop = FALSE]
  if (!nrow(hits)) {
    panel$.std_state_key <- NULL
    panel$.std_district_key <- NULL
    return(panel)
  }

  rows <- match(hits$.tracker_row, panel$.tracker_row)
  panel <- fill_source_values_by_tracker_row(panel, hits, source_cols, rows)

  if (!is.null(source_label)) {
    matched <- paste0(".matched_", source_label)
    if (!matched %in% names(panel)) panel[[matched]] <- FALSE
    panel[[matched]][rows[!is.na(rows)]] <- TRUE
  }

  panel$.std_state_key <- NULL
  panel$.std_district_key <- NULL
  panel
}

source_hits_with_values <- function(joined, source_cols) {
  if (!nrow(joined) || !length(source_cols)) return(joined[0, , drop = FALSE])
  has_value <- vapply(seq_len(nrow(joined)), function(i) {
    any(vapply(joined[i, source_cols, drop = FALSE], scalar_has_value, logical(1)))
  }, logical(1))
  joined[!is.na(joined$.tracker_row) & has_value, , drop = FALSE]
}

fill_source_values_by_tracker_row <- function(target, hits, source_cols, rows) {
  valid <- !is.na(rows)
  if (!any(valid) || !length(source_cols)) return(target)
  rows <- rows[valid]
  hits <- hits[valid, , drop = FALSE]
  for (nm in source_cols) {
    if (!nm %in% names(target)) target[[nm]] <- NA
    fill <- !vapply(target[[nm]][rows], scalar_has_value, logical(1)) &
      vapply(hits[[nm]], scalar_has_value, logical(1))
    if (any(fill)) target[[nm]][rows[fill]] <- hits[[nm]][fill]
  }
  target
}

scalar_has_value <- function(x) {
  if (is.data.frame(x)) return(nrow(x) > 0L)
  if (is.list(x)) {
    return(any(vapply(x, scalar_has_value, logical(1))))
  }
  length(x) > 0L && !all(is.na(x))
}
