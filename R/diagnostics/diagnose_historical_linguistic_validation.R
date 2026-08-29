# Historical linguistic-instrument geography diagnostics.

historical_linguistic_shrug_source_ids <- function() {
  c(
    "shrug_pc91r", "shrug_pc91u", "shrug_pc91dist",
    "shrug_pc01r", "shrug_pc01u", "shrug_pc01dist"
  )
}

require_historical_linguistic_shrug_sources <- function(raw_sources) {
  missing <- setdiff(historical_linguistic_shrug_source_ids(), names(raw_sources))
  if (length(missing)) {
    stop(
      "Historical linguistic geography is missing SHRUG sources: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

historical_1991_district_geography_summary <- function(bridge, min_population_coverage = 0.99) {
  out <- summarize_shrug_source_district_mapping(
    bridge,
    source_year = 1991L,
    target_year = 2001L,
    min_population_coverage = min_population_coverage
  )
  if (!nrow(out)) return(out)
  out$mapping_class[out$mapping_class == "splits_across_target_districts"] <-
    "splits_across_2001_districts"
  names(out)[names(out) == "population_total"] <- "population_1991_total"
  names(out)[names(out) == "population_deterministic"] <- "population_1991_deterministic"
  names(out)[names(out) == "n_target_districts"] <- "n_target_2001_districts"
  names(out)[names(out) == "exact_one_to_one"] <- "exact_language_persistence"
  names(out)[names(out) == "preferred_single_target"] <- "preferred_language_persistence"
  out
}

historical_linguistic_geography_sensitivity <- function(
    source_districts, thresholds = c(0.95, 0.98, 0.99, 0.995, 0.999, 1)) {
  x <- safe_df(source_districts)
  required <- c("population_coverage", "n_target_2001_districts")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Historical linguistic geography sensitivity lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  safe_bind_rows(lapply(thresholds, function(threshold) {
    eligible <- x$n_target_2001_districts == 1L &
      is.finite(x$population_coverage) & x$population_coverage >= threshold
    data.frame(
      population_coverage_threshold = threshold,
      eligible_districts = sum(eligible),
      eligible_population_1991 = sum(num(x$population_1991_total[eligible]), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

build_historical_linguistic_geography_1991_2001 <- function(raw_sources) {
  require_historical_linguistic_shrug_sources(raw_sources)
  bridge <- build_shrug_district_bridge_1991_2001(
    raw_sources$shrug_pc91r, raw_sources$shrug_pc91u,
    raw_sources$shrug_pc01r, raw_sources$shrug_pc01u,
    raw_sources$shrug_pc91dist, raw_sources$shrug_pc01dist
  )
  source_districts <- historical_1991_district_geography_summary(bridge)
  list(
    bridge = bridge,
    transition = build_district_transition_1991_2001(bridge),
    source_districts = source_districts,
    coverage_sensitivity = historical_linguistic_geography_sensitivity(source_districts),
    bridge_summary = summarize_shrid_bridge(bridge)
  )
}

save_historical_linguistic_geography_1991_2001 <- function(
    x, directory = "outputs/diagnostics/extended/instrument_relevance") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    source_districts = file.path(directory, "historical_linguistic_geography_1991_2001.csv"),
    coverage_sensitivity = file.path(directory, "historical_linguistic_geography_coverage_sensitivity.csv"),
    transition = file.path(directory, "historical_linguistic_transition_1991_2001.csv"),
    bridge_summary = file.path(directory, "historical_linguistic_shrid_bridge_1991_2001.csv")
  )
  write_diagnostic_csv(x$source_districts, paths[["source_districts"]])
  write_diagnostic_csv(x$coverage_sensitivity, paths[["coverage_sensitivity"]])
  write_diagnostic_csv(x$transition, paths[["transition"]])
  write_diagnostic_csv(x$bridge_summary, paths[["bridge_summary"]])
  unname(paths)
}

historical_linguistic_source_quality_geography_grid <- function(
    candidates, source_districts,
    coverage_thresholds = c(0.95, 0.98, 0.99, 0.995, 0.999),
    bound_width_thresholds = c(0.10, 0.25, 0.50, 1.00)) {
  source <- safe_df(candidates)
  geography <- safe_df(source_districts)
  source_required <- c(
    "state_code_1991", "district_code_1991", "atlas_population_1991",
    "atlas_source_status", "accepted_speaker_coverage_1991",
    "ling_distance_nonzero_bound_width_1991"
  )
  geography_required <- c(
    "state_code_1991", "district_code_1991",
    "preferred_language_persistence", "exact_language_persistence"
  )
  missing <- setdiff(source_required, names(source))
  if (length(missing)) {
    stop("Historical source-quality grid lacks language fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(geography_required, names(geography))
  if (length(missing)) {
    stop("Historical source-quality grid lacks geography fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  source$state_code_1991 <- pad_admin_code(source$state_code_1991, 2L)
  source$district_code_1991 <- pad_admin_code(source$district_code_1991, 2L)
  geography$state_code_1991 <- pad_admin_code(geography$state_code_1991, 2L)
  geography$district_code_1991 <- pad_admin_code(geography$district_code_1991, 2L)
  if (anyDuplicated(source[c("state_code_1991", "district_code_1991")])) {
    stop("Historical source-quality candidates have duplicate district keys.", call. = FALSE)
  }
  if (anyDuplicated(geography[c("state_code_1991", "district_code_1991")])) {
    stop("Historical source-quality geography has duplicate district keys.", call. = FALSE)
  }
  x <- merge(
    source[source_required], geography[geography_required],
    by = c("state_code_1991", "district_code_1991"), all.x = TRUE, sort = FALSE
  )
  base <- historical_linguistic_distance_quality_grid(
    source, coverage_thresholds, bound_width_thresholds
  )
  safe_bind_rows(lapply(seq_len(nrow(base)), function(i) {
    coverage_threshold <- base$min_accepted_coverage[[i]]
    bound_width_threshold <- base$max_distance_bound_width[[i]]
    source_ok <- x$atlas_source_status == "candidate" &
      num(x$accepted_speaker_coverage_1991) >= coverage_threshold &
      num(x$ling_distance_nonzero_bound_width_1991) <= bound_width_threshold
    preferred <- source_ok & x$preferred_language_persistence %in% TRUE
    exact <- source_ok & x$exact_language_persistence %in% TRUE
    data.frame(
      base[i, , drop = FALSE],
      n_preferred_geography = sum(preferred, na.rm = TRUE),
      preferred_geography_population_1991 = sum(num(x$atlas_population_1991[preferred]), na.rm = TRUE),
      n_preferred_states_1991 = length(unique(x$state_code_1991[preferred])),
      n_exact_geography = sum(exact, na.rm = TRUE),
      exact_geography_population_1991 = sum(num(x$atlas_population_1991[exact]), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

build_historical_linguistic_distance_validation <- function(atlas_source, geography) {
  required <- c("source_districts", "transition")
  missing <- setdiff(required, names(geography))
  if (length(missing)) {
    stop("Historical linguistic distance validation geography lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  candidates <- historical_linguistic_distance_1991_candidates(atlas_source)
  preferred <- apply_preferred_historical_linguistic_distance_quality_gate(candidates)
  list(
    preferred_rule = historical_linguistic_preferred_source_quality(),
    candidates = candidates,
    preferred_distance = preferred,
    source_quality_grid = historical_linguistic_distance_quality_grid(candidates),
    source_geography_grid = historical_linguistic_source_quality_geography_grid(
      candidates, geography$source_districts
    )
  )
}

historical_linguistic_carveout_benchmark <- function(
    geography, carveouts, admin_2001) {
  required <- c("source_districts", "transition")
  missing <- setdiff(required, names(geography))
  if (length(missing)) {
    stop("Historical linguistic geography object lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  source <- safe_df(geography$source_districts)
  transition <- safe_df(geography$transition)
  carveouts <- safe_df(carveouts)
  admin <- safe_df(admin_2001)
  require_fields <- function(x, fields, label) {
    missing <- setdiff(fields, names(x))
    if (length(missing)) stop(label, " lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  require_fields(
    source,
    c("state_code_1991", "district_code_1991", "population_1991_total"),
    "Historical linguistic geography"
  )
  require_fields(
    transition,
    c(
      "state_code_1991", "district_code_1991", "state_code_2001", "district_code_2001",
      "population_share_to_2001"
    ),
    "Historical linguistic transition"
  )
  require_fields(
    carveouts,
    c("district_1991", "pop_1991", "district_2001", "pct_01in91"),
    "Kumar-Somanathan carve-out source"
  )
  require_fields(admin, c("state_code", "district_code", "district_std"), "Census-2001 district registry")

  source$state_code_1991 <- pad_admin_code(source$state_code_1991, 2L)
  source$district_code_1991 <- pad_admin_code(source$district_code_1991, 2L)
  source$population_1991_total <- num(source$population_1991_total)
  transition$state_code_1991 <- pad_admin_code(transition$state_code_1991, 2L)
  transition$district_code_1991 <- pad_admin_code(transition$district_code_1991, 2L)
  transition$state_code_2001 <- pad_admin_code(transition$state_code_2001, 2L)
  transition$district_code_2001 <- pad_admin_code(transition$district_code_2001, 2L)
  transition$population_share_to_2001 <- num(transition$population_share_to_2001)
  admin$state_code <- pad_admin_code(admin$state_code, 2L)
  admin$district_code <- pad_admin_code(admin$district_code, 2L)
  admin$district_std <- canonicalize_district_name(admin$district_std)

  carveouts$.source_row <- seq_len(nrow(carveouts))
  carveouts$district_1991 <- plain_chr(carveouts$district_1991)
  carveouts$district_2001 <- plain_chr(carveouts$district_2001)
  carveouts$pop_1991 <- num(carveouts$pop_1991)
  carveouts$source_share_to_2001 <- num(carveouts$pct_01in91) / 100
  carveouts$target_district_std <- canonicalize_district_name(carveouts$district_2001)

  population_groups <- split(seq_len(nrow(source)), as.character(source$population_1991_total))
  population_match <- safe_bind_rows(lapply(population_groups, function(i) {
    rows <- source[i, , drop = FALSE]
    data.frame(
      pop_1991 = rows$population_1991_total[[1L]],
      n_geography_population_matches = nrow(rows),
      state_code_1991 = if (nrow(rows) == 1L) rows$state_code_1991[[1L]] else NA_character_,
      district_code_1991 = if (nrow(rows) == 1L) rows$district_code_1991[[1L]] else NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  out <- merge(carveouts, population_match, by = "pop_1991", all.x = TRUE, sort = FALSE)
  out$n_geography_population_matches <- num(out$n_geography_population_matches)
  out$n_geography_population_matches[!is.finite(out$n_geography_population_matches)] <- 0L

  target_registry <- unique(admin[c("state_code", "district_code", "district_std")])
  names(target_registry) <- c("state_code_2001", "district_code_2001", "target_district_std")
  bridge <- merge(
    transition, target_registry,
    by = c("state_code_2001", "district_code_2001"), all.x = TRUE, sort = FALSE
  )
  bridge <- bridge[c(
    "state_code_1991", "district_code_1991", "state_code_2001", "district_code_2001",
    "target_district_std", "population_share_to_2001"
  )]
  bridge$benchmark_key <- paste(
    bridge$state_code_1991, bridge$district_code_1991, bridge$target_district_std,
    sep = "__"
  )
  bridge_key_ok <- !is.na(bridge$target_district_std) & nzchar(bridge$target_district_std)
  if (anyDuplicated(bridge$benchmark_key[bridge_key_ok])) {
    stop("Historical SHRUG transition has duplicate source/target-name benchmark keys.", call. = FALSE)
  }
  out$benchmark_key <- paste(
    out$state_code_1991, out$district_code_1991, out$target_district_std,
    sep = "__"
  )
  bridge_match <- match(out$benchmark_key, bridge$benchmark_key)
  out$state_code_2001 <- bridge$state_code_2001[bridge_match]
  out$district_code_2001 <- bridge$district_code_2001[bridge_match]
  out$shrug_population_share_to_2001 <- bridge$population_share_to_2001[bridge_match]
  out$share_abs_diff <- abs(out$source_share_to_2001 - out$shrug_population_share_to_2001)
  out$benchmark_status <- ifelse(
    out$n_geography_population_matches == 0L,
    "source_population_not_found",
    ifelse(
      out$n_geography_population_matches > 1L,
      "source_population_not_unique",
      ifelse(is.na(bridge_match), "target_name_not_in_shrug_transition", "matched_edge")
    )
  )
  out <- out[order(out$.source_row), , drop = FALSE]
  out <- out[c(
    "district_1991", "pop_1991", "district_2001",
    "state_code_1991", "district_code_1991", "state_code_2001", "district_code_2001",
    "source_share_to_2001", "shrug_population_share_to_2001", "share_abs_diff",
    "n_geography_population_matches", "benchmark_status"
  )]
  rownames(out) <- NULL
  out
}

historical_linguistic_carveout_benchmark_summary <- function(benchmark) {
  x <- safe_df(benchmark)
  matched <- x[x$benchmark_status == "matched_edge", , drop = FALSE]
  source_key <- paste(x$district_1991, x$pop_1991, sep = "__")
  population_identified <- x[x$n_geography_population_matches == 1L, , drop = FALSE]
  population_identified_key <- paste(
    population_identified$district_1991, population_identified$pop_1991, sep = "__"
  )
  source_status <- unique(x[c("district_1991", "pop_1991", "n_geography_population_matches")])
  diff <- num(matched$share_abs_diff)
  diff <- diff[is.finite(diff)]
  data.frame(
    n_source_edges = nrow(x),
    n_source_districts = length(unique(source_key)),
    n_population_identified_source_districts = length(unique(population_identified_key)),
    n_population_not_found_source_districts = sum(source_status$n_geography_population_matches == 0L),
    n_population_ambiguous_source_districts = sum(source_status$n_geography_population_matches > 1L),
    share_source_districts_population_identified = if (length(unique(source_key))) {
      length(unique(population_identified_key)) / length(unique(source_key))
    } else {
      NA_real_
    },
    n_matched_edges = nrow(matched),
    share_source_edges_matched = if (nrow(x)) nrow(matched) / nrow(x) else NA_real_,
    median_absolute_share_difference = if (length(diff)) stats::median(diff, na.rm = TRUE) else NA_real_,
    p95_absolute_share_difference = if (length(diff)) unname(stats::quantile(diff, 0.95, na.rm = TRUE)) else NA_real_,
    max_absolute_share_difference = if (length(diff)) max(diff, na.rm = TRUE) else NA_real_,
    share_matched_edges_within_1pp = if (length(diff)) mean(diff <= 0.01, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

build_historical_linguistic_geography_external_benchmark <- function(
    geography, carveouts, admin_2001) {
  edges <- historical_linguistic_carveout_benchmark(geography, carveouts, admin_2001)
  list(
    edges = edges,
    summary = historical_linguistic_carveout_benchmark_summary(edges)
  )
}

save_historical_linguistic_geography_external_benchmark <- function(
    x, directory = "outputs/diagnostics/extended/instrument_relevance") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    edges = file.path(directory, "historical_linguistic_geography_carveout_benchmark.csv"),
    summary = file.path(directory, "historical_linguistic_geography_carveout_benchmark_summary.csv")
  )
  write_diagnostic_csv(x$edges, paths[["edges"]])
  write_diagnostic_csv(x$summary, paths[["summary"]])
  unname(paths)
}

historical_linguistic_rank_percentile <- function(x) {
  x <- num(x)
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  n <- sum(ok)
  if (!n) return(out)
  if (n == 1L) {
    out[ok] <- 0.5
    return(out)
  }
  out[ok] <- (rank(x[ok], ties.method = "average") - 1) / (n - 1)
  out
}

historical_linguistic_quintile <- function(x) {
  x <- num(x)
  out <- rep(NA_integer_, length(x))
  ok <- is.finite(x)
  n <- sum(ok)
  if (!n) return(out)
  out[ok] <- pmin(5L, pmax(1L, as.integer(ceiling(5 * rank(x[ok], ties.method = "average") / n))))
  out
}


historical_preferred_geography_panel <- function(source_districts, transition) {
  geography <- safe_df(source_districts)
  bridge <- safe_df(transition)
  geography_required <- c(
    "state_code_1991", "district_code_1991", "exact_language_persistence",
    "preferred_language_persistence", "population_coverage", "n_target_2001_districts"
  )
  transition_required <- c(
    "state_code_1991", "district_code_1991", "state_code_2001", "district_code_2001"
  )
  require_fields <- function(x, required, label) {
    missing <- setdiff(required, names(x))
    if (length(missing)) stop(label, " lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  require_fields(geography, geography_required, "Historical linguistic geography")
  require_fields(bridge, transition_required, "Historical linguistic transition")
  geography$state_code_1991 <- pad_admin_code(geography$state_code_1991, 2L)
  geography$district_code_1991 <- pad_admin_code(geography$district_code_1991, 2L)
  bridge$state_code_1991 <- pad_admin_code(bridge$state_code_1991, 2L)
  bridge$district_code_1991 <- pad_admin_code(bridge$district_code_1991, 2L)
  bridge$state_code_2001 <- pad_admin_code(bridge$state_code_2001, 2L)
  bridge$district_code_2001 <- pad_admin_code(bridge$district_code_2001, 2L)
  key_1991 <- function(x) paste(x$state_code_1991, x$district_code_1991, sep = "__")
  if (anyDuplicated(key_1991(geography))) stop("Historical linguistic geography has duplicate 1991 district keys.", call. = FALSE)

  transition_targets <- split(seq_len(nrow(bridge)), key_1991(bridge))
  target <- safe_bind_rows(lapply(transition_targets, function(i) {
    rows <- unique(bridge[i, transition_required, drop = FALSE])
    targets <- unique(rows[c("state_code_2001", "district_code_2001")])
    first <- rows[1L, c("state_code_1991", "district_code_1991"), drop = FALSE]
    if (nrow(targets) == 1L) {
      first$state_code_2001 <- targets$state_code_2001[[1L]]
      first$district_code_2001 <- targets$district_code_2001[[1L]]
    } else {
      first$state_code_2001 <- NA_character_
      first$district_code_2001 <- NA_character_
    }
    first$n_transition_targets <- nrow(targets)
    first
  }))
  out <- merge(geography, target, by = c("state_code_1991", "district_code_1991"), all.x = TRUE, sort = FALSE)
  transition_targets_n <- num(out$n_transition_targets)
  transition_targets_n[!is.finite(transition_targets_n)] <- 0L
  out$n_transition_targets <- as.integer(transition_targets_n)
  geography_targets_n <- num(out$n_target_2001_districts)
  if (any(!is.finite(geography_targets_n)) || any(transition_targets_n != geography_targets_n)) {
    stop("Historical geography summary and transition disagree on target-district counts.", call. = FALSE)
  }
  if (any(out$exact_language_persistence %in% TRUE & !(out$preferred_language_persistence %in% TRUE))) {
    stop("Exact historical geography must be a subset of preferred geography.", call. = FALSE)
  }
  preferred <- out$preferred_language_persistence %in% TRUE
  if (any(preferred & geography_targets_n != 1L)) {
    stop("Preferred historical geography must map to exactly one Census-2001 district.", call. = FALSE)
  }
  out
}

historical_linguistic_persistence_panel <- function(
    historical_distance, distance_2001, source_districts, transition) {
  historical <- safe_df(historical_distance)
  current <- safe_df(distance_2001)
  geography <- historical_preferred_geography_panel(source_districts, transition)

  historical_required <- c(
    "state_code_1991", "district_code_1991", "atlas_population_1991",
    "min_accepted_coverage", "max_distance_bound_width", "historical_language_status",
    "ling_distance_nonzero_mean_1991"
  )
  current_required <- c(
    "state_code_2001", "district_code_2001", "ling_distance_nonzero_mean"
  )
  missing_fields <- function(x, required, label) {
    missing <- setdiff(required, names(x))
    if (length(missing)) {
      stop(label, " lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  missing_fields(historical, historical_required, "Historical linguistic distance")
  missing_fields(current, current_required, "Census-2001 linguistic distance")
  historical$state_code_1991 <- pad_admin_code(historical$state_code_1991, 2L)
  historical$district_code_1991 <- pad_admin_code(historical$district_code_1991, 2L)
  current$state_code_2001 <- pad_admin_code(current$state_code_2001, 2L)
  current$district_code_2001 <- pad_admin_code(current$district_code_2001, 2L)
  key_1991 <- function(x) paste(x$state_code_1991, x$district_code_1991, sep = "__")
  key_2001 <- function(x) paste(x$state_code_2001, x$district_code_2001, sep = "__")
  if (anyDuplicated(key_1991(historical))) stop("Historical linguistic distance has duplicate 1991 district keys.", call. = FALSE)
  if (anyDuplicated(key_2001(current))) stop("Census-2001 linguistic distance has duplicate district keys.", call. = FALSE)
  thresholds <- unique(num(historical$min_accepted_coverage))
  thresholds <- thresholds[is.finite(thresholds)]
  if (length(thresholds) != 1L) {
    stop("Historical persistence requires one explicit accepted-speaker coverage threshold.", call. = FALSE)
  }
  bound_widths <- unique(num(historical$max_distance_bound_width))
  bound_widths <- bound_widths[is.finite(bound_widths)]
  if (length(bound_widths) != 1L) {
    stop("Historical persistence requires one explicit distance-bound threshold.", call. = FALSE)
  }

  out <- merge(
    historical, geography,
    by = c("state_code_1991", "district_code_1991"), all.x = TRUE, sort = FALSE
  )
  current_keep <- current[current_required]
  names(current_keep)[names(current_keep) == "ling_distance_nonzero_mean"] <-
    "ling_distance_nonzero_mean_2001"
  out <- merge(
    out, current_keep,
    by = c("state_code_2001", "district_code_2001"), all.x = TRUE, sort = FALSE
  )

  historical_ok <- out$historical_language_status %in% "eligible" &
    is.finite(num(out$ling_distance_nonzero_mean_1991))
  current_ok <- is.finite(num(out$ling_distance_nonzero_mean_2001))
  preferred_geo <- out$preferred_language_persistence %in% TRUE &
    num(out$n_transition_targets) == 1L
  out$persistence_status <- ifelse(
    !preferred_geo, "geography_not_preferred",
    ifelse(!historical_ok, "historical_language_ineligible",
      ifelse(!current_ok, "missing_2001_distance", "eligible"))
  )
  eligible <- out$persistence_status == "eligible"
  out$ling_distance_change_1991_2001 <- ifelse(
    eligible,
    num(out$ling_distance_nonzero_mean_2001) - num(out$ling_distance_nonzero_mean_1991),
    NA_real_
  )
  out$rank_percentile_1991 <- NA_real_
  out$rank_percentile_2001 <- NA_real_
  out$quintile_1991 <- NA_integer_
  out$quintile_2001 <- NA_integer_
  out$rank_percentile_1991[eligible] <- historical_linguistic_rank_percentile(
    out$ling_distance_nonzero_mean_1991[eligible]
  )
  out$rank_percentile_2001[eligible] <- historical_linguistic_rank_percentile(
    out$ling_distance_nonzero_mean_2001[eligible]
  )
  out$quintile_1991[eligible] <- historical_linguistic_quintile(
    out$ling_distance_nonzero_mean_1991[eligible]
  )
  out$quintile_2001[eligible] <- historical_linguistic_quintile(
    out$ling_distance_nonzero_mean_2001[eligible]
  )
  out$same_quintile <- ifelse(
    eligible,
    out$quintile_1991 == out$quintile_2001,
    NA
  )
  out$absolute_quintile_change <- ifelse(
    eligible,
    abs(out$quintile_2001 - out$quintile_1991),
    NA_integer_
  )
  out
}

historical_linguistic_weighted_correlation <- function(x, y, weight, rank_values = FALSE) {
  x <- num(x)
  y <- num(y)
  weight <- num(weight)
  ok <- is.finite(x) & is.finite(y) & is.finite(weight) & weight > 0
  if (sum(ok) < 2L) return(NA_real_)
  x <- x[ok]
  y <- y[ok]
  weight <- weight[ok]
  if (rank_values) {
    x <- rank(x, ties.method = "average")
    y <- rank(y, ties.method = "average")
  }
  if (length(unique(x)) < 2L || length(unique(y)) < 2L) return(NA_real_)
  unname(stats::cov.wt(cbind(x, y), wt = weight, cor = TRUE)$cor[1L, 2L])
}

historical_linguistic_persistence_metrics <- function(panel, exact_only = FALSE) {
  x <- safe_df(panel)
  keep <- x$persistence_status == "eligible"
  if (exact_only) keep <- keep & x$exact_language_persistence %in% TRUE
  x <- x[keep, , drop = FALSE]
  sample_name <- if (exact_only) "exact_one_to_one" else "preferred_geography"
  threshold <- unique(num(panel$min_accepted_coverage))
  threshold <- threshold[is.finite(threshold)]
  bound_width <- unique(num(panel$max_distance_bound_width))
  bound_width <- bound_width[is.finite(bound_width)]
  empty <- data.frame(
    sample = sample_name,
    min_accepted_coverage = if (length(threshold) == 1L) threshold else NA_real_,
    max_distance_bound_width = if (length(bound_width) == 1L) bound_width else NA_real_,
    n_districts = nrow(x), population_1991 = sum(num(x$atlas_population_1991), na.rm = TRUE),
    pearson = NA_real_, spearman = NA_real_,
    population_weighted_pearson = NA_real_, population_weighted_spearman = NA_real_,
    population_weighted_slope = NA_real_, population_weighted_r_squared = NA_real_,
    state_fe_population_weighted_slope = NA_real_, state_fe_population_weighted_r_squared = NA_real_,
    mean_absolute_change = NA_real_, mean_absolute_rank_change = NA_real_,
    same_quintile_share = NA_real_, mean_absolute_quintile_change = NA_real_,
    stringsAsFactors = FALSE
  )
  if (nrow(x) < 2L) return(empty)

  d91 <- num(x$ling_distance_nonzero_mean_1991)
  d01 <- num(x$ling_distance_nonzero_mean_2001)
  weight <- num(x$atlas_population_1991)
  ok <- is.finite(d91) & is.finite(d01) & is.finite(weight) & weight > 0
  if (sum(ok) < 2L) return(empty)
  x <- x[ok, , drop = FALSE]
  d91 <- d91[ok]
  d01 <- d01[ok]
  weight <- weight[ok]

  states <- plain_chr(x$state_code_2001)
  fit_data <- data.frame(
    d91 = d91, d01 = d01, state_code_2001 = states, weight = weight,
    stringsAsFactors = FALSE
  )
  weighted_fit <- stats::lm(d01 ~ d91, data = fit_data, weights = weight)
  state_fit <- if (length(unique(states)) >= 2L) {
    stats::lm(
      d01 ~ d91 + factor(state_code_2001),
      data = fit_data,
      weights = weight
    )
  } else {
    NULL
  }
  rank91 <- historical_linguistic_rank_percentile(d91)
  rank01 <- historical_linguistic_rank_percentile(d01)
  q91 <- historical_linguistic_quintile(d91)
  q01 <- historical_linguistic_quintile(d01)

  empty$n_districts <- nrow(x)
  empty$population_1991 <- sum(weight)
  empty$pearson <- if (length(unique(d91)) >= 2L && length(unique(d01)) >= 2L) {
    stats::cor(d91, d01, method = "pearson")
  } else {
    NA_real_
  }
  empty$spearman <- if (length(unique(d91)) >= 2L && length(unique(d01)) >= 2L) {
    stats::cor(d91, d01, method = "spearman")
  } else {
    NA_real_
  }
  empty$population_weighted_pearson <- historical_linguistic_weighted_correlation(
    d91, d01, weight, rank_values = FALSE
  )
  empty$population_weighted_spearman <- historical_linguistic_weighted_correlation(
    d91, d01, weight, rank_values = TRUE
  )
  empty$population_weighted_slope <- unname(stats::coef(weighted_fit)[["d91"]])
  empty$population_weighted_r_squared <- summary(weighted_fit)$r.squared
  if (!is.null(state_fit)) {
    empty$state_fe_population_weighted_slope <- unname(stats::coef(state_fit)[["d91"]])
    empty$state_fe_population_weighted_r_squared <- summary(state_fit)$r.squared
  }
  empty$mean_absolute_change <- mean(abs(d01 - d91))
  empty$mean_absolute_rank_change <- mean(abs(rank01 - rank91))
  empty$same_quintile_share <- mean(q01 == q91)
  empty$mean_absolute_quintile_change <- mean(abs(q01 - q91))
  empty
}

historical_linguistic_quintile_transition <- function(panel, exact_only = FALSE) {
  x <- safe_df(panel)
  keep <- x$persistence_status == "eligible"
  if (exact_only) keep <- keep & x$exact_language_persistence %in% TRUE
  x <- x[keep, , drop = FALSE]
  if (!nrow(x)) {
    return(data.frame(
      sample = character(), quintile_1991 = integer(), quintile_2001 = integer(),
      n_districts = integer(), population_1991 = numeric(), stringsAsFactors = FALSE
    ))
  }
  q91 <- historical_linguistic_quintile(x$ling_distance_nonzero_mean_1991)
  q01 <- historical_linguistic_quintile(x$ling_distance_nonzero_mean_2001)
  group <- interaction(q91, q01, drop = TRUE)
  sample_name <- if (exact_only) "exact_one_to_one" else "preferred_geography"
  safe_bind_rows(lapply(split(seq_len(nrow(x)), group), function(i) {
    data.frame(
      sample = sample_name,
      quintile_1991 = q91[[i[[1L]]]],
      quintile_2001 = q01[[i[[1L]]]],
      n_districts = length(i),
      population_1991 = sum(num(x$atlas_population_1991[i]), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

build_historical_linguistic_persistence_validation <- function(
    historical_distance, distance_2001, geography) {
  required <- c("source_districts", "transition")
  missing <- setdiff(required, names(geography))
  if (length(missing)) {
    stop("Historical linguistic geography object lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  panel <- historical_linguistic_persistence_panel(
    historical_distance, distance_2001, geography$source_districts, geography$transition
  )
  list(
    panel = panel,
    summary = safe_bind_rows(list(
      historical_linguistic_persistence_metrics(panel, exact_only = FALSE),
      historical_linguistic_persistence_metrics(panel, exact_only = TRUE)
    )),
    quintile_transition = safe_bind_rows(list(
      historical_linguistic_quintile_transition(panel, exact_only = FALSE),
      historical_linguistic_quintile_transition(panel, exact_only = TRUE)
    ))
  )
}

historical_linguistic_first_stage_registry <- function() {
  keep <- c(
    "instrument_only",
    "region_fe_census_controls",
    "state_fe_census_controls",
    "region_fe_expanded_controls",
    "state_fe_expanded_controls"
  )
  registry <- first_stage_absorption_registry()
  index <- match(keep, registry$specification_id)
  if (anyNA(index)) {
    stop("Historical first-stage registry is missing canonical absorption specifications.", call. = FALSE)
  }
  registry[index, , drop = FALSE]
}

historical_linguistic_predetermined_first_stage_registry <- function() {
  sets <- historical_baseline_1991_control_sets()
  data.frame(
    specification_id = c("state_fe_1991_pca_controls", "state_fe_1991_all_controls"),
    label = c(
      "1991 state FE + PCA91 predetermined controls",
      "1991 state FE + all selected 1991 predetermined controls"
    ),
    fixed_effect = "state_1991",
    controls = I(list(sets$pca, sets$all)),
    sequence = seq_len(2L),
    stringsAsFactors = FALSE
  )
}

historical_linguistic_first_stage_base_panel <- function(
    historical_distance, distance_2001, geography, district_panel,
    treatment = preferred_iv_variables()$treatment) {
  required <- c("source_districts", "transition")
  missing <- setdiff(required, names(geography))
  if (length(missing)) {
    stop("Historical linguistic geography object lacks: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  persistence <- historical_linguistic_persistence_panel(
    historical_distance, distance_2001,
    geography$source_districts, geography$transition
  )
  persistence <- persistence[persistence$persistence_status == "eligible", , drop = FALSE]
  if (!nrow(persistence)) {
    stop("No preferred historical districts are eligible for first-stage robustness.", call. = FALSE)
  }

  panel <- if (inherits(district_panel, "sf")) {
    sf::st_drop_geometry(district_panel)
  } else {
    as.data.frame(district_panel, stringsAsFactors = FALSE)
  }
  required_panel <- unique(c(
    "state_code_2001", "district_code_2001", "region", treatment,
    census_2001_diagnostic_controls()
  ))
  missing <- setdiff(required_panel, names(panel))
  if (length(missing)) {
    stop("Historical first-stage panel is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  panel$state_code_2001 <- pad_admin_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- pad_admin_code(panel$district_code_2001, 2L)
  key <- paste(panel$state_code_2001, panel$district_code_2001, sep = "__")
  if (anyDuplicated(key)) {
    stop("Historical first-stage panel has duplicate Census-2001 district keys.", call. = FALSE)
  }

  persistence_keep <- persistence[c(
    "state_code_1991", "district_code_1991", "state_code_2001", "district_code_2001",
    "exact_language_persistence", "min_accepted_coverage", "max_distance_bound_width",
    "ling_distance_nonzero_mean_1991", "ling_distance_nonzero_mean_2001"
  )]
  panel_keep <- panel[required_panel]
  merge(
    persistence_keep, panel_keep,
    by = c("state_code_2001", "district_code_2001"), all.x = TRUE, sort = FALSE
  )
}

historical_linguistic_first_stage_panel <- function(
    historical_distance, distance_2001, geography, district_panel,
    treatment = preferred_iv_variables()$treatment) {
  out <- historical_linguistic_first_stage_base_panel(
    historical_distance, distance_2001, geography, district_panel, treatment
  )
  prepare_first_stage_absorption_panel(
    out,
    treatment = treatment,
    instrument = c("ling_distance_nonzero_mean_1991", "ling_distance_nonzero_mean_2001")
  )
}

historical_linguistic_first_stage_estimates <- function(
    data, registry, treatment, exact_only = FALSE) {
  sample_name <- if (exact_only) "exact_one_to_one" else "preferred_geography"
  sample <- if (exact_only) {
    data[data$exact_language_persistence %in% TRUE, , drop = FALSE]
  } else {
    data
  }
  if (!nrow(sample)) return(data.frame())

  instruments <- c(
    historical_1991 = "ling_distance_nonzero_mean_1991",
    census_2001 = "ling_distance_nonzero_mean_2001"
  )
  safe_bind_rows(lapply(names(instruments), function(vintage) {
    instrument <- instruments[[vintage]]
    safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
      estimate <- estimate_first_stage_absorption_spec(
        sample, registry[i, , drop = FALSE], treatment, instrument
      )$summary
      estimate$sample <- sample_name
      estimate$instrument_vintage <- vintage
      estimate$min_accepted_coverage <- unique(num(sample$min_accepted_coverage))[[1L]]
      estimate$max_distance_bound_width <- unique(num(sample$max_distance_bound_width))[[1L]]
      estimate
    }))
  }))
}

historical_linguistic_attach_predetermined_controls <- function(panel, baseline_1991) {
  baseline <- validate_census_1991_district_keys(baseline_1991, "Historical first-stage 1991 baseline")
  variables <- historical_baseline_1991_variables()
  missing <- setdiff(variables, names(baseline))
  if (length(missing)) {
    stop("Historical first-stage 1991 baseline lacks variables: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  keep <- baseline[c(census_1991_keys(), variables)]
  merge(panel, keep, by = census_1991_keys(), all.x = TRUE, sort = FALSE)
}

historical_linguistic_predetermined_first_stage_sample <- function(
    panel, controls, treatment, exact_only = FALSE) {
  instruments <- c("ling_distance_nonzero_mean_1991", "ling_distance_nonzero_mean_2001")
  needed <- unique(c(treatment, instruments, controls, "state_code_1991", "region"))
  missing <- setdiff(needed, names(panel))
  if (length(missing)) {
    stop("Historical predetermined first-stage panel lacks variables: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  keep <- stats::complete.cases(panel[needed]) & nzchar(plain_chr(panel$state_code_1991))
  if (exact_only) keep <- keep & panel$exact_language_persistence %in% TRUE
  out <- panel[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

historical_linguistic_predetermined_first_stage_one <- function(
    data, specification, treatment, instrument) {
  controls <- unlist(specification$controls[[1L]], use.names = FALSE)
  rhs <- c(instrument, controls, "factor(state_code_1991)")
  fit <- stats::lm(stats::reformulate(rhs, response = treatment), data = data)
  inference <- clustered_lm_term_inference(fit, instrument, data$state_code_1991)
  nuisance <- c(controls, "factor(state_code_1991)")
  design <- stats::model.matrix(stats::reformulate(nuisance), data = data)
  residuals <- stats::lm.fit(
    design, cbind(instrument = num(data[[instrument]]), treatment = num(data[[treatment]]))
  )$residuals
  z_resid <- residuals[, "instrument"]
  d_resid <- residuals[, "treatment"]
  correlation <- stats::cor(z_resid, d_resid)
  data.frame(
    specification_id = specification$specification_id,
    specification = specification$label,
    sequence = specification$sequence,
    treatment = treatment,
    instrument = instrument,
    fixed_effect = specification$fixed_effect,
    control_blocks = if (specification$specification_id == "state_fe_1991_pca_controls") "predetermined_1991_pca" else "predetermined_1991_all",
    n_controls = length(controls),
    estimate = unname(stats::coef(fit)[[instrument]]),
    std.error = unname(inference[["std.error"]]),
    statistic = unname(inference[["statistic"]]),
    p.value = unname(inference[["p.value"]]),
    excluded_instrument_f = unname(inference[["partial_f"]]),
    partial_r_squared = if (is.finite(correlation)) correlation^2 else NA_real_,
    residual_instrument_sd = stats::sd(z_resid),
    residual_treatment_sd = stats::sd(d_resid),
    residual_correlation = correlation,
    instrument_variance_remaining = stats::var(z_resid) / stats::var(num(data[[instrument]])),
    n = stats::nobs(fit),
    n_states = length(unique(data$state_code_1991)),
    n_regions = length(unique(data$region)),
    status = "estimated",
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

historical_linguistic_predetermined_first_stage_estimates <- function(
    panel, registry, treatment, exact_only = FALSE) {
  sample_name <- if (exact_only) "exact_one_to_one" else "preferred_geography"
  instruments <- c(
    historical_1991 = "ling_distance_nonzero_mean_1991",
    census_2001 = "ling_distance_nonzero_mean_2001"
  )
  safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    specification <- registry[i, , drop = FALSE]
    controls <- unlist(specification$controls[[1L]], use.names = FALSE)
    sample <- historical_linguistic_predetermined_first_stage_sample(
      panel, controls, treatment, exact_only
    )
    if (!nrow(sample)) return(data.frame())
    safe_bind_rows(lapply(names(instruments), function(vintage) {
      estimate <- historical_linguistic_predetermined_first_stage_one(
        sample, specification, treatment, instruments[[vintage]]
      )
      estimate$sample <- sample_name
      estimate$instrument_vintage <- vintage
      estimate$min_accepted_coverage <- unique(num(sample$min_accepted_coverage))[[1L]]
      estimate$max_distance_bound_width <- unique(num(sample$max_distance_bound_width))[[1L]]
      estimate
    }))
  }))
}

historical_linguistic_first_stage_comparison <- function(estimates) {
  x <- safe_df(estimates)
  if (!nrow(x)) return(data.frame())
  id <- c(
    "sample", "specification_id", "specification", "sequence", "treatment",
    "fixed_effect", "control_blocks", "n_controls", "min_accepted_coverage",
    "max_distance_bound_width"
  )
  metrics <- c("estimate", "excluded_instrument_f", "partial_r_squared", "n", "n_states", "n_regions")
  one <- function(vintage, suffix) {
    out <- x[x$instrument_vintage == vintage, c(id, metrics), drop = FALSE]
    names(out)[match(metrics, names(out))] <- paste0(metrics, suffix)
    out
  }
  historical <- one("historical_1991", "_1991")
  current <- one("census_2001", "_2001")
  out <- merge(historical, current, by = id, all = TRUE, sort = FALSE)
  out$estimate_change_1991_vs_2001 <- out$estimate_1991 - out$estimate_2001
  out$f_change_1991_vs_2001 <- out$excluded_instrument_f_1991 - out$excluded_instrument_f_2001
  out$partial_r_squared_change_1991_vs_2001 <- out$partial_r_squared_1991 - out$partial_r_squared_2001
  out
}

build_historical_linguistic_first_stage_robustness <- function(
    historical_distance, distance_2001, geography, district_panel,
    treatment = preferred_iv_variables()$treatment, baseline_1991 = NULL) {
  base_panel <- historical_linguistic_first_stage_base_panel(
    historical_distance, distance_2001, geography, district_panel, treatment
  )
  panel <- prepare_first_stage_absorption_panel(
    base_panel, treatment = treatment,
    instrument = c("ling_distance_nonzero_mean_1991", "ling_distance_nonzero_mean_2001")
  )
  registry <- historical_linguistic_first_stage_registry()
  estimates <- safe_bind_rows(list(
    historical_linguistic_first_stage_estimates(panel, registry, treatment, exact_only = FALSE),
    historical_linguistic_first_stage_estimates(panel, registry, treatment, exact_only = TRUE)
  ))

  predetermined_registry <- historical_linguistic_predetermined_first_stage_registry()
  predetermined_estimates <- data.frame()
  if (!is.null(baseline_1991)) {
    predetermined_panel <- historical_linguistic_attach_predetermined_controls(
      base_panel, baseline_1991
    )
    predetermined_estimates <- safe_bind_rows(list(
      historical_linguistic_predetermined_first_stage_estimates(
        predetermined_panel, predetermined_registry, treatment, exact_only = FALSE
      ),
      historical_linguistic_predetermined_first_stage_estimates(
        predetermined_panel, predetermined_registry, treatment, exact_only = TRUE
      )
    ))
  }

  structure(
    list(
      panel = panel,
      registry = registry,
      estimates = estimates,
      comparison = historical_linguistic_first_stage_comparison(estimates),
      predetermined_registry = predetermined_registry,
      predetermined_estimates = predetermined_estimates,
      predetermined_comparison = historical_linguistic_first_stage_comparison(predetermined_estimates)
    ),
    class = "emi_historical_linguistic_first_stage"
  )
}

save_historical_linguistic_inference_validation <- function(
    distance_validation, persistence, first_stage,
    directory = "outputs/diagnostics/extended/instrument_relevance") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    distance = file.path(directory, "historical_linguistic_distance_1991.csv"),
    source_candidates = file.path(directory, "historical_linguistic_source_quality_candidates.csv"),
    source_quality_grid = file.path(directory, "historical_linguistic_source_quality_sensitivity.csv"),
    source_geography_grid = file.path(directory, "historical_linguistic_source_geography_sensitivity.csv"),
    persistence_panel = file.path(directory, "historical_linguistic_persistence_panel.csv"),
    persistence_summary = file.path(directory, "historical_linguistic_persistence_summary.csv"),
    quintile_transition = file.path(directory, "historical_linguistic_quintile_transition.csv"),
    first_stage_registry = file.path(directory, "historical_linguistic_first_stage_registry.csv"),
    first_stage_estimates = file.path(directory, "historical_linguistic_first_stage_estimates.csv"),
    first_stage_comparison = file.path(directory, "historical_linguistic_first_stage_comparison.csv"),
    predetermined_registry = file.path(directory, "historical_linguistic_predetermined_first_stage_registry.csv"),
    predetermined_estimates = file.path(directory, "historical_linguistic_predetermined_first_stage_estimates.csv"),
    predetermined_comparison = file.path(directory, "historical_linguistic_predetermined_first_stage_comparison.csv")
  )
  write_diagnostic_csv(distance_validation$preferred_distance, paths[["distance"]])
  write_diagnostic_csv(distance_validation$candidates, paths[["source_candidates"]])
  write_diagnostic_csv(distance_validation$source_quality_grid, paths[["source_quality_grid"]])
  write_diagnostic_csv(distance_validation$source_geography_grid, paths[["source_geography_grid"]])
  write_diagnostic_csv(persistence$panel, paths[["persistence_panel"]])
  write_diagnostic_csv(persistence$summary, paths[["persistence_summary"]])
  write_diagnostic_csv(persistence$quintile_transition, paths[["quintile_transition"]])
  write_diagnostic_csv(
    collapse_diagnostic_list_columns(first_stage$registry, "controls"),
    paths[["first_stage_registry"]]
  )
  write_diagnostic_csv(first_stage$estimates, paths[["first_stage_estimates"]])
  write_diagnostic_csv(first_stage$comparison, paths[["first_stage_comparison"]])
  write_diagnostic_csv(
    collapse_diagnostic_list_columns(first_stage$predetermined_registry, "controls"),
    paths[["predetermined_registry"]]
  )
  write_diagnostic_csv(first_stage$predetermined_estimates, paths[["predetermined_estimates"]])
  write_diagnostic_csv(first_stage$predetermined_comparison, paths[["predetermined_comparison"]])
  unname(paths)
}
