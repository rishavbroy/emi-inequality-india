# Review-first source matching and primary-panel eligibility for lineage v2.
# Names generate candidates. Only the tracked adjudication ledger can accept a
# source identity for production migration.

normalize_match_text <- function(x) {
  canonicalize_district_name(x)
}

token_jaccard_similarity <- function(x, y) {
  one <- function(a, b) {
    aa <- unique(strsplit(normalize_match_text(a), " ", fixed = TRUE)[[1]])
    bb <- unique(strsplit(normalize_match_text(b), " ", fixed = TRUE)[[1]])
    aa <- aa[nzchar(aa)]
    bb <- bb[nzchar(bb)]
    union_n <- length(union(aa, bb))
    if (!union_n) return(NA_real_)
    length(intersect(aa, bb)) / union_n
  }
  mapply(one, plain_chr(x), plain_chr(y), USE.NAMES = FALSE)
}

district_match_candidate_thresholds <- function() {
  c(jw = 0.90, dl = 0.70, trigram = 0.55, margin = 0.05)
}

empty_nss_district_roster_v2 <- function() {
  data.frame(
    source_row_id = character(), source_key = character(), wave = character(),
    source_code = character(), raw_state = character(), raw_district = character(),
    state_std = character(), district_std = character(), stringsAsFactors = FALSE
  )
}

#' Build a compact source-district roster
#'
#' The roster preserves the wave-specific NSS code when available. Those codes
#' are useful source-row identifiers, but they are not assumed to be Census or
#' LGD codes.
build_nss_district_roster_v2 <- function(source_2007, source_2017) {
  one <- function(x, wave, code_candidates, state_candidates, district_candidates) {
    x <- safe_df(x)
    required <- c("state_std", "district_std")
    if (!all(required %in% names(x))) return(empty_nss_district_roster_v2())
    code_col <- first_col(x, code_candidates)
    state_col <- first_col(x, state_candidates)
    district_col <- first_col(x, district_candidates)
    out <- data.frame(
      wave = wave,
      source_code = if (!is.null(code_col)) plain_chr(x[[code_col]]) else NA_character_,
      raw_state = if (!is.null(state_col)) plain_chr(x[[state_col]]) else plain_chr(x$state_std),
      raw_district = if (!is.null(district_col)) plain_chr(x[[district_col]]) else plain_chr(x$district_std),
      state_std = canonicalize_state_name(x$state_std),
      district_std = canonicalize_district_name(x$district_std),
      stringsAsFactors = FALSE
    )
    out <- unique(out[
      !is.na(out$state_std) & nzchar(out$state_std) &
        !is.na(out$district_std) & nzchar(out$district_std),
      , drop = FALSE
    ])
    identity <- ifelse(
      !is.na(out$source_code) & nzchar(out$source_code),
      out$source_code,
      out$district_std
    )
    out$source_key <- paste(wave, out$state_std, identity, sep = "__")
    out$source_row_id <- paste(out$source_key, out$district_std, sep = "__")
    groups <- split(seq_len(nrow(out)), out$source_row_id)
    safe_bind_rows(lapply(groups, function(i) {
      collapse_raw <- function(value) paste(sort(unique(value[!is.na(value) & nzchar(value)])), collapse = " | ")
      data.frame(
        source_row_id = out$source_row_id[[i[[1]]]],
        source_key = out$source_key[[i[[1]]]],
        wave = out$wave[[i[[1]]]],
        source_code = out$source_code[[i[[1]]]],
        raw_state = collapse_raw(out$raw_state[i]),
        raw_district = collapse_raw(out$raw_district[i]),
        state_std = out$state_std[[i[[1]]]],
        district_std = out$district_std[[i[[1]]]],
        stringsAsFactors = FALSE
      )
    }))
  }

  out <- safe_bind_rows(list(
    one(
      source_2007, "nss_2007_08",
      c("district_code_0708", "district_code", "source_code"),
      c("state_0708", "state_07", "state_08", "state"),
      c("district_0708", "district_07", "district_08", "district")
    ),
    one(
      source_2017, "nss_2017_18",
      c("district_code_1718", "district_code", "source_code"),
      c("state_1718", "state_17", "state_18", "state"),
      c("district_1718", "district_17", "district_18", "district")
    )
  ))
  if (!nrow(out)) empty_nss_district_roster_v2() else out
}

build_reference_units_v2 <- function(admin_2001, admin_2011, lgd_states = data.frame(), lgd_districts = data.frame()) {
  wanted <- c(
    "unit_id", "level", "state_code", "district_code", "state_std", "district_std",
    "valid_from", "valid_to", "source_id", "reference_vintage"
  )

  normalize_registry <- function(x, vintage) {
    x <- safe_df(x)
    for (nm in setdiff(wanted, names(x))) x[[nm]] <- rep(NA_character_, nrow(x))
    x$reference_vintage <- rep(vintage, nrow(x))
    x[wanted]
  }

  a01 <- normalize_registry(admin_2001, "2001")
  a11 <- safe_df(admin_2011)
  if (nrow(a11)) {
    states <- standardize_lgd_registry(lgd_states, "state")
    state_lookup <- unique(states[c("census2011_state_code", "state_name")])
    a11 <- merge(
      a11, state_lookup,
      by.x = "state_code", by.y = "census2011_state_code",
      all.x = TRUE, sort = FALSE
    )
    a11$state_std <- canonicalize_state_name(a11$state_name)
  }
  a11 <- normalize_registry(a11, "2011")

  lgd <- standardize_lgd_registry(lgd_districts, "district")
  lgd <- lgd[!is.na(lgd$district_lgd_code) & !is.na(lgd$district_name), , drop = FALSE]
  current <- data.frame(
    unit_id = paste0("lgd_district__", lgd$district_lgd_code),
    level = rep("district", nrow(lgd)),
    state_code = pad_admin_code(lgd$census2011_state_code, 2L),
    district_code = pad_admin_code(lgd$census2011_district_code, 3L),
    state_std = canonicalize_state_name(lgd$state_name),
    district_std = canonicalize_district_name(lgd$district_name),
    valid_from = NA_character_,
    valid_to = NA_character_,
    source_id = rep("lgd_districts", nrow(lgd)),
    reference_vintage = rep("current_lgd", nrow(lgd)),
    stringsAsFactors = FALSE
  )

  out <- unique(safe_bind_rows(list(a01, a11, current[wanted])))
  out[
    !is.na(out$state_std) & nzchar(out$state_std) &
      !is.na(out$district_std) & nzchar(out$district_std),
    , drop = FALSE
  ]
}

empty_source_matches_v2 <- function() {
  data.frame(
    source_row_id = character(), wave = character(), raw_state = character(), raw_district = character(),
    unit_id = character(), reference_vintage = character(), method = character(), source_id = character(),
    status = character(), note = character(), stringsAsFactors = FALSE
  )
}

empty_source_candidates_v2 <- function() {
  data.frame(
    source_row_id = character(), wave = character(), source_code = character(), state_std = character(),
    source_name_raw = character(), source_name = character(), candidate_unit = character(), candidate_name = character(),
    candidate_source_id = character(), reference_vintage = character(), candidate_method = character(), rank = integer(),
    jw = numeric(), dl = numeric(), trigram = numeric(), token = numeric(), score = numeric(),
    margin = numeric(), reciprocal_nearest = logical(), high_precision_candidate = logical(),
    stringsAsFactors = FALSE
  )
}

vintage_preference_v2 <- function(wave) {
  if (identical(wave, "nss_2007_08")) c("2001", "2011", "current_lgd") else c("2011", "current_lgd", "2001")
}

#' Exact-name candidates
#'
#' Exact names are not accepted automatically because unchanged names can hide
#' boundary changes. The output joins the same candidate ledger as fuzzy scores.
exact_source_candidates_v2 <- function(source_roster, reference_units) {
  source_roster <- safe_df(source_roster)
  reference_units <- safe_df(reference_units)
  if (!"raw_district" %in% names(source_roster)) source_roster$raw_district <- source_roster$district_std
  pairs <- merge(
    source_roster, reference_units,
    by = c("state_std", "district_std"),
    all = FALSE, sort = FALSE
  )
  if (!nrow(pairs)) return(empty_source_candidates_v2())

  groups <- split(seq_len(nrow(pairs)), pairs$source_row_id)
  safe_bind_rows(lapply(groups, function(i) {
    x <- pairs[i, , drop = FALSE]
    preference <- vintage_preference_v2(x$wave[[1]])
    vintage_rank <- match(x$reference_vintage, preference, nomatch = length(preference) + 1L)
    x <- x[order(vintage_rank, x$unit_id), , drop = FALSE]
    data.frame(
      source_row_id = x$source_row_id,
      wave = x$wave,
      source_code = x$source_code,
      state_std = x$state_std,
      source_name_raw = x$raw_district,
      source_name = x$district_std,
      candidate_unit = x$unit_id,
      candidate_name = x$district_std,
      candidate_source_id = x$source_id,
      reference_vintage = x$reference_vintage,
      candidate_method = "exact_normalized_name",
      rank = seq_len(nrow(x)),
      jw = 1, dl = 1, trigram = 1, token = 1, score = 1,
      margin = NA_real_,
      reciprocal_nearest = nrow(x) == 1L,
      high_precision_candidate = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}

score_match_candidates_v2 <- function(source_roster, reference_units, excluded_source_ids = character()) {
  need_pkg("stringdist", "district-lineage candidate scores")
  source_roster <- safe_df(source_roster)
  reference_units <- safe_df(reference_units)
  if (!"raw_district" %in% names(source_roster)) source_roster$raw_district <- source_roster$district_std
  source_open <- source_roster[!source_roster$source_row_id %in% excluded_source_ids, , drop = FALSE]
  if (!nrow(source_open)) return(empty_source_candidates_v2())

  pairs <- merge(
    source_open, reference_units,
    by = "state_std", all = FALSE, sort = FALSE,
    suffixes = c("_source", "_candidate")
  )
  if (!nrow(pairs)) return(empty_source_candidates_v2())
  source_name <- pairs$district_std_source
  candidate_name <- pairs$district_std_candidate
  pairs$jw <- 1 - stringdist::stringdist(source_name, candidate_name, method = "jw", p = 0.1)
  dl_distance <- stringdist::stringdist(source_name, candidate_name, method = "dl")
  denom <- pmax(nchar(source_name), nchar(candidate_name), 1L)
  pairs$dl <- pmax(0, 1 - dl_distance / denom)
  pairs$trigram <- stringdist::stringsim(source_name, candidate_name, method = "cosine", q = 3)
  pairs$token <- token_jaccard_similarity(source_name, candidate_name)
  pairs$score <- rowMeans(pairs[c("jw", "dl", "trigram", "token")], na.rm = TRUE)

  ranked <- safe_bind_rows(lapply(split(seq_len(nrow(pairs)), pairs$source_row_id), function(i) {
    x <- pairs[i, , drop = FALSE]
    x <- x[order(-x$score, -x$jw, x$unit_id), , drop = FALSE]
    x$rank <- seq_len(nrow(x))
    second <- if (nrow(x) >= 2L) x$score[[2]] else -Inf
    x$margin <- ifelse(x$rank == 1L, x$score - second, NA_real_)
    x
  }))

  candidate_groups <- split(seq_len(nrow(ranked)), ranked$unit_id)
  best_by_candidate <- stats::setNames(
    vapply(candidate_groups, function(i) {
      score <- ranked$score[i]
      ranked$source_row_id[i[which.max(score)]]
    }, character(1)),
    names(candidate_groups)
  )
  ranked$reciprocal_nearest <- ranked$rank == 1L &
    unname(best_by_candidate[ranked$unit_id]) == ranked$source_row_id
  threshold <- district_match_candidate_thresholds()
  ranked$high_precision_candidate <- ranked$rank == 1L & ranked$reciprocal_nearest &
    ranked$jw >= threshold[["jw"]] & ranked$dl >= threshold[["dl"]] &
    ranked$trigram >= threshold[["trigram"]] & ranked$margin >= threshold[["margin"]]
  ranked <- ranked[ranked$rank <= 5L, , drop = FALSE]

  data.frame(
    source_row_id = ranked$source_row_id,
    wave = ranked$wave,
    source_code = ranked$source_code,
    state_std = ranked$state_std,
    source_name_raw = ranked$raw_district,
    source_name = ranked$district_std_source,
    candidate_unit = ranked$unit_id,
    candidate_name = ranked$district_std_candidate,
    candidate_source_id = ranked$source_id,
    reference_vintage = ranked$reference_vintage,
    candidate_method = "fuzzy_name_candidate",
    rank = ranked$rank,
    jw = ranked$jw,
    dl = ranked$dl,
    trigram = ranked$trigram,
    token = ranked$token,
    score = ranked$score,
    margin = ranked$margin,
    reciprocal_nearest = ranked$reciprocal_nearest,
    high_precision_candidate = ranked$high_precision_candidate,
    stringsAsFactors = FALSE
  )
}

build_source_candidate_ledger_v2 <- function(source_roster, reference_units, adjudications = data.frame()) {
  adjudications <- read_adjudicated_source_matches_v2(adjudications)
  resolved <- adjudications$source_row_id[adjudications$status %in% c("accepted", "excluded")]
  exact <- exact_source_candidates_v2(source_roster, reference_units)
  exact_open <- exact[!exact$source_row_id %in% resolved, , drop = FALSE]
  exact_ids <- unique(exact_open$source_row_id)
  fuzzy <- score_match_candidates_v2(
    source_roster,
    reference_units,
    excluded_source_ids = union(resolved, exact_ids)
  )
  safe_bind_rows(list(exact_open, fuzzy))
}

read_adjudicated_source_matches_v2 <- function(x) {
  x <- safe_df(x)
  required <- c(
    "source_row_id", "wave", "raw_state", "raw_district", "unit_id",
    "method", "source_id", "status", "note"
  )
  for (nm in setdiff(required, names(x))) x[[nm]] <- NA_character_
  x <- x[!is.na(x$source_row_id) & nzchar(x$source_row_id), required, drop = FALSE]
  if (anyDuplicated(x$source_row_id)) {
    stop("District source adjudications must contain at most one row per source_row_id.", call. = FALSE)
  }
  allowed <- c("accepted", "excluded", "needs_review")
  invalid <- unique(x$status[!is.na(x$status) & nzchar(x$status) & !x$status %in% allowed])
  if (length(invalid)) stop("Unknown district source adjudication status: ", paste(invalid, collapse = ", "), call. = FALSE)
  x
}

build_adjudicated_source_matches_v2 <- function(adjudications, reference_units) {
  adjudications <- read_adjudicated_source_matches_v2(adjudications)
  if (!nrow(adjudications)) return(empty_source_matches_v2())
  reference <- unique(safe_df(reference_units)[c("unit_id", "reference_vintage")])
  accepted <- adjudications$status %in% "accepted"
  missing_unit <- accepted & (!adjudications$unit_id %in% reference$unit_id)
  if (any(missing_unit)) {
    stop(
      "Accepted district source adjudications reference unknown units: ",
      paste(adjudications$unit_id[missing_unit], collapse = ", "),
      call. = FALSE
    )
  }
  out <- merge(adjudications, reference, by = "unit_id", all.x = TRUE, sort = FALSE)
  out[c(
    "source_row_id", "wave", "raw_state", "raw_district", "unit_id",
    "reference_vintage", "method", "source_id", "status", "note"
  )]
}

resolve_lineage_terminals_v2 <- function(unit_ids, admin_events, admin_2001, admin_2011, max_depth = 30L) {
  events <- safe_df(admin_events)
  if (!nrow(events)) {
    events <- data.frame(from_unit = character(), to_unit = character(), status = character(), stringsAsFactors = FALSE)
  }
  for (nm in c("from_unit", "to_unit", "status")) {
    if (!nm %in% names(events)) events[[nm]] <- rep(NA_character_, nrow(events))
  }
  events <- unique(events[
    events$status %in% "accepted" &
      !is.na(events$from_unit) & nzchar(events$from_unit) &
      !is.na(events$to_unit) & nzchar(events$to_unit),
    c("from_unit", "to_unit"),
    drop = FALSE
  ])
  units_2001 <- unique(plain_chr(safe_df(admin_2001)$unit_id %||% character()))
  units_2011 <- unique(plain_chr(safe_df(admin_2011)$unit_id %||% character()))

  resolve_one <- function(start) {
    if (is.na(start) || !nzchar(start)) {
      return(c(terminal_unit = NA_character_, terminal_vintage = NA_character_, resolution_status = "missing_source_unit", lineage_path = NA_character_))
    }
    current <- start
    path <- current
    for (depth in seq_len(max_depth + 1L)) {
      if (current %in% units_2001) {
        return(c(terminal_unit = current, terminal_vintage = "2001", resolution_status = "resolved", lineage_path = paste(path, collapse = " <- ")))
      }
      if (current %in% units_2011) {
        return(c(terminal_unit = current, terminal_vintage = "2011", resolution_status = "resolved", lineage_path = paste(path, collapse = " <- ")))
      }
      parents <- unique(events$from_unit[events$to_unit == current])
      parents <- parents[!is.na(parents) & nzchar(parents)]
      if (!length(parents)) {
        return(c(terminal_unit = NA_character_, terminal_vintage = NA_character_, resolution_status = "no_accepted_parent_edge", lineage_path = paste(path, collapse = " <- ")))
      }
      if (length(parents) > 1L) {
        return(c(terminal_unit = NA_character_, terminal_vintage = NA_character_, resolution_status = "multiple_parent_non_nested", lineage_path = paste(c(path, parents), collapse = " <- ")))
      }
      current <- parents[[1]]
      if (current %in% path) {
        return(c(terminal_unit = NA_character_, terminal_vintage = NA_character_, resolution_status = "lineage_cycle", lineage_path = paste(c(path, current), collapse = " <- ")))
      }
      path <- c(path, current)
    }
    c(terminal_unit = NA_character_, terminal_vintage = NA_character_, resolution_status = "lineage_depth_exceeded", lineage_path = paste(path, collapse = " <- "))
  }

  unit_ids <- plain_chr(unit_ids)
  if (!length(unit_ids)) {
    return(data.frame(
      source_unit = character(), terminal_unit = character(), terminal_vintage = character(),
      resolution_status = character(), lineage_path = character(), stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(unit_ids, resolve_one)
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  out$source_unit <- plain_chr(unit_ids)
  out[c("source_unit", "terminal_unit", "terminal_vintage", "resolution_status", "lineage_path")]
}

normalize_admin_lookup_v2 <- function(x) {
  x <- safe_df(x)
  required <- c("unit_id", "state_code", "district_code")
  for (nm in setdiff(required, names(x))) x[[nm]] <- rep(NA_character_, nrow(x))
  x[required]
}

build_primary_mapping_eligibility <- function(
  source_roster, source_matches, transition_2001_2011,
  admin_2001, admin_2011, admin_events = data.frame()
) {
  source_roster <- safe_df(source_roster)
  matches <- safe_df(source_matches)
  if (!nrow(matches)) matches <- empty_source_matches_v2()
  out <- merge(
    source_roster,
    matches[c("source_row_id", "unit_id", "reference_vintage", "method", "status")],
    by = "source_row_id", all.x = TRUE, sort = FALSE
  )

  resolution <- resolve_lineage_terminals_v2(out$unit_id, admin_events, admin_2001, admin_2011)
  resolution$row_order <- seq_len(nrow(resolution))
  out$row_order <- seq_len(nrow(out))
  out <- merge(out, resolution, by.x = c("unit_id", "row_order"), by.y = c("source_unit", "row_order"), all.x = TRUE, sort = FALSE)

  a01 <- normalize_admin_lookup_v2(admin_2001)
  names(a01)[2:3] <- c("target_state_code_2001", "target_district_code_2001")
  out <- merge(out, a01, by.x = "terminal_unit", by.y = "unit_id", all.x = TRUE, sort = FALSE)

  a11 <- normalize_admin_lookup_v2(admin_2011)
  names(a11)[2:3] <- c("source_state_code_2011", "source_district_code_2011")
  out <- merge(out, a11, by.x = "terminal_unit", by.y = "unit_id", all.x = TRUE, sort = FALSE)
  transition <- safe_df(transition_2001_2011)
  deterministic <- transition[transition$mapping_class == "deterministic_containment", , drop = FALSE]
  if (nrow(deterministic)) {
    out <- merge(
      out,
      deterministic[c(
        "state_code_2011", "district_code_2011", "state_code_2001",
        "district_code_2001", "population_share_to_2001"
      )],
      by.x = c("source_state_code_2011", "source_district_code_2011"),
      by.y = c("state_code_2011", "district_code_2011"),
      all.x = TRUE, sort = FALSE
    )
  } else {
    out$state_code_2001 <- NA_character_
    out$district_code_2001 <- NA_character_
    out$population_share_to_2001 <- NA_real_
  }

  target_lookup <- normalize_admin_lookup_v2(admin_2001)
  names(target_lookup) <- c("bridged_target_unit_2001", "state_code_2001", "district_code_2001")
  out <- merge(
    out, target_lookup,
    by = c("state_code_2001", "district_code_2001"),
    all.x = TRUE, sort = FALSE
  )

  accepted <- out$status %in% "accepted"
  direct <- accepted & out$terminal_vintage %in% "2001"
  bridged <- accepted & out$terminal_vintage %in% "2011" & !is.na(out$bridged_target_unit_2001)
  out$mapping_class <- ifelse(
    direct,
    "identity_or_documented_rename_to_2001",
    ifelse(bridged, "deterministic_2011_to_2001", "unresolved_or_non_nested")
  )
  out$eligible_primary <- direct | bridged
  out$target_unit_2001 <- ifelse(direct, out$terminal_unit, out$bridged_target_unit_2001)
  out$target_state_code_2001 <- ifelse(direct, out$target_state_code_2001, out$state_code_2001)
  out$target_district_code_2001 <- ifelse(direct, out$target_district_code_2001, out$district_code_2001)
  out$exclusion_reason <- ifelse(
    out$eligible_primary,
    NA_character_,
    ifelse(
      out$status %in% "excluded",
      "documented_exclusion",
      ifelse(
        !(out$status %in% "accepted"),
        "source_identity_unadjudicated",
        ifelse(
          out$resolution_status != "resolved",
          paste0("geographic_lineage_", out$resolution_status),
          "geographic_transition_non_nested_or_incomplete"
        )
      )
    )
  )
  out <- out[order(out$row_order), , drop = FALSE]
  out$row_order <- NULL
  out
}

build_primary_source_crosswalk_v2 <- function(primary_eligibility) {
  x <- safe_df(primary_eligibility)
  required <- c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "state_std", "district_std", "target_unit_2001",
    "target_state_code_2001", "target_district_code_2001",
    "mapping_class", "eligible_primary"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Primary mapping eligibility is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- x[x$eligible_primary %in% TRUE, required, drop = FALSE]
  if (!nrow(out)) return(out)
  if (anyDuplicated(out$source_row_id)) {
    stop("Primary district source crosswalk must contain one row per source_row_id.", call. = FALSE)
  }
  incomplete <- is.na(out$target_unit_2001) | !nzchar(out$target_unit_2001)
  if (any(incomplete)) {
    stop("Primary district source crosswalk contains missing 2001 targets.", call. = FALSE)
  }
  out$eligible_primary <- NULL
  out
}

build_excluded_source_rows_v2 <- function(primary_eligibility) {
  x <- safe_df(primary_eligibility)
  required <- c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "state_std", "district_std", "exclusion_reason", "eligible_primary"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Primary mapping eligibility is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- x[!(x$eligible_primary %in% TRUE), required, drop = FALSE]
  out$eligible_primary <- NULL
  out
}

score_gold_set_v2 <- function(gold) {
  gold <- safe_df(gold)
  if (!nrow(gold)) return(gold)
  need_pkg("stringdist", "district match gold-set scoring")
  gold$source_key <- normalize_match_text(gold$source_name)
  gold$reference_key <- normalize_match_text(gold$reference_name)
  gold$jw <- 1 - stringdist::stringdist(gold$source_key, gold$reference_key, method = "jw", p = 0.1)
  raw_dl <- stringdist::stringdist(gold$source_key, gold$reference_key, method = "dl")
  gold$dl <- pmax(0, 1 - raw_dl / pmax(nchar(gold$source_key), nchar(gold$reference_key), 1L))
  gold$trigram <- stringdist::stringsim(gold$source_key, gold$reference_key, method = "cosine", q = 3)
  gold$token <- token_jaccard_similarity(gold$source_key, gold$reference_key)
  threshold <- district_match_candidate_thresholds()
  gold$passes_name_rule <- gold$jw >= threshold[["jw"]] & gold$dl >= threshold[["dl"]] &
    gold$trigram >= threshold[["trigram"]]
  gold
}

summarize_gold_set_v2 <- function(scored_gold) {
  x <- safe_df(scored_gold)
  if (!nrow(x)) return(data.frame())
  positive <- x$label == "match"
  negative <- x$label == "nonmatch"
  data.frame(
    metric = c("reviewed_matches", "reviewed_nonmatches", "match_rule_recall", "observed_nonmatch_acceptances"),
    value = c(
      sum(positive),
      sum(negative),
      if (any(positive)) mean(x$passes_name_rule[positive], na.rm = TRUE) else NA_real_,
      sum(x$passes_name_rule[negative], na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}
