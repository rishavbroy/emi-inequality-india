# Parallel district-lineage v2 diagnostic. The output is reviewable and does not
# replace the production crosswalk until all source identities and events are
# adjudicated and the migration-readiness invariants pass.

ensure_columns_v2 <- function(x, columns, value = NA) {
  x <- safe_df(x)
  for (nm in setdiff(columns, names(x))) x[[nm]] <- rep(value, nrow(x))
  x
}

build_isded_candidate_events_v2 <- function(raw_sources) {
  x <- clean_source_names(safe_df(raw_sources$isded_1951_2024 %||% data.frame()))
  source_district <- first_col(x, c("source district", "source_district"))
  dest_district <- first_col(x, c("dest district", "destination district", "dest_district"))
  source_year <- first_col(x, c("source year", "source_year"))
  dest_year <- first_col(x, c("dest year", "destination year", "dest_year"))
  state <- first_col(x, c("filter state", "state", "filter_state"))
  if (any(vapply(list(source_district, dest_district, source_year, dest_year, state), is.null, logical(1)))) {
    return(data.frame())
  }
  x <- unique(data.frame(
    source_district = plain_chr(x[[source_district]]),
    dest_district = plain_chr(x[[dest_district]]),
    source_year = num(x[[source_year]]),
    dest_year = num(x[[dest_year]]),
    filter_state = plain_chr(x[[state]]),
    stringsAsFactors = FALSE
  ))
  source_key <- paste(canonicalize_state_name(x$filter_state), canon(x$source_district), sep = "__")
  target_key <- paste(canonicalize_state_name(x$filter_state), canon(x$dest_district), sep = "__")
  changed <- !is.na(source_key) & nzchar(source_key) & !is.na(target_key) & nzchar(target_key) & source_key != target_key
  x <- x[changed & is.finite(x$dest_year) & x$dest_year >= 2001L & x$dest_year <= 2018L, , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  data.frame(
    event_id = paste0("candidate__isded_1951_2024__", seq_len(nrow(x))),
    effective_date = NA_character_,
    reported_year = NA_integer_,
    source_year = as.integer(x$source_year),
    target_year = as.integer(x$dest_year),
    date_precision = "census_anchor_interval",
    event_type = "census_anchor_lineage_candidate",
    from_state = x$filter_state,
    from_district = x$source_district,
    to_state = x$filter_state,
    to_district = x$dest_district,
    source_id = "isded_1951_2024",
    status = "candidate_unadjudicated",
    note = paste0(
      "ISDED links Census anchors ", x$source_year, "-", x$dest_year,
      "; it does not by itself establish an effective event date or territorial share."
    ),
    stringsAsFactors = FALSE
  )
}

build_candidate_admin_events_v2 <- function(district_tracker, raw_sources) {
  required <- c(
    "source_file_id", ".row_in_source", "source_state_raw", "source_district_raw",
    "target_state_raw", "target_district_raw", "source_year", "target_year",
    "event_year", "change_type"
  )
  tracker <- ensure_columns_v2(district_tracker, required)
  if (nrow(tracker)) {
    changed <- is.na(tracker$change_type) | tracker$change_type != "continuity"
    event_year <- num(tracker$event_year)
    target_year <- num(tracker$target_year)
    possible_period <-
      (is.finite(event_year) & event_year >= 2001L & event_year <= 2018L) |
      (!is.finite(event_year) & (!is.finite(target_year) | (target_year >= 2001L & target_year <= 2018L)))
    tracker <- tracker[changed & possible_period, , drop = FALSE]
  }

  tracker_events <- if (nrow(tracker)) {
    event_year <- suppressWarnings(as.integer(tracker$event_year))
    data.frame(
      event_id = paste0("candidate__", tracker$source_file_id, "__", tracker$.row_in_source),
      effective_date = NA_character_,
      reported_year = ifelse(is.finite(event_year), event_year, NA_integer_),
      source_year = suppressWarnings(as.integer(tracker$source_year)),
      target_year = suppressWarnings(as.integer(tracker$target_year)),
      date_precision = ifelse(is.finite(event_year), "year_only", "source_target_interval"),
      event_type = ifelse(is.na(tracker$change_type) | !nzchar(tracker$change_type), "unknown_tracker_relation", tracker$change_type),
      from_state = plain_chr(tracker$source_state_raw),
      from_district = plain_chr(tracker$source_district_raw),
      to_state = plain_chr(tracker$target_state_raw),
      to_district = plain_chr(tracker$target_district_raw),
      source_id = plain_chr(tracker$source_file_id),
      status = "candidate_unadjudicated",
      note = "Tracker relation is candidate evidence only; verify the date and territorial content before acceptance.",
      stringsAsFactors = FALSE
    )
  } else data.frame()

  district_mod <- safe_df(raw_sources$lgd_mod_districts %||% data.frame())
  lgd_events <- if (nrow(district_mod)) {
    roster <- district_mod
    data.frame(
      event_id = paste0("lgd_changed__district__", roster$entity_code),
      effective_date = NA_character_,
      reported_year = NA_integer_,
      source_year = 2011L,
      target_year = 2018L,
      date_precision = "download_interval_only",
      event_type = "unknown_modification",
      from_state = roster$state_name,
      from_district = NA_character_,
      to_state = roster$state_name,
      to_district = roster$entity_name,
      source_id = "lgd_mod_districts",
      status = "changed_unit_roster_only",
      note = "LGD identifies a district modified during 2011-01-01 to 2018-06-30 but this export does not encode its action, predecessor, or effective date.",
      stringsAsFactors = FALSE
    )
  } else data.frame()

  isded_events <- build_isded_candidate_events_v2(raw_sources)
  unique(safe_bind_rows(list(tracker_events, lgd_events, isded_events)))
}

build_current_component_registry_v2 <- function(raw_sources) {
  subdistricts <- standardize_lgd_registry(
    raw_sources$lgd_subdistricts %||% data.frame(),
    "subdistrict"
  )
  subdistricts <- data.frame(
    level = subdistricts$level,
    state_lgd_code = subdistricts$state_lgd_code,
    state_name = subdistricts$state_name,
    district_lgd_code = subdistricts$district_lgd_code,
    district_name = subdistricts$district_name,
    entity_code = subdistricts$subdistrict_lgd_code,
    entity_name = subdistricts$subdistrict_name,
    census2011_code = subdistricts$census2011_subdistrict_code,
    source_id = rep("lgd_subdistricts", nrow(subdistricts)),
    stringsAsFactors = FALSE
  )
  ulbs <- standardize_lgd_urban_local_bodies(
    raw_sources$lgd_urban_local_bodies %||% data.frame()
  )
  ulbs$district_lgd_code <- rep(NA_character_, nrow(ulbs))
  ulbs$district_name <- rep(NA_character_, nrow(ulbs))
  ulbs$source_id <- rep("lgd_urban_local_bodies", nrow(ulbs))
  ulbs <- ulbs[c(
    "level", "state_lgd_code", "state_name", "district_lgd_code",
    "district_name", "entity_code", "entity_name", "census2011_code", "source_id"
  )]
  iss <- standardize_iss_admin_units_2025(
    raw_sources$isded_admin_units_2025 %||% data.frame()
  )
  unique(safe_bind_rows(list(subdistricts, ulbs, iss)))
}

build_current_urban_coverage_v2 <- function(raw_sources) {
  coverage <- standardize_lgd_urban_coverage(
    raw_sources$lgd_urban_coverage %||% data.frame()
  )
  if (nrow(coverage)) coverage$source_id <- "lgd_urban_coverage"
  coverage
}

build_changed_component_roster_v2 <- function(raw_sources) {
  source_ids <- c(
    "lgd_mod_districts", "lgd_mod_subdistricts",
    "lgd_mod_villages", "lgd_mod_urban_local_bodies"
  )
  safe_bind_rows(lapply(source_ids, function(source_id) {
    out <- safe_df(raw_sources[[source_id]] %||% data.frame())
    if (!nrow(out)) return(data.frame())
    out$source_id <- source_id
    out
  }))
}

read_admin_events_v2 <- function(x) {
  x <- safe_df(x)
  required <- c("event_id", "effective_date", "event_type", "from_unit", "to_unit", "share", "source_id", "status", "note")
  for (nm in setdiff(required, names(x))) x[[nm]] <- rep(NA_character_, nrow(x))
  x <- x[!is.na(x$event_id) & nzchar(x$event_id), required, drop = FALSE]
  if (anyDuplicated(x[c("event_id", "from_unit", "to_unit")])) {
    stop("Adjudicated district events contain duplicate event edges.", call. = FALSE)
  }
  x$share <- num(x$share)
  invalid_share <- is.finite(x$share) & (x$share < 0 | x$share > 1)
  if (any(invalid_share)) stop("Adjudicated district-event shares must lie between zero and one.", call. = FALSE)
  allowed <- c("accepted", "rejected", "needs_review")
  invalid_status <- unique(x$status[!is.na(x$status) & nzchar(x$status) & !x$status %in% allowed])
  if (length(invalid_status)) stop("Unknown district-event status: ", paste(invalid_status, collapse = ", "), call. = FALSE)
  x
}

read_lineage_source_registry_v2 <- function(x) {
  x <- safe_df(x)
  required <- c("source_id", "citation", "path_or_url", "accessed")
  for (nm in setdiff(required, names(x))) x[[nm]] <- rep(NA_character_, nrow(x))
  x <- x[!is.na(x$source_id) & nzchar(x$source_id), required, drop = FALSE]
  if (anyDuplicated(x$source_id)) {
    stop("District-lineage source registry must contain unique source_id values.", call. = FALSE)
  }
  x
}

validate_lineage_source_references_v2 <- function(
  source_registry, source_matches = data.frame(), admin_events = data.frame(),
  allocation_weights = data.frame()
) {
  registry_ids <- unique(plain_chr(safe_df(source_registry)$source_id %||% character()))
  collect <- function(x, object_type, id_col) {
    x <- safe_df(x)
    if (!nrow(x) || !all(c(id_col, "source_id", "status") %in% names(x))) return(data.frame())
    x <- x[x$status %in% "accepted", , drop = FALSE]
    if (!nrow(x)) return(data.frame())
    data.frame(
      object_type = object_type,
      object_id = plain_chr(x[[id_col]]),
      source_id = plain_chr(x$source_id),
      stringsAsFactors = FALSE
    )
  }
  refs <- safe_bind_rows(list(
    collect(source_matches, "source_match", "source_row_id"),
    collect(admin_events, "admin_event", "event_id"),
    collect(allocation_weights, "allocation_weight", "source_unit")
  ))
  if (!nrow(refs)) {
    return(data.frame(
      object_type = character(), object_id = character(), source_id = character(),
      issue = character(), stringsAsFactors = FALSE
    ))
  }
  refs$issue <- ifelse(
    is.na(refs$source_id) | !nzchar(refs$source_id),
    "missing_source_id",
    ifelse(refs$source_id %in% registry_ids, NA_character_, "unregistered_source_id")
  )
  refs[!is.na(refs$issue), , drop = FALSE]
}

build_concordance_candidates_v2 <- function(raw_sources) {
  ids <- c(
    "concordance_plfs_nss", "concordance_census_plfs", "concordance_nrlm_plfs",
    "concordance_telangana", "concordance_census_region"
  )
  safe_bind_rows(lapply(ids, function(id) {
    x <- safe_df(raw_sources[[id]] %||% data.frame())
    if (!nrow(x)) return(data.frame())
    data.frame(
      source_id = id,
      row_in_source = seq_len(nrow(x)),
      raw_record = apply(x, 1, function(z) paste(paste(names(x), plain_chr(z), sep = "="), collapse = "; ")),
      evidence_role = "published_candidate_or_corroboration",
      stringsAsFactors = FALSE
    )
  }))
}

duplicate_key_diagnostics_v2 <- function(x, key_cols, source_id) {
  x <- safe_df(x)
  missing <- setdiff(key_cols, names(x))
  if (length(missing) || !nrow(x)) return(data.frame())
  key <- do.call(interaction, c(lapply(x[key_cols], plain_chr), list(drop = TRUE, lex.order = TRUE)))
  groups <- split(seq_len(nrow(x)), key)
  duplicates <- groups[lengths(groups) > 1L]
  if (!length(duplicates)) return(data.frame())
  non_key <- setdiff(names(x), key_cols)
  safe_bind_rows(lapply(duplicates, function(i) {
    payload <- if (length(non_key)) apply(x[i, non_key, drop = FALSE], 1, function(z) paste(plain_chr(z), collapse = "\r")) else rep("", length(i))
    data.frame(
      source_id = source_id,
      key = as.character(key[[i[[1]]]]),
      n_rows = length(i),
      duplicate_type = if (length(unique(payload)) == 1L) "identical" else "conflicting",
      row_numbers = paste(i, collapse = ";"),
      stringsAsFactors = FALSE
    )
  }))
}

candidate_event_relevant_to_sources_v2 <- function(events, source_roster) {
  events <- safe_df(events)
  sources <- safe_df(source_roster)
  if (!nrow(events) || !nrow(sources)) return(rep(FALSE, nrow(events)))
  source_key <- paste(sources$state_std, sources$district_std, sep = "__")
  from_key <- paste(canonicalize_state_name(events$from_state), canonicalize_district_name(events$from_district), sep = "__")
  to_key <- paste(canonicalize_state_name(events$to_state), canonicalize_district_name(events$to_district), sep = "__")
  from_key %in% source_key | to_key %in% source_key
}

build_evidence_requests_v2 <- function(candidate_events, source_roster, primary_eligibility) {
  events <- safe_df(candidate_events)
  source_roster <- safe_df(source_roster)
  eligibility <- safe_df(primary_eligibility)
  relevant <- candidate_event_relevant_to_sources_v2(events, source_roster)
  event_requests <- events[
    relevant & events$status %in% c("candidate_unadjudicated", "changed_unit_roster_only"),
    , drop = FALSE
  ]
  event_requests <- ensure_columns_v2(
    event_requests, c("reported_year", "source_year", "target_year", "effective_date"), NA
  )
  event_out <- if (nrow(event_requests)) data.frame(
    request_id = paste0("event__", event_requests$event_id),
    state = event_requests$to_state,
    affected_units = paste(event_requests$from_district, event_requests$to_district, sep = " -> "),
    period = ifelse(
      is.finite(num(event_requests$reported_year)),
      plain_chr(event_requests$reported_year),
      ifelse(
        is.finite(num(event_requests$source_year)) & is.finite(num(event_requests$target_year)),
        paste(event_requests$source_year, event_requests$target_year, sep = "-"),
        ifelse(is.na(event_requests$effective_date), "2001-2018", substr(event_requests$effective_date, 1, 4))
      )
    ),
    unresolved_question = "Confirm event type, effective date, territorial components, and validity for the NSS source period.",
    sources_checked = event_requests$source_id,
    requested_document = "Official Gazette or Revenue Department order; targeted Administrative Atlas annex only for unresolved 2001-2008 events.",
    stringsAsFactors = FALSE
  ) else data.frame()

  unresolved <- eligibility[!(eligibility$eligible_primary %in% TRUE), , drop = FALSE]
  source_out <- if (nrow(unresolved)) data.frame(
    request_id = paste0("source__", unresolved$source_row_id),
    state = unresolved$state_std,
    affected_units = unresolved$district_std,
    period = unresolved$wave,
    unresolved_question = unresolved$exclusion_reason,
    sources_checked = ifelse(is.na(unresolved$method), "exact and fuzzy candidate ledgers", unresolved$method),
    requested_document = "Resolve from code, official alias, or dated lineage evidence; do not accept a fuzzy name alone.",
    stringsAsFactors = FALSE
  ) else data.frame()
  unique(safe_bind_rows(list(event_out, source_out)))
}

build_migration_readiness_v2 <- function(
  missing_core, admin_2001, admin_2011, allocation_validation,
  source_roster, source_matches, primary_eligibility, duplicate_keys,
  adjudicated_allocation_validation, source_reference_issues
) {
  source_matches <- safe_df(source_matches)
  accepted_or_excluded <- source_matches$status %in% c("accepted", "excluded")
  resolved_ids <- source_matches$source_row_id[accepted_or_excluded]
  duplicates <- safe_df(duplicate_keys)
  data.frame(
    gate = c(
      "core_inputs_available", "unique_2001_unit_ids", "unique_2011_unit_ids",
      "allocation_weights_valid", "adjudicated_allocation_weights_valid",
      "all_adjudication_sources_registered", "no_conflicting_duplicate_keys",
      "all_source_rows_adjudicated",
      "all_accepted_rows_primary_eligible",
      "production_crosswalk_migration_ready"
    ),
    passed = c(
      !length(missing_core),
      nrow(admin_2001) > 0L && !anyDuplicated(admin_2001$unit_id),
      nrow(admin_2011) > 0L && !anyDuplicated(admin_2011$unit_id),
      nrow(allocation_validation) > 0L && all(allocation_validation$within_tolerance),
      !nrow(adjudicated_allocation_validation) || all(adjudicated_allocation_validation$within_tolerance),
      !nrow(source_reference_issues),
      !nrow(duplicates) || !any(duplicates$duplicate_type == "conflicting"),
      nrow(source_roster) > 0L && all(source_roster$source_row_id %in% resolved_ids),
      any(source_matches$status %in% "accepted") &&
        all(primary_eligibility$eligible_primary[primary_eligibility$status == "accepted"]),
      FALSE
    ),
    note = c(
      "All locality keys and the PC11 district geometry are present.",
      "Census 2001 unit IDs are code-based and unique.",
      "Census 2011 unit IDs are code-based and unique.",
      "Every SHRUG source-district allocation sums to one with no missing or negative weights.",
      "Every accepted tracked sensitivity allocation sums to one by source unit.",
      "Every accepted source match, event, and allocation cites a registered evidence source.",
      "Duplicate source or registry keys are either absent or identical.",
      "Every NSS source row is explicitly accepted or excluded in tracked metadata.",
      "Every accepted source row maps deterministically to a 2001 district.",
      "Remains false until the reviewed v2 ledgers replace the inherited production crosswalk."
    ),
    stringsAsFactors = FALSE
  )
}

summarize_shrid_bridge_v2 <- function(bridge) {
  bridge <- safe_df(bridge)
  if (!nrow(bridge)) {
    return(data.frame(
      bridge_status = character(), n_shrid = integer(), population = numeric(),
      area = numeric(), stringsAsFactors = FALSE
    ))
  }
  status <- plain_chr(bridge$bridge_status)
  groups <- split(seq_len(nrow(bridge)), status)
  safe_bind_rows(lapply(groups, function(i) {
    data.frame(
      bridge_status = status[[i[[1]]]],
      n_shrid = length(unique(bridge$shrid2[i])),
      population = sum_finite_or_na(bridge$population[i]),
      area = sum_finite_or_na(bridge$area[i]),
      stringsAsFactors = FALSE
    )
  }))
}

lineage_v2_summary <- function(
  inventory, admin_2001, admin_2011, bridge, transition, source_roster,
  source_matches, candidates, eligibility, events, current_components,
  urban_coverage, changed_components, evidence_requests
) {
  accepted <- source_matches$status %in% "accepted"
  data.frame(
    metric = c(
      "available_inputs", "missing_inputs", "admin_units_2001", "admin_units_2011",
      "shrid_bridge_rows", "deterministic_shrid_rows", "district_transition_rows",
      "nss_source_rows", "accepted_source_matches", "unadjudicated_source_rows",
      "candidate_rows", "primary_eligible_source_rows", "candidate_event_rows",
      "current_component_rows", "urban_coverage_rows", "changed_component_rows",
      "targeted_evidence_requests"
    ),
    value = c(
      sum(inventory$exists), sum(!inventory$exists), nrow(admin_2001), nrow(admin_2011),
      nrow(bridge), sum(bridge$deterministic %in% TRUE), nrow(transition),
      nrow(source_roster), sum(accepted),
      sum(!source_roster$source_row_id %in% source_matches$source_row_id[source_matches$status %in% c("accepted", "excluded")]),
      nrow(candidates), sum(eligibility$eligible_primary %in% TRUE), nrow(events),
      nrow(current_components), nrow(urban_coverage), nrow(changed_components),
      nrow(evidence_requests)
    ),
    stringsAsFactors = FALSE
  )
}

#' Build district-lineage v2 diagnostic bundle
build_district_lineage_v2 <- function(
  raw_sources, inventory, district_tracker, census_2001_languages,
  source_2007, source_2017
) {
  inventory <- safe_df(inventory)
  needed <- c(
    "shrug_pc01r", "shrug_pc01u", "shrug_pc11r", "shrug_pc11u",
    "shrug_pc01dist", "shrug_pc11dist", "shrug_pc11_district_geometry"
  )
  missing_core <- needed[!needed %in% names(raw_sources)]

  admin_2001 <- build_admin_registry_2001(
    census_2001_languages,
    raw_sources$lgd_states %||% data.frame()
  )
  admin_2011 <- if ("shrug_pc11_district_geometry" %in% names(raw_sources)) {
    build_admin_registry_2011(raw_sources$shrug_pc11_district_geometry)
  } else data.frame()
  bridge <- if (!length(missing_core)) {
    build_shrug_district_bridge(
      raw_sources$shrug_pc01r, raw_sources$shrug_pc01u,
      raw_sources$shrug_pc11r, raw_sources$shrug_pc11u,
      raw_sources$shrug_pc01dist, raw_sources$shrug_pc11dist
    )
  } else data.frame()
  transition <- build_district_transition_2001_2011(bridge)
  bridge_summary <- summarize_shrid_bridge_v2(bridge)
  bridge_df <- safe_df(bridge)
  bridge_qa <- bridge_df[!(bridge_df$deterministic %in% TRUE), , drop = FALSE]
  allocation_validation <- validate_allocation_weights(transition)

  source_roster <- build_nss_district_roster_v2(source_2007, source_2017)
  reference_units <- build_reference_units_v2(
    admin_2001, admin_2011,
    raw_sources$lgd_states %||% data.frame(),
    raw_sources$lgd_districts %||% data.frame()
  )
  adjudications <- raw_sources$lineage_adjudications %||% data.frame()
  adjudicated_events <- read_admin_events_v2(raw_sources$lineage_events %||% data.frame())
  adjudicated_weights <- read_adjudicated_allocation_weights_v2(
    raw_sources$lineage_allocation_weights %||% data.frame(),
    admin_2001
  )
  adjudicated_weight_validation <- validate_adjudicated_allocation_weights_v2(adjudicated_weights)
  source_matches <- build_adjudicated_source_matches_v2(adjudications, reference_units)
  candidates <- build_source_candidate_ledger_v2(source_roster, reference_units, adjudications)
  eligibility <- build_primary_mapping_eligibility(
    source_roster, source_matches, transition, admin_2001, admin_2011, adjudicated_events
  )
  primary_crosswalk <- build_primary_source_crosswalk_v2(eligibility)
  excluded_sources <- build_excluded_source_rows_v2(eligibility)

  candidate_events <- build_candidate_admin_events_v2(district_tracker, raw_sources)
  current_components <- build_current_component_registry_v2(raw_sources)
  urban_coverage <- build_current_urban_coverage_v2(raw_sources)
  changed_components <- build_changed_component_roster_v2(raw_sources)
  source_registry <- read_lineage_source_registry_v2(raw_sources$lineage_sources %||% data.frame())
  source_reference_issues <- validate_lineage_source_references_v2(
    source_registry, source_matches, adjudicated_events, adjudicated_weights
  )
  concordance <- build_concordance_candidates_v2(raw_sources)
  evidence_requests <- build_evidence_requests_v2(candidate_events, source_roster, eligibility)
  gold <- score_gold_set_v2(raw_sources$lineage_gold %||% data.frame())
  gold_summary <- summarize_gold_set_v2(gold)

  duplicate_keys <- safe_bind_rows(list(
    duplicate_key_diagnostics_v2(source_roster, "source_key", "nss_source_roster"),
    duplicate_key_diagnostics_v2(reference_units, "unit_id", "reference_units"),
    duplicate_key_diagnostics_v2(candidate_events, "event_id", "candidate_admin_events")
  ))
  readiness <- build_migration_readiness_v2(
    missing_core, admin_2001, admin_2011, allocation_validation,
    source_roster, source_matches, eligibility, duplicate_keys,
    adjudicated_weight_validation, source_reference_issues
  )
  summary <- lineage_v2_summary(
    inventory, admin_2001, admin_2011, bridge, transition, source_roster,
    source_matches, candidates, eligibility, candidate_events, current_components,
    urban_coverage, changed_components, evidence_requests
  )

  list(
    summary = summary,
    migration_readiness = readiness,
    source_inventory = inventory,
    source_registry = source_registry,
    source_reference_issues = source_reference_issues,
    admin_units_2001 = admin_2001,
    admin_units_2011 = admin_2011,
    shrid_bridge_summary = bridge_summary,
    shrid_bridge_qa = bridge_qa,
    district_transition_2001_2011 = transition,
    allocation_weight_validation = allocation_validation,
    nss_source_roster = source_roster,
    reference_units = reference_units,
    source_matches = source_matches,
    source_match_candidates = candidates,
    primary_mapping_eligibility = eligibility,
    primary_source_crosswalk = primary_crosswalk,
    excluded_source_rows = excluded_sources,
    candidate_admin_events = candidate_events,
    current_component_registry = current_components,
    current_urban_coverage = urban_coverage,
    changed_component_roster = changed_components,
    adjudicated_admin_events = adjudicated_events,
    adjudicated_allocation_weights = adjudicated_weights,
    adjudicated_allocation_validation = adjudicated_weight_validation,
    published_concordance_records = concordance,
    evidence_requests = evidence_requests,
    duplicate_keys = duplicate_keys,
    match_gold_scored = gold,
    match_gold_summary = gold_summary,
    missing_core_inputs = data.frame(source_id = missing_core, stringsAsFactors = FALSE)
  )
}

save_district_lineage_v2 <- function(diagnostics, dir = "outputs/diagnostics/extended/district_lineage_v2") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  names_to_write <- c(
    "summary", "migration_readiness", "source_inventory", "source_registry", "source_reference_issues",
    "admin_units_2001", "admin_units_2011",
    "shrid_bridge_summary", "shrid_bridge_qa",
    "district_transition_2001_2011", "allocation_weight_validation",
    "nss_source_roster", "reference_units", "source_matches", "source_match_candidates",
    "primary_mapping_eligibility", "primary_source_crosswalk", "excluded_source_rows",
    "candidate_admin_events", "current_component_registry",
    "current_urban_coverage", "changed_component_roster",
    "adjudicated_admin_events", "adjudicated_allocation_weights",
    "adjudicated_allocation_validation", "published_concordance_records", "evidence_requests",
    "duplicate_keys", "match_gold_scored", "match_gold_summary", "missing_core_inputs"
  )
  paths <- stats::setNames(vapply(names_to_write, function(nm) {
    value <- diagnostics[[nm]]
    if (is.null(value)) value <- data.frame()
    write_diagnostic_csv(value, file.path(dir, paste0(nm, ".csv")))
  }, character(1)), names_to_write)
  output_manifest(paths)
}
