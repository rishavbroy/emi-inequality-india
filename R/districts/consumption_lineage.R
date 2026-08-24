# Conservative handoff from named consumption source districts to Census-2001
# lineage. Survey district codes remain source identifiers only; accepted
# mappings come from exact Census-2001 identities or stable reviewed lineage.

empty_consumption_lineage_bridge <- function() {
  data.frame(
    survey_id = character(), source_state_code = character(),
    source_district_code = character(), state_std = character(),
    district_std = character(), source_unit_kind = character(),
    source_lineage_eligible = logical(), target_unit_2001 = character(),
    lineage_weight = numeric(), lineage_basis = character(),
    lineage_status = character(), stringsAsFactors = FALSE
  )
}

consumption_source_district_roster <- function(households) {
  x <- safe_df(households)
  required <- c(
    "survey_id", "source_state_code", "source_district_code",
    "state_std", "district_std", "source_unit_kind", "source_lineage_eligible"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Canonical consumption households lack source-lineage fields: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  out <- unique(x[required])
  out$survey_id <- plain_chr(out$survey_id)
  out$source_state_code <- plain_chr(out$source_state_code)
  out$source_district_code <- plain_chr(out$source_district_code)
  out$state_std <- canonicalize_state_name(out$state_std)
  out$district_std <- canonicalize_district_name(out$district_std)
  if (anyDuplicated(out[c("survey_id", "source_state_code", "source_district_code")])) {
    stop("Consumption source codes identify multiple district identities.", call. = FALSE)
  }
  out
}

lineage_distribution_signature <- function(target, weight) {
  keep <- !is.na(target) & nzchar(target) & is.finite(num(weight))
  target <- plain_chr(target[keep])
  weight <- num(weight[keep])
  if (!length(target)) return(NA_character_)
  ord <- order(target)
  paste(paste(target[ord], sprintf("%.12f", weight[ord]), sep = "="), collapse = ";")
}

reviewed_district_identity_lineage <- function(nss_source_roster, full_reviewed_source_crosswalk) {
  roster <- safe_df(nss_source_roster)
  crosswalk <- safe_df(full_reviewed_source_crosswalk)
  required_roster <- c("source_row_id", "wave", "state_std", "district_std")
  required_crosswalk <- c("source_row_id", "target_unit_2001", "weight", "basis", "panel_variant")
  if (!all(required_roster %in% names(roster)) || !all(required_crosswalk %in% names(crosswalk))) {
    stop("Reviewed lineage inputs lack district-identity fields.", call. = FALSE)
  }

  joined <- merge(
    roster[required_roster], crosswalk[required_crosswalk],
    by = "source_row_id", all = FALSE, sort = FALSE
  )
  if (!nrow(joined)) {
    return(list(mapping = data.frame(), conflicts = data.frame()))
  }
  joined$weight <- num(joined$weight)
  joined$state_std <- canonicalize_state_name(joined$state_std)
  joined$district_std <- canonicalize_district_name(joined$district_std)

  source_groups <- split(seq_len(nrow(joined)), joined$source_row_id)
  source_summary <- safe_bind_rows(lapply(source_groups, function(i) {
    part <- joined[i, , drop = FALSE]
    data.frame(
      source_row_id = part$source_row_id[[1L]],
      state_std = part$state_std[[1L]],
      district_std = part$district_std[[1L]],
      wave = part$wave[[1L]],
      signature = lineage_distribution_signature(part$target_unit_2001, part$weight),
      weight_sum = sum(part$weight),
      stringsAsFactors = FALSE
    )
  }))
  source_summary <- source_summary[
    is.finite(source_summary$weight_sum) & abs(source_summary$weight_sum - 1) < 1e-8 &
      !is.na(source_summary$signature) & nzchar(source_summary$signature),
    , drop = FALSE
  ]

  identity_key <- paste(source_summary$state_std, source_summary$district_std, sep = "\r")
  identity_groups <- split(seq_len(nrow(source_summary)), identity_key)
  stable <- safe_bind_rows(lapply(identity_groups, function(i) {
    part <- source_summary[i, , drop = FALSE]
    signatures <- unique(part$signature)
    if (length(unique(part$wave)) < 2L || length(signatures) != 1L) return(data.frame())
    data.frame(
      state_std = part$state_std[[1L]], district_std = part$district_std[[1L]],
      signature = signatures[[1L]], reviewed_wave_count = length(unique(part$wave)),
      source_row_id = sort(part$source_row_id)[[1L]],
      stringsAsFactors = FALSE
    )
  }))
  conflicts <- safe_bind_rows(lapply(identity_groups, function(i) {
    part <- source_summary[i, , drop = FALSE]
    signatures <- unique(part$signature)
    if (length(unique(part$wave)) < 2L || length(signatures) <= 1L) return(data.frame())
    data.frame(
      state_std = part$state_std[[1L]], district_std = part$district_std[[1L]],
      reviewed_wave_count = length(unique(part$wave)),
      reviewed_signature_count = length(signatures), stringsAsFactors = FALSE
    )
  }))
  if (!nrow(stable)) return(list(mapping = data.frame(), conflicts = conflicts))

  mapping <- merge(
    joined, stable[c("source_row_id", "signature")],
    by = "source_row_id", all = FALSE, sort = FALSE
  )
  mapping <- unique(data.frame(
    state_std = mapping$state_std,
    district_std = mapping$district_std,
    target_unit_2001 = mapping$target_unit_2001,
    lineage_weight = mapping$weight,
    lineage_basis = paste0("reviewed_crosswave_consensus:", mapping$basis),
    stringsAsFactors = FALSE
  ))
  list(mapping = mapping, conflicts = conflicts)
}

exact_census_2001_identity_lineage <- function(admin_units_2001) {
  admin <- safe_df(admin_units_2001)
  required <- c("unit_id", "state_std", "district_std")
  missing <- setdiff(required, names(admin))
  if (length(missing)) {
    stop("Census-2001 registry lacks district-identity fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- unique(data.frame(
    state_std = canonicalize_state_name(admin$state_std),
    district_std = canonicalize_district_name(admin$district_std),
    target_unit_2001 = plain_chr(admin$unit_id),
    lineage_weight = 1,
    lineage_basis = "exact_census_2001_identity",
    stringsAsFactors = FALSE
  ))
  if (anyDuplicated(out[c("state_std", "district_std")])) {
    stop("Census-2001 district names are not unique within normalized state identity.", call. = FALSE)
  }
  out
}

build_consumption_lineage_reference <- function(
    admin_units_2001, nss_source_roster, full_reviewed_source_crosswalk) {
  list(
    exact = exact_census_2001_identity_lineage(admin_units_2001),
    reviewed = reviewed_district_identity_lineage(
      nss_source_roster, full_reviewed_source_crosswalk
    )
  )
}

build_consumption_lineage_bridge <- function(households, lineage_reference) {
  source <- consumption_source_district_roster(households)
  if (!nrow(source)) return(empty_consumption_lineage_bridge())
  if (!is.list(lineage_reference) || is.null(lineage_reference$exact) ||
      is.null(lineage_reference$reviewed)) {
    stop("Consumption lineage reference is incomplete.", call. = FALSE)
  }
  exact <- safe_df(lineage_reference$exact)
  reviewed <- lineage_reference$reviewed

  source$row_id <- seq_len(nrow(source))
  exact_hit <- merge(
    source, exact, by = c("state_std", "district_std"), all.x = TRUE, sort = FALSE
  )
  exact_hit <- exact_hit[order(exact_hit$row_id), , drop = FALSE]
  exact_rows <- exact_hit[
    exact_hit$source_lineage_eligible %in% TRUE &
      !is.na(exact_hit$target_unit_2001) & nzchar(exact_hit$target_unit_2001),
    , drop = FALSE
  ]
  exact_ids <- unique(exact_rows$row_id)

  remaining <- source[
    source$source_lineage_eligible %in% TRUE & !source$row_id %in% exact_ids,
    , drop = FALSE
  ]
  reviewed_rows <- data.frame()
  if (nrow(remaining) && nrow(reviewed$mapping)) {
    reviewed_rows <- merge(
      remaining, reviewed$mapping,
      by = c("state_std", "district_std"), all = FALSE, sort = FALSE
    )
  }
  reviewed_ids <- unique(reviewed_rows$row_id)

  conflict_key <- if (nrow(reviewed$conflicts)) {
    paste(reviewed$conflicts$state_std, reviewed$conflicts$district_std, sep = "\r")
  } else character()
  unresolved <- remaining[!remaining$row_id %in% reviewed_ids, , drop = FALSE]
  unresolved_key <- paste(unresolved$state_std, unresolved$district_std, sep = "\r")
  unresolved$target_unit_2001 <- rep(NA_character_, nrow(unresolved))
  unresolved$lineage_weight <- rep(NA_real_, nrow(unresolved))
  unresolved$lineage_basis <- rep(NA_character_, nrow(unresolved))
  unresolved$lineage_status <- ifelse(
    unresolved_key %in% conflict_key,
    "reviewed_lineage_conflict",
    "unresolved_no_stable_lineage"
  )

  noneligible <- source[!(source$source_lineage_eligible %in% TRUE), , drop = FALSE]
  noneligible$target_unit_2001 <- rep(NA_character_, nrow(noneligible))
  noneligible$lineage_weight <- rep(NA_real_, nrow(noneligible))
  noneligible$lineage_basis <- rep(NA_character_, nrow(noneligible))
  noneligible$lineage_status <- rep("source_not_lineage_eligible", nrow(noneligible))

  if (nrow(exact_rows)) exact_rows$lineage_status <- "resolved_exact_2001"
  if (nrow(reviewed_rows)) reviewed_rows$lineage_status <- "resolved_reviewed_consensus"
  out <- safe_bind_rows(list(exact_rows, reviewed_rows, unresolved, noneligible))
  wanted <- names(empty_consumption_lineage_bridge())
  for (nm in setdiff(wanted, names(out))) out[[nm]] <- rep(NA, nrow(out))
  out <- out[wanted]

  resolved <- grepl("^resolved_", out$lineage_status)
  if (any(resolved)) {
    key <- paste(out$survey_id, out$source_state_code, out$source_district_code, sep = "\r")
    sums <- tapply(out$lineage_weight[resolved], key[resolved], sum)
    if (any(!is.finite(sums) | abs(sums - 1) > 1e-8)) {
      stop("Resolved consumption lineage weights must sum to one within source district.", call. = FALSE)
    }
  }
  out
}

attach_consumption_lineage <- function(households, bridge) {
  x <- safe_df(households)
  bridge <- safe_df(bridge)
  keys <- c("survey_id", "source_state_code", "source_district_code")
  missing_x <- setdiff(c(keys, "survey_weight", "household_size"), names(x))
  missing_b <- setdiff(c(keys, "target_unit_2001", "lineage_weight", "lineage_basis", "lineage_status"), names(bridge))
  if (length(missing_x) || length(missing_b)) {
    stop("Consumption lineage attachment inputs do not satisfy the canonical contract.", call. = FALSE)
  }
  x$.consumption_row_order <- seq_len(nrow(x))
  out <- merge(
    x, bridge[c(keys, "target_unit_2001", "lineage_weight", "lineage_basis", "lineage_status")],
    by = keys, all.x = TRUE, sort = FALSE
  )
  if (anyNA(out$lineage_status)) {
    stop("Consumption lineage bridge does not cover every source household geography.", call. = FALSE)
  }
  resolved <- grepl("^resolved_", out$lineage_status)
  out$lineage_survey_weight <- ifelse(
    resolved, num(out$survey_weight) * num(out$lineage_weight), NA_real_
  )
  out$lineage_person_weight <- ifelse(
    resolved, out$lineage_survey_weight * num(out$household_size), NA_real_
  )
  out <- out[order(out$.consumption_row_order, out$target_unit_2001), , drop = FALSE]
  out$.consumption_row_order <- NULL
  rownames(out) <- NULL
  out
}

summarize_consumption_lineage_coverage <- function(lineaged_households) {
  x <- safe_df(lineaged_households)
  required <- c(
    "survey_id", "household_id", "source_state_code", "source_district_code",
    "source_lineage_eligible", "lineage_status", "survey_weight",
    "household_size", "lineage_person_weight"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Lineaged consumption households lack coverage fields.", call. = FALSE)
  x$person_weight <- num(x$survey_weight) * num(x$household_size)
  source_key <- paste(x$survey_id, x$source_state_code, x$source_district_code, sep = "\r")
  source_status <- unique(data.frame(source_key = source_key, lineage_status = x$lineage_status))
  if (anyDuplicated(source_status$source_key)) {
    # Resolved allocations duplicate a source district but must share one status.
    status_n <- tapply(source_status$lineage_status, source_status$source_key, function(z) length(unique(z)))
    if (any(status_n > 1L)) stop("A consumption source district has conflicting lineage statuses.", call. = FALSE)
    source_status <- source_status[!duplicated(source_status$source_key), , drop = FALSE]
  }
  resolved <- grepl("^resolved_", x$lineage_status)
  base_rows <- !duplicated(paste(x$household_id, source_key, sep = "\r"))
  total_person <- sum(x$person_weight[base_rows], na.rm = TRUE)
  eligible_person <- sum(
    x$person_weight[base_rows & x$source_lineage_eligible %in% TRUE], na.rm = TRUE
  )
  resolved_person <- sum(x$lineage_person_weight[resolved], na.rm = TRUE)
  data.frame(
    survey_id = unique(x$survey_id)[[1L]],
    source_districts = nrow(source_status),
    resolved_source_districts = sum(grepl("^resolved_", source_status$lineage_status)),
    unresolved_source_districts = sum(source_status$lineage_status == "unresolved_no_stable_lineage"),
    conflicting_source_districts = sum(source_status$lineage_status == "reviewed_lineage_conflict"),
    noneligible_source_units = sum(source_status$lineage_status == "source_not_lineage_eligible"),
    eligible_person_weight_coverage = if (eligible_person > 0) resolved_person / eligible_person else NA_real_,
    total_person_weight_coverage = if (total_person > 0) resolved_person / total_person else NA_real_,
    stringsAsFactors = FALSE
  )
}


build_consumption_lineage_review_queue <- function(bridge) {
  x <- safe_df(bridge)
  required <- c(
    "survey_id", "source_state_code", "source_district_code", "state_std",
    "district_std", "source_unit_kind", "lineage_status"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Consumption lineage bridge lacks review fields.", call. = FALSE)
  keep <- x$lineage_status %in% c(
    "unresolved_no_stable_lineage", "reviewed_lineage_conflict"
  )
  unique(x[keep, required, drop = FALSE])
}

save_consumption_lineage_coverage <- function(coverage, path = "outputs/diagnostics/public/consumption_lineage_coverage.csv") {
  write_diagnostic_csv(safe_df(coverage), path)
}

save_consumption_lineage_review_queue <- function(queue, path = "outputs/diagnostics/extended/consumption/lineage_review_queue.csv") {
  write_diagnostic_csv(safe_df(queue), path)
}
