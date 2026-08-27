# Conservative handoff from named consumption source districts to Census-2001
# lineage. Survey district codes remain source identifiers only; accepted
# mappings come from exact Census-2001 identities, reviewed deterministic
# administrative ancestry, or stable reviewed lineage.

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


read_consumption_lineage_identity_aliases <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("state_std", "source_district_std", "target_district_std", "basis")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Consumption lineage identity aliases are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$state_std <- canonicalize_state_name(x$state_std)
  x$source_district_std <- canonicalize_district_name(x$source_district_std)
  x$target_district_std <- canonicalize_district_name(x$target_district_std)
  x$basis <- trimws(plain_chr(x$basis))
  key <- paste(x$state_std, x$source_district_std, sep = "\r")
  if (anyDuplicated(key)) stop("Consumption lineage identity aliases must be unique by source identity.", call. = FALSE)
  if (any(!nzchar(x$state_std) | !nzchar(x$source_district_std) | !nzchar(x$target_district_std) | !nzchar(x$basis))) {
    stop("Consumption lineage identity aliases contain empty required values.", call. = FALSE)
  }
  x
}

consumption_identity_alias_lineage <- function(identity_aliases, exact_lineage) {
  aliases <- safe_df(identity_aliases)
  if (!nrow(aliases)) return(data.frame())
  required <- c("state_std", "source_district_std", "target_district_std", "basis")
  missing <- setdiff(required, names(aliases))
  if (length(missing)) stop("Consumption identity aliases lack required fields: ", paste(missing, collapse = ", "), call. = FALSE)
  exact <- safe_df(exact_lineage)
  targets <- unique(data.frame(
    state_std = exact$state_std,
    target_district_std = exact$district_std,
    target_unit_2001 = exact$target_unit_2001,
    stringsAsFactors = FALSE
  ))
  out <- merge(aliases, targets, by = c("state_std", "target_district_std"), all.x = TRUE, sort = FALSE)
  if (any(is.na(out$target_unit_2001) | !nzchar(out$target_unit_2001))) {
    bad <- unique(paste(out$state_std[is.na(out$target_unit_2001) | !nzchar(out$target_unit_2001)], out$target_district_std[is.na(out$target_unit_2001) | !nzchar(out$target_unit_2001)], sep = "/"))
    stop("Consumption lineage aliases reference unknown Census-2001 districts: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(out[c("state_std", "source_district_std")])) {
    stop("Consumption lineage aliases resolve ambiguously.", call. = FALSE)
  }
  data.frame(
    state_std = out$state_std,
    district_std = out$source_district_std,
    target_unit_2001 = out$target_unit_2001,
    lineage_weight = 1,
    lineage_basis = paste0("reviewed_identity_alias:", out$basis),
    stringsAsFactors = FALSE
  )
}

reviewed_consumption_source_code_lineage <- function(
    nss_source_roster, full_reviewed_source_crosswalk) {
  roster <- safe_df(nss_source_roster)
  crosswalk <- safe_df(full_reviewed_source_crosswalk)
  required_roster <- c("source_row_id", "wave", "source_code")
  required_crosswalk <- c(
    "source_row_id", "target_unit_2001", "weight", "basis", "panel_variant"
  )
  if (!all(required_roster %in% names(roster)) ||
      !all(required_crosswalk %in% names(crosswalk))) {
    stop("Reviewed source-code lineage inputs lack required fields.", call. = FALSE)
  }

  # NSS-64 Schedule 1.0 and Schedule 25.2 use the same round-level district
  # identification system. Reuse only source identities that the canonical
  # district-lineage system already reviewed as a deterministic, whole
  # Census-2001 code identity. A parseable code by itself is never sufficient.
  roster <- roster[
    roster$wave == "nss_2007_08" &
      grepl("^[0-9]{5}$", plain_chr(roster$source_code)),
    required_roster,
    drop = FALSE
  ]
  if (!nrow(roster)) return(data.frame())

  joined <- merge(
    roster, crosswalk[required_crosswalk],
    by = "source_row_id", all = FALSE, sort = FALSE
  )
  if (!nrow(joined)) return(data.frame())

  joined$weight <- num(joined$weight)
  parsed_target <- nss64_census2001_unit_id(joined$source_code)
  keep <- !is.na(parsed_target) &
    joined$target_unit_2001 == parsed_target &
    is.finite(joined$weight) &
    abs(joined$weight - 1) < 1e-8 &
    joined$panel_variant == "deterministic"
  joined <- joined[keep, , drop = FALSE]
  if (!nrow(joined)) return(data.frame())

  source_code <- plain_chr(joined$source_code)
  out <- unique(data.frame(
    survey_id = "nss_2007_08_consumption",
    source_state_code = substr(source_code, 1L, 2L),
    source_district_code = substr(source_code, 4L, 5L),
    target_unit_2001 = joined$target_unit_2001,
    lineage_weight = 1,
    lineage_basis = paste0("reviewed_same_round_source_code:", joined$basis),
    stringsAsFactors = FALSE
  ))

  key <- paste(out$survey_id, out$source_state_code, out$source_district_code, sep = "\r")
  if (anyDuplicated(key)) {
    groups <- split(out$target_unit_2001, key)
    conflicting <- names(groups)[vapply(
      groups, function(x) length(unique(x)) > 1L, logical(1)
    )]
    if (length(conflicting)) {
      stop(
        "Reviewed consumption source codes resolve to conflicting Census-2001 targets.",
        call. = FALSE
      )
    }
    out <- out[!duplicated(key), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
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

consumption_admin_transition_lineage <- function(
    reference_units, admin_events, admin_units_2001, admin_units_2011,
    transition_2001_2011) {
  reference <- safe_df(reference_units)
  events <- safe_df(admin_events)
  admin_2001 <- safe_df(admin_units_2001)
  admin_2011 <- safe_df(admin_units_2011)
  transition <- safe_df(transition_2001_2011)

  # Administrative ancestry is an optional enrichment of the original
  # consumption-lineage contract. Legacy callers that provide none of the
  # enrichment inputs retain exact/alias/reviewed-crosswave behavior.
  enrichment_rows <- c(
    reference_units = nrow(reference),
    admin_units_2011 = nrow(admin_2011),
    transition_2001_2011 = nrow(transition)
  )
  if (!any(enrichment_rows > 0L)) return(data.frame())
  if (any(enrichment_rows == 0L)) {
    missing <- names(enrichment_rows)[enrichment_rows == 0L]
    stop(
      "Consumption administrative lineage enrichment is only partially configured; missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  required_reference <- c(
    "unit_id", "level", "state_code", "district_code",
    "state_std", "district_std", "reference_vintage"
  )
  missing_reference <- setdiff(required_reference, names(reference))
  if (length(missing_reference)) {
    stop(
      "Consumption administrative lineage reference lacks fields: ",
      paste(missing_reference, collapse = ", "),
      call. = FALSE
    )
  }

  current <- reference[
    reference$level == "district" &
      reference$reference_vintage == "current_lgd",
    required_reference,
    drop = FALSE
  ]
  if (!nrow(current)) return(data.frame())

  current$state_std <- canonicalize_state_name(current$state_std)
  current$district_std <- canonicalize_district_name(current$district_std)
  identity_key <- paste(current$state_std, current$district_std, sep = "\r")
  if (anyDuplicated(identity_key)) {
    stop(
      "Current LGD district identities are not unique after normalization.",
      call. = FALSE
    )
  }

  admin11 <- normalize_admin_lookup(admin_2011)
  admin11$state_code <- pad_admin_code(admin11$state_code, 2L)
  admin11$district_code <- pad_admin_code(admin11$district_code, 3L)
  admin11_key <- paste(admin11$state_code, admin11$district_code, sep = "\r")
  if (anyDuplicated(admin11_key)) {
    stop("Census-2011 district registry codes are not unique.", call. = FALSE)
  }

  direct_state <- pad_admin_code(current$state_code, 2L)
  direct_district <- pad_admin_code(current$district_code, 3L)
  direct_key <- paste(direct_state, direct_district, sep = "\r")
  direct_pos <- match(direct_key, admin11_key)
  direct_terminal <- admin11$unit_id[direct_pos]
  direct_terminal[
    is.na(direct_state) | !nzchar(direct_state) |
      is.na(direct_district) | !nzchar(direct_district)
  ] <- NA_character_

  event_resolution <- resolve_lineage_terminals(
    current$unit_id, events, admin_2001, admin_2011
  )
  event_terminal <- ifelse(
    event_resolution$resolution_status == "resolved",
    event_resolution$terminal_unit,
    NA_character_
  )

  terminal <- direct_terminal
  basis <- ifelse(
    !is.na(direct_terminal) & nzchar(direct_terminal),
    "current_lgd_census2011_code",
    NA_character_
  )
  event_available <- !is.na(event_terminal) & nzchar(event_terminal)
  both <- event_available & !is.na(direct_terminal) & nzchar(direct_terminal)
  conflict <- both & event_terminal != direct_terminal
  if (any(conflict)) {
    bad <- paste(
      current$state_std[conflict], current$district_std[conflict],
      sep = "/"
    )
    stop(
      "Current LGD Census-code and accepted-event lineage disagree for: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  use_event <- event_available & !both
  terminal[use_event] <- event_terminal[use_event]
  basis[use_event] <- "accepted_admin_event_parentage"

  deterministic <- deterministic_transition_2011_to_2001(transition)
  admin01 <- normalize_admin_lookup(admin_2001)
  admin01$state_code <- pad_admin_code(admin01$state_code, 2L)
  admin01$district_code <- pad_admin_code(admin01$district_code, 2L)
  target_key <- paste(admin01$state_code, admin01$district_code, sep = "\r")
  if (anyDuplicated(target_key)) {
    stop("Census-2001 district registry codes are not unique.", call. = FALSE)
  }

  target <- rep(NA_character_, nrow(current))
  terminal_basis <- rep(NA_character_, nrow(current))
  direct_2001 <- terminal %in% admin01$unit_id
  target[direct_2001] <- terminal[direct_2001]
  terminal_basis[direct_2001] <- "direct_2001_parent"

  terminal_2011 <- terminal %in% admin11$unit_id
  if (any(terminal_2011) && nrow(deterministic)) {
    terminal_pos <- match(terminal[terminal_2011], admin11$unit_id)
    source_state <- admin11$state_code[terminal_pos]
    source_district <- admin11$district_code[terminal_pos]
    deterministic$state_code_2011 <- pad_admin_code(
      deterministic$state_code_2011, 2L
    )
    deterministic$district_code_2011 <- pad_admin_code(
      deterministic$district_code_2011, 3L
    )
    transition_key <- paste(
      deterministic$state_code_2011,
      deterministic$district_code_2011,
      sep = "\r"
    )
    source_key <- paste(source_state, source_district, sep = "\r")
    transition_pos <- match(source_key, transition_key)
    mapped <- !is.na(transition_pos)

    target_state <- pad_admin_code(
      deterministic$state_code_2001[transition_pos[mapped]], 2L
    )
    target_district <- pad_admin_code(
      deterministic$district_code_2001[transition_pos[mapped]], 2L
    )
    mapped_target_pos <- match(
      paste(target_state, target_district, sep = "\r"),
      target_key
    )
    if (any(is.na(mapped_target_pos))) {
      stop(
        "Deterministic administrative lineage references unknown Census-2001 targets.",
        call. = FALSE
      )
    }

    current_pos <- which(terminal_2011)[mapped]
    target[current_pos] <- admin01$unit_id[mapped_target_pos]
    terminal_basis[current_pos] <- "deterministic_2011_to_2001"
  }

  keep <- !is.na(target) & nzchar(target) &
    !is.na(basis) & nzchar(basis)
  if (!any(keep)) return(data.frame())

  out <- data.frame(
    state_std = current$state_std[keep],
    district_std = current$district_std[keep],
    target_unit_2001 = target[keep],
    lineage_weight = 1,
    lineage_basis = paste0(
      "reviewed_admin_ancestry:",
      basis[keep], ":", terminal_basis[keep]
    ),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(out[c("state_std", "district_std")])) {
    stop("Administrative consumption lineage resolves ambiguously.", call. = FALSE)
  }
  out
}


build_consumption_lineage_reference <- function(
    admin_units_2001, nss_source_roster, full_reviewed_source_crosswalk,
    identity_aliases = NULL, reference_units = data.frame(),
    admin_events = data.frame(), admin_units_2011 = data.frame(),
    transition_2001_2011 = data.frame()) {
  exact <- exact_census_2001_identity_lineage(admin_units_2001)
  list(
    exact = exact,
    source_codes = reviewed_consumption_source_code_lineage(
      nss_source_roster, full_reviewed_source_crosswalk
    ),
    aliases = consumption_identity_alias_lineage(identity_aliases, exact),
    administrative = consumption_admin_transition_lineage(
      reference_units, admin_events, admin_units_2001, admin_units_2011,
      transition_2001_2011
    ),
    reviewed = reviewed_district_identity_lineage(
      nss_source_roster, full_reviewed_source_crosswalk
    )
  )
}

build_consumption_lineage_bridge <- function(households, lineage_reference) {
  source <- consumption_source_district_roster(households)
  if (!nrow(source)) return(empty_consumption_lineage_bridge())
  if (!is.list(lineage_reference) || is.null(lineage_reference$exact) ||
      is.null(lineage_reference$aliases) ||
      is.null(lineage_reference$administrative) ||
      is.null(lineage_reference$reviewed)) {
    stop("Consumption lineage reference is incomplete.", call. = FALSE)
  }
  exact <- safe_df(lineage_reference$exact)
  source_codes <- safe_df(lineage_reference$source_codes)
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

  source_code_rows <- data.frame()
  if (nrow(remaining) && nrow(source_codes)) {
    source_code_rows <- merge(
      remaining, source_codes,
      by = c("survey_id", "source_state_code", "source_district_code"),
      all = FALSE, sort = FALSE
    )
  }
  source_code_ids <- unique(source_code_rows$row_id)
  remaining <- remaining[!remaining$row_id %in% source_code_ids, , drop = FALSE]

  alias_rows <- data.frame()
  if (nrow(remaining) && nrow(lineage_reference$aliases)) {
    alias_rows <- merge(
      remaining, lineage_reference$aliases,
      by = c("state_std", "district_std"), all = FALSE, sort = FALSE
    )
  }
  alias_ids <- unique(alias_rows$row_id)
  remaining <- remaining[!remaining$row_id %in% alias_ids, , drop = FALSE]

  administrative_rows <- data.frame()
  if (nrow(remaining) && nrow(lineage_reference$administrative)) {
    administrative_rows <- merge(
      remaining, lineage_reference$administrative,
      by = c("state_std", "district_std"), all = FALSE, sort = FALSE
    )
  }
  administrative_ids <- unique(administrative_rows$row_id)
  remaining <- remaining[
    !remaining$row_id %in% administrative_ids,
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
  if (nrow(source_code_rows)) {
    source_code_rows$lineage_status <- "resolved_reviewed_source_code"
  }
  if (nrow(alias_rows)) alias_rows$lineage_status <- "resolved_reviewed_identity_alias"
  if (nrow(administrative_rows)) {
    administrative_rows$lineage_status <- "resolved_reviewed_admin_ancestry"
  }
  if (nrow(reviewed_rows)) reviewed_rows$lineage_status <- "resolved_reviewed_consensus"
  out <- safe_bind_rows(list(
    exact_rows, source_code_rows, alias_rows, administrative_rows,
    reviewed_rows, unresolved, noneligible
  ))
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


summarize_consumption_lineage_status_coverage <- function(lineaged_households) {
  x <- safe_df(lineaged_households)
  required <- c(
    "survey_id", "household_id", "source_state_code", "source_district_code",
    "source_lineage_eligible", "lineage_status", "survey_weight", "household_size"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Lineaged consumption households lack status-coverage fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  x$person_weight <- num(x$survey_weight) * num(x$household_size)
  source_key <- paste(
    x$survey_id, x$source_state_code, x$source_district_code,
    sep = "\\r"
  )
  household_source_key <- paste(x$household_id, source_key, sep = "\\r")
  base <- x[!duplicated(household_source_key), , drop = FALSE]

  source_status <- unique(data.frame(
    source_key = source_key,
    lineage_status = plain_chr(x$lineage_status),
    stringsAsFactors = FALSE
  ))
  if (anyDuplicated(source_status$source_key)) {
    counts <- tapply(
      source_status$lineage_status,
      source_status$source_key,
      function(z) length(unique(z))
    )
    if (any(counts > 1L)) {
      stop(
        "A consumption source district has conflicting lineage statuses.",
        call. = FALSE
      )
    }
    source_status <- source_status[
      !duplicated(source_status$source_key), , drop = FALSE
    ]
  }

  total_person <- sum(base$person_weight, na.rm = TRUE)
  eligible_person <- sum(
    base$person_weight[base$source_lineage_eligible %in% TRUE],
    na.rm = TRUE
  )
  statuses <- sort(unique(plain_chr(base$lineage_status)))

  safe_bind_rows(lapply(statuses, function(status) {
    rows <- plain_chr(base$lineage_status) == status
    status_person <- sum(base$person_weight[rows], na.rm = TRUE)
    eligible_status_person <- sum(
      base$person_weight[rows & base$source_lineage_eligible %in% TRUE],
      na.rm = TRUE
    )
    data.frame(
      survey_id = unique(plain_chr(base$survey_id))[[1L]],
      lineage_status = status,
      source_districts = sum(source_status$lineage_status == status),
      person_weight = status_person,
      total_person_weight_share = if (total_person > 0) {
        status_person / total_person
      } else {
        NA_real_
      },
      eligible_person_weight_share = if (eligible_person > 0) {
        eligible_status_person / eligible_person
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  }))
}

save_consumption_lineage_status_coverage <- function(
    coverage,
    path = "outputs/diagnostics/extended/consumption/lineage_status_coverage.csv") {
  write_diagnostic_csv(safe_df(coverage), path)
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
