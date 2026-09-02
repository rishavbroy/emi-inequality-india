# Source-contract diagnostics for NSS/PLFS labor microdata.

validate_nss64_source_pair <- function(usual_activity, migration, ddi_contract = NULL) {
  if (anyDuplicated(usual_activity$person_key) || anyDuplicated(migration$person_key)) {
    stop("NSS64 Block 4 and Block 6 person keys must each be unique.", call. = FALSE)
  }
  missing_migration <- setdiff(usual_activity$person_key, migration$person_key)
  missing_activity <- setdiff(migration$person_key, usual_activity$person_key)
  if (length(missing_migration) || length(missing_activity)) {
    stop("NSS64 Block 4 and Block 6 must cover the same household members.", call. = FALSE)
  }
  if (!is.null(ddi_contract)) {
    expected <- setNames(ddi_contract$case_count, ddi_contract$file_id)
    if (!identical(as.numeric(nrow(usual_activity)), as.numeric(expected[["F4"]])) ||
        !identical(as.numeric(nrow(migration)), as.numeric(expected[["F6"]]))) {
      stop("NSS64 person-source row counts do not match the DDI contract.", call. = FALSE)
    }
  }
  data.frame(
    source = c("usual_activity_block4", "migration_block6"),
    rows = c(nrow(usual_activity), nrow(migration)),
    unique_people = c(length(unique(usual_activity$person_key)), length(unique(migration$person_key))),
    positive_weight_share = c(mean(usual_activity$survey_weight > 0), mean(migration$survey_weight > 0)),
    stringsAsFactors = FALSE
  )
}


nss64_shared_design_columns <- nss_labor_shared_design_columns


validate_nss64_cross_block_design <- function(usual_activity, migration) {
  common <- nss64_shared_design_columns()
  missing <- union(
    setdiff(c("person_key", common), names(usual_activity)),
    setdiff(c("person_key", common), names(migration))
  )
  if (length(missing)) {
    stop(
      "NSS64 person sources lack shared design fields: ",
      paste(sort(unique(missing)), collapse = ", "),
      call. = FALSE
    )
  }
  lhs <- usual_activity[match(migration$person_key, usual_activity$person_key), c("person_key", common), drop = FALSE]
  rhs <- migration[c("person_key", common)]
  for (nm in common) {
    x <- lhs[[nm]]
    y <- rhs[[nm]]
    equal <- if (is.numeric(x) || is.numeric(y)) {
      x <- as.numeric(x)
      y <- as.numeric(y)
      (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
    } else {
      x <- plain_chr(x)
      y <- plain_chr(y)
      (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
    }
    bad <- which(!equal)
    if (length(bad)) {
      stop(
        "NSS64 Block 4 and Block 6 disagree on shared field ", nm,
        "; first mismatched person: ", lhs$person_key[[bad[[1L]]]], ".",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

nss64_source_district_code <- function(x) {
  x <- safe_df(x)
  required <- c("state_code", "district_code", "nss_region")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("NSS64 source rows lack geography fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  state <- plain_chr(x$state_code)
  district <- plain_chr(x$district_code)
  region <- plain_chr(x$nss_region)
  valid <- grepl("^[0-9]{2}$", state) & grepl("^[0-9]{2}$", district) &
    grepl("^[0-9]{3}$", region) & substr(region, 1L, 2L) == state
  if (!all(valid)) {
    stop("NSS64 state, NSS-region, and district codes are internally inconsistent.", call. = FALSE)
  }
  paste0(region, district)
}

nss64_reviewed_lineage_map <- function(full_reviewed_crosswalk) {
  crosswalk <- safe_df(full_reviewed_crosswalk)
  required <- c("wave", "source_code", "target_unit_2001", "weight", "panel_variant")
  missing <- setdiff(required, names(crosswalk))
  if (length(missing)) {
    stop("Reviewed district lineage crosswalk lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  map <- crosswalk[crosswalk$wave %in% "nss_2007_08", required, drop = FALSE]
  if (!nrow(map)) stop("Reviewed district lineage has no NSS 2007-08 mappings.", call. = FALSE)
  map$source_code <- gsub("[^0-9]", "", plain_chr(map$source_code))
  map$target_unit_2001 <- plain_chr(map$target_unit_2001)
  map$weight <- num(map$weight)
  deterministic <- map$panel_variant %in% "deterministic" &
    is.finite(map$weight) & abs(map$weight - 1) <= 1e-8 &
    !is.na(map$target_unit_2001) & nzchar(map$target_unit_2001)
  map <- map[deterministic, c("source_code", "target_unit_2001"), drop = FALSE]
  if (!nrow(map)) stop("NSS 2007-08 has no deterministic reviewed district mappings.", call. = FALSE)
  if (anyDuplicated(map$source_code)) {
    stop("Each deterministic NSS64 source district must map to one Census-2001 target.", call. = FALSE)
  }
  map[order(map$source_code), , drop = FALSE]
}

attach_nss64_reviewed_lineage <- function(persons, full_reviewed_crosswalk) {
  x <- safe_df(persons)
  if (!"person_key" %in% names(x)) stop("NSS64 person rows lack person_key.", call. = FALSE)
  x$source_district_code <- nss64_source_district_code(x)
  map <- nss64_reviewed_lineage_map(full_reviewed_crosswalk)
  idx <- match(x$source_district_code, map$source_code)
  x$target_unit_2001 <- map$target_unit_2001[idx]
  x$lineage_status <- ifelse(is.na(x$target_unit_2001), "unresolved_source_district", "resolved_reviewed_deterministic")
  x
}

summarize_nss_labor_lineage_support <- function(lineaged_persons) {
  x <- safe_df(lineaged_persons)
  required <- c(
    "source_district_code", "target_unit_2001", "lineage_status",
    "person_key", "state_code", "sector", "fsu", "survey_weight"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Lineaged NSS labor rows lack fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$.design_psu <- nss_labor_design_psu_key(x)
  groups <- split(seq_len(nrow(x)), x$source_district_code)
  out <- safe_bind_rows(lapply(groups, function(i) {
    rows <- x[i, , drop = FALSE]
    targets <- unique(stats::na.omit(plain_chr(rows$target_unit_2001)))
    status <- unique(plain_chr(rows$lineage_status))
    data.frame(
      source_district_code = rows$source_district_code[[1L]],
      target_unit_2001 = if (length(targets) == 1L) targets[[1L]] else NA_character_,
      lineage_status = if (length(status) == 1L) status[[1L]] else "mixed",
      n_sample_people = nrow(rows),
      n_fsu = length(unique(rows$.design_psu)),
      sum_person_weight = sum(num(rows$survey_weight)),
      kish_effective_n = kish_effective_n(rows$survey_weight),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$source_district_code), , drop = FALSE]
}

summarize_nss_labor_target_support <- function(lineaged_persons) {
  x <- safe_df(lineaged_persons)
  required <- c(
    "source_district_code", "target_unit_2001", "lineage_status",
    "state_code", "sector", "fsu", "survey_weight"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Lineaged NSS labor rows lack target-support fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  resolved <- x[nss_labor_lineage_is_resolved(x$lineage_status) &
                  !is.na(x$target_unit_2001) & nzchar(plain_chr(x$target_unit_2001)), , drop = FALSE]
  if (!nrow(resolved)) return(data.frame())
  resolved$.design_psu <- nss_labor_design_psu_key(resolved)
  groups <- split(seq_len(nrow(resolved)), resolved$target_unit_2001)
  out <- safe_bind_rows(lapply(groups, function(i) {
    rows <- resolved[i, , drop = FALSE]
    data.frame(
      target_unit_2001 = plain_chr(rows$target_unit_2001[[1L]]),
      n_source_districts = length(unique(rows$source_district_code)),
      n_sample_people = nrow(rows),
      n_fsu = length(unique(rows$.design_psu)),
      sum_person_weight = sum(num(rows$survey_weight)),
      kish_effective_n = kish_effective_n(rows$survey_weight),
      n_rural_people = sum(num(rows$sector) == 1, na.rm = TRUE),
      n_urban_people = sum(num(rows$sector) == 2, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$target_unit_2001), , drop = FALSE]
}

summarize_nss64_lineage_support <- summarize_nss_labor_lineage_support
summarize_nss64_target_support <- summarize_nss_labor_target_support

nss66_reviewed_lineage_map <- function(consumption_bridge) {
  bridge <- safe_df(consumption_bridge)
  required <- c(
    "survey_id", "source_state_code", "source_district_code", "target_unit_2001",
    "lineage_weight", "lineage_status"
  )
  missing <- setdiff(required, names(bridge))
  if (length(missing)) {
    stop("NSS66 same-round consumption lineage bridge lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  map <- bridge[bridge$survey_id == "nss_2009_10_type2", required, drop = FALSE]
  map$lineage_weight <- num(map$lineage_weight)
  resolved <- grepl("^resolved_", plain_chr(map$lineage_status)) &
    is.finite(map$lineage_weight) & abs(map$lineage_weight - 1) <= 1e-8 &
    !is.na(map$target_unit_2001) & nzchar(plain_chr(map$target_unit_2001))
  map <- unique(map[resolved, c("source_state_code", "source_district_code", "target_unit_2001", "lineage_status"), drop = FALSE])
  if (!nrow(map)) stop("NSS66 same-round lineage bridge has no resolved one-to-one districts.", call. = FALSE)
  key <- paste(map$source_state_code, map$source_district_code, sep = "
")
  if (anyDuplicated(key)) {
    stop("Each resolved NSS66 source district must map to one Census-2001 target.", call. = FALSE)
  }
  map
}

attach_nss66_reviewed_lineage <- function(persons, consumption_bridge) {
  x <- safe_df(persons)
  required <- c("person_key", "state_code", "district_code")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("NSS66 person rows lack lineage fields: ", paste(missing, collapse = ", "), call. = FALSE)
  map <- nss66_reviewed_lineage_map(consumption_bridge)
  source_key <- paste(plain_chr(x$state_code), plain_chr(x$district_code), sep = "
")
  map_key <- paste(plain_chr(map$source_state_code), plain_chr(map$source_district_code), sep = "
")
  idx <- match(source_key, map_key)
  x$source_district_code <- paste0(plain_chr(x$state_code), plain_chr(x$district_code))
  x$target_unit_2001 <- plain_chr(map$target_unit_2001[idx])
  x$lineage_status <- ifelse(
    is.na(x$target_unit_2001), "unresolved_source_district", "resolved_reviewed_deterministic"
  )
  x$lineage_basis <- ifelse(
    is.na(idx), NA_character_, paste0("same_round_consumption_bridge:", plain_chr(map$lineage_status[idx]))
  )
  x
}

build_nss66_source_diagnostics <- function(canonical_persons, lineaged_persons) {
  data.frame(
    source = "usual_activity_f4_f5_f6",
    rows = nrow(canonical_persons),
    unique_people = length(unique(canonical_persons$person_key)),
    positive_weight_share = mean(num(canonical_persons$survey_weight) > 0),
    resolved_person_share = mean(lineaged_persons$lineage_status == "resolved_reviewed_deterministic"),
    stringsAsFactors = FALSE
  )
}


plfs_2017_18_reviewed_lineage_map <- function(crosswalk, variant = c("primary", "deterministic")) {
  variant <- match.arg(variant)
  x <- safe_df(crosswalk)
  required <- c("wave", "source_code", "target_unit_2001")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Reviewed district lineage crosswalk lacks PLFS reuse fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x <- x[x$wave == "nss_2017_18", , drop = FALSE]
  if (variant == "primary") {
    if (!all(c("weight", "panel_variant", "basis") %in% names(x))) {
      stop("PLFS primary reuse requires weight, panel_variant, and basis fields.", call. = FALSE)
    }
    x$weight <- num(x$weight)
    x <- x[x$panel_variant == "primary" & is.finite(x$weight) & abs(x$weight - 1) <= 1e-8, , drop = FALSE]
    x$lineage_status <- ifelse(
      x$basis == "population_renormalized_min_99pct_mapped",
      "resolved_reviewed_primary",
      "resolved_reviewed_deterministic"
    )
    x$lineage_basis <- plain_chr(x$basis)
  } else {
    if ("panel_variant" %in% names(x)) {
      x <- x[x$panel_variant == "deterministic", , drop = FALSE]
    }
    if ("weight" %in% names(x)) {
      weight <- num(x$weight)
      x <- x[is.finite(weight) & abs(weight - 1) <= 1e-8, , drop = FALSE]
    }
    x$lineage_status <- "resolved_reviewed_deterministic"
    x$lineage_basis <- if ("mapping_class" %in% names(x)) {
      plain_chr(x$mapping_class)
    } else if ("basis" %in% names(x)) {
      plain_chr(x$basis)
    } else {
      NA_character_
    }
  }
  x$source_code <- gsub("[^0-9]", "", plain_chr(x$source_code))
  x$target_unit_2001 <- plain_chr(x$target_unit_2001)
  keep <- grepl("^[0-9]{5}$", x$source_code) &
    !is.na(x$target_unit_2001) & nzchar(x$target_unit_2001)
  map <- unique(x[keep, c(
    "source_code", "target_unit_2001", "lineage_status", "lineage_basis"
  ), drop = FALSE])
  if (!nrow(map) || anyDuplicated(map$source_code)) {
    stop("PLFS reuse requires unique reviewed NSS 2017-18 district mappings for variant ", variant, ".", call. = FALSE)
  }
  map[order(map$source_code), , drop = FALSE]
}

plfs_2017_18_source_district_code <- function(x) {
  x <- safe_df(x)
  required <- c("state_code", "district_code", "nss_region")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("PLFS person rows lack geography fields: ", paste(missing, collapse = ", "), call. = FALSE)
  state <- plain_chr(x$state_code)
  district <- plain_chr(x$district_code)
  region <- plain_chr(x$nss_region)
  valid <- grepl("^[0-9]{2}$", state) & grepl("^[0-9]{2}$", district) &
    grepl("^[0-9]{3}$", region) & substr(region, 1L, 2L) == state
  if (!all(valid)) stop("PLFS state, NSS-region, and district codes are internally inconsistent.", call. = FALSE)
  paste0(region, district)
}

attach_plfs_2017_18_reviewed_lineage <- function(persons, crosswalk, variant = c("primary", "deterministic")) {
  variant <- match.arg(variant)
  x <- safe_df(persons)
  if (!"person_key" %in% names(x)) stop("PLFS person rows lack person_key.", call. = FALSE)
  x$source_district_code <- plfs_2017_18_source_district_code(x)
  map <- plfs_2017_18_reviewed_lineage_map(crosswalk, variant)
  idx <- match(x$source_district_code, map$source_code)
  x$target_unit_2001 <- map$target_unit_2001[idx]
  x$lineage_status <- ifelse(
    is.na(x$target_unit_2001), "unresolved_source_district", map$lineage_status[idx]
  )
  x$lineage_basis <- ifelse(
    is.na(x$target_unit_2001), NA_character_, map$lineage_basis[idx]
  )
  x
}

build_plfs_2017_18_source_diagnostics <- function(canonical_persons, lineaged_persons, lineage_variant) {
  x <- safe_df(canonical_persons)
  lineaged <- safe_df(lineaged_persons)
  if (nrow(x) != nrow(lineaged)) stop("PLFS canonical and lineaged person universes differ.", call. = FALSE)
  resolved <- nss_labor_lineage_is_resolved(lineaged$lineage_status)
  data.frame(
    source = "first_visit_usual_status_f1",
    lineage_variant = lineage_variant,
    rows = nrow(x),
    unique_people = length(unique(x$person_key)),
    positive_weight_share = mean(num(x$survey_weight) > 0),
    source_districts = length(unique(plfs_2017_18_source_district_code(x))),
    resolved_source_districts = length(unique(lineaged$source_district_code[resolved])),
    resolved_person_share = mean(resolved),
    deterministic_person_share = mean(lineaged$lineage_status == "resolved_reviewed_deterministic"),
    accepted_primary_person_share = mean(lineaged$lineage_status == "resolved_reviewed_primary"),
    stringsAsFactors = FALSE
  )
}

build_labor_variant_comparison <- function(preferred_estimates, sensitivity_estimates) {
  preferred <- safe_df(preferred_estimates)
  sensitivity <- safe_df(sensitivity_estimates)
  required <- c("target_unit_2001", "outcome_id", "estimate", "std_error", "preferred_eligible")
  for (x in list(preferred, sensitivity)) {
    missing <- setdiff(required, names(x))
    if (length(missing)) {
      stop("Labor variant comparison lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  p <- preferred[required]
  c <- sensitivity[required]
  names(p)[3:5] <- paste0(names(p)[3:5], "_preferred")
  names(c)[3:5] <- paste0(names(c)[3:5], "_sensitivity")
  joined <- merge(p, c, by = c("target_unit_2001", "outcome_id"), all = TRUE, sort = FALSE)
  outcomes <- sort(unique(joined$outcome_id))
  rows <- lapply(outcomes, function(outcome) {
    x <- joined[joined$outcome_id == outcome, , drop = FALSE]
    common <- is.finite(x$estimate_preferred) & is.finite(x$estimate_sensitivity)
    delta <- abs(x$estimate_preferred[common] - x$estimate_sensitivity[common])
    changed <- delta > 1e-12
    correlation <- if (sum(common) >= 2L) {
      stats::cor(x$estimate_preferred[common], x$estimate_sensitivity[common])
    } else {
      NA_real_
    }
    data.frame(
      outcome_id = outcome,
      preferred_cells = sum(is.finite(x$estimate_preferred)),
      sensitivity_cells = sum(is.finite(x$estimate_sensitivity)),
      common_cells = sum(common),
      preferred_only_cells = sum(is.finite(x$estimate_preferred) & !is.finite(x$estimate_sensitivity)),
      preferred_eligible_cells = sum(x$preferred_eligible_preferred %in% TRUE, na.rm = TRUE),
      sensitivity_eligible_cells = sum(x$preferred_eligible_sensitivity %in% TRUE, na.rm = TRUE),
      changed_common_cells = sum(changed),
      changed_common_share = if (length(delta)) mean(changed) else NA_real_,
      estimate_correlation = correlation,
      median_abs_difference = if (length(delta)) stats::median(delta) else NA_real_,
      p90_abs_difference = if (length(delta)) as.numeric(stats::quantile(delta, .9, names = FALSE)) else NA_real_,
      max_abs_difference = if (length(delta)) max(delta) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

save_labor_variant_comparison <- function(
    x, filename, root = "outputs/diagnostics/extended/labor") {
  write_diagnostic_bundle(
    list(variant_comparison = safe_df(x)), root,
    c(variant_comparison = filename)
  )
}

build_plfs_2017_18_diagnostics <- function(canonical_persons, lineaged_persons, lineage_variant) {
  build_nss_labor_diagnostics(
    build_plfs_2017_18_source_diagnostics(canonical_persons, lineaged_persons, lineage_variant),
    lineaged_persons
  )
}

build_nss_labor_diagnostics <- function(source_validation, lineaged_usual_activity) {
  list(
    source_validation = safe_df(source_validation),
    lineage_support = summarize_nss_labor_lineage_support(lineaged_usual_activity),
    target_support = summarize_nss_labor_target_support(lineaged_usual_activity)
  )
}

build_nss64_source_diagnostics <- function(
    usual_activity, migration, ddi_contract, lineaged_usual_activity) {
  validation <- validate_nss64_source_pair(usual_activity, migration, ddi_contract)
  validate_nss64_cross_block_design(usual_activity, migration)
  build_nss_labor_diagnostics(validation, lineaged_usual_activity)
}

build_nss66_diagnostics <- function(canonical_persons, lineaged_persons) {
  build_nss_labor_diagnostics(
    build_nss66_source_diagnostics(canonical_persons, lineaged_persons),
    lineaged_persons
  )
}

save_nss_labor_diagnostics <- function(
    x, prefix, district_outcomes = NULL, root = "outputs/diagnostics/extended/labor") {
  if (!nzchar(prefix) || !grepl("^[a-z0-9_]+$", prefix)) {
    stop("NSS labor diagnostic prefix must be a non-empty file-safe identifier.", call. = FALSE)
  }
  names_and_objects <- list(
    source_validation = x$source_validation,
    lineage_support = x$lineage_support,
    target_support = if (is.null(district_outcomes)) x$target_support else district_outcomes$target_support
  )
  if (!is.null(district_outcomes)) {
    names_and_objects$outcome_registry <- district_outcomes$registry
    names_and_objects$district_outcomes <- district_outcomes$estimates
  }
  filenames <- stats::setNames(
    paste0(prefix, "_", names(names_and_objects), ".csv"),
    names(names_and_objects)
  )
  write_diagnostic_bundle(names_and_objects, root, filenames)
}

save_nss64_diagnostics <- function(
    x, district_outcomes = NULL, root = "outputs/diagnostics/extended/labor") {
  save_nss_labor_diagnostics(x, "nss64", district_outcomes, root)
}

save_nss66_diagnostics <- function(
    x, district_outcomes = NULL, root = "outputs/diagnostics/extended/labor",
    fallback_path = NULL) {
  suffixes <- c(
    "source_validation", "lineage_support", "target_support",
    "outcome_registry", "district_outcomes"
  )
  stale <- file.path(root, paste0("nss66_", suffixes, ".csv"))
  if (is.null(x)) {
    unlink(stale[file.exists(stale)])
    return(if (is.null(fallback_path)) character() else fallback_path)
  }
  save_nss_labor_diagnostics(x, "nss66", district_outcomes, root)
}

save_plfs_2017_18_diagnostics <- function(
    x, district_outcomes = NULL, root = "outputs/diagnostics/extended/labor") {
  save_nss_labor_diagnostics(x, "plfs_2017_18", district_outcomes, root)
}

build_nesstar_materialization_diagnostics <- function(materialization) {
  blocks <- safe_df(materialization$blocks)
  blocks$source_id <- materialization$source_id
  blocks$materialization_status <- materialization$status
  blocks$manifest_schema <- if (is.null(materialization$manifest_schema)) NA_character_ else materialization$manifest_schema
  blocks[c(
    "source_id", "materialization_status", "manifest_schema", "block_id", "relative_path",
    "exists", "rows", "bytes", "modified_at", "sha256"
  )]
}

save_nesstar_materialization_diagnostics <- function(
    x, filename, root = "outputs/diagnostics/extended/labor") {
  if (length(filename) != 1L || is.na(filename) || !nzchar(filename)) {
    stop("Nesstar materialization diagnostic filename must be one non-empty string.", call. = FALSE)
  }
  write_diagnostic_bundle(
    list(materialization = safe_df(x)), root, c(materialization = filename)
  )
}

save_plfs_source_package_diagnostics <- function(
    x, root = "outputs/diagnostics/extended/labor") {
  write_diagnostic_bundle(
    list(source_package = safe_df(x)), root,
    c(source_package = "plfs_2017_18_source_package.csv")
  )
}

labor_mechanism_registry <- function(wave_id = c("nss66", "plfs_2017_18")) {
  wave_id <- match.arg(wave_id)
  temporal_role <- c(nss66 = "early_post", plfs_2017_18 = "long_run_post")[[wave_id]]
  data.frame(
    outcome_id = c("labor_force_participation_age15plus", "employment_rate_age15plus"),
    source_id = rep("district_outcomes", 2L),
    variable = c("labor_force_participation_age15plus", "employment_rate_age15plus"),
    mechanism_family = c("labor_supply", "employment"),
    tier = rep("core", 2L),
    denominator = rep("age15plus", 2L),
    wave_id = rep(wave_id, 2L),
    temporal_role = rep(temporal_role, 2L),
    stringsAsFactors = FALSE
  )
}

labor_mechanism_source <- function(district_outcomes, registry) {
  x <- safe_df(district_outcomes)
  registry <- safe_df(registry)
  required <- c("target_unit_2001", "outcome_id", "estimate", "analysis_eligible")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Labor mechanism district outcomes lack fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(registry) || anyDuplicated(registry$outcome_id) ||
      !all(registry$outcome_id %in% x$outcome_id)) {
    stop("Labor mechanism registry does not match the district outcome family.", call. = FALSE)
  }
  x <- x[x$outcome_id %in% registry$outcome_id & x$analysis_eligible %in% TRUE, required, drop = FALSE]
  if (!nrow(x)) stop("Labor mechanism family has no preferred-support district outcomes.", call. = FALSE)
  if (anyDuplicated(x[c("target_unit_2001", "outcome_id")])) {
    stop("Labor mechanism district outcomes are not unique by target and outcome.", call. = FALSE)
  }

  targets <- sort(unique(plain_chr(x$target_unit_2001)))
  out <- data.frame(target_unit_2001 = targets, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(registry))) {
    outcome_id <- registry$outcome_id[[i]]
    variable <- registry$variable[[i]]
    part <- x[x$outcome_id == outcome_id, c("target_unit_2001", "estimate"), drop = FALSE]
    names(part)[[2L]] <- variable
    out <- merge(out, part, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  }
  out <- out[match(targets, out$target_unit_2001), , drop = FALSE]
  rownames(out) <- NULL
  out
}

labor_mechanism_specifications <- function(
    wave_id = c("nss66", "plfs_2017_18"),
    outcome = "labor_force_participation_age15plus",
    treatment = preferred_iv_variables()$treatment,
    control_registry = NULL,
    sample_suffix = "primary") {
  wave_id <- match.arg(wave_id)
  posttreatment_mechanism_specifications(
    outcome = outcome,
    treatment = treatment,
    sample_rule = paste(wave_id, sample_suffix, "core_labor_common_support", sep = "_"),
    control_registry = control_registry
  )
}

build_labor_mechanism_inference <- function(
    district_panel,
    district_outcomes,
    wave_id = c("nss66", "plfs_2017_18"),
    cfg = list(),
    control_registry = NULL,
    sample_suffix = "primary") {
  wave_id <- match.arg(wave_id)
  registry <- labor_mechanism_registry(wave_id)
  source <- labor_mechanism_source(district_outcomes, registry)
  specifications <- labor_mechanism_specifications(
    wave_id = wave_id,
    outcome = registry$variable[[1L]],
    control_registry = control_registry,
    sample_suffix = sample_suffix
  )
  label <- paste(toupper(wave_id), sample_suffix, "labor")
  panel <- prepare_posttreatment_mechanism_panel(
    district_panel = district_panel,
    sources = list(district_outcomes = source),
    registry = registry,
    specifications = specifications,
    label = label
  )
  estimate_posttreatment_mechanism_models(
    mechanism_panel = panel,
    registry = registry,
    specifications = specifications,
    cfg = cfg,
    label = label
  )
}

save_labor_mechanism_inference <- function(
    x, prefix, root = "outputs/diagnostics/extended/labor") {
  if (!nzchar(prefix) || !grepl("^[a-z0-9_]+$", prefix)) {
    stop("Labor mechanism diagnostic prefix must be a file-safe identifier.", call. = FALSE)
  }
  save_posttreatment_mechanism_outputs(
    x, root, prefix = paste0(prefix, "_mechanism_")
  )
}
