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
  resolved <- x[x$lineage_status %in% "resolved_reviewed_deterministic" &
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

build_nss64_source_diagnostics <- function(
    usual_activity, migration, ddi_contract, lineaged_usual_activity) {
  validation <- validate_nss64_source_pair(usual_activity, migration, ddi_contract)
  validate_nss64_cross_block_design(usual_activity, migration)
  list(
    source_validation = validation,
    lineage_support = summarize_nss_labor_lineage_support(lineaged_usual_activity),
    target_support = summarize_nss_labor_target_support(lineaged_usual_activity)
  )
}

save_nss64_diagnostics <- function(
    x, district_outcomes = NULL, root = "outputs/diagnostics/extended/labor") {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    source_validation = file.path(root, "nss64_source_validation.csv"),
    lineage_support = file.path(root, "nss64_lineage_support.csv"),
    target_support = file.path(root, "nss64_target_support.csv")
  )
  target_support <- x$target_support
  if (!is.null(district_outcomes)) {
    target_support <- district_outcomes$target_support
    paths <- c(
      paths,
      outcome_registry = file.path(root, "nss64_outcome_registry.csv"),
      district_outcomes = file.path(root, "nss64_district_outcomes.csv")
    )
    utils::write.csv(
      district_outcomes$registry, paths[["outcome_registry"]], row.names = FALSE, na = ""
    )
    utils::write.csv(
      district_outcomes$estimates, paths[["district_outcomes"]], row.names = FALSE, na = ""
    )
  }
  utils::write.csv(x$source_validation, paths[["source_validation"]], row.names = FALSE, na = "")
  utils::write.csv(x$lineage_support, paths[["lineage_support"]], row.names = FALSE, na = "")
  utils::write.csv(target_support, paths[["target_support"]], row.names = FALSE, na = "")
  unname(paths)
}

build_nss66_materialization_diagnostics <- function(materialization, canonical_persons = NULL, lineaged_persons = NULL) {
  blocks <- safe_df(materialization$blocks)
  blocks$source_id <- materialization$source_id
  blocks$materialization_status <- materialization$status
  blocks <- blocks[c(
    "source_id", "materialization_status", "block_id", "relative_path",
    "exists", "rows", "bytes", "modified_at", "sha256"
  )]
  source <- NULL
  if (!is.null(canonical_persons)) {
    if (is.null(lineaged_persons)) stop("NSS66 source diagnostics require lineaged persons.", call. = FALSE)
    source <- build_nss66_source_diagnostics(canonical_persons, lineaged_persons)
  }
  list(materialization = blocks, source_validation = source)
}

save_nss66_materialization_diagnostics <- function(
    x, root = "outputs/diagnostics/extended/labor") {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  source_path <- file.path(root, "nss66_source_validation.csv")
  paths <- c(materialization = file.path(root, "nss66_materialization.csv"))
  utils::write.csv(x$materialization, paths[["materialization"]], row.names = FALSE, na = "")
  if (!is.null(x$source_validation)) {
    paths <- c(paths, source_validation = source_path)
    utils::write.csv(x$source_validation, source_path, row.names = FALSE, na = "")
  } else if (file.exists(source_path)) {
    unlink(source_path)
  }
  unname(paths)
}
