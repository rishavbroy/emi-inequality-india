lineage_v2_recovery_class <- function(eligibility) {
  x <- safe_df(eligibility)
  if (!nrow(x)) return(character())
  ifelse(
    x$eligible_primary %in% TRUE,
    "preferred_mapping",
    ifelse(
      x$exclusion_reason %in% "documented_exclusion",
      "documented_survey_or_lineage_exclusion",
      ifelse(
        x$exclusion_reason %in% "dominant_parent_near_complete_requires_review",
        "dominant_parent_near_complete",
        ifelse(
          x$exclusion_reason %in% "multi_parent_allocation_sensitivity_only",
          "multi_parent_fractional_mapping",
          ifelse(
            x$exclusion_reason %in% "source_identity_unadjudicated",
            "source_identity_unresolved",
            "lineage_evidence_unresolved"
          )
        )
      )
    )
  )
}

build_lineage_v2_identity_reclassification <- function(eligibility) {
  x <- safe_df(eligibility)
  required <- c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "status", "eligible_primary", "target_unit_2001", "exclusion_reason",
    "allocation_target_count", "allocation_weight_sum", "allocation_basis",
    "allocation_source_id"
  )
  x <- ensure_columns_v2(x, required)
  x$recovery_class <- lineage_v2_recovery_class(x)
  x$recommended_panel <- ifelse(
    x$recovery_class %in% "preferred_mapping", "preferred",
    ifelse(
      x$recovery_class %in% c("dominant_parent_near_complete", "multi_parent_fractional_mapping"),
      "sensitivity",
      "excluded"
    )
  )
  out <- x[c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "status", "eligible_primary", "target_unit_2001", "exclusion_reason",
    "recovery_class", "recommended_panel", "allocation_target_count",
    "allocation_weight_sum", "allocation_basis", "allocation_source_id"
  )]
  out[order(out$wave, out$raw_state, out$raw_district), , drop = FALSE]
}

build_lineage_v2_district_loss_audit <- function(
  admin_2001, source_roster, eligibility, primary_crosswalk,
  sensitivity_crosswalk
) {
  admin <- safe_df(admin_2001)
  roster <- safe_df(source_roster)
  eligibility <- safe_df(eligibility)
  primary <- safe_df(primary_crosswalk)
  sensitivity <- safe_df(sensitivity_crosswalk)
  required <- c("unit_id", "state_code", "district_code")
  admin <- ensure_columns_v2(admin, required)
  out <- admin[required]
  names(out)[1] <- "target_unit_2001"

  count_targets <- function(crosswalk, prefix) {
    if (!nrow(crosswalk)) return(data.frame(target_unit_2001 = character(), stringsAsFactors = FALSE))
    split_wave <- split(crosswalk, crosswalk$wave)
    parts <- lapply(names(split_wave), function(wave) {
      part <- split_wave[[wave]]
      tab <- aggregate(source_row_id ~ target_unit_2001, part, function(z) length(unique(z)))
      names(tab)[2] <- paste0(prefix, "_", wave, "_source_count")
      tab
    })
    Reduce(function(a, b) merge(a, b, by = "target_unit_2001", all = TRUE), parts)
  }
  out <- merge(out, count_targets(primary, "preferred"), by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out <- merge(out, count_targets(sensitivity, "sensitivity"), by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  count_cols <- grep("_source_count$", names(out), value = TRUE)
  for (nm in count_cols) out[[nm]][is.na(out[[nm]])] <- 0L
  p07 <- out$preferred_nss_2007_08_source_count %||% rep(0L, nrow(out))
  p17 <- out$preferred_nss_2017_18_source_count %||% rep(0L, nrow(out))
  s07 <- out$sensitivity_nss_2007_08_source_count %||% rep(0L, nrow(out))
  s17 <- out$sensitivity_nss_2017_18_source_count %||% rep(0L, nrow(out))
  out$preferred_two_wave <- p07 > 0 & p17 > 0
  out$sensitivity_two_wave <- s07 > 0 & s17 > 0
  out$loss_stage <- ifelse(
    out$preferred_two_wave, "retained_preferred_two_wave",
    ifelse(
      s07 == 0, "no_2007_08_source_mapping",
      ifelse(
        s17 == 0, "no_2017_18_source_mapping",
        "available_only_under_sensitivity_rule"
      )
    )
  )
  out
}

build_lineage_v2_rule_sensitivity <- function(primary_crosswalk, sensitivity_crosswalk, eligibility) {
  primary <- safe_df(primary_crosswalk)
  sensitivity <- safe_df(sensitivity_crosswalk)
  eligibility <- safe_df(eligibility)
  primary_min <- primary[c("source_row_id", "wave", "target_unit_2001")]
  sensitivity_min <- sensitivity[c("source_row_id", "wave", "target_unit_2001")]
  accepted_single <- eligibility[
    eligibility$status %in% "accepted" &
      suppressWarnings(as.integer(eligibility$allocation_target_count)) == 1L &
      abs(suppressWarnings(as.numeric(eligibility$allocation_weight_sum)) - 1) < 1e-8,
    , drop = FALSE
  ]
  dominant_ids <- accepted_single$source_row_id[
    grepl("population_renormalized_min_99pct_mapped", plain_chr(accepted_single$allocation_basis), fixed = TRUE)
  ]
  variants <- list(
    preferred = primary_min,
    preferred_plus_dominant_parent = unique(rbind(
      primary_min,
      sensitivity_min[sensitivity_min$source_row_id %in% dominant_ids, , drop = FALSE]
    )),
    all_reviewed_sensitivity = sensitivity_min
  )
  safe_bind_rows(lapply(names(variants), function(name) {
    x <- variants[[name]]
    by_wave <- table(x$wave)
    targets <- split(unique(x[c("wave", "target_unit_2001")]), unique(x[c("wave", "target_unit_2001")])$wave)
    target_sets <- lapply(targets, function(z) unique(z$target_unit_2001))
    overlap <- if (all(c("nss_2007_08", "nss_2017_18") %in% names(target_sets))) {
      length(intersect(target_sets$nss_2007_08, target_sets$nss_2017_18))
    } else 0L
    data.frame(
      rule = name,
      source_rows_2007_08 = as.integer(by_wave["nss_2007_08"] %||% 0L),
      source_rows_2017_18 = as.integer(by_wave["nss_2017_18"] %||% 0L),
      target_districts_2007_08 = length(target_sets$nss_2007_08 %||% character()),
      target_districts_2017_18 = length(target_sets$nss_2017_18 %||% character()),
      two_wave_target_districts = overlap,
      stringsAsFactors = FALSE
    )
  }))
}

build_lineage_v2_recovery_gates <- function(loss_audit, reclassification) {
  loss <- safe_df(loss_audit)
  rec <- safe_df(reclassification)
  data.frame(
    gate = c(
      "all_2001_districts_accounted_for",
      "no_generic_transition_exclusions",
      "all_nonpreferred_identities_classified",
      "multi_parent_cases_remain_out_of_preferred"
    ),
    passed = c(
      nrow(loss) == 593L && !anyDuplicated(loss$target_unit_2001),
      !any(rec$exclusion_reason %in% "geographic_transition_non_nested_or_incomplete", na.rm = TRUE),
      all(!is.na(rec$recovery_class) & nzchar(rec$recovery_class)),
      all(!(rec$recovery_class %in% "multi_parent_fractional_mapping") | !(rec$eligible_primary %in% TRUE))
    ),
    stringsAsFactors = FALSE
  )
}
