lineage_recovery_class <- function(eligibility) {
  x <- safe_df(eligibility)
  if (!nrow(x)) return(character())
  ifelse(
    x$eligible_conservative %in% TRUE,
    "conservative_mapping",
    ifelse(
      x$exclusion_reason %in% "documented_exclusion",
      "documented_survey_or_lineage_exclusion",
      ifelse(
        x$exclusion_reason %in% "primary_near_complete_requires_review",
        "primary_near_complete",
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

build_lineage_identity_reclassification <- function(eligibility) {
  x <- safe_df(eligibility)
  required <- c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "status", "eligible_conservative", "target_unit_2001", "exclusion_reason",
    "allocation_target_count", "allocation_weight_sum", "allocation_basis",
    "allocation_source_id"
  )
  x <- ensure_columns(x, required)
  x$recovery_class <- lineage_recovery_class(x)
  x$recommended_panel <- ifelse(
    x$recovery_class %in% "conservative_mapping", "conservative",
    ifelse(
      x$recovery_class %in% "primary_near_complete",
      "primary",
      ifelse(
        x$recovery_class %in% "multi_parent_fractional_mapping",
        "full_reviewed",
        "excluded"
      )
    )
  )
  out <- x[c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "status", "eligible_conservative", "target_unit_2001", "exclusion_reason",
    "recovery_class", "recommended_panel", "allocation_target_count",
    "allocation_weight_sum", "allocation_basis", "allocation_source_id"
  )]
  out[order(out$wave, out$raw_state, out$raw_district), , drop = FALSE]
}

build_lineage_multi_parent_review_queue <- function(
  reclassification, full_reviewed_crosswalk
) {
  rec <- safe_df(reclassification)
  crosswalk <- safe_df(full_reviewed_crosswalk)
  ids <- unique(rec$source_row_id[
    rec$recovery_class %in% "multi_parent_fractional_mapping"
  ])
  out <- crosswalk[crosswalk$source_row_id %in% ids, , drop = FALSE]
  if (!nrow(out)) return(data.frame())

  metadata <- rec[rec$source_row_id %in% ids, c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "allocation_target_count", "allocation_weight_sum", "allocation_basis",
    "allocation_source_id"
  ), drop = FALSE]
  metadata <- metadata[!duplicated(metadata$source_row_id), , drop = FALSE]
  out <- merge(
    out,
    metadata,
    by = c("source_row_id", "wave", "source_code"),
    all.x = TRUE,
    sort = FALSE
  )
  out <- out[order(out$raw_state, out$raw_district, -out$weight), , drop = FALSE]
  out$allocation_rank <- ave(
    out$weight,
    out$source_row_id,
    FUN = function(x) rank(-x, ties.method = "first")
  )
  out$review_status <- "needs_fractional_validation"
  out$required_evidence <- paste(
    "Confirm predecessor districts and territorial shares with an official",
    "district history, Gazette or atlas schedule, LGD changed-locality record,",
    "or a validated locality-level crosswalk before considering primary use."
  )
  out[c(
    "source_row_id", "wave", "source_code", "raw_state", "raw_district",
    "target_unit_2001", "weight", "allocation_rank",
    "allocation_target_count", "allocation_weight_sum", "basis", "source_id",
    "review_status", "required_evidence"
  )]
}

build_lineage_district_loss_audit <- function(
  admin_2001, source_roster, eligibility, conservative_crosswalk,
  full_reviewed_crosswalk
) {
  admin <- safe_df(admin_2001)
  roster <- safe_df(source_roster)
  eligibility <- safe_df(eligibility)
  conservative <- safe_df(conservative_crosswalk)
  full_reviewed <- safe_df(full_reviewed_crosswalk)
  required <- c("unit_id", "state_code", "district_code")
  admin <- ensure_columns(admin, required)
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
  out <- merge(out, count_targets(conservative, "conservative"), by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out <- merge(out, count_targets(full_reviewed, "full_reviewed"), by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  count_cols <- grep("_source_count$", names(out), value = TRUE)
  for (nm in count_cols) out[[nm]][is.na(out[[nm]])] <- 0L
  p07 <- out$conservative_nss_2007_08_source_count %||% rep(0L, nrow(out))
  p17 <- out$conservative_nss_2017_18_source_count %||% rep(0L, nrow(out))
  s07 <- out$full_reviewed_nss_2007_08_source_count %||% rep(0L, nrow(out))
  s17 <- out$full_reviewed_nss_2017_18_source_count %||% rep(0L, nrow(out))
  out$conservative_two_wave <- p07 > 0 & p17 > 0
  out$full_reviewed_two_wave <- s07 > 0 & s17 > 0
  out$loss_stage <- ifelse(
    out$conservative_two_wave, "retained_conservative_two_wave",
    ifelse(
      s07 == 0, "no_2007_08_source_mapping",
      ifelse(
        s17 == 0, "no_2017_18_source_mapping",
        "available_only_under_full_reviewed_rule"
      )
    )
  )
  out
}

build_lineage_rule_sensitivity <- function(
  conservative_crosswalk, full_reviewed_crosswalk, eligibility
) {
  conservative <- safe_df(conservative_crosswalk)
  full_reviewed <- safe_df(full_reviewed_crosswalk)
  eligibility <- safe_df(eligibility)
  conservative_min <- conservative[c(
    "source_row_id", "wave", "target_unit_2001"
  )]
  full_reviewed_min <- full_reviewed[c(
    "source_row_id", "wave", "target_unit_2001"
  )]
  accepted_single <- eligibility[
    eligibility$status %in% "accepted" &
      suppressWarnings(as.integer(eligibility$allocation_target_count)) == 1L &
      abs(suppressWarnings(as.numeric(eligibility$allocation_weight_sum)) - 1) < 1e-8,
    , drop = FALSE
  ]
  primary_ids <- accepted_single$source_row_id[
    grepl("population_renormalized_min_99pct_mapped", plain_chr(accepted_single$allocation_basis), fixed = TRUE)
  ]
  variants <- list(
    conservative = conservative_min,
    primary = unique(rbind(
      conservative_min,
      full_reviewed_min[
        full_reviewed_min$source_row_id %in% primary_ids,
        ,
        drop = FALSE
      ]
    )),
    full_reviewed = full_reviewed_min
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

build_lineage_recovery_gates <- function(loss_audit, reclassification) {
  loss <- safe_df(loss_audit)
  rec <- safe_df(reclassification)
  data.frame(
    gate = c(
      "all_2001_districts_accounted_for",
      "no_generic_transition_exclusions",
      "all_nonprimary_identities_classified",
      "multi_parent_cases_remain_out_of_primary"
    ),
    passed = c(
      nrow(loss) == 593L && !anyDuplicated(loss$target_unit_2001),
      !any(rec$exclusion_reason %in% "geographic_transition_non_nested_or_incomplete", na.rm = TRUE),
      all(!is.na(rec$recovery_class) & nzchar(rec$recovery_class)),
      all(!(rec$recovery_class %in% "multi_parent_fractional_mapping") | !(rec$eligible_conservative %in% TRUE))
    ),
    stringsAsFactors = FALSE
  )
}


empty_primary_reviews <- function() {
  data.frame(
    source_row_id = character(), wave = character(), source_code = character(),
    raw_state = character(), raw_district = character(), terminal_unit = character(),
    target_unit_2001 = character(), review_status = character(),
    reviewed_panel = character(), evidence_basis = character(),
    evidence_source_ids = character(), notes = character(),
    stringsAsFactors = FALSE
  )
}

read_primary_reviews <- function(x) {
  x <- safe_df(x)
  if (!nrow(x)) return(empty_primary_reviews())
  required <- names(empty_primary_reviews())
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Primary-panel reviews are missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x <- x[required]
  if (anyDuplicated(x$source_row_id)) {
    stop("Primary-panel reviews must contain one row per source identity.", call. = FALSE)
  }
  accepted <- x$review_status %in% "accepted_primary"
  invalid <- accepted & (
    !(x$wave %in% "nss_2017_18") |
      !grepl("^pc2011__", x$terminal_unit) |
      !grepl("^pc2001__", x$target_unit_2001) |
      !nzchar(x$evidence_source_ids)
  )
  if (any(invalid)) {
    stop("Accepted primary-panel reviews require 2017-18 source identities, 2011 and 2001 unit IDs, and evidence sources.", call. = FALSE)
  }
  x
}

build_primary_source_crosswalk <- function(
  conservative_crosswalk, full_reviewed_crosswalk, reviews
) {
  conservative <- safe_df(conservative_crosswalk)
  full_reviewed <- safe_df(full_reviewed_crosswalk)
  reviews <- read_primary_reviews(reviews)
  accepted <- reviews[reviews$review_status %in% "accepted_primary", c(
    "source_row_id", "target_unit_2001"
  ), drop = FALSE]

  # A later evidence upgrade can promote a formerly primary-only source into
  # the conservative crosswalk. In that case the historical review is
  # redundant, not invalid, but it must still agree with the stronger mapping.
  promoted <- merge(
    conservative[c("source_row_id", "target_unit_2001")],
    accepted,
    by = "source_row_id", all = FALSE, sort = FALSE,
    suffixes = c("_conservative", "_reviewed")
  )
  conflicting_promotions <- promoted$source_row_id[
    promoted$target_unit_2001_conservative != promoted$target_unit_2001_reviewed
  ]
  if (length(conflicting_promotions)) {
    stop(
      "Accepted primary-panel reviews conflict with stronger conservative mappings: ",
      paste(conflicting_promotions, collapse = ", "), call. = FALSE
    )
  }

  pending <- accepted[
    !accepted$source_row_id %in% conservative$source_row_id,
    , drop = FALSE
  ]
  reviewed <- merge(
    full_reviewed, pending,
    by = c("source_row_id", "target_unit_2001"),
    all = FALSE, sort = FALSE
  )
  reviewed <- reviewed[
    reviewed$wave %in% "nss_2017_18" &
      reviewed$basis %in% "population_renormalized_min_99pct_mapped" &
      abs(suppressWarnings(as.numeric(reviewed$weight)) - 1) < 1e-8,
    , drop = FALSE
  ]
  missing_reviews <- setdiff(pending$source_row_id, reviewed$source_row_id)
  if (length(missing_reviews)) {
    stop(
      "Accepted primary-panel reviews do not match an eligible single-target allocation: ",
      paste(missing_reviews, collapse = ", "), call. = FALSE
    )
  }

  keep <- unique(c(conservative$source_row_id, reviewed$source_row_id))
  out <- full_reviewed[full_reviewed$source_row_id %in% keep, , drop = FALSE]
  missing_primary <- setdiff(conservative$source_row_id, out$source_row_id)
  if (length(missing_primary)) {
    stop("Full-reviewed crosswalk is missing conservative source identities.", call. = FALSE)
  }
  if (anyDuplicated(out$source_row_id)) {
    stop("Primary crosswalk must contain one row per source identity.", call. = FALSE)
  }
  out$panel_variant <- "primary"
  out
}

build_lineage_panel_variant_summary <- function(
  conservative_crosswalk, primary_crosswalk, full_reviewed_crosswalk,
  primary_reviews
) {
  variants <- list(
    conservative = safe_df(conservative_crosswalk),
    primary = safe_df(primary_crosswalk),
    full_reviewed = safe_df(full_reviewed_crosswalk)
  )
  descriptions <- c(
    conservative = "Deterministic official, registry, alias, and accepted single-parent mappings only.",
    primary = "Conservative mappings plus reviewed 2017-18 single-parent allocations with at least 99 percent SHRUG coverage and corroborating LGD or India State Stories evidence.",
    full_reviewed = "Primary mappings plus reviewed multi-parent fractional allocations; robustness specification only."
  )
  safe_bind_rows(lapply(names(variants), function(name) {
    x <- variants[[name]]
    wave_targets <- unique(x[c("wave", "target_unit_2001")])
    targets <- split(wave_targets, wave_targets$wave)
    target_sets <- lapply(targets, function(z) unique(z$target_unit_2001))
    overlap <- if (all(c("nss_2007_08", "nss_2017_18") %in% names(target_sets))) {
      length(intersect(target_sets$nss_2007_08, target_sets$nss_2017_18))
    } else 0L
    data.frame(
      panel_variant = name,
      source_rows_2007_08 = sum(x$wave %in% "nss_2007_08"),
      source_rows_2017_18 = sum(x$wave %in% "nss_2017_18"),
      target_districts_2007_08 = length(target_sets$nss_2007_08 %||% character()),
      target_districts_2017_18 = length(target_sets$nss_2017_18 %||% character()),
      two_wave_target_districts = overlap,
      accepted_primary_reviews = if (name == "primary") {
        sum(safe_df(primary_reviews)$review_status %in% "accepted_primary")
      } else 0L,
      description = descriptions[[name]],
      stringsAsFactors = FALSE
    )
  }))
}
