# Deterministic DISE district harmonization to Census-2001 geography.

dise_identity_table <- function(district_year) {
  x <- safe_df(district_year)
  out <- unique(data.frame(
    state_key = canonicalize_state_name(x$state_name_dise),
    district_key = canonicalize_district_name(x$district_name_dise),
    stringsAsFactors = FALSE
  ))
  out <- out[
    !is.na(out$state_key) & nzchar(out$state_key) &
      !is.na(out$district_key) & nzchar(out$district_key),
    , drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

dise_lineage_candidates <- function(state, district, target, source) {
  n <- length(target)
  if (!length(state) && !length(district) && !n) {
    return(data.frame(
      state_key = character(),
      district_key = character(),
      target_unit_2001 = character(),
      bridge_source = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (length(state) != n || length(district) != n) {
    stop("DISE lineage candidate fields must have equal lengths.", call. = FALSE)
  }
  data.frame(
    state_key = canonicalize_state_name(state),
    district_key = canonicalize_district_name(district),
    target_unit_2001 = plain_chr(target),
    bridge_source = rep(source, n),
    stringsAsFactors = FALSE
  )
}

dise_reviewed_lineage_candidates <- function(nss_source_roster, full_reviewed_source_crosswalk) {
  roster <- safe_df(nss_source_roster)
  crosswalk <- safe_df(full_reviewed_source_crosswalk)
  required_roster <- c("source_row_id", "state_std", "district_std")
  required_crosswalk <- c("source_row_id", "target_unit_2001", "weight", "panel_variant")
  if (!all(required_roster %in% names(roster)) || !all(required_crosswalk %in% names(crosswalk))) {
    stop("Reviewed lineage inputs lack required DISE bridge fields.", call. = FALSE)
  }
  deterministic <- crosswalk[
    is.finite(num(crosswalk$weight)) & abs(num(crosswalk$weight) - 1) < 1e-8 &
      crosswalk$panel_variant == "deterministic" &
      !is.na(crosswalk$target_unit_2001) & nzchar(crosswalk$target_unit_2001),
    c("source_row_id", "target_unit_2001"), drop = FALSE
  ]
  out <- merge(roster[required_roster], deterministic, by = "source_row_id", all = FALSE, sort = FALSE)
  unique(dise_lineage_candidates(
    out$state_std,
    out$district_std,
    out$target_unit_2001,
    "reviewed_deterministic_nss_lineage"
  ))
}

dise_admin_2001_candidates <- function(admin_units_2001) {
  admin <- safe_df(admin_units_2001)
  required <- c("unit_id", "state_std", "district_std")
  if (!all(required %in% names(admin))) {
    stop("Census-2001 administrative registry lacks required DISE bridge fields.", call. = FALSE)
  }
  unique(dise_lineage_candidates(
    admin$state_std,
    admin$district_std,
    admin$unit_id,
    "exact_census_2001_identity"
  ))
}

build_dise_deterministic_lineage_bridge <- function(
  district_year, nss_source_roster, full_reviewed_source_crosswalk, admin_units_2001
) {
  identities <- dise_identity_table(district_year)
  candidates <- unique(safe_bind_rows(list(
    dise_admin_2001_candidates(admin_units_2001),
    dise_reviewed_lineage_candidates(nss_source_roster, full_reviewed_source_crosswalk)
  )))
  candidates <- candidates[
    !is.na(candidates$state_key) & nzchar(candidates$state_key) &
      !is.na(candidates$district_key) & nzchar(candidates$district_key) &
      !is.na(candidates$target_unit_2001) & nzchar(candidates$target_unit_2001),
    , drop = FALSE
  ]
  matched <- merge(identities, candidates, by = c("state_key", "district_key"), all.x = TRUE, sort = FALSE)
  groups <- split(seq_len(nrow(matched)), paste(matched$state_key, matched$district_key, sep = "|"))
  out <- safe_bind_rows(lapply(groups, function(i) {
    part <- matched[i, , drop = FALSE]
    targets <- unique(part$target_unit_2001[!is.na(part$target_unit_2001) & nzchar(part$target_unit_2001)])
    sources <- unique(part$bridge_source[!is.na(part$bridge_source) & nzchar(part$bridge_source)])
    data.frame(
      state_key = part$state_key[[1]],
      district_key = part$district_key[[1]],
      target_unit_2001 = if (length(targets) == 1L) targets[[1]] else NA_character_,
      n_candidate_targets = length(targets),
      bridge_status = if (length(targets) == 1L) "deterministic_to_2001" else if (length(targets) > 1L) "ambiguous_reviewed_lineage" else "unresolved_no_deterministic_lineage",
      bridge_sources = if (length(sources)) paste(sort(sources), collapse = ";") else NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

sum_complete_counts <- function(x) {
  x <- num(x)
  if (!length(x) || any(!is.finite(x))) return(NA_real_)
  sum(x)
}

harmonize_dise_report_school_quality_to_2001 <- function(report_quality, bridge) {
  x <- safe_df(report_quality)
  bridge <- safe_df(bridge)
  x$state_key <- canonicalize_state_name(x$state_report)
  x$district_key <- canonicalize_district_name(x$district_report)
  x <- merge(
    x,
    bridge[c("state_key", "district_key", "target_unit_2001", "bridge_status")],
    by = c("state_key", "district_key"),
    all.x = TRUE,
    sort = FALSE
  )
  x <- x[
    x$bridge_status == "deterministic_to_2001" &
      !is.na(x$target_unit_2001) & nzchar(x$target_unit_2001),
    , drop = FALSE
  ]
  if (!nrow(x)) return(data.frame())

  groups <- split(
    seq_len(nrow(x)),
    paste(x$academic_year, x$target_unit_2001, sep = "|")
  )
  out <- safe_bind_rows(lapply(groups, function(i) {
    part <- x[i, , drop = FALSE]
    unique_sources <- unique(paste(part$state_key, part$district_key, sep = "|"))
    usable <- length(unique_sources) == 1L && nrow(part) == 1L
    data.frame(
      academic_year = part$academic_year[[1]],
      target_unit_2001 = part$target_unit_2001[[1]],
      dise_report_school_quality_source_districts = length(unique_sources),
      dise_report_school_quality_status = if (usable) {
        "one_to_one_report_ratio"
      } else {
        "multiple_source_districts_not_aggregated"
      },
      dise_report_pupils_per_teacher = if (usable) {
        num(part$report_pupils_per_teacher[[1]])
      } else NA_real_,
      dise_report_single_teacher_school_share = if (usable) {
        num(part$report_single_teacher_school_share[[1]])
      } else NA_real_,
      dise_report_girls_toilet_school_share = if (usable) {
        num(part$report_girls_toilet_school_share[[1]])
      } else NA_real_,
      girls_toilet_definition = if (usable) {
        plain_chr(part$girls_toilet_definition[[1]])
      } else NA_character_,
      source_pdf = if (usable) plain_chr(part$source_pdf[[1]]) else NA_character_,
      source_page = if (usable) num(part$source_page[[1]]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

harmonize_dise_counts_to_2001 <- function(district_year, bridge) {
  x <- safe_df(district_year)
  bridge <- safe_df(bridge)
  required_bridge <- c(
    "state_key", "district_key", "target_unit_2001", "bridge_status"
  )
  missing_bridge <- setdiff(required_bridge, names(bridge))
  if (length(missing_bridge)) {
    stop(
      "DISE lineage bridge is missing required columns: ",
      paste(missing_bridge, collapse = ", "),
      call. = FALSE
    )
  }
  x$state_key <- canonicalize_state_name(x$state_name_dise)
  x$district_key <- canonicalize_district_name(x$district_name_dise)
  x <- merge(
    x,
    bridge[required_bridge],
    by = c("state_key", "district_key"), all.x = TRUE, sort = FALSE
  )
  mapped <- x[
    x$bridge_status == "deterministic_to_2001" &
      !is.na(x$target_unit_2001) & nzchar(x$target_unit_2001),
    , drop = FALSE
  ]
  if (!nrow(mapped)) return(data.frame())

  # A source-level exclusion from the known-corrupt 2010-11 enrollment workbook
  # must survive geographic pooling. Otherwise the aggregation hierarchy could
  # resurrect the raw grade/direct/management counts that were deliberately
  # excluded when no reviewed report-card total was available.
  if ("dise_total_enrollment_source" %in% names(mapped)) {
    unusable_total <- plain_chr(mapped$dise_total_enrollment_source) ==
      "unavailable_without_report_total"
    for (name in c(
      "dise_grade_enrollment", "dise_direct_enrollment",
      "dise_management_enrollment"
    )) {
      if (name %in% names(mapped)) mapped[[name]][unusable_total] <- NA_real_
    }
  }

  total_candidate_columns <- intersect(c(
    "report_total_enrollment",
    "dise_grade_enrollment",
    "dise_direct_enrollment",
    "dise_management_enrollment"
  ), names(mapped))
  count_columns <- intersect(c(
    "dise_english_enrollment", "dise_hindi_enrollment",
    total_candidate_columns,
    if (!length(total_candidate_columns)) "dise_total_enrollment" else character(),
    "dise_government_enrollment", "dise_private_enrollment",
    "dise_government_schools", "dise_private_schools", "dise_total_schools",
    "dise_total_teachers", "dise_single_teacher_schools", "dise_girls_toilet_schools"
  ), names(mapped))
  groups <- split(seq_len(nrow(mapped)), paste(mapped$academic_year, mapped$target_unit_2001, sep = "|"))
  out <- safe_bind_rows(lapply(groups, function(i) {
    part <- mapped[i, , drop = FALSE]
    row <- data.frame(
      academic_year = part$academic_year[[1]],
      target_unit_2001 = part$target_unit_2001[[1]],
      dise_source_district_count = nrow(part),
      dise_source_districts = paste(sort(unique(plain_chr(part$district_name_dise))), collapse = ";"),
      stringsAsFactors = FALSE
    )
    for (nm in count_columns) row[[nm]] <- sum_complete_counts(part[[nm]])
    row
  }))
  if (length(total_candidate_columns)) {
    n <- nrow(out)
    report <- if ("report_total_enrollment" %in% names(out)) {
      out$report_total_enrollment
    } else rep(NA_real_, n)
    grade <- if ("dise_grade_enrollment" %in% names(out)) out$dise_grade_enrollment else rep(NA_real_, n)
    direct <- if ("dise_direct_enrollment" %in% names(out)) out$dise_direct_enrollment else rep(NA_real_, n)
    management <- if ("dise_management_enrollment" %in% names(out)) out$dise_management_enrollment else rep(NA_real_, n)
    preferred <- choose_dise_total_enrollment(
      grade, direct, management, report_enrollment = report
    )
    out$dise_total_enrollment <- preferred$dise_total_enrollment
    out$dise_total_enrollment_source <- preferred$dise_total_enrollment_source
    out$dise_direct_to_grade_enrollment_ratio <- ifelse(
      is.finite(direct) & direct >= 0 & is.finite(grade) & grade > 0, direct / grade, NA_real_
    )
    out$dise_management_to_grade_enrollment_ratio <- ifelse(
      is.finite(management) & management >= 0 & is.finite(grade) & grade > 0,
      management / grade,
      NA_real_
    )
  }
  if (all(c(
    "dise_english_enrollment", "dise_hindi_enrollment", "dise_total_enrollment"
  ) %in% names(out))) {
    out$dise_english_identity_resolved <- is.finite(out$dise_english_enrollment)
    out$dise_hindi_identity_resolved <- is.finite(out$dise_hindi_enrollment)
    out <- finalize_dise_language_measure(out)
  }
  if (all(c("dise_private_enrollment", "dise_government_enrollment") %in% names(out))) {
    management <- out$dise_private_enrollment + out$dise_government_enrollment
    out$dise_private_enrollment_share <- ifelse(
      is.finite(management) & management > 0,
      100 * out$dise_private_enrollment / management, NA_real_
    )
  }
  if (all(c("dise_private_schools", "dise_total_schools") %in% names(out))) {
    out$dise_private_school_share <- ifelse(
      is.finite(out$dise_total_schools) & out$dise_total_schools > 0,
      100 * out$dise_private_schools / out$dise_total_schools, NA_real_
    )
  }
  out <- finalize_dise_school_quality_measures(out)
  rownames(out) <- NULL
  out
}

build_dise_baseline_treatments_2001 <- function(harmonized_district_year) {
  x <- safe_df(harmonized_district_year)
  anchor <- x[x$academic_year == "2007-08", , drop = FALSE]
  if (anyDuplicated(anchor$target_unit_2001)) {
    stop("Harmonized DISE 2007-08 rows must be unique by Census-2001 unit.", call. = FALSE)
  }
  keep <- intersect(c(
    "target_unit_2001", "dise_source_district_count", "dise_emi_enrollment_share_total",
    "dise_hindi_enrollment_share_total", "dise_english_share_english_hindi",
    "dise_private_enrollment_share", "dise_private_school_share",
    "dise_emi_gross_enrollment_ratio_age_6_13",
    "dise_elementary_gross_enrollment_ratio_age_6_13",
    "dise_english_identity_resolved", "dise_hindi_identity_resolved"
  ), names(anchor))
  out <- anchor[keep]
  rename <- c(
    dise_source_district_count = "dise_source_district_count_0708",
    dise_emi_enrollment_share_total = "dise_emi_enrollment_share_total_0708",
    dise_hindi_enrollment_share_total = "dise_hindi_enrollment_share_total_0708",
    dise_english_share_english_hindi = "dise_english_share_english_hindi_0708",
    dise_private_enrollment_share = "dise_private_enrollment_share_0708",
    dise_private_school_share = "dise_private_school_share_0708",
    dise_emi_gross_enrollment_ratio_age_6_13 = "dise_emi_gross_enrollment_ratio_age_6_13_0708",
    dise_elementary_gross_enrollment_ratio_age_6_13 = "dise_elementary_gross_enrollment_ratio_age_6_13_0708",
    dise_english_identity_resolved = "dise_english_identity_resolved_0708",
    dise_hindi_identity_resolved = "dise_hindi_identity_resolved_0708"
  )
  for (nm in names(rename)) if (nm %in% names(out)) names(out)[names(out) == nm] <- rename[[nm]]
  pool <- x[
    x$academic_year %in% c("2005-06", "2006-07", "2007-08") &
      x$dise_english_identity_resolved %in% TRUE &
      is.finite(num(x$dise_english_enrollment)) & is.finite(num(x$dise_total_enrollment)),
    , drop = FALSE
  ]
  counts <- aggregate(
    pool[c("dise_english_enrollment", "dise_total_enrollment")],
    list(target_unit_2001 = pool$target_unit_2001), sum
  )
  years <- aggregate(
    pool$academic_year,
    list(target_unit_2001 = pool$target_unit_2001),
    function(v) length(unique(v))
  )
  names(years)[[2]] <- "dise_baseline_years_observed"
  counts <- merge(counts, years, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  counts$dise_emi_enrollment_share_total_0508_pooled <- ifelse(
    counts$dise_baseline_years_observed == 3L & is.finite(counts$dise_total_enrollment) & counts$dise_total_enrollment > 0,
    100 * counts$dise_english_enrollment / counts$dise_total_enrollment, NA_real_
  )
  out <- merge(
    out,
    counts[c("target_unit_2001", "dise_baseline_years_observed", "dise_emi_enrollment_share_total_0508_pooled")],
    by = "target_unit_2001", all.x = TRUE, sort = FALSE
  )

  if ("census_age_6_13_population" %in% names(x)) {
    age_pool <- x[
      x$academic_year %in% c("2005-06", "2006-07", "2007-08") &
        x$dise_english_identity_resolved %in% TRUE &
        is.finite(num(x$dise_english_enrollment)) &
        is.finite(num(x$census_age_6_13_population)) &
        num(x$census_age_6_13_population) > 0,
      , drop = FALSE
    ]
    if (nrow(age_pool)) {
      age_counts <- aggregate(
        age_pool[c("dise_english_enrollment", "census_age_6_13_population")],
        list(target_unit_2001 = age_pool$target_unit_2001),
        sum
      )
      age_years <- aggregate(
        age_pool$academic_year,
        list(target_unit_2001 = age_pool$target_unit_2001),
        function(v) length(unique(v))
      )
      names(age_years)[[2]] <- "dise_age_6_13_baseline_years_observed"
      age_counts <- merge(age_counts, age_years, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
      age_counts$dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled <- ifelse(
        age_counts$dise_age_6_13_baseline_years_observed == 3L &
          is.finite(age_counts$census_age_6_13_population) &
          age_counts$census_age_6_13_population > 0,
        100 * age_counts$dise_english_enrollment / age_counts$census_age_6_13_population,
        NA_real_
      )
      out <- merge(
        out,
        age_counts[c(
          "target_unit_2001", "dise_age_6_13_baseline_years_observed",
          "dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled"
        )],
        by = "target_unit_2001", all.x = TRUE, sort = FALSE
      )
    }
  }
  rownames(out) <- NULL
  out
}

attach_dise_treatments_to_panel_2001 <- function(panel, treatments) {
  geometry <- inherits(panel, "sf")
  x <- if (geometry) sf::st_drop_geometry(panel) else safe_df(panel)
  if (!"target_unit_2001" %in% names(x)) {
    stop("Analysis panel lacks target_unit_2001 required for DISE attachment.", call. = FALSE)
  }
  if (anyDuplicated(treatments$target_unit_2001)) {
    stop("DISE Census-2001 treatment table is not unique by target unit.", call. = FALSE)
  }
  x$.dise_panel_row <- seq_len(nrow(x))
  out <- merge(x, treatments, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(x)) stop("DISE Census-2001 treatment attachment changed panel row count.", call. = FALSE)
  out <- out[order(out$.dise_panel_row), , drop = FALSE]
  out$.dise_panel_row <- NULL
  rownames(out) <- NULL
  if (geometry) sf::st_set_geometry(out, sf::st_geometry(panel)) else out
}

build_dise_longitudinal_panel <- function(
  baseline_district_year,
  dynamic_district_year,
  nss_source_roster,
  full_reviewed_source_crosswalk,
  admin_units_2001,
  district_panel
) {
  district_year <- safe_bind_rows(list(baseline_district_year, dynamic_district_year))
  bridge <- build_dise_deterministic_lineage_bridge(
    district_year,
    nss_source_roster,
    full_reviewed_source_crosswalk,
    admin_units_2001
  )
  harmonized <- harmonize_dise_counts_to_2001(district_year, bridge)
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  design_vars <- unique(c(
    "target_unit_2001", "state_code_2001", "region",
    alternative_distance_variables()
  ))
  missing <- setdiff(design_vars, names(panel))
  if (length(missing)) {
    stop("District panel lacks longitudinal DISE design variables: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  design <- panel[design_vars]
  if (anyDuplicated(design$target_unit_2001)) {
    stop("District panel is not unique by Census-2001 target for DISE dynamics.", call. = FALSE)
  }
  out <- merge(harmonized, design, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out$academic_year <- plain_chr(out$academic_year)
  out
}
