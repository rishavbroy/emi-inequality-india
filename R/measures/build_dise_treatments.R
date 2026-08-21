# DISE/UDISE administrative EMI treatment constructions.

prepare_dise_medium_crosswalk <- function(crosswalk) {
  out <- safe_df(crosswalk)
  out$state_key <- canonicalize_state_name(out$state_report)
  out$district_key <- canonicalize_district_name(out$district_report)
  out$medium_slot <- suppressWarnings(as.integer(out$medium_slot))
  out$language_label <- trimws(plain_chr(out$language_label))
  key <- paste(out$academic_year, out$state_key, out$district_key, out$medium_slot, sep = "|")
  if (anyDuplicated(key)) stop("Canonicalized DISE medium crosswalk is not unique.", call. = FALSE)
  out
}

attach_dise_medium_identities <- function(district_year, crosswalk) {
  x <- safe_df(district_year)
  x$state_key <- canonicalize_state_name(x$state_name_dise)
  x$district_key <- canonicalize_district_name(x$district_name_dise)
  crosswalk <- prepare_dise_medium_crosswalk(crosswalk)

  slot_rows <- safe_bind_rows(lapply(1:5, function(slot) {
    data.frame(
      academic_year = x$academic_year,
      state_key = x$state_key,
      district_key = x$district_key,
      medium_slot = slot,
      slot_enrollment = num(x[[paste0("dise_medium_slot_", slot, "_enrollment")]]),
      .row = seq_len(nrow(x)),
      stringsAsFactors = FALSE
    )
  }))
  slot_rows <- merge(
    slot_rows,
    crosswalk[c("academic_year", "state_key", "district_key", "medium_slot", "language_label")],
    by = c("academic_year", "state_key", "district_key", "medium_slot"),
    all.x = TRUE,
    sort = FALSE
  )
  positive <- is.finite(slot_rows$slot_enrollment) & slot_rows$slot_enrollment > 0
  slot_rows$identity_known <- !is.na(slot_rows$language_label) & nzchar(slot_rows$language_label)

  summarize_language <- function(row_id, language) {
    part <- slot_rows[slot_rows$.row == row_id, , drop = FALSE]
    language_rows <- part$identity_known & part$language_label == language
    if (any(language_rows)) {
      values <- part$slot_enrollment[language_rows]
      if (any(!is.finite(values))) return(NA_real_)
      return(sum(values))
    }
    unresolved_positive <- is.finite(part$slot_enrollment) &
      part$slot_enrollment > 0 &
      !part$identity_known
    if (any(unresolved_positive)) return(NA_real_)
    0
  }
  x$dise_medium_identity_complete <- vapply(seq_len(nrow(x)), function(i) {
    part <- slot_rows[slot_rows$.row == i, , drop = FALSE]
    !any(is.finite(part$slot_enrollment) & part$slot_enrollment > 0 & !part$identity_known)
  }, logical(1))
  x$dise_english_enrollment <- vapply(
    seq_len(nrow(x)), summarize_language, numeric(1), language = "English"
  )
  x$dise_hindi_enrollment <- vapply(
    seq_len(nrow(x)), summarize_language, numeric(1), language = "Hindi"
  )
  x$dise_english_identity_resolved <- is.finite(x$dise_english_enrollment)
  x$dise_hindi_identity_resolved <- is.finite(x$dise_hindi_enrollment)
  x$dise_emi_enrollment_share_total <- ifelse(
    x$dise_english_identity_resolved &
      is.finite(x$dise_total_enrollment) &
      x$dise_total_enrollment > 0,
    100 * x$dise_english_enrollment / x$dise_total_enrollment,
    NA_real_
  )
  x$dise_hindi_enrollment_share_total <- ifelse(
    x$dise_hindi_identity_resolved &
      is.finite(x$dise_total_enrollment) &
      x$dise_total_enrollment > 0,
    100 * x$dise_hindi_enrollment / x$dise_total_enrollment,
    NA_real_
  )
  eh <- x$dise_english_enrollment + x$dise_hindi_enrollment
  x$dise_english_share_english_hindi <- ifelse(
    x$dise_english_identity_resolved &
      x$dise_hindi_identity_resolved &
      is.finite(eh) &
      eh > 0,
    100 * x$dise_english_enrollment / eh,
    NA_real_
  )
  x
}

dise_baseline_anchor <- function(district_year) {
  x <- district_year[district_year$academic_year == "2007-08", , drop = FALSE]
  x$state_07_key <- canonicalize_state_name(x$state_name_dise)
  x$district_07_key <- canonicalize_district_name(x$district_name_dise)
  key <- paste(x$state_07_key, x$district_07_key, sep = "|")
  if (anyDuplicated(key)) stop("DISE 2007-08 district names are not unique after canonicalization.", call. = FALSE)
  x
}

build_dise_baseline_treatments <- function(district_year, medium_crosswalk) {
  x <- attach_dise_medium_identities(district_year, medium_crosswalk)
  anchor <- dise_baseline_anchor(x)

  out <- anchor[c(
    "state_name_dise", "district_name_dise", "state_07_key", "district_07_key",
    "dise_emi_enrollment_share_total",
    "dise_hindi_enrollment_share_total", "dise_english_share_english_hindi",
    "dise_private_enrollment_share", "dise_private_school_share",
    "dise_medium_classification_ratio", "dise_medium_identity_complete",
    "dise_english_identity_resolved", "dise_hindi_identity_resolved"
  )]
  names(out)[names(out) == "dise_emi_enrollment_share_total"] <- "dise_emi_enrollment_share_total_0708"
  names(out)[names(out) == "dise_hindi_enrollment_share_total"] <- "dise_hindi_enrollment_share_total_0708"
  names(out)[names(out) == "dise_english_share_english_hindi"] <- "dise_english_share_english_hindi_0708"
  names(out)[names(out) == "dise_private_enrollment_share"] <- "dise_private_enrollment_share_0708"
  names(out)[names(out) == "dise_private_school_share"] <- "dise_private_school_share_0708"
  names(out)[names(out) == "dise_medium_classification_ratio"] <- "dise_medium_classification_ratio_0708"
  names(out)[names(out) == "dise_medium_identity_complete"] <- "dise_medium_identity_complete_0708"
  names(out)[names(out) == "dise_english_identity_resolved"] <- "dise_english_identity_resolved_0708"
  names(out)[names(out) == "dise_hindi_identity_resolved"] <- "dise_hindi_identity_resolved_0708"

  pool <- x[x$academic_year %in% c("2005-06", "2006-07", "2007-08"), , drop = FALSE]
  pool$state_key <- canonicalize_state_name(pool$state_name_dise)
  pool$district_key <- canonicalize_district_name(pool$district_name_dise)
  pool <- pool[
    pool$dise_english_identity_resolved &
      stats::complete.cases(pool[c("dise_english_enrollment", "dise_total_enrollment")]),
    , drop = FALSE
  ]
  counts <- aggregate(
    pool[c("dise_english_enrollment", "dise_total_enrollment")],
    list(state_07_key = pool$state_key, district_07_key = pool$district_key),
    sum,
    na.rm = TRUE
  )
  years <- aggregate(
    pool$academic_year,
    list(state_07_key = pool$state_key, district_07_key = pool$district_key),
    function(v) length(unique(v))
  )
  names(years)[[3]] <- "dise_baseline_years_observed"
  counts <- merge(counts, years, by = c("state_07_key", "district_07_key"), all.x = TRUE, sort = FALSE)
  counts$dise_emi_enrollment_share_total_0508_pooled <- ifelse(
    counts$dise_baseline_years_observed == 3L & counts$dise_total_enrollment > 0,
    100 * counts$dise_english_enrollment / counts$dise_total_enrollment,
    NA_real_
  )
  out <- merge(
    out,
    counts[c(
      "state_07_key", "district_07_key", "dise_baseline_years_observed",
      "dise_emi_enrollment_share_total_0508_pooled"
    )],
    by = c("state_07_key", "district_07_key"), all.x = TRUE, sort = FALSE
  )
  rownames(out) <- NULL
  out
}

attach_dise_treatments_to_panel <- function(panel, treatments) {
  geometry <- inherits(panel, "sf")
  x <- if (geometry) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  if (!all(c("state_07", "district_07") %in% names(x))) {
    stop("Analysis panel lacks harmonized 2007 district names required for DISE attachment.", call. = FALSE)
  }
  x$state_07_key <- canonicalize_state_name(x$state_07)
  x$district_07_key <- canonicalize_district_name(x$district_07)
  x$.dise_panel_row <- seq_len(nrow(x))
  treatment_cols <- setdiff(names(treatments), c("state_name_dise", "district_name_dise"))
  out <- merge(x, treatments[treatment_cols], by = c("state_07_key", "district_07_key"), all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(x)) stop("DISE treatment attachment changed the analysis-panel row count.", call. = FALSE)
  out <- out[order(out$.dise_panel_row), , drop = FALSE]
  out$.dise_panel_row <- NULL
  rownames(out) <- NULL
  if (geometry) sf::st_set_geometry(out, sf::st_geometry(panel)) else out
}

dise_construct_registry <- function() {
  data.frame(
    construct_id = c(
      "emi_total_0708", "emi_total_0508_pooled",
      "hindi_share_0708", "english_hindi_share_0708",
      "private_enrollment_share_0708", "private_school_share_0708"
    ),
    variable = c(
      "dise_emi_enrollment_share_total_0708",
      "dise_emi_enrollment_share_total_0508_pooled",
      "dise_hindi_enrollment_share_total_0708",
      "dise_english_share_english_hindi_0708",
      "dise_private_enrollment_share_0708",
      "dise_private_school_share_0708"
    ),
    analysis_scope = c(rep("structural_iv", 2), rep("relevance_only", 4)),
    label = c(
      "DISE 2007-08 English-medium enrollment / total enrollment",
      "DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment",
      "DISE 2007-08 Hindi-medium enrollment / total enrollment",
      "DISE 2007-08 English share among English + Hindi enrollment",
      "DISE 2007-08 private enrollment share",
      "DISE 2007-08 private-school share"
    ),
    stringsAsFactors = FALSE
  )
}
