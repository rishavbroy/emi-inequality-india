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

dise_construct_registry <- function() {
  data.frame(
    construct_id = c(
      "emi_total_0708", "emi_total_0508_pooled",
      "emi_age6_13_gross_0708", "emi_age6_13_gross_0508_pooled",
      "hindi_share_0708", "english_hindi_share_0708",
      "private_enrollment_share_0708", "private_school_share_0708"
    ),
    variable = c(
      "dise_emi_enrollment_share_total_0708",
      "dise_emi_enrollment_share_total_0508_pooled",
      "dise_emi_gross_enrollment_ratio_age_6_13_0708",
      "dise_emi_gross_enrollment_ratio_age_6_13_0508_pooled",
      "dise_hindi_enrollment_share_total_0708",
      "dise_english_share_english_hindi_0708",
      "dise_private_enrollment_share_0708",
      "dise_private_school_share_0708"
    ),
    analysis_scope = c(rep("structural_iv", 4), rep("relevance_only", 4)),
    domain = c(rep("medium", 6), rep("management", 2)),
    margin = c(
      "enrollment_composition", "pooled_enrollment_composition",
      "population_scaled_enrollment", "pooled_population_scaled_enrollment",
      "enrollment_composition", "english_hindi_composition",
      "enrollment_composition", "school_stock_composition"
    ),
    source_side = c(rep("administrative_equilibrium", 7), "administrative_supply"),
    paper_role = c(
      rep("formal_english_exposure", 4),
      "language_substitution", "language_substitution",
      "institution_choice_context", "institutional_environment"
    ),
    label = c(
      "DISE 2007-08 English-medium enrollment / total enrollment",
      "DISE pooled 2005-06 to 2007-08 English-medium enrollment / total enrollment",
      "DISE 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator",
      "DISE pooled 2005-06 to 2007-08 English-medium gross enrollment ratio, ages 6-13 denominator",
      "DISE 2007-08 Hindi-medium enrollment / total enrollment",
      "DISE 2007-08 English share among English + Hindi enrollment",
      "DISE 2007-08 private enrollment share",
      "DISE 2007-08 private-school share"
    ),
    stringsAsFactors = FALSE
  )
}
