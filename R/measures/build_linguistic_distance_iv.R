# This file is part of the EMI inequality research pipeline.

#' Read the auditable Shastry language-distance concordance
read_shastry_language_distance <- function(path = NULL) {
  if (is.null(path)) {
    root <- Sys.getenv("EMI_PROJECT_ROOT", unset = ".")
    path <- file.path(root, "data", "metadata", "shastry_language_distance.csv")
  }
  if (!file.exists(path)) stop("Missing Shastry language-distance concordance: ", path, call. = FALSE)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("canonical_language", "distance_from_hindi")
  if (!all(required %in% names(out))) stop("Language-distance concordance has an invalid schema.", call. = FALSE)
  out$canonical_language <- tools::toTitleCase(tolower(trimws(out$canonical_language)))
  out$distance_from_hindi <- num(out$distance_from_hindi)
  if (anyDuplicated(out$canonical_language)) stop("Language-distance concordance has duplicate language rows.", call. = FALSE)
  if (any(!is.finite(out$distance_from_hindi) | out$distance_from_hindi < 0 | out$distance_from_hindi > 5)) {
    stop("Language-distance concordance values must be finite integers from zero through five.", call. = FALSE)
  }
  out
}

normalize_language_label <- function(x) {
  tools::toTitleCase(tolower(trimws(plain_chr(x))))
}

census_mother_tongue_identity <- function(df) {
  canonical <- normalize_language_label(df$canonical_language)
  if (!"mother_tongue" %in% names(df)) return(canonical)
  mother <- normalize_language_label(df$mother_tongue)
  missing <- is.na(mother) | !nzchar(mother)
  mother[missing] <- canonical[missing]
  mother
}

linguistic_distance_degrees <- function(
  mother_tongue,
  canonical_language = mother_tongue,
  concordance = read_shastry_language_distance()
) {
  mother <- normalize_language_label(mother_tongue)
  canonical <- normalize_language_label(canonical_language)
  distance <- concordance$distance_from_hindi[
    match(mother, concordance$canonical_language)
  ]

  # C-16's Hindi/Urdu language groups contain distinct mother tongues. A child
  # row must not inherit zero distance solely from that group subtotal.
  protected_zero_group <- canonical %in% c("Hindi", "Urdu") &
    !is.na(mother) & nzchar(mother) & mother != canonical
  fallback <- !is.finite(distance) & !protected_zero_group
  distance[fallback] <- concordance$distance_from_hindi[
    match(canonical[fallback], concordance$canonical_language)
  ]
  distance
}

validate_supplied_linguistic_distances <- function(x) {
  value <- num(x)
  if (any(is.finite(value) & (value < 0 | value > 5))) {
    stop("ling_degrees must be in the 0-5 range.", call. = FALSE)
  }
  invisible(value)
}

#' Build the complete suite of Census 2001 linguistic-distance constructions
build_linguistic_distance_iv <- function(
  census_2001_languages,
  cfg = list(),
  glottolog = NULL,
  glottolog_crosswalk = NULL,
  historical_linguistics = NULL,
  shastry_adjudications = read_shastry_language_adjudications()
) {
  df <- std(safe_df(census_2001_languages), 2001L)
  if ("ling_degrees" %in% names(df)) validate_supplied_linguistic_distances(df$ling_degrees)

  required <- c("state_std", "district_std", "spkr_tot", "canonical_language")
  if (!nrow(df) || !all(required %in% names(df))) {
    return(linguistic_distance_out_of_pipeline("No real linguistic-distance column or full cleaned C-16 distribution was available."))
  }

  language <- census_mother_tongue_identity(df)
  if (!"ling_degrees" %in% names(df)) {
    df$ling_degrees <- resolve_shastry_language_degrees(df, adjudications = shastry_adjudications)
  }
  if (!is.null(glottolog) && !is.null(glottolog_crosswalk)) {
    df <- attach_glottolog_language_distance(df, glottolog, glottolog_crosswalk)
    non_ie_extension <- !is.finite(num(df$ling_degrees)) &
      language != "English" &
      !is.na(df$glottolog_family_id) &
      nzchar(df$glottolog_family_id) &
      df$glottolog_family_id != "indo1319"
    df$ling_degrees[non_ie_extension] <- 5
  } else if (!"glottolog_edge_distance" %in% names(df)) {
    df$glottolog_edge_distance <- NA_real_
  }

  if (!is.null(historical_linguistics)) {
    df <- attach_dyen_language_distance(df, historical_linguistics)
  } else if (!"dyen_noncognate_pct" %in% names(df)) {
    df$dyen_noncognate_pct <- NA_real_
  }

  is_english <- language == "English"
  df$distance_mapping_status <- ifelse(
    is_english,
    "special_english",
    ifelse(is.finite(num(df$ling_degrees)), "mapped", "unmapped")
  )

  split_i <- split(seq_len(nrow(df)), interaction(df[c("state_std", "district_std")], drop = TRUE))
  out <- safe_bind_rows(lapply(split_i, function(i) build_district_language_constructions(df[i, , drop = FALSE])))
  if (!nrow(out)) return(linguistic_distance_out_of_pipeline("No district linguistic-distance constructions could be computed."))

  names_df <- unique(df[intersect(c(
    "state_std", "district_std", "state", "district", "state_code", "district_code", "district_name"
  ), names(df))])
  out <- merge(out, names_df, by = c("state_std", "district_std"), all.x = TRUE, sort = FALSE)
  state_code <- first_col(out, c("state_code", "state"))
  if (!is.null(state_code)) out$state_01 <- census_2001_state_name(out[[state_code]])
  if ("district_name" %in% names(out)) out$district_01 <- out$district_name
  if (!"district_01" %in% names(out) && "district" %in% names(out)) out$district_01 <- out$district
  out$district_panel_id <- make_district_key(out$state_std, out$district_std, 2001L)
  validate_linguistic_distance_ranges(out)
}

build_district_language_constructions <- function(df) {
  speakers <- num(df$spkr_tot)
  distance <- num(df$ling_degrees)
  glottolog_distance <- num(df$glottolog_edge_distance %||% rep(NA_real_, nrow(df)))
  dyen_distance <- num(df$dyen_noncognate_pct %||% rep(NA_real_, nrow(df)))
  language <- census_mother_tongue_identity(df)
  valid_speakers <- is.finite(speakers) & speakers >= 0
  total <- sum(speakers[valid_speakers], na.rm = TRUE)
  english <- valid_speakers & language == "English"
  mapped <- valid_speakers & is.finite(distance) & !english
  unmapped <- valid_speakers & !is.finite(distance) & !english
  mapped_total <- sum(speakers[mapped], na.rm = TRUE)
  nonzero <- mapped & distance > 0

  share <- function(condition, denominator = total) {
    if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
    100 * sum(speakers[valid_speakers & condition], na.rm = TRUE) / denominator
  }
  weighted_value <- function(index, value) {
    denom <- sum(speakers[index], na.rm = TRUE)
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sum(speakers[index] * value[index], na.rm = TRUE) / denom
  }

  top3 <- select_top_mother_tongues(df[mapped, , drop = FALSE], 3L)
  top3_distance <- if (nrow(top3)) wmean(top3$ling_degrees, top3$spkr_tot) else NA_real_
  top3_coverage <- if (is.finite(total) && total > 0 && nrow(top3)) 100 * sum(num(top3$spkr_tot), na.rm = TRUE) / total else NA_real_

  out <- df[1, c("state_std", "district_std"), drop = FALSE]
  out$ling_distance_nonzero_mean <- weighted_value(nonzero, distance)
  out$ling_share_distance_ge3 <- share(mapped & distance >= 3)
  for (degree in 0:5) {
    out[[paste0("ling_share_distance_", degree)]] <- share(mapped & distance == degree)
    out[[paste0("ling_mapped_share_distance_", degree)]] <- share(
      mapped & distance == degree,
      denominator = mapped_total
    )
  }
  out$hindi_share <- share(language == "Hindi")
  out$urdu_share <- share(language == "Urdu")
  out$hindi_urdu_share <- share(language %in% c("Hindi", "Urdu"))
  out$native_english_share <- share(english)

  glottolog_reference <- valid_speakers & language %in% c("Hindi", "Urdu", "English")
  glottolog_eligible <- valid_speakers & !glottolog_reference
  glottolog_mapped <- glottolog_eligible & is.finite(glottolog_distance)
  glottolog_unmapped <- glottolog_eligible & !is.finite(glottolog_distance)
  glottolog_eligible_total <- sum(speakers[glottolog_eligible], na.rm = TRUE)
  out$ling_distance_glottolog_nonhindi_mean <- weighted_value(glottolog_mapped, glottolog_distance)
  out$ling_glottolog_mapped_speaker_share <- if (is.finite(glottolog_eligible_total) && glottolog_eligible_total > 0) {
    100 * sum(speakers[glottolog_mapped], na.rm = TRUE) / glottolog_eligible_total
  } else {
    NA_real_
  }
  out$ling_glottolog_unmapped_speaker_share <- if (is.finite(glottolog_eligible_total) && glottolog_eligible_total > 0) {
    100 * sum(speakers[glottolog_unmapped], na.rm = TRUE) / glottolog_eligible_total
  } else {
    NA_real_
  }

  dyen_reference <- valid_speakers & language %in% c("Hindi", "Urdu", "English")
  dyen_eligible <- valid_speakers & !dyen_reference
  dyen_mapped <- dyen_eligible & is.finite(dyen_distance)
  dyen_unmapped <- dyen_eligible & !is.finite(dyen_distance)
  dyen_eligible_total <- sum(speakers[dyen_eligible], na.rm = TRUE)
  out$ling_distance_dyen_noncognate_pct <- weighted_value(dyen_mapped, dyen_distance)
  out$ling_dyen_mapped_speaker_share <- if (is.finite(dyen_eligible_total) && dyen_eligible_total > 0) {
    100 * sum(speakers[dyen_mapped], na.rm = TRUE) / dyen_eligible_total
  } else {
    NA_real_
  }
  out$ling_dyen_unmapped_speaker_share <- if (is.finite(dyen_eligible_total) && dyen_eligible_total > 0) {
    100 * sum(speakers[dyen_unmapped], na.rm = TRUE) / dyen_eligible_total
  } else {
    NA_real_
  }

  out$ling_distance_top3_legacy <- top3_distance
  out$ling_top3_speaker_coverage <- top3_coverage
  out$ling_mapped_speaker_share <- if (is.finite(total) && total > 0) 100 * mapped_total / total else NA_real_
  out$ling_unmapped_speaker_share <- share(unmapped)
  out$ling_total_speakers <- total

  # Historical compatibility aliases remain explicit for archived comparisons and benchmarks.
  out$wavg_ling_degrees <- out$ling_distance_top3_legacy
  out
}

linguistic_distance_excluded_instruments <- function(denominator = c("all", "mapped")) {
  denominator <- match.arg(denominator)
  prefix <- if (identical(denominator, "mapped")) "ling_mapped_share_distance_" else "ling_share_distance_"
  paste0(prefix, 1:5)
}

linguistic_distance_language_controls <- function() {
  c("hindi_share", "urdu_share", "hindi_urdu_share", "native_english_share")
}

validate_linguistic_distance_ranges <- function(df) {
  distance_cols <- intersect(c(
    "ling_distance_nonzero_mean", "ling_distance_top3_legacy", "wavg_ling_degrees",
    "ling_distance_glottolog_nonhindi_mean", "ling_distance_dyen_noncognate_pct"
  ), names(df))
  for (nm in setdiff(distance_cols, c(
    "ling_distance_glottolog_nonhindi_mean", "ling_distance_dyen_noncognate_pct"
  ))) {
    value <- num(df[[nm]])
    if (any(is.finite(value) & (value < 0 | value > 5))) stop(nm, " must be in the 0-5 range.", call. = FALSE)
  }
  if ("ling_distance_glottolog_nonhindi_mean" %in% names(df)) {
    value <- num(df$ling_distance_glottolog_nonhindi_mean)
    if (any(is.finite(value) & value < 0)) stop("Glottolog distance must be non-negative.", call. = FALSE)
  }
  if ("ling_distance_dyen_noncognate_pct" %in% names(df)) {
    value <- num(df$ling_distance_dyen_noncognate_pct)
    if (any(is.finite(value) & (value < 0 | value > 100))) {
      stop("Dyen noncognate distance must be in the 0-100 range.", call. = FALSE)
    }
  }
  share_cols <- grep("(^ling_(mapped_)?share_distance_|_share$|speaker_coverage$)", names(df), value = TRUE)
  for (nm in share_cols) {
    value <- num(df[[nm]])
    if (any(is.finite(value) & (value < 0 | value > 100))) stop(nm, " must be in the 0-100 range.", call. = FALSE)
  }
  df
}

linguistic_distance_out_of_pipeline <- function(reason) {
  data.frame(status = "out_of_active_pipeline", reason = reason, stringsAsFactors = FALSE)
}
