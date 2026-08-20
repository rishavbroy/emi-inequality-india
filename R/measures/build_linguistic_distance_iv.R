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

linguistic_distance_degrees <- function(mother_tongue, concordance = read_shastry_language_distance()) {
  key <- tools::toTitleCase(tolower(trimws(plain_chr(mother_tongue))))
  concordance$distance_from_hindi[match(key, concordance$canonical_language)]
}

validate_supplied_linguistic_distances <- function(x) {
  value <- num(x)
  if (any(is.finite(value) & (value < 0 | value > 5))) {
    stop("ling_degrees must be in the 0-5 range.", call. = FALSE)
  }
  invisible(value)
}

#' Build the complete suite of Census 2001 linguistic-distance constructions
build_linguistic_distance_iv <- function(census_2001_languages, cfg = list()) {
  df <- std(safe_df(census_2001_languages), 2001L)
  if ("ling_degrees" %in% names(df)) validate_supplied_linguistic_distances(df$ling_degrees)

  required <- c("state_std", "district_std", "spkr_tot", "canonical_language")
  if (!nrow(df) || !all(required %in% names(df))) {
    return(linguistic_distance_out_of_pipeline("No real linguistic-distance column or full cleaned C-16 distribution was available."))
  }

  if (!"ling_degrees" %in% names(df)) {
    df$ling_degrees <- linguistic_distance_degrees(df$canonical_language)
  }
  language <- tools::toTitleCase(tolower(trimws(plain_chr(df$canonical_language))))
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
  language <- tools::toTitleCase(tolower(trimws(plain_chr(df$canonical_language))))
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
  weighted_distance <- function(index) {
    denom <- sum(speakers[index], na.rm = TRUE)
    if (!is.finite(denom) || denom <= 0) return(NA_real_)
    sum(speakers[index] * distance[index], na.rm = TRUE) / denom
  }

  top3 <- select_top_mother_tongues(df[mapped, , drop = FALSE], 3L)
  top3_distance <- if (nrow(top3)) wmean(top3$ling_degrees, top3$spkr_tot) else NA_real_
  top3_coverage <- if (is.finite(total) && total > 0 && nrow(top3)) 100 * sum(num(top3$spkr_tot), na.rm = TRUE) / total else NA_real_

  out <- df[1, c("state_std", "district_std"), drop = FALSE]
  out$ling_distance_nonzero_mean <- weighted_distance(nonzero)
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
    "ling_distance_nonzero_mean", "ling_distance_top3_legacy", "wavg_ling_degrees"
  ), names(df))
  for (nm in distance_cols) {
    value <- num(df[[nm]])
    if (any(is.finite(value) & (value < 0 | value > 5))) stop(nm, " must be in the 0-5 range.", call. = FALSE)
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
