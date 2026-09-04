# Pre-treatment Scheduled Tribe language-acquisition measures from Census 1991.

census_1991_st_hindi_belt_state_codes <- function() {
  # Historical equivalents of Shastry's Hindi-belt definition. Jharkhand,
  # Chhattisgarh, and Uttarakhand were still part of Bihar, Madhya Pradesh,
  # and Uttar Pradesh respectively in 1991.
  c("05", "08", "09", "13", "20", "21", "25", "28", "31")
}

build_census_1991_st16_validation <- function(st17, st16) {
  st17 <- safe_df(st17)
  st16 <- safe_df(st16)
  district_keys <- c("state_code_1991", "district_code_1991")
  language_keys <- c(district_keys, "mother_tongue")
  if (anyDuplicated(st17[language_keys]) || anyDuplicated(st16[language_keys])) {
    stop("Census 1991 ST language validation requires unique district mother-tongue rows.", call. = FALSE)
  }

  districts <- unique(st17[district_keys])
  out <- safe_bind_rows(lapply(seq_len(nrow(districts)), function(i) {
    key <- districts[i, , drop = FALSE]
    left <- st17[
      st17$state_code_1991 == key$state_code_1991 &
        st17$district_code_1991 == key$district_code_1991,
      c("mother_tongue", "mother_tongue_speakers"), drop = FALSE
    ]
    right <- st16[
      st16$state_code_1991 == key$state_code_1991 &
        st16$district_code_1991 == key$district_code_1991,
      c("mother_tongue", "mother_tongue_speakers_st16"), drop = FALSE
    ]
    if (!nrow(right)) {
      return(data.frame(
        state_code_1991 = key$state_code_1991,
        district_code_1991 = key$district_code_1991,
        validation_status = "st16_unavailable",
        n_language_cells = nrow(left), n_mismatched_cells = NA_integer_,
        absolute_speaker_difference = NA_real_, stringsAsFactors = FALSE
      ))
    }
    comparison <- merge(left, right, by = "mother_tongue", all = TRUE, sort = FALSE)
    comparison$mother_tongue_speakers[is.na(comparison$mother_tongue_speakers)] <- 0
    comparison$mother_tongue_speakers_st16[is.na(comparison$mother_tongue_speakers_st16)] <- 0
    delta <- comparison$mother_tongue_speakers - comparison$mother_tongue_speakers_st16
    mismatch <- delta != 0
    data.frame(
      state_code_1991 = key$state_code_1991,
      district_code_1991 = key$district_code_1991,
      validation_status = if (any(mismatch)) "speaker_counts_mismatch" else "exact",
      n_language_cells = nrow(comparison),
      n_mismatched_cells = sum(mismatch),
      absolute_speaker_difference = sum(abs(delta)),
      stringsAsFactors = FALSE
    )
  }))
  if (anyDuplicated(out[district_keys])) stop("Census 1991 ST validation produced duplicate districts.", call. = FALSE)
  out
}

census_1991_st_shastry_mapping <- function(
    atlas_registry = read_language_atlas_1991_languages(),
    concordance = read_shastry_language_distance(),
    adjudications = read_shastry_language_adjudications()) {
  resolved <- resolve_language_atlas_1991_shastry_mapping(
    atlas_registry, concordance, adjudications, scenario = "preferred"
  )
  long <- safe_bind_rows(list(
    data.frame(label = normalize_language_label(resolved$language_1991), shastry_distance = resolved$shastry_degree),
    data.frame(label = normalize_language_label(resolved$canonical_language), shastry_distance = resolved$shastry_degree)
  ))
  long <- long[!is.na(long$label) & nzchar(long$label) & is.finite(long$shastry_distance), , drop = FALSE]
  conflicts <- split(num(long$shastry_distance), long$label)
  conflict_names <- names(conflicts)[vapply(conflicts, function(x) length(unique(x)) > 1L, logical(1))]
  if (length(conflict_names)) stop("Historical language registry has conflicting Shastry mappings.", call. = FALSE)
  long <- long[!duplicated(long$label), , drop = FALSE]
  long
}

build_census_1991_st_language_panel <- function(
    st17, st16, shastry_mapping = census_1991_st_shastry_mapping()) {
  st17 <- safe_df(st17)
  validation <- build_census_1991_st16_validation(st17, st16)
  keys <- c("state_code_1991", "district_code_1991")
  panel <- merge(st17, validation, by = keys, all.x = TRUE, sort = FALSE)
  mapping <- safe_df(shastry_mapping)
  if (!all(c("label", "shastry_distance") %in% names(mapping)) || anyDuplicated(mapping$label)) {
    stop("Census 1991 ST Shastry mapping must contain unique label/distance rows.", call. = FALSE)
  }
  index <- match(normalize_language_label(panel$mother_tongue), normalize_language_label(mapping$label))
  panel$shastry_distance_1991 <- num(mapping$shastry_distance[index])
  panel$distance_mapping_status <- ifelse(is.finite(panel$shastry_distance_1991), "mapped", "unresolved")
  panel$st16_exact_validation <- panel$validation_status == "exact"
  panel$hindi_belt_1991 <- panel$state_code_1991 %in% census_1991_st_hindi_belt_state_codes()
  panel$preferred_st_language_sample <- panel$st16_exact_validation &
    panel$distance_mapping_status == "mapped" & panel$mother_tongue_speakers > 0
  panel
}
