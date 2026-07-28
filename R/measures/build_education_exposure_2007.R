# This file is part of the EMI inequality research pipeline.

education_exposure_age_specs <- function() {
  data.frame(
    specification = c("primary_5_19", "supplementary_6_17", "supplementary_6_14"),
    min_age = c(5L, 6L, 6L),
    max_age = c(19L, 17L, 14L),
    suffix = c("0708", "0708_age6_17", "0708_age6_14"),
    stringsAsFactors = FALSE
  )
}

#' Build enrollment and EMI exposure margins from one common child universe
build_education_exposure_2007 <- function(selection_data) {
  df <- safe_df(selection_data)
  needed <- c("district_code_0708", "AGE", "enrolled", "weight")
  if (!nrow(df) || !all(needed %in% names(df))) return(data.frame())

  enrolled <- tolower(plain_chr(df$enrolled)) %in% c("yes", "1", "true", "enrolled")
  medium_col <- first_col(df, c("MEDIUM_INSTRUCTION", "medium_instruction", "medium"))
  medium_known <- rep(FALSE, nrow(df))
  english_medium <- rep(FALSE, nrow(df))
  if (!is.null(medium_col)) {
    medium_value <- num(df[[medium_col]])
    medium_code <- ifelse(is.finite(medium_value), sprintf("%02d", as.integer(medium_value)), NA_character_)
    medium_known <- enrolled & !is.na(medium_code)
    english_medium <- medium_known & medium_code == "02"
  }

  specs <- education_exposure_age_specs()
  pieces <- lapply(seq_len(nrow(specs)), function(j) {
    keep <- is.finite(num(df$AGE)) & num(df$AGE) >= specs$min_age[j] & num(df$AGE) <= specs$max_age[j]
    aggregate_education_exposure_2007(
      df[keep, , drop = FALSE], enrolled[keep], medium_known[keep], english_medium[keep], specs$suffix[j]
    )
  })
  pieces <- Filter(function(x) nrow(safe_df(x)) > 0L, pieces)
  if (!length(pieces)) return(data.frame())
  Reduce(merge_education_exposure_2007, pieces)
}

aggregate_education_exposure_2007 <- function(df, enrolled, medium_known, english_medium, suffix) {
  df <- standardize_nss_2007_district_code(df)
  keys <- district_group_vars_2007(df)
  if (!nrow(df) || !length(keys)) return(data.frame())
  split_i <- split(seq_len(nrow(df)), interaction(df[keys], drop = TRUE))
  safe_bind_rows(lapply(split_i, function(i) {
    weight <- num(df$weight[i])
    valid <- is.finite(weight) & weight > 0
    eligible_weight <- sum(weight[valid], na.rm = TRUE)
    enrolled_weight <- sum(weight[valid & enrolled[i]], na.rm = TRUE)
    known_medium_weight <- sum(weight[valid & medium_known[i]], na.rm = TRUE)
    emi_weight <- sum(weight[valid & english_medium[i]], na.rm = TRUE)

    z <- df[i[[1]], keys, drop = FALSE]
    z[[paste0("eligible_child_weight_", suffix)]] <- eligible_weight
    z[[paste0("enrolled_child_weight_", suffix)]] <- enrolled_weight
    z[[paste0("known_medium_enrolled_weight_", suffix)]] <- known_medium_weight
    z[[paste0("emi_enrolled_child_weight_", suffix)]] <- emi_weight
    z[[paste0("enrollment_rate_", suffix)]] <- safe_percent(enrolled_weight, eligible_weight)
    z[[paste0("emi_share_enrolled_", suffix)]] <- safe_percent(emi_weight, known_medium_weight)
    z[[paste0("emi_exposure_all_children_", suffix)]] <- safe_percent(emi_weight, eligible_weight)
    z[[paste0("unknown_medium_share_enrolled_", suffix)]] <- safe_percent(enrolled_weight - known_medium_weight, enrolled_weight)
    z
  }))
}

safe_percent <- function(numerator, denominator) {
  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= 0) return(NA_real_)
  100 * numerator / denominator
}

validate_education_exposure_identity <- function(df, suffix = "0708", tolerance = 1e-8) {
  enrollment <- num(df[[paste0("enrollment_rate_", suffix)]]) / 100
  intensive <- num(df[[paste0("emi_share_enrolled_", suffix)]]) / 100
  exposure <- num(df[[paste0("emi_exposure_all_children_", suffix)]]) / 100
  unknown <- num(df[[paste0("unknown_medium_share_enrolled_", suffix)]])
  exact_support <- is.finite(enrollment) & is.finite(intensive) & is.finite(exposure) & (!is.finite(unknown) | abs(unknown) <= tolerance)
  if (any(exact_support & abs(exposure - enrollment * intensive) > tolerance)) {
    stop("EMI exposure must equal enrollment rate times EMI share among enrolled when medium is fully observed.", call. = FALSE)
  }
  df
}

merge_education_exposure_2007 <- function(x, y) {
  keys <- intersect(c("district_code_0708", "state_std", "district_std"), intersect(names(x), names(y)))
  if (!length(keys)) stop("Education-exposure age specifications lack common district keys.", call. = FALSE)
  merge(x, y, by = keys, all = TRUE, sort = FALSE)
}
