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

#' Classify NSS-64 school management without imputing unknown institutions
#'
#' NSS Schedule 25.2 Block 5 codes institution type as government (1), local
#' body (2), private aided (3), private unaided (4), and not known (5). Public
#' combines government and local-body institutions; unknown/missing codes remain
#' unknown rather than being assigned to either sector.
classify_nss_2007_school_management <- function(x) {
  code <- suppressWarnings(as.integer(num(x)))
  data.frame(
    management_known = code %in% 1:4,
    public = code %in% 1:2,
    private = code %in% 3:4,
    private_aided = code %in% 3L,
    private_unaided = code %in% 4L,
    stringsAsFactors = FALSE
  )
}

#' Build enrollment, medium, and institution-choice margins from one child universe
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

  institution_col <- first_col(df, c("TYPE_OF_INSTT", "type_of_instt", "institution_type"))
  management <- if (is.null(institution_col)) {
    classify_nss_2007_school_management(rep(NA_real_, nrow(df)))
  } else {
    classify_nss_2007_school_management(df[[institution_col]])
  }
  management[] <- lapply(management, function(x) enrolled & x)

  specs <- education_exposure_age_specs()
  pieces <- lapply(seq_len(nrow(specs)), function(j) {
    keep <- is.finite(num(df$AGE)) & num(df$AGE) >= specs$min_age[j] & num(df$AGE) <= specs$max_age[j]
    aggregate_education_exposure_2007(
      df[keep, , drop = FALSE], enrolled[keep], medium_known[keep], english_medium[keep],
      management[keep, , drop = FALSE], specs$suffix[j]
    )
  })
  pieces <- Filter(function(x) nrow(safe_df(x)) > 0L, pieces)
  if (!length(pieces)) return(data.frame())
  Reduce(merge_education_exposure_2007, pieces)
}

aggregate_education_exposure_2007 <- function(
    df, enrolled, medium_known, english_medium, management, suffix) {
  df <- standardize_nss_2007_district_code(df)
  keys <- district_group_vars_2007(df)
  if (!nrow(df) || !length(keys)) return(data.frame())
  if (nrow(management) != nrow(df)) stop("School-management indicators must align with child rows.", call. = FALSE)

  split_i <- split(seq_len(nrow(df)), interaction(df[keys], drop = TRUE))
  safe_bind_rows(lapply(split_i, function(i) {
    weight <- num(df$weight[i])
    valid <- is.finite(weight) & weight > 0
    weighted_sum <- function(flag) sum(weight[valid & flag[i]], na.rm = TRUE)

    eligible_weight <- sum(weight[valid], na.rm = TRUE)
    enrolled_weight <- weighted_sum(enrolled)
    known_medium_weight <- weighted_sum(medium_known)
    emi_weight <- weighted_sum(english_medium)
    known_management_weight <- weighted_sum(management$management_known)
    private_weight <- weighted_sum(management$private)

    classified <- enrolled & medium_known & management$management_known
    public_emi <- classified & management$public & english_medium
    public_nonemi <- classified & management$public & !english_medium
    private_emi <- classified & management$private & english_medium
    private_nonemi <- classified & management$private & !english_medium

    classified_weight <- weighted_sum(classified)
    public_known_medium_weight <- weighted_sum(management$public & medium_known)
    private_known_medium_weight <- weighted_sum(management$private & medium_known)
    private_aided_known_medium_weight <- weighted_sum(management$private_aided & medium_known)
    private_unaided_known_medium_weight <- weighted_sum(management$private_unaided & medium_known)
    public_emi_weight <- weighted_sum(public_emi)
    private_emi_weight <- weighted_sum(private_emi)
    private_aided_emi_weight <- weighted_sum(management$private_aided & english_medium)
    private_unaided_emi_weight <- weighted_sum(management$private_unaided & english_medium)

    cell_weights <- c(
      public_emi = public_emi_weight,
      public_nonemi = weighted_sum(public_nonemi),
      private_emi = private_emi_weight,
      private_nonemi = weighted_sum(private_nonemi)
    )

    z <- df[i[[1]], keys, drop = FALSE]
    z[[paste0("eligible_child_weight_", suffix)]] <- eligible_weight
    z[[paste0("enrolled_child_weight_", suffix)]] <- enrolled_weight
    z[[paste0("known_medium_enrolled_weight_", suffix)]] <- known_medium_weight
    z[[paste0("emi_enrolled_child_weight_", suffix)]] <- emi_weight
    z[[paste0("known_management_enrolled_weight_", suffix)]] <- known_management_weight
    z[[paste0("classified_schooling_weight_", suffix)]] <- classified_weight
    z[[paste0("enrollment_rate_", suffix)]] <- safe_percent(enrolled_weight, eligible_weight)
    z[[paste0("emi_share_enrolled_", suffix)]] <- safe_percent(emi_weight, known_medium_weight)
    z[[paste0("emi_exposure_all_children_", suffix)]] <- safe_percent(emi_weight, eligible_weight)
    z[[paste0("unknown_medium_share_enrolled_", suffix)]] <- safe_percent(enrolled_weight - known_medium_weight, enrolled_weight)
    z[[paste0("private_share_enrolled_", suffix)]] <- safe_percent(private_weight, known_management_weight)
    z[[paste0("unknown_management_share_enrolled_", suffix)]] <- safe_percent(enrolled_weight - known_management_weight, enrolled_weight)
    z[[paste0("emi_share_enrolled_public_", suffix)]] <- safe_percent(public_emi_weight, public_known_medium_weight)
    z[[paste0("emi_share_enrolled_private_", suffix)]] <- safe_percent(private_emi_weight, private_known_medium_weight)
    z[[paste0("emi_share_enrolled_private_aided_", suffix)]] <- safe_percent(private_aided_emi_weight, private_aided_known_medium_weight)
    z[[paste0("emi_share_enrolled_private_unaided_", suffix)]] <- safe_percent(private_unaided_emi_weight, private_unaided_known_medium_weight)
    z[[paste0("public_emi_exposure_all_children_", suffix)]] <- safe_percent(cell_weights[["public_emi"]], eligible_weight)
    z[[paste0("public_nonemi_exposure_all_children_", suffix)]] <- safe_percent(cell_weights[["public_nonemi"]], eligible_weight)
    z[[paste0("private_emi_exposure_all_children_", suffix)]] <- safe_percent(cell_weights[["private_emi"]], eligible_weight)
    z[[paste0("private_nonemi_exposure_all_children_", suffix)]] <- safe_percent(cell_weights[["private_nonemi"]], eligible_weight)
    z[[paste0("unknown_school_classification_share_all_children_", suffix)]] <- safe_percent(enrolled_weight - classified_weight, eligible_weight)
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

  cell_names <- paste0(
    c("public_emi_exposure_all_children_", "public_nonemi_exposure_all_children_",
      "private_emi_exposure_all_children_", "private_nonemi_exposure_all_children_"), suffix
  )
  unknown_name <- paste0("unknown_school_classification_share_all_children_", suffix)
  if (all(c(cell_names, unknown_name) %in% names(df))) {
    cells <- rowSums(as.data.frame(lapply(df[cell_names], num)), na.rm = FALSE) / 100
    unknown_classification <- num(df[[unknown_name]]) / 100
    complete <- is.finite(enrollment) & is.finite(cells) & is.finite(unknown_classification)
    if (any(complete & abs(enrollment - cells - unknown_classification) > tolerance)) {
      stop("NSS school-sector/medium cells plus unknown classification must exhaust enrolled children.", call. = FALSE)
    }
  }
  df
}

merge_education_exposure_2007 <- function(x, y) {
  keys <- intersect(c("district_code_0708", "state_std", "district_std"), intersect(names(x), names(y)))
  if (!length(keys)) stop("Education-exposure age specifications lack common district keys.", call. = FALSE)
  merge(x, y, by = keys, all = TRUE, sort = FALSE)
}

#' Stable NSS-64 social-group labels used by the schooling-access diagnostic
nss_2007_schooling_social_groups <- function() {
  c("Scheduled Tribe", "Scheduled Caste", "Other Backward Class", "Other")
}

#' Reuse the canonical schooling-margin builder within each NSS social group
#'
#' Social group is observed on the same child record as enrollment and, after
#' the Block-4/Block-5 child join, school medium/management. Splitting the child
#' universe before calling `build_education_exposure_2007()` guarantees that
#' group-specific margins retain exactly the same age windows, unknown-category
#' handling, weights, and denominator contracts as the aggregate treatment.
build_education_exposure_2007_by_social_group <- function(selection_data) {
  df <- safe_df(selection_data)
  if (!nrow(df) || !"SOCIAL_GROUP" %in% names(df)) return(data.frame())

  group <- plain_chr(df$SOCIAL_GROUP)
  levels <- nss_2007_schooling_social_groups()
  safe_bind_rows(lapply(levels, function(label) {
    keep <- !is.na(group) & group == label
    if (!any(keep)) return(data.frame())
    out <- build_education_exposure_2007(df[keep, , drop = FALSE])
    if (!nrow(out)) return(data.frame())
    out$social_group <- label
    out
  }))
}
