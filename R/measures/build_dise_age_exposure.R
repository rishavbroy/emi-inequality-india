# Census-anchored elementary-age denominators for longitudinal DISE exposure.

build_census_2011_to_2001_age_bridge <- function(district_transition_2001_2011) {
  build_complete_deterministic_transition_2011_to_2001(district_transition_2001_2011)
}

harmonize_census_2011_age_6_13_to_2001 <- function(age_2011, district_transition_2001_2011) {
  x <- safe_df(age_2011)
  x$source_unit_2011 <- paste0(
    "pc2011__", normalize_census_code(x$state_code, 2L), "__",
    normalize_census_code(x$district_code, 3L)
  )
  bridge <- build_census_2011_to_2001_age_bridge(district_transition_2001_2011)
  x <- merge(x, bridge, by = "source_unit_2011", all.x = TRUE, sort = FALSE)
  mapped <- x[!is.na(x$target_unit_2001) & nzchar(x$target_unit_2001), , drop = FALSE]
  if (!nrow(mapped)) return(data.frame())
  groups <- split(seq_len(nrow(mapped)), mapped$target_unit_2001)
  out <- safe_bind_rows(lapply(groups, function(index) {
    part <- mapped[index, , drop = FALSE]
    data.frame(
      target_unit_2001 = part$target_unit_2001[[1]],
      census_age_6_13_population_2011 = sum_complete_counts(part$census_age_6_13_population),
      census_2011_source_district_count = nrow(part),
      census_2011_source_districts = paste(sort(unique(part$district_name)), collapse = ";"),
      census_2011_parent_reconstruction_complete = all(
        part$census_2011_parent_reconstruction_complete %in% TRUE
      ),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

build_census_age_6_13_anchors <- function(
  age_2001,
  age_2011,
  district_transition_2001_2011
) {
  x2001 <- safe_df(age_2001)
  x2001$target_unit_2001 <- paste0(
    "pc2001__", normalize_census_code(x2001$state_code, 2L), "__",
    normalize_census_code(x2001$district_code, 2L)
  )
  x2001 <- x2001[c("target_unit_2001", "district_name", "census_age_6_13_population")]
  names(x2001) <- c(
    "target_unit_2001", "census_2001_district_name", "census_age_6_13_population_2001"
  )
  if (anyDuplicated(x2001$target_unit_2001)) {
    stop("Census-2001 C-13 anchor is not unique by district.", call. = FALSE)
  }
  x2011 <- harmonize_census_2011_age_6_13_to_2001(
    age_2011,
    district_transition_2001_2011
  )
  if (!nrow(x2011)) {
    x2011 <- data.frame(
      target_unit_2001 = character(),
      census_age_6_13_population_2011 = numeric(),
      census_2011_source_district_count = integer(),
      census_2011_source_districts = character(),
      census_2011_parent_reconstruction_complete = logical(),
      stringsAsFactors = FALSE
    )
  }
  out <- merge(x2001, x2011, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out$census_age_6_13_anchor_status <- ifelse(
    is.finite(out$census_age_6_13_population_2001) & out$census_age_6_13_population_2001 > 0 &
      is.finite(out$census_age_6_13_population_2011) & out$census_age_6_13_population_2011 > 0 &
      out$census_2011_parent_reconstruction_complete %in% TRUE,
    "two_census_anchors",
    "missing_complete_2011_parent_reconstruction"
  )
  out$census_age_6_13_annual_log_growth_2001_2011 <- ifelse(
    out$census_age_6_13_anchor_status == "two_census_anchors",
    log(out$census_age_6_13_population_2011 / out$census_age_6_13_population_2001) / 10,
    NA_real_
  )
  rownames(out) <- NULL
  out
}

academic_year_midpoint <- function(academic_year) {
  start <- suppressWarnings(as.integer(substr(plain_chr(academic_year), 1L, 4L)))
  ifelse(is.finite(start), start + 0.5, NA_real_)
}

project_census_age_6_13_population <- function(anchors, academic_years) {
  anchors <- safe_df(anchors)
  years <- data.frame(
    academic_year = sort(unique(plain_chr(academic_years))),
    academic_year_midpoint = academic_year_midpoint(sort(unique(plain_chr(academic_years)))),
    stringsAsFactors = FALSE
  )
  years <- years[is.finite(years$academic_year_midpoint), , drop = FALSE]
  anchors$.join <- 1L
  years$.join <- 1L
  out <- merge(anchors, years, by = ".join", all = FALSE, sort = FALSE)
  out$.join <- NULL
  usable <- out$census_age_6_13_anchor_status == "two_census_anchors" &
    is.finite(out$census_age_6_13_annual_log_growth_2001_2011)
  out$census_age_6_13_population <- ifelse(
    usable,
    out$census_age_6_13_population_2001 * exp(
      out$census_age_6_13_annual_log_growth_2001_2011 *
        (out$academic_year_midpoint - 2001)
    ),
    NA_real_
  )
  out$census_age_6_13_population_method <- ifelse(
    !usable,
    "unavailable_without_two_district_anchors",
    ifelse(
      out$academic_year_midpoint <= 2011,
      "log_linear_interpolation_2001_2011",
      "log_linear_extrapolation_post_2011"
    )
  )
  out <- out[order(out$target_unit_2001, out$academic_year_midpoint), , drop = FALSE]
  rownames(out) <- NULL
  out
}

attach_dise_age_6_13_exposure <- function(dise_panel, population_panel) {
  x <- safe_df(dise_panel)
  population <- safe_df(population_panel)
  if (anyDuplicated(paste(population$target_unit_2001, population$academic_year, sep = "|"))) {
    stop("Projected Census age-6-13 population is not unique by district-year.", call. = FALSE)
  }
  x$.dise_age_row <- seq_len(nrow(x))
  keep <- c(
    "target_unit_2001", "academic_year", "census_age_6_13_population",
    "census_age_6_13_population_2001", "census_age_6_13_population_2011",
    "census_age_6_13_annual_log_growth_2001_2011",
    "census_age_6_13_population_method"
  )
  out <- merge(x, population[keep], by = c("target_unit_2001", "academic_year"), all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(x)) {
    stop("Age-population attachment changed the DISE district-year row count.", call. = FALSE)
  }
  out <- out[order(out$.dise_age_row), , drop = FALSE]
  out$.dise_age_row <- NULL
  denom <- num(out$census_age_6_13_population)
  english <- num(out$dise_english_enrollment)
  total <- num(out$dise_total_enrollment)
  out$dise_emi_gross_enrollment_ratio_age_6_13 <- ifelse(
    is.finite(english) & english >= 0 & is.finite(denom) & denom > 0,
    100 * english / denom,
    NA_real_
  )
  out$dise_elementary_gross_enrollment_ratio_age_6_13 <- ifelse(
    is.finite(total) & total >= 0 & is.finite(denom) & denom > 0,
    100 * total / denom,
    NA_real_
  )
  rownames(out) <- NULL
  out
}
