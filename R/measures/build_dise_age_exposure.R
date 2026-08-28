# Census-anchored elementary-age denominators for longitudinal DISE exposure.

build_census_2011_to_2001_age_bridge <- function(district_transition_2001_2011) {
  transition <- safe_df(district_transition_2001_2011)
  required <- c(
    "state_code_2011", "district_code_2011", "state_code_2001", "district_code_2001",
    "population_share_to_2001", "area_share_to_2001", "shrid_coverage", "mapping_class"
  )
  missing <- setdiff(required, names(transition))
  if (length(missing)) {
    stop("District transition table lacks C-13 bridge fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  transition$source_unit_2011 <- paste0(
    "pc2011__",
    normalize_census_code(transition$state_code_2011, 2L), "__",
    normalize_census_code(transition$district_code_2011, 3L)
  )
  transition$target_unit_2001 <- paste0(
    "pc2001__",
    normalize_census_code(transition$state_code_2001, 2L), "__",
    normalize_census_code(transition$district_code_2001, 2L)
  )

  population_share <- num(transition$population_share_to_2001)
  area_share <- num(transition$area_share_to_2001)
  coverage <- num(transition$shrid_coverage)
  deterministic <- transition[
    !is.na(transition$mapping_class) &
      transition$mapping_class %in% deterministic_transition_mapping_classes() &
      is.finite(population_share) & abs(population_share - 1) < 1e-8 &
      is.finite(area_share) & abs(area_share - 1) < 1e-8 &
      is.finite(coverage) & abs(coverage - 1) < 1e-8,
    , drop = FALSE
  ]
  if (!nrow(deterministic)) return(data.frame())

  source_groups <- split(seq_len(nrow(deterministic)), deterministic$source_unit_2011)
  source_bridge <- safe_bind_rows(lapply(source_groups, function(index) {
    part <- deterministic[index, , drop = FALSE]
    targets <- unique(part$target_unit_2001)
    if (length(targets) != 1L) return(NULL)
    data.frame(
      source_unit_2011 = part$source_unit_2011[[1]],
      target_unit_2001 = targets[[1]],
      mapping_class = paste(sort(unique(part$mapping_class)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }))
  if (!nrow(source_bridge)) return(data.frame())
  if (anyDuplicated(source_bridge$source_unit_2011)) {
    stop("Deterministic Census-2011 age bridge is not unique by source district.", call. = FALSE)
  }

  # A Census-2001 parent is usable only when every 2011 source district that
  # contributes territory to that parent is itself deterministically assigned
  # back to that same parent. Otherwise summing only the deterministic children
  # would create a partial-parent population anchor.
  contributors <- unique(transition[c("source_unit_2011", "target_unit_2001")])
  contributors <- contributors[
    !is.na(contributors$source_unit_2011) & nzchar(contributors$source_unit_2011) &
      !is.na(contributors$target_unit_2001) & nzchar(contributors$target_unit_2001),
    , drop = FALSE
  ]
  names(source_bridge)[names(source_bridge) == "target_unit_2001"] <- "deterministic_target_2001"
  contributors <- merge(
    contributors,
    source_bridge[c("source_unit_2011", "deterministic_target_2001")],
    by = "source_unit_2011",
    all.x = TRUE,
    sort = FALSE
  )
  contributors$is_complete_source <- !is.na(contributors$deterministic_target_2001) &
    contributors$deterministic_target_2001 == contributors$target_unit_2001
  target_groups <- split(seq_len(nrow(contributors)), contributors$target_unit_2001)
  target_status <- safe_bind_rows(lapply(target_groups, function(index) {
    part <- contributors[index, , drop = FALSE]
    data.frame(
      target_unit_2001 = part$target_unit_2001[[1]],
      census_2011_contributing_source_count = length(unique(part$source_unit_2011)),
      census_2011_deterministic_source_count = sum(part$is_complete_source),
      census_2011_parent_reconstruction_complete = all(part$is_complete_source),
      stringsAsFactors = FALSE
    )
  }))

  names(source_bridge)[names(source_bridge) == "deterministic_target_2001"] <- "target_unit_2001"
  out <- merge(source_bridge, target_status, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out <- out[out$census_2011_parent_reconstruction_complete %in% TRUE, , drop = FALSE]
  if (nrow(out) && anyDuplicated(out$source_unit_2011)) {
    stop("Complete Census-2011 age bridge is not unique by source district.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
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
