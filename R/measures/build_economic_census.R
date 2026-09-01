# Canonical Economic Census measures and shared count/share semantics.

economic_census_share_specs <- function(include_informal = FALSE) {
  out <- c(
    female_employment_share = "female_employment",
    hired_employment_share = "hired_employment",
    private_employment_share = "private_employment",
    manufacturing_employment_share = "manufacturing_employment",
    services_employment_share = "services_employment"
  )
  if (include_informal) {
    out <- c(out, informal_employment_share = "informal_employment")
  }
  out
}

economic_census_longitudinal_measure_columns <- function() {
  c(
    "log_nonfarm_employment", "log_firms_total", "mean_employment_per_firm",
    names(economic_census_share_specs(FALSE))
  )
}

add_economic_census_derived_measures <- function(x, available = rep(TRUE, nrow(x))) {
  x <- safe_df(x)
  x$log_nonfarm_employment <- ifelse(available, log(num(x$nonfarm_employment)), NA_real_)
  x$log_firms_total <- ifelse(available, log(num(x$firms_total)), NA_real_)
  x$mean_employment_per_firm <- ifelse(
    available,
    num(x$nonfarm_employment) / num(x$firms_total),
    NA_real_
  )
  share_specs <- economic_census_share_specs("informal_employment" %in% names(x))
  for (share in names(share_specs)) {
    x[[share]] <- safe_count_share(x[[share_specs[[share]]]], x$nonfarm_employment)
  }
  invalid_share <- available & !stats::complete.cases(x[names(share_specs)])
  if (any(invalid_share)) {
    stop("Economic Census core employment shares must be bounded subsets of total employment.", call. = FALSE)
  }
  x
}

canonical_economic_census_2001_districts <- function(admin_units_2001) {
  admin <- safe_df(admin_units_2001)
  required <- c("level", "state_code", "district_code", "state_std", "district_std")
  if (length(setdiff(required, names(admin)))) {
    stop("Economic Census canonical district registry is missing required columns.", call. = FALSE)
  }
  admin <- admin[admin$level == "district", required, drop = FALSE]
  admin$state_code <- normalize_census_code(admin$state_code, 2L)
  admin$district_code <- normalize_census_code(admin$district_code, 2L)
  keys <- c("state_code", "district_code")
  if (!nrow(admin) || anyDuplicated(admin[keys]) || any(!stats::complete.cases(admin[keys]))) {
    stop("Economic Census canonical district registry must contain unique Census-2001 districts.", call. = FALSE)
  }
  admin$target_unit_2001 <- paste0("pc2001__", admin$state_code, "__", admin$district_code)
  admin
}

build_economic_census_2005_measures <- function(
    source,
    admin_units_2001,
    minimum_source_coverage = 0.99) {
  source <- validate_economic_census_source_counts(
    source, "SHRUG EC05 district source", economic_census_2005_count_columns()
  )
  admin <- canonical_economic_census_2001_districts(admin_units_2001)
  keys <- c("state_code", "district_code")

  source_key <- paste(source$state_code, source$district_code, sep = "/")
  admin_key <- paste(admin$state_code, admin$district_code, sep = "/")
  outside <- source_key[!source_key %in% admin_key]
  if (length(outside)) {
    stop(
      "SHRUG EC05 contains districts outside the canonical Census-2001 registry: ",
      paste(unique(outside), collapse = ", "),
      call. = FALSE
    )
  }
  coverage <- length(unique(source_key)) / length(unique(admin_key))
  if (!is.finite(coverage) || coverage < minimum_source_coverage) {
    stop(
      sprintf(
        "SHRUG EC05 Census-2001 district coverage %.3f is below the required %.3f.",
        coverage, minimum_source_coverage
      ),
      call. = FALSE
    )
  }

  out <- merge(admin, source, by = keys, all.x = TRUE, sort = FALSE)
  out <- out[match(admin_key, paste(out$state_code, out$district_code, sep = "/")), , drop = FALSE]
  out$source_available <- paste(out$state_code, out$district_code, sep = "/") %in% source_key
  out <- add_economic_census_derived_measures(out, out$source_available)
  rownames(out) <- NULL
  out
}

build_economic_census_2013_measures <- function(
    source,
    admin_units_2011,
    admin_units_2001,
    district_transition_2001_2011,
    expected_source_districts = 640L) {
  source <- validate_economic_census_source_counts(source, "SHRUG EC13 district source")
  admin_2011 <- safe_df(admin_units_2011)
  needed <- c("level", "state_code", "district_code", "district_std")
  if (length(setdiff(needed, names(admin_2011)))) {
    stop("Economic Census Census-2011 registry is missing required columns.", call. = FALSE)
  }
  admin_2011 <- admin_2011[admin_2011$level == "district", needed, drop = FALSE]
  admin_2011$state_code <- normalize_census_code(admin_2011$state_code, 2L)
  admin_2011$district_code <- normalize_census_code(admin_2011$district_code, 3L)
  keys <- c("state_code", "district_code")
  if (nrow(admin_2011) != as.integer(expected_source_districts) ||
      anyDuplicated(admin_2011[keys]) || any(!stats::complete.cases(admin_2011[keys]))) {
    stop(
      "Economic Census Census-2011 registry must contain the expected unique district universe.",
      call. = FALSE
    )
  }
  source_key <- paste(source$state_code, source$district_code, sep = "/")
  admin_key <- paste(admin_2011$state_code, admin_2011$district_code, sep = "/")
  if (!setequal(source_key, admin_key)) {
    stop("SHRUG EC13 must cover the complete expected Census-2011 district registry.", call. = FALSE)
  }

  idx <- match(source_key, admin_key)
  source$district_name <- admin_2011$district_std[idx]
  pooled <- harmonize_census_2011_counts_to_2001(
    source,
    district_transition_2001_2011,
    economic_census_common_count_columns()
  )
  canonical <- canonical_economic_census_2001_districts(admin_units_2001)
  out <- merge(
    canonical,
    pooled,
    by = "target_unit_2001",
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", "_pooled")
  )
  out <- out[match(canonical$target_unit_2001, out$target_unit_2001), , drop = FALSE]
  out$harmonized_available <- out$census_2011_parent_reconstruction_complete %in% TRUE &
    stats::complete.cases(out[economic_census_common_count_columns()])
  out <- add_economic_census_derived_measures(out, out$harmonized_available)
  rownames(out) <- NULL
  out
}

build_economic_census_change_measures <- function(ec05, ec13) {
  baseline <- safe_df(ec05)
  followup <- safe_df(ec13)
  measures <- economic_census_longitudinal_measure_columns()
  required <- c("target_unit_2001", measures)
  if (length(setdiff(required, names(baseline))) || length(setdiff(required, names(followup)))) {
    stop("Economic Census change construction is missing common longitudinal measures.", call. = FALSE)
  }
  baseline <- baseline[required]
  followup <- followup[c(
    "target_unit_2001", "census_2011_source_district_count",
    "census_2011_parent_reconstruction_complete", measures
  )]
  names(baseline)[-1L] <- paste0(measures, "_2005")
  names(followup)[names(followup) %in% measures] <- paste0(
    names(followup)[names(followup) %in% measures], "_2013"
  )
  out <- merge(followup, baseline, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  if (nrow(out) != nrow(followup) || anyDuplicated(out$target_unit_2001)) {
    stop("Economic Census changes require one EC05 row per harmonized EC13 parent.", call. = FALSE)
  }
  for (measure in measures) {
    out[[paste0(measure, "_change_2013_2005")]] <-
      num(out[[paste0(measure, "_2013")]]) - num(out[[paste0(measure, "_2005")]])
  }
  rownames(out) <- NULL
  out
}
