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

validate_economic_census_2005_it_baseline <- function(source) {
  source <- safe_df(source)
  required <- c(
    "state_code", "district_code", "nonfarm_firms_raw", "nonfarm_employment_raw",
    "it_firms", "it_employment"
  )
  missing <- setdiff(required, names(source))
  if (length(missing)) stop("EC05 IT baseline is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (any(!stats::complete.cases(source[c("state_code", "district_code")])) ||
      anyDuplicated(source[c("state_code", "district_code")])) {
    stop("EC05 IT baseline requires unique complete district keys.", call. = FALSE)
  }
  for (column in required[-c(1L, 2L)]) {
    value <- num(source[[column]])
    if (any(!is.finite(value) | value < 0)) stop("EC05 IT baseline counts must be finite and nonnegative.", call. = FALSE)
  }
  if (any(source$nonfarm_firms_raw <= 0) || any(source$nonfarm_employment_raw <= 0) ||
      any(source$it_firms > source$nonfarm_firms_raw) ||
      any(source$it_employment > source$nonfarm_employment_raw)) {
    stop("EC05 IT baseline violates establishment/employment subset accounting.", call. = FALSE)
  }
  source
}

economic_census_2005_name_aliases <- function(x) {
  key <- canonicalize_district_name(x)
  aliases <- c(
    "baska" = "baksa",
    "kamrup metro" = "kamrup metropolitan",
    "seraikela kharswan" = "saraikela kharsawan",
    "ashok nagar" = "ashoknagar",
    "burhanpor" = "burhanpur",
    "arval" = "arwal"
  )
  hit <- match(key, names(aliases), nomatch = 0L)
  key[hit > 0L] <- unname(aliases[hit[hit > 0L]])
  key
}

build_economic_census_2005_it_baseline <- function(
    source, admin_units_2001, admin_units_2011, district_transition_2001_2011) {
  source <- validate_economic_census_2005_it_baseline(source)
  if (!"district_name" %in% names(source)) {
    stop("EC05 IT geography harmonization requires official district names.", call. = FALSE)
  }
  admin01 <- canonical_economic_census_2001_districts(admin_units_2001)
  admin11 <- safe_df(admin_units_2011)
  admin11 <- admin11[admin11$level == "district", c("state_code", "district_code", "district_std"), drop = FALSE]
  admin11$state_code <- normalize_census_code(admin11$state_code, 2L)
  admin11$district_code <- normalize_census_code(admin11$district_code, 3L)
  admin11$name_key <- canonicalize_district_name(admin11$district_std)
  if (anyDuplicated(admin11[c("state_code", "name_key")])) {
    stop("Census-2011 district names must be unique within state for EC05 harmonization.", call. = FALSE)
  }

  source$state_code <- normalize_census_code(source$state_code, 2L)
  source$district_code <- normalize_census_code(source$district_code, 2L)
  source$name_key <- economic_census_2005_name_aliases(source$district_name)
  admin01$name_key <- canonicalize_district_name(admin01$district_std)
  source$key <- paste(source$state_code, source$district_code, sep = "/")
  admin01$key <- paste(admin01$state_code, admin01$district_code, sep = "/")

  # Preserve same-code identities unless the official EC05 name resolves to a
  # different Census-2001 district in the same state. That catches genuine merged
  # or recoded units (Mumbai) without treating harmless spelling drift as lineage.
  direct_idx <- match(source$key, admin01$key)
  name_idx <- match(
    paste(source$state_code, source$name_key, sep = "__"),
    paste(admin01$state_code, admin01$name_key, sep = "__")
  )
  conflict <- !is.na(direct_idx) & !is.na(name_idx) & direct_idx != name_idx
  direct_ok <- !is.na(direct_idx) & !conflict
  mapped <- source[direct_ok, , drop = FALSE]
  mapped$target_unit_2001 <- admin01$target_unit_2001[direct_idx[direct_ok]]

  unavailable <- character()
  if (any(conflict)) {
    unavailable <- c(
      unavailable,
      admin01$target_unit_2001[direct_idx[conflict]],
      admin01$target_unit_2001[name_idx[conflict]]
    )
  }

  extra <- source[is.na(direct_idx), , drop = FALSE]
  if (nrow(extra)) {
    idx11 <- match(
      paste(extra$state_code, extra$name_key, sep = "__"),
      paste(admin11$state_code, admin11$name_key, sep = "__")
    )
    if (any(is.na(idx11))) {
      stop("EC05 post-2001 districts could not be matched to the Census-2011 registry.", call. = FALSE)
    }
    extra$source_unit_2011 <- paste0(
      "pc2011__", extra$state_code, "__", admin11$district_code[idx11]
    )
    bridge <- build_complete_deterministic_transition_2011_to_2001(district_transition_2001_2011)
    exact_idx <- match(extra$source_unit_2011, bridge$source_unit_2011)
    if (any(!is.na(exact_idx))) {
      exact <- extra[!is.na(exact_idx), , drop = FALSE]
      exact$target_unit_2001 <- bridge$target_unit_2001[exact_idx[!is.na(exact_idx)]]
      mapped <- safe_bind_rows(list(mapped, exact))
    }
    unresolved <- extra$source_unit_2011[is.na(exact_idx)]
    if (length(unresolved)) {
      transition <- safe_df(district_transition_2001_2011)
      transition$source_unit_2011 <- paste0(
        "pc2011__", normalize_census_code(transition$state_code_2011, 2L), "__",
        normalize_census_code(transition$district_code_2011, 3L)
      )
      hit <- transition$source_unit_2011 %in% unresolved
      unavailable <- c(
        unavailable,
        paste0(
          "pc2001__", normalize_census_code(transition$state_code_2001[hit], 2L), "__",
          normalize_census_code(transition$district_code_2001[hit], 2L)
        )
      )
    }
  }

  unavailable <- unique(unavailable[!is.na(unavailable) & nzchar(unavailable)])
  mapped <- mapped[!mapped$target_unit_2001 %in% unavailable, , drop = FALSE]
  counts <- c("nonfarm_firms_raw", "nonfarm_employment_raw", "it_firms", "it_employment")
  if (nrow(mapped)) {
    for (column in counts) mapped[[column]] <- num(mapped[[column]])
    mapped$ec05_source_district_count <- 1L
    pooled <- stats::aggregate(
      mapped[c(counts, "ec05_source_district_count")],
      by = list(target_unit_2001 = mapped$target_unit_2001),
      FUN = sum
    )
  } else {
    pooled <- mapped[FALSE, c("target_unit_2001", counts), drop = FALSE]
    pooled$ec05_source_district_count <- integer()
  }

  out <- merge(admin01, pooled, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out <- out[match(admin01$target_unit_2001, out$target_unit_2001), , drop = FALSE]
  out$source_available <- !out$target_unit_2001 %in% unavailable & stats::complete.cases(out[counts])
  for (column in counts) out[[column]][!out$source_available] <- NA_real_
  out$it_firm_share_nonfarm <- safe_count_share(out$it_firms, out$nonfarm_firms_raw)
  out$it_employment_share_nonfarm <- safe_count_share(out$it_employment, out$nonfarm_employment_raw)
  rownames(out) <- NULL
  out
}
