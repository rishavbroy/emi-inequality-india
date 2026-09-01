# Canonical Economic Census measures and shared count/share semantics.

economic_census_share_specs <- function() {
  c(
    female_employment_share = "female_employment",
    hired_employment_share = "hired_employment",
    private_employment_share = "private_employment",
    informal_employment_share = "informal_employment",
    manufacturing_employment_share = "manufacturing_employment",
    services_employment_share = "services_employment"
  )
}

add_economic_census_derived_measures <- function(x, available = rep(TRUE, nrow(x))) {
  x <- safe_df(x)
  x$mean_employment_per_firm <- ifelse(
    available,
    num(x$nonfarm_employment) / num(x$firms_total),
    NA_real_
  )
  share_specs <- economic_census_share_specs()
  for (share in names(share_specs)) {
    x[[share]] <- safe_count_share(x[[share_specs[[share]]]], x$nonfarm_employment)
  }
  invalid_share <- available & !stats::complete.cases(x[names(share_specs)])
  if (any(invalid_share)) {
    stop("Economic Census core employment shares must be bounded subsets of total employment.", call. = FALSE)
  }
  x
}

build_economic_census_2005_measures <- function(
    source,
    admin_units_2001,
    minimum_source_coverage = 0.99) {
  source <- validate_economic_census_source_counts(source, "SHRUG EC05 district source")
  admin <- safe_df(admin_units_2001)
  required_admin <- c("level", "state_code", "district_code", "state_std", "district_std")
  if (length(setdiff(required_admin, names(admin)))) {
    stop("Economic Census canonical district registry is missing required columns.", call. = FALSE)
  }
  admin <- admin[admin$level == "district", required_admin, drop = FALSE]
  admin$state_code <- normalize_census_code(admin$state_code, 2L)
  admin$district_code <- normalize_census_code(admin$district_code, 2L)
  keys <- c("state_code", "district_code")
  if (!nrow(admin) || anyDuplicated(admin[keys]) || any(!stats::complete.cases(admin[keys]))) {
    stop("Economic Census canonical district registry must contain unique Census-2001 districts.", call. = FALSE)
  }

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
  out$target_unit_2001 <- paste0("pc2001__", out$state_code, "__", out$district_code)
  out$source_available <- paste(out$state_code, out$district_code, sep = "/") %in% source_key
  out <- add_economic_census_derived_measures(out, out$source_available)
  rownames(out) <- NULL
  out
}
