# Predetermined district controls from Census 2001.

census_2001_main_controls <- function() {
  c(
    'log_population_2001',
    'urban_share_2001',
    'adult_secondary_plus_share_2001',
    'sc_share_2001',
    'st_share_2001',
    'muslim_share_2001',
    'agricultural_worker_share_2001',
    'dependency_ratio_2001',
    'electricity_access_share_2001',
    'log_population_density_2001'
  )
}

census_2001_appendix_controls <- function() {
  c(
    'literacy_share_2001', 'worker_share_2001', 'hindu_share_2001',
    'banking_access_share_2001', 'television_ownership_share_2001',
    'telephone_ownership_share_2001', 'primary_schools_per_1000_children_2001'
  )
}

safe_share <- function(numerator, denominator, scale = 100) {
  nume <- num(numerator)
  deno <- num(denominator)
  out <- rep(NA_real_, length(nume))
  keep <- is.finite(nume) & is.finite(deno) & deno > 0
  out[keep] <- scale * nume[keep] / deno[keep]
  out
}

#' Construct the compact Census 2001 control set from district totals
#'
#' Input columns are explicit totals rather than rates so that aggregation from
#' lower geographic units occurs before ratios are calculated.
build_census_2001_controls <- function(district_totals) {
  x <- safe_df(district_totals)
  required <- c(
    'district_code_2001', 'population_total', 'population_urban',
    'population_age_7_plus', 'adult_secondary_plus', 'sc_population',
    'st_population', 'muslim_population', 'workers_total',
    'cultivators', 'agricultural_labourers', 'population_age_0_14',
    'population_age_15_64', 'population_age_65_plus', 'households_total',
    'households_electricity', 'area_sq_km'
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop('Census 2001 control input is missing columns: ', paste(missing, collapse = ', '), call. = FALSE)
  out <- data.frame(district_code_2001 = as.character(x$district_code_2001), stringsAsFactors = FALSE)
  out$log_population_2001 <- log(num(x$population_total))
  out$urban_share_2001 <- safe_share(x$population_urban, x$population_total)
  out$adult_secondary_plus_share_2001 <- safe_share(x$adult_secondary_plus, x$population_age_7_plus)
  out$sc_share_2001 <- safe_share(x$sc_population, x$population_total)
  out$st_share_2001 <- safe_share(x$st_population, x$population_total)
  out$muslim_share_2001 <- safe_share(x$muslim_population, x$population_total)
  out$agricultural_worker_share_2001 <- safe_share(num(x$cultivators) + num(x$agricultural_labourers), x$workers_total)
  out$dependency_ratio_2001 <- safe_share(num(x$population_age_0_14) + num(x$population_age_65_plus), x$population_age_15_64)
  out$electricity_access_share_2001 <- safe_share(x$households_electricity, x$households_total)
  density <- num(x$population_total) / num(x$area_sq_km)
  out$log_population_density_2001 <- ifelse(positive_finite(density), log(density), NA_real_)
  if (anyDuplicated(out$district_code_2001)) stop('Census 2001 controls contain duplicate district codes.', call. = FALSE)
  out
}
