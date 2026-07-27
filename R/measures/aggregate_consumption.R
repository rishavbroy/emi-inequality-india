# Household-consumption aggregation rules.

positive_finite <- function(x) {
  x <- num(x)
  is.finite(x) & x > 0
}

#' Mean monthly per-capita expenditure among represented people
#'
#' @param household_total Total monthly household expenditure.
#' @param household_size Number of household members.
#' @param household_weight NSS household multiplier.
#' @return Person-weighted mean monthly per-capita expenditure.
mean_expenditure_per_person <- function(household_total, household_size, household_weight) {
  total <- num(household_total)
  size <- num(household_size)
  weight <- num(household_weight)
  keep <- is.finite(total) & positive_finite(size) & positive_finite(weight)
  if (!any(keep)) return(NA_real_)
  sum(weight[keep] * total[keep], na.rm = TRUE) /
    sum(weight[keep] * size[keep], na.rm = TRUE)
}

#' Mean household MPCE
#'
#' Each represented household receives its NSS household multiplier. This is
#' retained for sensitivity analysis; it is not the preferred population-welfare
#' estimand.
mean_household_mpce <- function(household_total, household_size, household_weight) {
  total <- num(household_total)
  size <- num(household_size)
  weight <- num(household_weight)
  keep <- is.finite(total) & positive_finite(size) & positive_finite(weight)
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(total[keep] / size[keep], weight[keep])
}

#' Person-weighted Gini of household MPCE
person_weighted_mpce_gini <- function(household_total, household_size, household_weight) {
  total <- num(household_total)
  size <- num(household_size)
  weight <- num(household_weight)
  keep <- is.finite(total) & positive_finite(size) & positive_finite(weight)
  if (!any(keep)) return(NA_real_)
  wgini(total[keep] / size[keep], weight[keep] * size[keep])
}

#' Deflate monthly household expenditure
#'
#' The deflator must be positive and expressed relative to the chosen common
#' state-sector price reference.
deflate_household_expenditure <- function(household_total, price_deflator) {
  total <- num(household_total)
  deflator <- num(price_deflator)
  out <- rep(NA_real_, length(total))
  keep <- is.finite(total) & positive_finite(deflator)
  out[keep] <- total[keep] / deflator[keep]
  out
}
