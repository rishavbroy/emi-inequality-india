# Household consumption summaries for household and person estimands.

consumption_aggregate <- function(
  total_consumption, household_size, survey_weight, deflator = NULL
) {
  total <- num(total_consumption)
  size <- num(household_size)
  weight <- num(survey_weight)
  if (is.null(deflator)) {
    deflator <- rep(1, length(total))
  } else {
    deflator <- num(deflator)
  }

  valid <- is.finite(total) & is.finite(size) & size > 0 &
    is.finite(weight) & weight > 0 & is.finite(deflator) & deflator > 0
  total <- total[valid]
  size <- size[valid]
  weight <- weight[valid]
  deflator <- deflator[valid]
  if (!length(total)) {
    return(list(
      nominal_person_mean = NA_real_,
      nominal_household_mean = NA_real_,
      real_person_mean = NA_real_,
      real_household_mean = NA_real_,
      nominal_person_gini = NA_real_,
      real_person_gini = NA_real_,
      weighted_people = 0,
      weighted_households = 0
    ))
  }

  nominal_pc <- total / size
  real_pc <- nominal_pc / deflator
  person_weight <- weight * size
  list(
    nominal_person_mean = wmean(nominal_pc, person_weight),
    nominal_household_mean = wmean(nominal_pc, weight),
    real_person_mean = wmean(real_pc, person_weight),
    real_household_mean = wmean(real_pc, weight),
    nominal_person_gini = wgini(nominal_pc, person_weight),
    real_person_gini = wgini(real_pc, person_weight),
    weighted_people = sum(person_weight),
    weighted_households = sum(weight)
  )
}

consumption_total_from_per_capita <- function(per_capita, household_size) {
  num(per_capita) * num(household_size)
}
