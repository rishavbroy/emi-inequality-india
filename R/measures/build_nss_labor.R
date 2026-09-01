# Design-based NSS64 district labor and migration estimates on reviewed
# Census-2001 geography. Support rules are frozen before outcome estimation.

nss64_design_psu_key <- function(x) {
  x <- safe_df(x)
  required <- c("source_district_code", "state_code", "sector", "fsu")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("NSS64 rows lack PSU fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  interaction(
    plain_chr(x$source_district_code), plain_chr(x$state_code),
    num(x$sector), num(x$fsu),
    drop = TRUE, lex.order = TRUE
  )
}

nss64_design_stratum_key <- function(x) {
  x <- safe_df(x)
  required <- c(
    "source_district_code", "state_code", "sector", "stratum", "sub_stratum"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("NSS64 rows lack stratum fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  interaction(
    plain_chr(x$source_district_code), plain_chr(x$state_code),
    num(x$sector), num(x$stratum), num(x$sub_stratum),
    drop = TRUE, lex.order = TRUE
  )
}

nss64_support_rule <- function() {
  data.frame(
    min_fsu = 5L,
    min_kish_effective_n = 100,
    stringsAsFactors = FALSE
  )
}

nss64_outcome_registry <- function() {
  data.frame(
    outcome_id = c(
      "labor_force_participation_age15plus",
      "employment_rate_age15plus",
      "unemployment_rate_age15plus",
      "regular_salaried_share_employed_age15plus",
      "migrant_from_last_upr_share_age15plus"
    ),
    source = c("usual_activity", "usual_activity", "usual_activity", "usual_activity", "migration"),
    denominator = c("age15plus", "age15plus", "labor_force_age15plus", "employed_age15plus", "age15plus"),
    role = c("core", "core", "core", "core", "core"),
    temporal_role = rep("near_treatment_reference", 5L),
    stringsAsFactors = FALSE
  )
}

nss64_employed_status_codes <- function() c(11, 12, 21, 31, 41, 51)
nss64_unemployed_status_codes <- function() 81

nss64_labor_status_flags <- function(principal_status, subsidiary_status) {
  principal <- num(principal_status)
  subsidiary <- num(subsidiary_status)
  principal_employed <- principal %in% nss64_employed_status_codes()
  subsidiary_employed <- subsidiary %in% nss64_employed_status_codes()
  employed <- principal_employed | subsidiary_employed
  unemployed <- principal %in% nss64_unemployed_status_codes() & !subsidiary_employed
  combined_status <- ifelse(
    principal_employed, principal,
    ifelse(subsidiary_employed, subsidiary, principal)
  )
  data.frame(
    employed = employed,
    unemployed = unemployed,
    labor_force = employed | unemployed,
    regular_salaried = employed & combined_status == 31,
    stringsAsFactors = FALSE
  )
}

nss64_design_rows <- function(lineaged_usual_activity, migration) {
  usual <- safe_df(lineaged_usual_activity)
  mig <- safe_df(migration)
  required <- c(
    "person_key", "source_district_code", "state_code", "sector", "stratum", "sub_stratum", "fsu",
    "survey_weight", "target_unit_2001", "lineage_status", "age",
    "usual_principal_status", "usual_subsidiary_status"
  )
  missing <- setdiff(required, names(usual))
  if (length(missing)) {
    stop("Lineaged NSS64 usual-activity rows lack estimation fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!all(c("person_key", "enumeration_differs_last_upr") %in% names(mig))) {
    stop("NSS64 migration rows lack migration-status fields.", call. = FALSE)
  }
  resolved <- usual$lineage_status %in% "resolved_reviewed_deterministic" &
    !is.na(usual$target_unit_2001) & nzchar(plain_chr(usual$target_unit_2001))
  x <- usual[resolved, , drop = FALSE]
  if (!nrow(x)) stop("No reviewed NSS64 persons are available for district estimation.", call. = FALSE)

  midx <- match(x$person_key, mig$person_key)
  if (anyNA(midx)) stop("NSS64 migration source does not cover all reviewed usual-activity persons.", call. = FALSE)
  x$migrant_from_last_upr <- num(mig$enumeration_differs_last_upr[midx]) == 1
  flags <- nss64_labor_status_flags(
    x$usual_principal_status, x$usual_subsidiary_status
  )
  x$employed <- flags$employed
  x$unemployed <- flags$unemployed
  x$labor_force <- flags$labor_force
  x$regular_salaried <- flags$regular_salaried
  x$age15plus <- num(x$age) >= 15

  valid_age <- is.finite(num(x$age)) & num(x$age) >= 0
  valid_status <- is.finite(num(x$usual_principal_status))
  valid_migration <- num(mig$enumeration_differs_last_upr[midx]) %in% c(1, 2)
  if (!all(valid_age)) stop("Reviewed NSS64 persons contain invalid ages.", call. = FALSE)
  if (!all(valid_status)) stop("Reviewed NSS64 persons contain missing usual-principal activity status.", call. = FALSE)
  if (!all(valid_migration)) stop("Reviewed NSS64 persons contain invalid last-UPR migration codes.", call. = FALSE)

  x$.design_stratum <- nss64_design_stratum_key(x)
  x$.design_psu <- nss64_design_psu_key(x)
  x
}

nss64_survey_design_from_rows <- function(rows) {
  survey::svydesign(
    ids = ~.design_psu,
    strata = ~.design_stratum,
    weights = ~survey_weight,
    data = safe_df(rows),
    nest = TRUE
  )
}

nss64_target_support_classification <- function(target_support, rule = nss64_support_rule()) {
  x <- safe_df(target_support)
  required <- c("target_unit_2001", "n_fsu", "kish_effective_n")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("NSS64 target support lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  if (nrow(rule) != 1L || !all(c("min_fsu", "min_kish_effective_n") %in% names(rule))) {
    stop("NSS64 support rule must contain one min_fsu/min_kish_effective_n row.", call. = FALSE)
  }
  x$preferred_eligible <- x$n_fsu >= rule$min_fsu[[1L]] &
    x$kish_effective_n >= rule$min_kish_effective_n[[1L]]
  x$support_reason <- NA_character_
  thin_psu <- x$n_fsu < rule$min_fsu[[1L]]
  low_kish <- x$kish_effective_n < rule$min_kish_effective_n[[1L]]
  x$support_reason[thin_psu] <- "too_few_psus"
  x$support_reason[low_kish] <- ifelse(
    is.na(x$support_reason[low_kish]), "low_kish_effective_n",
    paste(x$support_reason[low_kish], "low_kish_effective_n", sep = ";")
  )
  x
}

nss64_outcome_domain <- function(rows, outcome_id) {
  x <- safe_df(rows)
  if (identical(outcome_id, "labor_force_participation_age15plus")) {
    return(list(rows = x$age15plus, value = x$labor_force))
  }
  if (identical(outcome_id, "employment_rate_age15plus")) {
    return(list(rows = x$age15plus, value = x$employed))
  }
  if (identical(outcome_id, "unemployment_rate_age15plus")) {
    return(list(rows = x$age15plus & x$labor_force, value = x$unemployed))
  }
  if (identical(outcome_id, "regular_salaried_share_employed_age15plus")) {
    return(list(rows = x$age15plus & x$employed, value = x$regular_salaried))
  }
  if (identical(outcome_id, "migrant_from_last_upr_share_age15plus")) {
    return(list(rows = x$age15plus, value = x$migrant_from_last_upr))
  }
  stop("Unsupported NSS64 labor outcome: ", outcome_id, call. = FALSE)
}

nss64_domain_support <- function(rows, domain_rows) {
  x <- safe_df(rows)
  if (length(domain_rows) != nrow(x)) stop("NSS64 domain-support index has the wrong length.", call. = FALSE)
  x <- x[domain_rows, , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  groups <- split(seq_len(nrow(x)), x$target_unit_2001)
  out <- safe_bind_rows(lapply(groups, function(i) {
    part <- x[i, , drop = FALSE]
    data.frame(
      target_unit_2001 = plain_chr(part$target_unit_2001[[1L]]),
      n_sample_people = nrow(part),
      n_fsu = length(unique(part$.design_psu)),
      sum_person_weight = sum(num(part$survey_weight)),
      kish_effective_n = kish_effective_n(part$survey_weight),
      stringsAsFactors = FALSE
    )
  }))
  out[order(out$target_unit_2001), , drop = FALSE]
}

estimate_nss64_district_outcome <- function(rows, design, rule, outcome_id) {
  domain <- nss64_outcome_domain(rows, outcome_id)
  if (!any(domain$rows)) stop("NSS64 outcome has an empty estimation domain: ", outcome_id, call. = FALSE)
  support <- nss64_target_support_classification(
    nss64_domain_support(rows, domain$rows), rule
  )
  domain_design <- design[domain$rows, ]
  domain_design <- update(domain_design, .outcome = as.numeric(domain$value[domain$rows]))
  result <- with_survey_lonely_psu_adjustment(survey::svyby(
    ~.outcome,
    ~target_unit_2001,
    domain_design,
    survey::svymean,
    na.rm = TRUE,
    vartype = "se",
    keep.names = FALSE,
    drop.empty.groups = FALSE
  ))
  result_df <- as.data.frame(result, stringsAsFactors = FALSE)
  estimate <- num(stats::coef(result))
  std_error <- num(survey::SE(result))
  if (!"target_unit_2001" %in% names(result_df) ||
      length(estimate) != nrow(result_df) || length(std_error) != nrow(result_df)) {
    stop("survey::svyby returned an unexpected NSS64 district result.", call. = FALSE)
  }
  out <- data.frame(
    target_unit_2001 = plain_chr(result_df$target_unit_2001),
    outcome_id = outcome_id,
    estimate = estimate,
    std_error = std_error,
    stringsAsFactors = FALSE
  )
  out <- merge(out, support, by = "target_unit_2001", all.x = TRUE, sort = FALSE)
  out$status <- ifelse(is.finite(out$estimate), "estimated", "not_estimable")
  out$analysis_eligible <- is.finite(out$estimate) & out$preferred_eligible %in% TRUE
  out[order(out$target_unit_2001), , drop = FALSE]
}

estimate_nss64_district_outcomes <- function(
    lineaged_usual_activity, migration, target_support,
    registry = nss64_outcome_registry(), rule = nss64_support_rule()) {
  registry <- safe_df(registry)
  if (!nrow(registry) || anyDuplicated(registry$outcome_id)) {
    stop("NSS64 labor registry must contain unique outcomes.", call. = FALSE)
  }
  rows <- nss64_design_rows(lineaged_usual_activity, migration)
  design <- nss64_survey_design_from_rows(rows)
  target_support <- nss64_target_support_classification(target_support, rule)
  estimates <- safe_bind_rows(lapply(registry$outcome_id, function(outcome_id) {
    estimate_nss64_district_outcome(rows, design, rule, outcome_id)
  }))
  list(
    registry = registry,
    support_rule = rule,
    target_support = target_support,
    estimates = estimates
  )
}
