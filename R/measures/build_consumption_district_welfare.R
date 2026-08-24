# Design-aware district welfare estimates from canonical, lineaged consumption
# households. The sampling multiplier represents households; MPCE is a
# person-level welfare concept, so district MPCE means use multiplier ×
# household size × lineage allocation weight.

consumption_design_rows <- function(lineaged_households) {
  x <- safe_df(lineaged_households)
  required <- c(
    "survey_id", "household_id", "source_state_code", "sector", "subround",
    "fsu", "stratum", "sub_stratum", "household_size", "target_unit_2001",
    "lineage_status", "lineage_weight", "lineage_person_weight", "real_mpce"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Lineaged consumption households lack survey-design fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  resolved <- grepl("^resolved_", plain_chr(x$lineage_status))
  x <- x[resolved, , drop = FALSE]
  if (!nrow(x)) stop("No resolved consumption households are available for district welfare estimation.", call. = FALSE)

  x$sector <- price_sector(x$sector)
  x$lineage_weight <- num(x$lineage_weight)
  x$lineage_person_weight <- num(x$lineage_person_weight)
  x$real_mpce <- num(x$real_mpce)

  # Round 66 has no urban sub-stratification. Preserve that design fact
  # explicitly instead of treating the released blank Sub_Stratum field as
  # missing data. Rural blanks remain invalid because rural strata are
  # subdivided in the 66th-round design.
  sub_stratum <- trimws(plain_chr(x$sub_stratum))
  blank_sub_stratum <- is.na(sub_stratum) | !nzchar(sub_stratum)
  invalid_sub_stratum <- blank_sub_stratum & x$sector != "urban"
  sub_stratum[blank_sub_stratum & x$sector == "urban"] <- "__none__"
  x$.design_sub_stratum <- sub_stratum

  valid <- !is.na(x$target_unit_2001) & nzchar(plain_chr(x$target_unit_2001)) &
    !is.na(x$fsu) & nzchar(plain_chr(x$fsu)) &
    !is.na(x$stratum) & nzchar(plain_chr(x$stratum)) &
    !invalid_sub_stratum & !is.na(x$sector) &
    positive_finite(x$lineage_weight) &
    positive_finite(x$lineage_person_weight) & positive_finite(x$real_mpce)
  if (!all(valid)) {
    counts <- c(
      target_unit_2001 = sum(is.na(x$target_unit_2001) | !nzchar(plain_chr(x$target_unit_2001))),
      fsu = sum(is.na(x$fsu) | !nzchar(plain_chr(x$fsu))),
      stratum = sum(is.na(x$stratum) | !nzchar(plain_chr(x$stratum))),
      sub_stratum = sum(invalid_sub_stratum),
      sector = sum(is.na(x$sector)),
      lineage_weight = sum(!positive_finite(x$lineage_weight)),
      lineage_person_weight = sum(!positive_finite(x$lineage_person_weight)),
      real_mpce = sum(!positive_finite(x$real_mpce))
    )
    counts <- counts[counts > 0]
    stop(
      "Resolved consumption households contain invalid survey-design values: ",
      paste(paste(names(counts), counts, sep = "="), collapse = ", "),
      call. = FALSE
    )
  }

  # NSS stratum/sub-stratum identifiers repeat across states and sectors.
  # Build nested design identifiers rather than treating the raw numbers as
  # globally unique. Sub-round is a fieldwork period, not a sampling stratum.
  x$.design_stratum <- interaction(
    x$source_state_code, x$sector, x$stratum, x$.design_sub_stratum,
    drop = TRUE, lex.order = TRUE
  )
  x$.design_psu <- interaction(
    x$source_state_code, x$sector, x$fsu,
    drop = TRUE, lex.order = TRUE
  )
  x
}

consumption_survey_design_from_rows <- function(rows) {
  survey::svydesign(
    ids = ~.design_psu,
    strata = ~.design_stratum,
    weights = ~lineage_person_weight,
    data = safe_df(rows),
    nest = TRUE
  )
}

build_consumption_survey_design <- function(lineaged_households) {
  consumption_survey_design_from_rows(consumption_design_rows(lineaged_households))
}

with_consumption_survey_adjustment <- function(expr) {
  old_options <- options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
  on.exit(options(old_options), add = TRUE)
  withCallingHandlers(
    expr,
    warning = function(w) {
      # survey deliberately warns for domain-level lonely PSUs even when the
      # requested adjustment is applied. Muffle only that handled condition so
      # strict builds remain warning-clean; all other warnings still propagate.
      if (grepl("has only one PSU at stage", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

kish_effective_n <- function(weight) {
  w <- num(weight)
  w <- w[positive_finite(w)]
  if (!length(w)) return(NA_real_)
  denom <- sum(w^2)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  sum(w)^2 / denom
}

consumption_district_support_from_rows <- function(x) {
  x <- safe_df(x)
  groups <- split(seq_len(nrow(x)), x$target_unit_2001)
  safe_bind_rows(lapply(groups, function(i) {
    part <- x[i, , drop = FALSE]
    data.frame(
      district_2001 = plain_chr(part$target_unit_2001[[1L]]),
      n_households = length(unique(part$household_id)),
      n_fsu = length(unique(part$.design_psu)),
      n_sample_person_equiv = sum(num(part$household_size) * num(part$lineage_weight)),
      sum_person_weight = sum(num(part$lineage_person_weight)),
      kish_effective_n = kish_effective_n(part$lineage_person_weight),
      stringsAsFactors = FALSE
    )
  }))
}

consumption_district_support <- function(lineaged_households) {
  consumption_district_support_from_rows(consumption_design_rows(lineaged_households))
}

read_consumption_welfare_outcomes <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "outcome_id", "estimand", "transform", "quantile", "quantile_interval", "quantile_rule",
    "role", "min_households", "min_fsu", "min_kish_effective_n", "max_relative_se"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Consumption welfare registry is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x) || anyDuplicated(x$outcome_id)) {
    stop("Consumption welfare registry must contain unique outcome_id rows.", call. = FALSE)
  }
  if (any(!x$estimand %in% c("survey_mean", "survey_quantile")) ||
      any(!x$transform %in% c("identity", "log"))) {
    stop("Consumption welfare registry contains unsupported estimands or transforms.", call. = FALSE)
  }
  x$quantile <- suppressWarnings(as.numeric(x$quantile))
  mean_row <- x$estimand == "survey_mean"
  quantile_row <- x$estimand == "survey_quantile"
  if (any(mean_row & is.finite(x$quantile)) ||
      any(quantile_row & (!is.finite(x$quantile) | x$quantile <= 0 | x$quantile >= 1))) {
    stop("Consumption welfare registry contains invalid quantile declarations.", call. = FALSE)
  }
  x$quantile_interval <- trimws(plain_chr(x$quantile_interval))
  x$quantile_rule <- trimws(plain_chr(x$quantile_rule))
  x$quantile_interval[is.na(x$quantile_interval)] <- ""
  x$quantile_rule[is.na(x$quantile_rule)] <- ""
  allowed_intervals <- c("mean", "beta", "xlogit", "asin", "score")
  allowed_rules <- c("math", "school", "shahvaish", paste0("hf", 1:9))
  invalid_quantile_method <- quantile_row & (
    !x$quantile_interval %in% allowed_intervals | !x$quantile_rule %in% allowed_rules
  )
  stray_quantile_method <- mean_row & (nzchar(x$quantile_interval) | nzchar(x$quantile_rule))
  if (any(invalid_quantile_method) || any(stray_quantile_method)) {
    stop("Consumption welfare registry contains invalid quantile uncertainty declarations.", call. = FALSE)
  }
  for (nm in c("min_households", "min_fsu", "min_kish_effective_n", "max_relative_se")) {
    x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  }
  if (any(!is.finite(x$min_households) | x$min_households < 1) ||
      any(!is.finite(x$min_fsu) | x$min_fsu < 1) ||
      any(!is.finite(x$min_kish_effective_n) | x$min_kish_effective_n <= 0) ||
      any(is.finite(x$max_relative_se) & x$max_relative_se <= 0)) {
    stop("Consumption welfare registry contains invalid support thresholds.", call. = FALSE)
  }
  x
}

consumption_welfare_value <- function(real_mpce, transform) {
  x <- num(real_mpce)
  if (identical(transform, "identity")) return(x)
  if (identical(transform, "log")) return(log(x))
  stop("Unsupported consumption welfare transform: ", transform, call. = FALSE)
}

consumption_support_reason <- function(out, rule, precision_ok) {
  reasons <- character(nrow(out))
  add_reason <- function(flag, label) {
    idx <- which(flag)
    if (!length(idx)) return(invisible(NULL))
    reasons[idx] <<- ifelse(nzchar(reasons[idx]), paste(reasons[idx], label, sep = ";"), label)
  }
  add_reason(out$n_households < rule$min_households, "thin_household_sample")
  add_reason(out$n_fsu < rule$min_fsu, "too_few_psus")
  add_reason(out$kish_effective_n < rule$min_kish_effective_n, "low_kish_effective_n")
  if (!all(is.na(precision_ok))) add_reason(!is.na(precision_ok) & !precision_ok, "high_relative_se")
  reasons[!nzchar(reasons)] <- NA_character_
  reasons
}

consumption_finalize_district_estimate <- function(
    estimates, support, rule, cv_applicable = FALSE) {
  estimates <- safe_df(estimates)
  required <- c("district_2001", "estimate", "std_error")
  missing <- setdiff(required, names(estimates))
  if (length(missing)) {
    stop("District welfare estimates are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- merge(estimates, support, by = "district_2001", all.x = TRUE, sort = FALSE)
  out$relative_se <- ifelse(
    is.finite(out$estimate) & out$estimate != 0,
    out$std_error / abs(out$estimate),
    NA_real_
  )
  out$cv <- if (isTRUE(cv_applicable)) out$relative_se else NA_real_
  valid <- is.finite(out$estimate) & is.finite(out$std_error) & out$std_error >= 0
  out$status <- ifelse(valid, "estimated", "not_estimable")
  out$reason <- ifelse(valid, NA_character_, "non_finite_design_estimate")
  out$sample_support_ok <- out$n_households >= rule$min_households[[1L]] &
    out$n_fsu >= rule$min_fsu[[1L]] &
    out$kish_effective_n >= rule$min_kish_effective_n[[1L]]
  max_rse <- rule$max_relative_se[[1L]]
  out$precision_ok <- if (is.finite(max_rse)) {
    valid & is.finite(out$relative_se) & out$relative_se <= max_rse
  } else {
    NA
  }
  out$preferred_eligible <- valid & out$sample_support_ok &
    if (is.finite(max_rse)) out$precision_ok else TRUE
  out$support_reason <- consumption_support_reason(out, rule, out$precision_ok)
  out <- out[c(
    "district_2001", "round_id", "outcome_id", "estimate", "std_error", "relative_se", "cv",
    "n_households", "n_fsu", "n_sample_person_equiv", "sum_person_weight", "kish_effective_n",
    "status", "reason", "sample_support_ok", "precision_ok", "preferred_eligible", "support_reason"
  )]
  out[order(out$district_2001), , drop = FALSE]
}

consumption_svyby_estimates <- function(result, survey_id, outcome_id) {
  estimate <- num(stats::coef(result))
  std_error <- num(survey::SE(result))
  result_df <- as.data.frame(result, stringsAsFactors = FALSE)
  if (!"target_unit_2001" %in% names(result_df)) {
    stop("survey::svyby returned an unexpected district-group schema.", call. = FALSE)
  }
  if (length(estimate) != nrow(result_df) || length(std_error) != nrow(result_df)) {
    stop("survey::svyby returned an unexpected coefficient/SE shape.", call. = FALSE)
  }
  data.frame(
    district_2001 = plain_chr(result_df$target_unit_2001),
    round_id = survey_id,
    outcome_id = outcome_id,
    estimate = estimate,
    std_error = std_error,
    stringsAsFactors = FALSE
  )
}

estimate_consumption_district_svymean <- function(rows, design, support, rule) {
  x <- rows
  x$.welfare_value <- consumption_welfare_value(x$real_mpce, rule$transform[[1L]])
  outcome_design <- update(design, .welfare_value = x$.welfare_value)
  result <- with_consumption_survey_adjustment(survey::svyby(
    ~.welfare_value,
    ~target_unit_2001,
    outcome_design,
    survey::svymean,
    na.rm = TRUE,
    vartype = "se",
    keep.names = FALSE,
    drop.empty.groups = FALSE
  ))
  survey_id <- unique(plain_chr(x$survey_id))[[1L]]
  estimates <- consumption_svyby_estimates(result, survey_id, rule$outcome_id[[1L]])
  consumption_finalize_district_estimate(
    estimates,
    support,
    rule,
    cv_applicable = identical(rule$transform[[1L]], "identity")
  )
}

estimate_consumption_district_svyquantile <- function(rows, design, support, rule) {
  x <- rows
  x$.welfare_value <- consumption_welfare_value(x$real_mpce, rule$transform[[1L]])
  outcome_design <- update(design, .welfare_value = x$.welfare_value)
  result <- with_consumption_survey_adjustment(survey::svyby(
    ~.welfare_value,
    ~target_unit_2001,
    outcome_design,
    survey::svyquantile,
    quantiles = rule$quantile[[1L]],
    ci = TRUE,
    interval.type = rule$quantile_interval[[1L]],
    qrule = rule$quantile_rule[[1L]],
    na.rm = TRUE,
    vartype = "se",
    keep.names = FALSE,
    drop.empty.groups = FALSE
  ))
  survey_id <- unique(plain_chr(x$survey_id))[[1L]]
  estimates <- consumption_svyby_estimates(result, survey_id, rule$outcome_id[[1L]])
  consumption_finalize_district_estimate(
    estimates,
    support,
    rule,
    cv_applicable = identical(rule$transform[[1L]], "identity")
  )
}


estimate_consumption_district_welfare <- function(lineaged_households, outcome_registry) {
  x <- consumption_design_rows(lineaged_households)
  survey_id <- unique(plain_chr(x$survey_id))
  if (length(survey_id) != 1L) stop("District welfare estimation requires one survey at a time.", call. = FALSE)
  registry <- safe_df(outcome_registry)
  if (!nrow(registry)) stop("Consumption welfare outcome registry is empty.", call. = FALSE)
  design <- consumption_survey_design_from_rows(x)
  support <- consumption_district_support_from_rows(x)
  safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    rule <- registry[i, , drop = FALSE]
    if (identical(rule$estimand[[1L]], "survey_mean")) {
      return(estimate_consumption_district_svymean(x, design, support, rule))
    }
    if (identical(rule$estimand[[1L]], "survey_quantile")) {
      return(estimate_consumption_district_svyquantile(x, design, support, rule))
    }
    stop("Unsupported registered consumption welfare estimand: ", rule$estimand[[1L]], call. = FALSE)
  }))
}

estimate_consumption_district_mean <- function(lineaged_households) {
  # Backward-compatible single-outcome wrapper used by focused tests and callers.
  registry <- data.frame(
    outcome_id = "real_mean_mpce", estimand = "survey_mean", transform = "identity", quantile = NA_real_,
    quantile_interval = "", quantile_rule = "", role = "primary",
    min_households = 1, min_fsu = 1, min_kish_effective_n = .Machine$double.eps,
    max_relative_se = NA_real_, stringsAsFactors = FALSE
  )
  estimate_consumption_district_welfare(lineaged_households, registry)
}

save_consumption_district_welfare <- function(
    outcomes, path = "outputs/diagnostics/public/consumption_district_welfare.csv") {
  write_diagnostic_csv(safe_df(outcomes), path)
}
