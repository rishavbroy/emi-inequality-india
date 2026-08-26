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

with_consumption_quantile_adjustment <- function(expr) {
  with_consumption_survey_adjustment(
    withCallingHandlers(
      expr,
      warning = function(w) {
        # survey::svyquantile documents NaN confidence limits when the
        # probability-scale interval cannot be inverted inside [0, 1].
        # The returned point estimate remains usable; downstream code converts
        # the non-finite SE into an explicit point-estimate-only status.
        if (identical(conditionMessage(w), "NaNs produced")) {
          invokeRestart("muffleWarning")
        }
      }
    )
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
    "outcome_id", "estimand", "transform", "quantile", "quantile_interval",
    "quantile_rule", "epsilon", "fgt_order", "role", "min_households",
    "min_fsu", "min_kish_effective_n", "max_relative_se"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Consumption welfare registry is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x) || anyDuplicated(x$outcome_id)) {
    stop("Consumption welfare registry must contain unique outcome_id rows.", call. = FALSE)
  }
  if (any(!x$estimand %in% c(
        "survey_mean", "survey_quantile", "survey_bottom_mean",
        "survey_gini", "survey_atkinson", "survey_fgt"
      )) ||
      any(!x$transform %in% c("identity", "log"))) {
    stop("Consumption welfare registry contains unsupported estimands or transforms.", call. = FALSE)
  }
  x$quantile <- suppressWarnings(as.numeric(x$quantile))
  mean_row <- x$estimand == "survey_mean"
  quantile_row <- x$estimand == "survey_quantile"
  bottom_mean_row <- x$estimand == "survey_bottom_mean"
  distribution_row <- x$estimand %in% c(
    "survey_gini", "survey_atkinson", "survey_fgt"
  )
  quantile_declared <- quantile_row | bottom_mean_row
  if (any((mean_row | distribution_row) & is.finite(x$quantile)) ||
      any(quantile_declared & (!is.finite(x$quantile) | x$quantile <= 0 | x$quantile >= 1))) {
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
  stray_quantile_method <- !quantile_row &
    (nzchar(x$quantile_interval) | nzchar(x$quantile_rule))
  if (any(invalid_quantile_method) || any(stray_quantile_method)) {
    stop("Consumption welfare registry contains invalid quantile uncertainty declarations.", call. = FALSE)
  }
  x$epsilon <- suppressWarnings(as.numeric(x$epsilon))
  atkinson_row <- x$estimand == "survey_atkinson"
  if (any(atkinson_row & (!is.finite(x$epsilon) | x$epsilon <= 0)) ||
      any(!atkinson_row & is.finite(x$epsilon))) {
    stop("Consumption welfare registry contains invalid Atkinson epsilon declarations.", call. = FALSE)
  }
  x$fgt_order <- suppressWarnings(as.numeric(x$fgt_order))
  fgt_row <- x$estimand == "survey_fgt"
  valid_fgt_order <- is.finite(x$fgt_order) &
    x$fgt_order %in% c(0, 1, 2) &
    x$fgt_order == floor(x$fgt_order)
  if (any(fgt_row & !valid_fgt_order) ||
      any(!fgt_row & is.finite(x$fgt_order))) {
    stop("Consumption welfare registry contains invalid FGT order declarations.", call. = FALSE)
  }
  if (any(distribution_row & x$transform != "identity")) {
    stop("Consumption distribution outcomes require the identity MPCE transform.", call. = FALSE)
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

consumption_sample_support_ok <- function(support, rule) {
  x <- safe_df(support)
  required <- c("n_households", "n_fsu", "kish_effective_n")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("District support is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  x$n_households >= rule$min_households[[1L]] &
    x$n_fsu >= rule$min_fsu[[1L]] &
    x$kish_effective_n >= rule$min_kish_effective_n[[1L]]
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
  if (!"uncertainty_requested" %in% names(out)) out$uncertainty_requested <- TRUE
  out$uncertainty_requested <- as.logical(out$uncertainty_requested)
  valid_point <- is.finite(out$estimate)
  valid_se <- is.finite(out$std_error) & out$std_error >= 0
  out$status <- ifelse(
    !valid_point,
    "not_estimable",
    ifelse(
      !out$uncertainty_requested | !valid_se,
      "point_estimate_only",
      "estimated"
    )
  )
  out$reason <- ifelse(
    !valid_point,
    "non_finite_point_estimate",
    ifelse(
      !out$uncertainty_requested,
      "uncertainty_not_requested_thin_support",
      ifelse(valid_se, NA_character_, "non_finite_design_uncertainty")
    )
  )
  out$sample_support_ok <- consumption_sample_support_ok(out, rule)
  max_rse <- rule$max_relative_se[[1L]]
  out$precision_ok <- if (is.finite(max_rse)) {
    ifelse(
      !out$uncertainty_requested | !valid_se | !is.finite(out$relative_se),
      NA,
      out$relative_se <= max_rse
    )
  } else {
    NA
  }
  inferentially_estimated <- valid_point & out$uncertainty_requested & valid_se
  out$preferred_eligible <- inferentially_estimated & out$sample_support_ok &
    if (is.finite(max_rse)) !is.na(out$precision_ok) & out$precision_ok else TRUE
  out$support_reason <- consumption_support_reason(out, rule, out$precision_ok)
  out <- out[c(
    "district_2001", "round_id", "outcome_id", "estimate", "std_error", "relative_se", "cv",
    "n_households", "n_fsu", "n_sample_person_equiv", "sum_person_weight", "kish_effective_n",
    "status", "reason", "uncertainty_requested", "sample_support_ok", "precision_ok", "preferred_eligible", "support_reason"
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

consumption_svyby_point_estimates <- function(result, survey_id, outcome_id) {
  estimate <- num(stats::coef(result))
  result_df <- as.data.frame(result, stringsAsFactors = FALSE)
  if (!"target_unit_2001" %in% names(result_df)) {
    stop("survey::svyby returned an unexpected district-group schema.", call. = FALSE)
  }
  if (length(estimate) != nrow(result_df)) {
    stop("survey::svyby returned an unexpected coefficient shape.", call. = FALSE)
  }
  data.frame(
    district_2001 = plain_chr(result_df$target_unit_2001),
    round_id = survey_id,
    outcome_id = outcome_id,
    estimate = estimate,
    std_error = NA_real_,
    uncertainty_requested = FALSE,
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
  estimates$uncertainty_requested <- TRUE
  consumption_finalize_district_estimate(
    estimates,
    support,
    rule,
    cv_applicable = identical(rule$transform[[1L]], "identity")
  )
}

consumption_bottom_mean_stat <- function(design, alpha) {
  need_pkg("convey", "design-based lower-tail welfare estimates")
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Bottom-share mean alpha must lie strictly between zero and one.", call. = FALSE)
  }

  lower <- convey::svyisq(
    ~.welfare_value,
    design = design,
    alpha = alpha,
    na.rm = TRUE,
    linearized = TRUE
  )
  lower_value <- num(stats::coef(lower))[[1L]]
  lower_linearized <- as.numeric(attr(lower, "linearized"))
  sampling_weight <- num(stats::weights(design, type = "sampling"))
  population_value <- sum(sampling_weight)

  if (!is.finite(lower_value) || !length(lower_linearized) ||
      length(lower_linearized) != length(sampling_weight) ||
      !is.finite(population_value) || population_value <= 0) {
    stop("Lower-tail welfare linearization returned an invalid design object.", call. = FALSE)
  }

  # convey::svyisq() supplies the design-linearized total below q_alpha.
  # Divide by alpha and then use convey::contrastinf() so uncertainty in the
  # represented-person denominator is retained in the bottom-share mean.
  contrast <- convey::contrastinf(
    quote(LOWER / POPULATION),
    list(
      LOWER = list(
        value = lower_value / alpha,
        lin = lower_linearized / alpha
      ),
      POPULATION = list(
        value = population_value,
        lin = rep(1, length(sampling_weight))
      )
    )
  )
  variance_total <- survey::svytotal(contrast$lin, design)
  standard_error <- num(survey::SE(variance_total))[[1L]]

  c(
    estimate = num(contrast$value)[[1L]],
    std_error = standard_error
  )
}

estimate_consumption_district_convey_domains <- function(
    rows, design, support, rule, statistic, cv_applicable = FALSE) {
  need_pkg("convey", "design-based consumption distribution estimates")
  x <- rows
  x$.welfare_value <- consumption_welfare_value(
    x$real_mpce, rule$transform[[1L]]
  )
  # convey_prep() stores the full design before domain subsetting, as required
  # by convey's linearized distributional estimators.
  outcome_design <- convey::convey_prep(
    update(design, .welfare_value = x$.welfare_value)
  )
  survey_id <- unique(plain_chr(x$survey_id))[[1L]]
  outcome_id <- rule$outcome_id[[1L]]

  estimates <- safe_bind_rows(lapply(
    plain_chr(support$district_2001),
    function(district_id) {
      domain_design <- subset(
        outcome_design,
        target_unit_2001 == district_id
      )
      stat <- with_consumption_survey_adjustment(
        statistic(domain_design, rule)
      )
      data.frame(
        district_2001 = district_id,
        round_id = survey_id,
        outcome_id = outcome_id,
        estimate = stat[["estimate"]],
        std_error = stat[["std_error"]],
        uncertainty_requested = TRUE,
        stringsAsFactors = FALSE
      )
    }
  ))

  consumption_finalize_district_estimate(
    estimates,
    support,
    rule,
    cv_applicable = cv_applicable
  )
}

estimate_consumption_district_bottom_mean <- function(rows, design, support, rule) {
  if (!identical(rule$transform[[1L]], "identity")) {
    stop("Bottom-share welfare means currently require the identity MPCE transform.", call. = FALSE)
  }
  estimate_consumption_district_convey_domains(
    rows, design, support, rule,
    statistic = function(domain_design, rule) {
      consumption_bottom_mean_stat(domain_design, rule$quantile[[1L]])
    },
    cv_applicable = TRUE
  )
}

consumption_distribution_stat <- function(design, rule) {
  estimand <- plain_chr(rule$estimand[[1L]])
  result <- switch(
    estimand,
    survey_gini = convey::svygini(
      ~.welfare_value, design = design, na.rm = TRUE
    ),
    survey_atkinson = convey::svyatk(
      ~.welfare_value, design = design,
      epsilon = rule$epsilon[[1L]], na.rm = TRUE
    ),
    survey_fgt = convey::svyfgt(
      ~.welfare_value,
      design = design,
      g = as.integer(rule$fgt_order[[1L]]),
      type_thresh = "abs",
      abs_thresh = tendulkar_real_poverty_line(),
      na.rm = TRUE
    ),
    stop("Unsupported convey distribution estimand: ", estimand, call. = FALSE)
  )
  c(
    estimate = num(stats::coef(result))[[1L]],
    std_error = num(survey::SE(result))[[1L]]
  )
}

estimate_consumption_district_distribution <- function(rows, design, support, rule) {
  if (!identical(rule$transform[[1L]], "identity")) {
    stop("Consumption distribution outcomes require the identity MPCE transform.", call. = FALSE)
  }
  estimate_consumption_district_convey_domains(
    rows, design, support, rule,
    statistic = consumption_distribution_stat,
    cv_applicable = FALSE
  )
}

estimate_consumption_district_svyquantile <- function(rows, design, support, rule) {
  x <- rows
  x$.welfare_value <- consumption_welfare_value(x$real_mpce, rule$transform[[1L]])
  outcome_design <- update(design, .welfare_value = x$.welfare_value)
  survey_id <- unique(plain_chr(x$survey_id))[[1L]]
  outcome_id <- rule$outcome_id[[1L]]

  # Partition domains before estimation so every district's quantile is
  # computed exactly once. Supported domains request point + design uncertainty;
  # thin domains request the descriptive point estimate only.
  support_ok <- consumption_sample_support_ok(support, rule)
  supported <- support$district_2001[support_ok]
  thin <- support$district_2001[!support_ok]
  pieces <- list()

  if (length(supported)) {
    supported_design <- subset(outcome_design, target_unit_2001 %in% supported)
    interval_result <- with_consumption_quantile_adjustment(survey::svyby(
      ~.welfare_value,
      ~target_unit_2001,
      supported_design,
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
    supported_estimates <- consumption_svyby_estimates(interval_result, survey_id, outcome_id)
    supported_estimates$uncertainty_requested <- TRUE
    pieces[[length(pieces) + 1L]] <- supported_estimates
  }

  if (length(thin)) {
    thin_design <- subset(outcome_design, target_unit_2001 %in% thin)
    point_result <- with_consumption_survey_adjustment(survey::svyby(
      ~.welfare_value,
      ~target_unit_2001,
      thin_design,
      survey::svyquantile,
      quantiles = rule$quantile[[1L]],
      ci = FALSE,
      se = FALSE,
      qrule = rule$quantile_rule[[1L]],
      na.rm = TRUE,
      keep.var = FALSE,
      keep.names = FALSE,
      drop.empty.groups = FALSE
    ))
    pieces[[length(pieces) + 1L]] <- consumption_svyby_point_estimates(
      point_result, survey_id, outcome_id
    )
  }

  estimates <- safe_bind_rows(pieces)
  expected <- sort(unique(plain_chr(support$district_2001)))
  returned <- sort(unique(plain_chr(estimates$district_2001)))
  if (!identical(returned, expected)) {
    stop("Quantile estimation did not return exactly one estimate per district domain.", call. = FALSE)
  }

  consumption_finalize_district_estimate(
    estimates,
    support,
    rule,
    cv_applicable = FALSE
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
    if (identical(rule$estimand[[1L]], "survey_bottom_mean")) {
      return(estimate_consumption_district_bottom_mean(x, design, support, rule))
    }
    if (rule$estimand[[1L]] %in% c(
        "survey_gini", "survey_atkinson", "survey_fgt"
      )) {
      return(estimate_consumption_district_distribution(x, design, support, rule))
    }
    stop("Unsupported registered consumption welfare estimand: ", rule$estimand[[1L]], call. = FALSE)
  }))
}

estimate_consumption_district_mean <- function(lineaged_households) {
  # Backward-compatible single-outcome wrapper used by focused tests and callers.
  registry <- data.frame(
    outcome_id = "real_mean_mpce", estimand = "survey_mean", transform = "identity", quantile = NA_real_,
    quantile_interval = "", quantile_rule = "", epsilon = NA_real_,
    fgt_order = NA_real_, role = "primary",
    min_households = 1, min_fsu = 1, min_kish_effective_n = .Machine$double.eps,
    max_relative_se = NA_real_, stringsAsFactors = FALSE
  )
  estimate_consumption_district_welfare(lineaged_households, registry)
}

save_consumption_district_welfare <- function(
    outcomes, path = "outputs/diagnostics/public/consumption_district_welfare.csv") {
  write_diagnostic_csv(safe_df(outcomes), path)
}
