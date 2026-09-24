# Cross-family analysis-design ontology.
#
# Specialized registries remain authoritative for estimation. This module only
# projects them onto one common schema so the repository can inventory what is
# actually implemented without constructing an indiscriminate Cartesian product
# of outcomes, treatments, instruments, controls, and estimators.

analysis_design_columns <- function() {
  c(
    "analysis_id", "family", "specification_id", "outcome", "outcome_construct_id",
    "baseline_construct_id", "treatment", "treatment_construct_id",
    "effect_modifier", "effect_modifier_construct_id",
    "instrument", "instrument_construct_ids", "instrument_vintage", "distance_measure_id",
    "language_adjustment_id", "adjustment_set", "control_strategy_id",
    "control_parameterization_id", "fixed_effect", "functional_form_id",
    "estimand", "estimator", "estimation_scope_id", "inference",
    "covariance_id", "weak_id_inference_id", "multiplicity_id",
    "sample_rule", "support_policy_id", "analysis_role",
    "admissible", "reason", "implemented"
  )
}

analysis_estimation_scope_registry <- function() {
  c(
    first_stage_diagnostic_suite = "first_stage_only",
    first_stage_comparison = "first_stage_only",
    iv_diagnostic_suite = "iv_diagnostic_suite",
    `2sls` = "structural_iv",
    `2sls_with_anderson_rubin` = "structural_iv",
    `first_stage+reduced_form+2sls+anderson_rubin` = "structural_iv",
    `reduced_form+2sls+anderson_rubin` = "structural_iv",
    bounded_exclusion_ar = "structural_iv_sensitivity",
    falsification_adaptive_set = "structural_iv_sensitivity",
    ols = "ols",
    native_speaker_weighted_ols = "weighted_ols",
    mother_tongue_speaker_weighted_ols = "weighted_ols",
    descriptive_mean = "descriptive_summary"
  )
}

analysis_inference_registry <- function() {
  data.frame(
    inference = c(
      "none", "HC1", "state_clustered", "state_1991_clustered",
      "state_clustered+holm",
      "state_clustered+anderson_rubin",
      "state_clustered+anderson_rubin+holm",
      "state_clustered+weak_iv_diagnostics",
      "state_clustered+bounded_exclusion_ar"
    ),
    covariance_id = c(
      "none", "HC1", "state_clustered", "state_1991_clustered",
      "state_clustered", "state_clustered", "state_clustered",
      "state_clustered", "state_clustered"
    ),
    weak_id_inference_id = c(
      "none", "none", "none", "none", "none",
      "anderson_rubin", "anderson_rubin", "weak_iv_diagnostic_suite",
      "bounded_exclusion_ar"
    ),
    multiplicity_id = c(
      "none", "none", "none", "none", "holm", "none", "holm", "none", "none"
    ),
    stringsAsFactors = FALSE
  )
}

analysis_functional_form_from_estimand <- function(estimand) {
  x <- plain_chr(estimand)
  out <- rep("linear", length(x))
  out[x %in% c("ancova", "descriptive_ancova")] <- "ancova"
  out[x %in% c("change", "descriptive_change")] <- "change"
  out[grepl("interaction", x, fixed = TRUE)] <- "linear_interaction"
  out
}

analysis_support_policy <- function(sample_rule) {
  x <- plain_chr(sample_rule)
  out <- rep(NA_character_, length(x))
  out[grepl("common_support$", x)] <- "common_support"
  out[grepl("fixed_complete_case$", x)] <- "fixed_complete_case"
  out[x == "public_model_specific_complete_case"] <- "model_specific_complete_case"
  out[grepl("^c17_", x)] <- "predefined_demographic_subgroup"
  out[grepl("^(social_group_gap|st_concentration)__", x)] <- "predefined_geographic_subgroup"
  out[grepl("^social_group_crosscut__", x)] <- "predefined_demographic_subgroup"
  out[grepl("^validated_", x)] <- "validated_source_sample"
  out[x == "historical_preferred_geography"] <- "historical_preferred_geography"
  out[x == "analysis_welfare_support"] <- "analysis_welfare_support"
  if (anyNA(out)) {
    stop(
      "Unregistered analysis sample-rule semantics: ",
      paste(unique(x[is.na(out)]), collapse = ", "),
      call. = FALSE
    )
  }
  out
}

analysis_design_frame <- function(...) {
  out <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  if (!"effect_modifier" %in% names(out)) out$effect_modifier <- rep("", nrow(out))
  for (nm in c(
      "outcome_construct_id", "baseline_construct_id",
      "treatment_construct_id", "effect_modifier_construct_id", "instrument_construct_ids",
      "distance_measure_id", "language_adjustment_id",
      "control_strategy_id", "control_parameterization_id", "functional_form_id"
  )) {
    if (!nm %in% names(out)) out[[nm]] <- rep("", nrow(out))
  }
  for (nm in c(
      "estimation_scope_id", "covariance_id", "weak_id_inference_id",
      "multiplicity_id", "support_policy_id"
  )) {
    if (!nm %in% names(out)) out[[nm]] <- rep("", nrow(out))
  }
  semantic_inputs <- c("estimand", "estimator", "inference", "sample_rule")
  if (nrow(out) && all(semantic_inputs %in% names(out))) {
    blank_form <- !nzchar(plain_chr(out$functional_form_id))
    out$functional_form_id[blank_form] <- analysis_functional_form_from_estimand(
      out$estimand[blank_form]
    )

    scopes <- analysis_estimation_scope_registry()
    estimator <- plain_chr(out$estimator)
    unknown_estimator <- setdiff(unique(estimator), names(scopes))
    if (length(unknown_estimator)) {
      stop(
        "Unregistered analysis estimator semantics: ",
        paste(unknown_estimator, collapse = ", "), call. = FALSE
      )
    }
    out$estimation_scope_id <- unname(scopes[estimator])

    inference_registry <- analysis_inference_registry()
    inference <- plain_chr(out$inference)
    match_idx <- match(inference, inference_registry$inference)
    if (anyNA(match_idx)) {
      stop(
        "Unregistered analysis inference semantics: ",
        paste(unique(inference[is.na(match_idx)]), collapse = ", "),
        call. = FALSE
      )
    }
    out$covariance_id <- inference_registry$covariance_id[match_idx]
    out$weak_id_inference_id <- inference_registry$weak_id_inference_id[match_idx]
    out$multiplicity_id <- inference_registry$multiplicity_id[match_idx]
    out$support_policy_id <- analysis_support_policy(out$sample_rule)
  }
  missing <- setdiff(analysis_design_columns(), names(out))
  if (length(missing)) {
    stop(
      "Analysis-design rows are missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  out <- out[analysis_design_columns()]
  if (nrow(out) && (anyDuplicated(out$analysis_id) || any(!nzchar(out$analysis_id)))) {
    stop("Analysis-design analysis_id values must be nonempty and unique.", call. = FALSE)
  }
  if (nrow(out) && any(!out$admissible & !nzchar(out$reason))) {
    stop("Non-admissible analysis designs require an explicit reason.", call. = FALSE)
  }
  out
}


analysis_design_resolve_construct_ids <- function(values, registry, explicit_ids = NULL) {
  x <- analysis_construct_frame(safe_df(registry))
  values <- plain_chr(values)
  explicit <- if (is.null(explicit_ids)) rep("", length(values)) else plain_chr(explicit_ids)
  if (length(explicit) != length(values)) {
    stop("Explicit construct references must align with analysis-design rows.", call. = FALSE)
  }

  resolve_value <- function(value, declared) {
    tokens <- trimws(strsplit(value, ";", fixed = TRUE)[[1L]])
    tokens <- tokens[nzchar(tokens)]
    if (nzchar(declared)) {
      ids <- trimws(strsplit(declared, ";", fixed = TRUE)[[1L]])
      if (any(!nzchar(ids)) || any(!ids %in% x$construct_id)) {
        stop("Invalid explicit construct reference: ", declared, call. = FALSE)
      }
      return(paste(ids, collapse = ";"))
    }
    if (!length(tokens)) return("")
    ids <- vapply(tokens, function(token) {
      hits <- which(x$variable == token)
      if (!length(hits)) return("")
      if (length(hits) > 1L) {
        stop(
          "Analysis design contains an ambiguous construct variable: ", token,
          ". Supply an explicit construct_id reference.", call. = FALSE
        )
      }
      x$construct_id[[hits]]
    }, character(1))
    if (any(!nzchar(ids))) return("")
    paste(ids, collapse = ";")
  }

  vapply(seq_along(values), function(i) resolve_value(values[[i]], explicit[[i]]), character(1))
}

link_analysis_design_constructs <- function(designs, construct_registry) {
  out <- analysis_design_frame(safe_df(designs))
  if (!nrow(out)) return(out)
  out$outcome_construct_id <- analysis_design_resolve_construct_ids(
    out$outcome, construct_registry, out$outcome_construct_id
  )
  out$baseline_construct_id <- analysis_design_resolve_construct_ids(
    rep("", nrow(out)), construct_registry, out$baseline_construct_id
  )
  out$treatment_construct_id <- analysis_design_resolve_construct_ids(
    out$treatment, construct_registry, out$treatment_construct_id
  )
  out$effect_modifier_construct_id <- analysis_design_resolve_construct_ids(
    out$effect_modifier, construct_registry, out$effect_modifier_construct_id
  )
  out$instrument_construct_ids <- analysis_design_resolve_construct_ids(
    out$instrument, construct_registry, out$instrument_construct_ids
  )
  out
}

analysis_design_from_iv <- function(
    specifications,
    family,
    estimator,
    estimand = "iv_design",
    inference = "state_clustered",
    analysis_role = "diagnostic",
    instrument_vintage = "2001",
    admissible = TRUE,
    reason = "registered_existing_design") {
  specs <- as_iv_specifications(specifications)
  if (!nrow(specs)) return(analysis_design_frame(
    analysis_id = character(), family = character(), specification_id = character(),
    outcome = character(), treatment = character(), instrument = character(),
    instrument_vintage = character(), adjustment_set = character(),
    control_strategy_id = character(), control_parameterization_id = character(),
    fixed_effect = character(), functional_form_id = character(),
    estimand = character(), estimator = character(),
    inference = character(), sample_rule = character(), analysis_role = character(),
    admissible = logical(), reason = character(), implemented = logical()
  ))

  estimand_value <- if (length(estimand) == 1L) rep(estimand, nrow(specs)) else estimand
  if ("estimand" %in% names(specs)) estimand_value <- plain_chr(specs$estimand)
  role_value <- if (length(analysis_role) == 1L) rep(analysis_role, nrow(specs)) else analysis_role
  vintage_value <- if (length(instrument_vintage) == 1L) {
    rep(instrument_vintage, nrow(specs))
  } else instrument_vintage
  distance_measure_value <- if ("distance_measure_id" %in% names(specs)) {
    plain_chr(specs$distance_measure_id)
  } else {
    rep("", nrow(specs))
  }
  language_adjustment_value <- if ("language_adjustment_id" %in% names(specs)) {
    plain_chr(specs$language_adjustment_id)
  } else {
    rep("", nrow(specs))
  }
  control_strategy_value <- if ("control_strategy_id" %in% names(specs)) {
    plain_chr(specs$control_strategy_id)
  } else {
    rep("", nrow(specs))
  }
  control_parameterization_value <- if ("control_parameterization_id" %in% names(specs)) {
    plain_chr(specs$control_parameterization_id)
  } else {
    rep("", nrow(specs))
  }
  functional_form_value <- if ("estimand" %in% names(specs)) {
    plain_chr(specs$estimand)
  } else {
    rep("linear", nrow(specs))
  }
  outcome_construct_value <- if (all(c("welfare_outcome_id", "outcome_round") %in% names(specs))) {
    paste("consumption", plain_chr(specs$outcome_round), plain_chr(specs$welfare_outcome_id), sep = "__")
  } else {
    rep("", nrow(specs))
  }
  baseline_construct_value <- if (all(c("welfare_outcome_id", "baseline_round") %in% names(specs))) {
    paste("consumption", plain_chr(specs$baseline_round), plain_chr(specs$welfare_outcome_id), sep = "__")
  } else {
    rep("", nrow(specs))
  }
  analysis_id_value <- if ("analysis_id" %in% names(specs)) {
    ids <- plain_chr(specs$analysis_id)
    expected_ids <- iv_analysis_id(family, specs$specification_id)
    if (!identical(ids, expected_ids)) {
      stop("Canonical IV specification analysis_id values disagree with the design family.", call. = FALSE)
    }
    ids
  } else {
    iv_analysis_id(family, specs$specification_id)
  }

  analysis_design_frame(
    analysis_id = analysis_id_value,
    family = rep(family, nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = plain_chr(specs$outcome),
    outcome_construct_id = outcome_construct_value,
    baseline_construct_id = baseline_construct_value,
    treatment = plain_chr(specs$treatment),
    instrument = vapply(
      specs$excluded_instruments,
      function(x) paste(plain_chr(unlist(x, use.names = FALSE)), collapse = ";"),
      character(1)
    ),
    instrument_vintage = vintage_value,
    distance_measure_id = distance_measure_value,
    language_adjustment_id = language_adjustment_value,
    adjustment_set = plain_chr(specs$adjustment_id),
    control_strategy_id = control_strategy_value,
    control_parameterization_id = control_parameterization_value,
    fixed_effect = plain_chr(specs$fixed_effect),
    functional_form_id = functional_form_value,
    estimand = estimand_value,
    estimator = rep(estimator, nrow(specs)),
    inference = rep(inference, nrow(specs)),
    sample_rule = plain_chr(specs$sample_rule),
    analysis_role = role_value,
    admissible = rep(isTRUE(admissible), nrow(specs)),
    reason = rep(reason, nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_public_iv <- function(specifications) {
  specs <- as_iv_specifications(specifications)
  roles <- if ("analysis_role" %in% names(specs)) {
    plain_chr(specs$analysis_role)
  } else {
    rep("public_registered", nrow(specs))
  }
  analysis_design_from_iv(
    specs,
    family = "public_iv",
    estimator = "2sls",
    estimand = "public_consumption_model",
    inference = "state_clustered",
    analysis_role = roles,
    reason = "registered_public_iv_design"
  )
}

analysis_design_district_mechanisms <- function(
    measure_registry,
    control_registry = NULL,
    instrument = preferred_iv_variables()$instrument) {
  measures <- preferred_district_mechanism_registry(measure_registry)
  adjustments <- district_mechanism_adjustment_registry(control_registry)
  rows <- lapply(seq_len(nrow(measures)), function(i) {
    measure <- measures[i, , drop = FALSE]
    analysis_design_frame(
      analysis_id = paste(
        "district_mechanism", measure$measure_id[[1L]], adjustments$specification_id,
        sep = "__"
      ),
      family = rep("district_mechanism", nrow(adjustments)),
      specification_id = paste(measure$measure_id[[1L]], adjustments$specification_id, sep = "__"),
      outcome = rep(measure$variable[[1L]], nrow(adjustments)),
      treatment = rep("", nrow(adjustments)),
      instrument = rep(instrument, nrow(adjustments)),
      instrument_vintage = rep("2001", nrow(adjustments)),
      adjustment_set = plain_chr(adjustments$specification_id),
      fixed_effect = plain_chr(adjustments$fixed_effect),
      estimand = rep("reduced_form_association", nrow(adjustments)),
      estimator = rep("ols", nrow(adjustments)),
      inference = rep("state_clustered", nrow(adjustments)),
      sample_rule = rep("outcome_fixed_complete_case", nrow(adjustments)),
      analysis_role = rep(plain_chr(measure$paper_role[[1L]]), nrow(adjustments)),
      admissible = rep(TRUE, nrow(adjustments)),
      reason = rep("predeclared_compact_mechanism_grid", nrow(adjustments)),
      implemented = rep(TRUE, nrow(adjustments))
    )
  })
  safe_bind_rows(rows)
}

analysis_design_c17 <- function(registry = census_c17_mechanism_registry()) {
  x <- safe_df(registry)
  analysis_design_frame(
    analysis_id = plain_chr(x$analysis_id),
    family = rep("c17_mechanism", nrow(x)),
    specification_id = plain_chr(x$specification_id),
    outcome = plain_chr(x$outcome),
    treatment = rep("", nrow(x)),
    instrument = plain_chr(x$distance_variable),
    instrument_vintage = rep("time_invariant_language_basis", nrow(x)),
    adjustment_set = rep("state_language_controls", nrow(x)),
    fixed_effect = rep("state", nrow(x)),
    functional_form_id = paste0("distance_", plain_chr(x$distance_form)),
    estimand = rep("language_behavior_association", nrow(x)),
    estimator = rep("native_speaker_weighted_ols", nrow(x)),
    inference = rep("HC1", nrow(x)),
    sample_rule = paste0(
      "c17_", plain_chr(x$sample), "_", tolower(plain_chr(x$sex))
    ),
    analysis_role = ifelse(x$preferred %in% TRUE, "preferred_mechanism", "robustness"),
    admissible = rep(TRUE, nrow(x)),
    reason = rep("registered_c17_mechanism_design", nrow(x)),
    implemented = rep(TRUE, nrow(x))
  )
}

analysis_design_dise <- function(
    constructs = dise_construct_registry(),
    control_registry = NULL,
    outcome = "real_log_consumption_change") {
  constructs <- safe_df(constructs)
  first_stage <- safe_bind_rows(lapply(seq_len(nrow(constructs)), function(i) {
    construct <- constructs[i, , drop = FALSE]
    specs <- iv_diagnostic_specification_registry(
      outcome = outcome,
      treatment = construct$variable[[1L]],
      control_registry = control_registry
    )
    rows <- analysis_design_from_iv(
      specs,
      family = paste0("dise_first_stage__", construct$construct_id[[1L]]),
      estimator = "first_stage_diagnostic_suite",
      estimand = "first_stage_relevance",
      analysis_role = plain_chr(construct$paper_role[[1L]])
    )
    rows$family <- "dise_first_stage"
    rows$analysis_id <- dise_analysis_id(
      "dise_first_stage", construct$construct_id[[1L]], rows$specification_id
    )
    rows
  }))

  structural <- constructs[constructs$analysis_scope == "structural_iv", , drop = FALSE]
  weak_iv <- safe_bind_rows(lapply(seq_len(nrow(structural)), function(i) {
    construct <- structural[i, , drop = FALSE]
    specs <- iv_diagnostic_specification_registry(
      outcome = outcome,
      treatment = construct$variable[[1L]],
      control_registry = control_registry
    )
    rows <- analysis_design_from_iv(
      specs,
      family = paste0("dise_weak_iv__", construct$construct_id[[1L]]),
      estimator = "2sls_with_anderson_rubin",
      estimand = "weak_iv_structural",
      inference = "state_clustered+anderson_rubin",
      analysis_role = plain_chr(construct$paper_role[[1L]])
    )
    rows$family <- "dise_weak_iv"
    rows$analysis_id <- dise_analysis_id(
      "dise_weak_iv", construct$construct_id[[1L]], rows$specification_id
    )
    rows
  }))
  safe_bind_rows(list(first_stage, weak_iv))
}

analysis_design_hindi_belt_first_stage <- function(control_registry = NULL) {
  analysis_design_from_iv(
    iv_hindi_belt_first_stage_specifications(control_registry = control_registry),
    family = "hindi_belt_first_stage",
    estimator = "first_stage_comparison",
    estimand = "first_stage_relevance",
    inference = "state_clustered",
    analysis_role = "regional_institutional_robustness",
    reason = "registered_shastry_hindi_belt_comparison"
  )
}

analysis_design_child_population_first_stage <- function(control_registry = NULL) {
  analysis_design_from_iv(
    iv_child_population_first_stage_specifications(control_registry = control_registry),
    family = "child_population_first_stage",
    estimator = "first_stage_comparison",
    estimand = "first_stage_relevance",
    inference = "state_clustered",
    analysis_role = "shastry_demographic_robustness",
    reason = "registered_shastry_child_population_comparison"
  )
}

analysis_design_census_mechanisms <- function(control_registry = NULL) {
  families <- list(
    migration = list(
      registry = census_migration_mechanism_registry(),
      sample_rule = "migration_mechanism_common_support"
    ),
    housing = list(
      registry = census_housing_mechanism_registry(),
      sample_rule = "housing_change_mechanism_common_support"
    )
  )
  safe_bind_rows(lapply(names(families), function(source) {
    registry <- families[[source]]$registry
    safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
      outcome <- registry$variable[[i]]
      specs <- posttreatment_mechanism_specifications(
        outcome = outcome,
        sample_rule = families[[source]]$sample_rule,
        control_registry = control_registry
      )
      rows <- analysis_design_from_iv(
        specs,
        family = paste0("census_", source, "__", registry$outcome_id[[i]]),
        estimator = "reduced_form+2sls+anderson_rubin",
        estimand = "post_treatment_mechanism",
        inference = "state_clustered+anderson_rubin",
        analysis_role = plain_chr(registry$mechanism_family[[i]])
      )
      rows$family <- paste0("census_", source, "_mechanism")
      rows$analysis_id <- posttreatment_mechanism_analysis_id(
        paste("census", source, sep = "__"),
        registry$outcome_id[[i]],
        rows$specification_id
      )
      rows
    }))
  }))
}


analysis_design_census_migration_hindi_belt <- function(control_registry = NULL) {
  spec <- census_migration_hindi_belt_skilled_specification(control_registry)
  outcome <- census_migration_mechanism_registry()
  outcome <- outcome[outcome$outcome_id == "skilled_recent_work_migration", , drop = FALSE]
  analysis_design_frame(
    analysis_id = posttreatment_mechanism_analysis_id(
      "census__migration_hindi_belt", outcome$outcome_id[[1L]], spec$specification_id[[1L]]
    ),
    family = "census_migration_hindi_belt",
    specification_id = plain_chr(spec$specification_id),
    outcome = plain_chr(outcome$variable),
    treatment = "",
    instrument = preferred_iv_variables()$instrument,
    instrument_vintage = "2001",
    distance_measure_id = plain_chr(spec$distance_measure_id),
    language_adjustment_id = plain_chr(spec$language_adjustment_id),
    adjustment_set = plain_chr(spec$adjustment_id),
    control_strategy_id = plain_chr(spec$control_strategy_id),
    control_parameterization_id = plain_chr(spec$control_parameterization_id),
    fixed_effect = plain_chr(spec$fixed_effect),
    functional_form_id = "linear",
    estimand = "hindi_belt_skilled_migration_reduced_form",
    estimator = "ols",
    inference = "state_clustered",
    sample_rule = plain_chr(spec$sample_rule),
    analysis_role = "geographic_robustness",
    admissible = TRUE,
    reason = "predeclared_hindi_belt_skilled_migration_restriction",
    implemented = TRUE
  )
}

analysis_design_economic_census_mechanisms <- function(control_registry = NULL) {
  registry <- economic_census_mechanism_registry()
  safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    specs <- economic_census_mechanism_specifications(
      outcome = registry$variable[[i]],
      control_registry = control_registry
    )
    rows <- analysis_design_from_iv(
      specs,
      family = paste0("economic_census__", registry$outcome_id[[i]]),
      estimator = "reduced_form+2sls+anderson_rubin",
      estimand = "post_treatment_firm_mechanism",
      inference = "state_clustered+anderson_rubin",
      analysis_role = plain_chr(registry$mechanism_family[[i]])
    )
    rows$family <- "economic_census_mechanism"
    rows$analysis_id <- posttreatment_mechanism_analysis_id(
      "economic_census", registry$outcome_id[[i]], rows$specification_id
    )
    rows
  }))
}

analysis_design_labor_mechanisms <- function(control_registry = NULL) {
  designs <- data.frame(
    wave_id = c("nss66", "plfs_2017_18", "plfs_2017_18"),
    sample_suffix = c("primary", "primary", "conservative"),
    analysis_role = c("early_post_mechanism", "long_run_mechanism", "geography_robustness"),
    stringsAsFactors = FALSE
  )
  safe_bind_rows(lapply(seq_len(nrow(designs)), function(j) {
    design <- designs[j, , drop = FALSE]
    registry <- labor_mechanism_registry(design$wave_id[[1L]])
    safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
      specs <- labor_mechanism_specifications(
        wave_id = design$wave_id[[1L]],
        outcome = registry$variable[[i]],
        control_registry = control_registry,
        sample_suffix = design$sample_suffix[[1L]]
      )
      rows <- analysis_design_from_iv(
        specs,
        family = paste0(
          "labor__", design$wave_id[[1L]], "__", design$sample_suffix[[1L]],
          "__", registry$outcome_id[[i]]
        ),
        estimator = "reduced_form+2sls+anderson_rubin",
        estimand = "post_treatment_labor_mechanism",
        inference = "state_clustered+anderson_rubin",
        analysis_role = design$analysis_role[[1L]]
      )
      rows$family <- "labor_mechanism"
      rows$outcome_construct_id <- registry$construct_id[[i]]
      rows$analysis_id <- posttreatment_mechanism_analysis_id(
        paste("labor", design$wave_id[[1L]], design$sample_suffix[[1L]], sep = "__"),
        registry$outcome_id[[i]],
        rows$specification_id
      )
      rows
    }))
  }))
}

analysis_design_historical_first_stages <- function(control_registry = NULL) {
  registry <- historical_linguistic_first_stage_registry(control_registry)
  instruments <- c(
    historical_1991 = "ling_distance_nonzero_mean_1991",
    census_2001 = "ling_distance_nonzero_mean_2001"
  )
  regular <- safe_bind_rows(lapply(names(instruments), function(vintage) {
    analysis_design_frame(
      analysis_id = paste("historical_first_stage", vintage, registry$specification_id, sep = "__"),
      family = rep("historical_first_stage", nrow(registry)),
      specification_id = paste(vintage, registry$specification_id, sep = "__"),
      outcome = rep(preferred_iv_variables()$treatment, nrow(registry)),
      treatment = rep(preferred_iv_variables()$treatment, nrow(registry)),
      instrument = rep(instruments[[vintage]], nrow(registry)),
      instrument_vintage = rep(vintage, nrow(registry)),
      adjustment_set = plain_chr(registry$specification_id),
      fixed_effect = plain_chr(registry$fixed_effect),
      estimand = rep("first_stage_relevance", nrow(registry)),
      estimator = rep("ols", nrow(registry)),
      inference = rep("state_clustered", nrow(registry)),
      sample_rule = rep("historical_preferred_geography", nrow(registry)),
      analysis_role = rep("historical_robustness", nrow(registry)),
      admissible = rep(TRUE, nrow(registry)),
      reason = rep("registered_historical_vintage_comparison", nrow(registry)),
      implemented = rep(TRUE, nrow(registry))
    )
  }))

  predetermined <- historical_linguistic_predetermined_first_stage_registry()
  predetermined_rows <- safe_bind_rows(lapply(names(instruments), function(vintage) {
    analysis_design_frame(
      analysis_id = paste("historical_predetermined", vintage, predetermined$specification_id, sep = "__"),
      family = rep("historical_predetermined_first_stage", nrow(predetermined)),
      specification_id = paste(vintage, predetermined$specification_id, sep = "__"),
      outcome = rep(preferred_iv_variables()$treatment, nrow(predetermined)),
      treatment = rep(preferred_iv_variables()$treatment, nrow(predetermined)),
      instrument = rep(instruments[[vintage]], nrow(predetermined)),
      instrument_vintage = rep(vintage, nrow(predetermined)),
      adjustment_set = plain_chr(predetermined$specification_id),
      fixed_effect = plain_chr(predetermined$fixed_effect),
      estimand = rep("first_stage_relevance", nrow(predetermined)),
      estimator = rep("ols", nrow(predetermined)),
      inference = rep("state_1991_clustered", nrow(predetermined)),
      sample_rule = rep("historical_preferred_geography", nrow(predetermined)),
      analysis_role = rep("predetermined_historical_robustness", nrow(predetermined)),
      admissible = rep(TRUE, nrow(predetermined)),
      reason = rep("registered_predetermined_1991_design", nrow(predetermined)),
      implemented = rep(TRUE, nrow(predetermined))
    )
  }))
  safe_bind_rows(list(regular, predetermined_rows))
}

analysis_design_schooling_consumption_bridge <- function(
    consumption_registry, control_registry = NULL) {
  specs <- schooling_consumption_bridge_specifications(
    consumption_registry, control_registry
  )
  outcome <- vapply(
    plain_chr(specs$welfare_specification_id),
    consumption_iv_variable_name,
    character(1),
    role = "outcome"
  )
  analysis_design_frame(
    analysis_id = plain_chr(specs$analysis_id),
    family = rep("schooling_consumption_bridge", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = outcome,
    treatment = plain_chr(specs$treatment),
    instrument = rep("", nrow(specs)),
    instrument_vintage = rep("not_applicable", nrow(specs)),
    adjustment_set = plain_chr(specs$adjustment_id),
    fixed_effect = plain_chr(specs$fixed_effect),
    estimand = paste0("descriptive_", plain_chr(specs$estimand)),
    estimator = rep("ols", nrow(specs)),
    inference = rep("state_clustered+holm", nrow(specs)),
    sample_rule = rep("schooling_welfare_common_support", nrow(specs)),
    analysis_role = rep("descriptive_schooling_welfare_bridge", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("registered_descriptive_schooling_welfare_design", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_nss64_social_group <- function(control_registry = NULL) {
  specs <- nss64_schooling_social_group_specifications()
  analysis_design_frame(
    analysis_id = plain_chr(specs$analysis_id),
    family = rep("nss64_social_group", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = paste0("gap__", plain_chr(specs$outcome)),
    treatment = rep("", nrow(specs)),
    instrument = rep(preferred_iv_variables()$instrument, nrow(specs)),
    instrument_vintage = rep("2001", nrow(specs)),
    adjustment_set = rep("state_main", nrow(specs)),
    fixed_effect = rep("state", nrow(specs)),
    estimand = rep("social_group_gap_distance_association", nrow(specs)),
    estimator = rep("ols", nrow(specs)),
    inference = rep("state_clustered+holm", nrow(specs)),
    sample_rule = paste0("social_group_gap__", plain_chr(specs$sample)),
    analysis_role = rep("descriptive_schooling_inequality", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("registered_social_group_gap_design", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_nss64_social_group_crosscut <- function() {
  specs <- nss64_schooling_social_group_crosscut_specifications()
  analysis_design_frame(
    analysis_id = plain_chr(specs$analysis_id),
    family = rep("nss64_social_group_crosscut", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = paste0("gap__", plain_chr(specs$outcome)),
    treatment = rep("", nrow(specs)),
    instrument = rep("", nrow(specs)),
    instrument_vintage = rep("not_applicable", nrow(specs)),
    adjustment_set = paste(plain_chr(specs$crosscut), plain_chr(specs$stratum), sep = "__"),
    fixed_effect = rep("none", nrow(specs)),
    functional_form_id = rep("descriptive_mean", nrow(specs)),
    estimand = rep("mean_district_social_group_gap", nrow(specs)),
    estimator = rep("descriptive_mean", nrow(specs)),
    inference = rep("none", nrow(specs)),
    sample_rule = paste0(
      "social_group_crosscut__", plain_chr(specs$crosscut), "__",
      tolower(plain_chr(specs$stratum))
    ),
    analysis_role = rep("descriptive_schooling_inequality", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("registered_social_group_access_crosscut", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_schooling_consumption_conversion <- function(
    consumption_registry, control_registry = NULL) {
  specs <- schooling_consumption_conversion_specifications(
    consumption_registry, control_registry
  )
  outcome <- vapply(
    plain_chr(specs$welfare_specification_id),
    consumption_iv_variable_name,
    character(1),
    role = "outcome"
  )
  analysis_design_frame(
    analysis_id = plain_chr(specs$analysis_id),
    family = rep("schooling_consumption_conversion", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = outcome,
    treatment = plain_chr(specs$treatment),
    effect_modifier = plain_chr(specs$modifier),
    # Census-2001 moderator variables are canonical variable-dictionary construct
    # IDs. Declare them explicitly so effect modification is a first-class
    # semantic axis rather than relying on downstream name inference.
    effect_modifier_construct_id = plain_chr(specs$modifier),
    instrument = rep("", nrow(specs)),
    instrument_vintage = rep("not_applicable", nrow(specs)),
    adjustment_set = rep("state_main", nrow(specs)),
    fixed_effect = rep("state", nrow(specs)),
    functional_form_id = rep("linear_interaction", nrow(specs)),
    estimand = rep("descriptive_schooling_by_baseline_capacity_interaction", nrow(specs)),
    estimator = rep("ols", nrow(specs)),
    inference = rep("state_clustered+holm", nrow(specs)),
    sample_rule = rep("schooling_conversion_common_support", nrow(specs)),
    analysis_role = rep("descriptive_effect_modification", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("predeclared_conversion_gradient_design", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_economic_census_it_opportunity <- function(
    consumption_registry, control_registry = NULL) {
  specs <- economic_census_it_opportunity_specifications(consumption_registry)
  outcome <- vapply(
    plain_chr(specs$welfare_specification_id),
    consumption_iv_variable_name,
    character(1),
    role = "outcome"
  )
  is_distance <- specs$predictor_role == "instrument"
  analysis_design_frame(
    analysis_id = plain_chr(specs$analysis_id),
    family = rep("economic_census_it_opportunity", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = outcome,
    outcome_construct_id = plain_chr(specs$outcome_construct_id),
    baseline_construct_id = plain_chr(specs$baseline_construct_id),
    treatment = ifelse(is_distance, "", plain_chr(specs$predictor)),
    effect_modifier = plain_chr(specs$modifier),
    effect_modifier_construct_id = plain_chr(specs$modifier_construct_id),
    instrument = ifelse(is_distance, plain_chr(specs$predictor), ""),
    instrument_vintage = ifelse(is_distance, "2001", "not_applicable"),
    adjustment_set = rep("state_main", nrow(specs)),
    fixed_effect = rep("state", nrow(specs)),
    functional_form_id = rep("linear_interaction", nrow(specs)),
    estimand = plain_chr(specs$estimand),
    estimator = rep("ols", nrow(specs)),
    inference = rep("state_clustered+holm", nrow(specs)),
    sample_rule = rep("ec05_it_opportunity_common_support", nrow(specs)),
    analysis_role = rep("descriptive_effect_modification", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("bounded_ec05_it_opportunity_heterogeneity", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_census_household_capacity <- function() {
  specs <- census_household_capacity_specifications()
  is_distance <- specs$predictor_role == "instrument"
  analysis_design_frame(
    analysis_id = plain_chr(specs$analysis_id),
    family = rep("census_household_capacity", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = plain_chr(specs$variable),
    outcome_construct_id = plain_chr(specs$construct_id),
    baseline_construct_id = plain_chr(specs$baseline_construct_id),
    treatment = ifelse(is_distance, "", plain_chr(specs$predictor)),
    instrument = ifelse(is_distance, plain_chr(specs$predictor), ""),
    instrument_vintage = ifelse(is_distance, "2001", "not_applicable"),
    adjustment_set = rep("state_main+outcome_baseline", nrow(specs)),
    fixed_effect = rep("state", nrow(specs)),
    functional_form_id = rep("change", nrow(specs)),
    estimand = plain_chr(specs$estimand),
    estimator = rep("ols", nrow(specs)),
    inference = rep("state_clustered+holm", nrow(specs)),
    sample_rule = rep("census_household_capacity_common_support", nrow(specs)),
    analysis_role = plain_chr(specs$analysis_role),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("bounded_coevolving_household_capacity_synthesis", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_st_concentration_heterogeneity <- function(control_registry = NULL) {
  specs <- english_opportunity_st_heterogeneity_specifications()
  analysis_design_frame(
    analysis_id = paste("st_concentration_heterogeneity", specs$specification_id, sep = "__"),
    family = rep("st_concentration_heterogeneity", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = plain_chr(specs$outcome),
    treatment = rep("", nrow(specs)),
    instrument = rep(preferred_iv_variables()$instrument, nrow(specs)),
    instrument_vintage = rep("2001", nrow(specs)),
    adjustment_set = paste0(
      "state_main_without_st_share__", plain_chr(specs$heterogeneity)
    ),
    fixed_effect = rep("state", nrow(specs)),
    estimand = ifelse(
      specs$heterogeneity == "continuous_interaction",
      "distance_by_st_share_interaction",
      "distance_slope_high_st_subset"
    ),
    estimator = rep("ols", nrow(specs)),
    inference = rep("state_clustered+holm", nrow(specs)),
    sample_rule = paste0("st_concentration__", plain_chr(specs$sample)),
    analysis_role = rep("descriptive_effect_modification", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("registered_st_concentration_heterogeneity_design", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_census_1991_st_language <- function() {
  specs <- census_1991_st_language_specifications()
  analysis_design_frame(
    analysis_id = plain_chr(specs$analysis_id),
    family = rep("census_1991_st_language", nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = plain_chr(specs$outcome),
    treatment = rep("", nrow(specs)),
    instrument = rep("shastry_distance_1991", nrow(specs)),
    instrument_vintage = rep("1991", nrow(specs)),
    adjustment_set = rep("state_1991", nrow(specs)),
    fixed_effect = rep("state_1991", nrow(specs)),
    estimand = rep("st_language_acquisition_association", nrow(specs)),
    estimator = rep("mother_tongue_speaker_weighted_ols", nrow(specs)),
    inference = rep("state_1991_clustered", nrow(specs)),
    sample_rule = plain_chr(specs$sample),
    analysis_role = rep("predetermined_language_behavior", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("registered_validated_1991_st_language_design", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_consumption_exclusion_sensitivity <- function(specifications) {
  analysis_design_from_iv(
    specifications,
    family = "consumption_exclusion_sensitivity",
    estimator = "bounded_exclusion_ar",
    inference = "state_clustered+bounded_exclusion_ar",
    analysis_role = "exclusion_restriction_sensitivity"
  )
}

analysis_design_falsification_adaptive_set <- function(specifications) {
  analysis_design_from_iv(
    specifications,
    family = "iv_falsification_adaptive_set",
    estimator = "falsification_adaptive_set",
    estimand = "falsification_adaptive_set",
    inference = "state_clustered",
    analysis_role = "overidentification_exclusion_sensitivity",
    reason = "registered_five_share_falsification_adaptive_set"
  )
}

compile_analysis_design_registry <- function(
    consumption_iv_specifications,
    english_opportunity_measure_registry,
    control_registry = NULL,
    public_iv_specifications = public_iv_specification_registry(control_registry),
    consumption_scalar_iv_robustness_specifications = NULL,
    consumption_treatment_robustness_specifications = NULL,
    consumption_alternative_welfare_specifications = NULL,
    consumption_control_strategy_specifications = NULL,
    consumption_control_parameterization_specifications = NULL,
    consumption_historical_adjustment_specifications = NULL,
    consumption_historical_concept_matched_specifications = NULL,
    consumption_exclusion_sensitivity_specifications = NULL,
    falsification_adaptive_specifications = NULL,
    consumption_registry = NULL,
    construct_registry = NULL) {
  core_iv <- analysis_design_from_iv(
    iv_diagnostic_specification_registry(control_registry = control_registry),
    family = "district_iv_diagnostic",
    estimator = "iv_diagnostic_suite",
    estimand = "iv_design",
    inference = "state_clustered+weak_iv_diagnostics",
    analysis_role = "diagnostic_universe"
  )
  consumption <- analysis_design_from_iv(
    consumption_iv_specifications,
    family = "consumption_iv",
    estimator = "first_stage+reduced_form+2sls+anderson_rubin",
    inference = "state_clustered+anderson_rubin",
    analysis_role = ifelse(
      toupper(plain_chr(consumption_iv_specifications$tier)) == "A",
      "preferred_outcome", "robustness"
    )
  )
  consumption_scalar <- if (is.null(consumption_scalar_iv_robustness_specifications)) {
    data.frame()
  } else {
    analysis_design_from_iv(
      consumption_scalar_iv_robustness_specifications,
      family = "consumption_iv",
      estimator = "first_stage+reduced_form+2sls+anderson_rubin",
      inference = "state_clustered+anderson_rubin+holm",
      analysis_role = "scalar_iv_robustness"
    )
  }
  consumption_treatment <- if (is.null(consumption_treatment_robustness_specifications)) {
    data.frame()
  } else {
    analysis_design_from_iv(
      consumption_treatment_robustness_specifications,
      family = "consumption_iv",
      estimator = "first_stage+reduced_form+2sls+anderson_rubin",
      inference = "state_clustered+anderson_rubin+holm",
      analysis_role = "treatment_definition_robustness"
    )
  }
  consumption_welfare <- if (is.null(consumption_alternative_welfare_specifications)) {
    data.frame()
  } else {
    analysis_design_from_iv(
      consumption_alternative_welfare_specifications,
      family = "consumption_iv",
      estimator = "first_stage+reduced_form+2sls+anderson_rubin",
      inference = "state_clustered+anderson_rubin+holm",
      analysis_role = "welfare_definition_robustness"
    )
  }
  consumption_control_strategy <- if (is.null(consumption_control_strategy_specifications)) {
    data.frame()
  } else {
    analysis_design_from_iv(
      consumption_control_strategy_specifications,
      family = "consumption_iv",
      estimator = "first_stage+reduced_form+2sls+anderson_rubin",
      inference = "state_clustered+anderson_rubin+holm",
      analysis_role = "control_strategy_robustness"
    )
  }
  consumption_control_parameterization <- if (is.null(consumption_control_parameterization_specifications)) {
    data.frame()
  } else {
    analysis_design_from_iv(
      consumption_control_parameterization_specifications,
      family = "consumption_iv",
      estimator = "first_stage+reduced_form+2sls+anderson_rubin",
      inference = "state_clustered+anderson_rubin+holm",
      analysis_role = "control_parameterization_robustness"
    )
  }
  consumption_historical_adjustment <- if (is.null(consumption_historical_adjustment_specifications)) {
    data.frame()
  } else {
    analysis_design_from_iv(
      consumption_historical_adjustment_specifications,
      family = "consumption_iv",
      estimator = "first_stage+reduced_form+2sls+anderson_rubin",
      inference = "state_clustered+anderson_rubin+holm",
      analysis_role = "historical_adjustment_robustness"
    )
  }
  consumption_historical_concept_matched <- if (is.null(consumption_historical_concept_matched_specifications)) {
    data.frame()
  } else {
    analysis_design_from_iv(
      consumption_historical_concept_matched_specifications,
      family = "consumption_iv",
      estimator = "first_stage+reduced_form+2sls+anderson_rubin",
      inference = "state_clustered+anderson_rubin+holm",
      analysis_role = "historical_concept_matched_robustness"
    )
  }
  consumption_exclusion_sensitivity <- if (is.null(consumption_exclusion_sensitivity_specifications)) {
    data.frame()
  } else {
    analysis_design_consumption_exclusion_sensitivity(
      consumption_exclusion_sensitivity_specifications
    )
  }
  falsification_adaptive <- if (is.null(falsification_adaptive_specifications)) {
    data.frame()
  } else {
    analysis_design_falsification_adaptive_set(
      falsification_adaptive_specifications
    )
  }
  out <- safe_bind_rows(list(
    analysis_design_public_iv(public_iv_specifications),
    core_iv,
    analysis_design_hindi_belt_first_stage(control_registry),
    analysis_design_child_population_first_stage(control_registry),
    consumption,
    consumption_scalar,
    consumption_treatment,
    consumption_welfare,
    consumption_control_strategy,
    consumption_control_parameterization,
    consumption_historical_adjustment,
    consumption_historical_concept_matched,
    consumption_exclusion_sensitivity,
    falsification_adaptive,
    analysis_design_district_mechanisms(
      english_opportunity_measure_registry, control_registry
    ),
    analysis_design_c17(),
    analysis_design_dise(control_registry = control_registry),
    analysis_design_census_mechanisms(control_registry),
    analysis_design_census_migration_hindi_belt(control_registry),
    analysis_design_economic_census_mechanisms(control_registry),
    if (is.null(consumption_registry)) data.frame() else
      analysis_design_economic_census_it_opportunity(consumption_registry, control_registry),
    analysis_design_labor_mechanisms(control_registry),
    analysis_design_historical_first_stages(control_registry),
    if (is.null(consumption_registry)) data.frame() else
      analysis_design_schooling_consumption_bridge(consumption_registry, control_registry),
    if (is.null(consumption_registry)) data.frame() else
      analysis_design_schooling_consumption_conversion(consumption_registry, control_registry),
    analysis_design_nss64_social_group(control_registry),
    analysis_design_nss64_social_group_crosscut(),
    analysis_design_census_household_capacity(),
    analysis_design_st_concentration_heterogeneity(control_registry),
    analysis_design_census_1991_st_language()
  ))
  out <- out[analysis_design_columns()]
  redundant_namespace <- startsWith(
    plain_chr(out$specification_id), paste0(plain_chr(out$family), "__")
  )
  if (any(redundant_namespace)) {
    stop(
      "Canonical specification_id values must not repeat the analysis-family namespace: ",
      paste(unique(out$analysis_id[redundant_namespace]), collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(construct_registry)) {
    out <- link_analysis_design_constructs(out, construct_registry)
  }
  if (anyDuplicated(out$analysis_id)) {
    stop("Compiled analysis-design registry contains duplicate analysis_id values.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}
