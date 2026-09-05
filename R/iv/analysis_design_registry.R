# Cross-family analysis-design ontology.
#
# Specialized registries remain authoritative for estimation. This module only
# projects them onto one common schema so the repository can inventory what is
# actually implemented without constructing an indiscriminate Cartesian product
# of outcomes, treatments, instruments, controls, and estimators.

analysis_design_columns <- function() {
  c(
    "analysis_id", "family", "specification_id", "outcome", "treatment",
    "instrument", "instrument_vintage", "distance_measure_id",
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
    ols = "ols",
    native_speaker_weighted_ols = "weighted_ols",
    mother_tongue_speaker_weighted_ols = "weighted_ols"
  )
}

analysis_inference_registry <- function() {
  data.frame(
    inference = c(
      "HC1", "state_clustered", "state_1991_clustered",
      "state_clustered+holm",
      "state_clustered+anderson_rubin",
      "state_clustered+anderson_rubin+holm",
      "state_clustered+weak_iv_diagnostics"
    ),
    covariance_id = c(
      "HC1", "state_clustered", "state_1991_clustered",
      "state_clustered", "state_clustered", "state_clustered",
      "state_clustered"
    ),
    weak_id_inference_id = c(
      "none", "none", "none", "none",
      "anderson_rubin", "anderson_rubin", "weak_iv_diagnostic_suite"
    ),
    multiplicity_id = c(
      "none", "none", "none", "holm", "none", "holm", "none"
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
  for (nm in c(
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

  analysis_design_frame(
    analysis_id = paste(family, plain_chr(specs$specification_id), sep = "__"),
    family = rep(family, nrow(specs)),
    specification_id = plain_chr(specs$specification_id),
    outcome = plain_chr(specs$outcome),
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
    analysis_id = paste("c17_mechanism", plain_chr(x$specification_id), sep = "__"),
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
    sample_rule = paste0("c17_", tolower(plain_chr(x$sex))),
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
    rows$analysis_id <- paste(
      "dise_first_stage", construct$construct_id[[1L]], rows$specification_id,
      sep = "__"
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
    rows$analysis_id <- paste(
      "dise_weak_iv", construct$construct_id[[1L]], rows$specification_id,
      sep = "__"
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
      rows$analysis_id <- paste(
        "census", source, registry$outcome_id[[i]], rows$specification_id,
        sep = "__"
      )
      rows
    }))
  }))
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
    rows$analysis_id <- paste(
      "economic_census", registry$outcome_id[[i]], rows$specification_id,
      sep = "__"
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
      rows$analysis_id <- paste(
        "labor", design$wave_id[[1L]], design$sample_suffix[[1L]],
        registry$outcome_id[[i]], rows$specification_id, sep = "__"
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
    analysis_id = paste("schooling_consumption_bridge", specs$specification_id, sep = "__"),
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
    sample_rule = rep("treatment_welfare_fixed_complete_case", nrow(specs)),
    analysis_role = rep("descriptive_schooling_welfare_bridge", nrow(specs)),
    admissible = rep(TRUE, nrow(specs)),
    reason = rep("registered_descriptive_schooling_welfare_design", nrow(specs)),
    implemented = rep(TRUE, nrow(specs))
  )
}

analysis_design_nss64_social_group <- function(control_registry = NULL) {
  specs <- nss64_schooling_social_group_specifications()
  analysis_design_frame(
    analysis_id = paste("nss64_social_group", specs$specification_id, sep = "__"),
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
    analysis_id = paste("census_1991_st_language", specs$specification_id, sep = "__"),
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
    consumption_registry = NULL) {
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
    analysis_design_district_mechanisms(
      english_opportunity_measure_registry, control_registry
    ),
    analysis_design_c17(),
    analysis_design_dise(control_registry = control_registry),
    analysis_design_census_mechanisms(control_registry),
    analysis_design_economic_census_mechanisms(control_registry),
    analysis_design_labor_mechanisms(control_registry),
    analysis_design_historical_first_stages(control_registry),
    if (is.null(consumption_registry)) data.frame() else
      analysis_design_schooling_consumption_bridge(consumption_registry, control_registry),
    analysis_design_nss64_social_group(control_registry),
    analysis_design_st_concentration_heterogeneity(control_registry),
    analysis_design_census_1991_st_language()
  ))
  out <- out[analysis_design_columns()]
  if (anyDuplicated(out$analysis_id)) {
    stop("Compiled analysis-design registry contains duplicate analysis_id values.", call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

candidate_design_columns <- function() {
  c(
    "candidate_id", "reference_section", "analysis_family", "design_role",
    "scientific_question", "design_axis", "outcome_scope", "treatment_scope",
    "instrument_scope", "adjustment_scope", "estimator_scope",
    "execution_policy", "multiplicity_family", "prerequisite",
    "admissible", "admissibility_reason", "implementation_status", "candidate_cells",
    "implemented_cells", "execution_cells", "rationale"
  )
}

candidate_design_row <- function(
    candidate_id,
    reference_section,
    analysis_family,
    design_role,
    scientific_question,
    design_axis,
    outcome_scope,
    treatment_scope,
    instrument_scope,
    adjustment_scope,
    estimator_scope,
    execution_policy,
    multiplicity_family = "not_applicable",
    prerequisite = "none",
    admissible = TRUE,
    admissibility_reason = NULL,
    implementation_status = "unimplemented",
    candidate_cells = NA_integer_,
    implemented_cells = 0L,
    execution_cells = implemented_cells,
    rationale) {
  if (is.null(admissibility_reason)) {
    admissibility_reason <- if (isTRUE(admissible)) {
      paste0("admissible_", design_role)
    } else {
      ""
    }
  }
  data.frame(
    candidate_id = candidate_id,
    reference_section = reference_section,
    analysis_family = analysis_family,
    design_role = design_role,
    scientific_question = scientific_question,
    design_axis = design_axis,
    outcome_scope = outcome_scope,
    treatment_scope = treatment_scope,
    instrument_scope = instrument_scope,
    adjustment_scope = adjustment_scope,
    estimator_scope = estimator_scope,
    execution_policy = execution_policy,
    multiplicity_family = multiplicity_family,
    prerequisite = prerequisite,
    admissible = admissible,
    admissibility_reason = admissibility_reason,
    implementation_status = implementation_status,
    candidate_cells = as.integer(candidate_cells),
    implemented_cells = as.integer(implemented_cells),
    execution_cells = as.integer(execution_cells),
    rationale = rationale,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

candidate_design_frame <- function(rows) {
  out <- safe_bind_rows(rows)
  missing <- setdiff(candidate_design_columns(), names(out))
  if (length(missing)) {
    stop(
      "Candidate-design rows are missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  out <- out[candidate_design_columns()]
  allowed_status <- c(
    "implemented", "partial", "unimplemented", "data_unavailable",
    "deferred", "not_applicable"
  )
  allowed_policy <- c(
    "estimate", "estimate_if_registered", "diagnostic_only", "report_only",
    "requires_data", "do_not_estimate"
  )
  if (!nrow(out) || anyDuplicated(out$candidate_id) || any(!nzchar(out$candidate_id)) ||
      any(!out$implementation_status %in% allowed_status) ||
      any(!out$execution_policy %in% allowed_policy) ||
      any(!nzchar(out$reference_section)) ||
      any(!nzchar(out$scientific_question)) ||
      any(!nzchar(out$admissibility_reason)) ||
      any(!nzchar(out$rationale))) {
    stop("Candidate-design ledger is malformed.", call. = FALSE)
  }
  policy_conflict <- (!out$admissible & out$execution_policy != "do_not_estimate") |
    (out$admissible & out$execution_policy == "do_not_estimate")
  if (any(policy_conflict)) {
    stop(
      "Candidate-design admissibility must agree with execution_policy.",
      call. = FALSE
    )
  }
  impossible <- out$implemented_cells > out$candidate_cells &
    is.finite(out$implemented_cells) & is.finite(out$candidate_cells)
  invalid_execution <- out$execution_cells > out$implemented_cells &
    is.finite(out$execution_cells) & is.finite(out$implemented_cells)
  if (any(impossible) || any(invalid_execution)) {
    stop(
      "Candidate-design cell counts must satisfy execution <= implemented <= candidate.",
      call. = FALSE
    )
  }
  out
}

count_alternative_consumption_welfare_designs <- function(
    consumption_specifications,
    welfare_registry) {
  nrow(build_consumption_alternative_welfare_registry(
    consumption_specifications,
    welfare_registry
  ))
}

build_iv_candidate_design_ledger <- function(
    public_specifications,
    consumption_specifications,
    control_registry = NULL,
    welfare_registry = NULL,
    english_opportunity_registry = NULL,
    consumption_scalar_iv_robustness_specifications = NULL,
    consumption_treatment_robustness_specifications = NULL,
    consumption_alternative_welfare_specifications = NULL,
    consumption_control_strategy_specifications = NULL,
    consumption_control_parameterization_specifications = NULL,
    consumption_historical_adjustment_specifications = NULL,
    consumption_historical_concept_matched_specifications = NULL) {
  public_specs <- as_iv_specifications(public_specifications)
  consumption_specs <- as_iv_specifications(consumption_specifications)
  control_registry <- resolve_census_2001_control_registry(control_registry)
  diagnostic_specs <- iv_diagnostic_specification_registry(
    control_registry = control_registry
  )
  canonical_specs <- iv_specification_registry(control_registry = control_registry)
  constructions <- iv_instrument_constructions()
  n_candidate_iv <- length(iv_candidate_design_adjustments()) *
    length(iv_candidate_design_constructions())
  n_consumption <- nrow(consumption_specs)
  n_consumption_scalar <- if (is.null(consumption_scalar_iv_robustness_specifications)) {
    0L
  } else {
    nrow(as_iv_specifications(consumption_scalar_iv_robustness_specifications))
  }
  n_consumption_treatment <- if (is.null(consumption_treatment_robustness_specifications)) {
    0L
  } else {
    nrow(as_iv_specifications(consumption_treatment_robustness_specifications))
  }
  n_consumption_welfare <- if (is.null(consumption_alternative_welfare_specifications)) {
    0L
  } else {
    nrow(as_iv_specifications(consumption_alternative_welfare_specifications))
  }
  n_consumption_control_strategy <- if (is.null(consumption_control_strategy_specifications)) {
    0L
  } else {
    nrow(as_iv_specifications(consumption_control_strategy_specifications))
  }
  n_consumption_control_parameterization <- if (is.null(consumption_control_parameterization_specifications)) {
    0L
  } else {
    nrow(as_iv_specifications(consumption_control_parameterization_specifications))
  }
  n_consumption_historical_adjustment <- if (is.null(consumption_historical_adjustment_specifications)) {
    0L
  } else {
    nrow(as_iv_specifications(consumption_historical_adjustment_specifications))
  }
  n_consumption_historical_concept_matched <- if (is.null(consumption_historical_concept_matched_specifications)) {
    0L
  } else {
    nrow(as_iv_specifications(consumption_historical_concept_matched_specifications))
  }
  n_diagnostic_iv <- nrow(diagnostic_specs)
  n_scalar <- sum(canonical_specs$n_excluded_instruments == 1L)
  n_multishare <- sum(canonical_specs$n_excluded_instruments > 1L)
  n_absorption_candidates <- length(iv_absorption_adjustments(control_registry))
  n_absorption_executions <- nrow(
    iv_absorption_specification_registry(control_registry = control_registry)
  )
  n_hindi_belt <- nrow(
    iv_hindi_belt_first_stage_specifications(control_registry = control_registry)
  )
  n_child_population <- nrow(
    iv_child_population_first_stage_specifications(control_registry = control_registry)
  )
  n_block_interventions <- length(iv_block_intervention_adjustments(control_registry))
  n_control_strategies <- length(iv_causal_control_strategy_adjustments(control_registry))
  n_relevance_parameterizations <- length(iv_main_parameterization_adjustments(control_registry))
  n_control_parameterizations <- length(
    iv_causal_control_parameterization_adjustments(control_registry)
  )
  n_dise_relevance <- nrow(dise_construct_registry()) * n_diagnostic_iv
  n_historical_vintage <- nrow(historical_linguistic_predetermined_first_stage_registry()) * 4L
  n_c17 <- nrow(census_c17_mechanism_registry())
  n_district_mechanism <- NA_integer_
  if (!is.null(english_opportunity_registry)) {
    n_district_mechanism <- nrow(
      preferred_district_mechanism_registry(english_opportunity_registry)
    ) * nrow(district_mechanism_adjustment_registry(control_registry))
  }
  n_alt_welfare <- if (is.null(welfare_registry)) NA_integer_ else {
    count_alternative_consumption_welfare_designs(consumption_specs, welfare_registry)
  }

  rows <- list(
    candidate_design_row(
      "relevance_geography_control_absorption",
      "Prompt 1 §§1-3",
      "first_stage_relevance",
      "diagnostic_relevance",
      "Where does the linguistic-distance/EMI relationship disappear as geography and predetermined controls are absorbed?",
      "geography_and_control_absorption",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "unadjusted, region/state FE, main/expanded, sequential, block interventions, and registered parameterizations",
      "first_stage",
      "diagnostic_only",
      implementation_status = "implemented",
      candidate_cells = n_absorption_candidates,
      implemented_cells = n_absorption_candidates,
      execution_cells = n_absorption_executions,
      rationale = "The reference treats attenuation across geography and controls as a scientific result. The registry therefore keeps the historical cumulative ladder but adds symmetric block-only, leave-one-block-out, and declared alternative-parameterization designs."
    ),
    candidate_design_row(
      "relevance_control_block_interventions",
      "Prompt 1 §3; control critique",
      "first_stage_relevance",
      "diagnostic_relevance",
      "Which theoretically named predetermined control family accounts for first-stage attenuation, rather than which arbitrary cumulative ordering happens to enter first?",
      "control_block",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "each main control block alone and each block omitted from the main set, under region/state FE",
      "first_stage",
      "diagnostic_only",
      implementation_status = "implemented",
      candidate_cells = n_block_interventions,
      implemented_cells = n_block_interventions,
      rationale = "Symmetric block interventions answer the theory question directly and avoid privileging human capital or an arbitrary cumulative block order."
    ),
    candidate_design_row(
      "relevance_control_parameterizations",
      "Prompt 1 §3; Response 2 controls",
      "first_stage_relevance",
      "diagnostic_relevance",
      "Does relevance depend on how human capital and economic structure are parameterized?",
      "control_parameterization",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "secondary-plus versus literacy crossed with compact versus decomposed economic structure, under region/state FE",
      "first_stage",
      "diagnostic_only",
      implementation_status = "implemented",
      candidate_cells = n_relevance_parameterizations,
      implemented_cells = n_relevance_parameterizations,
      rationale = "These are finite substitutions already declared by the control registry, not arbitrary covariate subsets."
    ),
    candidate_design_row(
      "relevance_scalar_linguistic_constructions",
      "Prompt 1 §4",
      "first_stage_relevance",
      "diagnostic_relevance",
      "Is weak conditional relevance specific to one linguistic-distance construction?",
      "instrument_definition",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "all registered scalar Shastry/legacy/Glottolog/Dyen/sensitivity constructions",
      "unadjusted plus region/state main/expanded designs",
      "first_stage",
      "diagnostic_only",
      implementation_status = "implemented",
      candidate_cells = n_scalar,
      implemented_cells = n_scalar,
      rationale = "Alternative scalar bases probe measurement robustness while preserving a one-instrument exclusion story."
    ),
    candidate_design_row(
      "relevance_multishare_instruments",
      "Prompt 1 §5",
      "first_stage_relevance",
      "diagnostic_only_overidentified",
      "Does retaining the full linguistic-distance distribution increase prediction, and what exclusion cost accompanies it?",
      "instrument_dimensionality",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "registered five-share instrument systems",
      "unadjusted plus region/state main/expanded designs",
      "first_stage+overidentification",
      "diagnostic_only",
      implementation_status = "implemented",
      candidate_cells = n_multishare,
      implemented_cells = n_multishare,
      rationale = "The reference explicitly treats strong five-share first stages as diagnostic rather than a preferred rescue because richer language-composition variation weakens the exclusion argument."
    ),
    candidate_design_row(
      "relevance_dise_treatment_definitions",
      "Prompt 1 §§6-8",
      "dise_relevance",
      "diagnostic_relevance",
      "Does the geographic attenuation survive alternative administrative definitions of EMI and nearby language/schooling constructs?",
      "treatment_definition",
      "all registered DISE structural-IV and relevance-only constructs",
      "DISE EMI, language-composition, and management constructs",
      "full registered diagnostic IV universe",
      "all registered diagnostic adjustments",
      "first_stage",
      "diagnostic_only",
      implementation_status = "implemented",
      candidate_cells = n_dise_relevance,
      implemented_cells = n_dise_relevance,
      rationale = "The reference uses DISE primarily to distinguish measurement failure from geographic attenuation; relevance-only constructs remain diagnostics rather than alternate causal treatments."
    ),
    candidate_design_row(
      "relevance_historical_distance_vintage",
      "Prompt 1 §§9,24",
      "historical_first_stage",
      "diagnostic_relevance",
      "Does the first-stage pattern survive a pre-treatment 1991 linguistic-distance vintage and predetermined 1991 adjustment sets?",
      "instrument_vintage",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "1991 versus Census-2001 linguistic distance",
      "1991 predetermined PCA/all controls; preferred/exact geography",
      "first_stage",
      "diagnostic_only",
      implementation_status = "implemented",
      candidate_cells = n_historical_vintage,
      implemented_cells = n_historical_vintage,
      rationale = "Historical vintage comparisons are an identification diagnostic; exact-match historical geography is not allowed to redefine the production instrument."
    ),
    candidate_design_row(
      "c17_behavioral_mechanism",
      "Response 2 C-17; Response 3 Phase 3",
      "c17_mechanism",
      "mechanism_and_falsification",
      "Within a state, do speakers of languages farther from Hindi acquire English more often conditional on multilingualism?",
      "language_group_mechanism",
      "English acquisition, Hindi substitution, multilingualism, sex heterogeneity",
      "language-group linguistic distance",
      "Shastry/Glottolog/Dyen and registered nonlinear forms",
      "state FE + native-language share + modal-language indicator",
      "weighted_ols_hc1",
      "estimate",
      multiplicity_family = "c17_registered_mechanisms",
      implementation_status = "implemented",
      candidate_cells = n_c17,
      implemented_cells = n_c17,
      rationale = "C-17 is a distinct state-by-language estimand and should not be forced into the district IV grid."
    ),
    candidate_design_row(
      "district_schooling_three_geography_grid",
      "Response 3 Phase 12",
      "district_schooling_mechanism",
      "paper_facing_relevance",
      "Where does the linguistic-distance signal attenuate across schooling-access, medium-choice, institution, and school-quality stages?",
      "mechanism_stage_by_geography",
      "preferred registered NSS and DISE district mechanism measures",
      "district schooling/mechanism outcomes",
      "preferred Shastry nonzero-mean distance",
      "unadjusted; region FE + main controls; state FE + main controls",
      "standardized_reduced_form",
      "estimate",
      multiplicity_family = "district_schooling_mechanisms",
      implementation_status = if (is.finite(n_district_mechanism)) "implemented" else "unimplemented",
      candidate_cells = n_district_mechanism,
      implemented_cells = if (is.finite(n_district_mechanism)) n_district_mechanism else 0L,
      rationale = "This is the explicitly predeclared paper-facing three-geography grid from the mechanism rescue plan; the larger IV permutation registry remains diagnostic."
    ),
    candidate_design_row(
      "public_headline_registered",
      "Prompt 1 §35; specification-governance consolidation",
      "public_iv",
      "primary_and_registered_robustness",
      "What are the registered headline consumption estimands under the preferred honest state-FE benchmark?",
      "headline_estimand",
      "public consumption change/ANCOVA plus inherited nominal robustness",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "registered public model-specific adjustments",
      "2sls",
      "estimate",
      multiplicity_family = "public_headline",
      implementation_status = "implemented",
      candidate_cells = nrow(public_specs),
      implemented_cells = nrow(public_specs),
      rationale = "Public output contract; all rows are canonical IV specifications and the state-FE benchmark is retained even when relevance is weak."
    ),
    candidate_design_row(
      "consumption_registered_dynamics",
      "Prompt 1 §§11-12,35",
      "consumption_iv",
      "registered_dynamic_family",
      "How do real-consumption effects vary by post-treatment horizon and ANCOVA versus change estimand?",
      "outcome_year_and_estimand",
      "registered real-mean-MPCE endpoint ANCOVA/change designs",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "state_main",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_registered_dynamics",
      implementation_status = "implemented",
      candidate_cells = n_consumption,
      implemented_cells = n_consumption,
      rationale = "Endpoint and estimand variation are substantively meaningful and were registered before estimation."
    ),
    candidate_design_row(
      "consumption_candidate_scalar_iv_grid",
      "Prompt 1 §§4,26-29,35",
      "consumption_iv",
      "candidate_robustness",
      "Do conclusions survive the serious scalar linguistic-distance bases and both defensible geographic adjustment levels?",
      "instrument_definition_by_geography",
      "registered consumption endpoint designs",
      preferred_iv_variables()$treatment,
      "Shastry/Glottolog/Dyen scalar candidates",
      "region_main/state_main",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_scalar_iv_robustness",
      implementation_status = if (n_consumption_scalar == n_consumption * n_candidate_iv) "implemented" else "partial",
      candidate_cells = n_consumption * n_candidate_iv,
      implemented_cells = n_consumption_scalar,
      execution_cells = n_consumption_scalar,
      rationale = "This six-design family is theoretically bounded by instrument basis and honest geographic adjustment. It is estimated on one common district sample across the six designs within each endpoint/estimand, with Holm correction both within endpoint and across the full registered family."
    ),
    candidate_design_row(
      "consumption_alternative_welfare_outcomes",
      "Prompt 1 §11",
      "consumption_iv",
      "candidate_robustness",
      "Are welfare conclusions robust to functional-form and distributional response definitions already measured by the survey-design welfare layer?",
      "response_definition",
      "mean-log, median, and bottom-40 welfare outcomes at supported endpoints",
      preferred_iv_variables()$treatment,
      "Shastry/Glottolog/Dyen scalar candidates",
      "region_main/state_main",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_welfare_robustness",
      prerequisite = "endpoint-specific support/comparability registry",
      implementation_status = if (is.finite(n_alt_welfare) && n_consumption_welfare == n_alt_welfare * n_candidate_iv) "implemented" else "partial",
      candidate_cells = if (is.finite(n_alt_welfare)) n_alt_welfare * n_candidate_iv else NA_integer_,
      implemented_cells = n_consumption_welfare,
      execution_cells = n_consumption_welfare,
      rationale = "These outcomes are already measured and theoretically interpretable. Only survey-compatible endpoint pairs enter, all six scalar designs share common support within each welfare definition, and Holm adjustment is frozen within definition and across the full secondary family."
    ),
    candidate_design_row(
      "emi_intensive_margin_robustness",
      "Prompt 1 §18; Response 2 EMI decomposition",
      "consumption_iv",
      "candidate_robustness",
      "Does the welfare relationship differ when treatment is English-medium choice conditional on enrollment rather than all-child exposure?",
      "treatment_definition",
      "registered consumption endpoint designs",
      intensive_margin_emi_treatment(),
      "Shastry/Glottolog/Dyen scalar candidates",
      "region_main/state_main",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_treatment_robustness",
      prerequisite = "registered enrolled-child EMI share and endpoint-specific common support",
      implementation_status = if (n_consumption_treatment == n_consumption * n_candidate_iv) "implemented" else if (n_consumption_treatment > 0L) "partial" else "unimplemented",
      candidate_cells = n_consumption * n_candidate_iv,
      implemented_cells = n_consumption_treatment,
      execution_cells = n_consumption_treatment,
      rationale = "The intensive medium margin is substantively distinct and was the original treatment concept; it is estimated as its own frozen 48-cell robustness family rather than silently substituted for all-child exposure or merged retrospectively with the already-observed preferred-treatment family."
    ),
    candidate_design_row(
      "consumption_control_strategy_robustness",
      "Prompt 1 §3; control-set critique; Shastry identification discussion",
      "consumption_iv",
      "candidate_robustness",
      "Do consumption conclusions depend on the causal role assigned to baseline controls, rather than on a single historically named main set?",
      "control_strategy",
      "registered consumption endpoint designs",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "region/state FE crossed with geography-only, compact-2001, and compact-2001-without-human-capital strategies",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_control_strategy",
      implementation_status = "implemented",
      candidate_cells = n_consumption * n_control_strategies,
      implemented_cells = n_consumption_control_strategy,
      execution_cells = n_consumption_control_strategy,
      rationale = paste(
        "No single 2001 control vector is uniquely justified by IV theory. Geography-only designs avoid conditioning",
        "on possible descendants of historical linguistic structure; compact 2001 adjustment addresses observed",
        "exclusion threats; and omitting human capital probes a particularly plausible language/education pathway.",
        "This finite strategy family is more interpretable than arbitrary covariate subsets."
      )
    ),
    candidate_design_row(
      "consumption_control_parameterization_robustness",
      "Prompt 1 §3; Response 2 controls",
      "consumption_iv",
      "candidate_robustness",
      "Do causal conclusions depend on theoretically substitutable measures of baseline human capital or economic structure?",
      "control_parameterization",
      "registered consumption endpoint designs",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "within compact 2001 adjustment, region/state FE crossed with registered literacy/secondary-plus and compact/decomposed economic-structure parameterizations",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_control_parameterization",
      implementation_status = "implemented",
      candidate_cells = n_consumption * n_control_parameterizations,
      implemented_cells = n_consumption_control_parameterization,
      execution_cells = n_consumption_control_parameterization,
      rationale = paste(
        "This family asks a measurement question conditional on the compact-2001 strategy. The benchmark",
        "secondary-plus/compact-economic specification is re-estimated on the same common sample as the",
        "three registered substitutions under each FE, so sensitivity is not confounded by sample drift.",
        "The finite 2 FE x 4 parameterization grid is distinct from causal control-strategy robustness."
      )
    ),
    candidate_design_row(
      "historical_1991_adjustment_robustness",
      "Prompt 1 §35; historical controls discussion",
      "consumption_iv",
      "candidate_robustness",
      "Do consumption conclusions survive replacing compact 2001 adjustment with a more remote predetermined 1991 PCA baseline on the same production geography and sample?",
      "control_vintage",
      "registered consumption endpoint designs",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "region/state crossed with compact-2001 benchmark and population-interpolated PCA91 controls at the frozen 99% source-coverage threshold",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_historical_adjustment",
      prerequisite = "G2 population-interpolated PCA91 controls on Census-2001 production geography",
      implementation_status = "implemented",
      candidate_cells = n_consumption * length(iv_historical_adjustment_comparison_adjustments(control_registry)),
      implemented_cells = n_consumption_historical_adjustment,
      execution_cells = n_consumption_historical_adjustment,
      rationale = paste(
        "The benchmark is re-estimated on the same four-design common sample as the 1991 adjustment,",
        "so the comparison is not driven by support changes. PCA91 is more remote from 2007-08 EMI and",
        "uses Census population, composition, literacy, and worker structure, but it is not a pure",
        "same-variable vintage substitution because the available 1991 and compact-2001 concept sets differ."
      )
    ),
    candidate_design_row(
      "historical_1991_concept_matched_robustness",
      "Historical controls follow-up; Vanneman 1961-91 archive",
      "consumption_iv",
      "candidate_robustness",
      "Does the remote-baseline result survive a 1991 control vector that more closely matches the compact-2001 concepts?",
      "historical_control_source",
      "registered consumption endpoint designs",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "region/state crossed with compact-2001, PCA91, and Vanneman concept-matched 1991 controls on common support",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "estimate",
      multiplicity_family = "consumption_historical_concept_matched",
      prerequisite = "verified Vanneman dist91 archive plus frozen G2 1991-to-2001 population interpolation",
      implementation_status = "implemented",
      candidate_cells = n_consumption * length(iv_historical_concept_matched_adjustments(control_registry)),
      implemented_cells = n_consumption_historical_concept_matched,
      execution_cells = n_consumption_historical_concept_matched,
      rationale = paste(
        "Vanneman dist91 supplies 1991 analogues for urbanization, SC/ST and Muslim composition,",
        "matriculation, agricultural employment, age dependency, and household electricity that are",
        "missing from the thinner PCA91 causal vector. Keeping PCA91 and compact-2001 benchmarks in",
        "the same six-design common-support family separates source/concept sensitivity from sample drift."
      )
    ),
    candidate_design_row(
      "shastry_geographic_access_controls",
      "Response 2 Shastry controls",
      "public_iv",
      "candidate_robustness",
      "Could market access/geography confound linguistic distance with gains from post-liberalization economic change?",
      "predetermined_geographic_access",
      "registered consumption endpoint designs",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "region/state + distance to 1991 major city + coast",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "requires_data",
      multiplicity_family = "shastry_geographic_access",
      prerequisite = "frozen 1991 city list and reviewed coastline geometry",
      implementation_status = "data_unavailable",
      candidate_cells = n_consumption * 2L,
      implemented_cells = 0L,
      rationale = "The reference identifies major-city distance and coast as theoretically motivated predetermined access controls, but they should not be fabricated from current population or substituted with unrelated rail-distance measures."
    ),
    candidate_design_row(
      "shastry_hindi_belt_region_comparison",
      "Response 2 Shastry controls",
      "first_stage_relevance",
      "candidate_robustness",
      "Does the broad-region relevance pattern reflect the distinctive Hindi-belt political/institutional environment?",
      "regional_institutional_control",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "region/no-state designs + Hindi-belt indicator",
      "first_stage",
      "estimate",
      prerequisite = "Shastry's published Hindi-belt state definition frozen on Census-2001 state codes",
      implementation_status = if (n_hindi_belt == 2L) "implemented" else "partial",
      candidate_cells = 2L,
      implemented_cells = n_hindi_belt,
      execution_cells = n_hindi_belt,
      rationale = paste(
        "Shastry defines this institutional control independently of district Hindi-speaker share.",
        "The diagnostic adds that state-level indicator to the main-control first stage with no FE and",
        "six-region FE on one common support; state FE are excluded because they absorb the indicator."
      )
    ),
    candidate_design_row(
      "shastry_child_population_5_19_comparison",
      "Response 2 Shastry controls",
      "first_stage_relevance",
      "candidate_robustness",
      "Does Shastry's age-eligible child-population scale control materially change the broad first-stage relationship?",
      "predetermined_demographic_control",
      "EMI/EMIE treatment relevance",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "main Census controls + log Census-2001 population age 5-19, with no FE or six-region FE",
      "first_stage",
      "estimate",
      prerequisite = "Census 2001 C-14 five-year age bands",
      implementation_status = if (n_child_population == 2L) "implemented" else "partial",
      candidate_cells = 2L,
      implemented_cells = n_child_population,
      execution_cells = n_child_population,
      rationale = "Response 2 recommends Shastry's child-population control only as a comparison specification. C-14 directly supplies district counts for ages 5-9, 10-14, and 15-19, so no new source or age interpolation is needed."
    ),
    candidate_design_row(
      "genuine_emi_school_supply",
      "Response 2 genuine EMI supply",
      "district_schooling_mechanism",
      "future_goal",
      "Does the local stock of schools offering English-medium instruction predict realized EMI exposure separately from enrollment demand?",
      "school_level_emi_supply",
      "district share of schools offering EMI",
      "not_applicable",
      "not_applicable",
      "school-level medium-of-instruction census or complete DISE school microdata",
      "descriptive+mechanism",
      "requires_data",
      prerequisite = "school-level medium-of-instruction records with complete school denominators",
      implementation_status = "data_unavailable",
      candidate_cells = NA_integer_,
      implemented_cells = 0L,
      rationale = "The archived DISE district-report-card files report children by medium, not the number of schools offering each medium, and explicitly warn that medium enrollment applies only to reporting schools. Re-labeling enrollment shares as school supply would violate the measurement contract."
    ),
    candidate_design_row(
      "ihds_emi_capability_mobility_followup",
      "Response 2 IHDS extension",
      "future_microdata",
      "future_goal",
      "Can individual school medium be linked longitudinally to English capability, migration, and early labor-market outcomes?",
      "individual_longitudinal_linkage",
      "English proficiency, migration, employment, occupation, wages",
      "individual EMI exposure",
      "not_applicable",
      "IHDS-I/II individual and household covariates",
      "longitudinal_microdata",
      "requires_data",
      prerequisite = "reviewed IHDS-I to IHDS-II linkage and feasibility sample",
      implementation_status = "deferred",
      candidate_cells = NA_integer_,
      implemented_cells = 0L,
      rationale = "This is a separately scoped follow-on paper, not a missing district-IV cell. The first task is a linkage/feasibility study, as specified in Response 2."
    ),
    candidate_design_row(
      "low_cost_private_school_followup",
      "Response 2 low-cost private schools",
      "future_schooling_project",
      "future_goal",
      "How did affordable private schooling evolve across the RTE period, and how does affordability interact with EMI and school quality?",
      "school_sector_affordability",
      "private-school participation, fees, quality, EMI",
      "not_applicable",
      "not_applicable",
      "NSS64/NSS71 expenditure or school-fee microdata",
      "descriptive+longitudinal",
      "requires_data",
      prerequisite = "predeclared affordability definition; private unaided must not be equated with low-cost",
      implementation_status = "deferred",
      candidate_cells = NA_integer_,
      implemented_cells = 0L,
      rationale = "Response 2 explicitly treats this as a separate paper-scale project. Fee/cost information is required before any school can be called low-cost."
    ),
    candidate_design_row(
      "mother_tongue_vs_emi_learning_followup",
      "Response 2 mother-tongue instruction",
      "future_learning_project",
      "future_goal",
      "How do mother-tongue instruction and English-medium instruction differ in comprehension and learning outcomes?",
      "instruction_language_learning",
      "learning and comprehension outcomes",
      "mother-tongue instruction versus EMI",
      "not_applicable",
      "individual/school learning covariates",
      "learning_outcome_models",
      "requires_data",
      prerequisite = "data observing instructional language and comparable learning outcomes",
      implementation_status = "deferred",
      candidate_cells = NA_integer_,
      implemented_cells = 0L,
      rationale = "Current NSS/DISE medium fields do not observe classroom language fidelity or comparable learning outcomes. The distinction remains scientifically important but belongs to a future data-supported project."
    ),
    candidate_design_row(
      "shastry_1987_wage_controls",
      "Response 2 Shastry controls",
      "replication_robustness",
      "deferred_replication",
      "Would closer replication of Shastry's pre-period wage and educated-wage controls materially change the historical comparison?",
      "historical_labor_market_controls",
      "Shastry-comparison designs",
      preferred_iv_variables()$treatment,
      "preferred Shastry nonzero-mean distance",
      "historical wage/educated-wage controls",
      "first_stage+outcome_robustness",
      "requires_data",
      prerequisite = "NSS 43rd-round geography and weighting subsystem",
      implementation_status = "deferred",
      candidate_cells = NA_integer_,
      implemented_cells = 0L,
      rationale = "The reference regards these controls as reconstructible but low priority because Census-2001 controls already cover much of the substantive dimension; no proxy should be invented."
    ),
    candidate_design_row(
      "consumption_full_diagnostic_cartesian",
      "Specification-governance critique",
      "consumption_iv",
      "non_goal",
      "Should every endpoint be crossed mechanically with every diagnostic IV specification?",
      "mechanical_cartesian",
      "registered consumption endpoint designs",
      preferred_iv_variables()$treatment,
      "all diagnostic constructions",
      "all diagnostic adjustments",
      "first_stage+reduced_form+2sls+anderson_rubin",
      "do_not_estimate",
      admissible = FALSE,
      admissibility_reason = "mechanical_cartesian_without_scientific_estimand",
      implementation_status = "not_applicable",
      candidate_cells = n_consumption * n_diagnostic_iv,
      implemented_cells = n_consumption,
      rationale = "Representability is not justification. The diagnostic universe exists to understand identification, not to create an outcome-model specification search."
    ),
    candidate_design_row(
      "cross_robustness_axes_without_interaction_rationale",
      "Specification-governance critique",
      "consumption_iv",
      "non_goal",
      "Should alternative outcome, treatment, instrument, and control robustness axes be fully crossed merely because each axis is individually defensible?",
      "mechanical_cross_axis_interactions",
      "all robustness outcomes",
      "all robustness treatments",
      "all robustness instruments",
      "all robustness controls",
      "all estimators",
      "do_not_estimate",
      admissible = FALSE,
      admissibility_reason = "cross_axis_interaction_lacks_independent_theory",
      implementation_status = "not_applicable",
      candidate_cells = NA_integer_,
      implemented_cells = 0L,
      rationale = "Each robustness axis can be scientifically justified without implying that every interaction among robustness axes is itself a scientific estimand; cross-axis interactions require their own theory and preregistration."
    ),
    candidate_design_row(
      "posttreatment_mechanisms_as_controls",
      "Response 3 methodological guardrail",
      "public_iv",
      "non_goal",
      "Should post-treatment mechanisms be conditioned on in the preferred outcome equation?",
      "bad_control",
      "post-treatment Census/firm/migration/labor measures",
      preferred_iv_variables()$treatment,
      "not_applicable",
      "not_applicable",
      "not_applicable",
      "do_not_estimate",
      admissible = FALSE,
      admissibility_reason = "post_treatment_bad_control_changes_estimand",
      implementation_status = "not_applicable",
      candidate_cells = NA_integer_,
      implemented_cells = 0L,
      rationale = "Post-treatment mechanisms are outcomes/evidence layers, not preferred baseline controls; conditioning on them would change the estimand and can induce bad-control bias."
    )
  )
  candidate_design_frame(rows)
}
