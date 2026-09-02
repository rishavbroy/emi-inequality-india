# Cross-family analysis-design ontology.
#
# Specialized registries remain authoritative for estimation. This module only
# projects them onto one common schema so the repository can inventory what is
# actually implemented without constructing an indiscriminate Cartesian product
# of outcomes, treatments, instruments, controls, and estimators.

analysis_design_columns <- function() {
  c(
    "analysis_id", "family", "specification_id", "outcome", "treatment",
    "instrument", "instrument_vintage", "adjustment_set", "fixed_effect",
    "estimand", "estimator", "inference", "sample_rule", "analysis_role",
    "admissible", "reason", "implemented"
  )
}

analysis_design_frame <- function(...) {
  out <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
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
    fixed_effect = character(), estimand = character(), estimator = character(),
    inference = character(), sample_rule = character(), analysis_role = character(),
    admissible = logical(), reason = character(), implemented = logical()
  ))

  estimand_value <- if (length(estimand) == 1L) rep(estimand, nrow(specs)) else estimand
  if ("estimand" %in% names(specs)) estimand_value <- plain_chr(specs$estimand)
  role_value <- if (length(analysis_role) == 1L) rep(analysis_role, nrow(specs)) else analysis_role
  vintage_value <- if (length(instrument_vintage) == 1L) {
    rep(instrument_vintage, nrow(specs))
  } else instrument_vintage

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
    adjustment_set = plain_chr(specs$adjustment_id),
    fixed_effect = plain_chr(specs$fixed_effect),
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

compile_analysis_design_registry <- function(
    consumption_iv_specifications,
    english_opportunity_measure_registry,
    control_registry = NULL,
    public_iv_specifications = public_iv_specification_registry(control_registry)) {
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
  out <- safe_bind_rows(list(
    analysis_design_public_iv(public_iv_specifications),
    core_iv,
    consumption,
    analysis_design_district_mechanisms(
      english_opportunity_measure_registry, control_registry
    ),
    analysis_design_c17(),
    analysis_design_dise(control_registry = control_registry),
    analysis_design_census_mechanisms(control_registry),
    analysis_design_economic_census_mechanisms(control_registry),
    analysis_design_labor_mechanisms(control_registry),
    analysis_design_historical_first_stages(control_registry)
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
    "candidate_id", "analysis_family", "design_role", "outcome_scope",
    "treatment_scope", "instrument_scope", "adjustment_scope",
    "estimator_scope", "admissible", "implementation_status",
    "candidate_cells", "implemented_cells", "rationale"
  )
}

candidate_design_frame <- function(...) {
  out <- data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
  missing <- setdiff(candidate_design_columns(), names(out))
  if (length(missing)) {
    stop(
      "Candidate-design rows are missing columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  out <- out[candidate_design_columns()]
  allowed <- c("implemented", "partial", "unimplemented", "not_applicable")
  if (!nrow(out) || anyDuplicated(out$candidate_id) || any(!nzchar(out$candidate_id)) ||
      any(!out$implementation_status %in% allowed) || any(!nzchar(out$rationale))) {
    stop("Candidate-design ledger is malformed.", call. = FALSE)
  }
  impossible <- out$implemented_cells > out$candidate_cells &
    is.finite(out$implemented_cells) & is.finite(out$candidate_cells)
  if (any(impossible)) {
    stop("Candidate-design implemented cells cannot exceed candidate cells.", call. = FALSE)
  }
  out
}

build_iv_candidate_design_ledger <- function(
    public_specifications,
    consumption_specifications,
    control_registry = NULL) {
  public_specs <- as_iv_specifications(public_specifications)
  consumption_specs <- as_iv_specifications(consumption_specifications)
  n_candidate_iv <- length(iv_candidate_design_adjustments()) *
    length(iv_candidate_design_constructions())
  diagnostic_specs <- iv_diagnostic_specification_registry(
    control_registry = control_registry
  )
  n_consumption <- nrow(consumption_specs)
  n_diagnostic_iv <- nrow(diagnostic_specs)

  candidate_design_frame(
    candidate_id = c(
      "public_headline_registered",
      "consumption_registered_dynamics",
      "consumption_candidate_scalar_iv_grid",
      "consumption_full_diagnostic_cartesian",
      "consumption_alternative_welfare_outcomes",
      "emi_intensive_margin_robustness",
      "historical_1991_adjustment_robustness",
      "posttreatment_mechanisms_as_controls"
    ),
    analysis_family = c(
      "public_iv", "consumption_iv", "consumption_iv", "consumption_iv",
      "consumption_iv", "consumption_iv", "public_iv", "public_iv"
    ),
    design_role = c(
      "primary_and_registered_robustness", "registered_dynamic_family",
      "candidate_robustness", "non_goal", "candidate_robustness",
      "candidate_robustness", "candidate_robustness", "non_goal"
    ),
    outcome_scope = c(
      "public consumption change/ANCOVA plus inherited nominal robustness",
      "registered real-mean-MPCE endpoint ANCOVA/change designs",
      "registered consumption endpoint designs",
      "registered consumption endpoint designs",
      "registered mean-log/median/bottom-40 welfare outcomes",
      "preferred real-consumption outcome family",
      "preferred public real-consumption outcome",
      "post-treatment Census/firm/migration/labor measures"
    ),
    treatment_scope = c(
      rep(preferred_iv_variables()$treatment, 5L),
      "emi_share_enrolled_0708",
      preferred_iv_variables()$treatment,
      preferred_iv_variables()$treatment
    ),
    instrument_scope = c(
      "nonzero_mean", "nonzero_mean",
      "Shastry/Glottolog/Dyen scalar candidates",
      "all diagnostic constructions",
      "Shastry/Glottolog/Dyen scalar candidates",
      "Shastry/Glottolog/Dyen scalar candidates",
      "nonzero_mean", "not_applicable"
    ),
    adjustment_scope = c(
      "registered public model-specific adjustments",
      "state_main",
      "region_main/state_main",
      "all diagnostic adjustments",
      "region_main/state_main",
      "region_main/state_main",
      "region/state with a separately registered 1991 predetermined set",
      "not_applicable"
    ),
    estimator_scope = c(
      "2sls", "first_stage+reduced_form+2sls+anderson_rubin",
      rep("first_stage+reduced_form+2sls+anderson_rubin", 4L),
      "2sls_with_weak_iv_diagnostics", "not_applicable"
    ),
    admissible = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
    implementation_status = c(
      "implemented", "implemented", "partial", "not_applicable",
      "unimplemented", "unimplemented", "unimplemented", "not_applicable"
    ),
    candidate_cells = c(
      nrow(public_specs), n_consumption,
      n_consumption * n_candidate_iv,
      n_consumption * n_diagnostic_iv,
      NA_integer_, n_candidate_iv, 2L, NA_integer_
    ),
    implemented_cells = c(
      nrow(public_specs), n_consumption,
      n_consumption, n_consumption,
      0L, 0L, 0L, 0L
    ),
    rationale = c(
      "Public output contract; all rows are now canonical IV specifications.",
      "Registered before estimation and tied to explicit endpoint/estimand metadata.",
      "The same six scalar-IV designs already used for post-treatment mechanisms provide a bounded robustness family without opening the full diagnostic search space.",
      "A mechanical endpoint-by-93-design Cartesian product is specification search rather than a theory-motivated robustness family.",
      "Distributional/functional-form outcomes are already measured, but any causal expansion should be separately registered with endpoint comparability and multiplicity rules before estimation.",
      "The enrolled-child EMI share is a substantively distinct intensive-margin treatment and may be a predeclared robustness family; it is not a replacement for all-child EMI exposure.",
      "A 1991 predetermined adjustment comparison is temporally distinct from adding more 2001 covariates and should be registered as a dedicated robustness family before use.",
      "Post-treatment Census, firm, migration, housing, and labor measures are outcomes/mechanisms rather than preferred controls."
    )
  )
}
