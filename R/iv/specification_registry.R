# Canonical IV specification metadata used by estimation and diagnostics.

order_iv_controls <- function(controls, canonical = census_2001_diagnostic_controls()) {
  controls <- unique(plain_chr(controls))
  c(
    canonical[canonical %in% controls],
    controls[!controls %in% canonical]
  )
}

iv_fixed_effect_terms <- function(fixed_effect = "none") {
  switch(
    fixed_effect,
    none = character(),
    region = "factor(region)",
    state = "factor(state_code_2001)",
    stop("Unknown IV fixed-effect specification: ", fixed_effect, call. = FALSE)
  )
}

iv_control_blocks <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  registry <- registry[registry$absorption_control %in% TRUE, , drop = FALSE]
  block_ids <- unique(registry$control_block[order(registry$block_sequence, registry$sequence)])
  stats::setNames(lapply(block_ids, function(block) {
    rows <- registry[registry$control_block == block, , drop = FALSE]
    rows$variable[order(rows$sequence)]
  }), block_ids)
}

iv_control_block_membership <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  active_blocks <- names(iv_control_blocks(registry))
  registry <- registry[
    registry$control_block %in% active_blocks &
      (registry$main_paper %in% TRUE | registry$absorption_control %in% TRUE),
    , drop = FALSE
  ]
  stats::setNames(lapply(active_blocks, function(block) {
    rows <- registry[registry$control_block == block, , drop = FALSE]
    rows$variable[order(rows$sequence)]
  }), active_blocks)
}

iv_included_control_blocks <- function(controls, control_registry = NULL) {
  blocks <- iv_control_block_membership(control_registry)
  names(blocks)[vapply(blocks, function(block) any(block %in% controls), logical(1))]
}

iv_without_human_capital <- function(controls, control_registry = NULL) {
  setdiff(controls, iv_control_block_membership(control_registry)$human_capital)
}

iv_instrument_constructions <- function() {
  list(
    nonzero_mean = list(
      label = "Mean distance among speakers above zero",
      excluded = "ling_distance_nonzero_mean",
      included = character(),
      coverage = "ling_mapped_speaker_share"
    ),
    distant_share = list(
      label = "Share speaking languages at distance three or higher",
      excluded = "ling_share_distance_ge3",
      included = character()
    ),
    top3_legacy = list(
      label = "Legacy top-three weighted mean",
      excluded = "ling_distance_top3_legacy",
      included = character()
    ),
    nonzero_mean_hindi_urdu = list(
      label = "Nonzero mean with combined Hindi-Urdu share",
      excluded = "ling_distance_nonzero_mean",
      included = "hindi_urdu_share"
    ),
    nonzero_mean_shastry = list(
      label = "Nonzero mean with Shastry composition controls",
      excluded = "ling_distance_nonzero_mean",
      included = c("hindi_urdu_share", "native_english_share")
    ),
    nonzero_mean_sensitivity_low = list(
      label = "Shastry nonzero mean under joint lower-degree adjudication sensitivity",
      excluded = "ling_distance_nonzero_mean_sensitivity_low",
      included = c("hindi_urdu_share", "native_english_share"),
      coverage = "ling_sensitivity_mapped_speaker_share"
    ),
    nonzero_mean_sensitivity_high = list(
      label = "Shastry nonzero mean under joint upper-degree adjudication sensitivity",
      excluded = "ling_distance_nonzero_mean_sensitivity_high",
      included = c("hindi_urdu_share", "native_english_share"),
      coverage = "ling_sensitivity_mapped_speaker_share"
    ),
    nonzero_mean_hindi_urdu_separate = list(
      label = "Nonzero mean with separate Hindi and Urdu shares",
      excluded = "ling_distance_nonzero_mean",
      included = c("hindi_share", "urdu_share")
    ),
    distance_shares_all = list(
      label = "Five distance shares; all-speaker denominator",
      excluded = linguistic_distance_excluded_instruments("all"),
      included = character()
    ),
    distance_shares_all_unmapped = list(
      label = "Five distance shares with unresolved and English shares controlled",
      excluded = linguistic_distance_excluded_instruments("all"),
      included = c("ling_unmapped_speaker_share", "native_english_share")
    ),
    distance_shares_mapped = list(
      label = "Five distance shares; mapped-speaker denominator",
      excluded = linguistic_distance_excluded_instruments("mapped"),
      included = character()
    ),
    glottolog_mean = list(
      label = "Glottolog edge-distance mean among non-Hindi/Urdu speakers",
      excluded = "ling_distance_glottolog_nonhindi_mean",
      included = character(),
      coverage = "ling_glottolog_mapped_speaker_share"
    ),
    glottolog_mean_shastry = list(
      label = "Glottolog edge-distance mean with Shastry composition controls",
      excluded = "ling_distance_glottolog_nonhindi_mean",
      included = c("hindi_urdu_share", "native_english_share"),
      coverage = "ling_glottolog_mapped_speaker_share"
    ),
    dyen_noncognate = list(
      label = "Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers",
      excluded = "ling_distance_dyen_noncognate_pct",
      included = character(),
      coverage = "ling_dyen_mapped_speaker_share"
    ),
    dyen_noncognate_shastry = list(
      label = "Dyen/Shastry noncognate percentage with composition controls",
      excluded = "ling_distance_dyen_noncognate_pct",
      included = c("hindi_urdu_share", "native_english_share"),
      coverage = "ling_dyen_mapped_speaker_share"
    )
  ) |>
    lapply(function(x) {
      if (is.null(x$coverage)) x$coverage <- "ling_mapped_speaker_share"
      x
    })
}

iv_candidate_design_adjustments <- function() {
  c("region_main", "state_main")
}

iv_adjustment_sets <- function(control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  list(
    unadjusted = list(
      label = "Unadjusted", fixed_effect = "none", controls = character(), tier = "B"
    ),
    region_main = list(
      label = "Six-region FE + main controls", fixed_effect = "region",
      controls = census_2001_main_controls(control_registry), tier = "A"
    ),
    region_expanded = list(
      label = "Six-region FE + expanded controls", fixed_effect = "region",
      controls = census_2001_absorption_controls(control_registry), tier = "B"
    ),
    state_main = list(
      label = "State FE + main controls", fixed_effect = "state",
      controls = census_2001_main_controls(control_registry), tier = "A"
    ),
    state_expanded = list(
      label = "State FE + expanded controls", fixed_effect = "state",
      controls = census_2001_absorption_controls(control_registry), tier = "B"
    )
  )
}

bind_iv_specification_rows <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

as_iv_specifications <- function(specifications) {
  x <- as.data.frame(specifications, stringsAsFactors = FALSE)
  required <- c(
    "specification_id", "outcome", "treatment", "fixed_effect",
    "controls", "included_language_controls", "excluded_instruments", "cluster"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "IV specifications are missing canonical columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  list_columns <- c("controls", "included_language_controls", "excluded_instruments")
  invalid <- list_columns[!vapply(x[list_columns], is.list, logical(1))]
  if (length(invalid)) {
    stop(
      "IV specification list-column contract was lost for: ",
      paste(invalid, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(x) && anyDuplicated(plain_chr(x$specification_id))) {
    stop("IV specification_id values must be unique.", call. = FALSE)
  }
  x
}

as_single_iv_specification <- function(specification) {
  x <- as_iv_specifications(specification)
  if (nrow(x) != 1L) {
    stop("A single canonical IV specification is required.", call. = FALSE)
  }
  x
}

iv_specification_row <- function(
  specification_id,
  adjustment_id,
  adjustment,
  construction_id,
  construction,
  outcome,
  treatment,
  fixed_effect,
  controls,
  included_language_controls,
  excluded_instruments,
  mapping_coverage_variable,
  panel_variant,
  sample_rule,
  cluster = "state_code_2001",
  tier = "B",
  sequence = NA_integer_,
  control_registry = NULL
) {
  data.frame(
    specification_id = specification_id,
    adjustment_id = adjustment_id,
    adjustment = adjustment,
    construction_id = construction_id,
    construction = construction,
    outcome = outcome,
    treatment = treatment,
    fixed_effect = fixed_effect,
    controls = I(list(order_iv_controls(
      controls, census_2001_diagnostic_controls(control_registry)
    ))),
    included_language_controls = I(list(included_language_controls)),
    excluded_instruments = I(list(excluded_instruments)),
    mapping_coverage_variable = mapping_coverage_variable,
    n_endogenous = 1L,
    n_excluded_instruments = length(excluded_instruments),
    panel_variant = panel_variant,
    sample_rule = sample_rule,
    cluster = cluster,
    tier = tier,
    sequence = sequence,
    stringsAsFactors = FALSE
  )
}

iv_specification_registry <- function(
  outcome = "real_log_consumption_change",
  treatment = preferred_iv_variables()$treatment,
  panel_variant = "primary",
  sample_rule = "alternative_distance_common_support",
  control_registry = NULL
) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  adjustments <- iv_adjustment_sets(control_registry)
  constructions <- iv_instrument_constructions()
  rows <- list()
  sequence <- 0L
  for (adjustment_id in names(adjustments)) {
    for (construction_id in names(constructions)) {
      sequence <- sequence + 1L
      adjustment <- adjustments[[adjustment_id]]
      construction <- constructions[[construction_id]]
      rows[[sequence]] <- iv_specification_row(
        specification_id = paste(adjustment_id, construction_id, sep = "__"),
        adjustment_id = adjustment_id,
        adjustment = adjustment$label,
        construction_id = construction_id,
        construction = construction$label,
        outcome = outcome,
        treatment = treatment,
        fixed_effect = adjustment$fixed_effect,
        controls = adjustment$controls,
        included_language_controls = construction$included,
        excluded_instruments = construction$excluded,
        mapping_coverage_variable = construction$coverage,
        panel_variant = panel_variant,
        sample_rule = sample_rule,
        tier = adjustment$tier,
        sequence = sequence,
        control_registry = control_registry
      )
    }
  }
  bind_iv_specification_rows(rows)
}

iv_absorption_adjustments <- function(control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  main <- census_2001_main_controls(control_registry)
  expanded <- census_2001_absorption_controls(control_registry)
  base <- list(
    instrument_only = list("Instrument only", "none", character()),
    region_fe = list("Six-region fixed effects", "region", character()),
    state_fe = list("State fixed effects", "state", character()),
    census_controls = list("Main Census controls", "none", main),
    region_fe_census_controls = list("Six-region fixed effects + main Census controls", "region", main),
    state_fe_census_controls = list("State fixed effects + main Census controls", "state", main),
    expanded_controls = list("Expanded Census controls", "none", expanded),
    region_fe_expanded_controls = list("Six-region fixed effects + expanded Census controls", "region", expanded),
    state_fe_expanded_controls = list("State fixed effects + expanded Census controls", "state", expanded),
    region_fe_main_without_human_capital = list(
      "Six-region FE + main controls without human capital", "region", iv_without_human_capital(main, control_registry)
    ),
    state_fe_main_without_human_capital = list(
      "State FE + main controls without human capital", "state", iv_without_human_capital(main, control_registry)
    ),
    region_fe_expanded_without_human_capital = list(
      "Six-region FE + expanded controls without human capital", "region", iv_without_human_capital(expanded, control_registry)
    ),
    state_fe_expanded_without_human_capital = list(
      "State FE + expanded controls without human capital", "state", iv_without_human_capital(expanded, control_registry)
    )
  )
  blocks <- iv_control_blocks(control_registry)
  canonical <- census_2001_diagnostic_controls(control_registry)
  cumulative <- lapply(seq_along(blocks), function(i) {
    order_iv_controls(
      unlist(blocks[seq_len(i)], use.names = FALSE),
      canonical
    )
  })
  for (fixed_effect in c("region", "state")) {
    fixed_label <- if (identical(fixed_effect, "region")) "Six-region FE" else "State FE"
    for (i in seq_along(blocks)) {
      id <- paste0(fixed_effect, "_fe_plus_", names(blocks)[[i]])
      base[[id]] <- list(
        paste0(fixed_label, " + through ", gsub("_", " ", names(blocks)[[i]])),
        fixed_effect,
        cumulative[[i]]
      )
    }
  }
  base
}

iv_absorption_specification_registry <- function(
  outcome = "real_log_consumption_change",
  treatment = preferred_iv_variables()$treatment,
  panel_variant = "primary",
  sample_rule = "alternative_distance_common_support",
  control_registry = NULL
) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  construction <- iv_instrument_constructions()$nonzero_mean
  adjustments <- iv_absorption_adjustments(control_registry)
  rows <- lapply(seq_along(adjustments), function(i) {
    id <- names(adjustments)[[i]]
    adjustment <- adjustments[[i]]
    iv_specification_row(
      specification_id = paste0("absorption__", id),
      adjustment_id = id,
      adjustment = adjustment[[1]],
      construction_id = "nonzero_mean",
      construction = construction$label,
      outcome = outcome,
      treatment = treatment,
      fixed_effect = adjustment[[2]],
      controls = adjustment[[3]],
      included_language_controls = construction$included,
      excluded_instruments = construction$excluded,
      mapping_coverage_variable = construction$coverage,
      panel_variant = panel_variant,
      sample_rule = sample_rule,
      tier = "B",
      sequence = i,
      control_registry = control_registry
    )
  })
  bind_iv_specification_rows(rows)
}

iv_specification_signature <- function(specification) {
  paste(
    specification$outcome[[1]],
    specification$treatment[[1]],
    specification$fixed_effect[[1]],
    paste(sort(unlist(specification$controls[[1]], use.names = FALSE)), collapse = ";"),
    paste(sort(unlist(specification$included_language_controls[[1]], use.names = FALSE)), collapse = ";"),
    paste(sort(unlist(specification$excluded_instruments[[1]], use.names = FALSE)), collapse = ";"),
    specification$panel_variant[[1]],
    specification$sample_rule[[1]],
    sep = "|"
  )
}

iv_diagnostic_specification_registry <- function(
  outcome = "real_log_consumption_change",
  treatment = preferred_iv_variables()$treatment,
  panel_variant = "primary",
  sample_rule = "alternative_distance_common_support",
  control_registry = NULL
) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  base <- iv_specification_registry(
    outcome, treatment, panel_variant, sample_rule, control_registry
  )
  absorption <- iv_absorption_specification_registry(
    outcome, treatment, panel_variant, sample_rule, control_registry
  )
  base_signatures <- vapply(seq_len(nrow(base)), function(i) {
    iv_specification_signature(base[i, , drop = FALSE])
  }, character(1))
  absorption_signatures <- vapply(seq_len(nrow(absorption)), function(i) {
    iv_specification_signature(absorption[i, , drop = FALSE])
  }, character(1))
  absorption <- absorption[!absorption_signatures %in% base_signatures, , drop = FALSE]
  out <- rbind(base, absorption)
  out$sequence <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

iv_specification_formula <- function(specification) {
  specification <- as_single_iv_specification(specification)
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  fixed <- iv_fixed_effect_terms(specification$fixed_effect[[1]])
  make_iv_formula(
    specification$outcome[[1]],
    specification$treatment[[1]],
    excluded,
    controls = unique(c(included, controls)),
    fixed_effects = fixed
  )
}

iv_first_stage_formula <- function(specification) {
  specification <- as_single_iv_specification(specification)
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  stats::reformulate(
    unique(c(excluded, included, controls, iv_fixed_effect_terms(specification$fixed_effect[[1]]))),
    response = specification$treatment[[1]]
  )
}

iv_specification_cluster_variable <- function(specification) {
  specification <- as_single_iv_specification(specification)
  cluster <- plain_chr(specification$cluster[[1]] %||% "")
  cluster <- cluster[nzchar(cluster)]
  if (length(cluster) != 1L) {
    stop("IV specification must declare exactly one cluster variable.", call. = FALSE)
  }
  cluster
}

iv_specification_cluster <- function(data, specification) {
  variable <- iv_specification_cluster_variable(specification)
  if (!variable %in% names(data)) {
    stop("IV specification cluster variable is missing: ", variable, call. = FALSE)
  }
  data[[variable]]
}

iv_specification_variables <- function(specification, include_outcome = TRUE) {
  formula <- if (isTRUE(include_outcome)) {
    iv_specification_formula(specification)
  } else {
    iv_first_stage_formula(specification)
  }
  unique(c(all.vars(formula), iv_specification_cluster_variable(specification)))
}

iv_diagnostic_registry <- function() {
  data.frame(
    diagnostic_id = c(
      "first_stage_joint_f", "effective_f", "partial_r_squared",
      "balance_covariates", "balance_joint", "anderson_rubin",
      "monotonicity_shape", "overidentification"
    ),
    family = c(
      "relevance", "relevance", "relevance", "independence", "independence",
      "weak_identification", "monotonicity", "overidentification"
    ),
    requires_outcome = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
    requires_overidentified = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    min_instruments = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L),
    max_instruments = c(Inf, Inf, Inf, Inf, 1L, Inf, 1L, Inf),
    implemented = rep(TRUE, 8L),
    stringsAsFactors = FALSE
  )
}

iv_diagnostic_applicability <- function(
  specifications = iv_diagnostic_specification_registry()
) {
  diagnostics <- iv_diagnostic_registry()
  rows <- lapply(seq_len(nrow(specifications)), function(i) {
    spec <- specifications[i, , drop = FALSE]
    safe_bind_rows(lapply(seq_len(nrow(diagnostics)), function(j) {
      diagnostic <- diagnostics[j, , drop = FALSE]
      n_inst <- spec$n_excluded_instruments[[1]]
      n_endog <- spec$n_endogenous[[1]]
      has_outcome <- nzchar(spec$outcome[[1]])
      applicable <- n_inst >= diagnostic$min_instruments[[1]] &&
        n_inst <= diagnostic$max_instruments[[1]] &&
        (!diagnostic$requires_outcome[[1]] || has_outcome) &&
        (!diagnostic$requires_overidentified[[1]] || n_inst > n_endog)
      reason <- if (applicable) {
        NA_character_
      } else if (diagnostic$requires_overidentified[[1]] && n_inst <= n_endog) {
        "exactly_identified"
      } else if (n_inst > diagnostic$max_instruments[[1]]) {
        paste0("multi_instrument_", diagnostic$diagnostic_id, "_not_defined")
      } else if (diagnostic$requires_outcome[[1]] && !has_outcome) {
        "outcome_not_defined"
      } else {
        "insufficient_excluded_instruments"
      }
      data.frame(
        specification_id = spec$specification_id,
        diagnostic_id = diagnostic$diagnostic_id,
        diagnostic_family = diagnostic$family,
        applicable = applicable,
        implemented = diagnostic$implemented[[1]],
        will_run = applicable && diagnostic$implemented[[1]],
        reason = reason,
        stringsAsFactors = FALSE
      )
    }))
  })
  safe_bind_rows(rows)
}
