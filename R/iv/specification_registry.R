# Canonical IV specification metadata used by estimation and diagnostics.

order_iv_controls <- function(controls, canonical = census_2001_diagnostic_controls()) {
  canonical[canonical %in% unique(controls)]
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

iv_control_blocks <- function() {
  list(
    basic_scale_geography = c(
      "log_population_2001", "urban_share_2001", "log_population_density_2001"
    ),
    social_composition = c("sc_share_2001", "st_share_2001", "muslim_share_2001"),
    human_capital = c("adult_secondary_plus_share_2001", "literacy_share_2001"),
    demography = "dependency_ratio_2001",
    economic_structure = c(
      "worker_share_2001", "cultivator_share_workers_2001",
      "agricultural_labourer_share_workers_2001"
    ),
    basic_development = "electricity_access_share_2001"
  )
}

iv_control_block_membership <- function() {
  blocks <- iv_control_blocks()
  blocks$economic_structure <- unique(c(
    "agricultural_worker_share_2001", blocks$economic_structure
  ))
  blocks
}

iv_included_control_blocks <- function(controls) {
  blocks <- iv_control_block_membership()
  names(blocks)[vapply(blocks, function(block) any(block %in% controls), logical(1))]
}

iv_without_human_capital <- function(controls) {
  setdiff(controls, iv_control_blocks()$human_capital)
}

iv_instrument_constructions <- function() {
  list(
    nonzero_mean = list(
      label = "Mean distance among speakers above zero",
      excluded = "ling_distance_nonzero_mean",
      included = character()
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
      label = "Five distance shares with unmapped share controlled",
      excluded = linguistic_distance_excluded_instruments("all"),
      included = "ling_unmapped_speaker_share"
    ),
    distance_shares_mapped = list(
      label = "Five distance shares; mapped-speaker denominator",
      excluded = linguistic_distance_excluded_instruments("mapped"),
      included = character()
    )
  )
}

iv_adjustment_sets <- function() {
  list(
    unadjusted = list(
      label = "Unadjusted", fixed_effect = "none", controls = character(), tier = "B"
    ),
    region_main = list(
      label = "Six-region FE + main controls", fixed_effect = "region",
      controls = census_2001_main_controls(), tier = "B"
    ),
    region_expanded = list(
      label = "Six-region FE + expanded controls", fixed_effect = "region",
      controls = census_2001_absorption_controls(), tier = "B"
    ),
    state_main = list(
      label = "State FE + main controls", fixed_effect = "state",
      controls = census_2001_main_controls(), tier = "A"
    ),
    state_expanded = list(
      label = "State FE + expanded controls", fixed_effect = "state",
      controls = census_2001_absorption_controls(), tier = "B"
    )
  )
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
  panel_variant,
  sample_rule,
  cluster = "state_code_2001",
  tier = "B",
  sequence = NA_integer_
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
    controls = I(list(order_iv_controls(controls))),
    included_language_controls = I(list(included_language_controls)),
    excluded_instruments = I(list(excluded_instruments)),
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
  sample_rule = "alternative_distance_common_support"
) {
  adjustments <- iv_adjustment_sets()
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
        panel_variant = panel_variant,
        sample_rule = sample_rule,
        tier = adjustment$tier,
        sequence = sequence
      )
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

iv_absorption_adjustments <- function() {
  main <- census_2001_main_controls()
  expanded <- census_2001_absorption_controls()
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
      "Six-region FE + main controls without human capital", "region", iv_without_human_capital(main)
    ),
    state_fe_main_without_human_capital = list(
      "State FE + main controls without human capital", "state", iv_without_human_capital(main)
    ),
    region_fe_expanded_without_human_capital = list(
      "Six-region FE + expanded controls without human capital", "region", iv_without_human_capital(expanded)
    ),
    state_fe_expanded_without_human_capital = list(
      "State FE + expanded controls without human capital", "state", iv_without_human_capital(expanded)
    )
  )
  blocks <- iv_control_blocks()
  cumulative <- lapply(seq_along(blocks), function(i) {
    order_iv_controls(unlist(blocks[seq_len(i)], use.names = FALSE))
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
  sample_rule = "alternative_distance_common_support"
) {
  construction <- iv_instrument_constructions()$nonzero_mean
  adjustments <- iv_absorption_adjustments()
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
      panel_variant = panel_variant,
      sample_rule = sample_rule,
      tier = "B",
      sequence = i
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
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
  sample_rule = "alternative_distance_common_support"
) {
  base <- iv_specification_registry(outcome, treatment, panel_variant, sample_rule)
  absorption <- iv_absorption_specification_registry(outcome, treatment, panel_variant, sample_rule)
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
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  stats::reformulate(
    unique(c(excluded, included, controls, iv_fixed_effect_terms(specification$fixed_effect[[1]]))),
    response = specification$treatment[[1]]
  )
}

iv_specification_cluster_variable <- function(specification) {
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
      "first_stage_joint_f", "partial_r_squared", "balance_covariates",
      "balance_joint", "anderson_rubin", "monotonicity_shape", "overidentification"
    ),
    family = c(
      "relevance", "relevance", "independence", "independence",
      "weak_identification", "monotonicity", "overidentification"
    ),
    requires_outcome = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE),
    requires_overidentified = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    min_instruments = c(1L, 1L, 1L, 1L, 1L, 1L, 2L),
    max_instruments = c(Inf, Inf, Inf, 1L, Inf, 1L, Inf),
    implemented = rep(TRUE, 7L),
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
