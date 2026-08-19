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
      rows[[sequence]] <- data.frame(
        specification_id = paste(adjustment_id, construction_id, sep = "__"),
        adjustment_id = adjustment_id,
        adjustment = adjustment$label,
        construction_id = construction_id,
        construction = construction$label,
        outcome = outcome,
        treatment = treatment,
        fixed_effect = adjustment$fixed_effect,
        controls = I(list(order_iv_controls(adjustment$controls))),
        included_language_controls = I(list(construction$included)),
        excluded_instruments = I(list(construction$excluded)),
        n_endogenous = 1L,
        n_excluded_instruments = length(construction$excluded),
        panel_variant = panel_variant,
        sample_rule = sample_rule,
        cluster = "state_code_2001",
        tier = adjustment$tier,
        sequence = sequence,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
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

iv_specification_variables <- function(specification) {
  cluster <- plain_chr(specification$cluster[[1]] %||% "")
  cluster <- cluster[nzchar(cluster)]
  unique(c(all.vars(iv_specification_formula(specification)), cluster))
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
    implemented = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

iv_diagnostic_applicability <- function(specifications = iv_specification_registry()) {
  diagnostics <- iv_diagnostic_registry()
  rows <- lapply(seq_len(nrow(specifications)), function(i) {
    spec <- specifications[i, , drop = FALSE]
    safe_bind_rows(lapply(seq_len(nrow(diagnostics)), function(j) {
      diagnostic <- diagnostics[j, , drop = FALSE]
      n_inst <- spec$n_excluded_instruments[[1]]
      n_endog <- spec$n_endogenous[[1]]
      has_outcome <- nzchar(spec$outcome[[1]])
      applicable <- n_inst >= diagnostic$min_instruments[[1]] &&
        (!diagnostic$requires_outcome[[1]] || has_outcome) &&
        (!diagnostic$requires_overidentified[[1]] || n_inst > n_endog)
      reason <- if (applicable) {
        NA_character_
      } else if (diagnostic$requires_overidentified[[1]] && n_inst <= n_endog) {
        "exactly_identified"
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
