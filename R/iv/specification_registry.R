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

iv_main_control_blocks <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  main <- census_2001_main_controls(registry)
  blocks <- iv_control_block_membership(registry)
  blocks <- lapply(blocks, intersect, y = main)
  blocks[vapply(blocks, length, integer(1)) > 0L]
}

iv_replace_main_controls <- function(
    remove = character(), add = character(), control_registry = NULL) {
  order_iv_controls(
    c(setdiff(census_2001_main_controls(control_registry), remove), add),
    census_2001_diagnostic_controls(control_registry)
  )
}

iv_adjustment <- function(label, fixed_effect, controls, ...) {
  list(
    label = label,
    fixed_effect = fixed_effect,
    controls = plain_chr(controls),
    ...
  )
}

iv_block_intervention_adjustments <- function(control_registry = NULL) {
  blocks <- iv_main_control_blocks(control_registry)
  main <- census_2001_main_controls(control_registry)
  rows <- list()
  for (fixed_effect in c("region", "state")) {
    fixed_label <- if (identical(fixed_effect, "region")) "Six-region FE" else "State FE"
    for (block_id in names(blocks)) {
      block <- blocks[[block_id]]
      label <- gsub("_", " ", block_id)
      rows[[paste(fixed_effect, "block_only", block_id, sep = "_")]] <- iv_adjustment(
        paste0(fixed_label, " + ", label, " block only"),
        fixed_effect,
        block
      )
      rows[[paste(fixed_effect, "main_without", block_id, sep = "_")]] <- iv_adjustment(
        paste0(fixed_label, " + main controls without ", label),
        fixed_effect,
        setdiff(main, block)
      )
    }
  }
  rows
}

iv_causal_control_strategy_adjustments <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  main <- census_2001_main_controls(registry)
  no_human_capital <- iv_without_human_capital(main, registry)
  rows <- list()
  for (fixed_effect in c("region", "state")) {
    fe_label <- if (fixed_effect == "region") "Six-region FE" else "State FE"
    rows[[paste(fixed_effect, "fe_only", sep = "_")]] <- iv_adjustment(
      label = fe_label,
      fixed_effect = fixed_effect,
      controls = character(),
      strategy = "geography_only",
      theoretical_role = paste(
        "Avoid conditioning on measured socioeconomic variables that may themselves",
        "lie on long-run pathways from linguistic structure."
      ),
      caution = "Places the exclusion burden on geographic fixed effects and the instrument design."
    )
    rows[[paste(fixed_effect, "compact_2001", sep = "_")]] <- iv_adjustment(
      label = paste0(fe_label, " + compact 2001 adjustment"),
      fixed_effect = fixed_effect,
      controls = main,
      strategy = "observed_exclusion_threat_adjustment",
      theoretical_role = paste(
        "Condition on a compact set of observed 2001 scale, composition, human-capital,",
        "economic-structure, demographic, and development differences."
      ),
      caution = paste(
        "These controls predate 2007-08 EMI but do not predate the historically determined",
        "linguistic instrument, so they are not automatically causally innocuous."
      )
    )
    rows[[paste(fixed_effect, "compact_2001_no_human_capital", sep = "_")]] <- iv_adjustment(
      label = paste0(fe_label, " + compact 2001 adjustment without human capital"),
      fixed_effect = fixed_effect,
      controls = no_human_capital,
      strategy = "potential_pathway_robustness",
      theoretical_role = paste(
        "Retain observed baseline adjustment while avoiding direct conditioning on",
        "pre-treatment education, a particularly plausible long-run language pathway."
      ),
      caution = "This is a pathway sensitivity, not a claim that other 2001 controls are necessarily exogenous."
    )
  }
  rows
}

iv_main_parameterization_adjustments <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  secondary <- "adult_secondary_plus_share_2001"
  literacy <- "literacy_share_2001"
  compact_economic <- "agricultural_worker_share_2001"
  decomposed_economic <- c(
    "worker_share_2001", "cultivator_share_workers_2001",
    "agricultural_labourer_share_workers_2001"
  )
  variants <- list(
    literacy = iv_replace_main_controls(
      remove = secondary, add = literacy, control_registry = registry
    ),
    decomposed_economic = iv_replace_main_controls(
      remove = compact_economic, add = decomposed_economic, control_registry = registry
    ),
    literacy_decomposed_economic = iv_replace_main_controls(
      remove = c(secondary, compact_economic),
      add = c(literacy, decomposed_economic),
      control_registry = registry
    )
  )
  rows <- list()
  for (fixed_effect in c("region", "state")) {
    fixed_label <- if (identical(fixed_effect, "region")) "Six-region FE" else "State FE"
    for (variant_id in names(variants)) {
      rows[[paste(fixed_effect, "main", variant_id, sep = "_")]] <- iv_adjustment(
        paste0(fixed_label, " + main-control ", gsub("_", " ", variant_id), " parameterization"),
        fixed_effect,
        variants[[variant_id]]
      )
    }
  }
  rows
}

iv_causal_control_parameterization_adjustments <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  canonical <- iv_adjustment_sets(registry)
  alternatives <- iv_main_parameterization_adjustments(registry)
  c(
    list(region_main = canonical$region_main),
    alternatives[grepl("^region_", names(alternatives))],
    list(state_main = canonical$state_main),
    alternatives[grepl("^state_", names(alternatives))]
  )
}

iv_distance_measure_registry <- function() {
  list(
    shastry_nonzero_mean = list(
      excluded = "ling_distance_nonzero_mean",
      coverage = "ling_mapped_speaker_share"
    ),
    shastry_distant_share = list(
      excluded = "ling_share_distance_ge3",
      coverage = "ling_mapped_speaker_share"
    ),
    legacy_top3 = list(
      excluded = "ling_distance_top3_legacy",
      coverage = "ling_mapped_speaker_share"
    ),
    shastry_nonzero_mean_sensitivity_low = list(
      excluded = "ling_distance_nonzero_mean_sensitivity_low",
      coverage = "ling_sensitivity_mapped_speaker_share"
    ),
    shastry_nonzero_mean_sensitivity_high = list(
      excluded = "ling_distance_nonzero_mean_sensitivity_high",
      coverage = "ling_sensitivity_mapped_speaker_share"
    ),
    distance_shares_all = list(
      excluded = linguistic_distance_excluded_instruments("all"),
      coverage = "ling_mapped_speaker_share"
    ),
    distance_shares_mapped = list(
      excluded = linguistic_distance_excluded_instruments("mapped"),
      coverage = "ling_mapped_speaker_share"
    ),
    glottolog_nonhindi_mean = list(
      excluded = "ling_distance_glottolog_nonhindi_mean",
      coverage = "ling_glottolog_mapped_speaker_share"
    ),
    dyen_noncognate = list(
      excluded = "ling_distance_dyen_noncognate_pct",
      coverage = "ling_dyen_mapped_speaker_share"
    )
  )
}

iv_language_adjustment_registry <- function() {
  list(
    none = character(),
    hindi_urdu = "hindi_urdu_share",
    shastry_composition = c("hindi_urdu_share", "native_english_share"),
    hindi_urdu_separate = c("hindi_share", "urdu_share"),
    unresolved_english = c("ling_unmapped_speaker_share", "native_english_share")
  )
}

iv_instrument_construction_registry <- function() {
  data.frame(
    construction_id = c(
      "nonzero_mean", "distant_share", "top3_legacy",
      "nonzero_mean_hindi_urdu", "nonzero_mean_shastry",
      "nonzero_mean_sensitivity_low", "nonzero_mean_sensitivity_high",
      "nonzero_mean_hindi_urdu_separate",
      "distance_shares_all", "distance_shares_all_unmapped",
      "distance_shares_mapped", "glottolog_mean", "glottolog_mean_shastry",
      "dyen_noncognate", "dyen_noncognate_shastry"
    ),
    distance_measure_id = c(
      "shastry_nonzero_mean", "shastry_distant_share", "legacy_top3",
      "shastry_nonzero_mean", "shastry_nonzero_mean",
      "shastry_nonzero_mean_sensitivity_low",
      "shastry_nonzero_mean_sensitivity_high",
      "shastry_nonzero_mean",
      "distance_shares_all", "distance_shares_all",
      "distance_shares_mapped", "glottolog_nonhindi_mean",
      "glottolog_nonhindi_mean", "dyen_noncognate", "dyen_noncognate"
    ),
    language_adjustment_id = c(
      "none", "none", "none",
      "hindi_urdu", "shastry_composition",
      "shastry_composition", "shastry_composition",
      "hindi_urdu_separate",
      "none", "unresolved_english",
      "none", "none", "shastry_composition",
      "none", "shastry_composition"
    ),
    label = c(
      "Mean distance among speakers above zero",
      "Share speaking languages at distance three or higher",
      "Legacy top-three weighted mean",
      "Nonzero mean with combined Hindi-Urdu share",
      "Nonzero mean with Shastry composition controls",
      "Shastry nonzero mean under joint lower-degree adjudication sensitivity",
      "Shastry nonzero mean under joint upper-degree adjudication sensitivity",
      "Nonzero mean with separate Hindi and Urdu shares",
      "Five distance shares; all-speaker denominator",
      "Five distance shares with unresolved and English shares controlled",
      "Five distance shares; mapped-speaker denominator",
      "Glottolog edge-distance mean among non-Hindi/Urdu speakers",
      "Glottolog edge-distance mean with Shastry composition controls",
      "Dyen/Shastry noncognate percentage among non-Hindi/Urdu speakers",
      "Dyen/Shastry noncognate percentage with composition controls"
    ),
    stringsAsFactors = FALSE
  )
}

iv_instrument_constructions <- function() {
  measures <- iv_distance_measure_registry()
  adjustments <- iv_language_adjustment_registry()
  registry <- iv_instrument_construction_registry()

  out <- lapply(seq_len(nrow(registry)), function(i) {
    row <- registry[i, , drop = FALSE]
    measure_id <- row$distance_measure_id[[1L]]
    adjustment_id <- row$language_adjustment_id[[1L]]
    if (!measure_id %in% names(measures)) {
      stop("Unknown linguistic-distance measure in IV construction registry: ", measure_id, call. = FALSE)
    }
    if (!adjustment_id %in% names(adjustments)) {
      stop("Unknown language adjustment in IV construction registry: ", adjustment_id, call. = FALSE)
    }
    measure <- measures[[measure_id]]
    list(
      label = row$label[[1L]],
      excluded = measure$excluded,
      included = adjustments[[adjustment_id]],
      coverage = measure$coverage,
      distance_measure_id = measure_id,
      language_adjustment_id = adjustment_id
    )
  })
  stats::setNames(out, registry$construction_id)
}

iv_candidate_design_adjustments <- function() {
  c("region_main", "state_main")
}

iv_candidate_design_constructions <- function() {
  c(
    primary_shastry = "nonzero_mean",
    robustness_glottolog = "glottolog_mean",
    robustness_dyen = "dyen_noncognate"
  )
}

# Shastry (2012, p. 299 in the published article; p. 17 in the
# circulated NBER draft) defines the Hindi belt for this robustness control as
# Bihar, Uttar Pradesh/Uttaranchal, Madhya Pradesh/Chhattisgarh, Haryana,
# Punjab, Rajasthan, Himachal Pradesh, Jharkhand, Chandigarh, and Delhi.
# Freeze the definition on Census-2001 state codes so later renamings do not
# change the historical specification.
shastry_hindi_belt_state_codes <- function() {
  c("02", "03", "04", "05", "06", "07", "08", "09", "10", "20", "22", "23")
}

shastry_hindi_belt_variable <- function() "shastry_hindi_belt"
shastry_child_population_variable <- function() "log_child_population_5_19_2001"

iv_shastry_added_control_first_stage_specifications <- function(
    added_control,
    added_control_label,
    specification_prefix,
    sample_rule,
    treatment = preferred_iv_variables()$treatment,
    control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  construction <- iv_instrument_constructions()$nonzero_mean
  main <- census_2001_main_controls(control_registry)
  adjustments <- list(
    main = iv_adjustment(
      paste0("Main Census controls + ", added_control_label),
      "none", c(main, added_control)
    ),
    region_main = iv_adjustment(
      paste0("Six-region FE + main Census controls + ", added_control_label),
      "region", c(main, added_control)
    )
  )
  rows <- lapply(seq_along(adjustments), function(i) {
    id <- names(adjustments)[[i]]
    adjustment <- adjustments[[i]]
    iv_specification_row(
      specification_id = paste0(specification_prefix, "__", id),
      adjustment_id = paste0(specification_prefix, "__", id),
      adjustment = adjustment$label,
      construction_id = "nonzero_mean",
      construction = construction$label,
      distance_measure_id = construction$distance_measure_id,
      language_adjustment_id = construction$language_adjustment_id,
      outcome = treatment,
      treatment = treatment,
      fixed_effect = adjustment$fixed_effect,
      controls = adjustment$controls,
      included_language_controls = construction$included,
      excluded_instruments = construction$excluded,
      mapping_coverage_variable = construction$coverage,
      panel_variant = "primary",
      sample_rule = sample_rule,
      tier = "B",
      sequence = i,
      control_registry = control_registry
    )
  })
  bind_iv_specification_rows(rows)
}

iv_hindi_belt_first_stage_specifications <- function(
    treatment = preferred_iv_variables()$treatment,
    control_registry = NULL) {
  iv_shastry_added_control_first_stage_specifications(
    added_control = shastry_hindi_belt_variable(),
    added_control_label = "Shastry Hindi-belt indicator",
    specification_prefix = "hindi_belt",
    sample_rule = "hindi_belt_first_stage_common_support",
    treatment = treatment,
    control_registry = control_registry
  )
}

iv_child_population_first_stage_specifications <- function(
    treatment = preferred_iv_variables()$treatment,
    control_registry = NULL) {
  iv_shastry_added_control_first_stage_specifications(
    added_control = shastry_child_population_variable(),
    added_control_label = "log population age 5-19",
    specification_prefix = "child_population",
    sample_rule = "child_population_first_stage_common_support",
    treatment = treatment,
    control_registry = control_registry
  )
}

iv_adjustment_sets <- function(control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  list(
    unadjusted = iv_adjustment(
      "Unadjusted", "none", character(), tier = "B"
    ),
    region_main = iv_adjustment(
      "Six-region FE + main controls", "region",
      census_2001_main_controls(control_registry), tier = "A"
    ),
    region_expanded = iv_adjustment(
      "Six-region FE + expanded controls", "region",
      census_2001_absorption_controls(control_registry), tier = "B"
    ),
    state_main = iv_adjustment(
      "State FE + main controls", "state",
      census_2001_main_controls(control_registry), tier = "A"
    ),
    state_expanded = iv_adjustment(
      "State FE + expanded controls", "state",
      census_2001_absorption_controls(control_registry), tier = "B"
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
  distance_measure_id,
  language_adjustment_id,
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
    distance_measure_id = distance_measure_id,
    language_adjustment_id = language_adjustment_id,
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


# Public headline specifications use the same canonical row contract as extended
# IV diagnostics. Model IDs are retained because tables, figures, and report
# values use these names as their public-output contract.
public_iv_specification_registry <- function(control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  variables <- preferred_iv_variables()
  construction <- iv_instrument_constructions()$nonzero_mean
  main_controls <- census_2001_main_controls(control_registry)
  rows <- list(
    consumption = list(
      outcome = "real_log_consumption_change",
      controls = main_controls,
      adjustment_id = "state_main",
      adjustment = "State FE + main Census-2001 controls",
      estimand = "change",
      role = "primary"
    ),
    consumption_ancova = list(
      outcome = "log_real_consumption_1718",
      controls = c(main_controls, "log_real_consumption_0708"),
      adjustment_id = "state_main_ancova",
      adjustment = "State FE + baseline real consumption + main Census-2001 controls",
      estimand = "ancova",
      role = "robustness"
    ),
    consumption_nominal = list(
      outcome = "log_consumption_difference",
      controls = main_controls,
      adjustment_id = "state_main_nominal",
      adjustment = "State FE + main Census-2001 controls; nominal outcome",
      estimand = "nominal_change",
      role = "legacy_outcome_robustness"
    ),
    consumption_legacy_controls = list(
      outcome = "log_consumption_difference",
      controls = legacy_2007_iv_controls(),
      adjustment_id = "state_legacy_2007_controls",
      adjustment = "State FE + inherited 2007 household controls; nominal outcome",
      estimand = "nominal_change",
      role = "legacy_control_robustness"
    )
  )

  out <- bind_iv_specification_rows(lapply(seq_along(rows), function(i) {
    id <- names(rows)[[i]]
    x <- rows[[i]]
    row <- iv_specification_row(
      specification_id = id,
      adjustment_id = x$adjustment_id,
      adjustment = x$adjustment,
      construction_id = "nonzero_mean",
      construction = construction$label,
      distance_measure_id = construction$distance_measure_id,
      language_adjustment_id = construction$language_adjustment_id,
      outcome = x$outcome,
      treatment = variables$treatment,
      fixed_effect = "state",
      controls = x$controls,
      included_language_controls = construction$included,
      excluded_instruments = construction$excluded,
      mapping_coverage_variable = construction$coverage,
      panel_variant = "primary",
      sample_rule = "public_model_specific_complete_case",
      tier = if (identical(x$role, "primary")) "A" else "B",
      sequence = i,
      control_registry = control_registry
    )
    row$estimand <- x$estimand
    row$analysis_role <- x$role
    row
  }))
  out
}

iv_specification_formulas <- function(specifications) {
  specs <- as_iv_specifications(specifications)
  stats::setNames(
    lapply(seq_len(nrow(specs)), function(i) {
      iv_specification_formula(specs[i, , drop = FALSE])
    }),
    plain_chr(specs$specification_id)
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
        distance_measure_id = construction$distance_measure_id,
        language_adjustment_id = construction$language_adjustment_id,
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

iv_historical_adjustment_comparison_adjustments <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  compact_2001 <- census_2001_main_controls(registry)
  remote_1991 <- historical_baseline_1991_pca_variables()
  list(
    region_compact_2001 = iv_adjustment(
      "Six-region FE + compact 2001 adjustment", "region", compact_2001,
      adjustment_vintage = "2001", adjustment_role = "benchmark_compact_2001"
    ),
    region_predetermined_1991 = iv_adjustment(
      "Six-region FE + population-interpolated PCA91 adjustment", "region", remote_1991,
      adjustment_vintage = "1991", adjustment_role = "remote_predetermined_baseline",
      caution = paste(
        "PCA91 covers a different but overlapping socioeconomic concept set from the compact 2001 controls;",
        "the comparison is a remote-baseline robustness exercise, not a pure same-variable vintage substitution."
      )
    ),
    state_compact_2001 = iv_adjustment(
      "State FE + compact 2001 adjustment", "state", compact_2001,
      adjustment_vintage = "2001", adjustment_role = "benchmark_compact_2001"
    ),
    state_predetermined_1991 = iv_adjustment(
      "State FE + population-interpolated PCA91 adjustment", "state", remote_1991,
      adjustment_vintage = "1991", adjustment_role = "remote_predetermined_baseline",
      caution = paste(
        "PCA91 covers a different but overlapping socioeconomic concept set from the compact 2001 controls;",
        "the comparison is a remote-baseline robustness exercise, not a pure same-variable vintage substitution."
      )
    )
  )
}



iv_historical_concept_matched_adjustments <- function(control_registry = NULL) {
  registry <- resolve_census_2001_control_registry(control_registry)
  compact_2001 <- census_2001_main_controls(registry)
  pca_1991 <- historical_baseline_1991_pca_variables()
  vanneman_1991 <- vanneman_historical_baseline_1991_variables()
  make <- function(fe, prefix) {
    list(
      compact_2001 = iv_adjustment(
        paste0(prefix, " + compact 2001 adjustment"), fe, compact_2001,
        adjustment_vintage = "2001", adjustment_role = "benchmark_compact_2001"
      ),
      pca_1991 = iv_adjustment(
        paste0(prefix, " + population-interpolated PCA91 adjustment"), fe, pca_1991,
        adjustment_vintage = "1991", adjustment_role = "remote_pca91_baseline"
      ),
      vanneman_1991 = iv_adjustment(
        paste0(prefix, " + Vanneman concept-matched 1991 adjustment"), fe, vanneman_1991,
        adjustment_vintage = "1991", adjustment_role = "concept_matched_vanneman_baseline",
        caution = paste(
          "Vanneman dist91 supplies closer analogues for urbanization, religion, educational attainment,",
          "agricultural employment, dependency, and electricity, but split 1991 districts are allocated",
          "to Census-2001 targets using the same frozen population-interpolation weights as PCA91."
        )
      )
    )
  }
  region <- make("region", "Six-region FE")
  state <- make("state", "State FE")
  c(
    setNames(region, paste0("region_", names(region))),
    setNames(state, paste0("state_", names(state)))
  )
}

iv_absorption_adjustments <- function(control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  main <- census_2001_main_controls(control_registry)
  expanded <- census_2001_absorption_controls(control_registry)
  base <- list(
    instrument_only = iv_adjustment("Instrument only", "none", character()),
    region_fe = iv_adjustment("Six-region fixed effects", "region", character()),
    state_fe = iv_adjustment("State fixed effects", "state", character()),
    census_controls = iv_adjustment("Main Census controls", "none", main),
    region_fe_census_controls = iv_adjustment("Six-region fixed effects + main Census controls", "region", main),
    state_fe_census_controls = iv_adjustment("State fixed effects + main Census controls", "state", main),
    expanded_controls = iv_adjustment("Expanded Census controls", "none", expanded),
    region_fe_expanded_controls = iv_adjustment(
      "Six-region fixed effects + expanded Census controls", "region", expanded
    ),
    state_fe_expanded_controls = iv_adjustment(
      "State fixed effects + expanded Census controls", "state", expanded
    ),
    region_fe_main_without_human_capital = iv_adjustment(
      "Six-region FE + main controls without human capital", "region",
      iv_without_human_capital(main, control_registry)
    ),
    state_fe_main_without_human_capital = iv_adjustment(
      "State FE + main controls without human capital", "state",
      iv_without_human_capital(main, control_registry)
    ),
    region_fe_expanded_without_human_capital = iv_adjustment(
      "Six-region FE + expanded controls without human capital", "region",
      iv_without_human_capital(expanded, control_registry)
    ),
    state_fe_expanded_without_human_capital = iv_adjustment(
      "State FE + expanded controls without human capital", "state",
      iv_without_human_capital(expanded, control_registry)
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
      base[[id]] <- iv_adjustment(
        paste0(fixed_label, " + through ", gsub("_", " ", names(blocks)[[i]])),
        fixed_effect,
        cumulative[[i]]
      )
    }
  }

  # The cumulative ladder is retained because it documents the historical
  # attenuation pattern, but it is order-dependent. Symmetric block-only and
  # leave-one-block-out interventions make each substantive control family
  # interpretable on its own. Alternative parameterizations are crossed only
  # where the control registry explicitly declares them as substitutes.
  base <- c(
    base,
    iv_block_intervention_adjustments(control_registry),
    iv_main_parameterization_adjustments(control_registry)
  )
  base
}

iv_absorption_specification_candidates <- function(
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
      adjustment = adjustment$label,
      construction_id = "nonzero_mean",
      construction = construction$label,
      distance_measure_id = construction$distance_measure_id,
      language_adjustment_id = construction$language_adjustment_id,
      outcome = outcome,
      treatment = treatment,
      fixed_effect = adjustment$fixed_effect,
      controls = adjustment$controls,
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


iv_specification_alias_map <- function(specifications) {
  if (!is.data.frame(specifications) || !nrow(specifications)) {
    stop("IV alias mapping requires a non-empty specification data frame.", call. = FALSE)
  }
  signatures <- vapply(seq_len(nrow(specifications)), function(i) {
    iv_specification_signature(specifications[i, , drop = FALSE])
  }, character(1))
  canonical_index <- match(signatures, signatures)
  data.frame(
    semantic_specification_id = plain_chr(specifications$specification_id),
    execution_specification_id = plain_chr(specifications$specification_id[canonical_index]),
    semantic_adjustment_id = plain_chr(specifications$adjustment_id),
    execution_adjustment_id = plain_chr(specifications$adjustment_id[canonical_index]),
    is_execution_alias = seq_len(nrow(specifications)) != canonical_index,
    stringsAsFactors = FALSE
  )
}

deduplicate_iv_specifications <- function(specifications) {
  aliases <- iv_specification_alias_map(specifications)
  out <- specifications[!aliases$is_execution_alias, , drop = FALSE]
  out$sequence <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}

iv_absorption_specification_aliases <- function(
    outcome = "real_log_consumption_change",
    treatment = preferred_iv_variables()$treatment,
    panel_variant = "primary",
    sample_rule = "alternative_distance_common_support",
    control_registry = NULL) {
  iv_specification_alias_map(iv_absorption_specification_candidates(
    outcome, treatment, panel_variant, sample_rule, control_registry
  ))
}

iv_absorption_specification_registry <- function(
    outcome = "real_log_consumption_change",
    treatment = preferred_iv_variables()$treatment,
    panel_variant = "primary",
    sample_rule = "alternative_distance_common_support",
    control_registry = NULL) {
  deduplicate_iv_specifications(iv_absorption_specification_candidates(
    outcome, treatment, panel_variant, sample_rule, control_registry
  ))
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
  deduplicate_iv_specifications(rbind(base, absorption))
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
