# Shared post-treatment mechanism inference on Census-2001 geography.

posttreatment_mechanism_specifications <- function(
    outcome,
    treatment = preferred_iv_variables()$treatment,
    sample_rule = "posttreatment_mechanism_common_support",
    control_registry = NULL) {
  registry <- iv_specification_registry(
    outcome = outcome,
    treatment = treatment,
    panel_variant = "primary",
    sample_rule = sample_rule,
    control_registry = control_registry
  )
  construction_ids <- unname(alternative_distance_design_constructions())
  keep <- registry$adjustment_id %in% iv_candidate_design_adjustments() &
    registry$construction_id %in% construction_ids
  out <- registry[keep, , drop = FALSE]
  expected <- as.vector(outer(
    iv_candidate_design_adjustments(), construction_ids, paste, sep = "__"
  ))
  if (!setequal(out$specification_id, expected) || anyDuplicated(out$specification_id)) {
    stop("Could not recover the candidate scalar-IV post-treatment mechanism designs.", call. = FALSE)
  }
  out
}

posttreatment_mechanism_design_variables <- function(specifications) {
  specifications <- as_iv_specifications(specifications)
  unique(c(
    "state_code_2001", "district_code_2001", "region",
    plain_chr(specifications$treatment),
    unlist(specifications$controls, use.names = FALSE),
    unlist(specifications$included_language_controls, use.names = FALSE),
    unlist(specifications$excluded_instruments, use.names = FALSE)
  ))
}

validate_posttreatment_mechanism_registry <- function(registry, sources, label) {
  registry <- safe_df(registry)
  required <- c(
    "outcome_id", "source_id", "variable", "mechanism_family", "tier", "denominator"
  )
  missing <- setdiff(required, names(registry))
  if (length(missing) || !nrow(registry) || anyDuplicated(registry$outcome_id) ||
      anyDuplicated(registry$variable)) {
    stop(label, " mechanism registry is malformed.", call. = FALSE)
  }
  if (!all(registry$source_id %in% names(sources))) {
    stop(label, " mechanism registry references an unknown source.", call. = FALSE)
  }
  registry
}

prepare_posttreatment_mechanism_panel <- function(
    district_panel,
    sources,
    registry,
    specifications,
    label = "District") {
  sources <- lapply(sources, safe_df)
  registry <- validate_posttreatment_mechanism_registry(registry, sources, label)

  target_sets <- lapply(sources, function(x) {
    if (!"target_unit_2001" %in% names(x) || anyDuplicated(x$target_unit_2001)) {
      stop(label, " harmonized mechanism sources must be unique by target_unit_2001.", call. = FALSE)
    }
    sort(unique(plain_chr(x$target_unit_2001)))
  })
  reference_targets <- target_sets[[1L]]
  if (!all(vapply(target_sets, identical, logical(1), reference_targets))) {
    stop(label, " harmonized mechanism sources have different district support.", call. = FALSE)
  }

  measures <- data.frame(target_unit_2001 = reference_targets, stringsAsFactors = FALSE)
  for (source_id in unique(registry$source_id)) {
    rows <- registry[registry$source_id == source_id, , drop = FALSE]
    source <- sources[[source_id]]
    missing <- setdiff(rows$variable, names(source))
    if (length(missing)) {
      stop(
        label, " mechanism source `", source_id, "` is missing variables: ",
        paste(missing, collapse = ", "), call. = FALSE
      )
    }
    measures <- merge(
      measures,
      source[c("target_unit_2001", rows$variable)],
      by = "target_unit_2001",
      all = FALSE,
      sort = FALSE
    )
  }
  measures <- measures[match(reference_targets, measures$target_unit_2001), , drop = FALSE]
  codes <- lineage_target_codes(measures$target_unit_2001)
  measures$state_code_2001 <- normalize_census_code(codes$state_code_2001, 2L)
  measures$district_code_2001 <- normalize_census_code(codes$district_code_2001, 2L)

  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  keys <- c("state_code_2001", "district_code_2001")
  panel$state_code_2001 <- normalize_census_code(panel$state_code_2001, 2L)
  panel$district_code_2001 <- normalize_census_code(panel$district_code_2001, 2L)
  if (anyDuplicated(panel[keys])) {
    stop("District panel is not unique by Census-2001 district for ", label, " mechanisms.", call. = FALSE)
  }

  support <- measures[c("target_unit_2001", keys)]
  panel_key <- paste(panel$state_code_2001, panel$district_code_2001, sep = "/")
  support_key <- paste(support$state_code_2001, support$district_code_2001, sep = "/")
  support$in_iv_panel <- support_key %in% panel_key

  joined <- merge(
    panel,
    measures[c(keys, registry$variable)],
    by = keys,
    all = FALSE,
    sort = FALSE
  )
  if (!nrow(joined)) {
    stop(label, " mechanisms have no overlap with the IV panel.", call. = FALSE)
  }

  design_variables <- posttreatment_mechanism_design_variables(specifications)
  required <- unique(c(design_variables, registry$variable))
  missing <- setdiff(required, names(joined))
  if (length(missing)) {
    stop(
      label, " mechanism panel is missing registered model columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  numeric_variables <- setdiff(
    design_variables,
    c("state_code_2001", "district_code_2001", "region")
  )
  for (variable in numeric_variables) joined[[variable]] <- num(joined[[variable]])
  joined$region <- as.character(joined$region)
  design_complete <- stats::complete.cases(joined[design_variables]) &
    nzchar(joined$state_code_2001) & nzchar(joined$district_code_2001) & nzchar(joined$region)
  projected <- joined[design_complete, , drop = FALSE]
  if (!nrow(projected)) {
    stop(label, " mechanisms have no complete registered IV-design support.", call. = FALSE)
  }
  if (length(unique(projected$region)) != length(panel_region_levels())) {
    stop(label, " IV-design support does not contain all six panel regions.", call. = FALSE)
  }
  projected_key <- paste(projected$state_code_2001, projected$district_code_2001, sep = "/")
  support$in_iv_design_support <- support_key %in% projected_key

  outcome_complete <- stats::complete.cases(projected[registry$variable])
  out <- projected[outcome_complete, , drop = FALSE]
  if (!nrow(out)) {
    stop(label, " mechanisms have no common complete outcome sample.", call. = FALSE)
  }
  analysis_key <- paste(out$state_code_2001, out$district_code_2001, sep = "/")
  support$all_mechanism_outcomes_complete <- support$in_iv_design_support & support_key %in% analysis_key
  support$in_common_analysis <- support_key %in% analysis_key
  support$exclusion_reason <- ifelse(
    !support$in_iv_panel,
    "not_in_iv_panel",
    ifelse(
      !support$in_iv_design_support,
      "incomplete_iv_design_support",
      ifelse(!support$all_mechanism_outcomes_complete, "incomplete_mechanism_outcomes", "included")
    )
  )

  attr(out, "n_harmonized_mechanism_districts") <- nrow(measures)
  attr(out, "n_iv_panel_overlap_districts") <- sum(support$in_iv_panel)
  attr(out, "mechanism_sample_support") <- support
  rownames(out) <- NULL
  out
}

add_posttreatment_mechanism_holm <- function(results, p_column, output_column, label = "District") {
  out <- safe_df(results)
  if (!all(c("specification_id", "status", p_column) %in% names(out))) {
    stop(label, " Holm adjustment lacks required result columns.", call. = FALSE)
  }
  out[[output_column]] <- NA_real_
  groups <- split(seq_len(nrow(out)), out$specification_id)
  for (index in groups) {
    usable <- index[out$status[index] == "estimated" & is.finite(num(out[[p_column]][index]))]
    if (length(usable)) {
      out[[output_column]][usable] <- stats::p.adjust(num(out[[p_column]][usable]), method = "holm")
    }
  }
  out
}

estimate_posttreatment_mechanism_models <- function(
    mechanism_panel,
    registry,
    specifications,
    cfg = list(),
    ar_points = 401L,
    label = "District") {
  panel <- safe_df(mechanism_panel)
  registry <- safe_df(registry)
  base_specs <- as_iv_specifications(specifications)
  sample_n <- nrow(panel)
  harmonized_n <- attr(mechanism_panel, "n_harmonized_mechanism_districts", exact = TRUE)
  if (!is.finite(harmonized_n)) harmonized_n <- sample_n
  overlap_n <- attr(mechanism_panel, "n_iv_panel_overlap_districts", exact = TRUE)
  if (!is.finite(overlap_n)) overlap_n <- sample_n
  sample_support <- attr(mechanism_panel, "mechanism_sample_support", exact = TRUE)
  if (is.null(sample_support)) sample_support <- data.frame()

  first_stage <- safe_bind_rows(lapply(seq_len(nrow(base_specs)), function(i) {
    spec <- base_specs[i, , drop = FALSE]
    estimate_alternative_distance_spec(
      panel, spec, treatment = spec$treatment[[1L]]
    )$summary
  }))
  if (any(num(first_stage$n) != sample_n)) {
    stop(label, " mechanism first stages did not use the registered common sample.", call. = FALSE)
  }

  reduced_form <- safe_bind_rows(lapply(seq_len(nrow(registry)), function(j) {
    outcome <- registry[j, , drop = FALSE]
    safe_bind_rows(lapply(seq_len(nrow(base_specs)), function(i) {
      spec <- base_specs[i, , drop = FALSE]
      spec$outcome <- outcome$variable[[1L]]
      estimate <- estimate_iv_reduced_form_spec(panel, spec, cfg)
      estimate$outcome_id <- outcome$outcome_id[[1L]]
      estimate$outcome_variable <- outcome$variable[[1L]]
      estimate$mechanism_family <- outcome$mechanism_family[[1L]]
      estimate$tier <- outcome$tier[[1L]]
      estimate$denominator <- outcome$denominator[[1L]]
      estimate$adjustment_id <- spec$adjustment_id[[1L]]
      estimate$construction_id <- spec$construction_id[[1L]]
      estimate$fixed_effect <- spec$fixed_effect[[1L]]
      estimate
    }))
  }))
  if (any(num(reduced_form$n) != sample_n)) {
    stop(label, " reduced forms did not use one common mechanism sample.", call. = FALSE)
  }
  reduced_form <- add_posttreatment_mechanism_holm(
    reduced_form, "p.value", "p_holm_within_spec", label
  )
  reduced_form <- reduced_form[c(
    "outcome_id", "outcome_variable", "mechanism_family", "tier", "denominator",
    "specification_id", "adjustment_id", "construction_id", "fixed_effect",
    "term", "estimate", "std.error", "statistic", "p.value",
    "p_holm_within_spec", "n", "status", "reason"
  )]

  weak_estimates <- list()
  weak_grids <- list()
  k <- 0L
  for (j in seq_len(nrow(registry))) {
    outcome <- registry[j, , drop = FALSE]
    for (i in seq_len(nrow(base_specs))) {
      spec <- base_specs[i, , drop = FALSE]
      spec$outcome <- outcome$variable[[1L]]
      result <- estimate_weak_iv_specification(
        panel, spec, cfg = cfg, ar_points = ar_points
      )
      if (is.null(result)) next
      k <- k + 1L
      summary <- result$summary
      summary$outcome_id <- outcome$outcome_id[[1L]]
      summary$outcome_variable <- outcome$variable[[1L]]
      summary$mechanism_family <- outcome$mechanism_family[[1L]]
      summary$tier <- outcome$tier[[1L]]
      summary$denominator <- outcome$denominator[[1L]]
      summary$fixed_effect <- spec$fixed_effect[[1L]]
      weak_estimates[[k]] <- summary

      grid <- result$grid
      if (nrow(grid)) {
        grid$outcome_id <- outcome$outcome_id[[1L]]
        grid$outcome_variable <- outcome$variable[[1L]]
        grid$adjustment_id <- spec$adjustment_id[[1L]]
        grid$construction_id <- spec$construction_id[[1L]]
        grid$fixed_effect <- spec$fixed_effect[[1L]]
        weak_grids[[k]] <- grid
      }
    }
  }
  weak_iv <- safe_bind_rows(weak_estimates)
  if (nrow(weak_iv) != nrow(registry) * nrow(base_specs) ||
      any(num(weak_iv$n) != sample_n)) {
    stop(label, " weak-IV models did not use one complete registered model grid.", call. = FALSE)
  }
  weak_iv <- add_posttreatment_mechanism_holm(
    weak_iv, "p_value_clustered", "p_value_clustered_holm_within_spec", label
  )
  weak_iv <- add_posttreatment_mechanism_holm(
    weak_iv, "anderson_rubin_p_beta0", "anderson_rubin_p_beta0_holm_within_spec", label
  )
  weak_iv <- weak_iv[c(
    "outcome_id", "outcome_variable", "mechanism_family", "tier", "denominator",
    "specification_id", "adjustment_id", "construction_id", "fixed_effect",
    "estimate_2sls", "std_error_clustered", "p_value_clustered",
    "p_value_clustered_holm_within_spec",
    "effective_f", "effective_f_critical_value", "effective_f_p_value", "effective_f_df",
    "reduced_form_joint_f", "reduced_form_joint_p",
    "anderson_rubin_f_beta0", "anderson_rubin_p_beta0",
    "anderson_rubin_p_beta0_holm_within_spec",
    "ar_95_lower", "ar_95_upper", "ar_95_empty", "ar_95_n_components",
    "ar_95_disconnected", "ar_95_contains_zero", "ar_95_grid_accepted_min",
    "ar_95_grid_accepted_max", "ar_95_left_truncated", "ar_95_right_truncated",
    "ar_95_components", "n", "status", "reason"
  )]

  list(
    registry = registry,
    sample_coverage = data.frame(
      n_harmonized_mechanism_districts = as.integer(harmonized_n),
      n_iv_panel_overlap_districts = as.integer(overlap_n),
      n_iv_design_support_districts = if (nrow(sample_support)) {
        sum(sample_support$in_iv_design_support)
      } else {
        sample_n
      },
      n_common_analysis_districts = sample_n,
      n_states = length(unique(panel$state_code_2001)),
      n_regions = length(unique(panel$region)),
      stringsAsFactors = FALSE
    ),
    sample_support = safe_df(sample_support),
    first_stage = first_stage,
    reduced_form = reduced_form,
    weak_iv = weak_iv,
    anderson_rubin_grid = safe_bind_rows(weak_grids)
  )
}

posttreatment_mechanism_persisted_components <- function() {
  c(
    "registry", "sample_coverage", "sample_support", "first_stage",
    "reduced_form", "weak_iv"
  )
}

extract_posttreatment_mechanism_result <- function(x) {
  components <- posttreatment_mechanism_persisted_components()
  direct <- all(components %in% names(x))
  prefixed <- all(paste0("mechanism_", components) %in% names(x))
  if (!direct && !prefixed) {
    stop("Post-treatment mechanism object is missing the shared inference contract.", call. = FALSE)
  }
  if (direct) {
    out <- x[components]
    out$anderson_rubin_grid <- safe_df(x$anderson_rubin_grid %||% data.frame())
  } else {
    out <- stats::setNames(x[paste0("mechanism_", components)], components)
    out$anderson_rubin_grid <- safe_df(x$mechanism_anderson_rubin_grid %||% data.frame())
  }
  out
}

save_posttreatment_mechanism_outputs <- function(
    x, directory, prefix = "mechanism_") {
  result <- extract_posttreatment_mechanism_result(x)
  components <- posttreatment_mechanism_persisted_components()
  objects <- result[components]
  filenames <- stats::setNames(
    paste0(prefix, components, ".csv"),
    components
  )
  stale_grid <- file.path(directory, paste0(prefix, "anderson_rubin_grid.csv"))
  write_diagnostic_bundle(objects, directory, filenames, stale = stale_grid)
}

summarize_posttreatment_mechanism_result <- function(
    x, family, temporal_role, analysis_role = "causal_mechanism") {
  result <- extract_posttreatment_mechanism_result(x)
  weak <- safe_df(result$weak_iv)
  reduced <- safe_df(result$reduced_form)
  keys <- c("outcome_id", "specification_id")
  if (!nrow(weak) || anyDuplicated(weak[keys])) {
    stop(family, " weak-IV mechanism rows must be unique by outcome and specification.", call. = FALSE)
  }
  if (!nrow(reduced) || anyDuplicated(reduced[keys])) {
    stop(family, " reduced-form mechanism rows must be unique by outcome and specification.", call. = FALSE)
  }
  rf <- reduced[c(keys, "p.value", "p_holm_within_spec")]
  names(rf)[names(rf) == "p.value"] <- "reduced_form_p_value"
  names(rf)[names(rf) == "p_holm_within_spec"] <- "reduced_form_p_holm"
  out <- merge(weak, rf, by = keys, all.x = TRUE, sort = FALSE)
  out <- out[match(
    paste(weak$outcome_id, weak$specification_id),
    paste(out$outcome_id, out$specification_id)
  ), , drop = FALSE]

  estimated <- plain_chr(out$status) == "estimated"
  out$first_stage_strong <- estimated &
    is.finite(num(out$effective_f)) &
    is.finite(num(out$effective_f_critical_value)) &
    num(out$effective_f) >= num(out$effective_f_critical_value)
  out$reduced_form_holm_signal <- estimated &
    is.finite(num(out$reduced_form_p_holm)) & num(out$reduced_form_p_holm) < 0.05
  out$ar_holm_rejects_zero <- estimated &
    is.finite(num(out$anderson_rubin_p_beta0_holm_within_spec)) &
    num(out$anderson_rubin_p_beta0_holm_within_spec) < 0.05
  out$ar_95_bounded <- estimated &
    !(out$ar_95_empty %in% TRUE) &
    !(out$ar_95_left_truncated %in% TRUE) &
    !(out$ar_95_right_truncated %in% TRUE)
  out$evidence_status <- ifelse(
    !estimated,
    "inference_unavailable",
    ifelse(
      out$ar_holm_rejects_zero,
      "weak_iv_robust_signal",
      ifelse(
        !out$first_stage_strong,
        "weak_iv_underidentified",
        "identified_no_weak_iv_robust_signal"
      )
    )
  )
  out$family <- family
  out$temporal_role <- temporal_role
  out$analysis_role <- analysis_role
  keep <- c(
    "family", "temporal_role", "analysis_role", "outcome_id", "outcome_variable",
    "mechanism_family", "tier", "denominator", "specification_id", "adjustment_id",
    "construction_id", "fixed_effect", "n", "effective_f", "effective_f_critical_value",
    "first_stage_strong", "reduced_form_p_value", "reduced_form_p_holm",
    "reduced_form_holm_signal", "anderson_rubin_p_beta0",
    "anderson_rubin_p_beta0_holm_within_spec", "ar_holm_rejects_zero",
    "ar_95_contains_zero", "ar_95_bounded", "ar_95_n_components",
    "ar_95_disconnected", "evidence_status"
  )
  out[keep]
}

build_posttreatment_mechanism_evidence <- function(families) {
  if (!is.list(families) || is.null(names(families)) || any(!nzchar(names(families)))) {
    stop("Mechanism evidence families must be a named list.", call. = FALSE)
  }
  grids <- lapply(names(families), function(family) {
    entry <- families[[family]]
    required <- c("result", "temporal_role")
    if (!is.list(entry) || !all(required %in% names(entry))) {
      stop("Mechanism evidence family `", family, "` is missing result or temporal_role.", call. = FALSE)
    }
    if (is.null(entry$result)) return(data.frame())
    summarize_posttreatment_mechanism_result(
      entry$result,
      family = family,
      temporal_role = entry$temporal_role,
      analysis_role = entry$analysis_role %||% "causal_mechanism"
    )
  })
  grid <- safe_bind_rows(grids)

  summary <- safe_bind_rows(lapply(names(families), function(family) {
    entry <- families[[family]]
    temporal_role <- entry$temporal_role
    analysis_role <- entry$analysis_role %||% "causal_mechanism"
    x <- if (nrow(grid)) {
      grid[
        grid$family == family & grid$temporal_role == temporal_role &
          grid$analysis_role == analysis_role,
        , drop = FALSE
      ]
    } else {
      data.frame()
    }
    if (!nrow(x)) {
      return(data.frame(
        family = family, temporal_role = temporal_role, analysis_role = analysis_role,
        availability_status = "not_available", n_outcomes = 0L, n_models = 0L,
        n_strong_first_stage = 0L, n_reduced_form_holm_signals = 0L,
        n_ar_holm_signals = 0L, n_bounded_ar_sets = 0L, n_underidentified = 0L,
        min_n = NA_real_, max_n = NA_real_, stringsAsFactors = FALSE
      ))
    }
    data.frame(
      family = family,
      temporal_role = temporal_role,
      analysis_role = analysis_role,
      availability_status = "available",
      n_outcomes = length(unique(x$outcome_id)),
      n_models = nrow(x),
      n_strong_first_stage = sum(x$first_stage_strong %in% TRUE),
      n_reduced_form_holm_signals = sum(x$reduced_form_holm_signal %in% TRUE),
      n_ar_holm_signals = sum(x$ar_holm_rejects_zero %in% TRUE),
      n_bounded_ar_sets = sum(x$ar_95_bounded %in% TRUE),
      n_underidentified = sum(x$evidence_status == "weak_iv_underidentified"),
      min_n = min(num(x$n), na.rm = TRUE),
      max_n = max(num(x$n), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  list(grid = grid, family_summary = summary)
}

save_posttreatment_mechanism_evidence <- function(
    x, directory = "outputs/diagnostics/extended/mechanisms") {
  write_diagnostic_bundle(
    list(
      evidence_grid = safe_df(x$grid),
      family_summary = safe_df(x$family_summary)
    ),
    directory
  )
}
