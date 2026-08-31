# Shared post-treatment Census mechanism inference on Census-2001 geography.

census_mechanism_specifications <- function(
    outcome,
    treatment = preferred_iv_variables()$treatment,
    sample_rule = "census_mechanism_common_support",
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
    stop("Could not recover the candidate scalar-IV Census mechanism designs.", call. = FALSE)
  }
  out
}

census_mechanism_design_variables <- function(specifications) {
  specifications <- as_iv_specifications(specifications)
  unique(c(
    "state_code_2001", "district_code_2001", "region",
    plain_chr(specifications$treatment),
    unlist(specifications$controls, use.names = FALSE),
    unlist(specifications$included_language_controls, use.names = FALSE),
    unlist(specifications$excluded_instruments, use.names = FALSE)
  ))
}

validate_census_mechanism_registry <- function(registry, sources, label) {
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

prepare_census_mechanism_panel <- function(
    district_panel,
    sources,
    registry,
    specifications,
    label = "Census") {
  sources <- lapply(sources, safe_df)
  registry <- validate_census_mechanism_registry(registry, sources, label)

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

  design_variables <- census_mechanism_design_variables(specifications)
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

add_census_mechanism_holm <- function(results, p_column, output_column, label = "Census") {
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

estimate_census_mechanism_models <- function(
    mechanism_panel,
    registry,
    specifications,
    cfg = list(),
    ar_points = 401L,
    label = "Census") {
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
  reduced_form <- add_census_mechanism_holm(
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
  weak_iv <- add_census_mechanism_holm(
    weak_iv, "p_value_clustered", "p_value_clustered_holm_within_spec", label
  )
  weak_iv <- add_census_mechanism_holm(
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
