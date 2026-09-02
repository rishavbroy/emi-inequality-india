# Registered consumption-welfare outcomes for the canonical IV architecture.
# This module prepares outcome data and self-describing IV specifications; it
# does not choose between reduced-form and 2SLS interpretation.

read_consumption_iv_outcome_registry <- function(path) {
  if (!file.exists(path)) {
    stop("Consumption IV outcome registry is missing: ", path, call. = FALSE)
  }
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "welfare_specification_id", "outcome_id", "outcome_round",
    "baseline_round", "estimand", "analysis_transform", "treatment",
    "instrument", "adjustment_id", "construction_id", "panel_variant",
    "sample_rule", "tier"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption IV outcome registry is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  chr <- required
  for (nm in chr) x[[nm]] <- trimws(plain_chr(x[[nm]]))
  if (!nrow(x) || anyDuplicated(x$welfare_specification_id) ||
      any(vapply(x[required], function(v) any(is.na(v) | !nzchar(v)), logical(1)))) {
    stop("Consumption IV outcome registry contains empty or duplicate identifiers.", call. = FALSE)
  }
  if (any(!x$estimand %in% c("ancova", "change", "level"))) {
    stop("Consumption IV outcome registry contains an unknown estimand.", call. = FALSE)
  }
  if (any(!x$analysis_transform %in% c("identity", "log"))) {
    stop("Consumption IV outcome registry contains an unknown analysis transform.", call. = FALSE)
  }
  supported_sample_rules <- c(
    "analysis_welfare_support", "preferred_welfare_support"
  )
  if (any(!x$sample_rule %in% supported_sample_rules)) {
    stop("Consumption IV outcome registry contains an unknown sample rule.", call. = FALSE)
  }
  if (any(toupper(x$tier) == "A" & x$sample_rule != "analysis_welfare_support")) {
    stop(
      "Tier-A consumption IV specifications must use ex-ante analysis_welfare_support.",
      call. = FALSE
    )
  }
  needs_baseline <- x$estimand %in% c("ancova", "change")
  if (any(needs_baseline & x$baseline_round == x$outcome_round)) {
    stop("Consumption IV outcome baseline and endpoint rounds must differ.", call. = FALSE)
  }
  x
}

consumption_iv_variable_name <- function(specification_id, role = c("outcome", "baseline")) {
  role <- match.arg(role)
  paste0(
    "welfare_iv__", gsub("[^A-Za-z0-9_]+", "_", plain_chr(specification_id)),
    "__", role
  )
}

transform_consumption_iv_value <- function(value, transform) {
  x <- num(value)
  switch(
    plain_chr(transform),
    identity = x,
    log = {
      out <- rep(NA_real_, length(x))
      keep <- positive_finite(x)
      out[keep] <- log(x[keep])
      out
    },
    stop("Unknown consumption IV analysis transform: ", transform, call. = FALSE)
  )
}

consumption_iv_support_column <- function(sample_rule) {
  switch(
    plain_chr(sample_rule),
    analysis_welfare_support = "analysis_eligible",
    preferred_welfare_support = "preferred_eligible",
    stop("Unknown consumption IV welfare sample rule: ", sample_rule, call. = FALSE)
  )
}

consumption_iv_round_rows <- function(welfare, outcome_id, round_id, sample_rule) {
  x <- safe_df(welfare)
  support_column <- consumption_iv_support_column(sample_rule)
  required <- c(
    "district_2001", "round_id", "outcome_id", "estimate", support_column
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption welfare input lacks IV outcome fields: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  out <- x[
    plain_chr(x$outcome_id) == outcome_id &
      plain_chr(x$round_id) == round_id,
    required,
    drop = FALSE
  ]
  if (!nrow(out)) {
    stop(
      "Consumption IV outcome registry references unavailable welfare rows: ",
      outcome_id, " / ", round_id,
      call. = FALSE
    )
  }
  if (anyDuplicated(out$district_2001)) {
    stop(
      "Consumption welfare input is not unique by district/outcome/round.",
      call. = FALSE
    )
  }
  out$iv_support <- out[[support_column]] %in% TRUE
  out
}

build_consumption_iv_specification_data <- function(welfare, specification) {
  spec <- safe_df(specification)
  if (nrow(spec) != 1L) {
    stop("A single consumption IV outcome specification is required.", call. = FALSE)
  }

  outcome_id <- spec$outcome_id[[1L]]
  endpoint_id <- spec$outcome_round[[1L]]
  baseline_id <- spec$baseline_round[[1L]]
  estimand <- spec$estimand[[1L]]
  transform <- spec$analysis_transform[[1L]]
  sample_rule <- spec$sample_rule[[1L]]

  endpoint <- consumption_iv_round_rows(
    welfare, outcome_id, endpoint_id, sample_rule
  )
  endpoint_value <- transform_consumption_iv_value(endpoint$estimate, transform)
  endpoint_ok <- endpoint$iv_support & is.finite(endpoint_value)

  if (estimand == "level") {
    return(data.frame(
      target_unit_2001 = plain_chr(endpoint$district_2001),
      outcome_value = ifelse(endpoint_ok, endpoint_value, NA_real_),
      baseline_value = NA_real_,
      welfare_support = endpoint_ok,
      stringsAsFactors = FALSE
    ))
  }

  baseline <- consumption_iv_round_rows(
    welfare, outcome_id, baseline_id, sample_rule
  )
  joined <- merge(
    endpoint[c("district_2001", "estimate", "iv_support")],
    baseline[c("district_2001", "estimate", "iv_support")],
    by = "district_2001", all = FALSE, sort = FALSE,
    suffixes = c("_endpoint", "_baseline")
  )
  endpoint_value <- transform_consumption_iv_value(
    joined$estimate_endpoint, transform
  )
  baseline_value <- transform_consumption_iv_value(
    joined$estimate_baseline, transform
  )
  common_ok <- joined$iv_support_endpoint %in% TRUE &
    joined$iv_support_baseline %in% TRUE &
    is.finite(endpoint_value) & is.finite(baseline_value)

  outcome_value <- if (estimand == "ancova") {
    endpoint_value
  } else if (estimand == "change") {
    endpoint_value - baseline_value
  } else {
    stop("Unknown consumption IV estimand: ", estimand, call. = FALSE)
  }

  data.frame(
    target_unit_2001 = plain_chr(joined$district_2001),
    outcome_value = ifelse(common_ok, outcome_value, NA_real_),
    baseline_value = ifelse(common_ok, baseline_value, NA_real_),
    welfare_support = common_ok,
    stringsAsFactors = FALSE
  )
}

attach_consumption_iv_outcomes <- function(panel, welfare, registry) {
  out <- panel
  if (!"target_unit_2001" %in% names(out)) {
    stop("Analysis panel lacks target_unit_2001 for consumption IV outcomes.", call. = FALSE)
  }
  specs <- safe_df(registry)
  if (!nrow(specs)) {
    stop("Consumption IV outcome registry is empty.", call. = FALSE)
  }

  panel_key <- plain_chr(out$target_unit_2001)
  if (anyDuplicated(panel_key)) {
    stop("Analysis panel target_unit_2001 must be unique.", call. = FALSE)
  }

  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, , drop = FALSE]
    data <- build_consumption_iv_specification_data(welfare, spec)
    pos <- match(panel_key, data$target_unit_2001)
    outcome_name <- consumption_iv_variable_name(
      spec$welfare_specification_id[[1L]], "outcome"
    )
    baseline_name <- consumption_iv_variable_name(
      spec$welfare_specification_id[[1L]], "baseline"
    )
    if (outcome_name %in% names(out) || baseline_name %in% names(out)) {
      stop("Consumption IV outcome columns would overwrite analysis-panel fields.", call. = FALSE)
    }
    out[[outcome_name]] <- data$outcome_value[pos]
    if (identical(spec$estimand[[1L]], "ancova")) {
      out[[baseline_name]] <- data$baseline_value[pos]
    }
  }
  out
}

compile_consumption_iv_design_row <- function(
    specification,
    adjustment_id,
    construction_id,
    specification_id,
    control_registry = NULL,
    tier = NULL,
    sample_rule = NULL,
    require_registered_instrument = FALSE,
    sequence = 1L) {
  x <- safe_df(specification)
  if (nrow(x) != 1L) stop("A single consumption IV outcome specification is required.", call. = FALSE)
  control_registry <- resolve_census_2001_control_registry(control_registry)
  adjustments <- iv_adjustment_sets(control_registry)
  constructions <- iv_instrument_constructions()
  if (!adjustment_id %in% names(adjustments)) {
    stop("Unknown consumption IV adjustment_id: ", adjustment_id, call. = FALSE)
  }
  if (!construction_id %in% names(constructions)) {
    stop("Unknown consumption IV construction_id: ", construction_id, call. = FALSE)
  }
  adjustment <- adjustments[[adjustment_id]]
  construction <- constructions[[construction_id]]
  excluded <- plain_chr(construction$excluded)
  if (length(excluded) != 1L) {
    stop("Consumption IV outcome designs require one excluded scalar instrument.", call. = FALSE)
  }
  if (isTRUE(require_registered_instrument) && !identical(excluded[[1L]], x$instrument[[1L]])) {
    stop("Consumption IV registry instrument does not match its construction.", call. = FALSE)
  }

  outcome_name <- consumption_iv_variable_name(
    x$welfare_specification_id[[1L]], "outcome"
  )
  controls <- adjustment$controls
  if (identical(x$estimand[[1L]], "ancova")) {
    controls <- c(
      controls,
      consumption_iv_variable_name(
        x$welfare_specification_id[[1L]], "baseline"
      )
    )
  }

  row <- iv_specification_row(
    specification_id = specification_id,
    adjustment_id = adjustment_id,
    adjustment = adjustment$label,
    construction_id = construction_id,
    construction = construction$label,
    outcome = outcome_name,
    treatment = x$treatment[[1L]],
    fixed_effect = adjustment$fixed_effect,
    controls = controls,
    included_language_controls = construction$included,
    excluded_instruments = construction$excluded,
    mapping_coverage_variable = construction$coverage,
    panel_variant = x$panel_variant[[1L]],
    sample_rule = sample_rule %||% x$sample_rule[[1L]],
    tier = tier %||% x$tier[[1L]],
    sequence = sequence,
    control_registry = control_registry
  )
  row$welfare_specification_id <- x$welfare_specification_id[[1L]]
  row$welfare_outcome_id <- x$outcome_id[[1L]]
  row$outcome_round <- x$outcome_round[[1L]]
  row$baseline_round <- x$baseline_round[[1L]]
  row$estimand <- x$estimand[[1L]]
  row$analysis_transform <- x$analysis_transform[[1L]]
  row
}

compile_consumption_iv_specifications <- function(registry, control_registry = NULL) {
  specs <- safe_df(registry)
  rows <- lapply(seq_len(nrow(specs)), function(i) {
    x <- specs[i, , drop = FALSE]
    compile_consumption_iv_design_row(
      x,
      adjustment_id = x$adjustment_id[[1L]],
      construction_id = x$construction_id[[1L]],
      specification_id = paste0("consumption__", x$welfare_specification_id[[1L]]),
      control_registry = control_registry,
      require_registered_instrument = TRUE,
      sequence = i
    )
  })
  bind_iv_specification_rows(rows)
}

consumption_welfare_round_is_supported <- function(outcome_row, survey_id) {
  surveys <- trimws(plain_chr(outcome_row$survey_ids[[1L]] %||% "*"))
  identical(surveys, "*") || survey_id %in% strsplit(surveys, ";", fixed = TRUE)[[1L]]
}

build_consumption_alternative_welfare_registry <- function(
    consumption_registry,
    welfare_registry) {
  consumption <- safe_df(consumption_registry)
  welfare <- safe_df(welfare_registry)
  required_consumption <- c(
    "welfare_specification_id", "outcome_round", "baseline_round", "estimand",
    "treatment", "instrument", "adjustment_id", "construction_id",
    "panel_variant", "sample_rule"
  )
  required_welfare <- c(
    "outcome_id", "role", "survey_ids", "iv_analysis_transform"
  )
  missing <- c(
    setdiff(required_consumption, names(consumption)),
    setdiff(required_welfare, names(welfare))
  )
  if (length(missing)) {
    stop(
      "Alternative consumption-welfare registry lacks fields: ",
      paste(unique(missing), collapse = ", "),
      call. = FALSE
    )
  }
  welfare <- welfare[welfare$role == "robustness", , drop = FALSE]
  rows <- list()
  k <- 0L
  for (i in seq_len(nrow(welfare))) {
    outcome <- welfare[i, , drop = FALSE]
    for (j in seq_len(nrow(consumption))) {
      base <- consumption[j, , drop = FALSE]
      supported <- consumption_welfare_round_is_supported(outcome, base$outcome_round[[1L]]) &&
        consumption_welfare_round_is_supported(outcome, base$baseline_round[[1L]])
      if (!supported) next
      k <- k + 1L
      rows[[k]] <- data.frame(
        welfare_specification_id = paste(
          "alt_welfare", outcome$outcome_id[[1L]], base$welfare_specification_id[[1L]], sep = "__"
        ),
        outcome_id = outcome$outcome_id[[1L]],
        outcome_round = base$outcome_round[[1L]],
        baseline_round = base$baseline_round[[1L]],
        estimand = base$estimand[[1L]],
        analysis_transform = outcome$iv_analysis_transform[[1L]],
        treatment = base$treatment[[1L]],
        instrument = base$instrument[[1L]],
        adjustment_id = base$adjustment_id[[1L]],
        construction_id = base$construction_id[[1L]],
        panel_variant = base$panel_variant[[1L]],
        sample_rule = base$sample_rule[[1L]],
        tier = "C",
        stringsAsFactors = FALSE
      )
    }
  }
  out <- safe_bind_rows(rows)
  if (!nrow(out) || anyDuplicated(out$welfare_specification_id)) {
    stop("Alternative consumption-welfare registry is empty or duplicated.", call. = FALSE)
  }
  out
}

compile_consumption_scalar_iv_family_specifications <- function(
    registry,
    family_prefix,
    sample_rule,
    tier,
    control_registry = NULL) {
  specs <- safe_df(registry)
  adjustments <- iv_candidate_design_adjustments()
  constructions <- unname(iv_candidate_design_constructions())
  rows <- list()
  k <- 0L
  for (i in seq_len(nrow(specs))) {
    x <- specs[i, , drop = FALSE]
    for (adjustment_id in adjustments) {
      for (construction_id in constructions) {
        k <- k + 1L
        rows[[k]] <- compile_consumption_iv_design_row(
          x,
          adjustment_id = adjustment_id,
          construction_id = construction_id,
          specification_id = paste(
            family_prefix,
            x$welfare_specification_id[[1L]],
            adjustment_id,
            construction_id,
            sep = "__"
          ),
          control_registry = control_registry,
          tier = tier,
          sample_rule = sample_rule,
          require_registered_instrument = FALSE,
          sequence = k
        )
      }
    }
  }
  out <- bind_iv_specification_rows(rows)
  expected <- nrow(specs) * length(adjustments) * length(constructions)
  if (nrow(out) != expected || anyDuplicated(out$specification_id)) {
    stop("Consumption scalar-IV family registry is incomplete or duplicated.", call. = FALSE)
  }
  out
}

compile_consumption_scalar_iv_robustness_specifications <- function(
    registry, control_registry = NULL) {
  compile_consumption_scalar_iv_family_specifications(
    registry,
    family_prefix = "consumption_scalar",
    sample_rule = "consumption_scalar_iv_common_support",
    tier = "B",
    control_registry = control_registry
  )
}

compile_consumption_alternative_welfare_specifications <- function(
    registry, control_registry = NULL) {
  compile_consumption_scalar_iv_family_specifications(
    registry,
    family_prefix = "consumption_welfare",
    sample_rule = "consumption_welfare_iv_common_support",
    tier = "C",
    control_registry = control_registry
  )
}

consumption_iv_common_sample_support <- function(panel, specifications, group_column) {
  specs <- as_iv_specifications(specifications)
  if (!group_column %in% names(specs)) {
    stop("Consumption IV common-sample grouping column is missing: ", group_column, call. = FALSE)
  }
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  groups <- split(seq_len(nrow(specs)), plain_chr(specs[[group_column]]))
  safe_bind_rows(lapply(names(groups), function(group_id) {
    group_specs <- specs[groups[[group_id]], , drop = FALSE]
    needed <- unique(unlist(lapply(seq_len(nrow(group_specs)), function(i) {
      iv_specification_variables(group_specs[i, , drop = FALSE], include_outcome = TRUE)
    }), use.names = FALSE))
    missing <- setdiff(needed, names(x))
    complete <- if (length(missing)) rep(FALSE, nrow(x)) else stats::complete.cases(x[needed])
    data.frame(
      group_id = group_id,
      group_column = group_column,
      n_panel = nrow(x),
      n_common = sum(complete),
      common_share = if (nrow(x)) mean(complete) else NA_real_,
      status = if (length(missing)) "missing_columns" else if (sum(complete) < 3L) {
        "insufficient_common_cases"
      } else {
        "ready"
      },
      missing_columns = if (length(missing)) paste(missing, collapse = ";") else NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

restrict_consumption_iv_to_common_samples <- function(panel, specifications, group_column) {
  specs <- as_iv_specifications(specifications)
  support <- consumption_iv_common_sample_support(panel, specs, group_column)
  bad <- support$status != "ready"
  if (any(bad)) {
    stop(
      "Consumption IV robustness common samples are not analysis-ready: ",
      paste(paste0(support$group_id[bad], "=", support$status[bad]), collapse = "; "),
      call. = FALSE
    )
  }
  out <- panel
  base <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  groups <- split(seq_len(nrow(specs)), plain_chr(specs[[group_column]]))
  for (group_id in names(groups)) {
    group_specs <- specs[groups[[group_id]], , drop = FALSE]
    outcomes <- unique(plain_chr(group_specs$outcome))
    if (length(outcomes) != 1L) {
      stop("Consumption IV common-sample groups must share one outcome column.", call. = FALSE)
    }
    needed <- unique(unlist(lapply(seq_len(nrow(group_specs)), function(i) {
      iv_specification_variables(group_specs[i, , drop = FALSE], include_outcome = TRUE)
    }), use.names = FALSE))
    complete <- stats::complete.cases(base[needed])
    out[[outcomes[[1L]]]][!complete] <- NA_real_
  }
  attr(out, "consumption_iv_common_sample_support") <- support
  out
}

summarize_consumption_iv_outcome_coverage <- function(panel, specifications) {
  specs <- as_iv_specifications(specifications)
  needed_all <- unique(unlist(lapply(seq_len(nrow(specs)), function(i) {
    iv_specification_variables(specs[i, , drop = FALSE], include_outcome = TRUE)
  }), use.names = FALSE))
  x <- iv_analysis_frame(panel, needed_all)
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    spec <- specs[i, , drop = FALSE]
    needed <- iv_specification_variables(spec, include_outcome = TRUE)
    missing <- setdiff(needed, names(x))
    complete <- if (length(missing)) {
      rep(FALSE, nrow(x))
    } else {
      stats::complete.cases(x[needed])
    }
    data.frame(
      specification_id = spec$specification_id[[1L]],
      welfare_specification_id = spec$welfare_specification_id[[1L]],
      outcome_round = spec$outcome_round[[1L]],
      baseline_round = spec$baseline_round[[1L]],
      estimand = spec$estimand[[1L]],
      n_panel = nrow(x),
      n_analysis_complete = sum(complete),
      analysis_share = if (nrow(x)) mean(complete) else NA_real_,
      status = if (length(missing)) "missing_columns" else if (sum(complete) < 3L) {
        "insufficient_complete_cases"
      } else {
        "ready"
      },
      missing_columns = if (length(missing)) paste(missing, collapse = ";") else NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

validate_consumption_iv_outcome_coverage <- function(coverage) {
  x <- safe_df(coverage)
  required <- c(
    "specification_id", "n_analysis_complete", "analysis_share",
    "status", "missing_columns"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Consumption IV outcome coverage is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  bad <- x$status != "ready"
  if (any(bad)) {
    detail <- paste(
      paste0(
        x$specification_id[bad], "=", x$status[bad],
        ifelse(
          is.na(x$missing_columns[bad]) | !nzchar(x$missing_columns[bad]),
          "",
          paste0("[", x$missing_columns[bad], "]")
        )
      ),
      collapse = "; "
    )
    stop(
      "Registered consumption IV outcome specifications are not analysis-ready: ",
      detail,
      call. = FALSE
    )
  }
  x
}

save_consumption_iv_outcome_coverage <- function(
    coverage,
    path = "outputs/diagnostics/extended/consumption/consumption_iv_outcome_coverage.csv") {
  write_diagnostic_csv(safe_df(coverage), path)
}

consumption_iv_formula_list <- function(specifications) {
  specs <- as_iv_specifications(specifications)
  formulas <- lapply(seq_len(nrow(specs)), function(i) {
    iv_specification_formula(specs[i, , drop = FALSE])
  })
  stats::setNames(formulas, plain_chr(specs$specification_id))
}


consumption_iv_second_stage_rows <- function(models, specifications, data) {
  specs <- as_iv_specifications(specifications)
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    spec <- specs[i, , drop = FALSE]
    id <- spec$specification_id[[1L]]
    model <- models[[id]]
    if (!inherits(model, "ivreg")) {
      return(data.frame(
        specification_id = id,
        estimate = NA_real_, std.error = NA_real_,
        statistic = NA_real_, p.value = NA_real_, n = NA_integer_,
        status = model$status %||% "not_estimated",
        reason = model$reason %||% NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    vc <- attr(model, "cluster_vcov")
    if (is.null(vc)) {
      cluster <- iv_model_cluster(model, data)
      inference <- if (is.null(cluster)) NULL else iv_clustered_inference(model, cluster)
      vc <- inference$vcov %||% NULL
    }
    term <- model_term_inference(model, spec$treatment[[1L]], vc)
    data.frame(
      specification_id = id,
      estimate = term[["estimate"]],
      std.error = term[["std.error"]],
      statistic = term[["statistic"]],
      p.value = term[["p.value"]],
      n = stats::nobs(model),
      status = if (all(is.finite(term[c("estimate", "std.error", "p.value")]))) {
        "estimated"
      } else {
        "inference_unavailable"
      },
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  }))
}

consumption_iv_first_stage_rows <- function(first_stage, specifications) {
  fs <- safe_df(first_stage)
  specs <- as_iv_specifications(specifications)
  safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    spec <- specs[i, , drop = FALSE]
    id <- spec$specification_id[[1L]]
    excluded <- unlist(spec$excluded_instruments[[1L]], use.names = FALSE)
    rows <- fs[plain_chr(fs$model) == id, , drop = FALSE]
    term_row <- rows[plain_chr(rows$term) == excluded[[1L]], , drop = FALSE]
    row <- if (nrow(term_row)) term_row[1L, , drop = FALSE] else if (nrow(rows)) {
      rows[1L, , drop = FALSE]
    } else {
      data.frame()
    }
    if (!nrow(row)) {
      return(data.frame(
        specification_id = id,
        first_stage_estimate = NA_real_,
        first_stage_std.error = NA_real_,
        first_stage_p.value = NA_real_,
        partial_f = NA_real_, partial_p = NA_real_,
        effective_f = NA_real_,
        effective_f_critical_value = NA_real_,
        effective_f_p_value = NA_real_,
        effective_f_df = NA_real_,
        effective_f_status = "not_estimated",
        effective_f_reason = "First-stage result is unavailable.",
        first_stage_n = NA_integer_,
        first_stage_status = "not_estimated",
        first_stage_reason = "First-stage result is unavailable.",
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      specification_id = id,
      first_stage_estimate = num(row$estimate[[1L]]),
      first_stage_std.error = num(row$std.error[[1L]]),
      first_stage_p.value = num(row$p.value[[1L]]),
      partial_f = num(row$partial_f[[1L]]),
      partial_p = num(row$partial_p[[1L]]),
      effective_f = num(row$effective_f[[1L]]),
      effective_f_critical_value = num(row$effective_f_critical_value[[1L]]),
      effective_f_p_value = num(row$effective_f_p_value[[1L]]),
      effective_f_df = num(row$effective_f_df[[1L]]),
      effective_f_status = plain_chr(row$effective_f_status[[1L]]),
      effective_f_reason = plain_chr(row$effective_f_reason[[1L]]),
      first_stage_n = as.integer(num(row$nobs[[1L]])),
      first_stage_status = plain_chr(row$status[[1L]]),
      first_stage_reason = plain_chr(row$reason[[1L]]),
      stringsAsFactors = FALSE
    )
  }))
}

estimate_consumption_iv_dynamic_spec <- function(
    panel, specification, cfg = list(), ar_level = 0.95, ar_points = 401L) {
  spec <- as_single_iv_specification(specification)
  id <- plain_chr(spec$specification_id[[1L]])
  needed <- iv_specification_variables(spec, include_outcome = TRUE)
  analysis_panel <- iv_analysis_frame(panel, needed)

  tryCatch({
    formula <- iv_specification_formula(spec)
    models <- estimate_2sls(
      analysis_panel, stats::setNames(list(formula), id), cfg
    )
    first_stage <- estimate_first_stage(models, analysis_panel, cfg)
    first_stage_row <- consumption_iv_first_stage_rows(first_stage, spec)
    reduced_form <- estimate_iv_reduced_form_spec(analysis_panel, spec, cfg)
    second_stage <- consumption_iv_second_stage_rows(models, spec, analysis_panel)
    ar <- estimate_anderson_rubin_spec(
      analysis_panel, spec, level = ar_level, points = ar_points
    )

    summary <- spec[c(
      "specification_id", "welfare_specification_id",
      "welfare_outcome_id", "outcome_round", "baseline_round",
      "estimand", "analysis_transform", "tier"
    )]
    summary <- merge(
      summary, first_stage_row,
      by = "specification_id", all.x = TRUE, sort = FALSE
    )

    rf <- reduced_form
    names(rf)[names(rf) != "specification_id"] <- paste0(
      "reduced_form_", names(rf)[names(rf) != "specification_id"]
    )
    ss <- second_stage
    names(ss)[names(ss) != "specification_id"] <- paste0(
      "second_stage_", names(ss)[names(ss) != "specification_id"]
    )

    summary <- merge(
      summary, rf, by = "specification_id", all.x = TRUE, sort = FALSE
    )
    summary <- merge(
      summary, ss, by = "specification_id", all.x = TRUE, sort = FALSE
    )
    summary <- merge(
      summary, ar$summary,
      by = "specification_id", all.x = TRUE, sort = FALSE
    )
    rownames(summary) <- NULL

    list(summary = summary, anderson_rubin_grid = ar$grid)
  }, error = function(e) {
    stop(
      "Consumption IV dynamics failed for ", id, ": ",
      conditionMessage(e),
      call. = FALSE
    )
  })
}

estimate_consumption_iv_dynamics <- function(
    panel, specifications, cfg = list(), ar_level = 0.95, ar_points = 401L) {
  specs <- as_iv_specifications(specifications)
  estimated <- lapply(seq_len(nrow(specs)), function(i) {
    estimate_consumption_iv_dynamic_spec(
      panel,
      specs[i, , drop = FALSE],
      cfg = cfg,
      ar_level = ar_level,
      ar_points = ar_points
    )
  })

  summary <- safe_bind_rows(lapply(estimated, `[[`, "summary"))
  grid <- safe_bind_rows(lapply(estimated, `[[`, "anderson_rubin_grid"))
  expected <- plain_chr(specs$specification_id)
  summary <- summary[
    match(expected, plain_chr(summary$specification_id)),
    ,
    drop = FALSE
  ]
  rownames(summary) <- NULL

  list(summary = summary, anderson_rubin_grid = grid)
}


validate_consumption_iv_dynamics <- function(dynamics, specifications) {
  specs <- as_iv_specifications(specifications)
  if (!is.list(dynamics) ||
      !all(c("summary", "anderson_rubin_grid") %in% names(dynamics))) {
    stop("Consumption IV dynamics must contain summary and Anderson-Rubin grid outputs.", call. = FALSE)
  }

  summary <- safe_df(dynamics$summary)
  required <- c(
    "specification_id",
    "first_stage_n", "first_stage_status",
    "reduced_form_n", "reduced_form_status",
    "second_stage_n", "second_stage_status",
    "n", "status",
    "partial_f", "effective_f", "effective_f_critical_value",
    "effective_f_p_value", "effective_f_df", "effective_f_status",
    "reduced_form_estimate", "reduced_form_std.error", "reduced_form_p.value",
    "second_stage_estimate", "second_stage_std.error", "second_stage_p.value",
    "anderson_rubin_p_beta0"
  )
  missing <- setdiff(required, names(summary))
  if (length(missing)) {
    stop(
      "Consumption IV dynamics summary is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  expected <- plain_chr(specs$specification_id)
  observed <- plain_chr(summary$specification_id)
  if (anyDuplicated(observed) || !setequal(expected, observed)) {
    stop(
      "Consumption IV dynamics summary does not contain exactly one row per registered specification.",
      call. = FALSE
    )
  }
  summary <- summary[match(expected, observed), , drop = FALSE]

  sample_ok <- with(
    summary,
    is.finite(first_stage_n) &
      first_stage_n == reduced_form_n &
      first_stage_n == second_stage_n &
      first_stage_n == n
  )
  status_ok <- summary$first_stage_status == "estimated" &
    summary$effective_f_status == "estimated" &
    summary$reduced_form_status == "estimated" &
    summary$second_stage_status == "estimated" &
    summary$status == "estimated"
  inference_ok <- is.finite(summary$partial_f) &
    is.finite(summary$effective_f) &
    is.finite(summary$effective_f_critical_value) &
    is.finite(summary$effective_f_p_value) &
    is.finite(summary$effective_f_df) &
    is.finite(summary$reduced_form_estimate) &
    is.finite(summary$reduced_form_std.error) &
    is.finite(summary$reduced_form_p.value) &
    is.finite(summary$second_stage_estimate) &
    is.finite(summary$second_stage_std.error) &
    is.finite(summary$second_stage_p.value) &
    is.finite(summary$anderson_rubin_p_beta0)

  bad <- !(sample_ok & status_ok & inference_ok)
  if (any(bad)) {
    detail <- paste0(
      summary$specification_id[bad],
      "[fs=", summary$first_stage_status[bad],
      ",mop=", summary$effective_f_status[bad],
      ",rf=", summary$reduced_form_status[bad],
      ",iv=", summary$second_stage_status[bad],
      ",ar=", summary$status[bad],
      ",n=", summary$first_stage_n[bad], "/",
      summary$reduced_form_n[bad], "/",
      summary$second_stage_n[bad], "/",
      summary$n[bad], "]"
    )
    stop(
      "Registered consumption IV dynamics are not analysis-ready: ",
      paste(detail, collapse = "; "),
      call. = FALSE
    )
  }

  grid <- safe_df(dynamics$anderson_rubin_grid)
  if (!nrow(grid) || !"specification_id" %in% names(grid) ||
      !all(expected %in% plain_chr(grid$specification_id))) {
    stop(
      "Consumption IV dynamics lack Anderson-Rubin grids for registered specifications.",
      call. = FALSE
    )
  }

  dynamics$summary <- summary
  dynamics
}

validate_consumption_iv_robustness_family <- function(
    dynamics,
    support,
    group_size = 6L,
    family_label = "Consumption IV robustness") {
  if (!is.list(dynamics) || !"summary" %in% names(dynamics)) {
    stop(family_label, " dynamics lack a summary.", call. = FALSE)
  }
  summary <- safe_df(dynamics$summary)
  support <- safe_df(support)
  required_summary <- c("welfare_specification_id", "n", "first_stage_n", "reduced_form_n", "second_stage_n")
  missing <- setdiff(required_summary, names(summary))
  if (length(missing) || !all(c("group_id", "n_common", "status") %in% names(support))) {
    stop(family_label, " common-sample validation lacks required fields.", call. = FALSE)
  }
  groups <- split(seq_len(nrow(summary)), plain_chr(summary$welfare_specification_id))
  if (!setequal(names(groups), plain_chr(support$group_id))) {
    stop(family_label, " summary and support groups differ.", call. = FALSE)
  }
  for (group_id in names(groups)) {
    index <- groups[[group_id]]
    expected_n <- num(support$n_common[match(group_id, plain_chr(support$group_id))])
    realized <- unique(num(summary$n[index]))
    stage_n <- unique(c(
      num(summary$first_stage_n[index]),
      num(summary$reduced_form_n[index]),
      num(summary$second_stage_n[index])
    ))
    if (length(index) != group_size || length(realized) != 1L || length(stage_n) != 1L ||
        !is.finite(expected_n) || realized[[1L]] != expected_n || stage_n[[1L]] != expected_n) {
      stop(family_label, " did not preserve registered common support for ", group_id, ".", call. = FALSE)
    }
  }
  dynamics
}

add_consumption_iv_family_multiplicity <- function(dynamics, multiplicity_family) {
  if (!is.list(dynamics) || !"summary" %in% names(dynamics)) {
    stop("Consumption IV robustness dynamics lack a summary.", call. = FALSE)
  }
  out <- dynamics
  summary <- safe_df(out$summary)
  required <- c(
    "welfare_specification_id", "reduced_form_p.value", "anderson_rubin_p_beta0"
  )
  missing <- setdiff(required, names(summary))
  if (length(missing)) {
    stop("Consumption IV multiplicity lacks fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  summary$reduced_form_p_holm_within_welfare <- NA_real_
  summary$anderson_rubin_p_beta0_holm_within_welfare <- NA_real_
  groups <- split(seq_len(nrow(summary)), plain_chr(summary$welfare_specification_id))
  for (index in groups) {
    summary$reduced_form_p_holm_within_welfare[index] <-
      holm_adjust_finite(summary$reduced_form_p.value[index])
    summary$anderson_rubin_p_beta0_holm_within_welfare[index] <-
      holm_adjust_finite(summary$anderson_rubin_p_beta0[index])
  }
  summary$reduced_form_p_holm_family <- holm_adjust_finite(summary$reduced_form_p.value)
  summary$anderson_rubin_p_beta0_holm_family <- holm_adjust_finite(summary$anderson_rubin_p_beta0)
  summary$multiplicity_family <- multiplicity_family
  out$summary <- summary
  out
}

save_consumption_iv_robustness_family <- function(
    dynamics,
    support,
    artifact_prefix,
    directory = "outputs/diagnostics/extended/consumption") {
  objects <- list(safe_df(dynamics$summary), safe_df(support))
  names(objects) <- c(artifact_prefix, paste0(artifact_prefix, "_common_support"))
  write_diagnostic_bundle(objects, directory = directory)
}

validate_consumption_scalar_iv_robustness <- function(dynamics, support) {
  validate_consumption_iv_robustness_family(
    dynamics, support, 6L, "Consumption scalar-IV robustness"
  )
}

add_consumption_scalar_iv_multiplicity <- function(dynamics) {
  add_consumption_iv_family_multiplicity(dynamics, "consumption_scalar_iv_robustness")
}

save_consumption_scalar_iv_robustness <- function(
    dynamics,
    support,
    directory = "outputs/diagnostics/extended/consumption") {
  save_consumption_iv_robustness_family(
    dynamics, support, "consumption_scalar_iv_robustness", directory
  )
}

save_consumption_iv_dynamics <- function(
    dynamics,
    directory = "outputs/diagnostics/extended/consumption") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  c(
    write_diagnostic_csv(
      safe_df(dynamics$summary),
      file.path(directory, "consumption_iv_dynamics.csv")
    ),
    write_diagnostic_csv(
      safe_df(dynamics$anderson_rubin_grid),
      file.path(directory, "consumption_iv_dynamics_anderson_rubin_grid.csv")
    )
  )
}
