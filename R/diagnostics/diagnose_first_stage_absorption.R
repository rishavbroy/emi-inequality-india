# Diagnose where the preferred linguistic-distance first stage loses relevance.

first_stage_control_blocks <- function() {
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

first_stage_control_block_membership <- function() {
  blocks <- first_stage_control_blocks()
  blocks$economic_structure <- unique(c(
    "agricultural_worker_share_2001", blocks$economic_structure
  ))
  blocks
}

first_stage_included_control_blocks <- function(controls) {
  blocks <- first_stage_control_block_membership()
  names(blocks)[vapply(blocks, function(block) any(block %in% controls), logical(1))]
}

order_first_stage_controls <- function(controls, canonical = census_2001_diagnostic_controls()) {
  canonical[canonical %in% unique(controls)]
}

first_stage_block_registry_rows <- function(fixed_effect, start_sequence) {
  blocks <- first_stage_control_blocks()
  cumulative <- lapply(seq_along(blocks), function(i) {
    order_first_stage_controls(unlist(blocks[seq_len(i)], use.names = FALSE))
  })
  prefix <- paste0(fixed_effect, "_fe_plus_")
  data.frame(
    specification_id = paste0(prefix, names(blocks)),
    label = paste0(
      if (identical(fixed_effect, "region")) "Six-region FE" else "State FE",
      " + through ", gsub("_", " ", names(blocks))
    ),
    fixed_effect = fixed_effect,
    controls = I(cumulative),
    sequence = start_sequence + seq_along(blocks),
    stringsAsFactors = FALSE
  )
}

first_stage_without_human_capital <- function(controls) {
  setdiff(controls, first_stage_control_blocks()$human_capital)
}

first_stage_absorption_registry <- function() {
  main <- census_2001_main_controls()
  expanded <- census_2001_absorption_controls()
  base <- data.frame(
    specification_id = c(
      "instrument_only", "region_fe", "state_fe", "census_controls",
      "region_fe_census_controls", "state_fe_census_controls",
      "expanded_controls", "region_fe_expanded_controls", "state_fe_expanded_controls",
      "region_fe_main_without_human_capital", "state_fe_main_without_human_capital",
      "region_fe_expanded_without_human_capital", "state_fe_expanded_without_human_capital"
    ),
    label = c(
      "Instrument only", "Six-region fixed effects", "State fixed effects",
      "Main Census controls", "Six-region fixed effects + main Census controls",
      "State fixed effects + main Census controls", "Expanded Census controls",
      "Six-region fixed effects + expanded Census controls",
      "State fixed effects + expanded Census controls",
      "Six-region FE + main controls without human capital",
      "State FE + main controls without human capital",
      "Six-region FE + expanded controls without human capital",
      "State FE + expanded controls without human capital"
    ),
    fixed_effect = c(
      "none", "region", "state", "none", "region", "state", "none", "region", "state",
      "region", "state", "region", "state"
    ),
    controls = I(list(
      character(), character(), character(), main, main, main,
      expanded, expanded, expanded,
      first_stage_without_human_capital(main), first_stage_without_human_capital(main),
      first_stage_without_human_capital(expanded), first_stage_without_human_capital(expanded)
    )),
    sequence = seq_len(13L),
    stringsAsFactors = FALSE
  )
  region_rows <- first_stage_block_registry_rows("region", 13L)
  state_rows <- first_stage_block_registry_rows("state", 13L + nrow(region_rows))
  out <- rbind(base, region_rows, state_rows)
  out[order(out$sequence), , drop = FALSE]
}

first_stage_absorption_variables <- function(
  treatment = "emi_exposure_all_children_0708",
  instrument = "ling_distance_nonzero_mean"
) {
  unique(c(
    treatment, instrument, "state_code_2001", "district_code_2001", "region",
    census_2001_diagnostic_controls()
  ))
}

prepare_first_stage_absorption_panel <- function(
  panel,
  treatment = "emi_exposure_all_children_0708",
  instrument = "ling_distance_nonzero_mean"
) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  needed <- first_stage_absorption_variables(treatment, instrument)
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("First-stage absorption panel is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)

  numeric_vars <- unique(c(treatment, instrument, census_2001_diagnostic_controls()))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$district_code_2001 <- plain_chr(x$district_code_2001)
  x$region <- as.character(x$region)
  keep <- stats::complete.cases(x[needed]) & nzchar(x$state_code_2001) &
    nzchar(x$district_code_2001) & nzchar(x$region)
  x <- x[keep, , drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) stop("No complete common support is available for first-stage absorption diagnostics.", call. = FALSE)
  if (length(unique(x$region)) != length(panel_region_levels())) {
    stop("First-stage absorption common support does not contain all six panel regions.", call. = FALSE)
  }
  x
}

first_stage_absorption_formula <- function(treatment, instrument, controls = character(), fixed_effect = "none") {
  rhs <- c(instrument, controls)
  if (identical(fixed_effect, "region")) rhs <- c(rhs, "factor(region)")
  if (identical(fixed_effect, "state")) rhs <- c(rhs, "factor(state_code_2001)")
  stats::reformulate(rhs, response = treatment)
}

first_stage_nuisance_terms <- function(controls = character(), fixed_effect = "none") {
  rhs <- controls
  if (identical(fixed_effect, "region")) rhs <- c(rhs, "factor(region)")
  if (identical(fixed_effect, "state")) rhs <- c(rhs, "factor(state_code_2001)")
  rhs
}

residualize_first_stage_variable <- function(data, variable, controls = character(), fixed_effect = "none") {
  rhs <- first_stage_nuisance_terms(controls, fixed_effect)
  if (!length(rhs)) return(num(data[[variable]]) - mean(num(data[[variable]])))
  stats::residuals(stats::lm(stats::reformulate(rhs, response = variable), data = data))
}

clustered_first_stage_inference <- function(fit, instrument, cluster) {
  inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  if (is.null(inference) || is.null(inference$vcov)) {
    coefs <- summary(fit)$coefficients
    row <- match(instrument, rownames(coefs))
    if (is.na(row)) return(c(std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, partial_f = NA_real_))
    return(c(
      std.error = coefs[row, "Std. Error"], statistic = coefs[row, "t value"],
      p.value = coefs[row, "Pr(>|t|)"], partial_f = coefs[row, "t value"]^2
    ))
  }
  table <- tryCatch(lmtest::coeftest(fit, vcov. = inference$vcov), error = function(e) NULL)
  row <- if (!is.null(table)) match(instrument, rownames(table)) else NA_integer_
  if (is.na(row)) return(c(std.error = NA_real_, statistic = NA_real_, p.value = NA_real_, partial_f = NA_real_))
  statistic <- suppressWarnings(as.numeric(table[row, 3]))
  c(
    std.error = suppressWarnings(as.numeric(table[row, 2])), statistic = statistic,
    p.value = suppressWarnings(as.numeric(table[row, 4])), partial_f = statistic^2
  )
}

partial_r_squared_first_stage <- function(data, treatment, instrument, controls = character(), fixed_effect = "none") {
  restricted_rhs <- first_stage_nuisance_terms(controls, fixed_effect)
  restricted <- if (length(restricted_rhs)) {
    stats::lm(stats::reformulate(restricted_rhs, response = treatment), data = data)
  } else {
    stats::lm(stats::as.formula(paste(treatment, "~ 1")), data = data)
  }
  full <- stats::lm(first_stage_absorption_formula(treatment, instrument, controls, fixed_effect), data = data)
  sse_restricted <- stats::deviance(restricted)
  if (!is.finite(sse_restricted) || sse_restricted <= 0) return(NA_real_)
  max(0, (sse_restricted - stats::deviance(full)) / sse_restricted)
}

estimate_first_stage_absorption_spec <- function(data, specification, treatment, instrument) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  fit <- stats::lm(first_stage_absorption_formula(treatment, instrument, controls, fixed_effect), data = data)
  inference <- clustered_first_stage_inference(fit, instrument, data$state_code_2001)
  z_resid <- residualize_first_stage_variable(data, instrument, controls, fixed_effect)
  d_resid <- residualize_first_stage_variable(data, treatment, controls, fixed_effect)

  data.frame(
    specification_id = specification$specification_id,
    specification = specification$label,
    sequence = specification$sequence,
    treatment = treatment,
    instrument = instrument,
    fixed_effect = fixed_effect,
    control_blocks = paste(first_stage_included_control_blocks(controls), collapse = ";"),
    n_controls = length(controls),
    estimate = unname(stats::coef(fit)[instrument]),
    std.error = unname(inference[["std.error"]]),
    statistic = unname(inference[["statistic"]]),
    p.value = unname(inference[["p.value"]]),
    excluded_instrument_f = unname(inference[["partial_f"]]),
    partial_r_squared = partial_r_squared_first_stage(data, treatment, instrument, controls, fixed_effect),
    residual_instrument_sd = stats::sd(z_resid),
    residual_treatment_sd = stats::sd(d_resid),
    residual_correlation = stats::cor(z_resid, d_resid),
    instrument_variance_remaining = stats::var(z_resid) / stats::var(num(data[[instrument]])),
    n = stats::nobs(fit),
    n_states = length(unique(data$state_code_2001)),
    n_regions = length(unique(data$region)),
    status = "estimated",
    reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

first_stage_state_residual_ranges <- function(data, registry, treatment, instrument) {
  safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    spec <- registry[i, , drop = FALSE]
    controls <- unlist(spec$controls[[1]], use.names = FALSE)
    fixed_effect <- spec$fixed_effect[[1]]
    z <- residualize_first_stage_variable(data, instrument, controls, fixed_effect)
    d <- residualize_first_stage_variable(data, treatment, controls, fixed_effect)
    tmp <- data.frame(state_code_2001 = data$state_code_2001, z = z, d = d)
    safe_bind_rows(lapply(split(tmp, tmp$state_code_2001), function(x) {
      data.frame(
        specification_id = spec$specification_id,
        state_code_2001 = x$state_code_2001[[1]],
        n_districts = nrow(x),
        instrument_min = min(x$z), instrument_max = max(x$z),
        instrument_range = diff(range(x$z)), instrument_sd = stats::sd(x$z),
        treatment_min = min(x$d), treatment_max = max(x$d),
        treatment_range = diff(range(x$d)), treatment_sd = stats::sd(x$d),
        stringsAsFactors = FALSE
      )
    }))
  }))
}

first_stage_full_specification <- function(registry) {
  registry[registry$specification_id == "state_fe_expanded_controls", , drop = FALSE]
}

first_stage_state_deletion <- function(data, specification, treatment, instrument) {
  full <- estimate_first_stage_absorption_spec(data, specification, treatment, instrument)
  safe_bind_rows(lapply(sort(unique(data$state_code_2001)), function(state) {
    reduced <- data[data$state_code_2001 != state, , drop = FALSE]
    row <- estimate_first_stage_absorption_spec(reduced, specification, treatment, instrument)
    row$omitted_state <- state
    row$estimate_change <- row$estimate - full$estimate
    row$f_change <- row$excluded_instrument_f - full$excluded_instrument_f
    row
  }))
}

first_stage_district_influence <- function(data, specification, treatment, instrument) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  fit <- stats::lm(first_stage_absorption_formula(treatment, instrument, controls, fixed_effect), data = data)
  dfb <- stats::dfbeta(fit)
  instrument_dfbeta <- if (instrument %in% colnames(dfb)) dfb[, instrument] else rep(NA_real_, nrow(data))
  data.frame(
    state_code_2001 = data$state_code_2001,
    district_code_2001 = data$district_code_2001,
    leverage = stats::hatvalues(fit),
    cooks_distance = stats::cooks.distance(fit),
    studentized_residual = stats::rstudent(fit),
    instrument_dfbeta = instrument_dfbeta,
    stringsAsFactors = FALSE
  )
}

first_stage_vif_diagnostics <- function(data, registry, treatment, instrument) {
  ids <- c(
    "region_fe_census_controls", "region_fe_expanded_controls",
    "state_fe_census_controls", "state_fe_expanded_controls"
  )
  safe_bind_rows(lapply(ids, function(id) {
    spec <- registry[registry$specification_id == id, , drop = FALSE]
    controls <- unlist(spec$controls[[1]], use.names = FALSE)
    fit <- stats::lm(first_stage_absorption_formula(
      treatment, instrument, controls, spec$fixed_effect[[1]]
    ), data = data)
    out <- compute_vif_if_applicable(fit)
    out$specification_id <- id
    out
  }))
}

#' Estimate a fixed-support first-stage absorption ladder
#'
#' The preferred Phase 1 treatment and full-distribution scalar instrument are
#' defaults. Public IV outputs remain unchanged until these diagnostics are reviewed.
diagnose_first_stage_absorption <- function(
  panel,
  treatment = "emi_exposure_all_children_0708",
  instrument = "ling_distance_nonzero_mean"
) {
  data <- prepare_first_stage_absorption_panel(panel, treatment, instrument)
  registry <- first_stage_absorption_registry()
  summary <- safe_bind_rows(lapply(seq_len(nrow(registry)), function(i) {
    estimate_first_stage_absorption_spec(data, registry[i, , drop = FALSE], treatment, instrument)
  }))
  full_spec <- first_stage_full_specification(registry)
  structure(
    list(
      summary = summary,
      registry = registry,
      common_support = data.frame(
        treatment = treatment, instrument = instrument, n = nrow(data),
        n_states = length(unique(data$state_code_2001)),
        n_regions = length(unique(data$region)), stringsAsFactors = FALSE
      ),
      state_residual_ranges = first_stage_state_residual_ranges(data, registry, treatment, instrument),
      state_deletion = first_stage_state_deletion(data, full_spec, treatment, instrument),
      district_influence = first_stage_district_influence(data, full_spec, treatment, instrument),
      vif = first_stage_vif_diagnostics(data, registry, treatment, instrument)
    ),
    class = "emi_first_stage_absorption"
  )
}

save_first_stage_absorption_diagnostics <- function(
  diagnostics,
  dir = "outputs/diagnostics/extended/instrument_relevance"
) {
  if (!inherits(diagnostics, "emi_first_stage_absorption")) stop("Expected first-stage absorption diagnostics.", call. = FALSE)
  registry <- diagnostics$registry
  registry$controls <- vapply(registry$controls, paste, collapse = ";", FUN.VALUE = character(1))
  output_manifest(c(
    specification_ladder = write_diagnostic_csv(diagnostics$summary, file.path(dir, "first_stage_absorption_ladder.csv")),
    specification_registry = write_diagnostic_csv(registry, file.path(dir, "first_stage_absorption_registry.csv")),
    common_support = write_diagnostic_csv(diagnostics$common_support, file.path(dir, "first_stage_absorption_common_support.csv")),
    state_residual_ranges = write_diagnostic_csv(diagnostics$state_residual_ranges, file.path(dir, "first_stage_state_residual_ranges.csv")),
    state_deletion = write_diagnostic_csv(diagnostics$state_deletion, file.path(dir, "first_stage_state_deletion.csv")),
    district_influence = write_diagnostic_csv(diagnostics$district_influence, file.path(dir, "first_stage_district_influence.csv")),
    vif = write_diagnostic_csv(diagnostics$vif, file.path(dir, "first_stage_vif.csv"))
  ))
}

alternative_distance_adjustments <- function() {
  list(
    unadjusted = list(label = "Unadjusted", fixed_effect = "none", controls = character()),
    region_main = list(label = "Six-region FE + main controls", fixed_effect = "region", controls = census_2001_main_controls()),
    region_expanded = list(label = "Six-region FE + expanded controls", fixed_effect = "region", controls = census_2001_absorption_controls()),
    state_main = list(label = "State FE + main controls", fixed_effect = "state", controls = census_2001_main_controls()),
    state_expanded = list(label = "State FE + expanded controls", fixed_effect = "state", controls = census_2001_absorption_controls())
  )
}

alternative_distance_constructions <- function() {
  list(
    nonzero_mean = list(
      label = "Mean distance among speakers above zero",
      excluded = "ling_distance_nonzero_mean", included = character()
    ),
    distant_share = list(
      label = "Share speaking languages at distance three or higher",
      excluded = "ling_share_distance_ge3", included = character()
    ),
    top3_legacy = list(
      label = "Legacy top-three weighted mean",
      excluded = "ling_distance_top3_legacy", included = character()
    ),
    nonzero_mean_hindi_urdu = list(
      label = "Nonzero mean with combined Hindi-Urdu share",
      excluded = "ling_distance_nonzero_mean", included = "hindi_urdu_share"
    ),
    nonzero_mean_hindi_urdu_separate = list(
      label = "Nonzero mean with separate Hindi and Urdu shares",
      excluded = "ling_distance_nonzero_mean", included = c("hindi_share", "urdu_share")
    ),
    distance_shares = list(
      label = "Five nonzero linguistic-distance shares",
      excluded = linguistic_distance_excluded_instruments(), included = character()
    )
  )
}

alternative_distance_registry <- function() {
  adjustments <- alternative_distance_adjustments()
  constructions <- alternative_distance_constructions()
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
        fixed_effect = adjustment$fixed_effect,
        controls = I(list(order_first_stage_controls(adjustment$controls))),
        included_language_controls = I(list(construction$included)),
        excluded_instruments = I(list(construction$excluded)),
        sequence = sequence,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

alternative_distance_variables <- function() {
  constructions <- alternative_distance_constructions()
  unique(c(
    unlist(lapply(constructions, `[[`, "excluded"), use.names = FALSE),
    unlist(lapply(constructions, `[[`, "included"), use.names = FALSE)
  ))
}

clustered_joint_wald_test <- function(fit, terms, cluster) {
  inference <- tryCatch(iv_clustered_inference(fit, cluster), error = function(e) NULL)
  if (is.null(inference) || is.null(inference$vcov) || !requireNamespace("car", quietly = TRUE)) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  hypotheses <- paste0(terms, " = 0")
  test <- tryCatch(
    car::linearHypothesis(fit, hypotheses, vcov. = inference$vcov, test = "F"),
    error = function(e) NULL
  )
  if (is.null(test) || nrow(test) < 2L || !all(c("F", "Pr(>F)") %in% names(test))) {
    return(c(statistic = NA_real_, p.value = NA_real_, df = length(terms)))
  }
  c(
    statistic = suppressWarnings(as.numeric(test[["F"]][[2]])),
    p.value = suppressWarnings(as.numeric(test[["Pr(>F)"]][[2]])),
    df = length(terms)
  )
}

partial_r_squared_instrument_set <- function(data, treatment, excluded, included, controls, fixed_effect) {
  nuisance <- unique(c(controls, included))
  restricted_rhs <- first_stage_nuisance_terms(nuisance, fixed_effect)
  restricted <- if (length(restricted_rhs)) {
    stats::lm(stats::reformulate(restricted_rhs, response = treatment), data = data)
  } else {
    stats::lm(stats::reformulate(character(), response = treatment), data = data)
  }
  full_rhs <- c(excluded, nuisance)
  if (identical(fixed_effect, "region")) full_rhs <- c(full_rhs, "factor(region)")
  if (identical(fixed_effect, "state")) full_rhs <- c(full_rhs, "factor(state_code_2001)")
  full <- stats::lm(stats::reformulate(full_rhs, response = treatment), data = data)
  denominator <- stats::deviance(restricted)
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  max(0, (denominator - stats::deviance(full)) / denominator)
}

estimate_alternative_distance_spec <- function(data, specification, treatment) {
  controls <- unlist(specification$controls[[1]], use.names = FALSE)
  included <- unlist(specification$included_language_controls[[1]], use.names = FALSE)
  excluded <- unlist(specification$excluded_instruments[[1]], use.names = FALSE)
  fixed_effect <- specification$fixed_effect[[1]]
  rhs <- unique(c(excluded, included, controls))
  if (identical(fixed_effect, "region")) rhs <- c(rhs, "factor(region)")
  if (identical(fixed_effect, "state")) rhs <- c(rhs, "factor(state_code_2001)")
  fit <- stats::lm(stats::reformulate(rhs, response = treatment), data = data)
  joint <- clustered_joint_wald_test(fit, excluded, data$state_code_2001)
  coefficients <- safe_bind_rows(lapply(excluded, function(term) {
    inference <- clustered_first_stage_inference(fit, term, data$state_code_2001)
    data.frame(
      specification_id = specification$specification_id,
      term = term,
      estimate = unname(stats::coef(fit)[term]),
      std.error = unname(inference[["std.error"]]),
      statistic = unname(inference[["statistic"]]),
      p.value = unname(inference[["p.value"]]),
      stringsAsFactors = FALSE
    )
  }))
  summary <- data.frame(
    specification_id = specification$specification_id,
    sequence = specification$sequence,
    adjustment_id = specification$adjustment_id,
    adjustment = specification$adjustment,
    construction_id = specification$construction_id,
    construction = specification$construction,
    fixed_effect = fixed_effect,
    excluded_instruments = paste(excluded, collapse = ";"),
    included_language_controls = paste(included, collapse = ";"),
    n_excluded_instruments = length(excluded),
    joint_excluded_f = unname(joint[["statistic"]]),
    joint_excluded_p = unname(joint[["p.value"]]),
    partial_r_squared = partial_r_squared_instrument_set(
      data, treatment, excluded, included, controls, fixed_effect
    ),
    n = stats::nobs(fit),
    n_states = length(unique(data$state_code_2001)),
    n_regions = length(unique(data$region)),
    stringsAsFactors = FALSE
  )
  list(summary = summary, coefficients = coefficients)
}

prepare_alternative_distance_panel <- function(panel, treatment = "emi_exposure_all_children_0708") {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else as.data.frame(panel, stringsAsFactors = FALSE)
  needed <- unique(c(
    treatment, "state_code_2001", "district_code_2001", "region",
    census_2001_diagnostic_controls(), alternative_distance_variables()
  ))
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("Alternative-distance panel is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  numeric_vars <- setdiff(needed, c("state_code_2001", "district_code_2001", "region"))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$district_code_2001 <- plain_chr(x$district_code_2001)
  x$region <- as.character(x$region)
  keep <- stats::complete.cases(x[needed]) & nzchar(x$state_code_2001) &
    nzchar(x$district_code_2001) & nzchar(x$region)
  x <- x[keep, , drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) stop("No complete common support is available for alternative-distance diagnostics.", call. = FALSE)
  if (length(unique(x$region)) != length(panel_region_levels())) {
    stop("Alternative-distance common support does not contain all six panel regions.", call. = FALSE)
  }
  x
}

diagnose_alternative_distance_first_stages <- function(
  panel,
  treatment = "emi_exposure_all_children_0708"
) {
  data <- prepare_alternative_distance_panel(panel, treatment)
  registry <- alternative_distance_registry()
  estimated <- lapply(seq_len(nrow(registry)), function(i) {
    estimate_alternative_distance_spec(data, registry[i, , drop = FALSE], treatment)
  })
  structure(
    list(
      summary = safe_bind_rows(lapply(estimated, `[[`, "summary")),
      coefficients = safe_bind_rows(lapply(estimated, `[[`, "coefficients")),
      registry = registry,
      common_support = data.frame(
        treatment = treatment, n = nrow(data),
        n_states = length(unique(data$state_code_2001)),
        n_regions = length(unique(data$region)), stringsAsFactors = FALSE
      )
    ),
    class = "emi_alternative_distance_first_stages"
  )
}

save_alternative_distance_first_stages <- function(
  diagnostics,
  dir = "outputs/diagnostics/extended/instrument_relevance"
) {
  if (!inherits(diagnostics, "emi_alternative_distance_first_stages")) {
    stop("Expected alternative-distance first-stage diagnostics.", call. = FALSE)
  }
  registry <- diagnostics$registry
  for (column in c("controls", "included_language_controls", "excluded_instruments")) {
    registry[[column]] <- vapply(registry[[column]], paste, collapse = ";", FUN.VALUE = character(1))
  }
  output_manifest(c(
    alternative_summary = write_diagnostic_csv(
      diagnostics$summary, file.path(dir, "alternative_distance_first_stage_summary.csv")
    ),
    alternative_coefficients = write_diagnostic_csv(
      diagnostics$coefficients, file.path(dir, "alternative_distance_first_stage_coefficients.csv")
    ),
    alternative_registry = write_diagnostic_csv(
      registry, file.path(dir, "alternative_distance_first_stage_registry.csv")
    ),
    alternative_support = write_diagnostic_csv(
      diagnostics$common_support, file.path(dir, "alternative_distance_first_stage_common_support.csv")
    )
  ))
}
