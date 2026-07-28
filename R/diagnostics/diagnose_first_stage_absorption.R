# Diagnose where the preferred linguistic-distance first stage loses relevance.

first_stage_control_blocks <- function() {
  list(
    basic_scale_geography = c(
      "log_population_2001", "urban_share_2001", "log_population_density_2001"
    ),
    social_composition = c("sc_share_2001", "st_share_2001", "muslim_share_2001"),
    human_capital = "adult_secondary_plus_share_2001",
    demography = "dependency_ratio_2001",
    economic_structure = "agricultural_worker_share_2001",
    basic_development = "electricity_access_share_2001"
  )
}

first_stage_absorption_registry <- function() {
  blocks <- first_stage_control_blocks()
  cumulative <- lapply(seq_along(blocks), function(i) unname(unlist(blocks[seq_len(i)], use.names = FALSE)))
  block_rows <- data.frame(
    specification_id = paste0("state_fe_plus_", names(blocks)),
    label = paste0("State FE + through ", gsub("_", " ", names(blocks))),
    fixed_effect = "state",
    controls = I(cumulative),
    sequence = 6L + seq_along(blocks),
    stringsAsFactors = FALSE
  )
  base <- data.frame(
    specification_id = c(
      "instrument_only", "region_fe", "state_fe", "census_controls",
      "state_fe_census_controls"
    ),
    label = c(
      "Instrument only", "Six-region fixed effects", "State fixed effects",
      "Census controls", "State fixed effects + Census controls"
    ),
    fixed_effect = c("none", "region", "state", "none", "state"),
    controls = I(list(
      character(), character(), character(), census_2001_main_controls(),
      census_2001_main_controls()
    )),
    sequence = 1:5,
    stringsAsFactors = FALSE
  )
  out <- rbind(base, block_rows)
  out[order(out$sequence), , drop = FALSE]
}

first_stage_absorption_variables <- function(
  treatment = "emi_exposure_all_children_0708",
  instrument = "ling_distance_nonzero_mean"
) {
  unique(c(
    treatment, instrument, "state_code_2001", "region",
    unlist(first_stage_control_blocks(), use.names = FALSE)
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

  numeric_vars <- unique(c(treatment, instrument, unlist(first_stage_control_blocks(), use.names = FALSE)))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$region <- as.character(x$region)
  keep <- stats::complete.cases(x[needed]) & nzchar(x$state_code_2001) & nzchar(x$region)
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

residualize_first_stage_variable <- function(data, variable, controls = character(), fixed_effect = "none") {
  rhs <- controls
  if (identical(fixed_effect, "region")) rhs <- c(rhs, "factor(region)")
  if (identical(fixed_effect, "state")) rhs <- c(rhs, "factor(state_code_2001)")
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
  restricted_rhs <- controls
  if (identical(fixed_effect, "region")) restricted_rhs <- c(restricted_rhs, "factor(region)")
  if (identical(fixed_effect, "state")) restricted_rhs <- c(restricted_rhs, "factor(state_code_2001)")
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
  estimate <- unname(stats::coef(fit)[instrument])

  data.frame(
    specification_id = specification$specification_id,
    specification = specification$label,
    sequence = specification$sequence,
    treatment = treatment,
    instrument = instrument,
    fixed_effect = fixed_effect,
    control_blocks = paste(names(first_stage_control_blocks())[vapply(first_stage_control_blocks(), function(block) all(block %in% controls), logical(1))], collapse = ";"),
    n_controls = length(controls),
    estimate = estimate,
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
  structure(
    list(summary = summary, registry = registry, common_support = data.frame(
      treatment = treatment, instrument = instrument, n = nrow(data),
      n_states = length(unique(data$state_code_2001)),
      n_regions = length(unique(data$region)), stringsAsFactors = FALSE
    )),
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
    common_support = write_diagnostic_csv(diagnostics$common_support, file.path(dir, "first_stage_absorption_common_support.csv"))
  ))
}
