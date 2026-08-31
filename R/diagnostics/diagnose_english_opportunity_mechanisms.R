# Compact district-level mechanism grid for the paper rescue.
#
# This reuses the canonical IV adjustment definitions and first-stage
# residual/inference machinery. It is a relevance diagnostic, not 2SLS: each
# registered district mechanism measure is treated as an outcome and related to
# the preferred linguistic-distance construction under exactly three geography
# specifications. Each outcome uses one fixed complete-case sample across those
# three specifications so changes across columns reflect adjustment rather than
# sample composition.

district_mechanism_adjustment_registry <- function() {
  ids <- c("unadjusted", "region_main", "state_main")
  adjustments <- iv_adjustment_sets()[ids]
  rows <- lapply(seq_along(adjustments), function(i) {
    adjustment <- adjustments[[i]]
    data.frame(
      specification_id = ids[[i]],
      label = adjustment$label,
      fixed_effect = adjustment$fixed_effect,
      controls = I(list(order_iv_controls(adjustment$controls))),
      sequence = i,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

preferred_district_mechanism_registry <- function(registry) {
  x <- safe_df(registry)
  x[x$unit == "district" & x$preferred %in% TRUE, , drop = FALSE]
}

prepare_district_mechanism_sample <- function(
    panel,
    outcome,
    instrument = "ling_distance_nonzero_mean",
    controls = census_2001_main_controls()) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  needed <- unique(c(
    outcome, instrument, "target_unit_2001", "state_code_2001", "region", controls
  ))
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop(
      "District mechanism panel is missing columns for ", outcome, ": ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  numeric_vars <- unique(c(outcome, instrument, controls))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$target_unit_2001 <- plain_chr(x$target_unit_2001)
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$region <- plain_chr(x$region)
  keep <- stats::complete.cases(x[needed]) &
    nzchar(x$target_unit_2001) & nzchar(x$state_code_2001) & nzchar(x$region)
  x <- x[keep, needed, drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) {
    stop("No complete district mechanism sample is available for ", outcome, ".", call. = FALSE)
  }
  if (anyDuplicated(x$target_unit_2001)) {
    stop("District mechanism sample is not unique by Census-2001 target.", call. = FALSE)
  }
  x
}

standardize_district_mechanism_estimate <- function(summary) {
  scale <- summary$residual_instrument_sd / summary$residual_treatment_sd
  summary$standardized_estimate <- ifelse(
    is.finite(scale), summary$estimate * scale, NA_real_
  )
  summary$standardized_std_error <- ifelse(
    is.finite(scale), summary$std.error * abs(scale), NA_real_
  )
  summary
}

estimate_district_mechanism_grid <- function(
    panel,
    measure,
    adjustments = district_mechanism_adjustment_registry(),
    instrument = "ling_distance_nonzero_mean") {
  outcome <- measure$variable[[1L]]
  controls <- unique(unlist(adjustments$controls, use.names = FALSE))
  sample <- prepare_district_mechanism_sample(panel, outcome, instrument, controls)
  estimates <- safe_bind_rows(lapply(seq_len(nrow(adjustments)), function(i) {
    estimate <- estimate_first_stage_absorption_spec(
      sample, adjustments[i, , drop = FALSE], outcome, instrument
    )$summary
    estimate <- standardize_district_mechanism_estimate(estimate)
    estimate$outcome_variable <- outcome
    estimate$residual_outcome_sd <- estimate$residual_treatment_sd
    estimate$treatment <- NULL
    estimate$residual_treatment_sd <- NULL
    estimate$measure_id <- measure$measure_id[[1L]]
    estimate$source <- measure$source[[1L]]
    estimate$stage <- measure$stage[[1L]]
    estimate$source_side <- measure$source_side[[1L]]
    estimate$paper_role <- measure$paper_role[[1L]]
    estimate$interpretation <- measure$interpretation[[1L]]
    estimate
  }))
  if (length(unique(estimates$n)) != 1L) {
    stop("District mechanism specifications changed sample size for ", outcome, ".", call. = FALSE)
  }
  estimates
}

diagnose_english_opportunity_district_mechanisms <- function(
    panel,
    registry,
    instrument = "ling_distance_nonzero_mean") {
  measures <- preferred_district_mechanism_registry(registry)
  if (!nrow(measures)) stop("No preferred district mechanism measures are registered.", call. = FALSE)
  adjustments <- district_mechanism_adjustment_registry()
  estimates <- safe_bind_rows(lapply(seq_len(nrow(measures)), function(i) {
    estimate_district_mechanism_grid(
      panel, measures[i, , drop = FALSE], adjustments, instrument
    )
  }))
  estimates <- estimates[order(
    match(estimates$measure_id, measures$measure_id), estimates$sequence
  ), , drop = FALSE]
  rownames(estimates) <- NULL
  list(
    measures = measures,
    specifications = adjustments,
    estimates = estimates
  )
}

save_english_opportunity_district_mechanisms <- function(
    diagnostics,
    dir = "outputs/diagnostics/extended/mechanisms") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  c(
    measure_registry = write_diagnostic_csv(
      diagnostics$measures, file.path(dir, "district_mechanism_measures.csv")
    ),
    specification_registry = write_diagnostic_csv(
      collapse_diagnostic_list_columns(diagnostics$specifications, "controls"),
      file.path(dir, "district_mechanism_specifications.csv")
    ),
    estimates = write_diagnostic_csv(
      diagnostics$estimates, file.path(dir, "district_mechanism_estimates.csv")
    )
  )
}
