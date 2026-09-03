# Shastry Hindi-belt robustness for the preferred linguistic-distance first stage.

shastry_hindi_belt_state_definition <- function() {
  codes <- shastry_hindi_belt_state_codes()
  data.frame(
    state_code_2001 = codes,
    state_name_2001 = census_2001_state_name(codes),
    stringsAsFactors = FALSE
  )
}

add_shastry_hindi_belt_indicator <- function(panel) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else {
    as.data.frame(panel, stringsAsFactors = FALSE)
  }
  if (!"state_code_2001" %in% names(x)) {
    stop("Hindi-belt diagnostic requires state_code_2001.", call. = FALSE)
  }
  state <- sprintf("%02d", as.integer(num(x$state_code_2001)))
  if (anyNA(state)) {
    stop("Hindi-belt diagnostic requires complete Census-2001 state codes.", call. = FALSE)
  }
  x[[shastry_hindi_belt_variable()]] <- as.integer(
    state %in% shastry_hindi_belt_state_codes()
  )
  x
}

prepare_hindi_belt_first_stage_panel <- function(panel, specifications) {
  specs <- as_iv_specifications(specifications)
  x <- add_shastry_hindi_belt_indicator(panel)
  controls <- unique(unlist(specs$controls, use.names = FALSE))
  required <- unique(c(
    specs$treatment[[1L]], unlist(specs$excluded_instruments[[1L]], use.names = FALSE),
    "state_code_2001", "district_code_2001", "region", controls
  ))
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Hindi-belt first-stage panel is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  numeric_vars <- setdiff(required, c("state_code_2001", "district_code_2001", "region"))
  for (variable in numeric_vars) x[[variable]] <- num(x[[variable]])
  x$state_code_2001 <- plain_chr(x$state_code_2001)
  x$district_code_2001 <- plain_chr(x$district_code_2001)
  x$region <- as.character(x$region)
  keep <- stats::complete.cases(x[required]) & nzchar(x$state_code_2001) &
    nzchar(x$district_code_2001) & nzchar(x$region)
  x <- x[keep, , drop = FALSE]
  rownames(x) <- NULL
  if (!nrow(x)) stop("No common support is available for the Hindi-belt diagnostic.", call. = FALSE)
  if (length(unique(x$region)) != length(panel_region_levels())) {
    stop("Hindi-belt common support does not contain all six panel regions.", call. = FALSE)
  }
  if (!setequal(unique(x[[shastry_hindi_belt_variable()]]), c(0, 1))) {
    stop("Hindi-belt common support must contain belt and non-belt districts.", call. = FALSE)
  }
  x
}

without_hindi_belt_control <- function(specification) {
  out <- specification
  out$specification_id <- paste0("baseline__", plain_chr(out$specification_id))
  out$adjustment <- sub(" \\+ Shastry Hindi-belt indicator$", "", plain_chr(out$adjustment))
  out$controls <- I(list(setdiff(
    unlist(out$controls[[1L]], use.names = FALSE), shastry_hindi_belt_variable()
  )))
  out
}

#' Compare preferred first stages with and without Shastry's Hindi-belt control
#'
#' Both cells use the same complete-case sample and main Census controls. State
#' FE are not admissible because the Hindi-belt indicator is state-level.
diagnose_hindi_belt_first_stage <- function(panel, control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  specs <- iv_hindi_belt_first_stage_specifications(control_registry = control_registry)
  data <- prepare_hindi_belt_first_stage_panel(panel, specs)
  instrument <- unlist(specs$excluded_instruments[[1L]], use.names = FALSE)
  treatment <- specs$treatment[[1L]]

  summary <- safe_bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    candidate <- specs[i, , drop = FALSE]
    baseline <- without_hindi_belt_control(candidate)
    base <- estimate_first_stage_absorption_spec(
      data, baseline, treatment, instrument, control_registry
    )$summary
    belt <- estimate_first_stage_absorption_spec(
      data, candidate, treatment, instrument, control_registry
    )$summary
    data.frame(
      specification_id = plain_chr(candidate$specification_id),
      adjustment_id = plain_chr(candidate$adjustment_id),
      fixed_effect = plain_chr(candidate$fixed_effect),
      n = belt$n,
      baseline_estimate = base$estimate,
      hindi_belt_estimate = belt$estimate,
      estimate_change = belt$estimate - base$estimate,
      baseline_excluded_instrument_f = base$excluded_instrument_f,
      hindi_belt_excluded_instrument_f = belt$excluded_instrument_f,
      excluded_instrument_f_change = belt$excluded_instrument_f - base$excluded_instrument_f,
      baseline_partial_r_squared = base$partial_r_squared,
      hindi_belt_partial_r_squared = belt$partial_r_squared,
      partial_r_squared_change = belt$partial_r_squared - base$partial_r_squared,
      status = belt$status,
      reason = belt$reason,
      stringsAsFactors = FALSE
    )
  }))
  common_support <- data.frame(
    sample_rule = specs$sample_rule[[1L]],
    n = nrow(data),
    n_states = length(unique(data$state_code_2001)),
    n_regions = length(unique(data$region)),
    n_hindi_belt = sum(data[[shastry_hindi_belt_variable()]] == 1),
    n_non_hindi_belt = sum(data[[shastry_hindi_belt_variable()]] == 0),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      summary = summary,
      state_definition = shastry_hindi_belt_state_definition(),
      common_support = common_support
    ),
    class = "emi_hindi_belt_first_stage"
  )
}

save_hindi_belt_first_stage_diagnostics <- function(
    diagnostics,
    dir = "outputs/diagnostics/extended/instrument_relevance") {
  if (!inherits(diagnostics, "emi_hindi_belt_first_stage")) {
    stop("Expected Hindi-belt first-stage diagnostics.", call. = FALSE)
  }
  output_manifest(write_diagnostic_bundle(
    list(
      comparison = diagnostics$summary,
      state_definition = diagnostics$state_definition,
      common_support = diagnostics$common_support
    ),
    directory = dir,
    filenames = c(
      comparison = "hindi_belt_first_stage_comparison.csv",
      state_definition = "hindi_belt_state_definition.csv",
      common_support = "hindi_belt_first_stage_common_support.csv"
    )
  ))
}
