# Compact district-level mechanism grid for the paper rescue.
#
# This reuses the canonical IV adjustment definitions and first-stage
# residual/inference machinery. It is a relevance diagnostic, not 2SLS: each
# registered district mechanism measure is treated as an outcome and related to
# the preferred linguistic-distance construction under exactly three geography
# specifications. Each outcome uses one fixed complete-case sample across those
# three specifications so changes across columns reflect adjustment rather than
# sample composition.

district_mechanism_adjustment_registry <- function(control_registry = NULL) {
  control_registry <- resolve_census_2001_control_registry(control_registry)
  canonical <- census_2001_diagnostic_controls(control_registry)
  ids <- c("unadjusted", "region_main", "state_main")
  adjustments <- iv_adjustment_sets(control_registry)[ids]
  rows <- lapply(seq_along(adjustments), function(i) {
    adjustment <- adjustments[[i]]
    data.frame(
      specification_id = ids[[i]],
      label = adjustment$label,
      fixed_effect = adjustment$fixed_effect,
      controls = I(list(order_iv_controls(adjustment$controls, canonical))),
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
    controls = census_2001_main_controls(),
    require_region = TRUE) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  geography <- c("target_unit_2001", "state_code_2001")
  if (isTRUE(require_region)) geography <- c(geography, "region")
  needed <- unique(c(outcome, instrument, geography, controls))
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
  keep <- stats::complete.cases(x[needed]) &
    nzchar(x$target_unit_2001) & nzchar(x$state_code_2001)
  if (isTRUE(require_region)) {
    x$region <- plain_chr(x$region)
    keep <- keep & nzchar(x$region)
  }
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
    instrument = "ling_distance_nonzero_mean",
    control_registry = NULL) {
  measures <- preferred_district_mechanism_registry(registry)
  if (!nrow(measures)) stop("No preferred district mechanism measures are registered.", call. = FALSE)
  adjustments <- district_mechanism_adjustment_registry(control_registry)
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

# Small descriptive heterogeneity family for the inequality narrative.
#
# The preferred estimand is the interaction between linguistic distance and
# predetermined Census-2001 Scheduled-Tribe population share. A high-ST sample
# is retained only as a threshold sensitivity. The high-ST cutoff is computed
# once from the full finite district panel so outcome missingness and the
# Hindi-belt restriction cannot redefine the subgroup after results are seen.

english_opportunity_st_heterogeneity_registry <- function() {
  data.frame(
    outcome_id = c("emi_all_children", "private_emi_all_children", "girls_toilet_share"),
    outcome = c(
      "emi_exposure_all_children_0708",
      "private_emi_exposure_all_children_0708",
      "dise_girls_toilet_school_share_0708"
    ),
    label = c(
      "All-child English-medium exposure",
      "Private English-medium exposure among all children",
      "Schools reporting girls' toilet facilities"
    ),
    source = c("nss_64_education", "nss_64_education", "dise"),
    stringsAsFactors = FALSE
  )
}

english_opportunity_st_heterogeneity_specifications <- function() {
  registry <- english_opportunity_st_heterogeneity_registry()
  grid <- merge(
    registry,
    expand.grid(
      sample = c("all_states", "hindi_belt"),
      heterogeneity = c("continuous_interaction", "high_st_subset"),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    ),
    by = NULL
  )
  grid$specification_id <- paste(
    "st_concentration", grid$outcome_id, grid$sample, grid$heterogeneity, sep = "__"
  )
  grid <- grid[c(
    "specification_id", "outcome_id", "outcome", "sample", "heterogeneity"
  )]
  if (nrow(grid) != 12L || anyDuplicated(grid$specification_id)) {
    stop("ST-concentration heterogeneity specification family must contain 12 unique cells.", call. = FALSE)
  }
  grid
}

english_opportunity_st_heterogeneity_controls <- function(control_registry = NULL) {
  setdiff(census_2001_main_controls(control_registry), "st_share_2001")
}

english_opportunity_high_st_cutoff <- function(panel, probability = 0.75) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  if (!"st_share_2001" %in% names(x)) {
    stop("English-opportunity ST heterogeneity requires st_share_2001.", call. = FALSE)
  }
  share <- num(x$st_share_2001)
  share <- share[is.finite(share)]
  if (!length(share)) {
    stop("English-opportunity ST heterogeneity has no finite ST-share observations.", call. = FALSE)
  }
  probability <- num(probability)[[1L]]
  if (!is.finite(probability) || probability <= 0 || probability >= 1) {
    stop("High-ST quantile probability must lie strictly between zero and one.", call. = FALSE)
  }
  unname(stats::quantile(share, probability, names = FALSE, type = 7))
}

prepare_english_opportunity_st_heterogeneity_sample <- function(
    panel,
    outcome,
    controls = english_opportunity_st_heterogeneity_controls(),
    instrument = preferred_iv_variables()$instrument) {
  x <- prepare_district_mechanism_sample(
    panel, outcome, instrument, unique(c(controls, "st_share_2001")),
    require_region = FALSE
  )
  x$hindi_belt_2001 <- x$state_code_2001 %in% shastry_hindi_belt_state_codes()
  x$st_share_10pp <- num(x$st_share_2001) / 10
  x
}

empty_english_opportunity_st_heterogeneity_result <- function(
    outcome_id, outcome, sample_id, heterogeneity, cutoff, n, n_states) {
  data.frame(
    outcome_id = outcome_id,
    outcome = outcome,
    sample = sample_id,
    heterogeneity = heterogeneity,
    high_st_cutoff_percent = cutoff,
    n_districts = n,
    n_states = n_states,
    term = if (heterogeneity == "continuous_interaction") {
      paste0(preferred_iv_variables()$instrument, ":st_share_10pp")
    } else {
      preferred_iv_variables()$instrument
    },
    estimate = NA_real_,
    std_error_state_clustered = NA_real_,
    p_value_state_clustered = NA_real_,
    status = "insufficient_support",
    stringsAsFactors = FALSE
  )
}

fit_english_opportunity_st_heterogeneity <- function(
    sample,
    outcome_id,
    outcome,
    sample_id = c("all_states", "hindi_belt"),
    heterogeneity = c("continuous_interaction", "high_st_subset"),
    high_st_cutoff,
    controls = english_opportunity_st_heterogeneity_controls(),
    instrument = preferred_iv_variables()$instrument) {
  sample_id <- match.arg(sample_id)
  heterogeneity <- match.arg(heterogeneity)
  x <- safe_df(sample)
  if (sample_id == "hindi_belt") x <- x[x$hindi_belt_2001 %in% TRUE, , drop = FALSE]
  if (heterogeneity == "high_st_subset") {
    x <- x[is.finite(x$st_share_2001) & x$st_share_2001 >= high_st_cutoff, , drop = FALSE]
  }
  n_states <- length(unique(x$state_code_2001))
  if (nrow(x) < 3L || n_states < 2L || !first_stage_positive_variation(x[[instrument]])) {
    return(empty_english_opportunity_st_heterogeneity_result(
      outcome_id, outcome, sample_id, heterogeneity, high_st_cutoff, nrow(x), n_states
    ))
  }

  if (heterogeneity == "continuous_interaction") {
    term <- paste0(instrument, ":st_share_10pp")
    rhs <- c(
      instrument, "st_share_10pp", term, controls,
      "factor(state_code_2001)"
    )
  } else {
    term <- instrument
    rhs <- c(
      instrument, "st_share_10pp", controls,
      "factor(state_code_2001)"
    )
  }
  fit <- stats::lm(stats::reformulate(rhs, response = outcome), data = x)
  inference <- clustered_lm_term_inference(fit, term, x$state_code_2001)
  coefficient <- stats::coef(fit)[[term]]
  data.frame(
    outcome_id = outcome_id,
    outcome = outcome,
    sample = sample_id,
    heterogeneity = heterogeneity,
    high_st_cutoff_percent = high_st_cutoff,
    n_districts = stats::nobs(fit),
    n_states = n_states,
    term = term,
    estimate = unname(coefficient),
    std_error_state_clustered = unname(inference[["std.error"]]),
    p_value_state_clustered = unname(inference[["p.value"]]),
    status = "estimated",
    stringsAsFactors = FALSE
  )
}

diagnose_english_opportunity_st_heterogeneity <- function(
    panel,
    control_registry = NULL,
    high_st_probability = 0.75) {
  registry <- english_opportunity_st_heterogeneity_registry()
  specifications <- english_opportunity_st_heterogeneity_specifications()
  controls <- english_opportunity_st_heterogeneity_controls(control_registry)
  cutoff <- english_opportunity_high_st_cutoff(panel, high_st_probability)
  samples <- lapply(seq_len(nrow(registry)), function(i) {
    prepare_english_opportunity_st_heterogeneity_sample(
      panel, registry$outcome[[i]], controls = controls
    )
  })
  names(samples) <- registry$outcome_id
  estimates <- safe_bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
    specification <- specifications[i, , drop = FALSE]
    result <- fit_english_opportunity_st_heterogeneity(
      samples[[specification$outcome_id[[1L]]]],
      specification$outcome_id[[1L]],
      specification$outcome[[1L]],
      sample_id = specification$sample[[1L]],
      heterogeneity = specification$heterogeneity[[1L]],
      high_st_cutoff = cutoff,
      controls = controls
    )
    result$specification_id <- specification$specification_id[[1L]]
    result
  }))
  if (nrow(estimates) != nrow(specifications) ||
      !setequal(estimates$specification_id, specifications$specification_id)) {
    stop("ST-concentration estimates do not match the canonical specification grid.", call. = FALSE)
  }
  estimates$p_value_holm_family <- holm_adjust_finite(estimates$p_value_state_clustered)
  list(
    registry = registry,
    specifications = specifications,
    controls = controls,
    high_st_probability = high_st_probability,
    high_st_cutoff_percent = cutoff,
    estimates = estimates
  )
}

save_english_opportunity_st_heterogeneity <- function(
    diagnostics,
    dir = "outputs/diagnostics/extended/mechanisms") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  c(
    registry = write_diagnostic_csv(
      diagnostics$registry,
      file.path(dir, "st_concentration_heterogeneity_registry.csv")
    ),
    estimates = write_diagnostic_csv(
      diagnostics$estimates,
      file.path(dir, "st_concentration_heterogeneity_estimates.csv")
    )
  )
}

english_opportunity_mechanism_geography_labels <- function() {
  c(
    unadjusted = "Unadjusted",
    region_main = "Region + controls",
    state_main = "Within state"
  )
}

english_opportunity_mechanism_figure_data <- function(
    c17_diagnostics,
    district_diagnostics,
    registry) {
  measures <- safe_df(registry)
  measures <- measures[measures$preferred %in% TRUE, , drop = FALSE]
  if (!nrow(measures)) stop("No preferred English-opportunity measures are available for the mechanism figure.", call. = FALSE)

  district <- safe_df(district_diagnostics$estimates %||% data.frame())
  district <- district[district$status == "estimated", , drop = FALSE]
  if (nrow(district)) {
    district$signal <- num(district$standardized_estimate)
    district$geography_id <- plain_chr(district$specification_id)
  }

  c17_registry <- safe_df(c17_diagnostics$registry %||% data.frame())
  c17_coefficients <- safe_df(c17_diagnostics$coefficients %||% data.frame())
  c17_summary <- safe_df(c17_diagnostics$model_summary %||% data.frame())
  c17_preferred <- c17_registry[c17_registry$preferred %in% TRUE, , drop = FALSE]
  c17_rows <- data.frame()
  if (nrow(c17_preferred) == 1L) {
    specification_id <- c17_preferred$specification_id[[1L]]
    coefficient <- c17_coefficients[
      c17_coefficients$specification_id == specification_id &
        c17_coefficients$term == "shastry_degree" &
        c17_coefficients$status == "estimated",
      , drop = FALSE
    ]
    measure <- measures[
      measures$source == "census_2001_c17" &
        measures$variable == c17_preferred$outcome[[1L]],
      , drop = FALSE
    ]
    if (nrow(coefficient) == 1L && nrow(measure) == 1L) {
      model_summary <- c17_summary[c17_summary$specification_id == specification_id, , drop = FALSE]
      c17_rows <- data.frame(
        measure_id = measure$measure_id[[1L]],
        source = measure$source[[1L]],
        stage = measure$stage[[1L]],
        source_side = measure$source_side[[1L]],
        paper_role = measure$paper_role[[1L]],
        interpretation = measure$interpretation[[1L]],
        specification_id = specification_id,
        geography_id = "state_main",
        signal = num(coefficient$signed_partial_correlation[[1L]]),
        partial_r_squared = num(coefficient$partial_r_squared[[1L]]),
        n = if (nrow(model_summary)) num(model_summary$n[[1L]]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }

  if (nrow(district)) {
    keep <- c(
      "measure_id", "source", "stage", "source_side", "paper_role",
      "interpretation", "specification_id", "geography_id", "signal",
      "partial_r_squared", "n"
    )
    district <- district[, keep, drop = FALSE]
  }
  out <- safe_bind_rows(list(c17_rows, district))
  out <- out[is.finite(num(out$signal)), , drop = FALSE]
  if (!nrow(out)) stop("No estimable English-opportunity mechanism signals are available for the figure.", call. = FALSE)

  labels <- english_opportunity_mechanism_geography_labels()
  out$geography <- unname(labels[out$geography_id])
  if (any(is.na(out$geography))) {
    stop("Mechanism figure received an unregistered geography specification.", call. = FALSE)
  }
  measure_order <- measures$measure_id
  out$measure_sequence <- match(out$measure_id, measure_order)
  out <- out[order(out$measure_sequence, match(out$geography_id, names(labels))), , drop = FALSE]
  rownames(out) <- NULL
  out
}

save_english_opportunity_mechanism_figure <- function(
    c17_diagnostics,
    district_diagnostics,
    registry,
    dir = "outputs/diagnostics/extended/mechanisms") {
  need_pkg("ggplot2", "English-opportunity mechanism figure")
  plot_data <- english_opportunity_mechanism_figure_data(
    c17_diagnostics, district_diagnostics, registry
  )
  geography_levels <- unname(english_opportunity_mechanism_geography_labels())
  plot_data$geography <- factor(plot_data$geography, levels = geography_levels)
  measure_levels <- rev(unique(plot_data$interpretation[order(plot_data$measure_sequence)]))
  plot_data$interpretation <- factor(plot_data$interpretation, levels = measure_levels)

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = signal, y = interpretation)
  ) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.35, linetype = 2) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::facet_wrap(~ geography, nrow = 1L) +
    ggplot2::coord_cartesian(xlim = c(-1, 1)) +
    ggplot2::labs(
      title = "Where linguistic-distance relevance survives",
      subtitle = "Signed partial correlations across predeclared mechanism outcomes",
      x = "Signed partial correlation with linguistic distance",
      y = NULL,
      caption = paste(
        "District rows use a fixed outcome-specific sample across columns; region/state models add predetermined Census-2001 controls.",
        "The C-17 row appears only within state and additionally conditions on native-language state share, modal-language status, and the distance-zero indicator.",
        "Rows organize evidence from different observational units; they are not a sequential mediation model."
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  save_plot_formats(
    plot,
    file.path(dir, "english_opportunity_mechanism_signal"),
    c("pdf", "png"),
    width = 11,
    height = 7.2,
    dpi = 300
  )
}

english_opportunity_mechanism_table <- function(
    c17_diagnostics,
    district_diagnostics,
    registry) {
  long <- english_opportunity_mechanism_figure_data(
    c17_diagnostics, district_diagnostics, registry
  )
  keep <- c("measure_id", "source", "stage", "interpretation", "geography_id", "signal")
  long <- long[, keep, drop = FALSE]

  id_columns <- c("measure_id", "source", "stage", "interpretation")
  wide <- stats::reshape(
    long,
    idvar = id_columns,
    timevar = "geography_id",
    direction = "wide"
  )
  desired <- names(english_opportunity_mechanism_geography_labels())
  for (geography_id in desired) {
    source_name <- paste0("signal.", geography_id)
    if (!source_name %in% names(wide)) wide[[source_name]] <- NA_real_
  }
  names(wide)[match(paste0("signal.", desired), names(wide))] <- desired

  registry_order <- preferred_district_mechanism_registry(registry)$measure_id
  c17_order <- safe_df(registry)
  c17_order <- c17_order[
    c17_order$unit == "state_language" & c17_order$preferred %in% TRUE,
    "measure_id", drop = TRUE
  ]
  measure_order <- c(c17_order, registry_order)
  wide <- wide[order(match(wide$measure_id, measure_order)), , drop = FALSE]
  wide <- wide[c(id_columns, desired)]
  names(wide)[match(desired, names(wide))] <- unname(
    english_opportunity_mechanism_geography_labels()[desired]
  )
  rownames(wide) <- NULL
  wide
}

save_english_opportunity_mechanism_table <- function(
    c17_diagnostics,
    district_diagnostics,
    registry,
    dir = "outputs/diagnostics/extended/mechanisms") {
  table <- english_opportunity_mechanism_table(
    c17_diagnostics, district_diagnostics, registry
  )
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  name <- "english_opportunity_mechanism"
  c(
    csv = save_table_csv(
      table, file.path(dir, paste0(name, ".csv")), public = TRUE
    ),
    tex = save_table_tex(
      table, file.path(dir, paste0(name, ".tex")), name, public = TRUE
    )
  )
}
