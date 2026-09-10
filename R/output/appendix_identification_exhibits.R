# Final-paper Appendix C identification exhibits.
#
# These builders summarize canonical identification diagnostics without
# re-estimating models. The appendix is intentionally a skeptical-reader
# summary rather than a dump of every registered robustness permutation.

appendix_c1_full_absorption_ladder <- function(diagnostics, control_registry = NULL) {
  if (!inherits(diagnostics, "emi_first_stage_absorption")) {
    stop("Appendix C1 requires canonical first-stage absorption diagnostics.", call. = FALSE)
  }
  x <- safe_df(diagnostics$semantic_summary)
  required <- c(
    "semantic_specification_id", "semantic_label", "semantic_fixed_effect",
    "semantic_control_blocks", "estimate", "std.error", "excluded_instrument_f",
    "partial_r_squared", "n", "status"
  )
  expected_ids <- names(iv_absorption_adjustments(control_registry))
  if (length(setdiff(required, names(x))) || !nrow(x) || anyDuplicated(x$semantic_specification_id) ||
      !setequal(plain_chr(x$semantic_specification_id), expected_ids)) {
    stop("Appendix C1 requires one row per declared semantic absorption specification.", call. = FALSE)
  }
  x <- x[match(expected_ids, plain_chr(x$semantic_specification_id)), , drop = FALSE]
  if (any(!x$status %in% "estimated") || any(!is.finite(num(x$excluded_instrument_f))) ||
      any(!is.finite(num(x$partial_r_squared)))) {
    stop("Appendix C1 requires estimable first stages for every declared semantic specification.", call. = FALSE)
  }
  csv <- x[, setdiff(required, "status"), drop = FALSE]
  out <- data.frame(
    Specification = plain_chr(csv$semantic_label),
    FE = ifelse(plain_chr(csv$semantic_fixed_effect) == "none", "None", plain_chr(csv$semantic_fixed_effect)),
    `Control blocks` = ifelse(is.na(csv$semantic_control_blocks) | !nzchar(plain_chr(csv$semantic_control_blocks)), "None", gsub(";", ", ", plain_chr(csv$semantic_control_blocks))),
    Estimate = sprintf("%.3f", num(csv$estimate)),
    SE = sprintf("%.3f", num(csv$std.error)),
    F = sprintf("%.3f", num(csv$excluded_instrument_f)),
    `Partial R2` = sprintf("%.4f", num(csv$partial_r_squared)),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_c2_within_state_residual_data <- function(panel) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  required <- c(
    "district_panel_id", "state_code_2001",
    "ling_distance_nonzero_mean", "emi_exposure_all_children_0708"
  )
  if (length(setdiff(required, names(x)))) {
    stop("Appendix C2 requires district identifiers, state, linguistic distance, and EMI exposure.", call. = FALSE)
  }
  x <- x[stats::complete.cases(x[required]) & nzchar(plain_chr(x$state_code_2001)), required, drop = FALSE]
  if (!nrow(x) || anyDuplicated(x$district_panel_id)) {
    stop("Appendix C2 requires unique complete-case districts.", call. = FALSE)
  }
  residualize <- function(variable, id, label) {
    values <- num(x[[variable]])
    residual <- values - ave(values, plain_chr(x$state_code_2001), FUN = mean)
    scale <- stats::sd(residual)
    if (!is.finite(scale) || scale <= 0) stop("Appendix C2 residual variation is degenerate.", call. = FALSE)
    data.frame(
      district_panel_id = plain_chr(x$district_panel_id),
      state_code_2001 = plain_chr(x$state_code_2001),
      measure_id = id,
      measure = label,
      residual = residual,
      residual_sd = residual / scale,
      stringsAsFactors = FALSE
    )
  }
  safe_bind_rows(list(
    residualize("ling_distance_nonzero_mean", "linguistic_distance", "Linguistic distance"),
    residualize("emi_exposure_all_children_0708", "emi_exposure", "All-child EMI exposure")
  ))
}

appendix_c2_residual_geography_plot <- function(panel) {
  need_pkg("ggplot2", "Appendix C residual-geography figure")
  need_pkg("sf", "Appendix C residual-geography figure")
  if (!inherits(panel, "sf")) stop("Appendix C2 requires sf district geometry.", call. = FALSE)
  d <- appendix_c2_within_state_residual_data(panel)
  panel_ids <- plain_chr(panel$district_panel_id)
  pieces <- lapply(unique(d$measure_id), function(id) {
    z <- d[d$measure_id == id, , drop = FALSE]
    idx <- match(z$district_panel_id, panel_ids)
    if (anyNA(idx)) stop("Appendix C2 residual districts do not match map geometry.", call. = FALSE)
    out <- panel[idx, , drop = FALSE]
    out$measure <- z$measure
    out$residual_sd <- z$residual_sd
    out
  })
  plot_data <- do.call(rbind, pieces)
  geometry_column <- attr(panel, "sf_column")
  state_geometry <- stats::aggregate(
    panel[geometry_column],
    by = list(state_code_2001 = plain_chr(panel$state_code_2001)),
    FUN = length,
    do_union = TRUE
  )
  ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = residual_sd), linewidth = 0.05) +
    ggplot2::geom_sf(data = state_geometry, fill = NA, linewidth = 0.3) +
    ggplot2::facet_wrap(~ measure, nrow = 1) +
    ggplot2::scale_fill_gradient2(midpoint = 0, name = "Within-state\nresidual (SD)") +
    ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(title = "Appendix C2. Identifying variation after state absorption") +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

appendix_c3_control_block_absorption <- function(diagnostics, control_registry = NULL) {
  if (!inherits(diagnostics, "emi_first_stage_absorption")) {
    stop("Appendix C3 requires canonical first-stage absorption diagnostics.", call. = FALSE)
  }
  x <- safe_df(diagnostics$semantic_summary)
  blocks <- names(iv_main_control_blocks(control_registry))
  fes <- c("region", "state")
  baseline_ids <- c(region = "region_fe_census_controls", state = "state_fe_census_controls")
  rows <- list()
  for (fe in fes) {
    baseline <- x[x$semantic_specification_id == baseline_ids[[fe]], , drop = FALSE]
    if (nrow(baseline) != 1L) stop("Appendix C3 lacks the main-control baseline for ", fe, " FE.", call. = FALSE)
    for (block in blocks) {
      only_id <- paste(fe, "block_only", block, sep = "_")
      without_id <- paste(fe, "main_without", block, sep = "_")
      only <- x[x$semantic_specification_id == only_id, , drop = FALSE]
      without <- x[x$semantic_specification_id == without_id, , drop = FALSE]
      if (nrow(only) != 1L || nrow(without) != 1L) {
        stop("Appendix C3 is missing registered block intervention `", block, "` under ", fe, " FE.", call. = FALSE)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        fixed_effect = fe,
        block_id = block,
        block_only_f = num(only$excluded_instrument_f)[[1L]],
        main_without_block_f = num(without$excluded_instrument_f)[[1L]],
        main_controls_f = num(baseline$excluded_instrument_f)[[1L]],
        block_only_partial_r2 = num(only$partial_r_squared)[[1L]],
        main_without_block_partial_r2 = num(without$partial_r_squared)[[1L]],
        stringsAsFactors = FALSE
      )
    }
  }
  csv <- safe_bind_rows(rows)
  if (any(!is.finite(as.matrix(csv[c("block_only_f", "main_without_block_f", "main_controls_f")])))) {
    stop("Appendix C3 requires finite registered block-intervention first stages.", call. = FALSE)
  }
  out <- data.frame(
    FE = ifelse(csv$fixed_effect == "region", "Six-region", "State"),
    `Control block` = tools::toTitleCase(gsub("_", " ", csv$block_id)),
    `Block only F` = sprintf("%.3f", csv$block_only_f),
    `Main without block F` = sprintf("%.3f", csv$main_without_block_f),
    `Main-controls F` = sprintf("%.3f", csv$main_controls_f),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_c4_geographic_scale_sensitivity <- function(absorption, hindi_belt, child_population) {
  if (!inherits(absorption, "emi_first_stage_absorption") ||
      !inherits(hindi_belt, "emi_hindi_belt_first_stage") ||
      !inherits(child_population, "emi_child_population_first_stage")) {
    stop("Appendix C4 requires canonical absorption, Hindi-belt, and child-population diagnostics.", call. = FALSE)
  }
  h <- safe_df(hindi_belt$summary)
  cp <- safe_df(child_population$summary)
  required_h <- c("adjustment_id", "baseline_excluded_instrument_f", "hindi_belt_excluded_instrument_f", "n", "status")
  required_cp <- c("adjustment_id", "baseline_excluded_instrument_f", "augmented_excluded_instrument_f", "n", "status")
  if (length(setdiff(required_h, names(h))) || length(setdiff(required_cp, names(cp))) ||
      nrow(h) != 2L || nrow(cp) != 2L || any(!h$status %in% "estimated") || any(!cp$status %in% "estimated")) {
    stop("Appendix C4 requires both registered added-control comparisons.", call. = FALSE)
  }
  added <- safe_bind_rows(list(
    data.frame(
      section = "Added controls", diagnostic = "Hindi-belt indicator",
      specification = plain_chr(h$adjustment_id), baseline_f = num(h$baseline_excluded_instrument_f),
      comparison_f = num(h$hindi_belt_excluded_instrument_f), value = num(h$hindi_belt_excluded_instrument_f),
      context = sprintf("N = %d", as.integer(h$n)), stringsAsFactors = FALSE
    ),
    data.frame(
      section = "Added controls", diagnostic = "Child population",
      specification = plain_chr(cp$adjustment_id), baseline_f = num(cp$baseline_excluded_instrument_f),
      comparison_f = num(cp$augmented_excluded_instrument_f), value = num(cp$augmented_excluded_instrument_f),
      context = sprintf("N = %d", as.integer(cp$n)), stringsAsFactors = FALSE
    )
  ))

  deletion <- safe_df(absorption$state_deletion)
  if (!all(c("omitted_state", "excluded_instrument_f") %in% names(deletion)) || !nrow(deletion)) {
    stop("Appendix C4 requires leave-one-state-out diagnostics.", call. = FALSE)
  }
  deletion_f <- num(deletion$excluded_instrument_f)
  if (any(!is.finite(deletion_f))) {
    stop("Appendix C4 requires finite leave-one-state-out first-stage diagnostics.", call. = FALSE)
  }
  min_i <- which.min(deletion_f); max_i <- which.max(deletion_f)
  deletion_rows <- data.frame(
    section = "Leave-one-state-out", diagnostic = c("Minimum F", "Maximum F"),
    specification = "State FE + expanded controls", baseline_f = NA_real_, comparison_f = NA_real_,
    value = deletion_f[c(min_i, max_i)],
    context = paste("Omit state", plain_chr(deletion$omitted_state[c(min_i, max_i)])),
    stringsAsFactors = FALSE
  )

  influence <- safe_df(absorption$district_influence)
  req_inf <- c("state_code_2001", "district_code_2001", "cooks_distance", "instrument_dfbeta")
  if (length(setdiff(req_inf, names(influence))) || !nrow(influence)) {
    stop("Appendix C4 requires district influence diagnostics.", call. = FALSE)
  }
  cook <- num(influence$cooks_distance)
  dfbeta <- num(influence$instrument_dfbeta)
  if (any(!is.finite(cook)) || any(!is.finite(dfbeta))) {
    stop("Appendix C4 requires finite district-influence diagnostics.", call. = FALSE)
  }
  cook_i <- which.max(cook); dfb_i <- which.max(abs(dfbeta))
  influence_rows <- data.frame(
    section = "District influence", diagnostic = c("Maximum Cook's distance", "Maximum absolute instrument DFBETA"),
    specification = "State FE + expanded controls", baseline_f = NA_real_, comparison_f = NA_real_,
    value = c(cook[cook_i], abs(dfbeta[dfb_i])),
    context = c(
      sprintf("State %s, district %s", plain_chr(influence$state_code_2001)[cook_i], plain_chr(influence$district_code_2001)[cook_i]),
      sprintf("State %s, district %s", plain_chr(influence$state_code_2001)[dfb_i], plain_chr(influence$district_code_2001)[dfb_i])
    ), stringsAsFactors = FALSE
  )
  csv <- safe_bind_rows(list(added, deletion_rows, influence_rows))
  out <- data.frame(
    Section = csv$section, Diagnostic = csv$diagnostic,
    Specification = gsub("__", " / ", csv$specification, fixed = TRUE),
    `Baseline F` = ifelse(is.finite(csv$baseline_f), sprintf("%.3f", csv$baseline_f), ""),
    `Comparison/statistic` = sprintf("%.3f", csv$value), Context = csv$context,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_c5_alternative_scalar_distances <- function(diagnostics) {
  if (!inherits(diagnostics, "emi_alternative_distance_first_stages")) {
    stop("Appendix C5 requires canonical alternative-distance first stages.", call. = FALSE)
  }
  x <- safe_df(diagnostics$summary)
  constructions <- c("nonzero_mean", "top3_legacy", "distant_share", "glottolog_mean", "dyen_noncognate")
  adjustments <- c("unadjusted", "region_main", "state_main")
  target <- expand.grid(adjustment_id = adjustments, construction_id = constructions, stringsAsFactors = FALSE)
  target$specification_id <- paste(target$adjustment_id, target$construction_id, sep = "__")
  x <- x[match(target$specification_id, x$specification_id), , drop = FALSE]
  if (nrow(x) != nrow(target) || any(is.na(x$specification_id)) ||
      any(!is.finite(num(x$joint_excluded_f))) || any(!is.finite(num(x$partial_r_squared)))) {
    stop("Appendix C5 requires every registered scalar-distance comparison on common support.", call. = FALSE)
  }
  csv <- x[, c("specification_id", "adjustment_id", "adjustment", "construction_id", "construction", "joint_excluded_f", "joint_excluded_p", "partial_r_squared", "n"), drop = FALSE]
  labels <- c(unadjusted = "Unadjusted", region_main = "Six-region + controls", state_main = "State + controls")
  out <- data.frame(
    `Distance construction` = plain_chr(csv$construction),
    Adjustment = unname(labels[plain_chr(csv$adjustment_id)]),
    F = sprintf("%.3f", num(csv$joint_excluded_f)),
    `Partial R2` = sprintf("%.4f", num(csv$partial_r_squared)),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_c6_mapping_composition_sensitivity <- function(diagnostics) {
  if (!inherits(diagnostics, "emi_alternative_distance_first_stages")) {
    stop("Appendix C6 requires canonical alternative-distance measurement diagnostics.", call. = FALSE)
  }
  summary <- safe_df(diagnostics$summary)
  coverage <- safe_df(diagnostics$coverage_sensitivity)
  leave_one <- safe_df(diagnostics$distance4_leave_one_out)
  distance4 <- safe_df(diagnostics$distance4_languages)
  thresholds <- linguistic_mapping_coverage_thresholds()
  cov_rows <- coverage[
    coverage$specification_id == "state_main__nonzero_mean" & coverage$minimum_mapped_share %in% thresholds,
    , drop = FALSE
  ]
  cov_rows <- cov_rows[match(thresholds, cov_rows$minimum_mapped_share), , drop = FALSE]
  if (nrow(cov_rows) != length(thresholds) || any(is.na(cov_rows$minimum_mapped_share)) ||
      any(!is.finite(num(cov_rows$joint_excluded_f))) || any(!is.finite(num(cov_rows$partial_r_squared)))) {
    stop("Appendix C6 requires all registered mapping-coverage thresholds.", call. = FALSE)
  }
  coverage_out <- data.frame(
    section = "Mapping coverage", diagnostic = paste0("Mapped-speaker share >= ", thresholds, "%"),
    statistic = "Excluded-instrument F", value = num(cov_rows$joint_excluded_f),
    partial_r_squared = num(cov_rows$partial_r_squared), n = as.integer(cov_rows$n),
    stringsAsFactors = FALSE
  )

  if (!all(c("omitted_distance4_language", "joint_excluded_f", "partial_r_squared", "n") %in% names(leave_one)) || !nrow(leave_one) ||
      any(!is.finite(num(leave_one$joint_excluded_f))) || any(!is.finite(num(leave_one$partial_r_squared)))) {
    stop("Appendix C6 requires distance-4 leave-one-language-out diagnostics.", call. = FALSE)
  }
  leave_out <- data.frame(
    section = "Distance-4 leave-one-out",
    diagnostic = paste("Omit", plain_chr(leave_one$omitted_distance4_language)),
    statistic = "Excluded-instrument F", value = num(leave_one$joint_excluded_f),
    partial_r_squared = num(leave_one$partial_r_squared), n = as.integer(leave_one$n),
    stringsAsFactors = FALSE
  )

  kashmiri <- distance4[plain_chr(distance4$mother_tongue) == "Kashmiri", , drop = FALSE]
  if (nrow(kashmiri) < 1L || !"speaker_share_of_distance4" %in% names(kashmiri) ||
      any(!is.finite(num(kashmiri$speaker_share_of_distance4)))) {
    stop("Appendix C6 requires the registered Kashmiri distance-4 decomposition.", call. = FALSE)
  }
  composition <- data.frame(
    section = "Distance-4 composition", diagnostic = "Kashmiri share of distance-4 speakers",
    statistic = "Speaker share (%)", value = sum(num(kashmiri$speaker_share_of_distance4)),
    partial_r_squared = NA_real_, n = NA_integer_, stringsAsFactors = FALSE
  )

  composition_ids <- c(
    "nonzero_mean_hindi_urdu", "nonzero_mean_hindi_urdu_separate",
    "nonzero_mean_sensitivity_low", "nonzero_mean_sensitivity_high",
    "distance_shares_all", "distance_shares_all_unmapped", "distance_shares_mapped"
  )
  ids <- paste("state_main", composition_ids, sep = "__")
  comp <- summary[match(ids, summary$specification_id), , drop = FALSE]
  if (nrow(comp) != length(ids) || any(is.na(comp$specification_id)) ||
      any(!is.finite(num(comp$joint_excluded_f))) || any(!is.finite(num(comp$partial_r_squared)))) {
    stop("Appendix C6 requires the registered language-composition and rich-vector specifications.", call. = FALSE)
  }
  model_rows <- data.frame(
    section = "Language composition / richer vectors", diagnostic = plain_chr(comp$construction),
    statistic = "Excluded-instrument F", value = num(comp$joint_excluded_f),
    partial_r_squared = num(comp$partial_r_squared), n = as.integer(comp$n),
    stringsAsFactors = FALSE
  )
  csv <- safe_bind_rows(list(coverage_out, composition, leave_out, model_rows))
  out <- data.frame(
    Section = csv$section, Diagnostic = csv$diagnostic, Statistic = csv$statistic,
    Value = sprintf("%.3f", csv$value),
    `Partial R2` = ifelse(is.finite(csv$partial_r_squared), sprintf("%.4f", csv$partial_r_squared), ""),
    N = ifelse(is.finite(csv$n), formatC(as.integer(csv$n), format = "d", big.mark = ","), ""),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_identification_exhibits <- function(
    first_stage_absorption, district_panel, hindi_belt_first_stage,
    child_population_first_stage, alternative_distance_first_stage,
    alternative_distance_measurement, control_registry = NULL) {
  list(
    appendix_c1_full_absorption_ladder = appendix_c1_full_absorption_ladder(first_stage_absorption, control_registry),
    appendix_c2_residual_geography = appendix_c2_residual_geography_plot(district_panel),
    appendix_c3_control_block_absorption = appendix_c3_control_block_absorption(first_stage_absorption, control_registry),
    appendix_c4_geographic_scale_sensitivity = appendix_c4_geographic_scale_sensitivity(
      first_stage_absorption, hindi_belt_first_stage, child_population_first_stage
    ),
    appendix_c5_alternative_scalar_distances = appendix_c5_alternative_scalar_distances(alternative_distance_first_stage),
    appendix_c6_mapping_composition_sensitivity = appendix_c6_mapping_composition_sensitivity(alternative_distance_measurement)
  )
}

save_appendix_identification_exhibits <- function(exhibits, cfg) {
  table_names <- c(
    "appendix_c1_full_absorption_ladder", "appendix_c3_control_block_absorption",
    "appendix_c4_geographic_scale_sensitivity", "appendix_c5_alternative_scalar_distances",
    "appendix_c6_mapping_composition_sensitivity"
  )
  figure_names <- "appendix_c2_residual_geography"
  if (!is.list(exhibits) || !all(c(table_names, figure_names) %in% names(exhibits))) {
    stop("Appendix C identification exhibit bundle is incomplete.", call. = FALSE)
  }
  written <- save_appendix_tables(exhibits, table_names, cfg)
  formats <- figure_formats(cfg)
  written <- c(
    written,
    save_plot_formats(
      exhibits[[figure_names]], appendix_figure_path_base(figure_names), formats,
      width = 8.2, height = 4.3
    )
  )
  unique(normalizePath(written, mustWork = FALSE))
}
