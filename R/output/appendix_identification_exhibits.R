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
  req_inf <- c(
    "state_code_2001", "district_code_2001", "leverage",
    "cooks_distance", "instrument_dfbeta"
  )
  if (length(setdiff(req_inf, names(influence))) || !nrow(influence)) {
    stop("Appendix C4 requires district influence diagnostics.", call. = FALSE)
  }
  leverage <- num(influence$leverage)
  cook <- num(influence$cooks_distance)
  dfbeta <- num(influence$instrument_dfbeta)
  leverage_one <- is.finite(leverage) & abs(leverage - 1) <= sqrt(.Machine$double.eps)
  undefined_cook <- !is.finite(cook)
  unexpected_undefined_cook <- undefined_cook & !leverage_one
  if (any(unexpected_undefined_cook) || any(!is.finite(dfbeta)) || !any(is.finite(cook))) {
    stop(
      paste(
        "Appendix C4 requires finite DFBETAs and Cook's distance wherever",
        "the fitted first stage has leverage below one."
      ),
      call. = FALSE
    )
  }
  cook_valid <- which(is.finite(cook))
  cook_i <- cook_valid[[which.max(cook[cook_valid])]]
  dfb_i <- which.max(abs(dfbeta))
  n_cook_undefined <- sum(undefined_cook)
  cook_context <- sprintf(
    "State %s, district %s; %d/%d finite%s",
    plain_chr(influence$state_code_2001)[cook_i],
    plain_chr(influence$district_code_2001)[cook_i],
    length(cook_valid), nrow(influence),
    if (n_cook_undefined) sprintf(" (%d leverage=1 omitted)", n_cook_undefined) else ""
  )
  influence_rows <- data.frame(
    section = "District influence", diagnostic = c("Maximum Cook's distance", "Maximum absolute instrument DFBETA"),
    specification = "State FE + expanded controls", baseline_f = NA_real_, comparison_f = NA_real_,
    value = c(cook[cook_i], abs(dfbeta[dfb_i])),
    context = c(
      cook_context,
      sprintf(
        "State %s, district %s",
        plain_chr(influence$state_code_2001)[dfb_i],
        plain_chr(influence$district_code_2001)[dfb_i]
      )
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



appendix_c8_historical_pretrend_data <- function(pretrend_validation) {
  if (!is.list(pretrend_validation) || is.null(pretrend_validation$joint_balance)) {
    stop("Appendix C8 requires canonical Vanneman pretrend validation.", call. = FALSE)
  }
  x <- safe_df(pretrend_validation$joint_balance)
  required <- c("predictor_id", "sample_id", "period_id", "domain", "joint_f", "joint_p", "n", "status")
  if (length(setdiff(required, names(x)))) {
    stop("Appendix C8 pretrend validation lacks required joint-balance fields.", call. = FALSE)
  }
  predictors <- c("eventual_emie", "census_2001_ld", "helms_lim_ld_1991")
  periods <- c("1961_1971", "1971_1981", "1981_1991")
  domains <- c("demography", "labor", "education")
  expected <- expand.grid(
    predictor_id = predictors, period_id = periods, domain = domains,
    stringsAsFactors = FALSE
  )
  x <- x[
    plain_chr(x$sample_id) == "historical_ld_support" &
      plain_chr(x$predictor_id) %in% predictors &
      plain_chr(x$period_id) %in% periods &
      plain_chr(x$domain) %in% domains,
    , drop = FALSE
  ]
  key <- paste(x$predictor_id, x$period_id, x$domain, sep = "__")
  expected_key <- paste(expected$predictor_id, expected$period_id, expected$domain, sep = "__")
  if (anyDuplicated(key) || !setequal(key, expected_key) ||
      any(plain_chr(x$status) != "estimated") ||
      any(!is.finite(num(x$joint_f))) || any(!is.finite(num(x$joint_p)))) {
    stop("Appendix C8 requires the complete 3 x 3 x 3 common-support pretrend grid.", call. = FALSE)
  }
  x <- x[match(expected_key, key), , drop = FALSE]
  predictor_labels <- c(
    eventual_emie = "Eventual EMI exposure",
    census_2001_ld = "2001 linguistic distance",
    helms_lim_ld_1991 = "1991 Helms-Lim distance"
  )
  domain_labels <- c(demography = "Demography", labor = "Labor", education = "Education")
  x$predictor_label <- unname(predictor_labels[plain_chr(x$predictor_id)])
  x$period_label <- gsub("_", "-", plain_chr(x$period_id), fixed = TRUE)
  x$domain_label <- unname(domain_labels[plain_chr(x$domain)])
  x$cell <- paste(x$period_label, x$domain_label, sep = "\n")
  x$minus_log10_p <- -log10(pmax(num(x$joint_p), .Machine$double.xmin))
  x$p_label <- ifelse(num(x$joint_p) < .001, "<.001", sub("^0", "", sprintf("%.3f", num(x$joint_p))))
  x
}

appendix_c8_historical_pretrend_plot <- function(pretrend_validation) {
  need_pkg("ggplot2", "Appendix C historical-pretrend figure")
  d <- appendix_c8_historical_pretrend_data(pretrend_validation)
  period_domain <- unlist(lapply(
    c("1961-1971", "1971-1981", "1981-1991"),
    function(period) paste(period, c("Demography", "Labor", "Education"), sep = "\n"),
    use.names = FALSE
  ))
  d$cell <- factor(d$cell, levels = period_domain)
  d$predictor_label <- factor(
    d$predictor_label,
    levels = c("Eventual EMI exposure", "2001 linguistic distance", "1991 Helms-Lim distance")
  )
  p <- ggplot2::ggplot(d, ggplot2::aes(x = cell, y = predictor_label, fill = minus_log10_p)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.45) +
    ggplot2::geom_text(ggplot2::aes(label = p_label), size = 3) +
    ggplot2::scale_fill_viridis_c(name = expression(-log[10](p)), option = "C") +
    ggplot2::labs(
      title = "Appendix C8. Historical pretrend diagnostics",
      x = NULL, y = NULL,
      caption = paste(
        "Cells report joint-test p-values on the common historical-language-support sample;",
        "darker shading denotes stronger evidence of association with pre-1991 changes.",
        "These balance diagnostics do not establish exogeneity."
      )
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0)
    )
  attr(p, "csv_data") <- d
  p
}

appendix_c13_robustness_family_census <- function(evidence) {
  if (!is.list(evidence) || is.null(evidence$grid) || is.null(evidence$family_summary)) {
    stop("Appendix C13 requires canonical consumption robustness evidence.", call. = FALSE)
  }
  grid <- safe_df(evidence$grid)
  x <- safe_df(evidence$family_summary)
  families <- c(
    "scalar_iv", "intensive_margin", "welfare_definition", "control_strategy",
    "control_parameterization", "historical_adjustment", "historical_concept_matched"
  )
  required <- c(
    "family", "n_models", "n_strong_first_stage", "max_effective_f",
    "n_reduced_form_family_signals", "n_ar_family_signals", "n_bounded_ar_sets",
    "min_n", "max_n"
  )
  if (length(setdiff(required, names(x))) || anyDuplicated(x$family) ||
      !setequal(plain_chr(x$family), families)) {
    stop("Appendix C13 requires all seven registered robustness families exactly once.", call. = FALSE)
  }
  x <- x[match(families, plain_chr(x$family)), , drop = FALSE]
  if (sum(as.integer(x$n_models)) != nrow(grid) || any(!is.finite(num(x$max_effective_f)))) {
    stop("Appendix C13 family counts must reconcile to the realized robustness grid.", call. = FALSE)
  }
  labels <- c(
    scalar_iv = "Scalar linguistic distance",
    intensive_margin = "EMI treatment definition",
    welfare_definition = "Welfare definition",
    control_strategy = "Control strategy",
    control_parameterization = "Control parameterization",
    historical_adjustment = "Historical adjustment",
    historical_concept_matched = "Concept-matched history"
  )
  out <- data.frame(
    Family = unname(labels[plain_chr(x$family)]),
    Models = as.integer(x$n_models),
    `Strong first stage` = as.integer(x$n_strong_first_stage),
    `Max effective F` = sprintf("%.2f", num(x$max_effective_f)),
    `RF family signals` = as.integer(x$n_reduced_form_family_signals),
    `AR family signals` = as.integer(x$n_ar_family_signals),
    `Bounded AR sets` = as.integer(x$n_bounded_ar_sets),
    `N range` = paste(formatC(as.integer(x$min_n), format = "d"), formatC(as.integer(x$max_n), format = "d"), sep = "-") ,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  total <- data.frame(
    Family = "All registered families", Models = sum(as.integer(x$n_models)),
    `Strong first stage` = sum(as.integer(x$n_strong_first_stage)),
    `Max effective F` = sprintf("%.2f", max(num(x$max_effective_f))),
    `RF family signals` = sum(as.integer(x$n_reduced_form_family_signals)),
    `AR family signals` = sum(as.integer(x$n_ar_family_signals)),
    `Bounded AR sets` = sum(as.integer(x$n_bounded_ar_sets)),
    `N range` = paste(min(as.integer(x$min_n)), max(as.integer(x$max_n)), sep = "-"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  out <- rbind(out, total)
  attr(out, "csv_data") <- x
  out
}

appendix_c14_exclusion_sensitivity <- function(sensitivity) {
  if (!is.list(sensitivity) || is.null(sensitivity$summary)) {
    stop("Appendix C14 requires canonical exclusion-sensitivity output.", call. = FALSE)
  }
  x <- safe_df(sensitivity$summary)
  required <- c(
    "specification_id", "outcome_round", "estimand", "calibration_id",
    "reduced_form_estimate", "reduced_form.std.error", "exclusion_ar_p_beta0",
    "exclusion_ar_95_contains_zero", "minimum_gamma_for_zero_95",
    "minimum_gamma_share_of_reduced_form_for_zero_95", "exclusion_ar_95_information"
  )
  if (length(setdiff(required, names(x)))) {
    stop("Appendix C14 exclusion sensitivity lacks required inversion fields.", call. = FALSE)
  }
  x <- x[plain_chr(x$calibration_id) == "exact_exclusion", , drop = FALSE]
  expected <- c(
    "consumption__long_2022__ancova", "consumption__long_2022__change",
    "consumption__long_2023__ancova", "consumption__long_2023__change"
  )
  x <- x[match(expected, plain_chr(x$specification_id)), , drop = FALSE]
  if (nrow(x) != length(expected) || any(is.na(x$specification_id)) ||
      any(!is.finite(num(x$reduced_form_estimate))) ||
      any(!is.finite(num(x$minimum_gamma_for_zero_95))) ||
      any(!is.finite(num(x$minimum_gamma_share_of_reduced_form_for_zero_95)))) {
    stop("Appendix C14 requires all four registered long-run exclusion designs.", call. = FALSE)
  }
  round_labels <- c(hces_2022_23 = "2022-23", hces_2023_24 = "2023-24")
  estimand_labels <- c(ancova = "ANCOVA", change = "Long change")
  out <- data.frame(
    Outcome = unname(round_labels[plain_chr(x$outcome_round)]),
    Estimand = unname(estimand_labels[plain_chr(x$estimand)]),
    `Reduced form (SE)` = sprintf("%.3f (%.3f)", num(x$reduced_form_estimate), num(x$reduced_form.std.error)),
    `Exact AR p, beta=0` = sprintf("%.3f", num(x$exclusion_ar_p_beta0)),
    `Zero in exact 95% set` = ifelse(as.logical(x$exclusion_ar_95_contains_zero), "Yes", "No"),
    `Min direct effect for zero` = sprintf("%.3f", num(x$minimum_gamma_for_zero_95)),
    `Share of |reduced form|` = sprintf("%.1f%%", 100 * num(x$minimum_gamma_share_of_reduced_form_for_zero_95)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- x
  out
}

appendix_c10_monotonicity_plot_data <- function(diagnostics) {
  if (!inherits(diagnostics, "emi_alternative_distance_first_stages")) {
    stop("Appendix C10 requires canonical alternative-distance inference diagnostics.", call. = FALSE)
  }
  spec <- "state_main__nonzero_mean"
  bins <- safe_df(diagnostics$monotonicity_bins)
  states <- safe_df(diagnostics$monotonicity_state_slopes)
  summary <- safe_df(diagnostics$monotonicity_summary)
  bins <- bins[plain_chr(bins$specification_id) == spec, , drop = FALSE]
  states <- states[plain_chr(states$specification_id) == spec & plain_chr(states$status) == "estimated", , drop = FALSE]
  summary <- summary[plain_chr(summary$specification_id) == spec, , drop = FALSE]
  if (nrow(bins) < 5L || nrow(states) < 5L || nrow(summary) != 1L ||
      any(!is.finite(num(bins$instrument))) || any(!is.finite(num(bins$treatment))) ||
      any(!is.finite(num(states$slope)))) {
    stop("Appendix C10 requires the registered state-main monotonicity diagnostics.", call. = FALSE)
  }
  list(bins = bins, states = states, summary = summary)
}

appendix_c10_monotonicity_plot <- function(diagnostics) {
  need_pkg("ggplot2", "Appendix C monotonicity figure")
  d <- appendix_c10_monotonicity_plot_data(diagnostics)
  bins <- d$bins
  states <- d$states
  bins$panel <- "A. Residualized first-stage bins"
  bins$x <- num(bins$instrument)
  bins$y <- num(bins$treatment)
  bins$state <- NA_character_
  states$panel <- "B. State-specific slopes"
  states$x <- seq_len(nrow(states))
  states$y <- sort(num(states$slope))
  states$state <- plain_chr(states$state_code_2001)[order(num(states$slope))]
  plot_data <- safe_bind_rows(list(
    bins[, c("panel", "x", "y", "state")],
    states[, c("panel", "x", "y", "state")]
  ))
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = 2) +
    ggplot2::geom_line(
      data = plot_data[plot_data$panel == "A. Residualized first-stage bins", , drop = FALSE],
      linewidth = 0.55
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ panel, scales = "free_x", nrow = 1) +
    ggplot2::labs(
      title = "Appendix C10. Monotonicity and sign heterogeneity",
      x = NULL,
      y = "Residualized EMI / state-specific slope",
      caption = paste(
        "Panel A shows the registered decile-bin diagnostic for the preferred state-FE first stage.",
        "Panel B orders estimated state-specific slopes; states below zero indicate sign heterogeneity.",
        "These are shape diagnostics, not additional identifying assumptions."
      )
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0)
    )
  attr(p, "csv_data") <- plot_data
  p
}

appendix_c11_multiple_instruments <- function(diagnostics) {
  if (!inherits(diagnostics, "emi_alternative_distance_first_stages")) {
    stop("Appendix C11 requires canonical alternative-distance inference diagnostics.", call. = FALSE)
  }
  fas <- safe_df(diagnostics$falsification_adaptive_summary)
  ids <- paste("state_main", c("distance_shares_all", "distance_shares_all_unmapped", "distance_shares_mapped"), sep = "__")
  fas <- fas[match(ids, plain_chr(fas$specification_id)), , drop = FALSE]
  required <- c(
    "specification_id", "construction_id", "n_instruments", "fas_lower", "fas_upper",
    "fas_contains_zero", "min_conditional_first_stage_f", "n_conditional_first_stage_f_below_10",
    "constituent_relevance_caution", "sargan_status", "sargan_p.value", "n", "status"
  )
  if (length(setdiff(required, names(fas))) || nrow(fas) != length(ids) || any(is.na(fas$specification_id)) ||
      any(!plain_chr(fas$status) %in% "estimated")) {
    stop("Appendix C11 requires all registered state-main multi-instrument FAS diagnostics.", call. = FALSE)
  }
  csv <- fas[, required, drop = FALSE]
  labels <- c(
    distance_shares_all = "All distance shares",
    distance_shares_all_unmapped = "All shares + unmapped",
    distance_shares_mapped = "Mapped-speaker shares"
  )
  out <- data.frame(
    Construction = unname(labels[plain_chr(csv$construction_id)]),
    `Sargan p` = ifelse(plain_chr(csv$sargan_status) == "estimated", sprintf("%.3f", num(csv$sargan_p.value)), "n/a"),
    `FAS lower` = sprintf("%.3f", num(csv$fas_lower)),
    `FAS upper` = sprintf("%.3f", num(csv$fas_upper)),
    `Contains 0` = ifelse(as.logical(csv$fas_contains_zero), "Yes", "No"),
    `Min conditional F` = sprintf("%.3f", num(csv$min_conditional_first_stage_f)),
    `Components F<10` = as.integer(csv$n_conditional_first_stage_f_below_10),
    N = formatC(as.integer(csv$n), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_c12_consumption_iv_dynamics_data <- function(dynamics) {
  if (is.null(dynamics) || !is.list(dynamics) || is.null(dynamics$summary)) {
    stop("Appendix C12 requires canonical consumption-IV dynamics outputs.", call. = FALSE)
  }
  x <- safe_df(dynamics$summary)
  required <- c(
    "outcome_round", "estimand", "reduced_form_estimate", "reduced_form_std.error",
    "second_stage_estimate", "second_stage_std.error", "ar_95_information", "ar_95_disconnected",
    "ar_95_contains_zero", "status"
  )
  if (length(setdiff(required, names(x))) || nrow(x) != 8L || any(!plain_chr(x$status) %in% "estimated")) {
    stop("Appendix C12 requires all eight registered consumption-IV horizon/estimand rows.", call. = FALSE)
  }
  rounds <- c("nss_2009_10_type2", "nss_2011_12_type2", "hces_2022_23", "hces_2023_24")
  estimands <- c("ancova", "change")
  expected <- as.vector(outer(rounds, estimands, paste, sep = "__"))
  key <- paste(plain_chr(x$outcome_round), plain_chr(x$estimand), sep = "__")
  if (anyDuplicated(key) || !setequal(key, expected)) {
    stop("Appendix C12 requires the registered four-horizon ANCOVA/change design.", call. = FALSE)
  }
  x <- x[match(expected, key), , drop = FALSE]
  horizon <- consumption_dynamic_round_label(x$outcome_round)
  metric_rows <- function(metric, estimate, se) data.frame(
    horizon = horizon,
    estimand = plain_chr(x$estimand),
    metric = metric,
    estimate = num(x[[estimate]]),
    std.error = num(x[[se]]),
    ar_information = plain_chr(x$ar_95_information),
    ar_disconnected = as.logical(x$ar_95_disconnected),
    ar_contains_zero = as.logical(x$ar_95_contains_zero),
    stringsAsFactors = FALSE
  )
  out <- safe_bind_rows(list(
    metric_rows("Reduced form", "reduced_form_estimate", "reduced_form_std.error"),
    metric_rows("2SLS", "second_stage_estimate", "second_stage_std.error")
  ))
  if (any(!is.finite(out$estimate)) || any(!is.finite(out$std.error))) {
    stop("Appendix C12 requires finite reduced-form and 2SLS estimates.", call. = FALSE)
  }
  out$conf.low <- out$estimate - stats::qnorm(0.975) * out$std.error
  out$conf.high <- out$estimate + stats::qnorm(0.975) * out$std.error
  out
}

appendix_c12_consumption_iv_dynamics_plot <- function(dynamics) {
  need_pkg("ggplot2", "Appendix C full consumption-IV dynamics figure")
  d <- appendix_c12_consumption_iv_dynamics_data(dynamics)
  d$horizon <- factor(d$horizon, levels = c("2009-10", "2011-12", "2022-23", "2023-24"))
  d$estimand <- factor(ifelse(d$estimand == "ancova", "ANCOVA", "Long change"), levels = c("ANCOVA", "Long change"))
  d$metric <- factor(d$metric, levels = c("Reduced form", "2SLS"))
  dodge <- ggplot2::position_dodge(width = 0.35)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = horizon, y = estimate, shape = estimand, group = estimand)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, linetype = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = dodge) +
    ggplot2::geom_point(size = 2.2, position = dodge) +
    ggplot2::facet_wrap(~ metric, nrow = 2, scales = "free_y") +
    ggplot2::labs(
      title = "Appendix C12. Full consumption IV dynamics",
      x = "Outcome horizon", y = "Coefficient with 95% Wald interval", shape = NULL,
      caption = paste(
        "Reduced-form and conventional 2SLS coefficients are shown on separate scales because their units differ.",
        "Anderson-Rubin topology is retained in the machine-readable companion data; disconnected or zero-containing sets",
        "are the weak-IV-robust interpretation and take precedence over the conventional 2SLS intervals."
      )
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom", plot.caption = ggplot2::element_text(size = 8, hjust = 0)
    )
  attr(p, "csv_data") <- d
  p
}

make_appendix_identification_exhibits <- function(
    first_stage_absorption, district_panel, hindi_belt_first_stage,
    child_population_first_stage, alternative_distance_first_stage,
    alternative_distance_measurement, alternative_distance_inference,
    consumption_iv_dynamics, historical_pretrend_validation,
    consumption_robustness_evidence, consumption_exclusion_sensitivity,
    control_registry = NULL) {
  list(
    appendix_c1_full_absorption_ladder = appendix_c1_full_absorption_ladder(first_stage_absorption, control_registry),
    appendix_c2_residual_geography = appendix_c2_residual_geography_plot(district_panel),
    appendix_c3_control_block_absorption = appendix_c3_control_block_absorption(first_stage_absorption, control_registry),
    appendix_c4_geographic_scale_sensitivity = appendix_c4_geographic_scale_sensitivity(
      first_stage_absorption, hindi_belt_first_stage, child_population_first_stage
    ),
    appendix_c5_alternative_scalar_distances = appendix_c5_alternative_scalar_distances(alternative_distance_first_stage),
    appendix_c6_mapping_composition_sensitivity = appendix_c6_mapping_composition_sensitivity(alternative_distance_measurement),
    appendix_c8_historical_pretrends = appendix_c8_historical_pretrend_plot(historical_pretrend_validation),
    appendix_c10_monotonicity = appendix_c10_monotonicity_plot(alternative_distance_inference),
    appendix_c11_multiple_instruments = appendix_c11_multiple_instruments(alternative_distance_inference),
    appendix_c12_consumption_iv_dynamics = appendix_c12_consumption_iv_dynamics_plot(consumption_iv_dynamics),
    appendix_c13_robustness_family_census = appendix_c13_robustness_family_census(consumption_robustness_evidence),
    appendix_c14_exclusion_sensitivity = appendix_c14_exclusion_sensitivity(consumption_exclusion_sensitivity)
  )
}

save_appendix_identification_exhibits <- function(exhibits, cfg) {
  table_names <- c(
    "appendix_c1_full_absorption_ladder", "appendix_c3_control_block_absorption",
    "appendix_c4_geographic_scale_sensitivity", "appendix_c5_alternative_scalar_distances",
    "appendix_c6_mapping_composition_sensitivity", "appendix_c11_multiple_instruments",
    "appendix_c13_robustness_family_census", "appendix_c14_exclusion_sensitivity"
  )
  figure_names <- c(
    "appendix_c2_residual_geography", "appendix_c8_historical_pretrends",
    "appendix_c10_monotonicity", "appendix_c12_consumption_iv_dynamics"
  )
  if (!is.list(exhibits) || !all(c(table_names, figure_names) %in% names(exhibits))) {
    stop("Appendix C identification exhibit bundle is incomplete.", call. = FALSE)
  }
  written <- save_appendix_tables(exhibits, table_names, cfg)
  formats <- figure_formats(cfg)
  figure_sizes <- list(
    appendix_c2_residual_geography = c(8.2, 4.3),
    appendix_c8_historical_pretrends = c(9.0, 4.2),
    appendix_c10_monotonicity = c(8.2, 4.5),
    appendix_c12_consumption_iv_dynamics = c(7.2, 6.5)
  )
  for (name in figure_names) {
    size <- figure_sizes[[name]]
    written <- c(
      written,
      save_plot_formats(
        exhibits[[name]], appendix_figure_path_base(name), formats,
        width = size[[1]], height = size[[2]]
      )
    )
    plot_csv <- attr(exhibits[[name]], "csv_data")
    if (!is.null(plot_csv)) {
      csv_path <- paste0(appendix_figure_path_base(name), ".csv")
      dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(plot_csv, csv_path, row.names = FALSE, na = "")
      written <- c(written, csv_path)
    }
  }
  unique(normalizePath(written, mustWork = FALSE))
}
