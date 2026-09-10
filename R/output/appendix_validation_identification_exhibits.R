# Final-paper Appendix B validation and Appendix C historical-identification exhibits.
#
# These builders are presentation-only. They consume canonical analytical objects
# and fail closed when registered validation or identification evidence is absent.

appendix_b1_lineage_readiness <- function(district_lineage) {
  if (!is.list(district_lineage)) stop("Appendix B1 requires canonical district-lineage output.", call. = FALSE)
  summary <- safe_df(district_lineage$summary)
  readiness <- safe_df(district_lineage$readiness)
  blockers <- safe_df(district_lineage$blockers)
  variants <- safe_df(district_lineage$panel_variant_summary)
  if (!all(c("metric", "value") %in% names(summary)) || !all(c("gate", "passed") %in% names(readiness)) ||
      !all(c("panel_variant", "two_wave_target_districts") %in% names(variants))) {
    stop("Appendix B1 requires lineage summary, readiness, and panel-variant metadata.", call. = FALSE)
  }
  metric_value <- function(metric) {
    row <- summary[summary$metric == metric, , drop = FALSE]
    if (nrow(row) != 1L) stop("Appendix B1 lineage summary is missing metric `", metric, "`.", call. = FALSE)
    num(row$value)[[1L]]
  }
  variant_value <- function(id) {
    row <- variants[variants$panel_variant == id, , drop = FALSE]
    if (nrow(row) != 1L) stop("Appendix B1 lineage variants are missing `", id, "`.", call. = FALSE)
    num(row$two_wave_target_districts)[[1L]]
  }
  if (any(!readiness$passed %in% TRUE)) stop("Appendix B1 may only publish after all lineage readiness gates pass.", call. = FALSE)
  csv <- data.frame(
    category = c(rep("Lineage readiness", 4L), rep("Two-wave panel membership", 3L)),
    metric = c(
      "Census-2001 reference districts", "Accepted source identities", "Unresolved blockers", "Readiness gates passed",
      "Conservative", "Primary", "Full reviewed"
    ),
    value = c(
      metric_value("admin_units_2001"), metric_value("accepted_source_matches"), nrow(blockers), nrow(readiness),
      variant_value("conservative"), variant_value("primary"), variant_value("full_reviewed")
    ),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Category = csv$category, Metric = csv$metric,
    Value = formatC(as.integer(csv$value), format = "d", big.mark = ","),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_b2_lineage_sensitivity <- function(review) {
  if (!is.list(review)) stop("Appendix B2 requires canonical lineage panel-variant review output.", call. = FALSE)
  panels <- safe_df(review$panel_summary)
  first_stage <- safe_df(review$first_stage)
  variants <- c("conservative", "primary", "full_reviewed")
  required_panels <- c("panel_variant", "unique_districts", "complete_iv_rows")
  required_first_stage <- c("panel_variant", "model", "term", "partial_f", "effective_f", "nobs", "status")
  if (length(setdiff(required_panels, names(panels))) || length(setdiff(required_first_stage, names(first_stage)))) {
    stop("Appendix B2 requires panel membership and first-stage summaries for all lineage variants.", call. = FALSE)
  }
  panels <- panels[match(variants, panels$panel_variant), required_panels, drop = FALSE]
  fs <- first_stage[
    first_stage$model == "consumption" &
      first_stage$term == "ling_distance_nonzero_mean" &
      first_stage$panel_variant %in% variants,
    required_first_stage, drop = FALSE
  ]
  fs <- fs[match(variants, fs$panel_variant), , drop = FALSE]
  if (nrow(panels) != 3L || nrow(fs) != 3L || any(is.na(panels$panel_variant)) || any(is.na(fs$panel_variant)) ||
      any(!fs$status %in% "estimated") || any(!is.finite(num(fs$partial_f))) || any(!is.finite(num(fs$effective_f)))) {
    stop("Appendix B2 requires all three registered lineage variants with estimated first stages.", call. = FALSE)
  }
  if (any(as.integer(panels$unique_districts) != as.integer(fs$nobs))) {
    stop("Appendix B2 panel membership and first-stage support must agree by lineage variant.", call. = FALSE)
  }
  labels <- c(
    conservative = "Conservative",
    primary = "Primary",
    full_reviewed = "Full reviewed"
  )
  csv <- data.frame(
    panel_variant = variants,
    n_districts = as.integer(panels$unique_districts),
    complete_iv_rows = as.integer(panels$complete_iv_rows),
    excluded_instrument_f = num(fs$partial_f),
    effective_f = num(fs$effective_f),
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    Variant = unname(labels[csv$panel_variant]),
    Districts = formatC(csv$n_districts, format = "d", big.mark = ","),
    `Complete IV rows` = formatC(csv$complete_iv_rows, format = "d", big.mark = ","),
    `Excluded-instrument F` = sprintf("%.3f", csv$excluded_instrument_f),
    `MOP effective F` = sprintf("%.3f", csv$effective_f),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_b3_consumption_reconstruction <- function(validation) {
  x <- safe_df(validation)
  required <- c("survey_id", "sector", "mpce_definition", "expected_mpce", "estimate_mpce", "abs_difference", "passed")
  missing <- setdiff(required, names(x))
  if (length(missing) || nrow(x) != 18L) {
    stop("Appendix B3 requires all 18 registered national consumption benchmark cells.", call. = FALSE)
  }
  if (any(!x$passed %in% TRUE)) stop("Appendix B3 may only publish passed consumption benchmarks.", call. = FALSE)
  csv <- x[, required, drop = FALSE]
  out <- data.frame(
    Survey = plain_chr(csv$survey_id), Sector = plain_chr(csv$sector),
    Recall = plain_chr(csv$mpce_definition),
    Official = sprintf("%.2f", num(csv$expected_mpce)),
    Reconstructed = sprintf("%.2f", num(csv$estimate_mpce)),
    `Absolute error` = sprintf("%.2f", num(csv$abs_difference)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_b4_hces_consistency_data <- function(welfare) {
  x <- safe_df(welfare)
  required <- c("district_2001", "round_id", "outcome_id", "estimate", "preferred_eligible")
  if (length(setdiff(required, names(x)))) stop("Appendix B4 requires canonical district welfare outputs.", call. = FALSE)
  outcomes <- c("real_mean_mpce", "mean_log_real_mpce", "weighted_median_real_mpce")
  x <- x[x$round_id %in% c("hces_2022_23", "hces_2023_24") & x$outcome_id %in% outcomes & x$preferred_eligible %in% TRUE, required, drop = FALSE]
  if (anyDuplicated(x[c("district_2001", "round_id", "outcome_id")])) {
    stop("Appendix B4 requires unique district-round-outcome welfare estimates.", call. = FALSE)
  }
  rows <- lapply(outcomes, function(outcome) {
    z <- x[x$outcome_id == outcome, , drop = FALSE]
    a <- z[z$round_id == "hces_2022_23", c("district_2001", "estimate"), drop = FALSE]
    b <- z[z$round_id == "hces_2023_24", c("district_2001", "estimate"), drop = FALSE]
    names(a)[2] <- "estimate_2022_23"; names(b)[2] <- "estimate_2023_24"
    m <- merge(a, b, by = "district_2001", all = FALSE, sort = FALSE)
    if (nrow(m) < 2L) stop("Appendix B4 requires common eligible districts in both HCES rounds.", call. = FALSE)
    m$outcome_id <- outcome
    m$pearson <- stats::cor(num(m$estimate_2022_23), num(m$estimate_2023_24))
    m
  })
  safe_bind_rows(rows)
}

appendix_b4_hces_consistency_summary <- function(welfare) {
  d <- appendix_b4_hces_consistency_data(welfare)
  labels <- c(
    real_mean_mpce = "Real mean MPCE",
    mean_log_real_mpce = "Mean log real MPCE",
    weighted_median_real_mpce = "Weighted median real MPCE"
  )
  rows <- safe_bind_rows(lapply(names(labels), function(id) {
    x <- d[d$outcome_id == id, , drop = FALSE]
    data.frame(
      outcome_id = id,
      outcome = unname(labels[[id]]),
      n = nrow(x),
      pearson = stats::cor(num(x$estimate_2022_23), num(x$estimate_2023_24)),
      stringsAsFactors = FALSE
    )
  }))
  out <- data.frame(
    Outcome = rows$outcome, N = formatC(rows$n, format = "d", big.mark = ","),
    `Pearson correlation` = sprintf("%.3f", rows$pearson),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- rows
  out
}

appendix_b4_hces_consistency_plot <- function(welfare) {
  need_pkg("ggplot2", "Appendix B HCES consistency figure")
  d <- appendix_b4_hces_consistency_data(welfare)
  summary <- attr(appendix_b4_hces_consistency_summary(welfare), "csv_data")
  facet_labels <- setNames(
    sprintf("%s\nN = %d; r = %.3f", summary$outcome, summary$n, summary$pearson),
    summary$outcome_id
  )
  d$outcome <- unname(facet_labels[d$outcome_id])
  ggplot2::ggplot(d, ggplot2::aes(x = estimate_2022_23, y = estimate_2023_24)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.35, linetype = 2) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::facet_wrap(~ outcome, scales = "free", nrow = 1) +
    ggplot2::labs(title = "Appendix B4. HCES cross-round district consistency", x = "2022-23 estimate", y = "2023-24 estimate") +
    ggplot2::theme_minimal(base_size = 10)
}

appendix_b5_historical_persistence_data <- function(persistence) {
  if (!is.list(persistence)) stop("Appendix B5 requires canonical historical persistence output.", call. = FALSE)
  p <- safe_df(persistence$panel)
  required <- c("state_code_2001", "district_code_2001", "persistence_status", "ling_distance_nonzero_mean_1991", "ling_distance_nonzero_mean_2001")
  if (length(setdiff(required, names(p)))) stop("Appendix B5 historical panel lacks required fields.", call. = FALSE)
  p <- p[p$persistence_status == "eligible", required, drop = FALSE]
  p <- p[stats::complete.cases(p[c("ling_distance_nonzero_mean_1991", "ling_distance_nonzero_mean_2001")]), , drop = FALSE]
  if (nrow(p) < 2L) stop("Appendix B5 requires at least two eligible historical districts.", call. = FALSE)
  if (anyDuplicated(p[c("state_code_2001", "district_code_2001")])) stop("Appendix B5 requires unique eligible historical districts.", call. = FALSE)
  p
}

appendix_b5_historical_persistence_plot <- function(persistence) {
  need_pkg("ggplot2", "Appendix B historical persistence figure")
  d <- appendix_b5_historical_persistence_data(persistence)
  ggplot2::ggplot(d, ggplot2::aes(x = ling_distance_nonzero_mean_1991, y = ling_distance_nonzero_mean_2001)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.35, linetype = 2) +
    ggplot2::geom_point(alpha = 0.65, size = 1.4) +
    ggplot2::labs(title = "Appendix B5. Historical linguistic-distance persistence", x = "1991 linguistic distance", y = "2001 linguistic distance") +
    ggplot2::theme_minimal(base_size = 10)
}

appendix_b6_language_source_validation <- function(primary_validation, helms_lim, persistence, district_panel) {
  if (!is.list(primary_validation) || !is.list(helms_lim) || !is.list(persistence)) stop("Appendix B6 requires canonical language-validation objects.", call. = FALSE)
  primary <- safe_df(primary_validation$comparison_summary)
  helms <- safe_df(helms_lim$summary)
  persist <- safe_df(persistence$summary)
  exact <- primary[primary$exact_required %in% TRUE, , drop = FALSE]
  pref <- persist[persist$sample == "preferred_geography" & persist$measure_id == "nonzero_mean", , drop = FALSE]
  if (!nrow(exact) || nrow(helms) != 1L || nrow(pref) != 1L || any(exact$exact_matches != exact$compared_districts)) {
    stop("Appendix B6 language-source validation is incomplete.", call. = FALSE)
  }
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else safe_df(district_panel)
  coverage_registry <- data.frame(
    validation = c("Shastry 2001 district coverage", "Glottolog 2001 district coverage", "Dyen 2001 district coverage"),
    distance = c("ling_distance_nonzero_mean", "ling_distance_glottolog_nonhindi_mean", "ling_distance_dyen_noncognate_pct"),
    mapped_share = c("ling_mapped_speaker_share", "ling_glottolog_mapped_speaker_share", "ling_dyen_mapped_speaker_share"),
    stringsAsFactors = FALSE
  )
  missing <- setdiff(c(coverage_registry$distance, coverage_registry$mapped_share), names(panel))
  if (length(missing)) stop("Appendix B6 district panel lacks language-coverage fields: ", paste(missing, collapse = ", "), call. = FALSE)
  coverage <- safe_bind_rows(lapply(seq_len(nrow(coverage_registry)), function(i) {
    distance <- num(panel[[coverage_registry$distance[[i]]]])
    mapped <- num(panel[[coverage_registry$mapped_share[[i]]]])
    data.frame(
      validation = coverage_registry$validation[[i]],
      n = sum(is.finite(distance)), metric = "District coverage share",
      value = mean(is.finite(distance)), secondary_metric = "Mean mapped-speaker share",
      secondary_value = mean(mapped[is.finite(distance) & is.finite(mapped)]), stringsAsFactors = FALSE
    )
  }))
  if (any(!is.finite(coverage$value)) || any(!is.finite(coverage$secondary_value))) stop("Appendix B6 language coverage contains non-finite metrics.", call. = FALSE)
  other <- data.frame(
    validation = c("Official Census 1991 source checks", "Helms-Lim vs project 1991 distance", "Project 1991 vs 2001 distance"),
    n = c(sum(exact$compared_districts), helms$n_atlas_preferred_overlap[[1]], pref$n_districts[[1]]),
    metric = c("Exact-match share", "Pearson correlation", "Pearson correlation"),
    value = c(sum(exact$exact_matches) / sum(exact$compared_districts), helms$pearson_correlation[[1]], pref$pearson[[1]]),
    secondary_metric = c("Required concepts", "Median absolute difference", "Population-weighted Pearson"),
    secondary_value = c(nrow(exact), helms$median_absolute_difference[[1]], pref$population_weighted_pearson[[1]]),
    stringsAsFactors = FALSE
  )
  csv <- safe_bind_rows(list(coverage, other))
  fmt_secondary <- ifelse(csv$secondary_metric == "Required concepts", formatC(as.integer(csv$secondary_value), format = "d"), sprintf("%.3f", csv$secondary_value))
  out <- data.frame(
    Validation = csv$validation, N = formatC(csv$n, format = "d", big.mark = ","), Metric = csv$metric,
    Value = sprintf("%.3f", csv$value), `Secondary check` = csv$secondary_metric,
    `Secondary value` = fmt_secondary, check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_b7_nss_dise_data <- function(panel, validation) {
  x <- if (inherits(panel, "sf")) sf::st_drop_geometry(panel) else safe_df(panel)
  v <- safe_df(validation)
  required <- c("state_code_2001", "dise_emi_enrollment_share_total_0708", "emi_share_enrolled_0708")
  if (length(setdiff(required, names(x)))) stop("Appendix B7 requires DISE and NSS measures on the canonical panel.", call. = FALSE)
  row <- v[v$comparison == "enrolled_total_denominator" & v$status == "estimated", , drop = FALSE]
  if (nrow(row) != 1L) stop("Appendix B7 requires the registered enrolled-total DISE-NSS validation.", call. = FALSE)
  d <- data.frame(state = plain_chr(x$state_code_2001), dise = num(x$dise_emi_enrollment_share_total_0708), nss = num(x$emi_share_enrolled_0708), stringsAsFactors = FALSE)
  d <- d[stats::complete.cases(d) & nzchar(d$state), , drop = FALSE]
  if (nrow(d) != as.integer(row$n[[1]])) stop("Appendix B7 panel support differs from registered DISE-NSS validation.", call. = FALSE)
  d$dise_residual <- d$dise - ave(d$dise, d$state, FUN = mean)
  d$nss_residual <- d$nss - ave(d$nss, d$state, FUN = mean)
  d
}

appendix_b7_nss_dise_plot <- function(panel, validation) {
  need_pkg("ggplot2", "Appendix B NSS-DISE validation figure")
  d <- appendix_b7_nss_dise_data(panel, validation)
  v <- safe_df(validation)
  row <- v[v$comparison == "enrolled_total_denominator" & v$status == "estimated", , drop = FALSE]
  raw_label <- sprintf("Raw district levels\nr = %.3f", num(row$pearson)[[1]])
  residual_label <- sprintf("Residualized by state\nr = %.3f", num(row$state_residual_pearson)[[1]])
  raw <- data.frame(panel = raw_label, x = d$nss, y = d$dise)
  residual <- data.frame(panel = residual_label, x = d$nss_residual, y = d$dise_residual)
  plot_data <- safe_bind_rows(list(raw, residual))
  ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = 0.35, linetype = 2) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::facet_wrap(~ panel, scales = "free", nrow = 1) +
    ggplot2::labs(title = "Appendix B7. NSS-DISE EMI agreement", x = "NSS EMI among enrolled", y = "DISE EMI enrollment share") +
    ggplot2::theme_minimal(base_size = 10)
}

appendix_b8_exact_reconciliation_row <- function(x, domain, period, reconciliation) {
  x <- safe_df(x)
  diff_column <- intersect(c("max_abs_total_difference", "max_abs_difference"), names(x))
  if (length(diff_column) != 1L || !nrow(x)) {
    stop("Appendix B8 exact reconciliation lacks a registered discrepancy field.", call. = FALSE)
  }
  discrepancy <- num(x[[diff_column]])
  if (any(!is.finite(discrepancy)) || max(abs(discrepancy)) > 1e-8) {
    stop("Appendix B8 may only publish exact Census universe reconciliations.", call. = FALSE)
  }
  if ("n_reference_districts" %in% names(x)) {
    refs <- unique(as.integer(x$n_reference_districts))
    overlaps <- as.integer(x$n_overlap_districts)
    if (length(refs) != 1L || any(!is.finite(overlaps))) {
      stop("Appendix B8 housing reconciliation has inconsistent support metadata.", call. = FALSE)
    }
    support <- if (min(overlaps) == max(overlaps)) {
      sprintf("%d / %d districts", min(overlaps), refs[[1L]])
    } else {
      sprintf("%d-%d / %d districts", min(overlaps), max(overlaps), refs[[1L]])
    }
    n_reference <- refs[[1L]]
    n_overlap_min <- min(overlaps)
    n_overlap_max <- max(overlaps)
  } else if ("n_districts" %in% names(x)) {
    ns <- unique(as.integer(x$n_districts))
    if (length(ns) != 1L || !is.finite(ns[[1L]])) stop("Appendix B8 reconciliation has inconsistent district counts.", call. = FALSE)
    support <- sprintf("%d districts", ns[[1L]])
    n_reference <- ns[[1L]]
    n_overlap_min <- ns[[1L]]
    n_overlap_max <- ns[[1L]]
  } else {
    stop("Appendix B8 reconciliation lacks district-support metadata.", call. = FALSE)
  }
  data.frame(
    domain = domain, period = period, reconciliation = reconciliation,
    support = support, diagnostic = "Max absolute count difference",
    value = max(abs(discrepancy)), n_reference = n_reference,
    n_overlap_min = n_overlap_min, n_overlap_max = n_overlap_max,
    stringsAsFactors = FALSE
  )
}

appendix_b8_census_universe_reconciliation <- function(
    migration, housing, households, workers) {
  if (!is.list(migration) || !is.list(housing) || !is.list(households) || !is.list(workers)) {
    stop("Appendix B8 requires canonical Census migration, housing, household, and worker diagnostics.", call. = FALSE)
  }
  required <- list(
    migration = c("d02_d03_2011_total_validation", "d03_d07_2011_recent_work_validation", "d02_population_2011_validation"),
    housing = c("source_validation_2001", "source_validation_2011"),
    households = c("source_validation_2001", "source_validation_2011"),
    workers = c("b25_b26_2001_main_occupation_validation", "b04_b25a_universe_validation", "b06_b25b_universe_validation")
  )
  objects <- list(migration = migration, housing = housing, households = households, workers = workers)
  for (name in names(required)) {
    if (!all(required[[name]] %in% names(objects[[name]]))) {
      stop("Appendix B8 is missing registered Census universe checks for ", name, ".", call. = FALSE)
    }
  }

  rows <- list(
    appendix_b8_exact_reconciliation_row(migration$d02_d03_2011_total_validation, "Migration", "2011", "D-02 vs D-03 migrant totals"),
    appendix_b8_exact_reconciliation_row(migration$d03_d07_2011_recent_work_validation, "Migration", "2011", "D-03 vs D-07 recent-work totals"),
    appendix_b8_exact_reconciliation_row(housing$source_validation_2001, "Housing", "2001", "Household/source universes"),
    appendix_b8_exact_reconciliation_row(housing$source_validation_2011, "Housing", "2011", "Household/source universes"),
    appendix_b8_exact_reconciliation_row(households$source_validation_2001, "Households", "2001", "Published household totals"),
    appendix_b8_exact_reconciliation_row(households$source_validation_2011, "Households", "2011", "Published household totals"),
    appendix_b8_exact_reconciliation_row(workers$b25_b26_2001_main_occupation_validation, "Workers", "2001", "B-25 vs B-26 occupation universe"),
    appendix_b8_exact_reconciliation_row(workers$b04_b25a_universe_validation, "Workers", "2011", "B-04 vs B-25A main-worker universe"),
    appendix_b8_exact_reconciliation_row(workers$b06_b25b_universe_validation, "Workers", "2011", "B-06 vs B-25B marginal-worker universe")
  )
  population <- safe_df(migration$d02_population_2011_validation)
  if (!all(c("n_districts", "max_migrant_stock_share_population") %in% names(population)) || nrow(population) != 1L ||
      !is.finite(num(population$max_migrant_stock_share_population)[[1L]]) || num(population$max_migrant_stock_share_population)[[1L]] > 1 + 1e-8) {
    stop("Appendix B8 migration-population universe validation is incomplete or invalid.", call. = FALSE)
  }
  rows <- append(rows, list(data.frame(
    domain = "Migration", period = "2011", reconciliation = "D-02 migrant stock vs Census population",
    support = sprintf("%d districts", as.integer(population$n_districts[[1L]])),
    diagnostic = "Max migrant stock / population",
    value = num(population$max_migrant_stock_share_population)[[1L]],
    n_reference = as.integer(population$n_districts[[1L]]),
    n_overlap_min = as.integer(population$n_districts[[1L]]),
    n_overlap_max = as.integer(population$n_districts[[1L]]),
    stringsAsFactors = FALSE
  )))
  csv <- safe_bind_rows(rows)
  order_key <- c(
    "Migration__2011__D-02 vs D-03 migrant totals",
    "Migration__2011__D-03 vs D-07 recent-work totals",
    "Migration__2011__D-02 migrant stock vs Census population",
    "Housing__2001__Household/source universes",
    "Housing__2011__Household/source universes",
    "Households__2001__Published household totals",
    "Households__2011__Published household totals",
    "Workers__2001__B-25 vs B-26 occupation universe",
    "Workers__2011__B-04 vs B-25A main-worker universe",
    "Workers__2011__B-06 vs B-25B marginal-worker universe"
  )
  csv <- csv[match(order_key, paste(csv$domain, csv$period, csv$reconciliation, sep = "__")), , drop = FALSE]
  if (nrow(csv) != 10L || any(is.na(csv$domain))) stop("Appendix B8 reconciliation registry is incomplete.", call. = FALSE)
  out <- data.frame(
    Domain = csv$domain, Period = csv$period, Reconciliation = csv$reconciliation,
    Support = csv$support, Diagnostic = csv$diagnostic,
    Value = ifelse(csv$diagnostic == "Max absolute count difference", sprintf("%.0f", csv$value), sprintf("%.3f", csv$value)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_b9_dise_publication_validation <- function(validation) {
  x <- safe_df(validation)
  required <- c("academic_year", "state", "district", "metric", "expected_value", "actual_value", "difference", "matches", "source_pdf", "source_page")
  if (length(setdiff(required, names(x))) || !nrow(x) || any(!x$matches %in% TRUE)) stop("Appendix B9 requires passing DISE publication checks.", call. = FALSE)
  csv <- x[, required, drop = FALSE]
  out <- data.frame(
    Year = csv$academic_year, District = paste(csv$district, csv$state, sep = ", "), Metric = csv$metric,
    Published = format(csv$expected_value, scientific = FALSE, trim = TRUE),
    Reconstructed = format(csv$actual_value, scientific = FALSE, trim = TRUE),
    Difference = format(csv$difference, scientific = FALSE, trim = TRUE),
    `Source page` = paste0(csv$source_pdf, ", p. ", csv$source_page), check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_c7_historical_balance <- function(balance) {
  if (!is.list(balance)) stop("Appendix C7 requires canonical 1991 baseline-balance output.", call. = FALSE)
  x <- safe_df(balance$joint_balance)
  predictors <- c("eventual_emie", "census_2001_ld", "helms_lim_ld_1991")
  domains <- c("demography", "human_capital", "economic_structure", "rural_development", "urban_development")
  x <- x[x$sample == "preferred_geography" & x$predictor_id %in% predictors & x$domain %in% domains, , drop = FALSE]
  x <- x[match(as.vector(outer(predictors, domains, paste, sep = "__")), paste(x$predictor_id, x$domain, sep = "__")), , drop = FALSE]
  if (nrow(x) != 15L || any(is.na(x$predictor_id)) || any(!x$status %in% "estimated")) stop("Appendix C7 requires three predictors by five historical balance domains.", call. = FALSE)
  csv <- x[, c("predictor_id", "domain", "n_tested_covariates", "joint_f", "joint_p", "n", "n_states"), drop = FALSE]
  out <- data.frame(
    Predictor = csv$predictor_id, Domain = csv$domain, `Covariates tested` = csv$n_tested_covariates,
    `Joint F` = sprintf("%.3f", csv$joint_f), `Joint p` = sprintf("%.3g", csv$joint_p), N = csv$n,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

appendix_c9_historical_first_stage <- function(first_stage) {
  if (!is.list(first_stage)) stop("Appendix C9 requires canonical historical first-stage output.", call. = FALSE)
  x <- safe_df(first_stage$comparison)
  ids <- c("instrument_only", "region_fe_census_controls", "state_fe_census_controls", "region_fe_expanded_controls", "state_fe_expanded_controls")
  x <- x[x$sample == "preferred_geography", , drop = FALSE]
  x <- x[match(ids, x$specification_id), , drop = FALSE]
  if (nrow(x) != 5L || any(is.na(x$specification_id)) || any(!x$status_1991 %in% "estimated") || any(!x$status_2001 %in% "estimated") || any(as.integer(x$n_1991) != as.integer(x$n_2001))) stop("Appendix C9 requires all five preferred historical first-stage specifications on common support.", call. = FALSE)
  csv <- x[, c("specification_id", "specification", "excluded_instrument_f_1991", "partial_r_squared_1991", "n_1991", "excluded_instrument_f_2001", "partial_r_squared_2001", "n_2001"), drop = FALSE]
  out <- data.frame(
    Specification = csv$specification, `1991 F` = sprintf("%.3f", csv$excluded_instrument_f_1991),
    `1991 partial R2` = sprintf("%.4f", csv$partial_r_squared_1991),
    `2001 F` = sprintf("%.3f", csv$excluded_instrument_f_2001),
    `2001 partial R2` = sprintf("%.4f", csv$partial_r_squared_2001), N = csv$n_1991,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  attr(out, "csv_data") <- csv
  out
}

make_appendix_validation_identification_exhibits <- function(
    district_lineage, consumption_mpce_validation, consumption_district_welfare,
    census_1991_primary_validation, historical_linguistic_persistence_validation,
    helms_lim_linguistic_distance_benchmark, district_panel, district_panel_with_dise,
    dise_iv_nss_validation, dise_publication_validation, lineage_panel_variant_review,
    census_migration_diagnostics, census_housing_diagnostics, census_household_diagnostics,
    census_worker_diagnostics, historical_baseline_balance_1991, historical_linguistic_first_stage_robustness) {
  list(
    appendix_b1_lineage_readiness = appendix_b1_lineage_readiness(district_lineage),
    appendix_b2_lineage_sensitivity = appendix_b2_lineage_sensitivity(lineage_panel_variant_review),
    appendix_b3_consumption_reconstruction = appendix_b3_consumption_reconstruction(consumption_mpce_validation),
    appendix_b4_hces_consistency_summary = appendix_b4_hces_consistency_summary(consumption_district_welfare),
    appendix_b4_hces_consistency = appendix_b4_hces_consistency_plot(consumption_district_welfare),
    appendix_b5_historical_language_persistence = appendix_b5_historical_persistence_plot(historical_linguistic_persistence_validation),
    appendix_b6_language_source_validation = appendix_b6_language_source_validation(census_1991_primary_validation, helms_lim_linguistic_distance_benchmark, historical_linguistic_persistence_validation, district_panel),
    appendix_b7_nss_dise_agreement = appendix_b7_nss_dise_plot(district_panel_with_dise, dise_iv_nss_validation),
    appendix_b8_census_universe_reconciliation = appendix_b8_census_universe_reconciliation(
      census_migration_diagnostics, census_housing_diagnostics, census_household_diagnostics, census_worker_diagnostics
    ),
    appendix_b9_dise_publication_validation = appendix_b9_dise_publication_validation(dise_publication_validation),
    appendix_c7_historical_balance = appendix_c7_historical_balance(historical_baseline_balance_1991),
    appendix_c9_historical_first_stage = appendix_c9_historical_first_stage(historical_linguistic_first_stage_robustness)
  )
}

save_appendix_validation_identification_exhibits <- function(exhibits, cfg) {
  table_names <- c("appendix_b1_lineage_readiness", "appendix_b2_lineage_sensitivity", "appendix_b3_consumption_reconstruction", "appendix_b4_hces_consistency_summary", "appendix_b6_language_source_validation", "appendix_b8_census_universe_reconciliation", "appendix_b9_dise_publication_validation", "appendix_c7_historical_balance", "appendix_c9_historical_first_stage")
  figure_names <- c("appendix_b4_hces_consistency", "appendix_b5_historical_language_persistence", "appendix_b7_nss_dise_agreement")
  if (!is.list(exhibits) || !all(c(table_names, figure_names) %in% names(exhibits))) stop("Appendix B/C validation exhibit bundle is incomplete.", call. = FALSE)
  written <- save_appendix_tables(exhibits, table_names, cfg)
  formats <- figure_formats(cfg)
  for (name in figure_names) {
    written <- c(written, save_plot_formats(exhibits[[name]], appendix_figure_path_base(name), formats, width = 8.2, height = 3.8))
  }
  unique(normalizePath(written, mustWork = FALSE))
}
