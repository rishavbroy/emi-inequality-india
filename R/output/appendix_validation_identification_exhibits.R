# Shared final-paper validation figures and historical-identification exhibits.
#
# These builders are presentation-only. Source validation that no longer has its own
# manuscript appendix remains enforced in the analytical modules that produce it.

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
    ggplot2::labs(title = "Historical persistence of linguistic distance from Hindi", x = "1991 linguistic distance", y = "2001 linguistic distance") +
    ggplot2::theme_minimal(base_size = 10)
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
    ggplot2::labs(x = "NSS EMI among enrolled", y = "DISE EMI enrollment share") +
    ggplot2::theme_minimal(base_size = 10)
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
    historical_linguistic_persistence_validation, district_panel_with_dise,
    dise_iv_nss_validation, historical_baseline_balance_1991,
    historical_linguistic_first_stage_robustness) {
  list(
    appendix_b5_historical_language_persistence = appendix_b5_historical_persistence_plot(
      historical_linguistic_persistence_validation
    ),
    appendix_b7_nss_dise_agreement = appendix_b7_nss_dise_plot(
      district_panel_with_dise, dise_iv_nss_validation
    ),
    appendix_c7_historical_balance = appendix_c7_historical_balance(
      historical_baseline_balance_1991
    ),
    appendix_c9_historical_first_stage = appendix_c9_historical_first_stage(
      historical_linguistic_first_stage_robustness
    )
  )
}

save_appendix_validation_identification_exhibits <- function(exhibits, cfg) {
  table_names <- c("appendix_c7_historical_balance", "appendix_c9_historical_first_stage")
  figure_names <- c("appendix_b5_historical_language_persistence", "appendix_b7_nss_dise_agreement")
  if (!is.list(exhibits) || !all(c(table_names, figure_names) %in% names(exhibits))) {
    stop("Shared validation/identification exhibit bundle is incomplete.", call. = FALSE)
  }
  written <- save_appendix_tables(exhibits, table_names, cfg)
  formats <- figure_formats(cfg)
  for (name in figure_names) {
    written <- c(
      written,
      save_plot_formats(
        exhibits[[name]], appendix_figure_path_base(name), formats, width = 8.2, height = 3.8
      )
    )
  }
  unique(normalizePath(written, mustWork = FALSE))
}
