# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' estimate spatial iv experimental
#'
#' Port the legacy eval=FALSE Chunk 30 spatial-2SLS attempts as an opt-in
#' benchmark/diagnostic artifact.  The attempts are documented and, when all
#' inputs are available, model formulas are constructed without being promoted
#' to the final public model.
estimate_spatial_iv_experimental <- function(district_panel, spatial_weights, cfg) {
  if (!diagnostic_enabled(cfg, "spatial_iv_experimental")) {
    status <- legacy_status_tbl("spatial_iv_experimental", "out_of_active_pipeline", "Experimental spatial IV is documented but not active.")
    return(list(status = status, augmented_panel_summary = data.frame(), model_status = data.frame(), failure_summary = summarize_spatial_iv_failures(status)))
  }
  panel <- add_spatial_lags(district_panel, spatial_weights, c("consumption_pct_change", "gini_change", "EMIE", "wavg_ling_degrees", "npeople_0708", "nhouses_0708", "consumption_0708", "gini_cons_0708"))
  models <- fit_spatial_lag_iv_attempts(panel, spatial_weights, cfg)
  status <- data.frame(diagnostic = "spatial_iv_experimental", status = "documented_experimental", reason = "Legacy eval=FALSE spatial lag IV attempts are reproduced as opt-in benchmark artifacts, not final models.", stringsAsFactors = FALSE)
  list(
    status = status,
    augmented_panel_summary = spatial_lag_summary(panel),
    model_status = models$model_status,
    coefficient_summary = models$coefficient_summary,
    clustered_coefficient_summary = models$clustered_coefficient_summary,
    diagnostics_summary = models$diagnostics_summary,
    failure_summary = summarize_spatial_iv_failures(models$model_status)
  )
}

add_spatial_lags <- function(district_panel, spatial_weights, vars) {
  panel <- if (inherits(district_panel, "sf")) sf::st_drop_geometry(district_panel) else as.data.frame(district_panel, stringsAsFactors = FALSE)
  if (!inherits(spatial_weights, "emi_spatial_weights") || !inherits(spatial_weights$listw, "listw")) return(panel)
  need_pkg("spdep", "spatial lag IV benchmark")
  rows <- spatial_weights$row_index %||% seq_len(nrow(panel))
  out <- panel[rows, , drop = FALSE]
  for (v in intersect(vars, names(out))) {
    out[[paste0("W_", legacy_spatial_lag_name(v))]] <- tryCatch(
      spdep::lag.listw(spatial_weights$listw, numeric_like(out[[v]]), zero.policy = TRUE),
      error = function(e) rep(NA_real_, nrow(out))
    )
  }
  if ("W_wLing" %in% names(out)) {
    out$W2_wLing <- tryCatch(spdep::lag.listw(spatial_weights$listw, numeric_like(out$W_wLing), zero.policy = TRUE), error = function(e) rep(NA_real_, nrow(out)))
  }
  out
}

legacy_spatial_lag_name <- function(v) {
  switch(v,
    consumption_pct_change = "consY",
    gini_change = "giniY",
    EMIE = "EMIE",
    wavg_ling_degrees = "wLing",
    v
  )
}

fit_spatial_lag_iv_attempts <- function(panel, spatial_weights = NULL, cfg = list()) {
  panel <- as.data.frame(panel, stringsAsFactors = FALSE)
  need <- c("consumption_pct_change", "gini_change", "W_consY", "W_giniY", "EMIE", "W_EMIE", "wavg_ling_degrees", "W_wLing", "W2_wLing")
  missing <- setdiff(need, names(panel))
  if (length(missing)) {
    status <- data.frame(model = c("model_sdm2sls_cons", "model_sdm2sls_gini"), status = "not_estimated", reason = paste("Missing variables:", paste(missing, collapse = ", ")), stringsAsFactors = FALSE)
    return(list(model_status = status, coefficient_summary = data.frame(), clustered_coefficient_summary = data.frame(), diagnostics_summary = data.frame()))
  }
  controls_needlag <- intersect(c("npeople_0708", "nhouses_0708", "consumption_0708", "gini_cons_0708"), names(panel))
  controls_lagged <- paste0("W_", controls_needlag)
  controls_lagged <- intersect(controls_lagged, names(panel))
  controls_nolag <- intersect(c("pct_urban", "pct_head_secondary_plus", "pct_muslim", "pct_st", "pct_obc", "pct_fem_head", "pct_medium_land", "pct_large_land"), names(panel))
  safe_formula <- function(dep, spatial_y) {
    make_iv_formula(
      dep = dep,
      endog = c(spatial_y, "EMIE", "W_EMIE"),
      instruments = c("wavg_ling_degrees", "W_wLing", "W2_wLing"),
      controls = unique(c(controls_needlag, controls_nolag, controls_lagged))
    )
  }
  forms <- list(
    model_sdm2sls_cons = safe_formula("consumption_pct_change", "W_consY"),
    model_sdm2sls_gini = safe_formula("gini_change", "W_giniY")
  )

  model_rows <- list()
  coef_rows <- list()
  cluster_rows <- list()
  diag_rows <- list()

  for (name in names(forms)) {
    form <- forms[[name]]
    if (!requireNamespace("ivreg", quietly = TRUE)) {
      model_rows[[name]] <- data.frame(model = name, status = "not_estimated", reason = "Package ivreg not installed.", formula = paste(deparse(form), collapse = " "), stringsAsFactors = FALSE)
      next
    }
    fit_attempt <- tryCatch(ivreg::ivreg(form, data = panel), error = function(e) e)
    if (inherits(fit_attempt, "error")) {
      model_rows[[name]] <- data.frame(model = name, status = "failed", reason = conditionMessage(fit_attempt), formula = paste(deparse(form), collapse = " "), nobs = NA_integer_, diagnostics_status = "not_run", cluster_se_status = "not_run", stringsAsFactors = FALSE)
      next
    }

    fit <- fit_attempt
    summary_attempt <- tryCatch(summary(fit, diagnostics = TRUE), error = function(e) e)
    diagnostics_status <- if (inherits(summary_attempt, "error")) paste("failed:", conditionMessage(summary_attempt)) else "estimated"
    cluster_attempt <- try_clustered_spatial_iv(fit, panel, name)
    cluster_status <- unique(cluster_attempt$status %||% "not_run")[[1]]
    model_rows[[name]] <- data.frame(
      model = name,
      status = "estimated",
      reason = "Legacy comments said these attempts did not work; current status only means ivreg returned an object, not that the model is suitable for final use.",
      formula = paste(deparse(form), collapse = " "),
      nobs = stats::nobs(fit),
      diagnostics_status = diagnostics_status,
      cluster_se_status = cluster_status,
      stringsAsFactors = FALSE
    )
    coef_rows[[name]] <- tidy_spatial_iv_coefficients(fit, name, "model_default")
    cluster_rows[[name]] <- cluster_attempt
    diag_rows[[name]] <- tidy_spatial_iv_diagnostics(summary_attempt, name)
  }

  list(
    model_status = safe_bind_rows(model_rows),
    coefficient_summary = safe_bind_rows(coef_rows),
    clustered_coefficient_summary = safe_bind_rows(cluster_rows),
    diagnostics_summary = safe_bind_rows(diag_rows)
  )
}

tidy_spatial_iv_coefficients <- function(fit, model_name, vcov_type) {
  mat <- tryCatch(stats::coef(summary(fit)), error = function(e) NULL)
  if (is.null(mat)) return(data.frame())
  out <- as.data.frame(mat, stringsAsFactors = FALSE)
  out$term <- rownames(mat)
  rownames(out) <- NULL
  names(out) <- gsub(" ", "_", tolower(names(out)))
  out$model <- model_name
  out$vcov_type <- vcov_type
  out[c("model", "vcov_type", "term", setdiff(names(out), c("model", "vcov_type", "term")))]
}

try_clustered_spatial_iv <- function(fit, panel, model_name) {
  if (!requireNamespace("sandwich", quietly = TRUE) || !requireNamespace("lmtest", quietly = TRUE)) {
    return(data.frame(model = model_name, status = "not_run", reason = "Packages sandwich and lmtest are required for the legacy clustered-SE coeftest attempt.", stringsAsFactors = FALSE))
  }
  cluster_col <- first_col(panel, c("region", "state_std", "state_20", "state_17", "state_07"))
  if (is.null(cluster_col)) {
    return(data.frame(model = model_name, status = "not_run", reason = "No region/state cluster column found.", stringsAsFactors = FALSE))
  }
  mf <- tryCatch(stats::model.frame(fit), error = function(e) data.frame())
  idx <- suppressWarnings(as.integer(rownames(mf)))
  cluster <- panel[[cluster_col]]
  if (length(idx) && all(is.finite(idx)) && max(idx, na.rm = TRUE) <= length(cluster)) cluster <- cluster[idx]
  vc <- tryCatch(sandwich::vcovCL(fit, cluster = cluster), error = function(e) e)
  if (inherits(vc, "error")) {
    return(data.frame(model = model_name, status = "failed", reason = conditionMessage(vc), stringsAsFactors = FALSE))
  }
  ct <- tryCatch(lmtest::coeftest(fit, vcov. = vc), error = function(e) e)
  if (inherits(ct, "error")) {
    return(data.frame(model = model_name, status = "failed", reason = conditionMessage(ct), stringsAsFactors = FALSE))
  }
  out <- as.data.frame(ct, stringsAsFactors = FALSE)
  out$term <- rownames(ct)
  rownames(out) <- NULL
  names(out) <- gsub(" ", "_", tolower(names(out)))
  out$model <- model_name
  out$status <- "estimated"
  out$cluster_column <- cluster_col
  out[c("model", "status", "cluster_column", "term", setdiff(names(out), c("model", "status", "cluster_column", "term")))]
}

tidy_spatial_iv_diagnostics <- function(summary_attempt, model_name) {
  if (inherits(summary_attempt, "error")) {
    return(data.frame(model = model_name, status = "failed", reason = conditionMessage(summary_attempt), stringsAsFactors = FALSE))
  }
  diag <- summary_attempt$diagnostics
  if (is.null(diag)) {
    return(data.frame(model = model_name, status = "not_available", reason = "summary(..., diagnostics = TRUE) returned no diagnostics table.", stringsAsFactors = FALSE))
  }
  out <- as.data.frame(diag, stringsAsFactors = FALSE)
  out$diagnostic <- rownames(diag)
  rownames(out) <- NULL
  names(out) <- gsub(" ", "_", tolower(names(out)))
  out$model <- model_name
  out$status <- "estimated"
  out[c("model", "status", "diagnostic", setdiff(names(out), c("model", "status", "diagnostic")))]
}

spatial_lag_summary <- function(panel) {
  panel <- as.data.frame(panel, stringsAsFactors = FALSE)
  lag_cols <- grep("^(W_|W2_)", names(panel), value = TRUE)
  if (!length(lag_cols)) return(data.frame())
  safe_bind_rows(lapply(lag_cols, function(nm) {
    x <- numeric_like(panel[[nm]])
    data.frame(variable = nm, n = sum(is.finite(x)), mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE), stringsAsFactors = FALSE)
  }))
}

summarize_spatial_iv_failures <- function(x) {
  if (is.data.frame(x) && nrow(x) && "status" %in% names(x)) {
    tab <- as.data.frame(table(status = x$status), stringsAsFactors = FALSE)
    names(tab) <- c("status", "n")
    tab$n <- as.integer(tab$n)
    return(tab)
  }
  data.frame(status = "not_run", n = 0L, stringsAsFactors = FALSE)
}
