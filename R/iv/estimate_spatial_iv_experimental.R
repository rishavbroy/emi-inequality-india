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
    model_status = models,
    failure_summary = summarize_spatial_iv_failures(models)
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
    return(data.frame(model = c("model_sdm2sls_cons", "model_sdm2sls_gini"), status = "not_estimated", reason = paste("Missing variables:", paste(missing, collapse = ", ")), stringsAsFactors = FALSE))
  }
  controls_needlag <- intersect(c("npeople_0708", "nhouses_0708", "consumption_0708", "gini_cons_0708"), names(panel))
  controls_lagged <- paste0("W_", controls_needlag)
  controls_lagged <- intersect(controls_lagged, names(panel))
  controls_nolag <- intersect(c("pct_urban", "pct_head_secondary_plus", "pct_muslim", "pct_st", "pct_obc", "pct_fem_head", "pct_medium_land", "pct_large_land"), names(panel))
  safe_formula <- function(dep, spatial_y) {
    make_iv_formula(
      dep = dep,
      endog = c(spatial_y, "EMIE", "W_EMIE"),
      exog = c(controls_needlag, controls_nolag, controls_lagged),
      inst = c(controls_needlag, controls_nolag, controls_lagged, "wavg_ling_degrees", "W_wLing", "W2_wLing")
    )
  }
  forms <- list(
    model_sdm2sls_cons = safe_formula("consumption_pct_change", "W_consY"),
    model_sdm2sls_gini = safe_formula("gini_change", "W_giniY")
  )
  safe_bind_rows(lapply(names(forms), function(name) {
    if (!requireNamespace("ivreg", quietly = TRUE)) {
      return(data.frame(model = name, status = "not_estimated", reason = "Package ivreg not installed.", formula = paste(deparse(forms[[name]]), collapse = " "), stringsAsFactors = FALSE))
    }
    tryCatch({
      fit <- ivreg::ivreg(forms[[name]], data = panel)
      data.frame(model = name, status = "estimated", reason = NA_character_, formula = paste(deparse(forms[[name]]), collapse = " "), nobs = stats::nobs(fit), stringsAsFactors = FALSE)
    }, error = function(e) {
      data.frame(model = name, status = "failed", reason = conditionMessage(e), formula = paste(deparse(forms[[name]]), collapse = " "), nobs = NA_integer_, stringsAsFactors = FALSE)
    })
  }))
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
