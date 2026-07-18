# This file is part of the EMI inequality research pipeline.

# sample-start: code-multicollinearity-spatial-autocorrelation

#' Structural-regressor formula for multicollinearity diagnostics
#'
#' For `ivreg` models, multicollinearity is diagnosed on the stage-two
#' structural regressors, not on the instrument matrix. The `ivreg` package
#' exposes this component through its formula/model-matrix methods.
multicollinearity_formula <- function(model) {
  if (inherits(model, "ivreg")) {
    return(stats::formula(model, component = "regressors"))
  }
  stats::formula(model)
}

#' Structural-regressor design matrix
multicollinearity_design_matrix <- function(model) {
  if (inherits(model, "ivreg")) {
    return(stats::model.matrix(model, component = "regressors"))
  }
  stats::model.matrix(model)
}

#' Model used for term-aware VIF/GVIF diagnostics
#'
#' `car::vif()` reports ordinary VIFs for one-degree-of-freedom terms and
#' generalized VIFs for factors or other multi-column terms. For an IV model,
#' fit an auxiliary OLS model with the same response and structural regressors.
#' VIFs describe collinearity in that regressor design, so the excluded
#' instrument matrix is deliberately outside this diagnostic.
multicollinearity_vif_model <- function(model) {
  if (!inherits(model, "ivreg")) return(model)
  stats::lm(
    multicollinearity_formula(model),
    data = stats::model.frame(model),
    weights = tryCatch(stats::weights(model), error = function(e) NULL),
    na.action = stats::na.exclude
  )
}

#' Normalize `car::vif()` output to a stable data-frame contract
normalize_vif_output <- function(x, model_scope) {
  if (is.null(x) || !length(x)) return(data.frame())
  if (is.matrix(x)) {
    out <- data.frame(
      term = rownames(x),
      gvif = as.numeric(x[, "GVIF"]),
      df = as.integer(x[, "Df"]),
      gvif_scaled = as.numeric(x[, "GVIF^(1/(2*Df))"]),
      stringsAsFactors = FALSE
    )
    out$vif <- ifelse(out$df == 1L, out$gvif, NA_real_)
  } else {
    out <- data.frame(
      term = names(x),
      gvif = as.numeric(x),
      df = 1L,
      gvif_scaled = sqrt(as.numeric(x)),
      vif = as.numeric(x),
      stringsAsFactors = FALSE
    )
  }
  out$model_scope <- model_scope
  out$status <- "estimated"
  out$reason <- NA_character_
  out[c("term", "model_scope", "df", "vif", "gvif", "gvif_scaled", "status", "reason")]
}

#' Compute term-aware VIF/GVIF diagnostics
compute_vif_if_applicable <- function(model) {
  scope <- if (inherits(model, "ivreg")) "ivreg_structural_regressors" else "model_regressors"
  if (!requireNamespace("car", quietly = TRUE)) {
    return(data.frame(
      term = NA_character_, model_scope = scope, df = NA_integer_,
      vif = NA_real_, gvif = NA_real_, gvif_scaled = NA_real_,
      status = "unavailable", reason = "Package 'car' is not installed.",
      stringsAsFactors = FALSE
    ))
  }
  tryCatch(
    normalize_vif_output(car::vif(multicollinearity_vif_model(model)), scope),
    error = function(e) data.frame(
      term = NA_character_, model_scope = scope, df = NA_integer_,
      vif = NA_real_, gvif = NA_real_, gvif_scaled = NA_real_,
      status = "unavailable", reason = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  )
}

multicollinearity_model_names <- function(iv_models) {
  models <- if (is.list(iv_models) && !inherits(iv_models, c("lm", "ivreg"))) iv_models else list(iv_models)
  model_names <- names(models)
  if (is.null(model_names)) model_names <- rep("", length(models))
  missing_names <- !nzchar(model_names)
  model_names[missing_names] <- paste0("model_", which(missing_names))
  list(models = models, names = model_names)
}

#' Diagnose one model's structural-regressor design
single_model_multicollinearity <- function(model, model_name) {
  scope <- if (inherits(model, "ivreg")) "ivreg_structural_regressors" else "model_regressors"
  design <- tryCatch({
    X <- multicollinearity_design_matrix(model)
    data.frame(
      model = model_name,
      diagnostic = "design_matrix",
      term = NA_character_,
      model_scope = scope,
      n = nrow(X),
      rank = qr(X)$rank,
      columns = ncol(X),
      kappa = kappa(X, exact = TRUE),
      df = NA_integer_,
      vif = NA_real_,
      gvif = NA_real_,
      gvif_scaled = NA_real_,
      status = "estimated",
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    data.frame(
      model = model_name,
      diagnostic = "design_matrix",
      term = NA_character_, model_scope = scope,
      n = NA_integer_, rank = NA_integer_, columns = NA_integer_, kappa = NA_real_,
      df = NA_integer_, vif = NA_real_, gvif = NA_real_, gvif_scaled = NA_real_,
      status = "out_of_active_pipeline", reason = conditionMessage(e),
      stringsAsFactors = FALSE
    )
  })

  vif <- compute_vif_if_applicable(model)
  if (!nrow(vif)) return(design)
  vif$model <- model_name
  vif$diagnostic <- "vif"
  vif$n <- NA_integer_
  vif$rank <- NA_integer_
  vif$columns <- NA_integer_
  vif$kappa <- NA_real_
  vif <- vif[names(design)]
  rbind(design, vif)
}

#' Diagnose multicollinearity
#'
#' @return A data frame containing a design-matrix summary row and term-level
#'   VIF/GVIF rows for every supplied model. Factor terms use GVIF and the
#'   dimension-adjusted `GVIF^(1/(2*Df))`; IV models use structural regressors.
diagnose_multicollinearity <- function(district_panel, iv_models, cfg) {
  named <- multicollinearity_model_names(iv_models)
  safe_bind_rows(Map(single_model_multicollinearity, named$models, named$names))
}

#' Save public multicollinearity diagnostics
save_multicollinearity_diagnostics <- function(diagnostics, path = "outputs/diagnostics/public/multicollinearity_diagnostics.csv") {
  write_diagnostic_csv(diagnostics, path)
}

# sample-end: code-multicollinearity-spatial-autocorrelation
