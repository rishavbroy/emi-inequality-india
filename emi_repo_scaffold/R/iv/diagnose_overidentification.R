# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' is overidentified
#'
#' @return A tibble, model object, list, or file path depending on context.
is_overidentified <- function(model_spec) {
  length(model_spec$excluded_instruments %||% character()) > length(model_spec$endogenous_vars %||% character())
}

#' diagnose overidentification
#'
#' @return A tibble, model object, list, or file path depending on context.
diagnose_overidentification <- function(iv_models, district_panel, cfg) {
  tibble::tibble(model = names(iv_models), test = "overidentification", applicable = FALSE, reason = "Current baseline has one excluded instrument for one endogenous regressor.")
}

#' run sargan if applicable
#'
#' @return A tibble, model object, list, or file path depending on context.
run_sargan_if_applicable <- function(model, model_spec, cfg) {
  if (!is_overidentified(model_spec)) return(NULL)
  if (!isTRUE(cfg$overidentification$sargan)) return(NULL)
  stop("TODO: implement Sargan diagnostic")
}

#' run gmm overid if applicable
#'
#' @return A tibble, model object, list, or file path depending on context.
run_gmm_overid_if_applicable <- function(model, model_spec, cfg) {
  if (!is_overidentified(model_spec)) return(NULL)
  if (!isTRUE(cfg$overidentification$robust_gmm)) return(NULL)
  stop("TODO: implement robust GMM overidentification diagnostic")
}

