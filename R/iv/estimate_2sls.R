# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' estimate 2sls
#'
estimate_2sls <- function(district_panel, formulas, cfg) {
  district_panel <- add_legacy_iv_aliases(district_panel)
  lapply(formulas, function(formula) {
    vars <- all.vars(formula)
    missing <- setdiff(vars, names(district_panel))
    if (length(missing)) {
      return(list(status = "out_of_active_pipeline", reason = paste("Missing variables:", paste(missing, collapse = ", "))))
    }
    if (!requireNamespace("ivreg", quietly = TRUE)) return(list(status = "out_of_active_pipeline", reason = "Package ivreg not installed."))
    fit <- ivreg::ivreg(formula, data = district_panel)
    cluster_col <- first_col(district_panel, c("state_20", "state_std", "state_0708"))
    if (!is.null(cluster_col)) {
      mf_rows <- suppressWarnings(as.integer(rownames(stats::model.frame(fit))))
      if (length(mf_rows) && all(!is.na(mf_rows))) attr(fit, "cluster_state") <- district_panel[[cluster_col]][mf_rows]
    }
    fit
  })
}

add_legacy_iv_aliases <- function(df) {
  df <- as.data.frame(df)
  alias <- function(new, old) if (old %in% names(df) && !new %in% names(df)) df[[new]] <<- df[[old]]
  alias("EMIE", "emie_2007")
  alias("consumption_0708", "consumption_2007")
  alias("gini_cons_0708", "gini_consumption_2007")
  alias("consumption_1718", "consumption_2017")
  if (!"consumption_pct_change" %in% names(df) && all(c("consumption_1718", "consumption_0708") %in% names(df))) {
    df$consumption_pct_change <- (num(df$consumption_1718) - num(df$consumption_0708)) / num(df$consumption_0708) * 100
  }
  if (!"gini_change" %in% names(df) && all(c("gini_cons_1718", "gini_cons_0708") %in% names(df))) {
    df$gini_change <- num(df$gini_cons_1718) - num(df$gini_cons_0708)
  }
  df
}

#' estimate consumption iv models
#'
estimate_consumption_iv_models <- function(district_panel, formulas, cfg) ivreg::ivreg(formulas$consumption, data = add_legacy_iv_aliases(district_panel))
#' estimate gini iv models
estimate_gini_iv_models <- function(district_panel, formulas, cfg) ivreg::ivreg(formulas$gini, data = add_legacy_iv_aliases(district_panel))

#' estimate model set
estimate_model_set <- function(district_panel, formulas, cfg) estimate_2sls(district_panel, formulas, cfg)
