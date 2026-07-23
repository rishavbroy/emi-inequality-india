# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.

#' estimate 2sls
#'
estimate_2sls <- function(district_panel, formulas, cfg) {
  lapply(formulas, function(formula) {
    vars <- all.vars(formula)
    missing <- setdiff(vars, names(district_panel))
    if (length(missing)) {
      return(list(status = "out_of_active_pipeline", reason = paste("Missing variables:", paste(missing, collapse = ", "))))
    }
    if (!requireNamespace("ivreg", quietly = TRUE)) return(list(status = "out_of_active_pipeline", reason = "Package ivreg not installed."))
    fit <- ivreg::ivreg(
      formula,
      data = district_panel,
      model = TRUE,
      x = TRUE,
      y = TRUE
    )
    cluster_col <- first_col(district_panel, c("state_20", "state_std", "state_0708"))
    if (!is.null(cluster_col)) {
      mf_rows <- suppressWarnings(as.integer(rownames(stats::model.frame(fit))))
      if (length(mf_rows) && all(!is.na(mf_rows))) attr(fit, "cluster_state") <- district_panel[[cluster_col]][mf_rows]
    }
    fit
  })
}
