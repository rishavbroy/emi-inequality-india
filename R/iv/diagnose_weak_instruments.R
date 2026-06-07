# This file is part of the EMI inequality research pipeline.
# Functions are intentionally small enough to be tested and called by _targets.R.


#' diagnose weak instruments
#'
diagnose_weak_instruments <- function(iv_models, district_panel, cfg) {
  estimate_first_stage(iv_models, district_panel, cfg)
}

#' jackknife first stage by state
#'
#' @return Explicit inactive status until state jackknife relevance checks are activated.
jackknife_first_stage_by_state <- function(...) {
  data.frame(
    status = "out_of_active_pipeline",
    reason = "State jackknife first-stage checks are documented as future relevance diagnostics and are not active.",
    stringsAsFactors = FALSE
  )
}

#' jackknife first stage by region
#'
#' @return Explicit inactive status until region jackknife relevance checks are activated.
jackknife_first_stage_by_region <- function(...) {
  data.frame(
    status = "out_of_active_pipeline",
    reason = "Region jackknife first-stage checks are documented as future relevance diagnostics and are not active.",
    stringsAsFactors = FALSE
  )
}

#' summarize weak iv metrics
#'
#' @return First non-empty diagnostics table, or explicit inactive status.
summarize_weak_iv_metrics <- function(...) {
  pieces <- list(...)
  pieces <- pieces[vapply(pieces, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]
  if (length(pieces)) return(safe_bind_rows(pieces))
  data.frame(
    status = "out_of_active_pipeline",
    reason = "No weak-IV diagnostics were supplied for summarization.",
    stringsAsFactors = FALSE
  )
}
