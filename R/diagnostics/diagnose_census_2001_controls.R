# Diagnostics for the active predetermined Census 2001 control specification.

diagnose_census_2001_controls <- function(panel, revised_models, revised_first_stage, source_coverage) {
  coverage <- summarise_census_2001_control_coverage(panel)
  model_rows <- safe_bind_rows(lapply(names(revised_models), function(name) {
    model <- revised_models[[name]]
    data.frame(
      model = name,
      status = if (inherits(model, "ivreg")) "estimated" else model$status %||% "unavailable",
      nobs = if (inherits(model, "ivreg")) stats::nobs(model) else NA_real_,
      reason = if (inherits(model, "ivreg")) NA_character_ else model$reason %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }))
  balance <- run_iv_balance_diagnostics(panel)
  joint_balance <- run_iv_joint_balance_diagnostics(panel)
  list(
    source_coverage = source_coverage,
    coverage = coverage,
    balance = balance,
    joint_balance = joint_balance,
    models = model_rows,
    first_stage = revised_first_stage
  )
}

save_census_2001_control_diagnostics <- function(
    x, dir = "outputs/diagnostics/extended/census_2001_controls") {
  objects <- x[c("source_coverage", "coverage", "balance", "joint_balance", "models", "first_stage")]
  filenames <- c(
    source_coverage = "source_coverage.csv",
    coverage = "control_coverage.csv",
    balance = "instrument_balance.csv",
    joint_balance = "instrument_balance_joint.csv",
    models = "revised_model_status.csv",
    first_stage = "revised_first_stage.csv"
  )
  write_diagnostic_bundle(objects, dir, filenames)
}
