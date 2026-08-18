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
  instrument_name <- preferred_iv_variables()$instrument
  balance <- safe_bind_rows(lapply(census_2001_main_controls(), function(variable) {
    value <- num(panel[[variable]])
    instrument <- num(panel[[instrument_name]])
    keep <- is.finite(value) & is.finite(instrument)
    data.frame(
      variable = variable,
      n = sum(keep),
      correlation_with_instrument = if (sum(keep) > 2L) stats::cor(value[keep], instrument[keep]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  list(source_coverage = source_coverage, coverage = coverage, balance = balance, models = model_rows, first_stage = revised_first_stage)
}

save_census_2001_control_diagnostics <- function(x, dir = "outputs/diagnostics/extended/census_2001_controls") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    source_coverage = file.path(dir, "source_coverage.csv"),
    coverage = file.path(dir, "control_coverage.csv"),
    balance = file.path(dir, "instrument_balance.csv"),
    models = file.path(dir, "revised_model_status.csv"),
    first_stage = file.path(dir, "revised_first_stage.csv")
  )
  utils::write.csv(x$source_coverage, files[["source_coverage"]], row.names = FALSE, na = "")
  utils::write.csv(x$coverage, files[["coverage"]], row.names = FALSE, na = "")
  utils::write.csv(x$balance, files[["balance"]], row.names = FALSE, na = "")
  utils::write.csv(x$models, files[["models"]], row.names = FALSE, na = "")
  utils::write.csv(x$first_stage, files[["first_stage"]], row.names = FALSE, na = "")
  unname(files)
}
