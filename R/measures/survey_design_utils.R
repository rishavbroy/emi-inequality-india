# Shared helpers for design-based survey estimation across consumption and labor.

with_survey_lonely_psu_adjustment <- function(expr) {
  old_options <- options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
  on.exit(options(old_options), add = TRUE)
  withCallingHandlers(
    expr,
    warning = function(w) {
      # survey can warn for domain-level lonely PSUs even when the requested
      # adjustment is applied. Muffle only that handled condition so strict
      # builds remain warning-clean; all other warnings still propagate.
      if (grepl("has only one PSU at stage", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
