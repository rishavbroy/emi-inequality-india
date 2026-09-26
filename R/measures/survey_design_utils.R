# Shared helpers for design-based survey estimation across consumption, labor,
# and education-selection analyses.

with_survey_lonely_psu <- function(expr, lonely_psu = c("adjust", "average"), adjust_domain = TRUE) {
  lonely_psu <- match.arg(lonely_psu)
  old_options <- options(
    survey.lonely.psu = lonely_psu,
    survey.adjust.domain.lonely = isTRUE(adjust_domain)
  )
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
