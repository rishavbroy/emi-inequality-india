# Compose extended diagnostic target families without changing target identities.
extended_diagnostic_target_definitions <- function() {
  c(
    extended_historical_target_definitions(),
    extended_lineage_target_definitions(),
    extended_census_target_definitions(),
    extended_economic_census_target_definitions(),
    extended_labor_target_definitions(),
    extended_mechanism_target_definitions(),
    extended_dise_target_definitions(),
    extended_iv_target_definitions()
  )
}
