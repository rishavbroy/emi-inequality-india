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
    extended_iv_target_definitions(),
    list(
      # The social-group diagnostic now has a main-paper figure consumer, so its
      # analytical object lives in the core graph. Extended mode persists the
      # full forensic CSV bundle without recomputing a parallel analysis.
      tar_target(
        diag_ext_nss64_schooling_social_group,
        save_nss64_schooling_social_group_diagnostic(
          nss64_schooling_social_group_diagnostic
        ),
        format = "file"
      )
    )
  )
}
