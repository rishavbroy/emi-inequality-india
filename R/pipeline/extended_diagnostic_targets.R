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
      tar_target(
        nss64_schooling_social_group_margins,
        build_education_exposure_2007_by_social_group(selection_data)
      ),
      tar_target(
        nss64_schooling_social_group_diagnostic,
        build_nss64_schooling_social_group_diagnostic(
          nss64_schooling_social_group_margins,
          district_panel,
          census_2001_control_registry
        )
      ),
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
