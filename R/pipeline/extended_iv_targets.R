# Iv extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_iv_target_definitions <- function() {
  list(
    tar_target(
      iv_candidate_design_ledger,
      build_iv_candidate_design_ledger(
        public_iv_specifications,
        consumption_iv_specifications,
        census_2001_control_registry
      )
    ),
    tar_target(
      diag_ext_iv_candidate_design_ledger,
      write_diagnostic_csv(
        iv_candidate_design_ledger,
        "outputs/diagnostics/extended/iv/candidate_design_ledger.csv"
      ),
      format = "file"
    ),
    tar_target(
      alternative_distance_analysis_panel,
      prepare_alternative_distance_panel(
        district_panel, control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      alternative_distance_augmentation_panel,
      project_alternative_distance_panel(
        district_panel,
        retain = "real_log_consumption_change",
        control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      alternative_distance_spec_registry,
      alternative_distance_registry(
        control_registry = census_2001_control_registry
      ),
      iteration = "list"
    ),
    tar_target(
      alternative_distance_specification,
      split(
        alternative_distance_spec_registry,
        seq_len(nrow(alternative_distance_spec_registry))
      ),
      iteration = "list"
    ),
    tar_target(
      alternative_distance_spec_diagnostic,
      diagnose_alternative_distance_specification(
        alternative_distance_analysis_panel,
        alternative_distance_specification
      ),
      pattern = map(alternative_distance_specification),
      iteration = "list"
    ),
    tar_target(
      alternative_distance_first_stage_base,
      assemble_alternative_distance_first_stages(
        alternative_distance_analysis_panel,
        alternative_distance_spec_registry,
        alternative_distance_spec_diagnostic
      )
    ),
    tar_target(
      alternative_distance_first_stages,
      augment_alternative_distance_diagnostics(
        alternative_distance_first_stage_base,
        alternative_distance_augmentation_panel,
        census_2001_languages,
        glottolog = glottolog_5_3,
        glottolog_crosswalk = census_glottolog_crosswalk
      )
    ),
    tar_target(diag_ext_alternative_distance_first_stages, save_alternative_distance_first_stages(alternative_distance_first_stages)),
    tar_target(
      census_2001_control_diagnostics,
      diagnose_census_2001_controls(
        district_panel, revised_iv_models, revised_first_stage_tests, census_2001_source_coverage
      )
    ),
    tar_target(
      diag_ext_census_2001_controls,
      save_census_2001_control_diagnostics(census_2001_control_diagnostics),
      format = "file"
    ),
    tar_target(
      consumption_outcome_comparison,
      compare_consumption_outcomes(district_panel, cfg)
    ),
    tar_target(
      diag_ext_consumption_prices,
      save_consumption_price_diagnostics(
        consumption_outcome_comparison,
        list(
          nss_2007_08 = consumption_households_2007,
          nss_2017_18 = consumption_households_2017,
          hces_2022_23 = consumption_households_real_hces_2022_23,
          hces_2023_24 = consumption_households_real_hces_2023_24
        ),
        district_panel
      ),
      format = "file"
    )
  )
}
