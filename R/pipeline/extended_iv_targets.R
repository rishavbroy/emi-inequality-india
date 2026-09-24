# IV extended-diagnostic target declarations.
# Final-paper robustness analyses are core-owned; this file persists their
# forensic outputs and owns registries/diagnostics not required by publication.
extended_iv_target_definitions <- function() {
  list(
    tar_target(
      diag_ext_consumption_scalar_iv_robustness_files,
      save_consumption_scalar_iv_robustness(
        consumption_scalar_iv_robustness,
        consumption_scalar_iv_robustness_support
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_treatment_robustness_files,
      save_consumption_iv_robustness_family(
        consumption_treatment_robustness,
        consumption_treatment_robustness_support,
        "consumption_treatment_robustness"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_alternative_welfare_files,
      save_consumption_iv_robustness_family(
        consumption_alternative_welfare_robustness,
        consumption_alternative_welfare_support,
        "consumption_alternative_welfare_robustness"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_control_strategy_files,
      save_consumption_iv_robustness_family(
        consumption_control_strategy_robustness,
        consumption_control_strategy_support,
        "consumption_control_strategy_robustness"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_control_parameterization_files,
      save_consumption_iv_robustness_family(
        consumption_control_parameterization_robustness,
        consumption_control_parameterization_support,
        "consumption_control_parameterization_robustness"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_historical_adjustment_files,
      save_consumption_iv_robustness_family(
        consumption_historical_adjustment_robustness,
        consumption_historical_adjustment_support,
        "consumption_historical_adjustment_robustness"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_historical_concept_matched_files,
      save_consumption_iv_robustness_family(
        consumption_historical_concept_matched_robustness,
        consumption_historical_concept_matched_support,
        "consumption_historical_concept_matched_robustness"
      ),
      format = "file"
    ),
    tar_target(
      diag_ext_schooling_consumption_bridge_files,
      save_schooling_consumption_bridge(schooling_consumption_bridge),
      format = "file"
    ),
    tar_target(
      diag_ext_schooling_consumption_conversion_files,
      save_schooling_consumption_conversion(schooling_consumption_conversion),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_robustness_evidence_files,
      save_consumption_robustness_evidence(consumption_robustness_evidence),
      format = "file"
    ),
    tar_target(
      diag_ext_consumption_exclusion_sensitivity_files,
      save_consumption_exclusion_sensitivity(consumption_exclusion_sensitivity),
      format = "file"
    ),
    tar_target(
      analysis_construct_registry,
      compile_analysis_construct_registry(
        english_opportunity_registry = english_opportunity_measure_registry,
        consumption_iv_registry = consumption_iv_outcome_registry,
        consumption_welfare_registry = consumption_welfare_outcomes,
        consumption_survey_registry = consumption_survey_registry
      )
    ),
    tar_target(
      diag_ext_analysis_construct_registry,
      write_diagnostic_csv(
        analysis_construct_registry,
        "outputs/diagnostics/extended/iv/construct_registry.csv"
      ),
      format = "file"
    ),
    tar_target(
      iv_falsification_adaptive_specs,
      iv_falsification_adaptive_specifications(
        iv_diagnostic_specification_registry(
          control_registry = census_2001_control_registry
        )
      )
    ),
    tar_target(
      analysis_design_registry,
      compile_analysis_design_registry(
        consumption_iv_specifications,
        english_opportunity_measure_registry,
        census_2001_control_registry,
        public_iv_specifications,
        consumption_scalar_iv_robustness_specifications,
        consumption_treatment_robustness_specifications,
        consumption_alternative_welfare_specifications,
        consumption_control_strategy_specifications,
        consumption_control_parameterization_specifications,
        consumption_historical_adjustment_specifications,
        consumption_historical_concept_matched_specifications,
        consumption_exclusion_sensitivity_specs,
        iv_falsification_adaptive_specs,
        consumption_registry = consumption_iv_outcome_registry,
        construct_registry = analysis_construct_registry
      )
    ),
    tar_target(
      diag_ext_analysis_design_registry,
      write_diagnostic_csv(
        analysis_design_registry,
        "outputs/diagnostics/extended/iv/analysis_design_registry.csv"
      ),
      format = "file"
    ),
    tar_target(
      iv_candidate_design_metadata_file,
      candidate_design_metadata_path(),
      format = "file"
    ),
    tar_target(
      iv_candidate_design_ledger,
      build_iv_candidate_design_ledger(
        public_iv_specifications,
        consumption_iv_specifications,
        census_2001_control_registry,
        consumption_welfare_outcomes,
        english_opportunity_measure_registry,
        consumption_scalar_iv_robustness_specifications,
        consumption_treatment_robustness_specifications,
        consumption_alternative_welfare_specifications,
        consumption_control_strategy_specifications,
        consumption_control_parameterization_specifications,
        consumption_historical_adjustment_specifications,
        consumption_historical_concept_matched_specifications,
        consumption_exclusion_sensitivity_specs,
        iv_falsification_adaptive_specs,
        metadata_path = iv_candidate_design_metadata_file
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
      diag_ext_alternative_distance_first_stages,
      save_alternative_distance_first_stages(alternative_distance_first_stages)$path,
      format = "file"
    ),
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
