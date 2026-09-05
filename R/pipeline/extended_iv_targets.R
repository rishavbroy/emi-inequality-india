# Iv extended-diagnostic target declarations.
# Statistical and data-construction logic remains in the domain modules; this file
# only groups target objects for pipeline composition.
extended_iv_target_definitions <- function() {
  list(
    tar_target(
      consumption_scalar_iv_robustness_specifications,
      compile_consumption_scalar_iv_robustness_specifications(
        consumption_iv_outcome_registry, census_2001_control_registry
      )
    ),
    tar_target(
      consumption_scalar_iv_robustness_support,
      consumption_iv_common_sample_support(
        consumption_iv_panel,
        consumption_scalar_iv_robustness_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_scalar_iv_robustness_panel,
      restrict_consumption_iv_to_common_samples(
        consumption_iv_panel,
        consumption_scalar_iv_robustness_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_scalar_iv_robustness,
      add_consumption_scalar_iv_multiplicity(
        validate_consumption_scalar_iv_robustness(
          validate_consumption_iv_dynamics(
            estimate_consumption_iv_dynamics(
              consumption_scalar_iv_robustness_panel,
              consumption_scalar_iv_robustness_specifications,
              cfg
            ),
            consumption_scalar_iv_robustness_specifications
          ),
          consumption_scalar_iv_robustness_support
        )
      )
    ),
    tar_target(
      diag_ext_consumption_scalar_iv_robustness_files,
      save_consumption_scalar_iv_robustness(
        consumption_scalar_iv_robustness,
        consumption_scalar_iv_robustness_support
      ),
      format = "file"
    ),
    tar_target(
      consumption_treatment_robustness_specifications,
      compile_consumption_treatment_robustness_specifications(
        consumption_iv_outcome_registry, census_2001_control_registry
      )
    ),
    tar_target(
      consumption_treatment_robustness_support,
      consumption_iv_common_sample_support(
        consumption_iv_panel,
        consumption_treatment_robustness_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_treatment_robustness_panel,
      restrict_consumption_iv_to_common_samples(
        consumption_iv_panel,
        consumption_treatment_robustness_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_treatment_robustness,
      add_consumption_iv_family_multiplicity(
        validate_consumption_iv_robustness_family(
          validate_consumption_iv_dynamics(
            estimate_consumption_iv_dynamics(
              consumption_treatment_robustness_panel,
              consumption_treatment_robustness_specifications,
              cfg
            ),
            consumption_treatment_robustness_specifications
          ),
          consumption_treatment_robustness_support,
          group_size = 6L,
          family_label = "Consumption intensive-margin EMI robustness"
        ),
        "consumption_treatment_robustness"
      )
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
      consumption_alternative_welfare_registry,
      build_consumption_alternative_welfare_registry(
        consumption_iv_outcome_registry,
        consumption_welfare_outcomes
      )
    ),
    tar_target(
      consumption_alternative_welfare_panel,
      attach_consumption_iv_outcomes(
        consumption_iv_panel,
        consumption_district_welfare,
        consumption_alternative_welfare_registry
      )
    ),
    tar_target(
      consumption_alternative_welfare_specifications,
      compile_consumption_alternative_welfare_specifications(
        consumption_alternative_welfare_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      consumption_alternative_welfare_support,
      consumption_iv_common_sample_support(
        consumption_alternative_welfare_panel,
        consumption_alternative_welfare_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_alternative_welfare_common_panel,
      restrict_consumption_iv_to_common_samples(
        consumption_alternative_welfare_panel,
        consumption_alternative_welfare_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_alternative_welfare_robustness,
      add_consumption_iv_family_multiplicity(
        validate_consumption_iv_robustness_family(
          validate_consumption_iv_dynamics(
            estimate_consumption_iv_dynamics(
              consumption_alternative_welfare_common_panel,
              consumption_alternative_welfare_specifications,
              cfg
            ),
            consumption_alternative_welfare_specifications
          ),
          consumption_alternative_welfare_support,
          group_size = 6L,
          family_label = "Consumption alternative-welfare robustness"
        ),
        "consumption_welfare_robustness"
      )
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
      consumption_control_strategy_specifications,
      compile_consumption_control_strategy_specifications(
        consumption_iv_outcome_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      consumption_control_strategy_support,
      consumption_iv_common_sample_support(
        consumption_iv_panel,
        consumption_control_strategy_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_control_strategy_panel,
      restrict_consumption_iv_to_common_samples(
        consumption_iv_panel,
        consumption_control_strategy_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_control_strategy_robustness,
      add_consumption_iv_family_multiplicity(
        validate_consumption_iv_robustness_family(
          validate_consumption_iv_dynamics(
            estimate_consumption_iv_dynamics(
              consumption_control_strategy_panel,
              consumption_control_strategy_specifications,
              cfg
            ),
            consumption_control_strategy_specifications
          ),
          consumption_control_strategy_support,
          group_size = 6L,
          family_label = "Consumption causal-control strategy robustness"
        ),
        "consumption_control_strategy"
      )
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
      consumption_control_parameterization_specifications,
      compile_consumption_control_parameterization_specifications(
        consumption_iv_outcome_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      consumption_control_parameterization_support,
      consumption_iv_common_sample_support(
        consumption_iv_panel,
        consumption_control_parameterization_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_control_parameterization_panel,
      restrict_consumption_iv_to_common_samples(
        consumption_iv_panel,
        consumption_control_parameterization_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_control_parameterization_robustness,
      add_consumption_iv_family_multiplicity(
        validate_consumption_iv_robustness_family(
          validate_consumption_iv_dynamics(
            estimate_consumption_iv_dynamics(
              consumption_control_parameterization_panel,
              consumption_control_parameterization_specifications,
              cfg
            ),
            consumption_control_parameterization_specifications
          ),
          consumption_control_parameterization_support,
          group_size = 8L,
          family_label = "Consumption control-parameterization robustness"
        ),
        "consumption_control_parameterization"
      )
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
      consumption_historical_adjustment_panel,
      attach_consumption_historical_adjustment_controls(
        consumption_iv_panel, historical_baseline_g2_sensitivity, .99
      )
    ),
    tar_target(
      consumption_historical_adjustment_specifications,
      compile_consumption_historical_adjustment_specifications(
        consumption_iv_outcome_registry, census_2001_control_registry
      )
    ),
    tar_target(
      consumption_historical_adjustment_support,
      consumption_iv_common_sample_support(
        consumption_historical_adjustment_panel,
        consumption_historical_adjustment_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_historical_adjustment_common_panel,
      restrict_consumption_iv_to_common_samples(
        consumption_historical_adjustment_panel,
        consumption_historical_adjustment_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_historical_adjustment_robustness,
      add_consumption_iv_family_multiplicity(
        validate_consumption_iv_robustness_family(
          validate_consumption_iv_dynamics(
            estimate_consumption_iv_dynamics(
              consumption_historical_adjustment_common_panel,
              consumption_historical_adjustment_specifications,
              cfg
            ),
            consumption_historical_adjustment_specifications
          ),
          consumption_historical_adjustment_support,
          group_size = 4L,
          family_label = "Consumption historical-adjustment robustness"
        ),
        "consumption_historical_adjustment"
      )
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
      consumption_vanneman_historical_controls,
      {
        raw_data_preflight
        build_population_interpolated_vanneman_baseline_1991_from_counts(
          historical_vanneman_1991_control_statistics,
          population_interpolation_geography_1991_2001_2011$crosswalk,
          .99
        )
      }
    ),
    tar_target(
      consumption_historical_concept_matched_panel,
      attach_consumption_historical_concept_matched_controls(
        consumption_iv_panel,
        historical_baseline_g2_sensitivity,
        consumption_vanneman_historical_controls,
        .99
      )
    ),
    tar_target(
      consumption_historical_concept_matched_specifications,
      compile_consumption_historical_concept_matched_specifications(
        consumption_iv_outcome_registry, census_2001_control_registry
      )
    ),
    tar_target(
      consumption_historical_concept_matched_support,
      consumption_iv_common_sample_support(
        consumption_historical_concept_matched_panel,
        consumption_historical_concept_matched_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_historical_concept_matched_common_panel,
      restrict_consumption_iv_to_common_samples(
        consumption_historical_concept_matched_panel,
        consumption_historical_concept_matched_specifications,
        "welfare_specification_id"
      )
    ),
    tar_target(
      consumption_historical_concept_matched_robustness,
      add_consumption_iv_family_multiplicity(
        validate_consumption_iv_robustness_family(
          validate_consumption_iv_dynamics(
            estimate_consumption_iv_dynamics(
              consumption_historical_concept_matched_common_panel,
              consumption_historical_concept_matched_specifications,
              cfg
            ),
            consumption_historical_concept_matched_specifications
          ),
          consumption_historical_concept_matched_support,
          group_size = 6L,
          family_label = "Consumption concept-matched historical-adjustment robustness"
        ),
        "consumption_historical_concept_matched"
      )
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
      schooling_consumption_bridge,
      diagnose_schooling_consumption_bridge(
        consumption_iv_panel,
        consumption_iv_outcome_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      diag_ext_schooling_consumption_bridge_files,
      save_schooling_consumption_bridge(schooling_consumption_bridge),
      format = "file"
    ),
    tar_target(
      consumption_robustness_evidence,
      build_consumption_robustness_evidence(list(
        scalar_iv = list(
          dynamics = consumption_scalar_iv_robustness,
          specifications = consumption_scalar_iv_robustness_specifications,
          analysis_role = "scalar_iv_robustness"
        ),
        intensive_margin = list(
          dynamics = consumption_treatment_robustness,
          specifications = consumption_treatment_robustness_specifications,
          analysis_role = "treatment_definition_robustness"
        ),
        welfare_definition = list(
          dynamics = consumption_alternative_welfare_robustness,
          specifications = consumption_alternative_welfare_specifications,
          analysis_role = "welfare_definition_robustness"
        ),
        control_strategy = list(
          dynamics = consumption_control_strategy_robustness,
          specifications = consumption_control_strategy_specifications,
          analysis_role = "control_strategy_robustness"
        ),
        control_parameterization = list(
          dynamics = consumption_control_parameterization_robustness,
          specifications = consumption_control_parameterization_specifications,
          analysis_role = "control_parameterization_robustness"
        ),
        historical_adjustment = list(
          dynamics = consumption_historical_adjustment_robustness,
          specifications = consumption_historical_adjustment_specifications,
          analysis_role = "historical_adjustment_robustness"
        ),
        historical_concept_matched = list(
          dynamics = consumption_historical_concept_matched_robustness,
          specifications = consumption_historical_concept_matched_specifications,
          analysis_role = "historical_concept_matched_robustness"
        )
      ))
    ),
    tar_target(
      diag_ext_consumption_robustness_evidence_files,
      save_consumption_robustness_evidence(consumption_robustness_evidence),
      format = "file"
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
        consumption_registry = consumption_iv_outcome_registry
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
        consumption_historical_concept_matched_specifications
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
