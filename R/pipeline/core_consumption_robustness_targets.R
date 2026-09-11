# Consumption-IV robustness targets required by final-paper Appendix C.
#
# These are registered analytical families, not exploratory output jobs. Core
# ownership keeps the publication exhibits reproducible when extended
# diagnostics are disabled; extended mode only persists the richer forensic
# CSV families and registries.

core_consumption_robustness_target_definitions <- function() {
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
      consumption_exclusion_sensitivity_specs,
      consumption_exclusion_sensitivity_specifications(consumption_iv_specifications)
    ),
    tar_target(
      consumption_exclusion_sensitivity,
      validate_consumption_exclusion_sensitivity(
        estimate_consumption_exclusion_sensitivity(
          consumption_iv_panel,
          consumption_exclusion_sensitivity_specs,
          consumption_iv_dynamics
        ),
        consumption_exclusion_sensitivity_specs
      )
    )
  )
}
