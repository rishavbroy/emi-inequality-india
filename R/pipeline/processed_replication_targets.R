# District-level replication from tracked processed inputs.
#
# The full reconstruction graph remains in `_targets.R`. This smaller graph
# reruns paper-facing district analyses without loading restricted raw survey
# files or individual-level selection records.

processed_replication_target_definitions <- function() {
  list(
    tar_target(config_path, "config/final.yml", format = "file"),
    tar_target(cfg, read_config(config_path)),
    tar_target(paths, build_paths()),
    tar_target(
      census_2001_control_registry_file,
      census_2001_control_registry_path(paths),
      format = "file"
    ),
    tar_target(
      census_2001_control_registry,
      read_census_2001_control_registry(census_2001_control_registry_file)
    ),
    tar_target(
      processed_district_panel_file,
      processed_replication_panel_path(paths),
      format = "file"
    ),
    tar_target(
      processed_consumption_welfare_file,
      processed_replication_welfare_path(paths),
      format = "file"
    ),
    tar_target(
      district_panel,
      read_processed_replication_panel(
        processed_district_panel_file,
        census_2001_control_registry
      )
    ),
    tar_target(
      consumption_district_welfare,
      read_processed_replication_welfare(processed_consumption_welfare_file)
    ),
    tar_target(
      consumption_iv_outcome_registry_file,
      path_metadata(paths, "consumption_iv_outcomes.csv"),
      format = "file"
    ),
    tar_target(
      consumption_iv_outcome_registry,
      read_consumption_iv_outcome_registry(consumption_iv_outcome_registry_file)
    ),
    tar_target(
      consumption_iv_specifications,
      compile_consumption_iv_specifications(
        consumption_iv_outcome_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      consumption_iv_panel,
      attach_consumption_iv_outcomes(
        district_panel,
        consumption_district_welfare,
        consumption_iv_outcome_registry
      )
    ),
    tar_target(
      consumption_iv_outcome_coverage,
      validate_consumption_iv_outcome_coverage(
        summarize_consumption_iv_outcome_coverage(
          consumption_iv_panel,
          consumption_iv_specifications
        )
      )
    ),
    tar_target(
      consumption_iv_dynamics,
      {
        consumption_iv_outcome_coverage
        validate_consumption_iv_dynamics(
          estimate_consumption_iv_dynamics(
            consumption_iv_panel,
            consumption_iv_specifications,
            cfg
          ),
          consumption_iv_specifications
        )
      }
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
      schooling_consumption_conversion,
      diagnose_schooling_consumption_conversion(
        consumption_iv_panel,
        consumption_iv_outcome_registry,
        census_2001_control_registry
      )
    ),
    tar_target(
      alternative_distance_analysis_panel,
      prepare_alternative_distance_panel(
        district_panel,
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
      first_stage_absorption_diagnostics,
      diagnose_first_stage_absorption(
        district_panel,
        control_registry = census_2001_control_registry
      )
    ),
    tar_target(
      processed_replication_files,
      save_processed_replication_results(
        consumption_iv_dynamics,
        schooling_consumption_bridge,
        schooling_consumption_conversion,
        alternative_distance_first_stage_base,
        first_stage_absorption_diagnostics
      ),
      format = "file"
    )
  )
}
