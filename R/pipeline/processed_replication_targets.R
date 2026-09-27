# District-level replication from tracked processed inputs.
#
# The full reconstruction graph remains in `_targets.R`. This smaller graph
# reruns paper-facing district analyses without loading restricted raw survey
# files or individual-level selection records. Shared analytical targets are
# composed from the same factories as the full build so the two graphs cannot
# drift in their statistical definitions.

processed_replication_target_definitions <- function() {
  c(
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
      )
    ),
    consumption_iv_analysis_target_definitions(),
    alternative_distance_base_target_definitions(),
    list(
      first_stage_absorption_target_definition(),
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
  )
}
