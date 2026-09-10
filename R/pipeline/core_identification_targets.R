# Publication-required identification diagnostics.
#
# Keep this factory narrow: it owns only the alternative-distance first-stage
# objects consumed by the main-paper identification boundary. Expensive
# decompositions, weak-IV outcome grids, monotonicity checks, and other forensic
# diagnostics remain extended and augment this common base when requested.
core_identification_target_definitions <- function() {
  list(
    tar_target(
      alternative_distance_analysis_panel,
      prepare_alternative_distance_panel(
        district_panel, control_registry = census_2001_control_registry
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
    )
  )
}
